author: SouthFox
title: Gitea:一款自托管的 Git 服务
date: 2022-07-17 19:02:07
tags: 技术, 建站
category: 技术
toc: true
---

别忘了泡上一杯茶！

,(read-more)

##  绕不开的存在

当然，谈到 `Git` 服务时，肯定绕不过 `GitHub` 。为什么要大费周章自建呢？`GitHub` 不好吗？

是很好，但是也没那么好，因为：

- `GitHub` 是一家商业公司，那么首先盈利肯定是首要目的，为了不倒闭，指不准未来哪天就变质了。
- 同上，如果还是个自由软件爱好者的话，想必 `GitHub` 最近一些事件带来的臭味也不必我再提了……
- `Git` 是分布式的，那么选择同样也是……不知道现在还有多少人还傻傻认为 `Git` 和 `GitHub` 是一体的呢？

也可参见 [GiveUpGitHub](https://sfconservancy.org/GiveUpGitHub/) 一文章。

## Gitea

`Gitea` 是一个用 `Go` 编写、面向自建的、轻量级的 `Git` 服务。其安装十分便捷，直接下载个可执行文件也可快速搭建起来，当然为了后续跟其它服务联动，还是用 `docker-compose` 方便点。

话不多说，新建一个文件夹然后往里建 `docker-compose.yml` 文件并写入以下配置：

```yaml
version: "3.7"

services:
  gitea:
    image: gitea/gitea:latest
    container_name: gitea
    restart: unless-stopped
    environment:
      - USER_UID=1000
      - USER_GID=1000
    volumes:
      - ./data/gitea:/data
      - /etc/timezone:/etc/timezone:ro
      - /etc/localtime:/etc/localtime:ro
    ports:
      - "127.0.0.1:3000:3000"
      - "2222:22"
    networks:
      - cicd_net

  drone:
    container_name: drone
    image: drone/drone:latest
    restart: unless-stopped
    depends_on:
      - gitea
    environment:
      # https://docs.drone.io/server/provider/gitea/
      - DRONE_DATABASE_DRIVER=sqlite3
      - DRONE_DATABASE_DATASOURCE=/data/database.sqlite
      - DRONE_GITEA_SERVER=https://my.git.server/
      - DRONE_GIT_ALWAYS_AUTH=false
      - DRONE_RPC_SECRET=changeme...
      - DRONE_SERVER_PROTO=https
      - DRONE_SERVER_HOST=https://cicd.git.server
      - DRONE_GITEA_CLIENT_ID=changeme...
      - DRONE_GITEA_CLIENT_SECRET=changeme...
    ports:
      - "127.0.0.1:3001:80"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - ./data/drone:/data
    networks:
      - cicd_net

  drone-runner:
    container_name: drone-runner
    image: drone/drone-runner-docker:latest
    restart: unless-stopped
    depends_on:
      - drone
    environment:
      # https://docs.drone.io/runner/docker/installation/linux/
      # https://docs.drone.io/server/metrics/
      - DRONE_RPC_PROTO=http
      - DRONE_RPC_HOST=drone
      - DRONE_RPC_SECRET=changeme...
      - DRONE_RUNNER_NAME="action-runner"
      - DRONE_RUNNER_CAPACITY=2
      - DRONE_RUNNER_NETWORKS=cicd_net
      - DRONE_DEBUG=false
      - DRONE_TRACE=false
    networks:
      - cicd_net
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock

networks:
  cicd_net:
    name: cicd_net
```

因为预想场景是私人自用，所以选择了 `SQlite3` 数据库，觉得别扭可以自己改掉……

然后准备两个域名，这里用 `my.git.server` 域名指定用来运行 `Gitea` 服务的域名，一个 `cicd.git.server` 域名用来指定运行后续的自动构建服务的域名（可选）。

首先为 `my.git.server` 域名设定`DNS` 后申请证书，`certbot certonly --nginx -d my.git.server`

，申请后写入 `nginx` 配置：

```nginx
#不要忘了替换成自己的域名
server {
    listen 80;
    listen [::]:80;
    server_name my.git.server;

    location /.well-known/acme-challenge {}
    location / {
        return 301 https://$host$request_uri;
    }
}

server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;
    server_name my.git.server;

    access_log  /var/log/nginx/access.log;
    #root /home/plume/Plume/ ;

    ssl_certificate /etc/letsencrypt/live/my.git.server/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/my.git.server/privkey.pem;

    # for ssl conf: https://cipherli.st/
    ssl_protocols TLSv1.2 TLSv1.3;# Requires nginx >= 1.13.0 else use TLSv1.2
    ssl_prefer_server_ciphers on;
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;# openssl dhparam -out /etc/letsencrypt/ssl-dhparam.pem 4096
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384;
    ssl_ecdh_curve secp384r1; # Requires nginx >= 1.1.0
    ssl_session_timeout  10m;
    ssl_session_cache shared:SSL:10m;
    ssl_session_tickets off; # Requires nginx >= 1.5.9
    ssl_stapling on; # Requires nginx >= 1.3.7
    ssl_stapling_verify on; # Requires nginx => 1.3.7
    resolver 9.9.9.9 80.67.169.12 valid=300s;
    resolver_timeout 5s;
    add_header Strict-Transport-Security "max-age=63072000; includeSubDomains; preload";
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    #如果不想显示外链图片可把 img-src 一栏改掉
    add_header Content-Security-Policy "default-src 'self'; img-src *; frame-ancestors 'self'; frame-src https:";

    location / {
        proxy_pass http://localhost:3000/;
        proxy_set_header Host $http_host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        client_max_body_size 50m;
    }
}
```

之后 `nginx -s reload` 重载配置，然后 `docker-compose up -d gitea` 把 `Gitea` 服务启动，之后前往 `my.git.server` 地址进行配置并新建一个管理员账户。

### Drone

到此服务就基本可用了，但是我相信很多人割舍不下 `GitHub` 的一个原因就是因为它的 `Actions` 很香。不过可以选择 `Drone` ，一个同样用 `GO` 编写的、轻量级的自动构建服务，也可以让体验往 `GitHub Actions` 靠拢。

首先为 `cicd.git.server` 域名申请证书并写入 `nginx` 配置文件，配置文件可复用上面的，只需要更改相关域名和 `nginx` 配置文件里的 `location /` 里的 `proxy_pass` 端口号就行（本例子是 `3001`）。

之后为 `Drone` 和 `Gitea` 联动做准备，登陆自己 `Gitea` 实例的帐号→设置→应用→创建新的 OAuth2 应用程序，应用名称随意填（本例子填 drone），重定向 URI 填入 `https://cicd.git.server/login` （替换成自己的域名）。

点击创建应用→会生成一个客户端 ID 和一个客户端密钥→替换到上述 `docker-compose.yml` 里 `drone` 一栏的 `DRONE_GITEA_CLIENT_ID` 和 `DRONE_GITEA_CLIENT_SECRET` 。

之后再使用 `openssl rand -hex 16` 命令生成一串随机字符串给 `DRONE_RPC_SECRET` 使用（有两个地方需要替换）。

确认无误后使用 `docker-compose up -d` 启动全部服务，之后前往 `https://cicd.git.server` 地址查看是否正常运行，是的话点击登陆，看是否能够和 `Gitea` 进行联动，如果显示错误请检查是否与上述配置一致（例如我之前就遇到了无法验证的错误，排查后发现是某个路径多带了一个 `/` 导致之后生成的验证路径出错，去掉 `/` 之后就正常了）。

### 仓库操作

因为宿主机的 `22` 端口已被占用，所以克隆或者 `PUSH` 仓库使用的端口应该为 `2222`，觉得别扭也有其他教程教如何与宿主机共用端口，我就不折腾了（反正 `Just work!` ）。

还有如果为网站使用了 `Cloudflare` 之类的 `CDN` 服务的话，那么 `SSH` 协议也是不能用的，得用网站域名下的真实 `IP` 地址克隆或 `PUSH` 仓库。

如果像我什么都没改的话，想要克隆仓库就会得到这么一个奇怪的地址：

`git clone ssh://git@机子真实IP:2222/用户名/仓库名.git`

反正 `Just work`  ！

### 一个案例

那么接下来就讲讲我用这套服务的案例吧，自动生成文件并推送一个 `HUGO` 博客。

首先为仓库根目录写下 `.drone.yml` 文件：

```yaml
kind: pipeline
name: build

steps:
- name: build
  image: klakegg/hugo:alpine
  commands:
  - hugo

- name: deploy
  image: node
  environment:
    CLOUDFLARE_API_TOKEN:
      from_secret: api_token
    CLOUDFLARE_ACCOUNT_ID:
      from_secret: account_id
  commands:
  - npm install -g wrangler
  - npx wrangler pages publish public --project-name 项目名 --commit-dirty=true
```

可以见到和 `GitHub Actions` 的配置还是挺像的，琢磨琢磨还是挺快上手的。

其中 `steps` 指定了所需步骤，`image` 指定了需要什么样的 `docker` 镜像，第一栏拉取了 `klakegg/hugo` 镜像并使用 `hugo` 命令生成静态文件。

之后是 `deploy` 一栏里的 `environment` ，像 `GitHub` 一样，密钥相关可以使用 `secret` 功能导入到环境变量中，可在自己的 `cicd.git.server` →相关仓库→ `Settings` → `Secrets` 里进行导入。

接下来使用 `node` 镜像安装了个 `wrangler` 包，这是 `Cloudflare Pages` 部署要用的，如果也用 `Cloudflare Pages` 部署的话可以参考:

[Wrangler pages commands](https://developers.cloudflare.com/workers/wrangler/commands/#pages)

[Running Wrangler in CI/CD](https://developers.cloudflare.com/workers/wrangler/ci-cd/)

PS：不得不吐槽相关操作首先必须要用 `project create` 命令里的 `--production-branch` 显式指定部署分支否则接下来的部署都会被识别为预览而不会真正部署到……被这个坑卡了好一会……

之后再进行相关操作后应该能看见自己 `Gitea` 实例也有小绿勾了（小红叉也行，至少证实自动部署服务有在用了）！

### 加主题

默认主题还挺程序员风格的，既然都自建了，肯定要加点主题快乐一下了。

以[现代主题](https://codeberg.org/Freeplay/Gitea-Modern/)为例。

如果是以上面的配置文件搭建起来的话要在自己文件夹下找到 `./data/gitea/gitea` 路径，并在里头新建 `public/css` 下 `wget https://codeberg.org/Freeplay/Gitea-Modern/raw/branch/main/Gitea/theme-gitea-modern.css`  获取主题文件。

之后再编辑 `./data/gitea/gitea/conf/app.ini` 文件，在最后面加上：

```ini
请自己想象下……
因为现在我服务器炸了……
```

之后再用 `docker-compose down`  和 `docker-compose up -d` 重启服务。

### 安全

如果没有相关要求，请务必关闭实例的注册功能。

参见，[记一次自建 Gitea + Drone 实例被挖矿的经历](https://imlonghao.com/59.html)

也是修改 `./data/gitea/gitea/conf/app.ini` 里的：

```ini
请自己查阅……
因为我现在服务器炸了
```

之后再用 `docker-compose down`  和 `docker-compose up -d` 重启服务，然后再检查站点是否关闭了注册入口。

### 参考

[CICD With DroneCI and Gitea Using Docker Compose](https://blog.ruanbekker.com/blog/2021/03/09/cicd-with-droneci-and-gitea-using-docker-compose/)

[Gitea Docs](https://docs.gitea.io/en-us/)

[Drone Docs](https://docs.drone.io/)

[透過 Drone 建立自動部署流程，部署排程設定與權限管理 - 薛丁格的工程師](https://tech.ray247k.com/blog/202106-drone-cicd-3-advanced-cron-job/)
