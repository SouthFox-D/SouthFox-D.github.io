author: SouthFox
title: 搭建Matrix即时通信服务
date: 2022-04-15 20:54:19
tags: 技术
category: 技术
---

总之稍微记录一下。

,(read-more)

- 事先约定 `matrix.org` 是前端地址 `synapse.matrix.org` 是后端地址，实际请改成自己的……具体为啥这么做可以看[官方文档](https://matrix-org.github.io/synapse/latest/delegate.html)，如果嫌麻烦也可以不启用这功能……

- 新建文件夹，在里面新建一个 `docker-compose.yml` 文件，往里写入

```yaml
#也感谢糖喵提供的配置文件~
version: "3.4"

services:
  synapse:
    hostname: matrix
    image: matrixdotorg/synapse:latest
    restart: always
    container_name: matrix_server   
    depends_on:
      - db
      - redis
    ports:
      - "127.0.0.1:8001:8008"
    volumes:
      - ./synapse/data:/data
    networks:
      - synapse_network
      - external_network
    healthcheck:
      test: ["CMD-SHELL", "curl -s localhost:8008/health || exit 1"]

  db:
    image: postgres
    restart: always
    container_name: matrix_db
    volumes:
      - ./synapse/db:/var/lib/postgresql/data
    environment:
      POSTGRES_USER: synapse
      POSTGRES_PASSWORD: 随便什么密码
      POSTGRES_DB: synapse
      POSTGRES_INITDB_ARGS: "--encoding='UTF8' --lc-collate='C' --lc-ctype='C'"
    networks:
      - synapse_network
    healthcheck:
      test: ["CMD", "pg_isready", "-U", "synapse"]

  redis:
    image: redis:6.0-alpine
    restart: always
    container_name: matrix_redis  
    volumes:
      - ./synapse/redis:/data
    networks:
      - synapse_network
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]

networks:
  synapse_network:
    internal: true
  external_network:
```

- 之后运行 `docker-compose run --rm -e SYNAPSE_SERVER_NAME=前端地址 synapse generate` 命令生成配置文件，之后检查在 `./synapse/data` 路径下是否有叫 `homeserver.yaml` 的配置文件，编辑配置文件 `nano ./synapse/data/homeserver.yaml`

```yaml
# 重点改以下配置
server_name: "matrix.org"

public_baseurl: https://synapse.matrix.org/

serve_server_wellknown: true

database:
  name: psycopg2
  txn_limit: 10000
  args:
    user: synapse
    password: docker 配置写的随便什么密码
    database: synapse
    host: db
    port: 5432
    cp_min: 5
    cp_max: 10

#database:
#  name: sqlite3
#  args:
#    database: /data/homeserver.db
#↑注释掉使用 sqlite3 的配置

redis:
  # Uncomment the below to enable Redis support.
  #
  enabled: true

  # Optional host and port to use to connect to redis. Defaults to
  # localhost and 6379
  #
  host: redis
  port: 6379
```

- 之后再启动服务，`docker-compose start`
- 编辑 `matrix.org` 的 `nginx` 配置文件加入以下配置

```nginx
    location /.well-known/matrix/client {
        return 200 '{"m.homeserver": {"base_url": "synapse.matrix.org"}}';
        default_type application/json;
        add_header Access-Control-Allow-Origin *;
    }

    location /.well-known/matrix/server {
        return 200 '{"m.server": "synapse.matrix.org:443"}';
        default_type application/json;
        add_header Access-Control-Allow-Origin *;
    }
#注意替换自己的前端后端地址
```

- 新建 `synapse.matrix.org` 的 `dns` ，指向服务器地址，再 `certbot certonly --nginx -d synapse.matrix.org` 申请证书
- 新建一个 `synapse.matrix.org` 的配置文件

```nginx
server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;

    server_name synapse.matrix.org;

    ssl_certificate /etc/letsencrypt/live/synapse.matrix.org/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/synapse.matrix.org/privkey.pem;

    # Various TLS hardening settings
    # https://raymii.org/s/tutorials/Strong_SSL_Security_On_nginx.html
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers on;
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384;
    ssl_session_timeout  10m;
    ssl_session_cache shared:SSL:10m;
    ssl_session_tickets on;
    ssl_stapling on;
    ssl_stapling_verify on;


    location ~ ^(/_matrix|/_synapse/client) {
        # note: do not add a path (even a single /) after the port in `proxy_pass`,
        # otherwise nginx will canonicalise the URI and cause signature verification
        # errors.
        proxy_pass http://127.0.0.1:8001;
        proxy_set_header X-Forwarded-For $remote_addr;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Host $host;

        # Nginx by default only allows file uploads up to 1M in size
        # Increase client_max_body_size to match max_upload_size defined in homeserver.yaml
        client_max_body_size 500M;
    }

}
```

- 重载 `nginx` 配置文件，`nginx -s reload`
- 之后去[检查服务](https://federationtester.matrix.org/)（需科学）输入自己的前端地址 `matrix.org` 检查是否正常
- 用 `docker-compose exec synapse /bin/bash` 进入 `synapse` 容器

```shell
cd data
#注册新用户
register_new_matrix_user -c homeserver.yaml http://localhost:8008 
#注册完后用 exit 退出容器
exit
```

- 完成后用任意一个客户端登陆即可使用，注意登陆用的地址是后端地址 `synapse.matrix.org` 
