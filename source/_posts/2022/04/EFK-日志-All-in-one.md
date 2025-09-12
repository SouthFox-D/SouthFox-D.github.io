author: SouthFox
title: 'EFK:日志 All in one……？'
date: 2022-04-04 17:04:30
tags: 技术
category: 技术
toc: true
---

突然发现自己搭的应用似乎上了两位数了。

,(read-more)

其中也不乏一些日志大户，每次排错时都要自己进去用日志命令脑死 `PgUp, PgDn` 一页一页翻找吗？

刚好最近看的书里提出了一种日志管理实现：`ELK`，即 `Elasticsearch, Logstash, Kibana` 三种开源应用的简称，其中:

`Elasticsearch`：负责给日志数据产生索引，方便检索。

`Logstash`：接收并处理日志，方便其他应用使用。

`Kibana`：一个展示前端，有酷炫的一众图表模板用来可视化数据。

似乎名头一直都在很多年了，使用起来似乎比较重量级，不知道有没有其他更轻量的实现……不过我也找不到其他实现就先这么用吧……

## 安装

因为搭建的应用以 `Docker` 居多，其中默认支持的日志方式有 `Fluentd`，所以就用这个代替 `Logstash` 了，所以是 `EFK` 方案。

`Fluentd` 采用包安装的方式安装，毕竟要用它处理宿主机上的一些日志，就不考虑塞到 `Docker` 里折腾自己了。

### 前期准备

参见[官网文档](https://docs.fluentd.org/installation/before-install)：

#### 安装 chrony

```shell
apt-get install chrony
```

此步骤是为了日志消息能有更准确的时间戳的样子？

#### 调高文件描述符

执行

```shell
ulimit -n
```

命令，要是显示 `1024` 的话，那么就得调高一点，防止文件描述符用完报错，

编辑 `/etc/security/limits.conf` 文件，在最后面加上：

```shell
root soft nofile 65536
root hard nofile 65536
* soft nofile 65536
* hard nofile 65536
```

之后重启，再次执行

```shell
ulimit -n
```

命令，显示 `65536` 即可成功。

#### 其他

其他的网络调优懒得搞了……问就是摸了！

### 安装 Fluentd

用的是 `Ububtu 20.04` ，参照[官方文档](https://docs.fluentd.org/installation/install-by-deb)，执行

```shell
curl -fsSL https://toolbelt.treasuredata.com/sh/install-ubuntu-focal-td-agent4.sh | sh
```

而且里面的版本描述是用大版本号的英文描述的，还得我翻[维基](https://zh.wikipedia.org/zh/Ubuntu)查，好好用阿拉伯数字不好嘛……

### 安装 EK

这里就用 `Docker` 省点力了……

```yaml
version: '3'
services:
  elasticsearch:
    image: elasticsearch:tag
    restart: always
    ports:
      - "127.0.0.1:9200:9200"
    environment:
      ES_JAVA_OPTS: "-Xmx256m -Xms256m"
      ELASTIC_PASSWORD: "you_password"
      http.cors.enabled: "true"
      http.cors.allow-origin: "*"
      xpack.security.enabled: "true"
      discovery.type: "single-node"
      xpack.security.authc.api_key.enabled: "true"

  kibana:
    image: kibana:tag
    restart: always
    links:
      - "elasticsearch"
    ports:
      - "127.0.0.1:5601:5601"
    environment:
      - ELASTICSEARCH_USERNAME=elastic
      - ELASTICSEARCH_PASSWORD="you_password"
```

 其中 `tag` 应该去 [`Docker Hub` 页面](https://hub.docker.com/_/elasticsearch)寻找，因为没有默认标签的样子只能手动指定版本了……而且注意要选择修了高危漏洞的版本。

之后 `up` 起来待用。

（**注意**这里没做数据持久化处理，有兴趣的可以自行找找要挂载哪些路径……）

## 配置

### Fluentd

配置文件路径是 `/etc/td-agent/td-agent.conf` ，

其中分为 `Input` 端，用 `<sourec>` 表示，`Output` 端，用 `<match>` 处理。

首先增加以下：

```shell
<source>
  @type forward
  port 24224
  bind 127.0.0.1
</source>

<match *.**>
  @type copy

  <store>
    @type elasticsearch
    host 127.0.0.1
    port 9200
    user elastic
    password you_password
    logstash_format true
    logstash_prefix fluentd
    logstash_dateformat %Y%m%d
    include_tag_key true
    type_name access_log
    tag_key @log_name
    flush_interval 1s
  </store>

  <store>
    @type stdout
  </store>
</match>
```

并且 `systemctl restart td-agent` 重启应用，开放`24224` 端口让 `Docker`  能够进行联协，并将相应数据通过 `9200` 端口汇入到 `Elasticsearch` 进行处理。

### Docker

管理每个 `docker-compose.yml` 文件，在每个服务后面加上相应的 `logging` 块：

```yaml
services:
  xxx:
  ......
  ......
  logging:
    driver: fluentd
  
  yyy:
  ......
  ......
  logging:
    driver: fluentd
```

(是的，感觉这样确实有点蠢，不过没找到其他更好实现而且只要做一次就先忍了……)

之后 `down` 再 `up` 重建容器即可。

### Kibana

没出错的话现在日志应该源源不断汇入到 `Elasticsearch` 并在处理了，现在该浏览成果了。

老调重弹之申请域名、指向域名、申请证书、反代 `5601` 端口，完成后访问对应域名应该就能进到管理面板了。

展开左边的菜单栏 `Management~Stack Management ->  Kibana~Index patterns -> Create index patterns ` 应该能看到有类似 `fluentd-YYMMDD` 形式的数据了，新建一个名叫 `fluentd-*` 的索引将数据囊括进来，就搞定了，之后可以去 `Analytics~Discover` 查看数据了……

到此，管理 `Docker` 产生的日志就这样搞定了……

## 额外

当然还没结束，毕竟还要更多！

### Nginx

`Fluentd` 直接安装就是为了更方便处理相应日志的，但是针对 `nginx` 来说，因为 `Fluentd` 使用的用户 `td-agent` 没有相应访问 `nginx` 日志的权限，那么首先要用

```shell
usermod -aG adm td-agent
```

命令放行对应目录，[参见](https://docs.fluentd.org/parser#how-to-use)。

之后在 `/etc/td-agent/td-agent.conf` 配置文件中新增以下配置：

```shell
<source>
  @type tail
  path /var/log/nginx/*access.log,/var/log/nginx/*access.log.1
  pos_file /var/log/td-agent/httpd-access.log.pos
  tag nginx.access
  <parse>
    @type nginx
  </parse>
</source>

<source>
  @type tail
  path /var/log/nginx/*error.log,/var/log/nginx/*erroe.log.1
  pos_file /var/log/td-agent/httpd-error.log.pos
  tag nginx.error
  <parse>
    @type nginx
  </parse>
</source>
```

然后 `systemctl restart td-agent` 重启应用。

其中一个小问题就是 `error` 日志会因为不能格式化而导致不能正确处理，当然只限 `error` 日志，所以就……眼不见为净了……反正这也算单独一个分类了……吧。

### 网络传输

既然 `All in one`，自然要处理好其他主机的服务了， `Fluentd` 也自带一个 `http` 方式进行传输。

#### 主机

在 `/etc/td-agent/td-agent.conf` 配置文件中新增以下配置：

```shell
<source>
  @type http
  port 9880
  bind 127.0.0.1
  body_size_limit 32m
  keepalive_timeout 10s
</source>
```

然后 `systemctl restart td-agent` 重启应用，再老调重弹之申请域名、指向域名、申请证书、反代 `9800` 端口。

#### 客机

在 `/etc/td-agent/td-agent.conf` 配置文件中新增以下配置：

```shell
<match **>
  @type http

    endpoint https://申请的域名:443/api
    open_timeout 3

    <format>
      @type json
    </format>
    json_array true
    <buffer>
      flush_interval 10s
    </buffer>
</match>
```

然后 `systemctl restart td-agent` 重启应用，

之后观察 `Kibana` 面板，发现有 `@logname` 为 `api` 的数据就算成功……这个 `logname` 发现不知道怎么改……算了，就当作区分不同机子了吧……

## 总结

这么处理下来，就应该能愉快的探索自己产生的日志了，能够直接指定时间段和按属性过滤条目已经暴杀直接翻找日志文件的方式了……

感觉能摸索的地方还有挺多的呢，不用各种追踪脚本仅凭日志文件也能发现很多东西呢……

不过难受的点就是，`EFK` 名头那么大，但是相关文档真的很少（就算用英文搜索）……感觉自己在用一个过气项目的样子，不知道是不是我不得要领……

最后附张图，用面板五分钟就搭出来的看板,感觉还能再挖掘点东西呢。

![看板](https://media.southfox.me/ipfs/bafkreicf4fknbhjyqw5dgh5vgg5pfl6fx2bwiiw745pun7ap7zf3tzdqdy)
