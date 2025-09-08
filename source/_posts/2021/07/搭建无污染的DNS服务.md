author: SouthFox
title: 搭建无污染的DNS服务
date: 2021-07-06 17:29:50
tags: 技术, DNS
category: 技术
toc: true
---

DNS 作为互联网世界的电话簿，重要性不言而喻。但是平常使用时，默认情况都是在裸奔，非常不安全，劫持和污染处处存在，所以搭建一个自己放心的 DNS 服务还是有必要的……

<!-- more -->

## 准备

- VPS （国内延迟低、国外无阻碍，看取舍吧。有条件也可以用树莓派之类的）
- coredns（用于配置服务器的 Dns Over Tls 「dot」或 Dns Over Https 「doh」）
- dnsmasq（用于转发 dns 请求）
- pihole（可选，可以干掉追踪器和广告）
- dnsproxy（将请求转发到其他加密 DNS 服务器上）





### Coredns

安卓自从 9 版本之后就内置了 Dns Over Tls 「dot」 配置，叫做 `私人DNS` ，这样进行配置就不用一个一个改 wifi 设定，同时还对蜂窝网络起效果，所以可以用 `CoreDNS`  来加密设备到服务器之间的请求……

`CoreDNS` 同样使用 `Golang` 编写，仓库内提供了[可执行程序](https://github.com/coredns/coredns/releases) 和 [systemd ](https://github.com/coredns/deployment/tree/master/systemd)文件，就算你的发行版没有提供 `CoreDNS` 的打包也可以自行写个服务。

配置如下：

```
tls://.:853 {
  tls /etc/coredns/cert.crt /etc/coredns/cert.key
    forward . 127.0.0.1:53
}
```

作用是将 853 端口的 dot 请求转发到 53 端口所运行的服务上……

证书用 `certbot` 申请，偷偷摸摸的用 853 ，国内的云服务商应该不会注意到（毕竟盯着的是 443 和 80 端口的情况多一点……吧）





### Pihole （可选）

`pihole` 的好处就是网页的控制面板很好用，看着面板中的统计数据将有非常大的满足感，除此之外就没有啥了。

如果不是全新机子的话，还是用 `docker-compose` 安装吧，pihole 对于安装环境还是很挑剔的。

首先安装 `docker` 并启动，然后安装 `docker-compose` ，新建文件夹下新建 `docker-compose.yml` 文件，输入：

```dockerfile
version: "3"

# More info at https://github.com/pi-hole/docker-pi-hole/ and https://docs.pi-hole.net/
services:
  pihole:
    container_name: pihole
    image: pihole/pihole:latest
    ports:
      - "53:53/tcp"
      - "53:53/udp"
      - "67:67/udp"
      - "宿主机想开放的端口:下方配置 WEB_PORT 所写/tcp"
    environment:
      TZ: 'Asia/Shanghai'
      WEBPASSWORD: '网页管理面板密码'
      WEB_PORT: 需要开放的端口
      PIHOLE_DNS_: '8.8.8.8'
      ServerIP: '服务器 Ip'
      #VIRTUAL_HOST: '服务器访问管理面板域名'
      #DNSMASQ_USER: 'pihole'

    volumes:
      - './etc-pihole/:/etc/pihole/'
      - './etc-dnsmasq.d/:/etc/dnsmasq.d/'
    # Recommended but not required (DHCP needs NET_ADMIN)
    #   https://github.com/pi-hole/docker-pi-hole#note-on-capabilities
    cap_add:
      - NET_ADMIN
    restart: unless-stopped
```

注意 `ServerIP` 和 `VIRTUAL_HOST` 要写对，要不然会被禁止访问……





#### 列表

装了 pihole 要发挥最大的作用就得找一个优质的屏蔽列表。

可以使用 [anti-AD](https://github.com/privacy-protection-tools/anti-AD) ，能屏蔽国内大部分追踪器和广告地址，对于 pihole 的配置文件在 [这](https://anti-ad.net/domains.txt) 。





### Dnsmasq

打开配置文件，更改 `port` 监听端口， `server`  写上游 dns 地址（`:` 要用 `#` 代替）。

无日志的 `dot` , `doh` 服务器一般都是在国外，一般延迟都很糟糕，所以对于国内的域名来说访问延迟将会很大。可以用 `Dnsmasq` 搭配 [dnsmasq-china-list](https://github.com/felixonmars/dnsmasq-china-list) 项目，起到分流的作用，国内的常用域名送到国内的公共 DNS 服务解析，除此之外走加密的 DNS 服务。

不想装 `pihole` 的话，可以使用 [anti-AD](https://github.com/privacy-protection-tools/anti-AD) 项目里的 [anti-ad-for-dnsmasq.conf](https://anti-ad.net/anti-ad-for-dnsmasq.conf) 配置文件……





### Dnsproxy

转发到上游的 dot 、 doh 请求。

项目地址在 [这里](https://github.com/AdguardTeam/dnsproxy ) ，而且似乎没有提供服务文件，所以得用 `screen` 挂着了。

基本上能用的 dot  、 doh 服务器被 [封](https://www.solidot.org/story?sid=67104) 的差不多了，能用的 只有 `Cloudflare` 的了，如果对无日志不在意的话可以用腾讯云的 `dnspod` 。





## 总结

到此，一个长长的 dns 链条就形成了：

```
手机 -- dot 请求 --> coredns --> pihole -- 屏蔽或放行 --> dnsmasq -- 分流 --> 国内列表/列表外 --> dnsproxy --> dot/doh 服务器
```

如果不贪恋 `pihole` 的控制面板的话，它的功能完全可以交给 `dnsmasq` 的。

只是没有控制面板的话，查误杀之类的事就会很麻烦……

搭好后应该使用抓包程序查看数据包，判断设备到服务器是否 套上了 tls ，服务器查看日志，看是否走了加密 dns 服务。
