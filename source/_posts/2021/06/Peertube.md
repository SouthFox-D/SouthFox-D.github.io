author:  SouthFox
title: 在自己电脑上搭建 Peertube
date: 2021-06-17 20:48:43
tags: 技术, 建站 
category: 技术
toc: true
---

Peertube 是一个自由、去中心化、邦联制运作的视频平台。

<!--more-->

具体的介绍可以看视频：

<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts allow-popups" title="What is PeerTube?" src="https://framatube.org/videos/embed/9c9de5e8-0a1e-484a-b099-e80766180a6d?subtitle=zh" frameborder="0" allowfullscreen></iframe>

比起搭建在托管服务器上，搭建在自己电脑上可以有诸多好处，最大的好处就是存储空间和管理的自由。当然服务的运行会随着电脑的休眠而暂停，要是想一直运行的话得不间断开机……（当然也可以使用树莓派、 NAS 的方式来做到不间断开机）



## 准备

### 公网 IP

想要将自己电脑上的服务开放至整个互联网，公网 IP 是大前提，这一点请咨询运营商，有一些比较宽松，有一些就很扯皮了。不过珠三角地区电信运营商可以在宽带帐号 at 后加 `pub.`，重启猫后就是公网 IP 了。

当然，也可以使用 IPV6 ，不过 2021 年，IPV6 的普及率也不足百分之三十。就算自己的服务能够建起来，能访问的地区也是少数，所以还是满怀希望的等待吧！ 

拿到公网 IP 后，如果是用了 WIFI 模式或者有路由器的，得先在设备面板里启用端口映射，才能确保访问。



PS：国情所在，就算有了公网 IP ，也是封了 `80`,`443`,`8080`这些端口的，所以得使用不常用的高位端口。



### 域名

家用的宽带就算拿到了公网 IP ，也会不定时更换地址，所以要固定访问地址和配置 HTTPS 首先得需要一个域名 ，当然要是不计较这个的话，这一点可以略过。但是这种方式搭建出来的实例适合运行在内网上，因为  “**PeerTube does not support webserver host change**”（不支持域名改变），第一次运行之后再次更改域名将导致报错……

域名可以去域名商寻找，当然也可以使用免费域名，最主流的是使用 [Freenom](https://www.freenom.com/zh/index.html?lang=zh) ，可以免费申请到 .cf .ga .tk 三种类型的域名。



### DDNS

之后就是将公网 IP 绑定到域名上， Cloudflare 关闭了 .cf .ga .tk 使用 API 更改解析的方式，要更改得去网页面板更改。所以得用别家的，我使用的是 [Vultr](https://www.vultr.com/docs/how-to-setup-dynamic-dns) 。



## 安装

根据官方的 [文档](https://docs.joinpeertube.org/) 走就是了，搭建一般都是在 Linux 下， Win 系统可以尝试用 `WSL` ，我采用的方式是用 Docker 安装，毕竟这样不容易搞乱原本日常用的环境。

不过在使用 certbot 镜像时，注意将命令替换成 `--preferred-challenges dns -d example.com` 因为国内家用宽带无法拿到 443 端口，所以得用 DNs 方式申请域名。

### 坑

安装时倒是有一些坑陷了好久……

第一点就是端口要配置成一致的，不能容器和外面的不一样，要不然会因为不一致而报错。

但是就算这样也会导致 `Cannot retrieve OAuth Client credentials: undefined. Ensure you have correctly configured PeerTube (config/ directory), in particular the "webserver" section.` 错误，解决方式是在官方 nginx 模板 `location @api` 处 `proxy_set_header Host $host` 后面添加 `:所使用的端口` 才能解决问题[参考]（https://github.com/Chocobozzz/PeerTube/issues/3608）。



第二就是修改了 Docker 配置文件之后不自知，导致一直报错，非常脑残……所以有时候得全面检查配置文件才行…… 



## 总结

期待数据自主的那天吧！不用忍受寡头背后叔叔的气，希望去中心化的互联网的环境到来……
