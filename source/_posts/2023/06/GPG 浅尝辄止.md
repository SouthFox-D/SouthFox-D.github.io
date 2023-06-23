---
author: SouthFox
title: GPG 浅尝辄止
date: 2023-06-23 22:27:06
tags: 
- GPG
- 自托管
category: 技术
fedi_url: https://foxsay.southfox.me/@SouthFox/110595035535673468
---

成为 `Geek` 众多步的其中一步，捣鼓 `GPG` (

<!--more-->

密码学让人掉头发，但也很重要，因为它是「普通人」们在赛博空间的基石。任何对「赛博自由」有兴趣的人都应该看过 `GPG` 之类的文章或书籍吧，相关的加密工具套件让蚂蚁也有了战胜大象的可能，所以 `Geek` 们乐于折腾这相关方面的东西也不足为奇了。

## 前言

话要从哪里说起呢？思来想去发现没什么想说的，毕竟对 `GPG` 感兴趣的前置条件是对 「赛博自由」感兴趣，而「赛博自由」也是一个很难说清的东西，就像空气一样。空气当然是很重要的东西，但是去卖力「推销」空气只会让人兴趣乏乏。

所以我就单纯列列最影响我对「赛博自由」这一观点的来源了：

[网络独立宣言 - 维基百科](https://zh.wikipedia.org/wiki/%E7%BD%91%E7%BB%9C%E7%8B%AC%E7%AB%8B%E5%AE%A3%E8%A8%80)

《永久记录》 - 爱德华·斯诺登

[什么是自由软件？ - GNU](https://www.gnu.org/philosophy/free-sw.zh-cn.html)

零零年左右的那些对互联网有着美好畅想的书籍 - 不管地摊书还是教材

## 公钥私钥

`GPG` 是一个加密套件，覆盖了多种加密算法和类型，但是大家一般折腾的都是「非对称加密」的公钥私钥，简单来说公钥相当于一个带锁箱子，私钥相当于对应锁的钥匙。

大家都会把公钥发布出来，然后需要进行加密沟通时就用公钥进行加密（相当于将信件转入箱子并锁上），这样只有持有私钥（相当于对应锁的钥匙）的人才能知道内容。

如果想来点小小的数学风暴可以看：

[银行密码系统安全吗？质数（素数）到底有啥用？李永乐老师11分钟讲RSA加密算法（2018最新）](https://www.bilibili.com/video/BV1Ts411H7u9/)

## 最小系统

 `GPG` 作为密码学软件让人摸不着头脑，一大堆的相关流程让人生怵，不过从繁杂的电路中拆出一个「最小系统」还是可以的，最后系统没冒烟能用下去就没关系了……大概。

`GPG` 在一众 `Linux` 发行版中应该是预装的，所以调出个终端直接用就是了，总体来说使用 `gpg --full-gen-key` 命令就能生成一个了:

- 输入 `gpg --full-generate-key` 开始生成密钥。

- 密钥类型可以选择默认选项： 1 RSA and RSA.

- 密钥长度可以使用 `4096` 来得到强壮的密钥。

- 选择失效日期，建议 `2y` （两年）。

按照提示继续设置个人信息，注意名称和邮件在公钥里是公开可见的，所以最好不要填入真名或是公开使用的邮箱。然后就输入一个密码，这个密码会在进行私钥操作的时候需要用到，所以尽量设计得复杂一点同时能记下来（写下来或是放到密码管理器里）。

生成完以后可以使用 `gpg --list-keys [生成密钥时填的邮箱地址]` 来列出具体的信息（也可使用 `-k` 来简写 `--list-keys` 选项）：

```shell
# 例如 
gpg --list-keys test@outlook.com
# 可能输出以下结果
sec   rsa3072 2023-01-01 [SC]
      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
uid           [ultimate] test <test@test.com>
ssb   rsa3072 2023-1-01 [E]
ssb   rsa3072 2023-1-01 [S]
```

其中 `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX` 的 40 位字符对应的是 `finperprint` （密钥指纹），在认证其它人的公钥时会用到。

## 发布公钥

接下来就是将公钥发布出去了，具体来说用：

`gpg --export --armor [密钥指纹] > pub_key.gpg` 

就能将公钥导出为文件，其中 `--armor` 选项是导出为人类可读形式，可用可不用。

得到这个公钥之后就是将它发布出去，例如将文件发在网盘上分享；或是将其内容发在共享文档上（需要使用 `--armor` 选项）；或是发布在博客上。

或是使用 `gpg --send-key [密钥指纹]` 命令将其发在公共 `keyserver` 上，但是默认使用的公共 `keyserver` 没有可删除性，就是说无法撤销，所以要保证生成公钥时没填入敏感信息。

### WKD

当然也有一种方式是通过 `WKD (Web Key Directory)`  方式公布自己的公钥，`WKD` 的思想很简单，就是将邮件地址的哈希串放到规范的指定位置，在使用支持 `WKD` 的客户端时就会默认从对应的地址导入公钥 （`GunPG 2.1.23` 版后默认从 `WKD` 地址导入）例如 `aheinecke@intevation.de` 的邮箱地址就对应 `https://intevation.de/.well-known/openPGPkey/hu/g8td9rsyatrazsoiho37j9n3g5ypp34h` 。

要获取 `WKD` 哈希可以使用 `gpg -k --with-wkd-hash [生成密钥时填的邮箱地址]` 命令：

```shell
pub   rsa3072 2023-01-01 [SC]
      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
uid           [ultimate] test <test@test.com>
              ****************@test.com
ssb   rsa3072 2023-1-01 [E]
ssb   rsa3072 2023-1-01 [S]

```

其中 `****************` 那串字符串对应的就是 `WKD ` 哈希。

之后可以使用 `gpg --export [生成密钥时填的邮箱地址] > [WKD 哈希]` 导出为文件，然后放到对应的服务器上，如果使用 `Nginx` 可以在邮箱对应的域名（如 `aheinecke@intevation.de` 对应 `intevation.de` 域名）配置文件下这样写：

```ng
    location /.well-known/openpgpkey/hu {
        add_header Access-Control-Allow-Origin *;
        alias /var/www/html/.well-known/openpgpkey/hu;  #对应的具体路径
    }    
    location /.well-known/openpgpkey/policy { # 可能一些旧版实现会用到这个路径
        add_header Access-Control-Allow-Origin *;
        alias /var/www/html/.well-known/openpgpkey/hu;  #对应的具体路径
    }

```

之后可以在其它账户或设备上使用 `gpg --locate-key [生成密钥时填的邮箱地址]` 来测试能否导入公钥（需 `GunPG 2.1.23` 及以后版本）。

## 验证公钥

非对称加密的一个致命处就是如何保证收到的公钥真的是所发布的人呢？所以需要进行确认，最理想的做法是将公钥拷到移动设备上然后肉身跟需要交流的人碰面然后互换公钥。当然赛博空间上很难做到这一点，所以需要设立多个源进行交叉验证，例如在网盘分享公钥出来然后在博客上公布出密钥指纹；将公钥通过邮件发送然后通过电话方式通知密钥指纹等，这样才能防止可能潜在的欺骗行为。

比如我通过 `WKD` 方式发布了公钥，那么我要在博客的相关页面公布出指纹来方便他人确认。

## 以及……

选用一个支持这些操作的客户端，比如 `Thunderbird` 进行加解密操作并不算太复杂，可参见自由软件基金会写的指南：

[电子邮件加密指南 - FSF](https://emailselfdefense.fsf.org/zh-hans/)

当然 `GPG` 还有其它的用法比如签名或是文件加解密等，或是硬件设备之类的，但是这些就太掉入「兔子洞」了。

同时 `GPG` 作为一个加密中的 「瑞士军刀」为了广泛用途也带来了很多其它问题，所以其实更适合作为一个「兜底方案」。平常应该使用专门针对使用场景的设计方案例如文件加密使用 [Age](https://github.com/FiloSottile/age) ，聊天使用 [Matrix](https://matrix.org/) 等。

## 参考

- [2021年，用更现代的方法使用PGP（上）- Ulyc](https://ulyc.github.io/2021/01/13/2021%E5%B9%B4-%E7%94%A8%E6%9B%B4%E7%8E%B0%E4%BB%A3%E7%9A%84%E6%96%B9%E6%B3%95%E4%BD%BF%E7%94%A8PGP-%E4%B8%8A/)
- [GPG 密钥轮换小记 - [余光的部落格](https://idawnlight.com/)](https://idawnlight.com/2022/gpg-key-rotation-notes/)
- [Web Key Directory (WKD) / Web Key Service (WKS) what is the difference? - gunpgp wiki](https://wiki.gnupg.org/WKD)
- [Setting up OpenPGP Web Key Directory (WKD) - uriports](https://www.uriports.com/blog/setting-up-openpgp-web-key-directory/)
- [GPG入门教程 - 阮一峰的网络日志](https://www.ruanyifeng.com/blog/2013/07/gpg.html)



