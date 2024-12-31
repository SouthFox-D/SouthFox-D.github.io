---
author: SouthFox
title: Emacs 使用再记（3）
date: 2023-01-15 15:22:05
tags:
- Mastodon
- Matrix
- Emacs
category: 技术
toc: true
---

两种喜欢的事遇到一起，多是一件美事啊。

<!--more-->

折腾 `Emacs` 最重要就是开心，而能开心的事莫过于想实现的功能已经提前有人去实现了，这时候只要愉快导包就好了。

## Mastodon

[Mastodon.el](https://codeberg.org/martianh/mastodon.el) 是一个 `Emacs` 平台的 `Mastodon` 客户端，为什么啥事都要用 `Emacs` 来实现呢？因为只要将文字送入 `Emacs` 解析，那么就能够和其它包进行联动，这可是很诱人的事啊。

### 安装

`Mastodon.el` 也已经上传到了 `MELPA` 上面，可以直接进行安装。使用时只要指定实例地址和用户名：

```lisp
(setq mastodon-instance-url "https://social.instance.org"
      mastodon-active-user "example_user")
```

然后直接 `M-x mastodon` 运行指令，然后就会往剪贴板复制一串授权地址，粘贴到登陆的了实例浏览器进行打开，获取授权码再复制回 `Emacs` 就能完全授权了。

### 使用

当然如果是使用 `Doom emacs` 配置集的话，自带的 `vim` 按键绑定会覆盖掉 `mastodon.el` 的自带按键设定，所以还是得自己设置一下。

```lisp
(map! :leader
      :prefix ("o")
      :desc "Mastodon"          "M" #'mastodon)

(map! :after mastodon
      :map mastodon-mode-map
      :n "[ [" #'mastodon-tl--goto-prev-toot
      :n "] ]" #'mastodon-tl--goto-next-toot
      :n "g k" #'mastodon-tl--previous-tab-item
      :n "g j" #'mastodon-tl--next-tab-item

      :n "q" #'kill-current-buffer
      :n "Q" #'kill-buffer-and-window

      ;;; timelines
      :n "#" #'mastodon-tl--get-tag-timeline
      :n "A" #'mastodon-profile--get-toot-author
      :n "F" #'mastodon-tl--get-federated-timeline
      :n "H" #'mastodon-tl--get-home-timeline
      :n "L" #'mastodon-tl--get-local-timeline
      :n "N" #'mastodon-notifications-get
      :n "O" #'mastodon-profile--my-profile
      :n "P" #'mastodon-profile--show-user
      :n "T" #'mastodon-tl--thread

      ;;; toot actions
      :n "K" #'mastodon-toot--bookmark-toot-toggle
      :n "R" #'mastodon-toot--toggle-boost
      :n "c" #'mastodon-tl--toggle-spoiler-text-in-toot
      :n "C" #'mastodon-toot--copy-toot-url
      :n "o" #'mastodon-url-lookup
      :n "d" #'mastodon-toot--delete-toot
      :n "D" #'mastodon-toot--delete-draft-toot
      :n "f" #'mastodon-toot--toggle-favourite
      :n "r" #'mastodon-toot--reply
      :n "u" #'mastodon-tl--update
      :n "v" #'mastodon-tl--poll-vote

      ;;; toot!
      :n "t" #'mastodon-toot

      ;;; mastodon additions
      :n "S"    #'mastodon-search--search-query
      :n "V F"  #'mastodon-profile--view-favourites
      :n "V B"  #'mastodon-profile--view-bookmarks
      :n "V L" #'mastodon-tl--view-list-timeline
      )
```

第一处就是使用 `SPC o M` 来打开 `mastodon.el` 这个包，实现快速摸鱼，第二处设置则是照虎画猫其他人的[配置](https://github.com/coutego/evil-collection/blob/master/modes/mastodon/evil-collection-mastodon.el)来实现的，虽然没看文档（坏习惯），但是猜测 `:map mastodon-mode-map` 是选择指定那个模式下的按键配置，`:n` 是覆盖 `vim` 那个模式下的按键绑定，`n` 应该是普通模式。

按键绑定改来改去，要在不影响原本的 `hjkl` 和 `wby` 常用指令还要做到方便（自己）记忆还是有点难的，毕竟能兼容了原本按键绑定进行复制粘贴起也比较好操作。

总体来看还是倾向于一个补充，毕竟 `Emacs` 网络相关的操作确实不太行，跟 `Web`  端确实是不能比的，但是能够快速摸鱼外加和其它包联携的潜力（比如打开收藏夹进行进行分析；直接拷贝嘟文内容送入待办等），还要啥自行车呢。

## Matrix

接下来就是聊天了，其实作为和 `Emacs` 同一个时代的产品，`IRC` 自然是支持众多的，不过我已经选择另一款聊天协议了 [Matrix](https://matrix.org/) 。

### 安装

虽然也有一些包支持但是现在还在积极开发的也就 [ement.el](https://github.com/alphapapa/ement.el) 其作者 `alphapapa` 也是有名的 `Emacs` 使用者了，写了很多流行的包。

`ement.el` 现在也是上传到了 `GNU ELPA` 可以直接进行安装，但是实现起来那叫一个坑多啊，虽然大部分都是自己的问题。

安装后首先要登陆，但发现一直登陆不上去，之后排查是自己的 [Delegation](https://matrix-org.github.io/synapse/latest/delegate.html#well-known-delegation) 功能没有加上 `https://` 例如 `return 200 '{"m.homeserver": {"base_url": "synapse.matrix.org"}}'; `  ，应该是 `return 200 '{"m.homeserver": {"base_url": "https://synapse.matrix.org"}}';` 。

没有做到这一点导致 `ement.el` 发起的链接不成功，虽然很想指责一下是包的健壮性不足但姑且还是把责任揽到自己身上吧……

登陆之后发现解密不了加密消息，看说明才发现是 `ement.el` 不原生支持加密功能，想想也对，想支持得用 `ELisp` 去造轮子太麻烦了（当然我觉得主要原因是作者对于用 `Matrix` 发送加密消息没什么兴趣）。

### 配置

不过还有曲线救国方案就是用 [pantalaimon](https://github.com/matrix-org/pantalaimon) ，一个代理程序，可以将加密消息解密提供给其它不支持加密的客户端使用。

安装上可以直接用 `pip` 进行安装 `pip install pantalaimon` 之后得编辑 `~/.config/pantalaimon/pantalaimon.conf` 下的配置文件指定实例地址和相关配置。

```
[配置名称，随便填]
Homeserver = https://实例地址
ListenAddress = localhost
ListenPort = 8009
VerifySessions = False
UseKeyring = False
IgnoreVerification = True
```

注意如果启用了  [Delegation](https://matrix-org.github.io/synapse/latest/delegate.html#well-known-delegation) 功能的话实例地址得填真正使用的服务地址。后面三行是不断试错加进去的，因为这个程序真的是缺文档，所以也不清楚对实际运行有没有影响。

之后再启动程序 `pantalaimon --log-level debug` ，但是现在还没有真正运行，首先在 `Emacs` 里进行登陆并且指定使用的地址:

按下 `M-S ；` 运行指定本地反代的地址的指令：

```lisp
(ement-connect :uri-prefix "http://localhost:8009")
```

然后输入帐号密码进行登陆，看日志应该能发现有所输出，等待到数据同步后 `Emacs` 显示了房间列表后。切换到另一个已经登陆的客户端上应该能发现两个设备进行登陆，一个叫 `pantalaimon` 另一个是随机字符串是 `ement.el` 使用的。两个都显示未支持加密功能。这时先用 `Emacs` 进入一个开启加密的房间然后用 `ement-room-send-message` 指令发送一条消息，理所当然的不会成功，因为 `pantalaimon` 还没通过验证，但现在应该会显示 `pantalaimon` 设备变成支持加密的未知设备了。

这时在开启一个终端输入 `panctl` 指令进入控制，参考[这篇文章](https://www.cogitri.dev/posts/10-pantalaimon-setup/)进行验证操作 ，具体来说就是用指令 `start-verification @自己帐号:自己实例 @自己帐号:自己实例 已经登陆的设备的设备号` 发起验证，然后用那一个已经登陆的设备确实验证请求开启交叉验证……其中可能会卡住或许要多发起几次。

确实 `emoji` 匹配后就使用 `confirm-verification @自己帐号:自己实例 @自己帐号:自己实例 已经登陆的设备的设备号` 指令验证 `pantalaimon` 设备。转过头去 `Emacs` 里再尝试发起加密房间的消息查看是否成功。

### 再配置

之后如果希望将 `pantalaimon` 作为系统服务开机自启的话就发现不行，会遇到相关总线问题，因为其中一些 `pyDbus` 什么的调用导致无法挂在后台吧。

权宜之计是按照这个[说法](https://github.com/matrix-org/pantalaimon/issues/144#issuecomment-1345473181)手动编辑 `pantalaimon` 包下的 `ui.py` 文件关闭 `UI` 功能：

```py
UI_ENABLED = False
```

然后就能当作 `service` 进行管理理，代价就是无法使用 `panctl` 命令，不过只要没有再配置的需求还是能接受的，或是从一开始就使用 `Docker` 然后按照这[说法](https://github.com/matrix-org/pantalaimon/issues/144#issuecomment-1376163550)进行配置。

### 再再配置

然后记得配置

```lisp
(setq ement-save-sessions t)
```

将会话数据保留下来，要不然每次登陆都会新申请一个设备，每开一次 `Emacs` 来这样一次那很快就要达到上百个设备了。

配置此选项后关闭 `Emacs` 应该能注意到关闭 `Emacs` 会将会话数据写入，没有的话还得重启 `Emacs` 使用`(ement-connect :uri-prefix "http://localhost:8009")` 指令再登陆几次。检查 `ement-sessions-file` 变量是否有被设置，默认是 `"~/.cache/ement.el"` ，到该路径检查，有没有设置反代地址和保存了相关 `token` ，有的话就万事大吉了，之后也可以直接用 `ement-connect` 直接登陆，因为反代地址已经保存了所以也不用再去特意指定。

### 再再再配置

跟 `mastodon.el` 问题一样，`Doom emacs` 的按键绑定会覆盖掉相关绑定，不过我想将上面让人头大的解决掉按键绑定就不是什么难题了。

## 总结

两款常用服务配置下来那就挺让人舒适的了，快速刷轴摸鱼，聊天也可以在 `Emacs` 实现，主要优点是不走 `Web` 端的话，资源是比较省的，不用再加载几兆几十兆的脚本外加浏览器吃掉的内存了，两种喜欢的事加起来，多是一件美事啊。
