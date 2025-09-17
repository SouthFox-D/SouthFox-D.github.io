author: SouthFox
title: Steam Deck 可劲折腾
date: 2025-05-26 11:40:11
toc: true
tags: 游戏, Steam Deck, Guix
category: 技术
---

> 多年以后，面对着 steam deck，我都将想起起一七一八年在 linux 为了打游戏的折腾的那个下午，什么 prime-run 啊什么调 winecfg 啊……

谁能想象 Steam Deck 就这么成为了 `echo "$(( $(date +%Y) + 1 )) is the year for Linux gaming\!"` 最有力的推手呢？

,(read-more)

### 折腾，开始！

Steam Deck 默认搭载了基于 Arch 的系统（BTW...） SteamOS ，除了一些刚出的热门「大作」和不愿兼容反作弊的游戏， Steam Deck 的运行效果已经很难让人相信是一台 Linux 游戏机了。

不过既然基于 Arch 那么就该让我鼓捣下了吧，没成想这成为了我这半年最大折腾的开始……

### 打折教父的魔法

一开始我还时把 Steam Deck 当成一个普通的 Linux 设备把玩的，首先按下 Steam 键到电源设置进入桌面环境，熟悉的 KDE 桌面铺面而来，然后就是打开里面的终端然后开始安装 Emacs （不然呢？），
然后发现 Steam Deck 首先需要设置一个密码然后关闭只读模式（那时我对不可变系统没什么概念）执行一些换源操作 balabala ，大概差不多是这个样子：

```bash
passwd
sudo steamos-readonly disable
sudo pacman-key --init
sudo pacman-key --populate archlinux
sudo pacman-key --populate holo
sudo pacman -S emacs
```

执行完后就装上了熟悉的紫薯布丁，然后顺便想着配配代理吧然后装上 yay 开始鼓捣，最后发现菜单里更新通道可以选择稳定还是预览，作为一个滚动更新享受者我就换成了预览通道，安装更新，重启之后继续用着几天我就发现不对劲了：

,(mastodon-embed https://foxsay.southfox.me/@SouthFox/112997597522720272/embed)

没成想重启之后发现所有的东西都消失了，我的紫薯布丁还有 yay 的一众配置，最后东翻翻西翻翻才发现 Steam Deck 的升级策略类似现在某些安卓手机厂商的 AB 升级策略，系统分区会制作 AB 两份，升级时处在 A 状态时只升 B 完成升级后切换成 B ，好处就是可以优雅回滚，坏处就是占用双份空间还有……配置会被刷掉。感觉就像是灰姑娘里仙女教母施展的魔法，过了零点，所有东西都将回归原样。

### 感觉不太能开箱的箱子

这可就傻眼了，没办法只好去找找替代方案了：

,(mastodon-embed https://foxsay.southfox.me/@southfox/113061467070774840/embed)

然后发现新版本的 SteamOS 内置了 distrobox 这个程序可以用容器建立一个系统在里面执行一个程序，折腾来折腾去发现 distrobox 里带 systemd 的系统总有些问题，最后发现既然 distrobox 依托于 podman 为什么不直接在外面操作 podman 呢？

,(mastodon-embed https://foxsay.southfox.me/@SouthFox/113091661012546611/embed)

但最后发现 podman 也有些问题，首先就是 podman 即使设置为 `restart-policy=always` 的 pod 在开机时也不会拉起，因为这是由一个 systemd service 执行的，而这个 service 也是 steamOS 的一部分，即使 enable 后在系统更新后也会回归原样（那个时候还不晓得 `/etc/systemd/system` 的目录是不受升级策略影响的）……

折腾到最后发现其实也不需要 root 权限执行代理程序，因为 Steam Deck 在配置菜单里可以指定代理至少让 Steam 的流量全走指定的代理，同时还发现 `/home` 目录不受更新影响，然后发现 podman 也带了一个快要被废弃的导出 systemd 配置生成功能，那就放到用户级别的 systemd service 文件到 `~/.config/systemd/user` 里设成 enable 就能开机自动运行了，大概像这样：

```bash
podman create --name proxy xxx:latest
podman generate systemd --restart-policy=always -t 1 proxy > ~/.config/systemd/proxy.service
systemctl --uesr daemon-reload
systemctl --user enable proxy.service
```

就这样，一切都很美好。

### 地下室光头你害人不浅

但美好总不能长久，因为那段时间以撒的结合推出了联机更新，了解里面联机用的是 P2P 模式所以就动了用 Zerotier 组 VPN 来方便联机的念头，联机效果很不错但发现一个严重问题就是 Zerotier 需要 Root 权限……

虽然求快直接使用 pacman 安装的方式，但这总会消失（`/usr/bin` 目录难逃更新毒手），所以一边玩着以撒一边寻找方案，最后在互联网搜索到一篇文章 [Installing ZeroTier on Steam Deck](https://wimpysworld.com/posts/install-zerotier-on-steamdeck/) 里面提到了 [rwfus](https://github.com/ValShaped/rwfus) 项目。里面有一些脚本可以保持一些应用逃出更新策略，但这个项目基本就是很多 bash 黑魔法同时项目主页提到了不能执行 `sudo pcaman -Syu` 否则系统绝对会出问题，在 issues 里也看到了相应的案例，所以对直接用这个项目还是有点发怵。

但这些项目也同时推荐的 nix 这个选择，也看到了一些消息高版本 SteamOS 放行了 /nix 目录方便用 nix 安装，所以……

,(mastodon-embed https://foxsay.southfox.me/@SouthFox/113311627550276261/embed)

### Nix ，你是？不能忘记的人……

所以最后就是鼓捣上了 Nix ，对 Nix 的印象还是来源查 Guix 资料顺带了解的，那时因为要用到 Guix 里的一些软件装上了 Guix 但只是浅尝辄止只当作简单包管理器来用。只晓得跟 Arch 一样教徒众多然后用一种「奇奇怪怪」语法写配置文件。

在安装 Nix 时就遇到了很多问题，不停通过官方脚本直接安装不停调参数安装最后发现直接用相应项目 [DeterminateSystems | nix-installer ](https://github.com/DeterminateSystems/nix-installer) 来是最合适 Steam Deck，同时很多问题都是网络问题，在前面弄个代理就少了很多问题。

之后用 Nix home manager 加上当祈求者「求」着用 Nix 的好友帮忙下攒起了一份配置，但是 Nix home manager 只是针对用户级的配置，对于要用到 Root 权限的程序还是很困难，不过在 [system manager](https://github.com/numtide/system-manager) 这个项目的帮助下生成一些 service 文件到 `/etc/systemd/system` 目录并调用 `/nix/store` 下可执行文件的方式迂回实现了想要的功能。[配置仓库地址](https://git.southfox.me/southfox/.deck)

就这么用了几个月虽然甜蜜但还是有点摩擦：

- nix 对于[纯洁」的坚持像是到了洁癖的地步，因为换成了使用配置文件 cli 系的程序，不想用 sops 程序将加密后的配置放在仓库里。 nix 的配置是从 git 的 worktree 检出来的，没有 staged 的文件在后续是不能读到的。最后一种曲折的方式实现：在开始通过有值的配置文件替换准备好文件的模板，插值进去准备好后再 switch flake ，最后 git restore 恢复配置模板文件。

- system manager 的逻辑是在生成 `/etc/.system-manager-static` 文件夹然后再链接到 `/etc/systemd/system` 等目录，而从两三个月前 `/etc/.system-manager-static` 好像也被升级策略制裁了导致撑不过系统升级，最近这些个月我还要在系统升级后进入桌面环境调用终端在 system manager 手动部署一次，感觉还是用着 nix 跟没用一样。

### Guix ，这会是最终的道路吗？

在折腾 Nix 的同时 Guix 也没有放下，在慢慢用 Guix home 攒配置还有开发一些功能后，好感度慢慢加到终于触发了剧情，在这几天终于决定踢在 Steam Deck 踢掉 Nix 换成 Guix 。因为 Nix 设计上并不是一个完备的语言，连 system manager 这种扩展功能还要使用 Rust 来写，还有 Nix 对于纯洁的坚持对我也太难容忍了。

所以在参考了一篇讲述如何安装 Nix 的博文 [Nix on the Steam Deck](https://determinate.systems/posts/nix-on-the-steam-deck) 照虎画猫的形式弄了下，遵循相同的逻辑在 /home 目录建立目录存放文件然后在 guix-daemon 通过依赖关系 mount 到真实目录，脚本大致这样：

```bash
mkdir /home/gnu/store -p
mkdir /home/var-guix

cat > /etc/systemd/system/guix-directory.service <<EOF
[Unit]
Description=Create a `/gnu` directory to be used for bind mounting
PropagatesStopTo=guix-daemon.service
PropagatesStopTo=gnu-store.mount
PropagatesStopTo=var-guix.mount
DefaultDependencies=no

[Service]
Type=oneshot
ExecStart=steamos-readonly disable
ExecStart=mkdir -vp /gnu
ExecStart=chmod -v 0755 /gnu
ExecStart=chown -v root /gnu
ExecStart=chgrp -v root /gnu
ExecStart=mkdir -vp /var/guix
ExecStart=chmod -v 0755 /var/guix
ExecStart=chown -v root /var/guix
ExecStart=chgrp -v root /var/guix
ExecStart=steamos-readonly enable
RemainAfterExit=true
EOF

cat > /etc/systemd/system/gnu.mount <<EOF
[Unit]
Description=Mount `/home/gnu` on `/gnu`
PropagatesStopTo=guix-daemon.service
PropagatesStopTo=guix-directory.service
After=guix-directory.service
Requires=guix-directory.service
ConditionPathIsDirectory=/gnu
DefaultDependencies=no
RequiredBy=guix-daemon.service

[Mount]
What=/home/gnu
Where=/gnu
Type=none
DirectoryMode=0755
Options=bind
EOF

cat > /etc/systemd/system/var-guix.mount <<EOF
[Unit]
Description=Mount `/home/var-guix` on `/var/guix`
PropagatesStopTo=guix-daemon.service
PropagatesStopTo=guix-directory.service
After=guix-directory.service
Requires=guix-directory.service
ConditionPathIsDirectory=/var/guix
DefaultDependencies=no
RequiredBy=guix-daemon.service

[Mount]
What=/home/var-guix
Where=/var/guix
Type=none
DirectoryMode=0755
Options=bind
EOF

cat > /etc/systemd/system/ensure-guix-symlinked.service <<EOF
[Unit]
Description=Ensure Guix related units which are symlinked resolve
After=gnu-store.mount
After=var-guix.mount
Requires=guix-directory.service
Requires=gnu-store.mount
Requires=var-guix.mount
DefaultDependencies=no

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/bin/systemctl daemon-reload

[Install]
WantedBy=sysinit.target
EOF

systemctl daemon-reload
systemctl enable --now ensure-guix-symlinked.service.service

wget 'https://git.savannah.gnu.org/gitweb/?p=guix.git;a=blob_plain;f=etc/guix-install.sh;hb=HEAD' -O guix-install.sh
# guix 安装时会在 /usr/local/bin/guix 创建 guix 软链接，虽然系统更新后
# /usr/local 下的目录会被重置掉，但现在暂时 disable readonly 来「欺骗」安装脚本
steamos-readonly disable
# 使用 GUIX_ALLOW_OVERWRITE 环境变量来强制向已存在的 /gnu 目录安装
GUIX_ALLOW_OVERWRITE=t bash guix-install.sh
steamos-readonly enable
```

安期间因为不明原因导致安装脚本报错，这个时候重新安装得先 `systemctl stop gnu-store.mount` 关掉 `/gnu/store` 的只读挂载然后重新试一次。装完成后可以通过 `ls /gnu/store | wc -l` 和 `ls /home/gnu/store | wc -l` 对比两边的数据是同步且挂载是有效的。安装后最好通过 reboot 命令手动重启而不要通过 Steam Deck 的更新重启，因为通过 Steam Deck 的更新重启后我发现之前的对 `/etc/systemd/system` 的修改全都消失了（这就是为什么会有上面这个脚本），看起来 Steam Deck 的更新流程有点奇怪的魔法，之后通过再试一次然后通过 reboot 的方式才看到修改的内容留着。

最后的问题就是在 `/usr/local/bin` 路径会被清掉所以之后要用到 guix 程序时替换成绝对路径 `/var/guix/profiles/per-user/root/current-guix/bin/guix` 或使用别名，另外在执行 `guix pull` 完成后还发现了 `/var/guix/profiles/per-user/root/current-guix` 并没有指向最新 profile 的问题（问题，问题还是问题！），出现这种情况得自己手动改动下。

### 最终话！（希望如此）

在安装完 guix 后在通过 scheme 和一些 gexp 「术法」就能生成一个…… systemd service 文件（没办法， systemd 的淫威实在太大了）来调用 `/gnu/store` 下的二进制文件就能做到跨更新保持一下自定义配置了，不过还是感叹 Steam Deck 以一种诡异的方式推了我一把来去学习相应知识，同时在半年前对「在 Steam Deck 使用 Guix 作为包管理器」搜遍互联网毫无头绪到现在暴力硬怼实现也是感概自己的进步。

在可劲折腾的最后却发现自己没什么动力打开游戏了，估计是这几天一直在玩「折腾 Steam Deck 配置」这么款游戏有点过载了吧。;-)
