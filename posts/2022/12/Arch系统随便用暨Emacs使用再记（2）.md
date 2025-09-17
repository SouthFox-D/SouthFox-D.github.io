author: SouthFox
title: Arch 系统随便用暨 Emacs 使用再记（2）
date: 2022-12-3 20:00:00
tags: 系统, Arch, Emacs
category: 技术
toc: true
---

我还挺喜欢用 `Manjaro` 的，就喜欢这种低人一等的感觉。

,(read-more)

反正系统相关的我也不懂啦，用着玩的，不想对这些 `Linux` 发行版使用什么个排序算法争出个最优。用 `Manjaro` 单纯就是安装方便同时教程好找点……不过最近（二十天前），当我更新包时突然开始报错「`libssl3.0` 未找到。」去网上搜发现五六天前 `Manjaro` 论坛就有此问题，但也只是捕风捉影并没有实际解决方案。

之后电脑新开一些应用已经打不开了，在终端里也只会一直显示库错误，估计系统已经成为一个僵尸了，一旦重新启动估计就会立马爆炸吧（重新启动果然如此，连终端环境都进不了）。

没办法，重装吧！对于这种问题向来我都是重装解决一切问题的，心想这次还是不要用 `KDE` 了，试试其它桌面环境吧，结果没想到 `Manjaro` 系统安装程序会卡在最后一步安装系统引导上（现在想来应该是智熄操作没有选到 `UEFI` 选项导致的）。

## Arch 随便用

### 懒不动

那么既然已经装不下了那就换个口味吧，换成更上游的 `Arch` 。之前一直没用 `Arch` 的原因就是系统安装没有提供一个图形化界面，就觉得很麻烦，不过查 `wiki` 时发现现在会附带一个 `archinstall` 程序辅助安装，那么就尝试用用看吧，能这么惬意决定重装也是因为大部分数据都是放在另一快数据盘上，是和系统盘分开的。

第一时间就发现的是 `Arch` 的镜像挺小的，1G 还不到，但之后就发现为什么是这样了。因为只包括了基本的组件，要成为一个能用系统的东西还得从网上下，在运行 `archinstall` 还要需求联网还卡住了几分钟的我才顿悟到……

不过说是辅助安装但也只是一个选项菜单而已，方便选择磁盘之类的，不过总比没有好吧，选好系统环境、额外包、挂载目录进行安装……最后报错识别不了另一个盘的某个分区，又不是在安装的盘上为什么还要去管其它盘上的事啊，重试了几次后依然卡在这里，没办法，想懒懒不动了，只好自己手动装了。

### 随便装

手动装就一步一步自己慢慢盯着了，看着 `wiki` 再顺便开几个教程帖，之后慢慢对着下来，无非就是选择分区然后挂载点之类的（我这种大懒狐一个 `/`  就够了），然后选择速度快的镜像站把基础系统组件还有网络管理包之类的装上去就完事了。

之后 `unmount`，重启，噔噔噔……新系统就来了，不过没有选择桌面环境所以显示的还是挺「刻板印象里的黑客风」黑黢黢命令窗口。桌面环境想了想还是选择 `GNOME` 吧，没错，我就喜欢卡又多 `BUG` 低人几等的感觉，只能说习惯的力量还是惊人的，本来其实还想体验 `i3` 环境来着但是到现在一次都没打开过（

安装重新登陆后，一个毛坯房 `GNOME` 环境就出来了，看看系统占用，发现真的挺轻量的，内存占用在 1.5G 左右，只有 `Manjaro` 一半左右。感想就是 `Minecraft` 热门整合包和自己配整合包吧，现在自用的这款系统还是有点缺东少西，比如最重要的代理和 `Emacs` 了。

### 随便折腾

代理的事因为相关软件挂在 `yay` 上，但是安装 `yay` 要自己构建下一大堆 `GO` 相关东西而又需要代理环境，经典先有鸡还是先有蛋了属于是，不过好在可以用其它设备开放的代理环境救救急，这时候才发现 `Manjaro` 默认把 `yay` 集成到了自己的源中是有理由的……

安装 `yay` 后就能愉快的继续偷 `Arch` 的……嘶，我现在就是 `Arch` 系统了，所以是光明正大来用！

#### 编辑器

首先是使用 `neovim` ，`Emacs` 这位大爷还是等所有东西都搞定了在把祂请过来吧。

#### 字体

```shell
sudo pacman -S noto-fonts-cjk
```

庞大的汉字在计算机世界还是要折腾一番的，不下中文字体开个浏览器就全是口口口的豆腐框了，所以要 `noto(fu)` 字体来拯救一下。

#### 输入法

好在在之前系统自爆前把 `Rime` 的配置文件抢救了下来，所以直接使用相应包：

```shell
sudo pacman -S fcitx5-im fcitx5-rime
```

然后编辑 `/etc/environment` 文件，添加以下几行

```shell
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS=@im=fcitx
SDL_IM_MODULE=fcitx
GLFW_IM_MODULE=ibus
```

然后重新登录就好了，反正 [ArchWiki](https://wiki.archlinuxcn.org/wiki/Fcitx5) 是这么说的，之后把之前备份的 `Rime` 配置覆盖回 `~/.local/share/fcitx5/rime` 里就好了，使用的方案是 [双拼自然码方案](https://github.com/mutoe/rime)和 [rimerc](https://github.com/Bambooin/rimerc) ，毕竟没有网络词和大公司的算法加持，想要加快输入速度只能从自己的手法上入手了。

#### 按键绑定

```shell
sudo pacman -S gnome-tweak-tool
```

突然发现 `Gnome tweak` 里面的 `Keyboard & Mouse` 里有一个 `Emacs Input` 选项，打开它后就能在操作系统里实现 `Emacs Keybinding` 了，就 `C-a` 将光标移动到行首这种的，真实 `Emacs EveryWhere` 了。然后 `/usr/share/themes/Emacs/gtk-2.0-key/gtkrc` 文件是具体的配置，可以让猫猫来看具体定义了哪些按键（我是指，`cat` 命令……）

#### 窗口切换

```shell
sudo pacman -S gnome-shell-extensions
```

然后在 `gnome-extensions` 开启 `Window List` 选项，这样就能像 `Win` 系统一样显示一个任务栏在窗口底部，如果这不是默认启用的我真想不到平常使用 `GNOME` 是怎么切换窗口的？难道这是 `Win` 系统带来的我永远摆脱不了的烙印？

#### 钥匙环儿

```shell
sudo pacman -S gnome-keyring
```

我一般使用 `Nextcloud` 的桌面客户端来同步数据的，如果缺少这个就会无限首次重新需求登录，似乎 `Minecraft` 启动器也会有这个问题，所以还是得装的。

#### 其它

其它的有了 `yay` 也挺容易装了比如 `oh-my-zsh`  之类的，看情况调个顺眼的用用,

其中 `keyd` 这个应用可以改键，把 `Esc` 和 `Caps` 调换之类的，虽然 `GNOME` 里有什么组件能改不过我也是路径依懒惯了……

## Emacs

终于最后就是把这位神请回来了，

```shell
sudo pacman -S emacs-nativecomp ripgrep
```

虽然完全搞不懂但是听说加了 `nativecomp` 会让运行速度有改善的样子，之后克隆下来 `doomemacs` 的配置仓库，将之前的备份过的配置文件放入 `~/.doom` 里，然后开始安装，之后就只能等了，几百个包安装编译啥的估计要登上十多分钟。

### 邮件

```
yay -S mu isync
```

参照[这篇文章](https://blog.lazkani.io/posts/email-setup-with-isync-notmuch-afew-msmtp-and-emacs/)完成 `mbsync(isync)` 设定，然后 `mbsync -a` 拉取邮件，然后参照[这篇文章](https://liujiacai.net/blog/2021/03/05/emacs-love-mail-feed/)完成 `mu4e` 配置（`mu4e` 在 `doomemacs` 也有相应配置可启用），然后：

```shell
export XAPIAN_CJK_NGRAM=true
# 只需要执行一次 init，可以指定多个邮件地址
mu init --my-address your-mail@qq.com --my-address your-mail@gmail.com -m ~/.mail
# index 在每次收取邮件后都需要执行，mu4e 可以配置自动执行
mu index
```

就能为邮件建立索引，之后在 `Emacs` 里使用 `Spc o m ` 即可打开 `mu4e` 界面进行使用了。

### Latex

`Emacs` 里预览公式需要 `latex`，使用

```shell
sudo pacman -S texlive-langextra
```

安装 `texlive` 发行版同时还要指定是 `extra` 包才能应付公式预览。

### Emacs everywhere

类似于空间文里的随身空间，直接在任意能输入文字的地方启动一个 `Emacs` 编辑器，这样就不用离开自己的安乐窝了。

```shell
sudo pacman -S xclip xdotool xorg-xprop xorg-xwininfo
```

然后在 `gnome` 里指定一个快捷键绑定上 `emacsclient --eval "(emacs-everywhere)"`  就能想用就用 ` Emacs` 这个随身编辑器了。
