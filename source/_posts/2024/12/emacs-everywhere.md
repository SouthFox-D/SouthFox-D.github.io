author: SouthFox
title: Emacs 使用再记（4）- EMACS EVERYWHERE!
date: 2024-12-31 14:30:03
tags: Emacs, 技术
category: 技术
---

离开 Emacs ，怎么可能？

<!--more-->

在移动端使用 emacs 是有能见到的好处的，例如随时随地开记一点笔记或者处理一些脚本，而且随着原生安卓移植版的推出，在安卓 emacs 里查看多媒体也成了可能的事……不过这里面需要折腾的量嘛……

## Syncthing

### 安装

在折腾了众多方案后，最终还是选择了 Syncthing 来作为同步方案，因为它是以文件的形式存在本地，同步只是修改，如果走一众网络协议在 org-roam 这种需要不断监听文件修改的应用场景会让延迟变得难以忍受。

Arch 系统的话简单使用 `sudo pacman -S syncthing` 即可，如果是用 Debian 系的话，首先要换成官方源确保安装最新版本：

```bash
curl -s https://syncthing.net/release-key.txt | sudo apt-key add -
echo "deb https://apt.syncthing.net/ syncthing release" | sudo tee /etc/apt/sources.list.d/syncthing.list
sudo apt update
sudo apt install syncthing
```

安装完成后使用

```bash
sudo systemctl enable syncthing@$USER
sudo systemctl start syncthing@$USER
```

命令将软件设成开机启动。

安卓端 Syncthing 同步使用 Syncthing-fork ，因为里面由针对移动端的优化，如果使用 Syncthing 安卓端很容易收到系统的后台管理限制导致软件冻住然后半天无法进行同步，而 Syncthing-fork 有始终同步和间隔同步的设置能应付后台管理限制严格的系统。

[GitHub](https://f-droid.org/packages/com.github.catfriend1.syncthingandroid/)

[F-droid](https://github.com/Catfriend1/syncthing-android-fdroid)

### 概览

Syncthing 会将联结起来的节点称为集群，然后通过「文件夹 ID」来标识集群内需要同步的内容，所以要保证集群内的共享「文件夹 ID」不能冲突，这点设计上就跟 Resilio Sync 这种面向公网的软件不太一样。

### WEBUI 配置

启动后在本机前往 `http://127.0.0.1:8384` 地址即可使用内置的 Web 界面进行配置，添加和配置共享文件夹都可以这么做，如果是在服务器上可以使用 SSH 端口转发将服务器的 127.0.0.1:8384 转发到本地进行操作。

### 命令行配置

以两台设备：设备A 设备B 为例子：

使用 `syncthing cli show system | grep .myID` 命令即可显示本服务上的 ID 。然后使用 `syncthing cli config devices add --device-id $设备B ID` 即可将相关设备添加到名单中。之后使用（假设需要共享的「文件夹 ID」为 org）：

```bash
mkdir -p ~/Sync/org
syncthing cli config folders add --id org --label org --path ~/Sync/org
```

即可添加一个共享文件夹，其中 id 在各个链接的设备是唯一标识的， label 为说明性内容可随便定义， path 为共享文件夹放置的路径（务必要指定否则默认指定成配置成 home 目录会造成不必要的麻烦）。

设定好后，可使用：

```bash
syncthing cli config folders org devices add --device-id $添加设备ID
```

命令将设备添加到共享设备中，设备 B 做一次相同的操作但将设备 ID 换成对方就行了：

```bash
syncthing cli config devices add --device-id $设备A ID
mkdir -p ~/Sync/org
syncthing cli config folders add --id org --label org --path ~/Sync/org
syncthing cli config folders org devices add --device-id $设备A ID
```

## 移动端 Org-mode

Org-mode 很强大，但很多功能都得依托于 Emacs 环境，而且对于 GTD 功能上的提醒和展示功能，安卓 Emacs 也很难做到，索性 Org-mode 只是些纯文件，所以可以通过安卓上 Orgzly Revived 这个 App 来解析文件并调用安卓系统来补足这方面的不足（闹钟、通知提醒；桌面小组件展示待办）。

[GitHub](https://github.com/orgzly-revived/orgzly-android-revived)

[F-droid](https://f-droid.org/packages/com.orgzlyrevived/)

在设置了 Syncthing 同步后，移动端就有同步过后的 Org 文件了，然后可在「设置->同步->存储库->右上角加号->目录」添加一个本地目录（会解析目录下所有的 org 文件所以建议将相关 GTD 文件放到单独的目录里）。然后在「设置->同步设置」将相关的自动同步开上，因为 Orgzly Revived 设计上也是将数据记录在自己应用的数据库里，不会立马回刷到文件中。

## Emacs

Emacs 原生安卓版项目主页在（建议读读主页的描述文件的 FAQ）：

[Android ports for GNU Emacs - Browse Files at SourceForge.net](https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/)

上，里面包含了具有相同签名的 Termux 安装包（在项目主页 Termux 目录下），这样 Emacs 即可访问相同 ID Termux 里 pkg install 安装的应用，不过因为签名问题需要卸载掉原先的 Termux，所以之前有安装过 Termux 的话需要做好数据备份然后卸载装上项目里的 Termux 。

安装 emacs 好后就可启动了，当然再次之前需要给 emacs 赋予访问全部文件的权限，在较高版本安卓，文件权限可以分目录级粒度赋予，所以需要「全部文件访问权限」。如果能在系统的设置菜单找到相应配置就直接赋予 emacs, 如果不行，可以打开 emacs 后在主页菜单栏 Edit->Execute Command 里（相当于 M-x ，是的，在安卓上还是得尽量依赖菜单栏）然后输入 `android-request-storage-access` 申请获取「全部文件访问权限」。

然后接下来就是激动的配置时间了，可以在 termux 里安装一个 vim 然后编辑 `/data/data/org.gnu.emacs/files/.emacs.d/init.el`（这何尝不是一种……）或者在 emacs 里直接打开编辑 `.emacs/.emacs.d/` 里的文件。

我现在的配置文件如下：

```elisp
;; BASIC SETUP:
;; package setup - bootstrap the package system
(require 'package)
(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
        '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
        package-archive-priorities
        '(("MELPA"        . 15)
        ("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)))

(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(eval-when-compile
    (require 'use-package)
    (require 'org-capture))

;; custom
;; 防止主题信任等变量弄乱 init.el 文件
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
    (load custom-file))

;; ORG
(use-package org
    :ensure t
    :init
    (setq org-src-fontify-natively t)
    :config
    (setq word-wrap-by-category t)
    (add-hook 'org-mode-hook 'visual-line-mode)
    (setq org-ellipsis "⇣"))

(use-package org-roam
    :ensure t
    :custom
    (org-roam-directory (file-truename "/storage/emulated/0/Sync/org/Note/org-roam"))
    :bind (("C-c n r r" . org-roam-buffer-toggle)
            ("C-c n r f" . org-roam-node-find)
            ("C-c n r i" . org-roam-node-insert)
            ("C-c n r n" . org-roam-capture)
            ;; Dailies
            ("C-c n r d t" . org-roam-dailies-goto-today)
            ("C-c n r d T" . org-roam-dailies-capture-today)))

;; ui
(use-package vertico
    :ensure t
    :config
    (vertico-mode))

(use-package orderless
    :ensure t
    :init
    (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; 使用工具栏来做一些特定操作
(defun android-toggle-keyboard ()
    (interactive)
    (if touch-screen-display-keyboard
        (progn
        (setq touch-screen-display-keyboard nil)
        (tool-bar-add-item
            "disconnect" 'android-toggle-keyboard
            'android-toggle-keyboard
            :help "Toggle keyboard")
        (message "Disable virtual keyboard"))
    (setq touch-screen-display-keyboard t)
    (tool-bar-add-item
        "connect" 'android-toggle-keyboard
        'android-toggle-keyboard
        :help "Toggle keyboard")
    (message "Enable virtual keyboard")))

(defun android-tool-bar-configs ()
    (when (and (fboundp 'tool-bar-mode)
                (string-equal system-type "android"))
    (tool-bar-mode +1)
    (setq tool-bar-position 'bottom)
    (setq tool-bar-button-margin 27)
    (setq tool-bar-map '(keymap nil))
    (android-general-tool-bar 'tool-bar-add-item nil)))

(defun android-general-tool-bar (fun map)
    (mapc (lambda (args)
            (apply fun args))
            `(("left-arrow" tool-bar-item-left arrow-left ,map)
            ("right-arrow" tool-bar-item-tab arrow-right ,map)
            ("info" org-roam-dailies-capture-today org-roam-dailies-capture-today ,map)
            ("jump-to" org-roam-dailies-goto-today org-roam-dailies-goto-today ,map)
            ("up-arrow" delete-other-windows delete-other-windows ,map)

            ("connect" android-toggle-keyboard android-toggle-keyboard ,map)
    )))
(define-key key-translation-map (kbd "<XF86Back>") (kbd "C-g"))
(define-key key-translation-map (kbd "<volume-up>") (kbd "C-c"))
(define-key key-translation-map [tool-bar arrow-left] (kbd "M-x"))
(define-key key-translation-map [tool-bar arrow-right] (kbd "TAB"))

;; 在 org-capture 时动态改变工具栏的选项
(defvar capture-tool-bar-map
    (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "checked" #'org-capture-finalize 'capture-finalize map)
    (tool-bar-local-item "close" #'org-capture-kill 'capture-kill map)
    map))
(defun set-capture-tool-bar ()
    (setq-local tool-bar-map capture-tool-bar-map))
(add-hook 'org-capture-mode-hook #'set-capture-tool-bar)

(android-tool-bar-configs)

;; 为安卓开启像素滚动
(setq touch-screen-enable-hscroll nil)
(setq touch-screen-precision-scroll t)

;; theme
(use-package ef-themes
    :ensure t)
(load-theme 'ef-winter)
```

改得地方并不多，主要还是一些修改源添加一些常用包（vertico + orderlessv 真是谁用谁知道！）和用工具栏进行一些操作。说来也是感叹，在 PC 上大家都不怎么开的工具栏和菜单栏，在安卓这种很难接入键盘输入的场景下反而是重度依赖了起来。

# 参考

- [syncthing setup exclusively with CLI · GitHub](https://gist.github.com/Jonny-exe/9bad76c3adc6e916434005755ea70389)
- [Install and Use Syncthing on Ubuntu 22.04|20.04|18.04 | ComputingForGeeks](https://computingforgeeks.com/how-to-install-and-use-syncthing-on-ubuntu/)
- [在 Android Emacs 中使用 doom-emacs | 跬步](https://yuchen-lea.github.io/2024-02-04-android-emacs-with-doom-emacs/) （是的，当然也可以在安卓上装 doom emacs 但不是很推荐，因为 doom emacs 大量使用调用子进程进行安装而安卓系统不是很支持这种「玩法」导致安装在网络流畅的情况下也可能耗时半小时之多，而且相关键位在没有键盘输入情况下也难以分配）
- [用安卓native emacs+termux emacs，抛砖引玉说一下我体验emacs everywhere的个人指南 - #23，来自 DR MING](https://emacs-china.org/t/native-emacs-termux-emacs-emacs-everywhere/27135/23?u=southfox)
