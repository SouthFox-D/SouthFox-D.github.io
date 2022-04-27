---
author: SouthFox
title: Emacs随记（1）
date: 2022-04-19 13:09:05
tags:
- Emacs
category: 技术
toc: true
---

小心，传教士来了.jpg

<!-- more -->

`Emacs`，一款诞生于 `1975` 年的开源软件，时间并没有葬送这款软件，直至现在也依然也有不少“奇怪”的人在日常使用它。

## Why?

讲为什么要使用 `Emacs` 不如先讲讲为什么不用 `Emacs`。

- 它对 `Win` 系统支持不是很友好
- 全按键操作很劝退从微机课学起电脑的“现代”信息原住民
- 如果说 `Vim` 是小众编辑器，那么 `Emacs` 更是小众中的小众，导致很多东西都要自己去翻找
- 经常出现一些让人恼火的小问题，结合上点更让人吐血

还不够劝退吗？那为什么我还要用 `Emacs` 呢？

- 它的内存占用，4G 的电脑也带的动，我是受够了套壳框架带来的吃内存大户了……浏览器 + `VS Code` 已经挤得我电脑动弹不得了。
- 它的哲学，如果说 `Linux` 的哲学是一切皆文件，那么 `Emacs` 的哲学就是对于文本的操控了，将电子邮件导入到待办事项，把即时通讯里的通天记录裁剪到收藏里，就连近些火热的双链也有人去实现……因为这些都是“文本”，所以他们就可以被 `Emacs` 各个包处理。这些功能要让其他软件去实现，非得用上不同软件拼凑不可，同时他们之间的联系也很脆弱，全看软件开发者是否开放……
- 它的智慧，虽然我现在也是摸索着用了一段时间，到现在也才是打开次数跟 `VS Code` 持平的状态，不过我也足以感受到其中的“智慧”了，它确实很适合培养所谓的“计算机素养”，哪怕用它的人中很多并不是程序员。

## Why Doom Emacs？

现在我在用的是 [Doom Emacs](https://github.com/hlissner/doom-emacs) ，他们之间的关系有点像是 `Minecraft` 和 `整合包` 之间的关系，原版生存固然足够好玩且富有深度了，但还是太朴素了，想弄点更大的乐子那自然是往里灌上一众 `MOD` 了。而配好一整套相关 `MOD` 的即是整合包了，`Doom Emacs` 在各种配置整合包里算得上是高效和……轻量？（毕竟我也没用过其他的配置）

同时其中还附带了 `evil` 这个包，可以让 `Emacs` 用上 `Vim` 的操作，想想这确实挺“邪恶”，`Emacs` 和 `Vim` 之间的“圣战”一直打得昏天暗地，谁知其中竟然分裂出一个教派结合了两方？也确实不愧于 `Doom` 之名啊……

能蹭上 `Vim` 的操作也是好事，因为 `Vim` 的相关资料比 `Emacs` 多上许多，比较好找到相关资料，也避免了 [Emacs 小拇指](https://zh.wikipedia.org/zh-hans/Emacs#Emacs%E5%B0%8F%E6%8B%87%E6%8C%87)症状。

### 安装

`Emacs` 在各种 `Linux` 发行版自带的包管理应该能轻松找到（`Mac OS` 应该也能简单安装到），安装后运行一次就会生成 `~/.emacs.d` 路径下的文件夹。

确认文件夹存在后运行：

```shell
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d

~/.emacs.d/bin/doom install
```

其中安装会到 `GitHub` 下载一两百个包，所以提前配置好一个科学网络环境是必须的……

安装结束后，运行后并且……！
### 存活下来

#### 按键缩写

大部分资料都遵循以下缩写——

| Emacs 功能键 | 缩写 | 对应键盘按键(PC/Mac) |
| ------------ | ---- | -------------------- |
| Space        | Spc  | Space(空格)          |
| Control      | C    | Ctrl / Control       |
| Meta         | M    | Alt / Option         |
| Shift        | S    | Shift / Shift        |

例如：

 `Spc .` 是先按下 `空格` 键**再**按下 `.` 键。

`M-x` 是按下 `Alt` 键**同时**按下 `x` 键。

#### 实际操作

对于入门而言，使用 `S .` 即可打开文件管理，之后就可以选择一个文件进行打开了。

`Vim` 编辑模式可能对于入门来说也是有点痛苦，不过初始时掌握 `i` 和 `:w` 也够用了。

不用特地去记快捷键，使用 `M-x` 搜索相应的命令大部分足够了，当知道自己在高频使用某个命令，那么这时候才稍微用一下快捷键并记忆就足够了。

找到自己想要的功能用兴趣做导向是最优的，`org-mode` 就是个不错的选择。

#### 参考教程

[Vim 教程 - Oeasy](https://github.com/overmind1980/oeasyvim)

[Doom Emacs 视频教程 - Zaiste Programming](https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj) [B 站搬运](https://space.bilibili.com/432142040/channel/seriesdetail?sid=432482)

[一年成为Emacs高手 (像神一样使用编辑器)](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org)

[怎么学习 Emacs ? 达到真正融汇贯通的境界? ](https://manateelazycat.github.io/emacs/2018/12/11/study-emacs.html)

## 总结

先写成这样吧，下篇大概会写点 `org-roam` 和 `org-mode` 相关配置……不过也得等我自己先折腾了，有东西能写了才能写……
