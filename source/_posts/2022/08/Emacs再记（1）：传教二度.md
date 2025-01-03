---
author: SouthFox
title: Emacs再记（1）：传教二度
date: 2022-08-16 14:17:20
tags:
- Emacs
category: 技术
toc: true
---

论都 2022 年了为什么要用 Emacs 。

<!-- more -->

`Emacs` 最早可以追溯到 `1975`年，经过多年发展诞生了许多分支，不过现在都是特指 `GNU Emacs`，时间没有葬送这款软件，直至现在也有不少人在使用它。

## Emacs 之世界观

经过将近一年的使用，我对于 `Emacs` 也有了更多的了解，索性再写一遍第（1）篇吧。

`Emacs` ，其[官网](https://www.gnu.org/software/emacs/)对它的描述是：一个可拓展的、客制化的自由（免费）文本编辑器—and more。当然这么多天用下来，我发现其实 `Emacs` 本质并不是一个文本编辑器，因为它有一片自己的世界……

举个不恰当的例子来说，其它文本编辑器里编辑文本像是挥舞武器，安装一个插件并运用其功能像是吟唱一个魔法，其底层实现方式大家都一样，之间的区别不是很大。

而 `Emacs`，表面看是挥舞武器，但实际是因为心中有战斗的决心所以才能具现一把专属武器并战斗，表面上看是吟唱魔法，但实际上是因为其所在世界是一个失落的高科技世界，吟唱魔法其实是在和空中散落的纳米单元进行沟通然后由单元实现具体效果……虽然最后 `Emacs` 和其他文本编辑器实现了同样效果，但是它们的「世界观」是有根本不同的。

## 自由

查看 `Emacs` 代码仓库就能发现，其构成代码有近六成是由一个叫作 `Emacs Lisp` 的语言构成的，`Emacs` 以其说是一款文本编辑器倒不如说是作为 `Emacs Lisp` 的运行环境，打开软件那出现的窗口和菜单，是 `Emacs` 自己不断修改自己的体现。

这样的实现方式，更是让 `Emacs` 对于用户的态度和其他编辑器有很大不同，再举个不恰当的例子来说：

其它文本编辑器就像一个人类：

- 想要其它功能的话请指定，比如沟通语言从英语切换成汉语……什么？想要克林贡语，不好意思，没有，请自行查询插件商店吧。

- 商店里没有？请自己参照接口标准自行实现一个吧？什么，你还想要修改大脑的核心功能？不可能！太危险了！
- 真想要修改大脑核心功能的话，请自行修改「源代码」去。

`Emacs`：

- 犹如一个不定形的阿米巴变形虫，时刻不停在蠕动、变形……
- 想要切换到特定功能？好啊好啊，请自行在「配置文件」里指定吧……
- 没有想要的功能？那请自行在「配置文件」里实现具体功能吧……等我重载之后就能看到新功能了……
- 想干预一个核心功能？随便……

所以对于 `Emacs` 来说，它不像其他软件的 `.ini`  ` .env` 一样，只能附加几个参数或者在 `A` 和 `B` 已有的功能切换，在 ` Emacs` 里，没有 `C` 功能，你可以自行在「配置文件」里实现，因为「配置文件」即是「源代码」，因为 `Emacs` 是在那旧时代下诞生的软件，它完全信任用户。

## 混乱

当然自由的代价就是导致了其黑魔法满天飞，大魔法师用着呼风唤雨，而学徒却看着一大堆选择摸不着头脑。

不过随着社区的聚集，现在也出现了 [Spacemacs](https://www.spacemacs.org/) 和 [Doom emacs](https://github.com/doomemacs/doomemacs) 这样的整合网上优秀第三方包和配置的配置集，新手直接使用的话可以在一开始时就能体验到 `Emacs` 生态里的大部分优秀功能。

## 低效

建立在 `Emacs Lisp` 上的 `Emacs` 其一大特征就是……慢……当然大多数场景不会感受到，但是出现了这个情况就真的有点锻炼耐心了。而且 `Emacs` 说好听点是历史悠久，难听点就是历史包袱过重了，一些上古代码散落在各处，非常高效地拖慢了 `Emacs` 在一些场景的性能让其低效（感谢 `Emacs` 让我在 2022 年体会到了多图杀猫的效果）。

## 甜美错觉

如果在多了解一点的话，可能就会发现很多人都在吐槽 `Emacs` 用着像是一款操作系统一样，收发电子邮件、记录笔记、安排待办、写代码、甚至进行聊天。因为 `Emacs` 其「万物皆文本」的哲学，代码是文本，笔记和待办也是，甚至文件列表或是菜单栏也是（文本定义九宫格.jpg），也借助于 `Emacs Lisp` 的灵活性，只要你能想到有关于文字的编辑功能，你就能够去实现。

而且这些功能都做为一个包被 `Emacs` 载入，所以这些功能还能进行联动，比如将待办里的任务作为电子邮件发送，将文件夹内的全文搜索结果裁剪为笔记，在笔记中嵌入可以执行的代码……因为这就是 `Emacs` ，虽然实现的过程中可能像是在布满乐高的地板上赤脚行走，让人痛苦不堪感到沮丧，或许在 `Emacs` 里实现还不如直接用专用软件效果来得好，但是 `Emacs` 它就是让人产生了这样的错觉：

> 我们必须实现，我们必将实现。

## 参考

[Emacs 自力求生指南 ── 前言](https://nyk.ma/posts/emacs-intro/)

[一年成为Emacs高手 (像神一样使用编辑器)](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org)

[怎么学习 Emacs ? 达到真正融汇贯通的境界? ](https://manateelazycat.github.io/emacs/2018/12/11/study-emacs.html)
