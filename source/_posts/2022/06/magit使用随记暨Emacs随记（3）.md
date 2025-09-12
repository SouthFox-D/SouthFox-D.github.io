author: SouthFox
title: Magit使用随记暨Emacs随记（3）
date: 2022-06-08 14:38:26
tags: Emacs, Git
category: 技术
---

`magit` 是个好东西。

,(read-more)

`Magit` 是一个 `Emacs` 的包，将一大堆长又长的 `Git` 指令封装成了按键操作，对于受够了了在终端下按够了 `TAB` 键的人来说，是一种解放，无怪 `Magit` 的评价在整个 `Emacs` 生态里评价那么好了。

## Git

`Magit` 是建立在 `Git` 上的，意味着要使用得先对 `Git` 有一定了解，[Pro Git](https://book.douban.com/subject/27133267/) 是一本不错的入门书，而且做为一本「开源」书籍，也很容易在网上找到可在线浏览的方式。

### 奇怪比喻

`Git` 的精髓在于它的设计，工作区、暂存区以及版本库，用 `Git` 进行工作一般如下所示（图出自 Pro Git）。

![](https://media.southfox.me/ipfs/bafkreidcpqxgpknpmc5nmtjqzulncjwfdauiorlt6lcggxfswjh36rjelq)

用小学生写作业做比喻的话，草稿纸即是工作区，做好相关计算后把结果誊抄到作业本上，作业本既是暂存区（`Index`、`Stage`），最后确认无误后即可把作业本放入书包，书包既是版本库了。

`Git` 的设计给了使用者操作上的灵活，却也让人感到冗长，设置别名可以缓解这种情况，不过还是没有优化到单键操作的 `Magit` 偷懒偷得多。

## Magit

### 一般流程

在 `Doom Emacs` 下，使用 `SPC g g` 即可选择一个 `Git` 仓库（如果已经打开了一个 `Git` 仓库的的文件即可直接打开当前仓库下的 `Magit` 界面）打开 `Magit` 界面。

![](https://media.southfox.me/ipfs/bafkreiele2ihm55y4rbemvz4dojlnukapibj4zn5isssj2x5huqsq76qdq)

在 `Magit` 界面下，可以很方便看到工作区发生变动的文件（Unstaged changes），以及已经加入到暂存区的文件（Staged changes）。

![](https://media.southfox.me/ipfs/bafkreicttv7rkuzq5ishn6x3lix4rwdtkka7dubm5rjh67teehgcbh7nzy)

光标在某个文件上时，按下 `TAB` 即可展示文件的变动，按下回车即可直接跳转到文件进行编辑，按下 `s` 即可将文件暂存，对处于暂存区文件按下 `u` 即可取消暂存（`S` 和 `U` 则是应用于全体文件）。忘掉 `git add xxx` 吧，谁会怀念它？

确认暂存区的改动后即可进行提交（commit）将其送入版本库了，对应的快捷键是 `c`，对于很多复杂的 `Git` 操作附带了很多选项来完成，`Magit` 把能用的操作都罗列了出来，做到了看菜下饭。

对于提交（commit） 操作，其里面常用的有：

- `- s` 或 `- S` 附上签名消息或使用 `GPG` 进行签名认证。
- `c` 进行 `commit` 操作，之后会打开一个新区域输入消息，输入完毕后用 `C-c C-c` 进行确认（`C-c C-k` 为取消提交）。
- `a` 进行 `--amend` 操作，即为重新进行提交操作，适用于提交完了才发现漏掉了几个文件没有添加，或者提交信息写错了的情况。

实际用习惯了之后基本就是无脑操作了，比如使用 `p p` 快捷键，即可直接推送到默认的分支上，如果要进行其他操作比如推送到其他分支上，那么按一下 `p` 之后再看信息提示进行操作也不会太难。

对于更多操作，按下 `?` 即可进入提示菜单，或是翻阅 `Magit` 的[手册](https://magit.vc/manual/magit.html)了解更多操作。

### 从实战到跑路

现在来说说我拿 `Magit` 实际干过的事吧，

首先是要心动，看上了其他分支的功能，打算抄抄作业，那么第一件事就是把对方仓库克隆下来。在 `Magit` 界面按下 `M` 即可进入 `Remote` 界面，按下 `a` 添加，输入名称以及仓库地址来添加一个 `Remote` 。

接着开始检索对方仓库的提交，检出自己需要的提交，按下 `l` 打开 `log` 界面，按下 `- F` 搜索提交消息，之后输入想检索的提交消息，例如 `exclusive list`，再按下 `o` ，选择搜索其他（other）分支的历史记录，选择之前添加的 `Remote` 仓库的分支。

![](https://media.southfox.me/ipfs/bafkreiduuqcpezsvunsggij4gfahdn53kxiacwewsbmciglk62v44yoane)

可以看到搜索出了相关的三条结果，从最旧的开始合并，将光标移动到最旧的提交，按下 `A A` 进行一次 `cherry-pick` 。

![](https://media.southfox.me/ipfs/bafkreifkslyzgyvcm5zkapevoxkrnkdubh7uwbwwjjpjrkmwur43k4ji7i)

啊啊啊……！一大堆冲突，毕竟这次的提交很老旧了，是三年前的提交，发生变化很正常……

感叹一句没有银弹，`Git` 对于冲突也从不僭越，会去自作主张搞定。所以只能自己上了，对于这种老旧的提交，可以前去 `GitHub` 的那次提交消息里查看改动了哪些文件，再前去最新版本文件下仔细比对最新的样子是什么。当然 `Magit` 同时也提供了一个命令 `magit-log-buffer-file`（`SPC g L`）查看当前文件下有那些提交，

![检出更改](https://media.southfox.me/ipfs/bafkreied5lnnuofc7isxw3ufrg6j7gbs4ptzs27pkuxq54tcicnltl3k5e)

不过用这种方式查看双方提交记录的话，得先 `Stash` 修改再切换到对方分支查看历史记录又切换回来，感觉挺麻烦……所以我就先用 `GitHub` 查看历史了，或许有更快速方便的方法？

总之，确定该怎样合并后即可进入 `Unstaged changes` 上的任一文件按下 `E` 打开 `Ediff` 选单，再按下 `m` 解决冲突。

![](https://media.southfox.me/ipfs/bafkreie5it6wivelcnp63l34epa377lijrksptmodz7nwsb7325n4g4zsq)

进入冲突选单后，用 `n` 键跳转到下一冲突处（`p` 为上一个），接着按下 `a` 或 `b` 选择保留那一方的修改，这次的提交要结合两者，所以首先选择一方，然后用 `SPC w k` 将焦点移动到上方文件编辑区进行编辑，编辑完后再用 `SPC w j` 将焦点移回 `Ediff` 区。处理完全部冲突后，按下 `q` 退出，再处理其他文件的冲突。

处理完全部冲突后，按下 `c c` 进行一次提交，之后按下 `p p` 推送到 `GitHub` 上，祈祷部署能够成功吧！

如果想要合并的提交不是那么久远的话，可以用 `Y` 按键对应的 `Cherries` 操作来检出自己想要的提交，虽然我没用过（
