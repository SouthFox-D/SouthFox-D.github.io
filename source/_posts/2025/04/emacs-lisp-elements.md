author: Protesilaos Stavrou, SouthFox
title: Emacs lisp 原本
toc: true
date: 2025-04-15 23:55:58
tags: Emacs
category: 技术
---

> 这篇文章是一篇翻译！

这本书，由 Protesilaos Stavrou（也称 "Prot" ）撰写，提供了 Emacs Lisp 编程语言的概览。

- 官方网站: <https://protesilaos.com/emacs/emacs-lisp-elements>
- Git 仓库: <https://github.com/protesilaos/emacs-lisp-elements>

,(read-more)

## 开始学习 Emacs Lisp

本书旨在为你提供 Emacs Lisp（也称 Elisp）的宏观认知。Elisp 是你用来扩展 Emacs 的编程语言。Emacs 是一个可编程的文本编辑器：它解释 Emacs Lisp 代码并做出反应。你确实可以使用 Emacs 而无需编写任何一行代码：它已经有非常多的功能了。不过你也可以通过随时运行一些 Elisp 代码来编程它，使其完全按照你想要的方式运行，这些代码可以是你自己编写的，也可以是从其他人那里获得的，例如以软件包的形式。

用编程定制自己的文本编辑器有用又有趣。比如，你可以将你一系列重复操作组合成一个命令，然后将该命令分配给一个按键绑定：按下该键——嘭！——即可一并执行所有中间步骤。这让你更有效率并将编辑器变成一个舒适的工作环境。

编写代码有趣之处在于你如何编写代码。你不用承担任何责任，不用！你为了编程而编程。这是一种开拓视野的娱乐活动。此外你还可以培养 Elisp 技能，如果你将来选择修改 Emacs 的某些行为，这些技能可能会有所帮助。

折腾 Emacs 是体验的一部分。它教会如何对你的编辑器工作方式直抒己见。关键要足够掌握 Elisp，这样你就能在玩乐上避免花费过多时间，或者因为一些琐事导致无法正常工作感到丧气。我本人是一个爱好钻研之人，没有计算机科学或相关背景：我通过折腾编辑器、反复试验的方式来学习 Emacs Lisp。我表面上的目标是改进我某些一遍又一遍重复的小操作：我寻求效率，却发现了一些更深刻的东西。学习扩展我的编辑器是一种充实的体验，也让我变得更有效率。我对 Emacs 按照我想要的方式运行感到满意。

本书中的每一章通常都简短扼要。有些对初学者更友好，而有些则更深入探讨了高级主题。章节之间有链接，就像参考手册该做一样。你可以通过来回跳转的方式寻找你需要的内容。

你在这里看到的文章是散文和代码的结合。后者可能是实际 Elisp 代码或是描绘基本范式的伪代码。我鼓励你在 Emacs 里或在 Emacs 随时可用的情况下阅读本书。这样，你可以摆弄我给你的函数，以进一步领会它们间的细微差别。

我使用的“宏观认知”途径包含了我在使用 Emacs Lisp 时经常遇到的概念。本书不能替代 Emacs Lisp 参考手册，也不应当将任何我关于 Elisp 的诠释作为权威。

祝你好运并玩得开心！


## Emacs Lisp 求值

你在 Emacs 中做的每一件事都会调用某个函数，然后对 Emacs Lisp 代码进行求值，读取返回值并产生副作用 ([副作用和返回值](#副作用和返回值))。

你在键盘上按下一个键一个字符就被写入当前缓冲区（buffer）。这是一个绑定到一个键上的函数，它实际上是一个 _交互式_ 函数，因为你是通过按键绑定而不是通过某个程序来调用它。交互式函数被称为“命令”（commands）。不过不要让交互式这个实现细节干扰你的注意力，要知道你在 Emacs 中每一个操作都涉及 Emacs Lisp 的求值。

另一种常见的交互形式是使用 M-x (`execute-extended-command`) 键，默认运行命令 `execute-extended-command` ：它会产生一个 minibuffer 输入栏，要求你按名称选择一个命令接收后并执行。

Emacs 可以在任何地方求值 Elisp 代码。如果你的缓冲区中有一些 Elisp 代码，你可以将光标（cursor）放在末尾的右括号，然后输入 C-x C-e (`eval-last-sexp`)。同样你可以使用命令 `eval-buffer` 和 `eval-region` 分别对当前缓冲区或选中区域进行操作。

`eval-last-sexp` 也适用于符号求值 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。例如，如果你将光标放在变量 `buffer-file-name` 的末尾并使用 C-x C-e (`eval-last-sexp`)，你会获得该变量的值，这个值要么是 `nil` 要么是你文件系统中正在编辑文件的路径。

有时上述方法不适用于你想做的事。假设你打算编写一个复制当前缓冲区文件路径的命令，那么你的代码需要检测变量 `buffer-file-name` 的值 ([作为数据结构的缓冲区](#作为数据结构的缓冲区))。但你肯定不想在文件中输入 `buffer-file-name` ，然后使用前面提到的某个 Elisp 求值命令，最后再撤销你的编辑。这既笨拙又容易出错！在当前缓冲区中运行 Elisp 代码的最佳方式是键入 M-: (`eval-expression`): 它会打开 minibuffer 并接收你写入的想要求值的代码。然后键入 RET 来开始求值。求值是在把最后的缓冲区当成当前缓冲区下运行的（即调用 `eval-expression` 之前的缓冲区）。

这里有一些 Emacs Lisp 代码，你可以在 (i) 对应于文件的缓冲区 与 (ii) 与硬盘上任何文件都无关的缓冲区中尝试运行。

```emacs-lisp
;; 在打开了文件的缓冲区和没有打开文件的缓冲区
;; 求值`eval-expression' 。
(if buffer-file-name
    (message "此文件的路径是 `%s'" buffer-file-name)
  (message "抱歉伙计，这个缓冲区没有打开任何文件"))
```

当你试验代码时，你可能也想测试它的行为表现。使用 `ielm` 命令打开一个交互式 shell 。它让你进入一个提示符，你可以在其中键入任何 Elisp 代码并键入 RET 来求值它。返回值会输出到正下方。或者切换到 `*scratch*` 缓冲区。如果它使用主模式 `lisp-interaction-mode` （这是 `initial-major-mode`  变量的默认值）那么你可以在该缓冲区中自由移动并在一些代码的末尾键入 C-j (`eval-print-last-sexp`) 来求值它。这与 `eval-last-sexp` 的工作方式几乎相同，但有了将返回值放在你刚刚求值表达式下方的效果。

除此之外，你还可以依靠 Emacs 的自文档特性来知晓当前状态。例如要了解变量 `major-mode` 的缓冲区局部值（buffer-local value），你可以执行 C-h v (`describe-variable`)，然后搜索该变量。帮助缓冲区将告诉你 `major-mode` 的当前值。此帮助命令和许多其他命令，如 `describe-function`, `describe-keymap`, `describe-key` 和 `describe-symbol` 提供了 Emacs 对给定对象的洞察力。帮助缓冲区还会显示相关信息，例如给定函数的文件路径或变量是否为缓冲区的局部变量。

Emacs 是“自文档”的，因为它会报告身状态。你不需要显式更新帮助缓冲区。这在求值相关代码自动执行： Emacs 会向你展示你任何你在处理东西的最新值。


## 副作用和返回值

Emacs Lisp 有函数。它们接受输入并产生输出。在最纯粹形式中，函数是一个只有返回值的计算过程，它不会改变所处环境中的任何东西。一个函数的返回值可以用作另一个函数的输入，这实际上构成了一个计算链。因此你可以依赖一个函数的返回值来表达类似“如果这个成功，那么做另一件事，否则做其它的事甚至什么都不做”。

Elisp 是扩展和控制 Emacs 的语言。这意味着它会影响编辑器的状态。当你运行一个函数时，它可以产生永久性影响，例如在光标（cursor）处插入一些文本、删除一个缓冲区、创建一个新窗口等等等等。这些更改会影响到未来的函数调用。例如前一个函数删除了某个缓冲区，那么写入该缓冲区的下一个函数就无法工作了：缓冲区已经不见了！

当你编写 Elisp 时，你必须同时考虑返回值和副作用。如果你大意了，你会因为那些没有深思熟虑的环境变化得到意想不到的结果。但如果你仔细使用副作用，你就能够完全挖掘 Elisp 的潜力。例如想象你定义了一个函数，它有这样的逻辑：“创建一个缓冲区，转到那里，写入一些文本，将缓冲区保存到我偏好位置的文件中，最后回到我调用此函数之前的位置，同时让创建的缓冲区处于打开中。”所有的这一切都是有用的副作用。你的函数可能有一些有意义的返回值，你可以将其用作另一个函数的输入。例如你的函数可能会返回它生成的缓冲区对象让下一个函数可以在那里做些事情，比如在一个单独的窗框（frame)中显示该缓冲区然后调大文本。

这里的思想是操纵编辑器的状态让 Emacs 做你想作的事。有时这意味着你的代码有副作用，但有时副作用是没用的甚至不符合你的预期结果。随着你收获更多经验并扩充技能，你会不断精炼需要做什么的直觉 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。不用担心，放松就好！


## 作为数据结构的缓冲区

缓冲区以字符序列的形式把持数据。例如这里的数据就是你打开文件时看到的文本。每个字符都有一个数字用于表示位置。函数 `point` 给出你所在游标（point）的位置，这通常对应于光标所在的位置 ([Emacs Lisp 求值](#emacs-lisp-求值))。在缓冲区的开头， `point` 将返回 `1` ([副作用与返回值](#副作用和返回值))。有大量函数返回缓冲区位置，例如 `point-min`, `point-max`, `line-beginning-position` 和 `re-search-forward` 。其中一些会有副作用，比如 `re-search-forward` 会将光标移动到给定匹配处。

当你在 Emacs Lisp 中编程时，你经常依赖缓冲区来做以下一些事：

将文件内容提取为字符串
: 将缓冲区视为一个大字符串。你可以使用函数 `buffer-string` 获取全部内容作为一个可能极其巨大的字符串。你也可以使用 `buffer-substring` 函数和对应的 `buffer-substring-no-properties` 函数获取两个缓冲区位置之间的子字符串 ([文本具有自身属性](#文本具有自身属性))。想象一下你将其作为更广泛操作的一部分来执行，该操作 (i) 打开文件，(ii) 转到特定位置，(iii) 复制找到的文本，(iv) 切换到另一个缓冲区，然后 (v) 将找到的内容写入这个新缓冲区。

展示某些操作的结果
: 你也许有一个显示即将来临的假期函数。你的代码在后台进行计算，并最终将一些文本写入缓冲区。最终产物被显示出来。这取决于你如何处理它，你可能需要求值函数 `get-buffer-create` 或其更精确替代 `get-buffer` 。如果你需要清除已存在缓冲区里的内容，你可能会使用 `with-current-buffer` 宏临时切换到你指向的缓冲区然后调用函数 `erase-buffer` 删除所有内容，要么使用 `delete-region` 将删除范围框定在缓冲区两个位置之间。最后，函数 `display-buffer` 或 `pop-to-buffer` 会将缓冲区显示在 Emacs 窗口（window）中。

将变量与给定缓冲区关联
: 在 Emacs Lisp 中，变量可以采用为缓冲区局部值（buffer-local value），该值与其全局值不同。有些变量被声明为始终是缓冲区局部的，例如 `buffer-file-name~、~fill-column` 和 `default-directory` 。假设你在做返回指定目录中文件缓冲区列表的事。你会遍历 `buffer-list` 函数的返回值，通过检测 `buffer-file-name` 的特定值来相应过滤结果 ([`if`, `cond` 等基本控制流](#if-cond-等基本控制流))，这个特定变量始终可用。不过你可以使用 `setq-local` 宏将值赋给当前缓冲区中的变量。

后一点也许是最开放的一点。缓冲区就像一组变量的集合，包括它们的内容、正在运行的主模式（major-mode)以及它们拥有的缓冲区局部值（buffer-local value）。在下面的代码块中，我使用 `seq-filter` 函数遍历函数 `buffer-list` 的返回值 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。

```emacs-lisp
(seq-filter
 (lambda (buffer)
   "返回缓冲区当其为可见的并且其主模式派生自 `text-mode' 。"
   (with-current-buffer buffer
     ;; 对于不打算让用户看到的缓冲区，
     ;; 惯例是它们的名称以一个空格开头
     ;; 我们现在不关心这些缓冲区。
     (and (not (string-prefix-p " " (buffer-name buffer)))
          (derived-mode-p 'text-mode))))
 (buffer-list))
```

这将返回一个缓冲区对象列表，这些对象通过了以下检测：(i) 对用户“可见” 且 (ii) 它们的主模式是 `text-mode` 要么是从中派生的。上述代码也可以这样写 ([何时使用具名函数或 lambda 函数](#何时使用具名函数或-lambda-函数))：

```emacs-lisp
(defun my-buffer-visble-and-text-p (buffer)
  "返回缓冲区当其为可见的并且其主模式派生自 `text-mode' 。"
  (with-current-buffer buffer
     ;; 对于不打算让用户看到的缓冲区，
     ;; 惯例为名称以一个空格开头
     ;; 我们现在不关心这些缓冲区。
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (derived-mode-p 'text-mode))))

(seq-filter #'my-buffer-visble-and-text-p (buffer-list))
```

与缓冲区一样，Emacs 窗口（windows）和窗框（frame) 也有它们自己的参数。我不会讨论这些，因为它们的用途更为专门化，而且概念是相同的。只需知道它们是数据结构，你可以利用它们为你带来优势，例如遍历它们 ([对列表中的元素进行映射](#对列表中的元素进行映射))。


## 文本具有自身属性

如像数据结构一样工作的缓冲区 ([作为数据结构的缓冲区](#作为数据结构的缓冲区))，任何文本也可能有与其关联的属性。这是你可以使用 Emacs Lisp 检阅的元数据。例如当你在某个编程缓冲区中看到语法高亮时，这就是文本属性的效果。某个函数负责“属性化”（propertise）或“字体化”（fontify）相关文本，并决定将一个称为 "face" 的对象应用于它。Face 是将排版属性和颜色属性捆绑在一起的造物，例如家族和字重、前景和背景的色调。要获取一个光标处（cursor)文本包含属性信息的帮助缓冲区，请键入 M-x (`execute-extended-command`) 然后调用命令 `describe-char` ，它会告诉你它看到的字符、它用什么字体渲染的、它的代码游标是什么，以及它的文本属性是什么。

假设你正在编写你自己的主模式。在实验的早期阶段，你想手动将文本属性添加到缓冲区中所有出现的短语 `I have properties` 上，该缓冲区的主模式是 `fundamental-mode` ，所以你可以这样做（[上次搜索的匹配数据](#上次搜索的匹配数据)）：

```emacs-lisp
(defun my-add-properties ()
  "在当前缓冲区为 \"I have properties\" 文本添加属性。"
  (goto-char (point-min))
  (while (re-search-forward "I have properties" nil t)
    (add-text-properties (match-beginning 0) (match-end 0) '(face error))))
```

实际实验一下这个。使用 C-x b (`switch-to-buffer`)，输入一些与现在存在缓冲区不匹配的随机字符，然后按 RET 打开该新缓冲区。它运行 `fundamental-mode` ，意味着没有“字体化”（propertise）发生，因此 `my-add-properties` 会按预期工作。现在粘贴以下内容：

```fundamental
This is some sample text. Will the phrase "I have properties" use the `bold' face?

What does it even mean for I have properties to be bold?
```

继续使用 M-: (`eval-expression`) 并调用函数 `my-add-properties` 。成功了吗？它应用的 face 叫做 `error` 。忽略这个词的意思：我选择它是因为它的样式化方式通常有相当强烈和明显（尽管你当前的主题可能会有差异）。

有一些函数可以查找给定缓冲区位置的属性，还有一些函数可以向前和向后搜索给定的属性。具体的细节现在不重要。我只想让你记住，文本不仅仅是其组成的字符。要了解详情，可键入 M-x (`execute-extended-command`) 来调用命令 `shortdoc` 。它会要你选择一个文档组。选择 `text-properties` 以了解更多信息。当然，对那里列出的所有内容都使用 `shortdoc`  ，我一直都是这样做的。


## 符号-匹配括号表达式和引用

对于不熟悉 Emacs Lisp 的人来说，这是一种括号非常多的语言！这是一个简单的函数定义：

```emacs-lisp
(defun my-greet-person (name)
  "对给定的 name 说 hello"
  (message "Hello %s" name))
```

我刚刚定义了一个名为 `my-greet-person` 的函数。它有一个参数列表，具体来说，是只包含一个参数的列表，参数名为 `name` 。然后是可选的文档字符串，供用户理解代码和/或函数的意图。 `my-greet-person` 接受 `name` 并将其作为参数传递给 `message` 函数，最终打印出问候语。 `message` 函数将文本记录在 `*Messages*` 缓冲区中，你可以使用 C-h e (`view-echo-area-messages`) 直接访问该缓冲区。无论如何，下面是一个给定一个预期参数的 `my-greet-person` 调用：

```emacs-lisp
(my-greet-person "Protesilaos")
```

现在用多个参数做同样的事情：

```emacs-lisp
(defun my-greet-person-from-country (name country)
  "对给定住在 COUNTRY 的 NAME 说 hello"
  (message "Hello %s of %s" name country))
```

然后这样子调用：

```emacs-lisp
(my-greet-person-from-country "Protesilaos" "Cyprus")
```

即使对于最基本的任务，你也会用到很多括号。但不用害怕！这实际上让结构化理解你的代码变得更简单。如果现在感觉不是这样，那是因为你还不习惯。一旦你习惯了，可就回不去了。

任何 Lisp 方言 (Emacs Lisp 也是其中之一）的基本思想是：你有用来定义列表的括号；列表由元素组成；列表要么被求值产生某些计算结果，要么按原样返回以用于其他求值 ([副作用与返回值](#副作用和返回值))：

列表作为函数调用
: 当求值一个列表时，第一个元素是函数名，其余元素是传递给该函数的参数。你已经在上面看到了这一点，我是如何用 `"Protesilaos"` 作为参数调用 `my-greet-person` 的。 `my-greet-person-from-country` 也是同样的原理，参数是 `"Protesilaos"` 和 `"Cyprus"` 。


列表作为数据
: 当不求值一个列表时，它的所有元素在一开始都没有特殊含义。它们都作为一个列表原样返回，没有进一步的改变。当你不希望你的列表被求值时，你可以在它前面加上一个单引号字符。例如 `'("Protesilaos" "Prot" "Cyprus")` 是一个包含三个元素的列表并且应该原样返回。

考虑你还没见过的下一种场景。你想从包含元素的列表中获取一些数据。在最基本的层面上，函数 `car` 和 `cdr` 分别返回第一个元素和包含所有剩余元素的列表：

```emacs-lisp
(car '("Protesilaos" "Prot" "Cyprus"))
;; => "Protesilaos"

(cdr '("Protesilaos" "Prot" "Cyprus"))
;; => ("Prot" "Cyprus")
```

这里的单引号特别重要，因为它指示 Emacs 不要对列表求值。否则对这个列表求值会将第一个元素，即 `"Protesilaos"` 视为函数名，并将列表其余部分视为该函数的参数。由于你并没有定义这样的函数，你会得到一个报错。

Emacs Lisp 中的某些数据类型是“自求值”的。这意味着如果你对它们求值，它们的返回值就是你看到的东西。例如，字符串 `"Protesilaos"` 的返回值是 `"Protesilaos"` 。这对于字符串、数字、关键字、符号以及特殊的 `nil` 或 `t` 都成立。下面是一个包含这些类型的列表，你可以通过调用函数 `list` 来构建它：

```emacs-lisp
(list "Protesilaos" 1 :hello 'my-greet-person-from-country nil t)
;; => ("Protesilaos" 1 :hello 'my-greet-person-from-country nil t)
```

`list` 函数对传递给它的参数进行求值，除非它们被引用（quote）。自求值是你得到没有明显变化的返回值的原因。注意 `my-greet-person-from-country` 的引用方式与我们引用一个不希望求值的列表的方式相同。如果没有它， `my-greet-person-from-country` 将被求值，除非它被定义为一个变量，否则将得到一个报错。

将单引号视为一个明确的指令：“不要对后续求值”。更具体说，它是一个指令，让通常会发生求值的情况下不进行求值 ([列表里的部分求值](#列表里的部分求值))。换言之，你可不想在一个被引用的列表内部引用某些东西，因为那等同于对其引用两次：

```emacs-lisp
;; 这样做是正确的：
'(1 :hello my-greet-person-from-country)

;; 这样引用 `my-greet-person-from-country' 是错误的因为
;; 整个列表不会被求值，这里的错误在于你引用了已经引用的东西
;; 就像在做：
;; ''my-greet-person-from-country.
'(1 :hello 'my-greet-person-from-country)
```

现在你可能想知道为什么我们引用了 `my-greet-person-from-country` 但其他没有引用其他东西？原因是你在那里看到的其他所有东西实际上都是“自引用”的，即自求值的另一面。而 `my-greet-person-from-country` 是一个符号。一个“符号”是一个自身之外某物的引用：它要么表示某个计算——一个函数——或为一个变量的值。如果你写一个符号而不引用它，你实际上是在告诉 Emacs “给我这个符号所代表的值”。就 `my-greet-person-from-country` 而言，你尝试这样做会得到一个错误，因为这个符号不是一个变量，因此尝试从中获取值是无法生效的。

还请记住，Emacs Lisp 有一个“宏”的概念，它基本上是一个模板系统，用于编写扩展成其他实际代码然后才求值的代码。在一个宏内部，你控制了引用的如何完成，这意味着前面提到的规则可能不适用于宏的调用，即使它们仍然在宏的扩展形式内部使用 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。

随着你接触更多的 Emacs Lisp 代码，你会遇到前面带有井号的引号，例如 `#'some-symbol` 。这个 sharp quote ，正如它被称呼的那样，与常规引号相同，但增加了特指为函数的语义。程序员可以更好地表达给定表达式意图，字节编译器也能在内部执行必要的检查和优化。考虑这点，请阅读关于函数 `quote` 和 `function` 的内容，它们分别对应于常规引号和 sharp quote 。


## 列表里的部分求值

你已经对 Emacs Lisp 代码的样子有了一些概念 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。你有一个列表：它要么被求值；要么按照原样获取。还有另一种情况，即列表应该被部分求值，或者更具体地说，它应该被当作数据而不是函数调用，同时仍然需要求值某些元素。

在下面的代码块中，我定义了一个名为 `my-greeting-in-greek` 的变量，这是一个希腊语中常用的短语，字面意思是“祝你健康”，发音为 "yah sou" 。为什么要用希腊语？好吧，你已经学习了生成 Lisp 这一切的 `lambda` ，所以你不妨也了解其余的部分 ([何时使用具名函数或 lambda 函数](#何时使用具名函数或-lambda-函数))！

```emacs-lisp
(defvar my-greeting-in-greek "Γεια σου"
  "希腊语中的祝某人健康。")
```

现在我要用 `message` 函数做些实验来更好理解求值是如何工作的。让我从引用列表然后按原样获取它的情况开始：

```emacs-lisp
(message "%S" '(one two my-greeting-in-greek four))
;;=> "(one two my-greeting-in-greek four)"
```

你会注意到变量 `my-greeting-in-greek` 没有被求值。我得到了符号即实际的 `my-greeting-in-greek` 而不是它所代表的值。这是预期的结果，因为整个列表都被引用了，因此其中的所有内容都不会被求值。

现在检查下一个代码块，以理解我如何告诉 Emacs 我希望整个列表仍然被引用，但特别地让 `my-greeting-in-greek` 被求值来替换它代表的值：

```emacs-lisp
(message "%S" `(one two ,my-greeting-in-greek four))
;; => "(one two \"Γεια σου\" four)"
```

请仔细注意这里的语法。我使用的是反引号或称反撇号，而不是单引号，在我们的例子中这也被称为准引号（quasi quote）。它的行为类似于单引号，但除了前面带有逗号的内容。逗号是一个“对后面的东西求值”的指令，并且只在准引号（quasi quote）的列表内部有效。后面跟着的“东西”要么是一个符号，要么是一个列表。列表当然也可以是一个函数调用。这里让我用 `concat` 来问候某个人，同时将所有内容作为一个列表返回：

```emacs-lisp
(message "%S" `(one two ,(concat my-greeting-in-greek " " "Πρωτεσίλαε") four))
;; => "(one two \"Γεια σου Πρωτεσίλαε\" four)"
```

请记住如果你不引用这个列表就会得到一个错误，因为第一个元素 `one` 会被视为一个函数的符号，该函数将用所有其他元素作为其参数来调用。很可能在你现在的 Emacs 会话中没有定义 `one` 为函数，或者那些参数对它来说没有意义。另外， `two` 和 `four` 随后会视为变量，因为它们没有被引用，在这种情况下，那些变量也必须被定义，否则会导致更多错误产生。

除了逗号操作符，还有 `,@` （这到底怎么发音？可能 "comma at" ？），这表示为 “拼接”（splicing）。这是用来说“返回值是一个列表但希望你移除它最外层的括号”的术语。实际上，原本会返回 `'(one two three)` 的代码现在返回 `one two three` 。这种差异在单独使用情况下可能没有多大意义，但一旦你将这些元素视为应独立求值的表达式，而不是仅仅作为被引用列表的元素时，它就有意义了。我不会在这里详细阐述一个例子，因为我认为最好在定义宏的背景下进行介绍 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。

你很可能不需要用到部分求值的知识。它在宏中更常见但在任何地方都可以使用。不管怎样都要认识到它，因为在某些情况下你需要理解你所某些依赖的代码在做些什么。

最后，既然我向你介绍了一些希腊语单词，那我现在就把你当作我的朋友了。这里有一个我小时候的笑话。我试图向我的英语老师解释某个事情。由于我缺乏表达自己的词汇，我开始使用希腊语单词。我的老师有严格只用英语回应的规定，所以她说："It is all Greek to me."（对我来说都是希腊语「中文里类似表示为“对我来说都是天书”」）。我不知道她在说一个俗语，意思是“我不明白你在说什么”，我草率回答说:"Yes, Greek madame; me no speak England very best."（是的，希腊语，夫人;我英语说得不是很好）。其实我当时已经算入门了，但我还是忍不住调侃一下。就像你应该记得享受折腾 Emacs 的时光一样。但好了！回到这本书。


## 宏或特殊形式里的求值

在最基础的 Emacs Lisp 代码场景中，你有要么被求值要么不被求值的列表 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。如果你玩得更高级一点，你有只被部分求值的列表 ([列表里的部分求值](#列表里的部分求值))。但有时你看着一段代码，却无法理解为什么常规的引用和求值规则不适用。在看到实际例子之前，先检查一个典型的函数调用，它也涉及一个变量的求值：

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")
```

你在部分求值里遇到过这段代码。这里有一个对函数 `concat` 的调用后面跟着三个参数。其中一个参数是一个变量即 `my-greeting-in-greek` 。当这个列表被求值时 Emacs 实际首先对参数（包括 `my-greeting-in-greek` ）进行求值来获取它们各自的值，然后才用这些值调用 `concat` 。你可以将整个操作过程想象如下：

-   这里是一个列表。
-   它没有被引用。
-   那你应该对它求值。
-   第一个元素是函数名。
-   剩余的元素是传递给该函数的参数。
-   检查参数是什么。
-   对每个参数求值获取其真实值。
-   字符串是自求值的，而 `my-greeting-in-greek` 是一个变量。
-   你现在拥有了每个参数的值，包括符号 `my-greeting-in-greek` 的值。
-   用你得到的所有值调用 `concat` 。

换句话说，下面两个产生相同的结果（假设 `my-greeting-in-greek` 是常量）：

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")

(concat "Γεια σου" " " "Πρωτεσίλαε")
```

这是可预测的。它遵循单引号的基本逻辑：如果它被引用了，就不对它求值然后原样返回，否则就对它求值并返回值。但是你会发现很多情形下这种预期的模式似乎没有被遵循。考虑这个用 `setq` 将符号绑定到给定值的常见场景：

```emacs-lisp
(setq my-test-symbol "Protesilaos of Cyprus")
```

上面的表达式看起来像一个函数调用，这意味 (i) 列表没有被引用，(ii) 第一个元素是函数名，以及 (iii) 剩余元素是传递给该函数的参数。在某种程度上这是正确的，不过你可能会期望 `my-test-symbol` 当成一个变量，它会被原地求值以返回其结果，而这个结果反过来将是传递给函数的实际参数。然而这并不是 `setq` 的工作方式。原因是它是一个特殊情形而里面执行的是这个：

```emacs-lisp
(set 'my-test-symbol "Protesilaos of Cyprus")
```

在这里事情就符合预期。背后没有发生什么魔法。 `setq` 是为了让用户不必每次都加上引用号。是的，这使得推论它变得有点困难，但你会习惯它，最终一切都会变得有意义。希望你会习惯这种特殊形式，就像你在 `setq` 以及 `defun` 等许多其他形式中发现的那样。这有一个你已经见过的函数：

```emacs-lisp
(defun my-greet-person-from-country (name country)
  "对给定住在 COUNTRY 的 NAME 说 hello"
  (message "Hello %s of %s" name country))
```

如果应用标准求值规则，那么参数列表应该被视作引用。否则你会觉得 `(name country)` 将解释成一个函数调用，其中 `name` 是函数符号， `country` 是它的参数并且 `country` 本身也应该是一个变量。但这并不是实际发生的情况，因为 `defun` 会在内部将该参数列表视为已引用。

另一个常见的场景是使用 `let` ([`if-let*` 控制流及其相关形式](#if-let-控制流及其相关形式) )。它的一般形式如下：

```emacs-lisp
;; 这是一段伪代码
(let LIST-OF-LISTS-AS-VARIABLE-BINDINGS
  BODY-OF-THE-FUNCTION)
```

`LIST-OF-LISTS-AS-VARIABLE-BINDINGS` 是一个列表，其中每个元素都是 `(SYMBOL VALUE)` 形式的列表。这里有一些实际代码：

```emacs-lisp
(let ((name "Protesilaos")
      (country "Cyprus"))
  (message "Hello %s of %s" name country))
```

继续关于特殊形式主题，如果 `let` 是一个典型函数调用，那么 `LIST-OF-LISTS-AS-VARIABLE-BINDINGS` 就必须引用。否则它会被求值，在这种情况下，第一个元素将是函数名。这会返回一个错误，因为函数名将对应于另一个列表即 `(name "Protesilaos")` 而不是一个符号。不过使用 `let` 时一切正常，因为它内部对其 `LIST-OF-LISTS-AS-VARIABLE-BINDINGS` 进行了引用。

对于许多特殊形式和宏都有类似预期行为，比如流行的 `use-package` （可用于在你的 Emacs 初始文件中配置相关包）。这些宏是如何工作取决于它们的设计，我不会在这里深入探讨技术细节，因为我希望这本书能够长期有用，侧重于理念而不是可能随时间变化的实现细节。

要了解给定的宏会实际扩展成什么，请将光标放在末尾的右括号并调用命令 `pp-macroexpand-last-sexp` 。它将生成一个新的缓冲区，显示扩展后的 Emacs Lisp 代码。这才是宏实际替换后求值的内容。

理解这些基础后是时候来编写一个宏了。这就像一个模板，使你能够避免重复自己。在语法上宏很可能依赖于准引用（quasi quote） 、逗号操作符以及拼接机制的使用 ([列表里的部分求值](#列表里的部分求值))。这里有一个简单的场景，我们希望在一个临时缓冲区中运行一些代码同时将 `default-directory` 设成用户的家目录。

```emacs-lisp
(defmacro my-work-in-temp-buffer-from-home (&rest expressions)
  "在临时缓冲区将 `default-directory' 设为用户家目录并求值 EXPRESSIONS 。"
  `(let ((default-directory ,(expand-file-name "~/")))
     (with-temp-buffer
       (message "Running all expression from the `%s' directory" default-directory)
       ,@expressions)))
```

在这个定义里 `&rest` 会让后面的参数成为一个列表。所以你可以向它传递任意数量的参数，所有参数都将被收集到一个名为 `EXPRESSIONS` 的列表中。合理使用的部分求值确保了宏不会立即求值，而只在被调用时才求值。传递给它的参数将替换到你指定的位置。这是一个这个宏的调用：

```emacs-lisp
(progn
  (message "Now we are doing something unrelated to the macro")
  (my-work-in-temp-buffer-from-home
   (message "We do stuff inside the macro")
   (+ 1 1)
   (list "Protesilaos" "Cyprus")))
```

如果你将光标放在 `my-work-in-temp-buffer-from-home` 的右括号处，你可以通过键入 M-x (`execute-extended-command`) 然后调用命令 `pp-macroexpand-last-sexp` 来确认它扩展成什么样子。这是我得到的结果：

```emacs-lisp
(let ((default-directory "/home/prot/"))
  (with-temp-buffer
    (message "Running all expression from the `%s' directory" default-directory)
    (message "We do stuff inside the macro")
    (+ 1 1)
    (list "Protesilaos" "Cyprus")))
```

将其同上下文中的其余代码拼在一起我得到了这个：

```emacs-lisp
(progn
  (message "Now we are doing something unrelated to the macro")
  (let ((default-directory "/home/prot/"))
    (with-temp-buffer
      (message "Running all expression from the `%s' directory" default-directory)
      (message "We do stuff inside the macro")
      (+ 1 1)
      (list "Protesilaos" "Cyprus"))))
```

记住这个例子，考虑将 Elisp 宏当成：“这个小东西帮助我更简洁表达这个更大的过程而实际运行的代码仍然是后者的代码。”

我上面写的宏其主体以准引用（quasi quote）开始，所以你无法体会到求值里的细微差别。让我向你展示另一种方法，我编写一个宏，让它可以定义几个几乎相同的交互式函数 ([让你的交互式函数也能从 Lisp 调用中运行](#让你的交互式函数从-lisp-调用中运行))。

```emacs-lisp
(defmacro my-define-command (name &rest expressions)
  "定义命令跟对应 NAME 运行指定 EXPRESSIONS."
  (declare (indent 1))
  (unless (symbolp name)
    (error "I want NAME to be a symbol"))
  (let ((modifined-name (format "modified-version-of-%s" name)))
    `(defun ,(intern modifined-name) ()
       (interactive)
       ,(message "The difference between `%s' and `%s'" modifined-name name)
       ,@expressions)))
```

`my-define-command` 大致可以分为两部分：(i) 立刻求值的部分和 (ii) 展开后进一步求值的部分。后一部分以 quasi quote 开始。当调用宏时这种区别很重要，因为前一部分会立即执行，所以如果我们遇到错误它将永远不会展开并运行 `EXPRESSIONS` 。对下面的例子使用 `pp-macroexpand-last-sexp` 来看看我的意思。为方便起见我在每种情形下面包含了宏展开结果。

```emacs-lisp
(my-define-command first-demo
  (message "This is what my function does")
  (+ 1 10)
  (message "And this"))
;; =>
;;
;; (defun modified-version-of-first-demo nil
;;   (interactive)
;;   "The difference between ‘modified-version-of-first-demo’ and ‘first-demo’"
;;   (message "This is what my function does")
;;   (+ 1 10)
;;   (message "And this"))


(my-define-command second-demo
  (list "Protesilaos" "Cyprus")
  (+ 1 1)
  (message "Arbitrary expressions here"))
;; =>
;;
;; (defun modified-version-of-second-demo nil
;;   (interactive)
;;   "The difference between ‘modified-version-of-second-demo’ and ‘second-demo’"
;;   (list "Protesilaos" "Cyprus")
;;   (+ 1 1)
;;   (message "Arbitrary expressions here"))


(my-define-command "error scenario"
  (list "Will" "Not" "Reach" "This")
  (/ 2 0))
;; => ERROR...
```

你需要宏吗？并不总是需要，但有些情况下一个定义良好的宏会使你的代码更优雅。重要的是你对求值如何运作有一个概念，这样你就不会被所有那些括号搞晕。否则你可能会发生实际得到的结果与期望不符的事。


## 对列表中的元素进行映射

编程中一个常见状况是遍历一个列表里的元素并对每个元素执行一些计算。Emacs Lisp 有通用的 `while` 循环，以及一系列专门用于遍历列表元素的函数，例如 `mapcar`, `mapc`, `dolist`, `seq-filter`, `seq-remove` 等等等等。根据你在做的事情：遍历元素的目的是产生一些副作用和/或检测返回值 ([副作用与返回值](#副作用和返回值))。我将向你展示一些例子，让你决定哪种工具最适合手头的任务。

从 `mapcar` 开始，它将一个函数应用到列表中的每个元素。然后它获取每次迭代的返回值并将它们收集到一个新列表中然后作为 `mapcar` 整体的返回值。在下面的代码块中我使用 `mapcar` 遍历一个数字列表，将它们增加 `10` 并返回一个包含增加后数字的新列表。

```emacs-lisp
(mapcar
 (lambda (number)
   (+ 10 number))
 '(1 2 3 4 5))
;; => (11 12 13 14 15)
```

在上面的代码块中，我使用了一个 `lambda` ，即匿名函数 ([何时使用具名函数或 lambda 函数](#何时使用具名函数或-lambda-函数))。下面是相同的代码不过使用具名函数：

```emacs-lisp
(defun my-increment-by-ten (number)
  "Add 10 to NUMBER."
  (+ 10 number))

(mapcar #'my-increment-by-ten '(1 2 3 4 5))
;; => (11 12 13 14 15)
```

注意这里我们引用了具名函数 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。

`mapcar` 将返回值收集到一个新列表中有时可能没什么用。假设你想对求值某个函数做到保存所有打开文件中未保存的缓冲区。在这种情况下你不在乎累积结果：你只想直接获得保存缓冲区的副作用。为此你可以使用 `mapc` ，它总是返回它操作的列表：

```emacs-lisp
(mapc
 (lambda (buffer)
   (when (and (buffer-file-name buffer)
              (buffer-modified-p buffer))
     (save-buffer)))
 (buffer-list))
```

替代上述方法的是 `dolist` ，它用于产生副作用但总返回 `nil` ：

```emacs-lisp
(dolist (buffer (buffer-list))
  (when (and (buffer-file-name buffer)
             (buffer-modified-p buffer))
    (save-buffer)))
```

你会注意到 `dolist` 是一个宏，所以它的某些部分与基本列表及求值规则表现不同 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。这需要适应代码的编写方式。

什么时候使用 `dolist` 而不是 `mapc` 是风格问题。如果你使用具名函数在我看来 `mapc` 更简洁。否则 `dolist` 更容易阅读。这是我使用方式的一些伪代码：

```emacs-lisp
;; 我喜欢这个：
(mapc #'NAMED-FUNCTION LIST)

;; 我也喜欢用 `dolist' 替换 `mapc' 和里面的 `lambda' ：
(dolist (element LIST)
  (OPERATE-ON element))

;; 我不喜欢这个：
(mapc
 (lambda (element)
   (OPERATE-ON element))
 LIST)
```

虽然 `dolist` 和 `mapc` 是为了副作用，但你仍然可以在 `let` 和相关形式帮助下来累积结果 ([`if-let*` 控制流及其相关形式 ](#if-let-控制流及其相关形式))。根据具体情况这种方法可能比依赖 `mapcar` 更有意义。这是一个带注释的草稿：

```emacs-lisp
;; 以空列表 `found-strings' 开始。
(let ((found-strings nil))
  ;; 使用 `dolist' 检测列表里的每个元素 '("Protesilaos" 1 2 3 "Cyprus").
  (dolist (element '("Protesilaos" 1 2 3 "Cyprus"))
    ;; 如果元素是字符串则 `push' 到 `found-strings' ，否则跳过它。
    (when (stringp element)
      (push element found-strings)))
  ;; 现在我们完成了 `dolist' ，返回 `found-strings' 最新值。
  found-strings)
;; => ("Cyprus" "Protesilaos")


;; 同上面一样但反转结果，这可能更符合直觉一点：
(let ((found-strings nil))
  (dolist (element '("Protesilaos" 1 2 3 "Cyprus"))
    (when (stringp element)
      (push element found-strings)))
  (nreverse found-strings))
;; => ("Protesilaos" "Cyprus")
```

为了完整起见，前面的例子如果使用 `mapcar` 则必须这样做：

```emacs-lisp
(mapcar
 (lambda (element)
   (when (stringp element)
     element))
 '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" nil nil nil "Cyprus")


(delq nil
      (mapcar
       (lambda (element)
         (when (stringp element)
           element))
       '("Protesilaos" 1 2 3 "Cyprus")))
;; => ("Protesilaos" "Cyprus")
```

因为 `mapcar` 会开心地累积所有的返回值，它返回一个包含 `nil` 的列表。如果你想要那样，你可能甚至不会费力在那里使用 `when` 子句。 `delq` 被应用于 `mapcar` 的返回值用来删除所有 `nil` 实例。现在将这个繁琐的流程与 `seq-filter` 进行比较：

```emacs-lisp
(seq-filter #'stringp '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" "Cyprus")
```

当你只需要检测元素是否满足谓词函数并返回该元素时， `seq-filter` 是最佳工具。但你不能返回其他东西而 `mapcar` 会毫无怨言地接受任何返回值，例如以下代码：

```emacs-lisp
(delq nil
      (mapcar
       (lambda (element)
         (when (stringp element)
           ;; `mapcar' 会积累任意返回值，所以需要的话我们能改变
           ;; 每个元素的返回值
           (upcase element)))
       '("Protesilaos" 1 2 3 "Cyprus")))
;; => ("PROTESILAOS" "CYPRUS")

(seq-filter
 (lambda (element)
   (when (stringp element)
     ;; `seq-filter' 只返回那些具有非 nil 返回值的元素，
     ;; 但它返回的是元素本身，而不是在这里返回的值。
     ;; 换句话说，这个 `lambda' 做了没意义的事。
     (upcase element)))
 '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" "Cyprus")
```

你该怎么遍历列表元素取决于你想做什么。没有单个函数能为你做到所有事。理解里面细微差别，你就可以开始了。哦，还有，一定要看看内置的 `seq` 库（使用 M-x (`execute-extended-command`)，调用 `find-library` ，然后搜索 `seq` ）。然后看看 `seq.el` 的源代码：它定义了许多函数，如 `seq-take~、~seq-find~、~seq-union` 。另一种方法是调用命令 `shortdoc` 并阅读文档组里 `list` 以及 `sequence` 里的内容。


## 上次搜索的匹配数据

当你使用 Emacs Lisp 时你会遇到“匹配数据”（match data）的概念以及相关的函数 `match-data~、~match-beginning~、~match-string` 等等。这些指的是上次搜索的结果通常用来给 `re-search-forward~、~looking-at~、~string-match` 及相关函数执行。每次你执行搜索时，匹配数据都会更新。请注意这是个常见的副作用 ([副作和返回值](#副作用和返回值))。如果你忘记了它你的代码很可能做不对事。

在下面的代码块中我定义了一个函数，它在当前缓冲区中进行搜索并返回一个不带文本属性的匹配数据列表([文本具有自身属性](#文本具有自身属性))。

```emacs-lisp
(defun my-get-match-data (regexp)
  "使用 REGEXP 搜索前（下）面的内容并返回匹配结果否则返回 nil 。"
  (when (re-search-forward regexp nil t)
    (list
     :beginning (match-beginning 0)
     :end (match-end 0)
     :string (match-string-no-properties 0))))
```

你可以用一个字符串参数来调用它，该参数为一个 Emacs Lisp 正则表达式：

```emacs-lisp
(my-get-match-data "Protesilaos.*Cyprus")
```

如果正则表达式匹配，那么你会得到匹配数据。这是一些示例文本：

```fundamental
Protesilaos lives in the mountains of Cyprus.
```

将光标放在该文本前面，并使用 M-: (`eval-expression`) 来求值我上面带上的 regexp 的 `my-get-match-data` 。你会如正预期得到一个返回值。

按照 `my-get-match-data` 的编写，它做了两件事：(i) 它有将光标移动到找到的文本末尾的副作用，以及 (ii) 它返回一个包含我指定的匹配数据的列表。在许多情况下，你不希望有前面提到的副作用：光标应该停留在原来的位置。因此你可以将你的代码包装在 `save-excursion` 中 ([切换到另一个缓冲区、窗口或 narrowed](#切换到另一个缓冲区-窗口或-narrowed))：它会做到它必须做到的事并最终恢复 `point` ([运行一些代码或回退到其他代码](#运行一些代码或回退到其他代码))：

```emacs-lisp
(defun my-get-match-data (regexp)
  "使用 REGEXP 搜索前（下）面的内容并返回匹配结果否则返回 nil 。"
  (save-excursion ; 我们使用 `save-excursion' 包装了这里的代码来抑制副作用
    (when (re-search-forward regexp nil t)
      (list
       :beginning (match-beginning 0)
       :end (match-end 0)
       :string (match-string-no-properties 0)))))
```

如果你对这个版本的 `my-get-match-data` 求值然后重试我上面函数调用，你会注意到你是怎样得到预期的返回值而且没有移动光标（cursor）到匹配文本末尾的副作用。在实践中这是一个有用的工具，可以与 `save-match-data` 结合使用。想象一下你想在你执行的另一次搜索内部进行一次前向搜索，例如检测上下文中是否存在某个正则表达式的匹配，但需要抑制对你操作的匹配数据的修改。因此：

```emacs-lisp
(defun my-get-match-data-with-extra-check (regexp)
  "使用 REGEXP 向前（下）搜索且后面不能有空格匹配，返回匹配数据，否则返回 nil 。"
  (save-excursion
    (when (and (re-search-forward regexp nil t)
               (save-match-data (not (looking-at "[\s\t]+"))))
      ;; 返回第一次搜索的匹配数据,
      ;; 第二次搜索（用于检查空格或制表符）
      ;; 只是一个额外的检查，我们不想使用它的匹配数据，因此
      ;; 在它周围使用了 `save-match-data' 。
      (list
       :beginning (match-beginning 0)
       :end (match-end 0)
       :string (match-string-no-properties 0)))))
```

对函数 `my-get-match-data-with-extra-check` 求值，然后用 M-: (`eval-expression`) 调用它进行测试，它在下面的第二个例子中返回一个非 `nil` 值，但在第一个例子中不返回。这是预期结果。

```emacs-lisp
(my-get-match-data-with-extra-check "Protesilaos.*Cyprus")
;; => nil


;; Protesilaos, also known as "Prot", lives in the mountains of Cyprus   .

(my-get-match-data-with-extra-check "Protesilaos.*Cyprus")
;; => (:beginning 41988 :end 42032 :string "Protesilaos lives in the mountains of Cyprus")


;; Protesilaos lives in the mountains of Cyprus.
```


## 切换到另一个缓冲区、窗口或 narrowed

当你以 Emacs Lisp 编程方式做事时，你会遇到需要离开当前位置的情况。你可能需要切换到另一个缓冲区、切换到给定缓冲区的窗口、甚至修改你正在编辑的缓冲区中可见内容。在任何时候这都涉及一个或多个副作用，这些副作用很可能应该在你的函数完成其工作时被撤销 ([副作用与返回值](#副作用和返回值))。

可能最常见的情况是恢复 `point` 。你有一些在缓冲区中向后或向前移动以执行匹配给定文本片段的的代码。但是之后你需要将光标留在它原来的位置否则用户会失去方向感。将你的代码包装在 `save-excursion` 中，你就搞定了，正如我在别处展示的那样 ([上次搜索的匹配数据](#上次搜索的匹配数据))：

```emacs-lisp
(save-excursion ; 当事情做完后归还 `point'
  MOVE-AROUND-IN-THIS-BUFFER)
```

`save-window-excursion` 的原理相同，它允许你选择另一个窗口（例如 `select-window` ），在其缓冲区中移动，然后恢复窗口的原状：

```emacs-lisp
(save-window-excursion
  (select-window SOME-WINDOW)
  MOVE-AROUND-IN-THIS-BUFFER)
```

`save-restriction` 允许你恢复缓冲区的当前 narrow 状态。然后你可以选择 `widen` 或 `narrow-to-region` （还有像 `org-narrow-to-subtree` 这样的相关命令），做你必须要做的事后然后将缓冲区恢复到开始状态。

```emacs-lisp
;; 这里我们假设我们开始于一个展开状态。
;; 然后我们收窄到当前的 Org 标题以获取下面所有内容作为一个巨大字符串。
;; 最后由于 `save-restriction' 的作用我们会再次展开。
(save-restriction
  (org-narrow-to-subtree)
  (buffer-string))
```

根据具体情况你可能得需要组合使用上述方法。请注意 `save-restriction` 的文档告诉你要将 `save-excursion` 作为最外层的调用。除此之外，你还会发现一些情况需要不同的方法来执行对应条件下的行为 ([运行一些代码或回退到其他代码](#运行一些代码或回退到其他代码))。


## `if`, `cond` 等基本控制流

你不需要任何条件逻辑来执行基本操作。例如如果你编写一个向下移动 15 行的命令，当它无法移动超过你指定的数量时，它自然会在缓冲区末尾停止。使用 `defun` ，你编写一个交互式函数（即一个“命令”）来无条件用 `forward-line` 向下移动 15 行（用负数调用它会反向移动）：

```emacs-lisp
(defun my-15-lines-down ()
  "向下移动最多 15 行。"
  (interactive)
  (forward-line 15))
```

`my-15-lines-down` 基本是最简洁的形式：它包装了一个基本函数并向其传递一个固定参数，在本例中是数字 `15` 。使用 M-x (`execute-extended-command`) 然后输入名字调用此命令。它有用！一旦你决定要在满足给定条件时才执行某些操作，事情就会变得更加复杂。这种不同分支之间的逻辑序列“控制流”是用 `if`, `when`, `unless` 和 `cond` 等等来表达的。根据具体情况， `and` 以及 `or` 可能也够用了。

让你的 `my-15-lines-down` 变得更聪明一点怎么样？当它处于缓冲区的最末尾时，让它向上移动 15 行。为什么？因为这只是一个演示，所以为什么不呢？检测游标（point）是否在缓冲区末尾的谓词函数是 `eobp` 。一个“谓词”是一个函数其条件满足时返回 true（技术上讲是非 `nil` ），否则返回 `nil` ([副作用与返回值](#副作用和返回值))。至于这个奇怪的名字，Emacs Lisp 中惯例以 `p` 后缀结束作为谓词函数名称：如果函数名由多个单词组成（通常用破折号分隔）则谓词函数命名为 `NAME-p` ，例如 `string-match-p` ；否则命名为 `NAMEp` 例如 `stringp` 。

```emacs-lisp
(defun my-15-lines-down-or-up ()
  "向下移动最多 15 行或者掉头当`eobp' 为非 nil 。"
  (interactive)
  (if (eobp)
      (forward-line -15)
    (forward-line 15)))
```

对这个函数求值，然后键入 M-x (`execute-extended-command`) 并调用 `my-15-lines-down-or-up` 来感受一下。下面是一个类似想法，如果 `eobp` 返回非 `nil` 它会抛出错误并退出正在做的事情：

```emacs-lisp
(defun my-15-lines-down-or-error ()
  "当 `eobp' 返回非 nil 值时抛错，否则向下移动 15 行"
  (interactive)
  (if (eobp)
      (error "Already at the end; will not move further")
    (forward-line 15)))
```

Emacs Lisp 的一个怪癖（或许一直以来都被认为是一个特性）是它的缩进方式。只需标记你写好的代码并键入 TAB: Emacs 会负责按应有的方式对其进行缩进。在 `if` 语句的情况下，"then" 部分比逻辑的 "else" 部分缩进得更深。这种缩进没有特殊含义：你可以把所有东西写在一行上，比如 `(if COND THIS ELSE)` ，顺便说一下，这看起来像一个典型列表 ([符号，匹配括号表达式和引用](#符号-匹配括号表达式和引用))。缩进的作用是帮助你意识到括号的不平衡。如果不同的表达式以看着奇怪的方式对齐，那么你很可能缺少一个括号或多了括号。通常，同一级别的表达式都会以相同的方式对齐。更深层次的表达式会有更多的缩进，依此类推。经验会让你能发现括号不匹配的错误。但即使你没有识别出来，你最终也会得到一个错误。放心吧！

`if` 的写法就像一个接受两个或多个参数的函数。“更多”的部分都算作 "else" 逻辑的一部分。因此 `(if COND THIS)` 没有 "else" 逻辑，而 `(if COND THIS ELSE1 ELSE2 ELSE3)` 将按顺序运行 `ELSE1=、=ELSE2` 和 `ELSE3` 作为 "else" 分支的一部分。当加上到合适的缩进时，它看起来是这样的：

```emacs-lisp
(if COND
    THIS
  ELSE1
  ELSE2
  ELSE3)
```

那么如果 `THIS` 部分需要大于一个函数调用呢？Elisp 有 `progn` 形式，你可以用它来包装函数调用并将它们作为一个单一参数传递。把它们放在一起你的代码现在会像这样：

```emacs-lisp
(if COND
    (progn
      THIS1
      THIS2
      THIS3)
  ELSE1
  ELSE2
  ELSE3)
```

如果你不需要 "else" 部分，使用 `when` 来表达你的意思。在内部这是一个宏实际上代表 `(if COND (progn EXPRESSIONS))` ，其中 `EXPRESSIONS` 是一个或多个表达式。一个 `when` 看起来像这样：

```emacs-lisp
(when COND
  THIS1
  THIS2
  THIS3)
```

同样， `unless` 的意味者 `(when (not COND) EXPRESSIONS)` 。它也是一个宏扩展为一个 `if` 语句：

```emacs-lisp
(unless COND
  THIS1
  THIS2
  THIS3)
```

当你检测的条件有多个部分时，你可以使用 `and` 以及 `or` ：

```emacs-lisp
(when (or THIS THAT)
  EXPRESSIONS)

(when (and THIS THAT)
  EXPRESSIONS)

(when (or (and THIS THAT) OTHER)
  EXPRESSIONS)
```

根据具体情况，多个 `if`, `when`, `or`, `and` 的组合看起来很别扭。你可以使用 `cond` 将逻辑分解为不同的条件，这些条件从上到下依次检测。 `cond` 的写法是一个列表的列表，这些列表不需要引用 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。抽象地说，它看起来像这样：

```emacs-lisp
(cond
 (CONDITION1
  CONSEQUENCES1)
 (CONDITION2
  CONSEQUENCES2)
 (CONDITION3
  CONSEQUENCES3)
 (t
  CONSEQUENCES-FALLBACK))
```

每个 CONSEQUENCES 都可以是任意数量的表达式，就像你上面看到的 `when` 一样。这是一个展示 `cond` 行为的玩具函数：

```emacs-lisp
(defun my-toy-cond (argument)
  "根据 ARGUMENT 类型返回结果。"
  (cond
   ((and (stringp argument)
         (string-blank-p argument))
    (message "You just gave me a blank string; try harder!"))
   ((stringp argument)
    (message "I see you can do non-blanks string; I call that progress."))
   ((null argument)
    (message "Yes, the nil is an empty list like (), but do not worry about it"))
   ((listp argument)
    (message "Oh, I see you are in the flow of using lists!"))
   ((symbolp argument)
    (message "What's up with the symbols, mate?"))
   ((natnump argument)
    (message "I fancy those natural numbers!"))
   ((numberp argument)
    (message "You might as well be a math prodigy!"))
   (t
    (message "I have no idea what type of thing your argument `%s' is" argument))))
```

我希望你对其求值并传递不同的参数来测试它做了什么 ([Emacs Lisp 求值](#emacs-lisp-求值))。这里有两个例子：

```emacs-lisp
(my-toy-cond "")
;; => "You just gave me a blank string; try harder!"

(my-toy-cond '(1 2 3))
;; => "Oh, I see you are in the flow of using lists!"
```

以上所有内容在 Emacs Lisp 中都很常见。另一个强大的宏是 `pcase` ，由于其特殊性，我们将单独考虑它 ([`pcase` 及其相关形式的模式匹配](#pcase-及其相关形式的模式匹配))。


## `if-let*` 控制流及其相关形式

`let` 和 `let*` 声明了仅在当前作用域内（即 `let` 的 `BODY` 部分）可使用的变量。比如：

```emacs-lisp
(let BINDINGS
  BODY)

(let ((variable1 value1)
      (variable2 value2))
  BODY)
```

`BINDINGS` 是一个列表的列表，不需要引用 ([宏或特殊形式内部的求值](#宏或特殊形式里的求值))。而 `BODY` 由一个或多个表达式组成，我在本书的其他地方也将其命名为 `EXPRESSIONS` 。 `let` 和 `let*` （读作 "let star"）的区别在于，后者让较早的绑定可被用于较晚的绑定，就像这样：

```emacs-lisp
;; 这可以生效因为 `greeting' 可以使用 `name' 和 `country' ，
;; 多亏了 `let*' ：
(let* ((name "Protesilaos")
       (country "Cyprus")
       (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))

;; 但这失败了……
(let ((name "Protesilaos")
      (country "Cyprus")
      (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))
```

有时你想要做的是，当且仅当这些绑定都非 `nil` 时才创建它们。如果它们的值是 `nil` 那么它们对你来说是没用的，在这种情况下你会想做别的事 ([`if`, `cond` 等基本控制流](#if-cond-等基本控制流))。当你使用函数调用或某个其他变量的返回值创建绑定时值可能是 `nil` 也可能不是。你总可以写这样的代码：

```emacs-lisp
(let ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
      (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
  (if (and variable1 variable2) ; 简单检测两个变量是否非 nil
      THIS
    ELSE))
```

但是你可以用 `if-let*` 做同样的事情，其中 `THIS` 部分仅在所有绑定都非 `nil` 时运行：

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

在 `ELSE` 部分，绑定 `variable1` 和 `variable2` 不存在：它们只存在于代码 `THIS` 部分。

`when-let*` 与 `when` 相同，意味着它没有 "else" 逻辑。如果它的某个绑定是 `nil` ，那么整个 `when-let*` 返回 `nil` 。无需再赘述这一点。

随着你深入研究 Emacs Lisp 生态，你会遇到 `if-let*` 用法，它 (i) 像 `let` 或 `let*` 一样创建多个绑定，但 (ii) 也使用一个谓词函数来测试它们是否应该继续执行逻辑的 `THIS` 部分。记住，如果 `if-let*` 的某个绑定返回 `nil` ，它会直接转到 `ELSE` 。考虑这个例子：

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          ;; 下划线 _ 表明：“不用绑定这个；
          ;; 我只关心返回值是否为非 nil ”。我们在这里检测
          ;; `variable1' 是否是字符串：如果是
          ;; 我们继续之后绑定，否则移动到代码
          ;; 的 ELSE 部分
          (_ (stringp variable1))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

没有绝对胜出的方法，关键在于为手头的任务使用正确的工具。有时你希望创建绑定，即使它们的值是 `nil` 。选择有意义的方式。


## `pcase` 及其相关形式的模式匹配

一旦你掌握了用 Emacs Lisp 表达想法的流程，你将能熟练使用 `if~、~cond` 及类似形式 ([`if`, `cond` 等基本控制流](#if-cond-等基本控制流))。如果使用 `if-let*` ，你甚至可能玩得更高级 ([ `if-let*` 及其相关形式控制流](#if-let-控制流及其相关形式))。然而无论你怎么做，有些情况下更简洁的表达方式无疑更好。这就是 `pcase` 发挥作用的地方。在其基本的表达中，它类似于 `cond` ，因为它测试给定表达式的返回值与一系列条件的匹配情况。这里有一个例子，将变量 `major-mode` 的缓冲区局部值（buffer-local value）与几个已知的符号进行相等比较：

```emacs-lisp
(pcase major-mode
  ('org-mode (message "You are in Org"))
  ('emacs-lisp-mode (message "You are in Emacs Lisp"))
  (_ (message "You are somewhere else")))
```

上面与这个 `cond` 的想法相同：

```emacs-lisp
(cond
 ((eq major-mode 'org-mode)
  (message "You are in Org"))
 ((eq major-mode 'emacs-lisp-mode)
  (message "You are in Emacs Lisp"))
 (t
  (message "You are somewhere else")))
```

一些程序员可能会争论说 `pcase` 更优雅。我认为在这个具体例子中确实如此，但我想保持灵活和务实：我会使用对我正在编写的代码更有意义的任何方式。谈到优雅，我想告诉你几乎所有的条件逻辑都可以用一种意想不到的方法来完成。考虑一下我在本书中的例子是如何重复使用 `message` 的，而实际上唯一改变的部分是传递给该函数的实际字符串/参数。这样做同样有效：

```emacs-lisp
(message
 (pcase major-mode
   ('org-mode "You are in Org")
   ('emacs-lisp-mode "You are in Emacs Lisp")
   (_ "You are somewhere else")))
```

对于 `if~、~when` 和其余的也是同样的概念。

回到 `pcase` 有什么不同的主题。如果你阅读它的文档，你会意识到它有自己的迷你语言或称为“领域特定语言”（DSL）。这对于宏来说很常见 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。它们定义了求值如何完成以及哪种表达式被特殊处理。那么让我送你这个玩具函数，它说明了现在讨论 DSL 的一些主要特性：

```emacs-lisp
(defun my-toy-pcase (argument)
  "使用 `pcase' 根据 ARGUMENT 来返回合适结果。"
  (pcase argument
    (`(,one ,_ ,three)
     (message "List where first element is `%s', second is ignored, third is `%s'" one three))
    (`(,one . ,two)
     (message "Cons cell where first element is `%s' and second is `%s'" one two))
    ((pred stringp)
     (message "The argument is a string of some sort"))
    ('hello
     (message "The argument is equal to the symbol `hello'"))
    (_ (message "This is the fallback"))))
```

对函数求值然后试一下 ([Emacs Lisp 求值](#emacs-lisp-求值))。下面是几个例子：

```emacs-lisp
(my-toy-pcase '("Protesilaos" "of" "Cyprus"))
;; => "List where first element is ‘Protesilaos’, second is ignored, third is ‘Cyprus’"

(my-toy-pcase '("Protesilaos" . "Cyprus"))
;; => "Cons cell where first element is ‘Protesilaos’ and second is ‘Cyprus’"
```

其中一些子句是 `cond` 的不同表达方式，说不定更好，但在我看来并非绝对的赢家。令人印象深刻且真正带来范式转变的是“解构”（destructuring）的概念，即对表达式进行模式匹配，它有效地将列表或序对的元素通过 `let` 绑定到它们对应的索引。用于这种解构的语法是晦涩难懂的，直到你将其与用于部分求值的准引用（quasi quote）和逗号联系起来 ([列表里的部分求值](#列表里的部分求值))。考虑到这一点，将 `pcase-let`, `pcase-let*`, `pcase-lambda` 和 `pcase-dolist` 视为普通的 `let`, `let*`, `lambda` 和 `dolist` 的增加了支持解构特性的变体。不过，它们并没有做 `pcase` 的任何额外功能——只是在它们熟悉的行为之上增加了解构！这在你处理函数返回值为列表的时候特别有用。我不会详细阐述，因为这是一个高级用例。如果你已经到了那个水平你不需要我告诉你该写什么。对于像我一样其他人，通常处理更简单的代码， `pcase-let` 足以说明这个原理：

```emacs-lisp
(defun my-split-string-at-space (string)
  "以空格分割 STRING 并放在一个列表内然后返回。"
  (split-string string "\s"))

(pcase-let ((`(,one ,_ ,three) (my-split-string-at-space "Protesilaos of Cyprus")))
  (message "This is like `let', but we got `%s' and `%s' via destructuring" one three))
;; => "This is like ‘let’, but we got ‘Protesilaos’ and ‘Cyprus’ via destructuring"
```

你是否使用 `pcase` 和解构一般取决于你。你不需要它们来编写高质量的代码。不过你可能会同意那些人的观点，认为它们更优雅。因此选择使用它们来使代码简洁而更富有表现力。


## 运行一些代码或回退到其他代码

你典型代码会依赖 `if~、~cond` 等控制流 ([`if`, `cond` 等基本控制流](#if-cond-等基本控制流))。根据你的具体需求或风格考虑，它甚至可能包括 `pcase` ([ `pcase` 及其相关形式的模式匹配](#pcase-及其相关形式的模式匹配)) 以及 `if-let*` ([ `if-let*` 及其相关形式控制流](#if-let-控制流及其相关形式))。然而有些情况下你必须在主要操作结束或退出后运行额外的代码。其思想是清理你创建的任何中间状态。逻辑是“用所有必要副作用做这件事，然后无论发生什么也要做那件事以便撤销副作用。” 这是 "unwinding" 的概念，通过 `unwind-protect` 实现。

在下面的代码块中我定义了一个函数，它会产生一个 minibuffer 输入栏，要求你提供 `y` 或 `n` 的答案，这是 "yes" 或 "no" 的简写。它检测 `y-or-n-p` 的返回值来决定需要做什么。当输入栏打开时，该函数会高亮显示当前缓冲区中正则表达式 `(defun` 的所有匹配。在你完成 minibuffer 及其后续后，这些高亮都必须消失。

```emacs-lisp
(defun my-prompt-with-temporary-highlight ()
  "询问确认和等待时高亮所有正则匹配。"
  (let ((regexp "(defun"))
    (unwind-protect
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (message "You have decided to proceed")
            (message "You prefer not to continue")))
      (unhighlight-regexp regexp))))
```

在你的 Emacs 中尝试上面的代码来感受一下。当“是或否”提示处于活动状态时，也执行 C-g (`keyboard-quit`) 或 C-] (`abort-recursive-edit`) 来确认就算在输入栏后续代码从未运行高亮也会被移除。你甚至可以修改函数以产生错误：它将创建一个 backtrace ，当你从 `*Backtrace*` 窗口执行 q (`debugger-quit`) 后，仍然会产生 unwinding 的效果。

```emacs-lisp
(defun my-prompt-with-temporary-highlight-try-with-error ()
  "询问确认和等待时高亮所有正则匹配。"
  (let ((regexp "(defun"))
    (unwind-protect
        (progn
          (highlight-regexp regexp)
          (error "This error makes no sense here; close the backtrace to test the unwinding")
          (if (y-or-n-p "Should we proceed or not? ")
              (message "You have decided to proceed")
            (message "You prefer not to continue")))
      (unhighlight-regexp regexp))))
```

退一步看，你会发现 `unwind-protect` 是像 `save-excursion` 和 `save-restriction` ([切换到另一个缓冲区、窗口或 narrowed](#切换到另一个缓冲区-窗口或-narrowed)) 这样专门形式的更为通用的形式，同时它支持着 `save-match-data` ([上次搜索的匹配数据](#上次搜索的匹配数据)) 以及许多其他函数/宏，例如 `with-temp-buffer` 和 `save-window-excursion~。~unwind-protect` ，但不对对信号（例如来自 `error` 函数的信号）做出特殊响应：它会允许错误发生，这意味着将显示回溯并且你的代码将在那里退出（但是 unwinding 仍然会起作用，正如我已之前解释过的，在你关闭 backtrace 后）。要让你的代码以更可控的方式处理信号，你必须依赖 `condition-case` 。

使用 `condition-case` 你可以完全控制代码的行为，包括它应该如何处理错误。换句话说，你的 Elisp 将表达这样的意思：“我想做这个，但如果我得到一个错误，我想做那个来代替。” 有许多信号需要考虑，所有这些都来自 `signal` 函数。这些包括符号 `error`, `user-error`, `args-out-of-range`, `wrong-type-argument`, `wrong-length-argument` 和 `quit` ，此外还有程序员任何可能认为必要的其他信号。在下面的代码块中，我向你展示 `condition-case` 的样式。请记住有时由于底层的实现方式，你不会像通常那样进行引用 ([宏或特殊形式里的求值](#宏或特殊形式里的求值))。我使用的例子与我用于 `unwind-protect` 的例子相同。

```emacs-lisp
(defun my-prompt-with-temporary-highlight-and-signal-checks ()
  "询问确认和等待时高亮所有正则匹配。"
  (let ((regexp "(defun"))
    (condition-case nil
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (user-error "You have decided to proceed; but we need to return a `user-error'")
            (error "You prefer not to continue; but we need to return an `error'")))
      (:success
       (unhighlight-regexp regexp)
       (message "No errors, but still need to unwind what we did, plus whatever else we want here"))
      (quit
       (unhighlight-regexp regexp)
       (message "This is our response to the user aborting the prompt"))
      (user-error
       (unhighlight-regexp regexp)
       (message "This is our response to the `user-error' signal"))
      (error
       (unhighlight-regexp regexp)
       (message "This is our response to the `error' signal")))))
```

上面的函数说明了前面提到的 unwinding 概念和处理信号的机制。 `condition-case` 的抽象结构在我看来像是 `let~、~unwind-protect` 和 `cond` 的混合体。这些条件可能包括特殊的处理程序 `:success` 正如我在那里展示的那样。当然我写的代码不会永远导致那个特定成功情况，但你可以修改提示符之后发生的事情，比如说调用 `message` 而不是 `user-error` 函数，这将被视为一个成功的结果。否则我认为我写的表达式准确地告诉你这个程序如何响应它接收到的信号。

我还没有涵盖的是 `condition-case` 类似 `let` 的方面，即它如何将错误数据绑定到此作用域里的变量。在我上面实现中，它是你看到的 `nil` 意味着我选择不执行这样的绑定，因为我不需要它的数据。下面我决定使用它，仅仅为了演示。

```emacs-lisp
(defun my-prompt-with-temporary-highlight-and-signal-checks-with-error-report ()
  "询问确认和等待时高亮所有正则匹配。"
  (let ((regexp "(defun"))
    (condition-case error-data-i-got
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (user-error "You have decided to proceed; but we need to return a `user-error'")
            (error "You prefer not to continue; but we need to return an `error'")))
      (:success
       (unhighlight-regexp regexp)
       (message "No errors, but still need to unwind what we did, plus whatever else we want here")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (quit
       (unhighlight-regexp regexp)
       (message "This is our response to the user aborting the prompt")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (user-error
       (unhighlight-regexp regexp)
       (message "This is our response to the `user-error' signal")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (error
       (unhighlight-regexp regexp)
       (message "This is our response to the `error' signal")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got))))))
```

有时 `unwind-protect` 和 `condition-case` 是适合这项工作的工具。我希望这些例子给了你宏观的认知，你现在已经准备好用 Emacs Lisp 来编写你自己的程序了。


## 何时使用具名函数或 lambda 函数

`lambda` 是一个匿名函数。它与 `defun` 相对，后者定义一个拥有给定名称的函数。何时使用哪一个很大程度上是风格问题。不过在某些情况下某种方法更合适。经验法则是：如果你需要多次使用该函数，那么给它一个名字然后通过名字调用它。否则，你实际上每次都会重新定义它，这使得重写你的程序变得困难。相比之下，如果函数只是临时用用的，那么用 `lambda` 就可以了。

在某些情形下，你会有一个内部使用 `lambda` 的命名函数。先修改你会在本书中找到的一个例子 ([对列表中的元素进行映射](#对列表中的元素进行映射))：

```emacs-lisp
(defun my-increment-numbers-by-ten (numbers)
  "对 NUMBERS 中的每一个元素加 10 然后返回新的列表。"
  (mapcar
   (lambda (number)
     (+ 10 number))
   numbers))

(my-increment-numbers-by-ten '(1 2 3))
;; => (11 12 13)
```

具名函数内部的 `lambda` 也可以在 `let` 的帮助下用于重复做某事。例如你可能有一个函数需要使用 `mapc` 作为副作用来问候一个人员列表，并且你不想多次定义同一个函数：

```emacs-lisp
(defun my-greet-teams (&rest teams)
  "对处在 TEAMS 的每一个人说 hello 然后返回一个包含所有团队成员的列表。
TEAMS 的每一个元素都是存着字符串的列表。"
  (let* ((greet-name (lambda (name)
                       (message "Hello %s" name)))
         (greet-team-and-names (lambda (team)
                                 (message "Greeting the team of `%s'..." team)
                                 (mapc greet-name team))))
    (mapcar greet-team-and-names teams)))

(my-greet-teams
 '("Pelé" "Ronaldo")
 '("Maradona" "Messi")
 '("Beckenbauer" "Neuer")
 '("Platini" "Zidane")
 '("Baresi" "Maldini")
 '("Eusebio" "Cristiano Ronaldo")
 '("Xavi" "Iniesta")
 '("Charlton" "Shearer")
 '("Puskas" "Kubala")
 '("All of the Greece Euro 2004 squad ;)"))
;; => (("Pelé" "Ronaldo") ("Maradona" "Messi") ...)
```

问候语在这种情况下是副作用并且可以在 `*Messages*` 缓冲区中找到。你可以使用 C-h e (`view-echo-area-messages`) 快速访问该缓冲区。 `my-greet-teams` 具体在做什么并不重要。专注于一个命名函数及其内部匿名函数的组合。


## 让你的交互式函数从 Lisp 调用中运行

当函数使用 `interactive` 规范声明时，它就可以交互式使用。这会将它们变成“命令”然后可以通过名称调用，首先执行 M-x (`execute-extended-command`) 然后找到该命令。它也可以分配给一个按键并直接通过按该键调用。在其最简单的形式中， `interactive` 规范是一个未引用的列表，如 `(interactive)` 。这里有一个简单的例子，它调用 `read-string` 来产生一个 minibuffer 输入栏，该输入栏接受用户输入并将其作为字符串返回：

```emacs-lisp
(defun my-greet-person ()
  (interactive)
  (message "Hello %s" (read-string "Whom to greet? ")))
```

上述实现问题在于它仅在交互式使用中有用。如果你想通过在程序中非交互式地发出这样的问候，你需要编写另一个函数，该函数做几乎相同的事情只是它接受一个 `NAME` 参数。像这样：

```emacs-lisp
(defun my-greet-person-with-name (name)
  "问候给定的 NAME 。"
  (message "Hello %s" name))
```

你不需要编写两个实际上做同样事情的独立函数。相反，你可以有一个带有参数的函数，它根据是交互式调用还是编程方式调用来决定如何获取传递给它的参数的值。考虑这种情况：

```emacs-lisp
(defun my-greet-interactive-and-non-interactive (name)
  "问候给定的 NAME 。
如果交互式调用，生成一个输入框询问 NAME 。

如果通过 Lisp 调用， NAME 为一个字符串。"
  (interactive (list (read-string "Whom to greet? ")))
  (message "Hello %s" name))
```

我在上面写的文档准确地告诉你发生了什么。不过让我进一步解释 `interactive` ：它接受一个参数，该参数是一个列表，对应于当前 `defun` 的参数列表。在这种情况下， `defun` 有一个包含单个元素 `NAME` 的参数列表。因此， `interactive` 也有一个包含一个元素的列表，其值对应于 `NAME` 。如果参数不止一个，那么 `interactive` 必须相应地编写：它的每个元素将对应于列表中相同索引的参数。

你传递给 `interactive` 这个表达式列表本质上是将值绑定到参数的预备工作。当你交互式地调用上面的函数时，你实际上告诉 Emacs 在这种情况下 `NAME` 是调用 `read-string` 的返回值。对于更多参数原理也是相同，但我还是写下来以明确说明：

```emacs-lisp
(defun my-greet-with-two-parameters (name country)
  "用来自 COUNTRY 的 NAME 问候某人。
当交互式调用时，产生一个 minibuffer 输入栏，询问 NAME
接着是另一个提示，询问 COUNTRY。

当是来自 Lisp 的调用时， NAME 和 COUNTRY 是字符串。"
  (interactive
   (list
    (read-string "Whom to greet? ")
    (read-string "Where from? ")))
  (message "Hello %s of %s" name country))

(my-greet-with-two-parameters "Protesilaos" "Cyprus")
;; => "Hello Protesilaos of Cyprus"
```

仔细编写 `interactive` 声明，你最终会得到一个既高效又灵活的丰富代码库。
