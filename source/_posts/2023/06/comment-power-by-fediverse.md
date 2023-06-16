---
author: SouthFox
title: 使用 Mastodon 作为博客的评论系统
date: 2023-06-16 11:20:30
tags:
- Mastodon
- 联邦宇宙
- 技术
category: 技术
---

又在博客的评论系统上左右摇摆了，就算没有人来评论。

<!--more-->

之前写过一篇[使用 Giscus 作为博客评论系统](https://blog.southfox.me/2022/01/%E4%B8%BA%E5%8D%9A%E5%AE%A2%E6%94%AF%E6%8C%81%E8%AF%84%E8%AE%BA%E7%B3%BB%E7%BB%9F/)的文章，一年下来的使用体验感受良好，只需要经过一点点配置就能为博客嵌入一个支持 `Markdown` 支持、代码高亮、表情反应、邮件通知、数据背靠 `GitHub`的评论系统，性价比十分之高。但代价也是背靠 `GitHub` ，作为一个大商业公司「发病」是一个扔骰子骰子的过程，随着时间流逝，扔出个「大发病」的概率将会趋近于必然，可能是什么「大会员」或是「API 大收费」。为了避免最坏情况发生所以有必要准备一些备选方案。

比如 [Cusdis](https://cusdis.com/) 是我之前尝试果一阵的评论系统方案，虽然各方面都比较优秀，但是一些小小的「毛刺」也让我放弃继续使用里。更大的原因也是我开始觉得为了一年不到十多条的评论是否有必要开一个数据库+评论系统。

所以最后是打算复用一些建立的应用作为博客的评论系统，例如 [cactus](https://cactus.chat/) 评论系统就可以使用 `Matrix` 聊天协议作为博客的评论系统。不过我觉得着有点「重」了，而且 `Matrix` 主流实现 `Synapse` 对于房间、媒体管理这些支持并不算太好，要是被恶意刷请求很难清理干净。

所以最后目光放向了 `Mastodon` ，现在谷歌上搜索 `mastodon blog comment`  就能搜索出很多方案里，本次我也是参（tou）考（qie）整（feng）合（he）了其中两篇的解决方案：

[Adding comments to your blog, powered by mastodon](https://blog.thms.uk/2023/02/mastodon-comments)

[Mastodon as comment system for your static blog ](https://danielpecos.com/2022/12/25/mastodon-as-comment-system-for-your-static-blog/)

主要思想就是利用 `Mastodon` 的帖子 `API` 获取一条博文下的所有回复，然后处理后插入的指定位置，并不是太复杂。

## 实现

作为一个备选方案我并没有删掉之前基于 `Giscus` 的评论方案，现在是处于共存状态。要启用基于 `Mastodon` 的方案就在文章的元数据中指定 `fedi_url` 这个变量：

```ejs
<% if (item.no_comment){ %>
  <!-- no comment -->
<% } else { %>
  <% if (item.fedi_url){ %>
    <div id="comments">
      <p id="mastodon-comments-list"></p>
      <script src="<%- url_for('./js/fedicomment.js') %>" post-url="<%- item.fedi_url %>"> async</script>
    </div>
    <noscript>Enable JavaScript to view the comments.</a></noscript>
  <% } else { %>
    <script src="https://giscus.app/client.js"
				data-*="……"
            async>
    </script>
  <% } %>
<% } %>
```

然后引入 `fedicomment.js` 这个文件，不直接通过模板直接写入主要还是因为自己自找没去设置里 `CSP` 禁止了 `inline javascript` 。

  ```ejs
  <script src="<%- url_for('./js/fedicomment.js') %>" post-url="<%- item.fedi_url %>"> async</script>
  ```

这一段指定了 `post-url` 这个属性，作为一个变量传入脚本文件里，参见：[Get data attribute of script tag?](https://stackoverflow.com/questions/14904378/get-data-attribute-of-script-tag)

之后在脚本文件里将作为全局变量调用：

```javas
var post_url = document.currentScript.getAttribute("post-url");
```

实际的脚本文件课参见 `GitHub` 上： [fedicomment.js](https://github.com/SouthFox-D/SouthFox-D.github.io/blob/hexo/themes/freemind/source/js/fedicomment.js) （毕竟要是直接复制过来也太凑字数了），不过实际也是整合了前面提到的两篇参考文章。

## 缺点

**麻烦：**使用这套方案的最大特点就是要多一步操作，因为 `Mastodon` 的帖子 `id` 是根据时间戳生成的，不能提前知晓。所以得在那边提前发好文然后获取帖子链接再来插入到博文这里。或许通过配置自动构建的形式使用帐号的 `token` 预先发好帖然后自动插回文件再提交是种解决办法，但是肯定少不了折腾。而且虽说是在博客上的评论系统但是不能在博客上直接评论。

**管理：**如果不是身为站点管理的话，那么将无法管理帖文下恶意评论，只能在站点层面的封禁才能移除评论，而且即使身为管理，也无法单独对某条帖子进行移除，可能直接改动数据库才能办到吧。

**限制：**如果站点开启了「安全模式」的话，那么将无法直接调用帖文的 `API` ，这样自然谈不上在博客中显示评论了。

### 总结

总体来说，对于想复用服务的人或是自由协议爱好者来说可以一试，不过其实相比 `Mastodon` ，直接在博客上用 `Serveless` 函数平台服务兼容 `ActivityPub` 协议似乎是个更好的选择？不过这就相当于从依赖 `GitHub` 改为依赖其它平台了……  
