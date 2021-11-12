---
title: 狐说
date: 2021-07-11 17:45:57
---

<html>
<link rel="stylesheet" href="/css/foxsay.css" media="screen" type="text/css">
    <main id="app">
        <aside>
        </aside>
        <section class="item" v-for="item in contents" v-cloak>
            <bb v-html='item.attributes.content'></bb>
            <time v-html:datetime="item.attributes.time">{{item.attributes.time}}</time>
        </section>
        <div class="load-ctn">
            <button class="btn btn-default " v-on:click="loadMore" v-if="contents" v-cloak>加载更多</button>
            <p class="tip" v-else>加载中……</p>
        </div>
    </main>
<script src="https://cdn.bootcss.com/vue/2.6.11/vue.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/leancloud-storage@4.5.3/dist/av-min.js"></script>
<script src="/js/foxsay.js"></script>
</html>