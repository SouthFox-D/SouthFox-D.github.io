(define-module (hole page)
  #:use-module (haunt site)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (hole html)
  #:use-module (hole blog)
  #:use-module (hole theme)
  #:use-module (hole tags)
  #:use-module (hole site)
  #:use-module (ice-9 match)
  #:export (static-page
            about-page
            friends-page
            archives-page
            tags-page
            search-page))

(define (static-page title file-name body)
  (lambda (site posts)
    (make-page file-name
               (with-layout fox-theme site title `(div (@ (class "content")) ,body))
               sxml->html)))

(define* (about-page)
  (static-page
   "关于"
   "about/index.html"
   `((h2 "关于")
     (p "欢迎点击这个页面，不管是自动执行的程序还是有血有肉的生物。在互联网闲逛到一些个人博客时，我通常都会首先点开个人博客
（尤其是首页都是些高深技术文章的那种博客）以便来了解博主，但想想这样做有点不太「公平」，毕竟我的博客还没有一个关于页面。")
     (blockquote "所以现在（2023-04-28）我就写了。")
     (p "但虽说是关于页面，但我感觉也没什么好说的，或许我应该像维基百科的一些人一样，罗列出很多很多标签框，一框一框地勾画出
自己。但我是个懒人，所以就不打算这么干了。")
     (p "不过根据以上的文字，其实就能大致看出我来了，一个迷迷糊糊不知所云想到什么就说什么随处可见的「狐狸」。")
     (h3 "我的果壳")
     (p "我现在一般都在" (code "联邦宇宙")
        "中待着，如果还不知道「联邦宇宙」是什么。那么很简单，其实电子邮箱就是一个" (code "联邦宇宙")
        "（联邦宇宙定义九宫格.jpg）。")
     (h4 "电子邮箱")
     (p "我的邮箱：" (code "master[at]southfox.me"))
     (h4 "ActivityPub")
     (p "互联网混沌未开诞生出的电子邮件是可以互联的，那为什么现在几十年过去了，人们却只能在一些说不清道不明的「邪恶实体」
的围墙花园上待着呢？")
     (p "所以像是电子邮件一样的 "
        (code "ActivityPub")
        " 协议出现了，我所运行的软件是 "
        (code "Mastodon")
        " ，具体帐号是：")
     (code "[at]SouthFox[at]foxsay.southfox.me")
     (p "如果说大众常用的社交软件像是大城市，那么现在由 " (code "ActivityPub") " 协议组成的社交网络就是城郊，一些比较「富庶」
的村落可能也会很热闹。那么我自己维护的实例就是一个偏僻角落的小屋，到了晚上周围将会是一点光亮都没有，不过我依然感到自在。")
     (h4 "Matrix")
     (p "并不是那个有着草莓味和蓝莓味胶囊的 " (code "Matrix") " ，是聊天协议"
        (code Matrix) "，不选 " (code XMPP)
        " 主要还是因为……像是抓阄一样选择的就选到了 " (code "Matrix") " 就这么待着了，仅此而已。")
     (p "我的 " (code Matrix) " 帐号是：" )
     (code "[at]southfox:southfox.me")
     (p "欢迎啊，欢迎啊！在黄昏时我通常都会站在我的站点「门口」，用上可以给五头奶牛挤奶的时间等待客人的到来，可惜通常没有人联系我。")
     (p "不过大多数时候我还是希望有人来的，愿能在更加开放的互联网上相遇吧。")
     (br)
     ,(comment-place "about/index.html"))))

(define* (make-friend name link #:optional (description ""))
  `(li (a (@ (href ,link)) ,(string-append name
                                           (if (string= description "")
                                               ""
                                               " | ")
                                           description))))

(define (make-friend-group title link-group)
  `((h2 ,title)
    (ul
     ,@(map (lambda (link)
              (apply make-friend link))
            link-group))))

(define (friends-page)
  (static-page
   "Friends"
   "friends/index.html"
   `((h1 "Friends")
     ,(make-friend-group
       "联邦星球"
       '(("suica 的博客" "https://suicablog.cobaltkiss.blue")
         ("小球飞鱼" "https://mantyke.icu" "我们会遇见鲸鱼吗？")
         ("此方方有限公司" "https://blog.konata.vip" "Everything is interesting if you go into it deeply enough.")
         ("Yoozy" "http://woods.sharktale.xyz" "一花一世界，一叶一菩提")
         ("瓠樽" "https://blog.dylanwu.space" "以瓠為樽而浮乎江湖")
         ("江尚寒" "https://jiangshanghan.art.blog" "一潭星动")
         ("关门说话" "https://shutgnblink.blog/" "一只生活在海底的哺乳动物")))
     ,(make-friend-group
       "万维世界"
       '(("王小嗨" "https://sogola.com" "A Marxist inside.")
         ("浮云翩迁之间" "https://blognas.hwb0307.com" "百代繁华一朝都，谁非过客；千秋明月吹角寒，花是主人。")
         ("迷失的小K" "https://blog.kclub.tech" "Just for fun")
         ("L3ON" "https://l3on.site" "不可勝在己，可勝在敵")
         ("s0urce's Lab" "https://blog.src.moe" "No black and white in the blue.")
         ("JIPA233の小窝" "https://imjipa.top/" "Deed divides beings into lower and higher ones.")
         ("无叶之境" "https://lonleaf.com" "这是一个非常正常的网站，像教科书一样正常(๑•̀ω•́๑)")
         ("滑翔闪" "https://blog.huaxiangshan.com/zh-cn/" "正在学习经济学的二次元|")
         ("水气掠过" "https://dilutepillow.github.io/" "晚饭后，一起散步吧。")
         ("Pinpe 的云端" "https://pinpe.top" "一个属于自己的云朵")
         ("酥米的小站" "https://www.sumi233.top/" "终有一日，寻梦中人")
         ("躬行笔记" "https://www.fuzhoupyy.work" "古人学问无遗力，少壮工夫老始成。 纸上得来终觉浅，绝知此事要躬行。")
         ("Rebel Zhang 的个人网站" "https://rebel1725.codeberg.page/" "Rebel Zhang 的个人网站")
         ))
     ,(make-friend-group
       "失联之地"
       '(("Albert's Blog" "https://blog.lingyf.com" "逆水行舟 不进则退")
         ("歪皮" "http://www.gene-yp.com/" "Just love, understanding and positivity")))
     ,(comment-place "friends/index.html"))))

(define (archives-page)
  (lambda (site posts)
    (make-page "archives/index.html"
               (with-layout
                fox-theme
                site
                "Archives"
                `(div (@ (class "content"))
                  (h2 "归档")
                  (ul
                   ,(map (lambda (post)
                           `(li (a (@ (href ,(string-append
                                              "/"
                                              (hole/uri-encode (site-post-slug site post)))))
                                   ,(post-ref post 'title))))
                         (posts/reverse-chronological posts)))))
               sxml->html)))

(define (tags-page)
  (lambda (site posts)
    (make-page "tags/index.html"
               (with-layout
                fox-theme
                site
                "Tags"
                `(div (@ (class "content"))
                  (h2 "标签")
                  (ul
                   ,(map (match-lambda
                           ((tag count)
                            `(li (a (@ (class "tag")
                                       (href ,(hole/uri-encode (tag-uri tag))))
                                    ,tag ": " ,count))))
                         (count-tags posts)))))
               sxml->html)))

(define (search-page)
  (lambda (site posts)
    (make-page "search/index.html"
               (with-layout
                fox-theme
                site
                "Search"
                `((link (@ (href "/pagefind/pagefind-ui.css")
                           (rel "stylesheet")))
                  (script (@ (src "/pagefind/pagefind-ui.js")))
                  (div (@ (class "content"))
                       (h2 "Search")
                       (div (@ (id "search")))
                       (script (@ (src "/assets/js/init-search.js"))))))
               sxml->html)))
