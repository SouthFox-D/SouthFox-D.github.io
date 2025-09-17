(define-module (hole theme)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (hole blog)
  #:export (comment-place
            parse-read-more
            fox-theme))

(define navbar
  '(nav (@ (class "nav"))
    (a (@ (href "/")
          (class "brand"))
       (span "狐狸反走矣"))
    (input (@ (id "bmenub")
              (type "checkbox")
              (class "show")))
    (label (@ (for "bmenub")
              (class "burger pseudo button"))
           ☰)
    (div (@ (class "menu"))
         (a (@ (href "/archives/")) "归档")
         (a (@ (href "/tags/")) "标签")
         (a (@ (href "/about/")) "关于")
         )))

(define footer
  `(footer (@ (class "footer"))
    (div (@ (class "copyright"))
         (div (p "© SouthFox "
                 ,(number->string (date-year (current-date)))
                 " ,Font by "
                 (a (@ (href "https://github.com/SolidZORO/zpix-pixel-font"))
                    "Zpix")))
         (div (p "Power by "
                 (a (@ (href "https://dthompson.us/projects/haunt.html"))
                    "haunt")
                 " ,source can be found "
                 (a (@ (href "https://git.southfox.me/southfox/blog"))
                    "here"))))))

(define* (sidebar #:key post)
  `(div (@ (class "sidebar"))
    (div (@ (class "widget"))
         (h4 "公告")
         (p "博客开发中……")
         (ul
          (li "导航栏 DONE")
          (li "标签 DONE")
          (li "代码高亮 TODO")
          (li "按钮样式 TODO")
          (li "文章内上一篇下一篇导航 TODO")))
    (div (@ (class "widget"))
         (h4 "链接")
         (ul
          (li (a (@ (href "/friends/"))
                 "友链"))
          (li (a (@ (href "https://www.travellings.cn/train.html"))
                 "开往"))
          (li (a (@ (href "https://foreverblog.cn/go.html"))
                 "虫洞"))
          (li (a (@ (href "/rss2.xml"))
                 "Rss"))
          (li (a (@ (href "https://www.fsf.org/appeal"))
                 (img (@ (src "/assets/img/6838639.png")))))))
    ,(if post
         `(div (@ (class "widget"))
           (h4 "标签")
           (ul
            ,@(map (lambda (tag)
                     `(li (a (@ (href ,(string-append "/tags/" tag)))
                             ,tag)))
                   (post-tags post))))
         '())))

(define* (fox-default-layout site title body #:key post)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1")))
      ,(if post
           `(meta (@ (property "og:title")
                     (content
                      ,(string-append (post-ref post 'title)
                                      " — "
                                      (site-title site)))))
           '())
      (meta (@ (property "og:site_name")
               (content ,(site-title site))))
      (title ,(string-append title " — " (site-title site)))
      (link (@ (rel "stylesheet")
               (href "/assets/css/main.css"))))
     (body
      ,navbar
      (div (@ (class "container flex"))
           ,body
           ,(sidebar #:post post))
      ,footer))))

(define (get-comment-term post)
  (let* ((%post-list (cdr (string-split (post-file-name post) #\/)))
          (post-list (if (equal? "_posts" (car %post-list))
                         (cdr %post-list)
                         %post-list))
          (list-length (length post-list)))
     (string-join (append (list-head post-list (- list-length 1))
                          (list (string-replace-substring
                                 (list-ref post-list (- list-length 1))
                                 ".md"
                                 "/")))
                  "/")))

(define (comment-place comment-term)
  `(div
    (@ (class "comment"))
    (blockquote
     "如不想授权 Giscus 应用，也可以点击下方"
     (strong "左上角数字")
     "直接跳转到 Github Discussions 进行评论。")
    (script
     (@ (src "https://giscus.app/client.js")
        (data-repo "SouthFox-D/SouthFox-D.github.io")
        (data-repo-id "MDEwOlJlcG9zaXRvcnkyMjg3NDM0MjQ=")
        (data-category "博客评论")
        (data-category-id "DIC_kwDODaJZAM4CA7bf")
        (data-mapping "specific")
        (data-term ,comment-term)
        (data-reactions-enabled "0")
        (data-emit-metadata "0")
        (data-input-position "top")
        (data-theme "dark_dimmed")
        (data-lang "zh-CN")
        (data-loading "lazy")
        (crossorigin "anonymous")
        (async "true")))))

(define (fox-default-post-template post)
  `(div (@ (class "content"))
    (div
     (h2 ,(post-ref post 'title))
     (h3 "by " ,(post-ref post 'author)
         " — " ,(date->string (post-date post) "~Y-~m-~d"))
     (div ,(post-sxml post)))
    ,(comment-place (get-comment-term post))))

(define (parse-read-more post)
  (let loop ((sxml (post-sxml post))
             (result '()))
    (match sxml
      (() (car (post-sxml post)))
      (('(span (@ (id "more"))) _ ...)
       (reverse result))
      ((head . tail)
       (loop tail (cons head result))))))

(define (fox-default-collection-template site title posts prefix)
  (define (post-uri post)
    (string-append (or prefix "") "/"
                   (site-post-slug site post) ".html"))
  `(div (@ (class "content"))
    (h2 ,title)
    ,@(map (lambda (post)
             `((h3
                (a (@ (href ,(post-uri post)))
                   ,(post-ref post 'title)
                   " — "
                   ,(date->string (post-date post) "~Y-~m-~d")))
               (div (@ (class "post"))
                    ,(parse-read-more post))))
           posts)))

(define (fox-default-pagination-template site body previous-page next-page)
  `((,@body
     (div (@ (class "pagination"))
          ,(if previous-page
               `(a (@ (href ,previous-page)) "← 上一页")
               '())
          (a (@ (href "/")) " 主页 ")
          ,(if next-page
               `(a (@ (href ,next-page)) "下一页 →")
               '())))))

(define fox-theme
  (theme #:name "Fox"
         #:layout fox-default-layout
         #:post-template fox-default-post-template
         #:collection-template fox-default-collection-template
         #:pagination-template fox-default-pagination-template))
