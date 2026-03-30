(define-module (hole theme)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (hole site)
  #:use-module (hole builder blog)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-19)
  #:use-module (sxml match)
  #:use-module (sxml transform)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:use-module (syntax-highlight lisp)
  #:use-module (hole syntax-highlight clojure)
  #:use-module (hole syntax-highlight python)
  #:use-module (hole syntax-highlight javascript)
  #:export (comment-place
            parse-read-more
            fox-theme))

(define build-date (current-date))

(define (maybe-highlight-code lang source)
  (let ((lexer (match lang
                 ('clojure lex-clojure)
                 ('scheme lex-scheme)
                 ('lisp lex-lisp)
                 ('elisp lex-lisp)
                 ('emacs-lisp lex-lisp)
                 ('python lex-python)
                 ('javascript lex-javascript)
                 ;; TODO
                 ('hy lex-lisp)
                 (_ #f))))
    (if lexer
        (highlights->sxml (highlight lexer source))
        source)))

(define (highlight-code . tree)
  (sxml-match tree
              ((code (@ (class ,class) . ,attrs) ,source)
               (let ((lang (string->symbol
                            (string-drop class (string-length "language-")))))
                 `(code (@ ,@attrs)
                   ,(maybe-highlight-code lang source))))
              (,other other)))

(define (%hole-sxml-rules post)
  `((code . ,highlight-code)
    (*text* . ,(lambda (tag str) str))
    (*default* . ,(lambda (. arg) arg))))

(define* (post-process-sxml sxml #:key post)
  (pre-post-order sxml (%hole-sxml-rules post)))

(define navbar
  '(nav (@ (class "nav"))
    (a (@ (class "skip-link") (href "#main-content")) "跳到主要内容")
    (a (@ (href "/")
          (class "brand")
          (aria-label "狐狸反走矣 - 返回首页"))
       (span "狐狸反走矣"))
    (input (@ (id "bmenub")
              (type "checkbox")
              (class "show")
              (aria-label "打开菜单")))
    (label (@ (for "bmenub")
              (class "burger pseudo button")
              (aria-label "菜单符号"))
           ☰)
    (div (@ (class "menu"))
         (a (@ (href "/archives/")) "归档")
         (a (@ (href "/tags/")) "标签")
         (a (@ (href "/search/")) "搜索")
         (a (@ (href "/about/")) "关于")
         )))

(define footer
  `(footer (@ (class "footer"))
    (div (@ (class "copyright"))
         (div (p "© SouthFox "
                 ,(number->string (date-year build-date))
                 " ,Font by "
                 (a (@ (href "https://github.com/SolidZORO/zpix-pixel-font"))
                    "Zpix")))
         (div (p "Power by "
                 (a (@ (href "https://dthompson.us/projects/haunt.html"))
                    "haunt")
                 " ,source can be found "
                 (a (@ (href "https://git.southfox.me/southfox/blog"))
                    "here"))))))

(define (parse-post-toc post)
  (let loop ((sxml (post-sxml post))
             (result '()))
    (match sxml
      (() (reverse result) )
      ((`(,hl (@ (id ,hid)) ,headline)  tail ...)
       (if (char=? #\h (string-ref (symbol->string hl) 0))
           (loop tail (cons `(li (a (@ (href ,(string-append "#" hid)))
                                    ,(if (string? headline)
                                         headline
                                         (car (last-pair headline)))))
                            result))
           (loop tail result)))
      ((head . tail)
       (loop tail result)))))

(define* (sidebar #:key post)
  `(div (@ (class "sidebar"))
    (div (@ (class "widget"))
         (h4 "链接")
         (ul
          (li (a (@ (href "/friends/"))
                 "友链"))
          (li (a (@ (href "https://www.travellings.cn/train.html"))
                 "开往"))
          (li (a (@ (href "https://foreverblog.cn/go.html"))
                 "虫洞"))
          (li (a (@ (href "/feed.xml"))
                 "订阅（Atom)"))
          (li (a (@ (href "/rss2.xml"))
                 "订阅（Rss)"))
          (li (a (@ (href "https://www.lisperati.com/logo.html"))
                 (img (@ (src "/assets/img/lisplogo_warning2_128.png")
                         (alt "Lisp logo warning")))))
          (li (a (@ (href "https://www.fsf.org/appeal"))
                 (img (@ (src "/assets/img/6838639.png")
                         (alt "FSF appeal"))))))
         (div
          (a (@ (href "https://xn--sr8hvo.ws/previous"))
             "←")
          (a (@ (href "https://xn--sr8hvo.ws"))
             "IndieWeb Webring 💍")
          (a (@ (href "https://xn--sr8hvo.ws/next"))
             "→"))
         (div
          (a (@ (href "https://fediring.net/previous?host=blog.southfox.me"))
             "←")
          (a (@ (href "https://fediring.net"))
             "Fediring 💍")
          (a (@ (href "https://fediring.net/next?host=blog.southfox.me"))
             "→")))
    (div (@ (class "widget"))
         (h4 "设置")
         (div
          (label (@ (for theme-select)) "主题设置：")
          (select (@ (id "theme-select"))
                  (option (@ (value "plain")) "普通")
                  (option (@ (value "386")) "386")
                  (option (@ (value "frutiger-aero")) "Frutiger Aero")
                  ))
         (div
          (label (@ (for font-select)) "字体设置：")
          (select (@ (id "font-select"))
                  (option (@ (value "plain")) "普通")
                  (option (@ (value "zpix")) "像素"))))
    ,(if post
         `(div (@ (class "widget"))
           (h4 "标签")
           (ul
            ,@(map (lambda (tag)
                     `(li (a (@ (href ,(string-append "/tag/" tag)))
                             ,tag)))
                   (post-tags post))))
         '())
    ,(if post
         (let ((post-backlinks (reverse (post-ref post 'backlinks))))
           (if (not (nil? post-backlinks))
               `(div (@ (class "widget"))
                 (h4 "反向链接")
                 (ul
                  ,@(map (lambda (link)
                           `(li ,link))
                         (post-ref post 'backlinks))))
               '()))
         '())
    ,(if post
         `(div (@ (class "widget"))
           (h4 "目录")
           (ul
            ,(parse-post-toc post)))
         '())))

(define* (fox-default-layout site title body #:key post)
  `((doctype "html")
    (html (@ (lang "zh-CN"))
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
      (meta (@ (property "og:image")
               (content "https://blog.southfox.me/favicon.png")))
      (meta (@ (name "fediverse:creator")
               (content "SouthFox@foxsay.southfox.me")))
      (title ,(string-append title " — " (site-title site)))
      (link (@ (rel "me")
               (href "https://foxsay.southfox.me/@SouthFox")))
      (link (@ (rel "me")
               (href "https://codeberg.org/southfox")))
      (script
       (raw "document.documentElement.setAttribute('data-theme', localStorage.getItem('theme') || '386');
      document.documentElement.setAttribute('data-font', localStorage.getItem('font') || 'zpix');"))
      (link (@ (rel "stylesheet")
               (href "/assets/css/main.css")))
      (script (@ (src "/assets/js/lips.min.js") (defer "defer")))
      (script (@ (type "text/x-scheme") (defer "defer"))
              (lips (let ((theme (or (localStorage.getItem "theme") "386"))
                          (font (or (localStorage.getItem "font") "zpix")))
                      (let ((theme-select (document.getElementById "theme-select"))
                            (font-select (document.getElementById "font-select")))
                        (define (string=? a b)
                          (== (a.cmp b) 0))
                        (define (loop-set options value)
                          (let loop ((i 0))
                            (if (> i (- (length options) 1))
                                i
                                (let ((option (get options i)))
                                  (if (string=? option.value value)
                                      (option.setAttribute "selected" #t)
                                      (loop (+ 1 i)))))))
                        (loop-set theme-select.options theme)
                        (loop-set font-select.options font)
                        (define (set-theme theme-name)
                          (localStorage.setItem "theme" theme-name)
                          (document.documentElement.setAttribute "data-theme" theme-name))
                        (define (set-font font-name)
                          (localStorage.setItem "font" font-name)
                          (document.documentElement.setAttribute "data-font" font-name))
                        (theme-select.addEventListener
                         "change"
                         (lambda (event)
                           (set-theme event.target.value)))
                        (font-select.addEventListener
                         "change"
                         (lambda (event)
                           (set-font event.target.value)))))))
      ,(if (and post (post-ref post 'lips))
           (map (lambda (script)
                  `(script (@ (src ,(string-append "/assets/lips/" script))
                              (type "text/x-scheme")
                              (bootstrap ""))))
                (map string-trim-both (string-split (post-ref post 'lips) #\:)))
           '())

      (link (@ (rel "alternate")
               (href "/feed.xml")
               (title ,(site-title site))
               (type "application/atom+xml")))
      (link (@ (rel "alternate")
               (href "/rss2.xml")
               (title ,(site-title site))
               (type "application/rss+xml"))))
     (body
      ,navbar
      (div (@ (class "container flex"))
           ,(post-process-sxml body #:post post)
           ,(sidebar #:post post))
      ,footer))))

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
        (async "")))))

(define* (fox-default-post-template post #:key previous-post next-post)
  `(div (@ (class "content"))
    (main (@ ,(if (equal? (post-ref post 'feed-only) "t")
                  '(data-pagefind-ignore "true")
                  '(data-pagefind-body "true"))
             (id "main-content") (tabindex "-1"))
          (div
           (h1 (@ (id "post-title")) ,(post-ref post 'title))
           (p (@ (class "post-meta"))
              (span "by " ,(post-ref post 'author))
              (time (@ (data-pagefind-sort "date"))
                    ,(date->string (post-date post) "~Y-~m-~d")))
           (div ,(post-sxml post))))
    (div (@ (class "pagination"))
         ,(if previous-post
              `(a (@ (class "btn")
                     (href ,previous-post))
                "← 上一页")
              `(a (@ (class "btn disabled"))
                "← 上一页"))
         (a (@ (class "btn") (href "/")) " 主页 ")
         ,(if next-post
              `(a (@ (class "btn")
                     (href ,next-post))
                "下一页 →")
              `(a (@ (class "btn disabled"))
                "下一页 →")))
    ,(if (equal? (post-ref post 'feed-only) "t")
         '()
         (comment-place (hexo-post-slug post)))))

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
    (hole/uri-encode (string-append
                      (or prefix "")
                      (site-post-slug site post))))
  `(div (@ (class "content"))
    (main (@ (id "main-content") (tabindex "-1"))
          (h2 ,title)
          ,@(map (lambda (post)
                   `((h3
                      (a (@ (href ,(post-uri post)))
                         ,(post-ref post 'title)
                         " — "
                         ,(date->string (post-date post) "~Y-~m-~d")))
                     (div (@ (class "post"))
                          ,(parse-read-more post))))
                 posts))))

(define (fox-default-pagination-template site body previous-page next-page)
  `((,@body
     (div (@ (class "pagination"))
          ,(if previous-page
               `(a (@ (class "btn")
                      (href ,previous-page))
                 "← 上一页")
               `(a (@ (class "btn disabled"))
                 "← 上一页"))
          (a (@ (class "btn") (href "/")) " 主页 ")
          ,(if next-page
               `(a (@ (class "btn")
                      (href ,next-page))
                 "下一页 →")
               `(a (@ (class "btn disabled"))
                 "下一页 →"))))))

(define fox-theme
  (theme #:name "Fox"
         #:layout fox-default-layout
         #:post-template fox-default-post-template
         #:collection-template fox-default-collection-template
         #:pagination-template fox-default-pagination-template))
