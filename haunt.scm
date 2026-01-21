(use-modules (haunt asset)
             (haunt post)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (hole blog)
             (hole theme)
             (hole reader)
             (hole rss)
             (hole atom)
             (hole page)
             (hole tags)
             (hole site)
             )

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:posts-directory "posts"
      #:readers (list fox-commonmark-reader fox-org-mode-reader)
      #:builders (list (hole/blog
                        #:theme fox-theme
                        #:collections `((("最近文章" "没那么近文章" "有点老文章" "尘封文章" "古旧文章" "可以说是黑历史的文章")
                                         "index.html" ,posts/reverse-chronological))
                        #:posts-per-page 10)
                       (about-page)
                       (friends-page)
                       (archives-page)
                       (tags-page)
                       (tags->page)
                       (search-page)
                       (hole/atom-feed)
                       (hole/rss-feed)
                       (static-directory "assets")
                       (static-directory "_site" "/"))
      #:make-slug hexo-post-slug)
