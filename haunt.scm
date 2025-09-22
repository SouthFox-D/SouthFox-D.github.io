(use-modules (haunt asset)
             (haunt post)
             (haunt builder atom)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (hole blog)
             (hole theme)
             (hole reader)
             (hole rss)
             (hole page)
             (hole tags)
             (hole site)
             )

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:readers (list fox-commonmark-reader fox-org-reader)
      #:posts-directory "posts"
      #:builders (list (hole/blog
                        #:theme fox-theme
                        #:collections `(("最近文章" "index.html" ,posts/reverse-chronological))
                        #:posts-per-page 10)
                       (about-page)
                       (friends-page)
                       (archives-page)
                       (tags-page)
                       (tags->page)
                       (atom-feed)
                       (hole/rss-feed)
                       (static-directory "assets")
                       (static-directory "_size" "/"))
      #:make-slug hexo-post-slug)
