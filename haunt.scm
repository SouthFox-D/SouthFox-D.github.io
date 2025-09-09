(use-modules (haunt asset)
             (haunt post)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (hole theme))

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:readers (list commonmark-reader)
      #:posts-directory "source/_posts"
      #:builders (list (blog
                        #:theme fox-theme
                        #:collections `(("Fox" "index.html" ,posts/reverse-chronological))
                        )
                       (atom-feed)
                       (static-directory "assets")
                       ))
