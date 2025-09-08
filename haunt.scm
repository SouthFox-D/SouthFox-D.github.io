(use-modules (haunt asset)
             (haunt post)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site))

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:readers (list commonmark-reader)
      #:posts-directory "source/_posts"
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)
                       ;; (static-directory "images")
                       ))
