(use-modules (haunt asset)
             (haunt post)
             (haunt builder atom)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (ice-9 string-fun)
             (hole blog)
             (hole theme)
             (hole reader))


(define (hexo-post-slug post)
  (or (post-ref post 'slug)
      (let* ((%post-list (cdr (string-split (post-file-name post) #\/)))
             (post-list (if (equal? "_posts" (car %post-list))
                            (cdr %post-list)
                            %post-list))
             (list-length (length post-list)))
        (string-join (append (list-head post-list (- list-length 1))
                             (list (string-replace-substring
                                    (list-ref post-list (- list-length 1))
                                    ".md"
                                    "/index")))
                     "/"))))

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:readers (list fox-commonmark-reader)
      #:posts-directory "source/_posts"
      #:builders (list (hole/blog
                        #:theme fox-theme
                        #:collections `(("最近文章" "index.html" ,posts/reverse-chronological))
                        #:posts-per-page 10)
                       (atom-feed)
                       (static-directory "assets"))
      #:make-slug hexo-post-slug)
