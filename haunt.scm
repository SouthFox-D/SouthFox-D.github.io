(use-modules (haunt asset)
             (haunt post)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (hole theme)
             (hole reader)
             (hole site)
             (hole builder atom)
             (hole builder blog)
             (hole builder rss)
             (hole builder page)
             (hole builder tag)
             )

(setenv "LANG" "C.UTF-8")

(define site-builders
  (wrap-builders
   (list filter-feed-only)
   (blog/collection->page
    #:theme fox-theme
    #:collections `((("最近文章" "没那么近文章" "有点老文章" "尘封文章" "古旧文章" "可以说是黑历史的文章")
                     "index.html" ,posts/reverse-chronological))
    #:posts-per-page 10)
   (about-page)
   (friends-page)
   (archives-page)
   (tag-page)
   (tags->page)
   (search-page)
   (guestbook-page)
   (hole/atom-feed)
   (hole/rss-feed)
   (static-directory "assets")
   (static-directory "_site" "/")))

(define post-builders
  (wrap-builders
   (list reverse-chronological-posts
         inject-backlinks
         inject-feed-only-section
         inject-expire-warning-section)
   (blog/post->page #:theme fox-theme)))

(define feed-builders
  (list (hole/atom-feed)
        (hole/rss-feed)))

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me"))
      #:posts-directory "posts"
      #:readers (list fox-commonmark-reader fox-org-mode-reader)
      #:builders (cons* site-builders post-builders feed-builders)
      #:make-slug hexo-post-slug)
