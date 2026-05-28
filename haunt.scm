(use-modules (haunt asset)
             (haunt post)
             (haunt builder assets)
             (haunt reader commonmark)
             (haunt site)
             (haunt utils)
             (hole theme)
             (hole reader)
             (hole site)
             (hole i18n)
             (hole builder atom)
             (hole builder blog)
             (hole builder rss)
             (hole builder page)
             (hole builder tag)
             )

(setenv "LANG" "C.UTF-8")

(define (group-by key-proc lst)
  (let ((table (make-hash-table)))
    (for-each (lambda (item)
                (let ((k (key-proc item)))
                  (hash-set! table k (cons item (hash-ref table k '())))))
              lst)
    (hash-map->list cons table)))

(define (%base-transformers)
  (append
   (if (equal? (getenv "BUILD_DRAFTS") "t")
       '()
       (list filter-draft))))

(define (with-transformers specific-transformers . builders)
  (apply wrap-builders
         (append (%base-transformers) specific-transformers)
         builders))

(define (wrap-lang proc)
  (lambda (site posts)
    (let* ((default-lang (or (assoc-ref (site-default-metadata site) 'lang) "zh-CN"))
           (groups-alist (group-by (lambda (p) (or (post-ref p 'lang) default-lang)) posts)))
      (flat-map (lambda (pair)
                  (let ((lang (car pair))
                        (lang-posts (cdr pair)))
                    (parameterize ((blog-language lang))
                      (proc site lang-posts))))
                groups-alist))))

(define (site-builders)
  (with-transformers
   (list filter-feed-only)
   (blog/collection->page
    #:theme (fox-theme)
    #:collections `((,(t_ 'collections-titles) "index.html" ,posts/reverse-chronological))
    #:posts-per-page 10)
   (wrap-lang
    (lambda (site posts)
      ((blog/collection->page
        #:theme (fox-theme)
        #:prefix (blog-language)
        #:collections `((,(t_ 'collections-titles) "index.html" ,posts/reverse-chronological))
        #:posts-per-page 10)
       site posts)))
   (about-page)
   (friends-page)
   (archives-page)
   (tag-page)
   (tags->page)
   (search-page)
   (guestbook-page)
   (static-directory "assets")
   (static-directory "_site" "/")))

(define (post-builders)
  (with-transformers
   (list reverse-chronological-posts
         inject-backlinks
         inject-feed-only-section
         inject-expire-warning-section)
   (wrap-lang
    (blog/post->page #:theme (fox-theme)))
   (hole/atom-feed)
   (hole/rss-feed)))

(site #:title "狐狸反走矣"
      #:domain "blog.southfox.me"
      #:default-metadata
      '((author . "SouthFox")
        (email  . "master@southfox.me")
        (lang   . "zh-CN"))
      #:posts-directory "posts"
      #:readers (list fox-commonmark-reader fox-org-mode-reader)
      #:builders (list (site-builders) (post-builders))
      #:make-slug hexo-post-slug)
