(define-module (hole site)
  #:use-module (hole sxml)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module ((sxml xpath) #:select (sxpath))
  #:use-module (ice-9 string-fun)
  #:use-module (web uri)
  #:export (hexo-post-slug
            hole/uri-encode
            wrap-builders
            inject-backlinks
            inject-feed-only-section
            filter-feed-only
            filter-draft
            inject-expire-warning-section
            reverse-chronological-posts
            ))

(define ascii-alnum-chars
  (string->char-set
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

(define unreserved-chars
  (char-set-union ascii-alnum-chars
                  (string->char-set "-._~")
                  (string->char-set "/")))

(define (hole/uri-encode str)
  (uri-encode str #:unescaped-chars unreserved-chars))

(define (hexo-post-slug post)
  (or (post-ref post 'slug)
      (let* ((%post-list (cdr (string-split (post-file-name post) #\/)))
             (post-list (if (equal? "posts" (car %post-list))
                            (cdr %post-list)
                            %post-list))
             (list-length (length post-list)))
        (string-append
         "/"
         (string-join (append (list-head post-list (- list-length 1))
                              (list (string-replace-substring
                                     (string-replace-substring
                                      (list-ref post-list (- list-length 1))
                                      ".md" "/")
                                     ".org" "/")))
                      "/")))))

(define (post-set post metadata p-sxml)
  (make-post (post-file-name post)
             (append metadata (post-metadata post))
             p-sxml))

(define (extract-links sxml)
  (let ((query (sxpath
                `(// a ,(lambda (node)
                          (if (and
                               (not (equal? (sxml-attribute-ref 'class node ) "external_link"))
                               (string-prefix? "/20" (or (sxml-attribute-ref 'href node) "")))
                              node '()))))))
    (query sxml)))

(define (inject-backlinks site posts)
  (let* ((backlink-map (make-hash-table))
         (_ (for-each (lambda (source-post)
                        (when (not (equal? (post-ref source-post 'feed-only) "t"))
                          (let ((links (extract-links (post-sxml source-post)))
                                (source-target (site-post-slug site source-post)))
                            (for-each (lambda (link-node)
                                        (let ((target-slug (uri-path
                                                            (string->relative-ref
                                                             (sxml-attribute-ref 'href link-node)))))
                                          (hash-set! backlink-map target-slug
                                                     (cons `(a (@ (href ,source-target)) ,(post-ref source-post 'title))
                                                           (hash-ref backlink-map target-slug '())))))
                                      links))))
                      posts))
         (enriched-posts (map (lambda (post)
                                (let* ((slug (site-post-slug site post))
                                       (bls (hash-ref backlink-map slug '())))
                                  (post-set post `((slug . ,slug)
                                                   (backlinks . ,(delete-duplicates bls)))
                                            (post-sxml post))))
                              posts)))
      (cons site enriched-posts)))

(define (feed-only? post)
  (equal? (post-ref post 'feed-only) "t"))

(define (inject-feed-only-section site posts)
  (cons site
        (map (lambda (p)
               (if (feed-only? p)
                   (post-set p '()
                             (cons '(section "这是一篇在公开页面找不到的 Feed 专享文章，就让这篇文章成为公开的秘密吧……")
                                   (post-sxml p)))
                   p))
             posts)))

(define (filter-feed-only site posts)
  (let ((filtered (filter (lambda (p)
                            (not (equal? (post-ref p 'feed-only) "t")))
                          posts)))
    (cons site filtered)))

(define (filter-draft site posts)
  (let ((filtered (filter (lambda (p)
                            (not (equal? (post-ref p 'draft) "t")))
                          posts)))
    (cons site filtered)))

(define build-date (current-date))
(define expire-days 720)
(define (post-expire-warning lisp?)
  (let* ((base-warning (format #f "此篇是技术文章并距离当前构建时间超过 ~s 天，内容可能已经过时。" expire-days))
           (lisp-note "（不过，鉴于这是 Lisp 相关内容，可能也没那么容易过时。）")
           (full-warning (if lisp? (string-append base-warning lisp-note) base-warning)))
    `(section ,full-warning)))

(define (inject-expire-warning-section site posts)
  (cons site
        (map (lambda (post)
               (let* ((tags (or (post-ref post 'tags) '()))
                      (tech-post? (member "技术" tags))
                      (lisp-post? (member "Lisp" tags))
                      (diff-seconds (time-second (time-difference
                                                  (date->time-utc build-date)
                                                  (date->time-utc (post-date post)))))
                      (should-show-warning? (and tech-post?
                                                 (> diff-seconds (* 60 60 24 expire-days))))
                      (p-sxml (if should-show-warning?
                                  (cons (post-expire-warning lisp-post?) (post-sxml post))
                                  (post-sxml post))))
                 (if should-show-warning?
                     (post-set post '()
                               (cons (post-expire-warning lisp-post?)
                                     (post-sxml post)))
                     post)))
             posts)))

(define (reverse-chronological-posts site posts)
  (cons site
        (posts/reverse-chronological posts)))

(define (wrap-builders transformers . builders)
  (lambda (site posts)
    (let* ((result (fold (lambda (transformer acc)
                           (transformer (car acc) (cdr acc)))
                         (cons site posts)
                         transformers))
           (final-site (car result))
           (final-posts (cdr result)))
      (flat-map (lambda (builder)
                  (builder final-site final-posts))
                builders))))
