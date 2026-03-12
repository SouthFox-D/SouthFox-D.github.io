(define-module (hole site)
  #:use-module (hole sxml)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-1)
  #:use-module ((sxml xpath) #:select (sxpath))
  #:use-module (ice-9 string-fun)
  #:use-module (web uri)
  #:export (hexo-post-slug
            hole/uri-encode
            wrap-builders
            inject-backlinks
            filter-feed-only
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

(define (post-set post metadata)
  (make-post (post-file-name post)
             (append metadata (post-metadata post))
             (post-sxml post)))

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
                                        (let ((target-slug (sxml-attribute-ref 'href link-node)))
                                          (hash-set! backlink-map target-slug
                                                     (cons `(a (@ (href ,source-target)) ,(post-ref source-post 'title))
                                                           (hash-ref backlink-map target-slug '())))))
                                      links))))
                      posts))
         (enriched-posts (map (lambda (post)
                                (let* ((slug (site-post-slug site post))
                                       (bls (hash-ref backlink-map slug '())))
                                  (post-set post `((slug . ,slug)
                                                   (backlinks . ,(delete-duplicates bls))))))
                              posts)))
      (cons site enriched-posts)))

(define (filter-feed-only site posts)
  (let ((filtered (filter (lambda (p)
                            (not (equal? (post-ref p 'feed-only) "t")))
                          posts)))
    (cons site filtered)))

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
