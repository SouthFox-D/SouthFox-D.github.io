(define-module (hole site)
  #:use-module (haunt post)
  #:use-module (ice-9 string-fun)
  #:use-module (web uri)
  #:export (hexo-post-slug
            hole/uri-encode))

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
        (string-join (append (list-head post-list (- list-length 1))
                             (list (string-replace-substring
                                    (string-replace-substring
                                     (list-ref post-list (- list-length 1))
                                     ".md" "/")
                                    ".org" "/")))
                     "/"))))
