(define-module (hole site)
  #:use-module (haunt post)
  #:use-module (ice-9 string-fun)
  #:export (hexo-post-slug))

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
