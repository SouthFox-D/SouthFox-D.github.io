(define-module (hole theme)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt builder blog)
  #:use-module (srfi srfi-19)
  #:export (fox-theme))

(define (ugly-default-layout site title body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title ,(string-append title " — " (site-title site)))
      (link (@ (rel "stylesheet")
               (href "/assets/css/main.css"))))
     (body
      (h1 ,(site-title site))
      ,body))))

(define (fox-default-post-template post)
  `((div (@ (class "container") )
     (h2 ,(post-ref post 'title))
     (h3 "by " ,(post-ref post 'author)
         " — " ,(date->string (post-date post) "~Y-~m-~d"))
     (div ,(post-sxml post)))))

(define (fox-default-collection-template site title posts prefix)
  (define (post-uri post)
    (string-append (or prefix "") "/"
                   (site-post-slug site post) ".html"))
  `((h3 ,title)
    (ul
     ,@(map (lambda (post)
              `(li
                (a (@ (href ,(post-uri post)))
                   ,(post-ref post 'title)
                   " — "
                   ,(date->string* (post-date post)))))
            posts))))

(define (fox-default-pagination-template site body previous-page next-page)
  `(,@body
    (div
     ,(if previous-page
          `(a (@ (href ,previous-page)) "← Previous")
          '())
     " — "
     ,(if next-page
          `(a (@ (href ,next-page)) "Next →")
          '()))))

(define fox-theme
  (theme #:name "Fox"
         #:layout ugly-default-layout
         #:post-template fox-default-post-template
         #:collection-template fox-default-collection-template
         #:pagination-template fox-default-pagination-template))
