;;; Haunt --- Static site generator for GNU Guile
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of Haunt.
;;;
;;; Haunt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Haunt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Haunt.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Page builders
;;
;;; Code:

(define-module (hole blog)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:export (theme
            theme?
            theme-name
            theme-layout
            theme-post-template
            theme-collection-template
            theme-pagination-template
            with-layout
            render-post
            render-collection

            date->string*

            hole/blog))

(define-record-type <theme>
  (make-theme name layout post-template collection-template pagination-template)
  theme?
  (name theme-name)
  (layout theme-layout)
  (post-template theme-post-template)
  (collection-template theme-collection-template)
  (pagination-template theme-pagination-template))

(define (ugly-default-layout site title body)
  `((doctype "html")
    (head
     (meta (@ (charset "utf-8")))
     (title ,(string-append title " — " (site-title site))))
    (body
     (h1 ,(site-title site))
     ,body)))

(define (ugly-default-post-template post)
  `((h2 ,(post-ref post 'title))
    (h3 "by " ,(post-ref post 'author)
        " — " ,(date->string* (post-date post)))
    (div ,(post-sxml post))))

(define (ugly-default-collection-template site title posts prefix)
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

(define (ugly-default-pagination-template site body previous-page next-page)
  `(,@body
    (div
     ,(if previous-page
          `(a (@ (href ,previous-page)) "← Previous")
          '())
     " — "
     ,(if next-page
          `(a (@ (href ,next-page)) "Next →")
          '()))))

(define* (theme #:key
                (name "Untitled")
                (layout ugly-default-layout)
                (post-template ugly-default-post-template)
                (collection-template ugly-default-collection-template)
                (pagination-template ugly-default-pagination-template))
  (make-theme name layout post-template collection-template
              pagination-template))

(define* (with-layout theme site title body #:key post)
  ((theme-layout theme) site title body #:post post))

(define (render-post theme site post)
  ((theme-post-template theme) post))

(define (render-collection theme site title posts prefix)
  ((theme-collection-template theme) site title posts prefix))

(define (render-pagination theme site body previous-page next-page)
  ((theme-pagination-template theme) site body previous-page next-page))

(define (date->string* date)
  "Convert DATE to human readable string."
  (date->string date "~a ~d ~B ~Y"))

(define* (hole/blog #:key theme prefix post-prefix
               (collections
                `(("Recent Posts" "index.html" ,posts/reverse-chronological)))
               posts-per-page)
  "Return a procedure that transforms a list of posts into pages
decorated by THEME, whose URLs start with PREFIX.  Post pages may be
nested deeper in the file hierarchy than collection pages by
specifying the POST-PREFIX argument.

If POSTS-PER-PAGE is specified, collections will be broken up into
several pages with up to POSTS-PER-PAGE posts on each page."
  (define (make-file-name base-name)
    (string-append (or prefix "") (if prefix "/" "") base-name))

  (lambda (site posts)
    (define (post->page post)
      (let ((base-name (string-append (if post-prefix
                                          (string-append post-prefix "/")
                                          "")
                                      (site-post-slug site post)
                                      ".html"))
            (title (post-ref post 'title))
            (body ((theme-post-template theme) post)))
        (serialized-artifact (make-file-name base-name)
                             (with-layout theme site title body
                                          #:post post)
                             sxml->html)))

    (define (paginate base-name items)
      (define (make-page-file-name i)
        (make-file-name
         ;; First page does not get a page number added to the file
         ;; name.
         (if (= 0 i)
             (string-append base-name ".html")
             (string-append base-name "-"
                            (number->string i)
                            ".html"))))
      (define (make-page i items)
        (list (make-page-file-name i) (reverse items)))
      (let loop ((items items)
                 (n 0)
                 (i 0)
                 (page '()))
        (if (= n posts-per-page)
            (cons (make-page i page) (loop items 0 (+ i 1) '()))
            (match items
              (()
               (list (make-page i page)))
              ((item . rest)
               (loop rest (+ n 1) i (cons item page)))))))

    (define collection-post-prefix
      (if prefix
          (if post-prefix
              (string-append prefix "/" post-prefix)
              prefix)
          (or post-prefix "")))

    (define collection->page
      (match-lambda
        ((title file-name filtered-posts)
         ;; Earlier versions of Haunt, which did not have collection
         ;; pagination, told users to include a full file name, not
         ;; just a base name, so we continue to honor that style of
         ;; configuration.
         (let ((base-name (if (string-suffix? ".html" file-name)
                              (string-take file-name
                                           (- (string-length file-name) 5))
                              file-name)))
           (define (make-collection-page current-page prev-page next-page)
             (match current-page
               ((file-name posts)
                (let* ((coll-sxml (render-collection theme site title
                                                     posts collection-post-prefix))
                       (page-sxml (with-layout theme site title
                                               (render-pagination theme
                                                                  site
                                                                  coll-sxml
                                                                  (match prev-page
                                                                    (#f #f)
                                                                    ((file-name _) file-name))
                                                                  (match next-page
                                                                    (#f #f)
                                                                    ((file-name _) file-name))))))
                  (serialized-artifact file-name page-sxml sxml->html)))))
           (if posts-per-page
               (let loop ((pages (paginate base-name filtered-posts))
                          (prev-page #f))
                 (match pages
                   (()
                    '())
                   ((last-page)
                    (list (make-collection-page last-page prev-page #f)))
                   ((and (page . rest) (_ next-page . _))
                    (cons (make-collection-page page prev-page next-page)
                          (loop rest page)))))
               (list
                (serialized-artifact (string-append base-name ".html")
                                     (with-layout theme site title
                                                  (render-collection theme site title
                                                                     filtered-posts
                                                                     collection-post-prefix))
                                     sxml->html)))))))

    ;; Produce a new collections lists, but with the filters applied
    ;; to the actual posts.
    (define collections*
      (map (match-lambda
             ((title file-name filter)
              (list title file-name (filter posts))))
           collections))

    ;; Collect the subset of posts that belong to this blog.  Those
    ;; are the only posts that will have dedicated pages rendered.
    (define posts*
      (delete-duplicates
       (append-map (match-lambda ((_ _ posts) posts)) collections*)))

    (append (map post->page posts*)
            (append-map collection->page collections*))))
