;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.lonestar.org>
;;; Copyright © 2020 Dimakakos Dimakis <bendersteed@teknik.io>
;;; Copyright © 2025 SouthFox <master@southfox.me>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (hole builder tag)
  #:use-module (hole builder blog)
  #:use-module (hole theme)
  #:use-module (hole site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (hole html)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:export (tags->page tag-page))

(define (group-by-tag posts)
  "Given a lisp of haunt posts generate a list grouping tags with the
posts associated with it."
  (let ((table (make-hash-table)))
    (hash-set! table "untagged" '())
    (for-each (lambda (post)
                (let ((tags (post-ref post 'tags)))
                  (if tags
                      (for-each (lambda (tag)
                                  (let ((current (hash-ref table tag)))
                                    (if current
                                        (hash-set! table tag (cons post current))
                                        (hash-set! table tag (list post)))))
                                tags)
                      (hash-set! table "untagged" (cons post (hash-ref table "untagged"))))))
              posts)
    (hash-fold alist-cons '() table)))

(define (count-tags posts)
  "Return a list of tags associated with their count in descending
order."
  (sort (map (lambda (tag)
               (list (car tag) (length (cdr tag))))
             (group-by-tag posts))
        (lambda (a b) (> (cadr a) (cadr b)))))

(define* (tags-template site posts #:key title)
  `((div (@ (class "content"))
     (h1 "#" ,title)
     (ul
      ,(map (lambda (post)
              `(li (a (@ (href ,(hole/uri-encode (string-append "/" (site-post-slug site post)))))
                      ,(post-ref post 'title)
                      " — "
                      ,(date->string (post-date post) "~Y-~m-~d"))))
            (posts/reverse-chronological posts))))))

(define (tag-uri tag)
  "Given a TAG return the page that contains only posts associated
with that TAG."
  (string-append "/tag/" tag "/index.html"))

(define (tags->page)
  (lambda (site posts)
    (flat-map (match-lambda
                ((tag . posts)
                 (make-page (tag-uri tag)
                            (with-layout fox-theme site "Tags" (tags-template site posts #:title tag))
                            sxml->html)))
              (group-by-tag posts))))

(define (tag-page)
  (lambda (site posts)
    (make-page "tags/index.html"
               (with-layout
                fox-theme
                site
                "Tags"
                `(div (@ (class "content"))
                  (h2 "标签")
                  (ul
                   ,(map (match-lambda
                           ((tag count)
                            `(li (a (@ (class "tag")
                                       (href ,(hole/uri-encode (tag-uri tag))))
                                    ,tag ": " ,count))))
                         (count-tags posts)))))
               sxml->html)))
