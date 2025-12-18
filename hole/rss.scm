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
;; Atom feed builder.
;;
;;; Code:

(define-module (hole rss)
  #:use-module (hole site)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (haunt artifact)
  #:use-module (haunt site)
  #:use-module (haunt post)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (haunt html)
  #:use-module (haunt serve mime-types)
  #:use-module (web uri)
  #:use-module (hole theme)
  #:export (hole/rss-feed))

(define-record-type <enclosure>
  (make-enclosure title url extra)
  enclosure?
  (title enclosure-title)
  (url enclosure-url)
  (extra enclosure-extra))

(define (enclosure-mime-type enclosure)
  (mime-type (enclosure-url enclosure)))

(define char-set:enclosure-key
  (char-set-union char-set:letter+digit
                  (char-set-delete char-set:punctuation #\: #\")
                  (char-set-delete char-set:symbol #\=)))

(define (parse-enclosure s)
  (call-with-input-string s
    (lambda (port)
      (define (assert-char char)
        (let ((c (read-char port)))
          (unless (eqv? c char)
            (error "enclosure: parse: expected" char "got" c))))
      (define (whitespace? char)
        (char-set-contains? char-set:whitespace char))
      (define (consume-whitespace)
        (match (peek-char port)
          ((? eof-object?) *unspecified*)
          ((? whitespace?)
           (read-char port)
           (consume-whitespace))
          (_ *unspecified*)))
      (define (read-escape-character)
        (match (read-char port)
          (#\" #\")
          (#\\ #\\)
          (char (error "enclosure: parse: invalid escape character:" char))))
      (define (read-unquoted-string)
        (list->string
         (let loop ()
           (let ((c (peek-char port)))
             (cond
              ((eof-object? c)
               '())
              ((char-set-contains? char-set:enclosure-key c)
               (read-char port)
               (cons c (loop)))
              (else
               '()))))))
      (define (read-string)
        (if (eqv? (peek-char port) #\")
            (begin
              (assert-char #\")
              (list->string
               (let loop ()
                 (match (read-char port)
                   ((? eof-object?)
                    (error "enclosure: parse: EOF while reading string"))
                   (#\" '())
                   (#\\ (cons (read-escape-character) (loop)))
                   (char (cons char (loop)))))))
            (read-unquoted-string)))
      (define (read-key)
        (string->symbol (read-unquoted-string)))
      (let loop ((attrs '()))
        (consume-whitespace)
        (if (eof-object? (peek-char port))
            (make-enclosure (assq-ref attrs 'title)
                            (assq-ref attrs 'url)
                            (let loop ((attrs attrs))
                              (match attrs
                                (() '())
                                ((((or 'title 'url) . _) . rest)
                                 (loop rest))
                                ((attr . rest)
                                 (cons attr (loop rest))))))
            (let ((key (read-key)))
              (assert-char #\:)
              (loop (cons (cons key (read-string)) attrs))))))))

(register-metadata-parser! 'enclosure parse-enclosure)

(define (sxml->xml* sxml port)
  "Write SXML to PORT, preceded by an <?xml> tag."
  (display "<?xml version=\"1.0\" encoding=\"utf-8\"?>" port)
  (sxml->xml sxml port))

(define (date->string* date)
  "Convert date to RFC-822 formatted string."
  (date->string date "~a, ~d ~b ~Y ~H:~M:~S ~z"))

(define* (post->item-entry site post #:key (blog-prefix ""))
  "Convert POST into an Atom <entry> XML node."
  (let ((uri (uri->string
              (build-uri (site-scheme site)
                         #:host (site-domain site)
                         #:path (hole/uri-encode
                                 (string-append blog-prefix "/"
                                                (site-post-slug site post)
                                                "index.html"))))))
    `(item
      (title ,(post-ref post 'title))
      (guid ,uri)
      (pubDate ,(date->string* (post-date post)))
      (link ,uri)
      (description ,(sxml->html-string (parse-read-more post)))
      (content:encoded (@ (type "html"))
                       ,(sxml->html-string (post-sxml post)))
      ,@(map (lambda (enclosure)
               `(link (@ (rel "enclosure")
                         (title ,(enclosure-title enclosure))
                         (href ,(enclosure-url enclosure))
                         (type ,(enclosure-mime-type enclosure))
                         ,@(map (match-lambda
                                  ((key . value)
                                   (list key value)))
                                (enclosure-extra enclosure)))))
             (post-ref-all post 'enclosure)))))

(define* (hole/rss-feed #:key
                        (file-name "rss2.xml")
                        (filter posts/reverse-chronological)
                        (last-updated (current-date))
                        (max-entries 20)
                        (blog-prefix ""))
  "Return a builder procedure that renders a list of posts as an RSS
feed.  All arguments are optional:

FILE-NAME: The page file name.

SUBTITLE: The feed subtitle.

FILTER: The procedure called to manipulate the posts list before rendering.

LAST-UPDATED: The feed last updated date.

MAX-ENTRIES: The maximum number of posts to render in the feed.

BLOG-PREFIX: The prefix for all post URLs, which is the combination of
the blog's prefix and post prefix."
  (lambda (site posts)
    (let ((uri (uri->string
                (build-uri (site-scheme site)
                           #:host (site-domain site)
                           #:path (string-append "/" file-name)))))
      (serialized-artifact file-name
                           `(rss (@ (version "2.0")
                                    (xmlns:atom "http://www.w3.org/2005/Atom")
                                    (xmlns:content "http://purl.org/rss/1.0/modules/content/"))
                             (channel
                              (title ,(site-title site))
                              (pubDate ,(date->string* last-updated))
                              (atom:link (@ (href ,uri) (rel "self") (type "application/rss+xml")))
                              (link ,(uri->string
                                          (build-uri
                                           (site-scheme site)
                                           #:host (site-domain site))))
                              (generator "https://dthompson.us/projects/haunt.html")
                              ,@(map (cut post->item-entry site <>
                                          #:blog-prefix blog-prefix)
                                     (take-up-to max-entries (filter posts)))))
                           sxml->xml*))))
