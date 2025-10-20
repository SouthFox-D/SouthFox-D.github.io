;; Copyright (C) 2015  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (hole sxml)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (commonmark node)
  #:use-module (ice-9 string-fun)
  #:export (hole/document->sxml
            shortcode-node?))

;; Document -> xml
;; converts the document into HTML
;; !!!

(define (node-type? n t)
  (and (node? n) (eq? (node-type n) t)))

(define (shortcode-node? n)
  (node-type? n 'shortcode))

(define (hole/document->sxml d)
  (if (document-node? d)
      (fold (lambda (elem prev) (cons (node->sxml elem) prev)) '() (node-children d))
      (error "not a document node")))

(define (node->sxml n)
  (cond ((thematic-break-node? n) (thematic-break-node->sxml n))
        ((paragraph-node? n) (paragraph-node->sxml n))
        ((block-quote-node? n) (block-quote-node->sxml n))
        ((code-block-node? n) (code-block-node->sxml n))
        ((fenced-code-node? n) (fenced-code-node->sxml n))
        ((shortcode-node? n) (shortcode-node->sxml n))
        ((heading-node? n) (heading-node->sxml n))
        ((list-node? n) (list-node->sxml n))
        ((text-node? n) (text-node->sxml n))
        ((code-span-node? n) (code-span-node->sxml n))
        ((softbreak-node? n) (softbreak-node->sxml n))
        ((hardbreak-node? n) (hardbreak-node->sxml n))
        ((emphasis-node? n) (emphasis-node->sxml n))
        ((link-node? n) (link-node->sxml n))
        ((image-node? n) (image-node->sxml n))
        (else (error "unknown node"))))

(define (thematic-break-node->sxml n)
  '(hr))

(define (paragraph-node->sxml n)
  `(p ,@(fold-nodes node->sxml (node-children n))))

(define (text-node->sxml n)
  (last-child n))

(define (code-span-node->sxml n)
  `(code ,@(node-children n)))

(define (block-quote-node->sxml n)
  `(blockquote ,@(fold-nodes node->sxml (node-children n))))

(define (code-block-node->sxml n)
  `(pre (code ,@(node-children n))))

(define (fenced-code-node->sxml n)
  `(pre (code ,(infostring (assq-ref (node-data n) 'info-string)),@(node-children n))))

(define (mastodon-embed url)
  `(div
    (blockquote
     (@ (style "background: #FCF8FF; border-radius: 8px; border: 1px solid #C9C4DA; margin: 0; max-width: 540px; min-width: 270px; overflow: hidden; padding: 0;")
        (data-embed-url ,(string-append url "/embed"))
        (class "mastodon-embed")) " "
     (a (@ (target "_blank")
           (style "align-items: center; color: #1C1A25; display: flex; flex-direction: column; font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Oxygen, Ubuntu, Cantarell, 'Fira Sans', 'Droid Sans', 'Helvetica Neue', Roboto, sans-serif; font-size: 14px; justify-content: center; letter-spacing: 0.25px; line-height: 20px; padding: 24px; text-decoration: none;")
           (href ,url)) " "
           (svg
            (@ (xmlns "http://www.w3.org/2000/svg:svg" )
               (xmlns:xlink "http://www.w3.org/1999/xlink")
               (width "32")
               (viewBox "0 0 79 75")
               (height "32"))
            (path
             (@ (fill "currentColor")
                (d "M63 45.3v-20c0-4.1-1-7.3-3.2-9.7-2.1-2.4-5-3.7-8.5-3.7-4.1 0-7.2 1.6-9.3 4.7l-2 3.3-2-3.3c-2-3.1-5.1-4.7-9.2-4.7-3.5 0-6.4 1.3-8.6 3.7-2.1 2.4-3.1 5.6-3.1 9.7v20h8V25.9c0-4.1 1.7-6.2 5.2-6.2 3.8 0 5.8 2.5 5.8 7.4V37.7H44V27.1c0-4.9 1.9-7.4 5.8-7.4 3.5 0 5.2 2.1 5.2 6.2V45.3h8ZM74.7 16.6c.6 6 .1 15.7.1 17.3 0 .5-.1 4.8-.1 5.3-.7 11.5-8 16-15.6 17.5-.1 0-.2 0-.3 0-4.9 1-10 1.2-14.9 1.4-1.2 0-2.4 0-3.6 0-4.8 0-9.7-.6-14.4-1.7-.1 0-.1 0-.1 0s-.1 0-.1 0 0 .1 0 .1 0 0 0 0c.1 1.6.4 3.1 1 4.5.6 1.7 2.9 5.7 11.4 5.7 5 0 9.9-.6 14.8-1.7 0 0 0 0 0 0 .1 0 .1 0 .1 0 0 .1 0 .1 0 .1.1 0 .1 0 .1.1v5.6s0 .1-.1.1c0 0 0 0 0 .1-1.6 1.1-3.7 1.7-5.6 2.3-.8.3-1.6.5-2.4.7-7.5 1.7-15.4 1.3-22.7-1.2-6.8-2.4-13.8-8.2-15.5-15.2-.9-3.8-1.6-7.6-1.9-11.5-.6-5.8-.6-11.7-.8-17.5C3.9 24.5 4 20 4.9 16 6.7 7.9 14.1 2.2 22.3 1c1.4-.2 4.1-1 16.5-1h.1C51.4 0 56.7.8 58.1 1c8.4 1.2 15.5 7.5 16.6 15.6Z"))))
           " "
           (div (@ (style "color: #787588; margin-top: 16px;")) "Post by SouthFox") " "
           (div (@ (style "font-weight: 500;")) "View on Mastodon") " ") " ")
    (script (@ (data-allowed-prefixes "https://foxsay.southfox.me/")
               (async "true")
               (src "https://foxsay.southfox.me/embed.js")))))

(define (shortcode-node->sxml n)
  (let* ((%content (last-child n))
         (content (substring %content 2 (- (string-length %content) 1)))
         (shortcode (car (string-split content #\ )))
         (shortcode-data (cdr (string-split content #\ ))))
    (cond ((string= shortcode "read-more") `(span (@ (id "more"))))
          ((string= shortcode "mastodon-embed") (apply mastodon-embed shortcode-data))
          (else (error "unknown shortcode")))))

(define (heading-node->sxml n)
  `(,(level n)
    (@ (id ,(string-join (map (lambda (text)
                                (string-replace-substring text " " ""))
                              (map car (fold-nodes node-children (node-children n))))
                         "")))
      ,@(fold-nodes node->sxml (node-children n))))

(define (list-type n)
  (case (assq-ref (node-data n) 'type)
    ((bullet) 'ul)
    (else 'ol)))

(define (list-tight? node)
  (assq-ref (node-data node) 'tight))

(define (list-node->sxml n)
  `(,(list-type n) ,@(if (list-tight? n)
                         (fold-nodes tight-item-node->sxml (node-children n))
                         (fold-nodes item-node->sxml (node-children n)))))

(define (item-node->sxml n)
  `(li ,@(fold-nodes node->sxml (node-children n))))

(define (tight-item-node->sxml node)
  `(li ,@(if (paragraph-node? (last-child node))
             (fold-nodes node->sxml (node-children (last-child node)))
             (fold-nodes node->sxml (node-children node)))))

(define (softbreak-node->sxml n)
  "\n")

(define (hardbreak-node->sxml n)
  '(br))

(define (emphasis-type n)
  (case (assq-ref (node-data n) 'type)
    ((em) 'em)
    (else 'strong)))

(define (emphasis-node->sxml n)
  `(,(emphasis-type n) ,@(fold-nodes node->sxml (node-children n))))

(define (destination node)
  (assq-ref (node-data node) 'destination))

(define (title node)
  (assq-ref (node-data node) 'title))

(define (link-node->sxml node)
  (let* ((dest (destination node))
         (title (title node))
         (id (assq-ref (node-data node) 'id))
         (is-sup? (assq-ref (node-data node) 'is-sup?))
         (attrs `((href ,dest)
                  ,@(if title (list (list 'title title)) '())
                  ,@(if id (list (list 'id id)) '())))
         (children (fold-nodes node->sxml (node-children node))))
    (if is-sup?
        `(sup (a (@ ,@attrs) ,@children))
        `(a (@ ,@attrs) ,@children))))

(define (fold-text node)
  (fold (lambda (elem prev)
          (append (node->text elem) prev))
        '()
        (node-children node)))

(define (node->text node)
  (if (text-node? node)
      (node-children node)
      (fold-text node)))

(define (alt-text node)
  (string-concatenate (fold-text node)))

(define (image-node->sxml node)
  (let ((dest (destination node))
        (title (title node)))
    `(img (@ (src ,dest) (alt ,(alt-text node)) ,@(if title (list (list 'title title)) '())))))

(define (infostring s)
  (let ((language (string-trim-both s)))
    (if (string-null? language)
        '(@)
        `(@ (class ,(string-append "language-" language))))))

(define (level n)
  (case (assq-ref (node-data n) 'level)
    ((1) 'h1)
    ((2) 'h2)
    ((3) 'h3)
    ((4) 'h4)
    ((5) 'h5)
    ((6) 'h6)))

(define (fold-nodes f ns)
  (fold (lambda (elem prev) (cons (f elem) prev)) '() ns))
