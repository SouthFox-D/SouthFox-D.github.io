;; Copyright (C) 2015, 2016  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (hole org-mode inlines)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (commonmark common)
  #:use-module (hole org-mode node)
  #:export (org-mode/parse-inlines))

(define re-start-ticks (make-regexp "^=+"))
(define re-ticks (make-regexp "=+"))
(define re-start-org-link (make-regexp "^\\[\\[+"))
(define re-org-link (make-regexp "\\]\\]+"))
(define re-org-link-split (make-regexp "(.*)\\]\\[(.*)"))

(define re-start-org-footnotes-link (make-regexp "^\\[fn\\:+"))
(define re-end-org-footnotes-link (make-regexp "\\]+"))

(define re-main (make-regexp "^[^=*_\\\n[&/+]+"))

(define (start-ticks? text)
  (regexp-exec re-start-ticks (text-value text) (text-position text)))

(define (end-ticks? text)
  (regexp-exec re-ticks (text-value text) (text-position text)))

(define (start-org-link? text)
  (regexp-exec re-start-org-link (text-value text) (text-position text)))

(define (end-org-link? text)
  (regexp-exec re-org-link (text-value text) (text-position text)))

(define (start-org-footnodes-link? text)
  (regexp-exec re-start-org-footnotes-link (text-value text) (text-position text)))

(define (end-org-footnodes-link? text)
  (regexp-exec re-end-org-footnotes-link (text-value text) (text-position text)))

(define (normal-text? text)
  (regexp-exec re-main (text-value text) (text-position text)))

(define (entity-or-numeric? text)
  (regexp-exec re-entity-or-numeric (text-value text) (text-position text)))

(define (match-length match)
  (string-length (match:substring match 0)))

(define (make-text text position)
  (cons text position))

(define (text-value text)
  (car text))

(define (text-position text)
  (cdr text))

(define (text-move text position)
  (make-text (text-value text) position))

(define (text-advance text increment)
  (make-text (text-value text) (+ (text-position text) increment)))

(define (text-advance-skip text char-pred)
  (make-text (text-value text) (or (string-skip (text-value text) char-pred (text-position text))
                                   (text-position text))))

(define (text-substring text start end)
  (substring (text-value text) start end))

(define (text-char text)
  (string-ref (text-value text) (text-position text)))

(define (text-length text)
  (string-length (text-value text)))

(define (text-end? text)
  (>= (text-position text) (string-length (text-value text))))

(define-record-type <delim-stack>
  (make-delim-stack delims nodes)
  delim-stack?
  (delims delim-stack-delims)
  (nodes delim-stack-nodes))

(define (make-empty-delim-stack)
  (make-delim-stack '() '()))

(define (delim-stack-empty? delim-stack)
  (null? (delim-stack-delims delim-stack)))

(define (delim-stack-push delim-stack delim nodes)
  (match delim-stack
    (($ <delim-stack> delims ns)
     (make-delim-stack (cons delim delims) (cons nodes ns)))))

(define (delim-stack-pop delim-stack)
  (match delim-stack
    (($ <delim-stack> delims nodes)
     (make-delim-stack (cdr delims) (cdr nodes)))))

(define (delim-stack-peek delim-stack)
  (match delim-stack
    (($ <delim-stack> delims nodes)
     (values (car delims) (car nodes)))))

(define (delim-stack-replace-delim delim-stack delim)
  (match delim-stack
    (($ <delim-stack> delims nodes)
     (make-delim-stack (cons delim (cdr delims)) nodes))))

(define-record-type <delimiter>
  (make-delimiter ch count open close)
  delimiter?
  (ch delimiter-ch)
  (count delimiter-count)
  (open delimiter-open?)
  (close delimiter-close?))

(define (whitespace? text position)
  (or (not position) (char-whitespace? (string-ref text position))))

(define (char-punctuation? ch)
  (char-set-contains? char-set:punctuation ch))

(define (punctuation? text position)
  (and position (char-punctuation? (string-ref text position))))

(define (left-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)
  (and (not whitespace-after)
       (or (not punctuation-after) whitespace-before punctuation-before)))

(define (right-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)
  (and (not whitespace-before)
       (or (not punctuation-before) whitespace-after punctuation-after)))

(define (match? open-delim close-delim)
  (eq? (delimiter-ch open-delim) (delimiter-ch close-delim)))

(define (matching-opening? delim-stack delim)
  (find (cut match? <> delim) (delim-stack-delims delim-stack)))

(define (remake-delimiter count delim)
  (make-delimiter (delimiter-ch delim) count (delimiter-open? delim) (delimiter-close? delim)))

(define (make-reference-lookup document)
  (let ((references (node-get-data document 'link-references)))
    (if references
        (lambda (link-label) (assoc-ref references (string-map char-downcase link-label)))
        (const #f))))

;; Node -> Node
;; parses the inline text of paragraphs and heading nodes
(define (org-mode/parse-inlines node)
  (let ((ref-proc (make-reference-lookup node)))
    (define (parse-inner node)
      (cond ((not (node? node)) node)
            ((or (paragraph-node? node) (heading-node? node)) (parse-inline node ref-proc))
            ((attr-node? node)
             (let ((attr-node (parse-inline node (make-reference-lookup node))))
               (make-node (node-type (last-child attr-node))
                          (append (node-data (last-child attr-node)) (node-data attr-node))
                          (list (last-child (last-child attr-node))))))
            (else (make-node (node-type node) (node-data node) (map parse-inner (node-children node))))))
    (parse-inner node)))

(define (delim->text delim)
  (make-text-node (make-string (delimiter-count delim) (delimiter-ch delim))))

(define (ascii-punctuation-characters? ch)
  (define ascii-punc-set (string->char-set ascii-punctuation-characters))
  (char-set-contains? ascii-punc-set ch))

(define* (blank-trailing-space? node #:optional (offset 1))
  (let ((str (last-child node)))
    (and (<= offset (string-length str))
         (case (string-ref str (- (string-length str) offset))
           ((#\space) #t)
           (else #f)))))

(define (remove-trailing-space nodes)
  (let* ((str (last-child (car nodes)))
         (new-str (string-trim-right str #\space)))
    (if (> (string-length new-str) 0)
        (cons (make-text-node new-str) (cdr nodes))
        (cdr nodes))))

(define (parse-newline text nodes delim-stack ref-proc)
  (let ((new-text (text-advance-skip (text-advance text 1) #\space)))
    (if (and (not (null? nodes)) (text-node? (car nodes)) (blank-trailing-space? (car nodes)))
        (parse-char new-text
                    (cons (if (blank-trailing-space? (car nodes) 2)
                              (make-hardbreak-node)
                              (make-softbreak-node))
                          (remove-trailing-space nodes))
                    delim-stack ref-proc)
        (parse-char new-text
                    (cons (make-softbreak-node) nodes)
                    delim-stack ref-proc))))

(define (parse-backslash text nodes delim-stack ref-proc)
  (let* ((next-ch-text (text-advance text 1))
         (next-ch (and (not (text-end? next-ch-text)) (text-char next-ch-text))))
    (cond ((eq? next-ch #\newline)
           (parse-char (text-advance-skip (text-advance next-ch-text 1) #\space)
                       (cons (make-hardbreak-node) nodes)
                       delim-stack ref-proc))
          ((and next-ch (ascii-punctuation-characters? next-ch))
           (parse-char (text-advance next-ch-text 1)
                       (cons (make-text-node (string next-ch)) nodes)
                       delim-stack ref-proc))
          (else (parse-char next-ch-text (cons (make-text-node "\\") nodes)
                            delim-stack ref-proc)))))

(define (build-org-link text)
  (define image-suffixes
    '("png" "jpeg" "jpg" "gif" "svg" "webp"))
  (let ((start-ticks (start-org-link? text)))
    (if (not start-ticks)
        (let ((start-footnotes-ticks (start-org-footnodes-link? text)))
          (if (not start-footnotes-ticks)
              (values (+ 1 (text-position text)) (make-text-node "["))
              (begin
                (let loop ((end-footnotes-ticks (end-org-footnodes-link? (text-move text (match:end start-footnotes-ticks 0)))))
                  (cond ((not end-footnotes-ticks)
                         (values (match:end start-footnotes-ticks 0)
                                 (make-text-node (match:substring start-footnotes-ticks 0))))
                        (else
                         (let* ((link-content (text-substring
                                               text
                                               (match:end start-footnotes-ticks 0)
                                               (match:start end-footnotes-ticks 0))))
                           (values (match:end end-footnotes-ticks 0)
                                   (make-link-node
                                    (list (make-text-node link-content))
                                    (if (= 0 (text-position text))
                                        (string-append "#" link-content "r")
                                        (string-append "#" link-content))
                                    #f
                                    #:id (if (= 0 (text-position text))
                                             link-content
                                             (string-append link-content "r"))
                                    #:is-sup? #t)))))))))
        (begin
          (let loop ((end-ticks (end-org-link? (text-move text (match:end start-ticks 0)))))
            (cond ((not end-ticks)
                   (values (match:end start-ticks 0)
                           (make-text-node (match:substring start-ticks 0))))
                  ((= (match-length start-ticks) (match-length end-ticks))
                   (let* ((link-content (text-substring text (match:end start-ticks 0)
                                                        (match:start end-ticks 0)))
                          (link-match (regexp-exec re-org-link-split link-content 0)))
                     (if link-match
                         (values (match:end end-ticks 0)
                                 (make-link-node
                                  (list (make-text-node (match:substring link-match 2)))
                                  (match:substring link-match 1) #t
                                  #:is-image? (any (lambda (suffix)
                                                     (string-suffix? suffix (match:substring link-match 1)))
                                                      image-suffixes)))
                         (values (match:end end-ticks 0)
                                 (make-link-node
                                  (list (make-text-node link-content))
                                  link-content #f)))))
                  (else (loop (end-ticks? (text-move text (match:end end-ticks 0)))))))))))

(define (parse-org-link text nodes delim-stack ref-proc)
  (let-values (((pos node) (build-org-link text)))
    (parse-char (text-move text pos) (cons node nodes)
                delim-stack ref-proc)))

(define (parse-entity-numeric text nodes delim-stack ref-proc)
  (let ((entity-match (entity-or-numeric? text)))
    (if entity-match
        (let ((str (entity->string (match:substring entity-match 1))))
          (if str
              (parse-char (text-move text (match:end entity-match 0))
                          (cons (make-text-node str) nodes)
                          delim-stack ref-proc)
              (parse-char (text-advance text 1) (cons (make-text-node "&") nodes)
                          delim-stack ref-proc)))
        (parse-char (text-advance text 1) (cons (make-text-node "&") nodes)
                    delim-stack ref-proc))))

(define (parse-normal-text text nodes delim-stack ref-proc)
  (let ((normal-text (normal-text? text)))
    (parse-char (text-move text (match:end normal-text 0))
                (cons (make-text-node (match:substring normal-text 0)) nodes)
                delim-stack ref-proc)))

(define (pop-remaining-delim nodes delim-stack)
  (if (delim-stack-empty? delim-stack)
      (if (and (not (null? nodes)) (text-node? (car nodes))) (remove-trailing-space nodes) nodes)
      (let-values (((d n) (delim-stack-peek delim-stack)))
        (pop-remaining-delim (append nodes (cons (delim->text d) n))
                             (delim-stack-pop delim-stack)))))

(define (org-emphasis-pre-char? ch)
  (or (char-whitespace? ch)
      (string-index "-( {'\"" ch)))

(define (org-emphasis-post-char? ch)
  (or (char-whitespace? ch)
      (string-index "-),:;'\"? " ch)))

(define (at-line-start? text)
  (= (text-position text) 0))

(define (at-line-end? text pos)
  (>= pos (string-length (text-value text))))

(define (marker->node-type ch)
  (case ch
    ((#\*) 'strong)
    ((#\/) 'em)
    ((#\=) 'code)
    ((#\~) 'code)
    ((#\_) 'underline)
    ((#\+) 'delete)
    (else 'text)))

(define (parse-symmetric-span text)
  (let* ((val (text-value text))
         (pos (text-position text))
         (ch (text-char text))
         (pre-pos (- pos 1)))
    (if (and (not (at-line-start? text))
             (not (org-emphasis-pre-char? (string-ref val pre-pos))))
        (values #f #f)
        (let* ((span-end (string-skip val ch pos))
               (count (- (or span-end (string-length val)) pos))
               (content-start (or span-end (string-length val))))
          (if (and (< content-start (string-length val))
                   (char-whitespace? (string-ref val content-start)))
              (values #f #f)
              (let loop ((search-pos content-start))
                (let ((end-match (string-index val ch search-pos)))
                  (cond
                   ((not end-match) (values #f #f))
                   (else
                    (let* ((this-end (string-skip val ch end-match))
                           (this-count (- (or this-end (string-length val)) end-match))
                           (post-char-pos (or this-end (string-length val))))
                      (if (= count this-count)
                          (let ((char-before-end (string-ref val (- end-match 1))))
                            (if (and (not (char-whitespace? char-before-end))
                                     (or (at-line-end? text post-char-pos)
                                         (org-emphasis-post-char? (string-ref val post-char-pos))))
                                (values post-char-pos
                                        (make-emphasis-node
                                         (list (make-text-node (substring val content-start end-match)))
                                         (marker->node-type ch) ))
                                (loop post-char-pos)))
                          (loop (or this-end (string-length val))))))))))))))

(define (parse-char text nodes delim-stack ref-proc)
  (if (text-end? text)
      (pop-remaining-delim nodes delim-stack)
      (let ((current-ch (text-char text)))
        (case current-ch
          ((#\newline) (parse-newline text nodes delim-stack ref-proc))
          ((#\\) (parse-backslash text nodes delim-stack ref-proc))
          ((#\= #\* #\~ #\_ #\+ #\/)
           (let-values (((pos node) (parse-symmetric-span text)))
             (if pos
                 (parse-char (text-move text pos) (cons node nodes)
                             delim-stack ref-proc)
                 (parse-char (text-advance text 1) (cons (make-text-node (string current-ch)) nodes)
                             delim-stack ref-proc))))
          ((#\[) (parse-org-link text nodes delim-stack ref-proc))
          ((#\&) (parse-entity-numeric text nodes delim-stack ref-proc))
          (else (parse-normal-text text nodes delim-stack ref-proc))))))

(define (parse-inline node ref-proc)
  (let ((text (last-child (last-child node))))
    (make-node (node-type node) (node-data node) (parse-char (make-text text 0) '()
                                                             (make-empty-delim-stack)
                                                             ref-proc))))
