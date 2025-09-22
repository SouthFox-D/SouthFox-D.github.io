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

(define-module (hole org-inlines)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (commonmark node)
  #:use-module (commonmark common)
  #:export (org/parse-inlines))

(define re-start-ticks (make-regexp "^=+"))
(define re-ticks (make-regexp "=+"))
(define re-start-org-link (make-regexp "^\\[\\[+"))
(define re-org-link (make-regexp "\\]\\]+"))
(define re-org-link-split (make-regexp "(.*)\\]\\[(.*)"))

(define re-main (make-regexp "^[^=*_\\\n[!<&]+"))
(define re-autolink (make-regexp (string-append "^<([a-zA-Z][a-zA-Z0-9+.-]{1,31}:[^ \t\n<>"
                                                control-characters "]*)>")))
(define re-email-autolink (make-regexp
                           (string-append "^<([a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@"
                                          "[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?"
                                          "(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>")))


(define (start-ticks? text)
  (regexp-exec re-start-ticks (text-value text) (text-position text)))

(define (end-ticks? text)
  (regexp-exec re-ticks (text-value text) (text-position text)))

(define (start-org-link? text)
  (regexp-exec re-start-org-link (text-value text) (text-position text)))

(define (end-org-link? text)
  (regexp-exec re-org-link (text-value text) (text-position text)))

(define (normal-text? text)
  (regexp-exec re-main (text-value text) (text-position text)))

(define (autolink? text)
  (regexp-exec re-autolink (text-value text) (text-position text)))

(define (email-autolink? text)
  (regexp-exec re-email-autolink (text-value text) (text-position text)))

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

(define (scan-delim text)
  (define (count-delim delim-end position)
    (- (or delim-end (text-length text)) position))
  (let* ((ch (text-char text))
         (position (text-position text))
         (text (text-value text))
         (delim-end (string-skip text ch position))
         (delim-start (string-skip-right text ch 0 position))
         (whitespace-before (whitespace? text delim-start))
         (whitespace-after (whitespace? text delim-end))
         (punctuation-before (punctuation? text delim-start))
         (punctuation-after (punctuation? text delim-end))
         (left (left-flanking? whitespace-after punctuation-after whitespace-before punctuation-before))
         (right (right-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)))
    (case ch
      ((#\*) (make-delimiter ch (count-delim delim-end position) left right))
      ((#\_) (make-delimiter ch (count-delim delim-end position)
                             (and left (or (not right) punctuation-before))
                             (and right (or (not left) punctuation-after)))))))

(define (match? open-delim close-delim)
  (eq? (delimiter-ch open-delim) (delimiter-ch close-delim)))

(define (matching-opening? delim-stack delim)
  (find (cut match? <> delim) (delim-stack-delims delim-stack)))

(define (remake-delimiter count delim)
  (make-delimiter (delimiter-ch delim) count (delimiter-open? delim) (delimiter-close? delim)))

(define (match-delim opening-delim closing-delim)
  (let ((open-count (delimiter-count opening-delim))
        (close-count (delimiter-count closing-delim)))
    (cond ((or (= open-count close-count 1) (= open-count close-count 2))
           (list #f #f))
          ((>= open-count 2 close-count)
           (list (remake-delimiter (- open-count close-count) opening-delim) #f))
          ((<= open-count 2 close-count)
           (list #f (remake-delimiter (- close-count open-count) closing-delim)))
          ((odd? close-count)
           (list (remake-delimiter (- open-count 1) opening-delim)
                 (remake-delimiter (- close-count 1) closing-delim)))
          (else (list (remake-delimiter (- open-count 2) opening-delim)
                      (remake-delimiter (- close-count 2) closing-delim))))))

(define (make-reference-lookup document)
  (let ((references (node-get-data document 'link-references)))
    (if references
        (lambda (link-label) (assoc-ref references (string-map char-downcase link-label)))
        (const #f))))

;; Node -> Node
;; parses the inline text of paragraphs and heading nodes
(define (org/parse-inlines node)
  (let ((ref-proc (make-reference-lookup node)))
    (define (parse-inner node)
      (cond ((not (node? node)) node)
            ((or (paragraph-node? node) (heading-node? node)) (parse-inline node ref-proc))
            (else (make-node (node-type node) (node-data node) (map parse-inner (node-children node))))))
    (parse-inner node)))

(define (emphasis-type delim)
  (case (delimiter-count delim)
    ((1) 'em)
    (else 'strong)))

(define (delim->text delim)
  (make-text-node (make-string (delimiter-count delim) (delimiter-ch delim))))

(define (parse-emphasis text nodes delim-stack ref-proc)
  (define (parse-matching-delim delim matching-delim)
    (let loop ((ds delim-stack)
               (ns nodes))
      (let-values (((d n) (delim-stack-peek ds)))
        (if (eq? d matching-delim)
            (match (match-delim matching-delim delim)
              ((#f #f)
               (parse-char (text-advance text (delimiter-count delim))
                           (cons (make-emphasis-node ns (emphasis-type delim)) n)
                           (delim-stack-pop ds) ref-proc))
              ((od #f)
               (parse-char (text-advance text (delimiter-count delim))
                           (list (make-emphasis-node ns (emphasis-type delim)))
                           (delim-stack-replace-delim ds od) ref-proc))
              ((#f cd)
               (parse-char (text-advance text (delimiter-count matching-delim))
                           (cons (make-emphasis-node ns (emphasis-type matching-delim)) n)
                           (delim-stack-pop ds) ref-proc))
              ((od cd)
               (let ((difference (- (delimiter-count delim) (delimiter-count cd))))
                 (parse-char (text-advance text difference)
                             (list (make-emphasis-node ns (if (= 1 difference) 'em 'strong)))
                             (delim-stack-replace-delim ds od) ref-proc))))
            (loop (delim-stack-pop ds) (append ns (cons (delim->text d) n)))))))
  (let ((delim (scan-delim text)))
    (cond ((and (delimiter-close? delim) (delimiter-open? delim))
           (let ((matching-delim (matching-opening? delim-stack delim)))
             (if matching-delim
                 (parse-matching-delim delim matching-delim)
                 (parse-char (text-advance text (delimiter-count delim))
                             '()
                             (delim-stack-push delim-stack delim nodes) ref-proc))))
          ((delimiter-close? delim)
           (let ((matching-delim (matching-opening? delim-stack delim)))
             (if matching-delim
                 (parse-matching-delim delim matching-delim)
                 (parse-char (text-advance text (delimiter-count delim))
                             (cons (delim->text delim) nodes)
                             delim-stack ref-proc))))
          ((delimiter-open? delim)
           (parse-char (text-advance text (delimiter-count delim))
                       '()
                       (delim-stack-push delim-stack delim nodes) ref-proc))
          (else (parse-char (text-advance text (delimiter-count delim))
                            (cons (delim->text delim) nodes)
                            delim-stack ref-proc)))))

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

(define (parse-ticks text)
  (let ((start-ticks (start-ticks? text)))
    (let loop ((end-ticks (end-ticks? (text-move text (match:end start-ticks 0)))))
      (cond ((not end-ticks)
             (values (match:end start-ticks 0)
                     (make-text-node (match:substring start-ticks 0))))
            ((= (match-length start-ticks) (match-length end-ticks))
             (values (match:end end-ticks 0)
                     (make-code-span-node (text-substring text (match:end start-ticks 0)
                                                          (match:start end-ticks 0)))))
            (else (loop (end-ticks? (text-move text (match:end end-ticks 0)))))))))

(define (parse-code-span text nodes delim-stack ref-proc)
  (let-values (((pos node) (parse-ticks text)))
    (parse-char (text-move text pos) (cons node nodes)
                delim-stack ref-proc)))

(define (build-org-link text)
  (let ((start-ticks (start-org-link? text)))
    (if (not start-ticks)
        (values (+ 1 (text-position text)) (make-text-node "["))
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
                                  (match:substring link-match 1) #f))
                         (values (match:end end-ticks 0)
                                 (make-link-node
                                  (list (make-text-node link-content))
                                  link-content #f)))))
                  (else (loop (end-ticks? (text-move text (match:end end-ticks 0)))))))))))

(define (parse-org-link text nodes delim-stack ref-proc)
  (let-values (((pos node) (build-org-link text)))
    (parse-char (text-move text pos) (cons node nodes)
                delim-stack ref-proc)))

(define (parse-autolink text)
  (let ((autolink-match (autolink? text)))
    (if autolink-match
        (values (make-link-node (list (make-text-node (match:substring autolink-match 1)))
                                (match:substring autolink-match 1) #f)
                (text-move text (match:end autolink-match 0)))
        (let ((email-match (email-autolink? text)))
          (if email-match
              (values (make-link-node (list (make-text-node (match:substring email-match 1)))
                                      (string-append "mailto:" (match:substring email-match 1)) #f)
                      (text-move text (match:end email-match 0)))
              (values #f text))))))

(define (parse-autolink-or-html text nodes delim-stack ref-proc)
  (let-values (((autolink text) (parse-autolink text)))
    (if autolink
        (parse-char text (cons autolink nodes) delim-stack ref-proc)
        (parse-char (text-advance text 1) (cons (make-text-node "<") nodes) delim-stack ref-proc))))


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

(define (parse-char text nodes delim-stack ref-proc)
  (if (text-end? text)
      (pop-remaining-delim nodes delim-stack)
      (case (text-char text)
        ((#\newline) (parse-newline text nodes delim-stack ref-proc))
        ((#\\) (parse-backslash text nodes delim-stack ref-proc))
        ((#\=) (parse-code-span text nodes delim-stack ref-proc))
        ((#\* #\_) (parse-emphasis text nodes delim-stack ref-proc))
        ((#\[) (parse-org-link text nodes delim-stack ref-proc))
        ((#\&) (parse-entity-numeric text nodes delim-stack ref-proc))
        ((#\<) (parse-autolink-or-html text nodes delim-stack ref-proc))
        (else (parse-normal-text text nodes delim-stack ref-proc)))))

(define (parse-inline node ref-proc)
  (let ((text (last-child (last-child node))))
    (make-node (node-type node) (node-data node) (parse-char (make-text text 0) '()
                                                             (make-empty-delim-stack)
                                                             ref-proc))))
