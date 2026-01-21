(define-module (hole reader)
  #:use-module (commonmark)
  #:use-module (hole commonmark blocks)
  #:use-module (commonmark inlines)
  #:use-module (hole org-mode blocks)
  #:use-module (hole org-mode inlines)
  #:use-module (haunt reader)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (hole sxml)
  #:export (fox-commonmark-reader
            fox-org-mode-reader))

(define* (hole/commonmark->sxml #:optional (string-or-port (current-input-port)))
  (let ((port (if (string? string-or-port)
                  (open-input-string string-or-port)
                  string-or-port)))
    (hole/document->sxml (parse-inlines (commonmark/parse-blocks port)))))

(define fox-commonmark-reader
  (make-reader (make-file-extension-matcher "md")
               (lambda (file)
                 (call-with-input-file file
                   (lambda (port)
                     (values (read-metadata-headers port)
                             (hole/commonmark->sxml port)))))))

(define* (hole/org->sxml #:optional (string-or-port (current-input-port)))
  (let ((port (if (string? string-or-port)
                  (open-input-string string-or-port)
                  string-or-port)))
    (hole/document->sxml (org-mode/parse-inlines (org-mode/parse-blocks port)))))

(define %org-metadata-parsers
  (make-hash-table))

(define (org-metadata-parser key)
  (or (hash-ref %org-metadata-parsers key) identity))

(define (org-register-metadata-parser! name parser)
  (hash-set! %org-metadata-parsers name parser))

(define (parse-metadata key value)
  ((org-metadata-parser key) value))

(org-register-metadata-parser!
 'tags
 (lambda (str)
   (map string-trim-both (string-split str #\:))))

(org-register-metadata-parser! 'date string->date*)

(define (org/read-metadata-headers port)
  (let loop ((metadata '())
             (start-parse #f))
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)
        (error "end of file while reading metadata: " (port-filename port)))
       ((and start-parse
             (not (string-prefix? "#+" line)))
        (unread-char #\newline port)
        (unread-string line port)
        metadata)
       ((and (not start-parse)
             (not (string-prefix? "#+" line)))
        (loop metadata #f))
       (else
        (match (map string-trim-both (string-split-at line #\:))
          ((key value)
           (loop (alist-cons (string->symbol (string-drop key 2))
                             (parse-metadata (string->symbol (string-drop key 2)) value)
                             metadata)
                 #t))
          (_ (error "something wrong"))))))))

(define fox-org-mode-reader
  (make-reader (make-file-extension-matcher "org")
               (lambda (file)
                 (call-with-input-file file
                   (lambda (port)
                     (values (org/read-metadata-headers port)
                             (hole/org->sxml port)))))))
