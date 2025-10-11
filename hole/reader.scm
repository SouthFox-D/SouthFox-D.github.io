(define-module (hole reader)
  #:use-module (commonmark)
  #:use-module (commonmark inlines)
  #:use-module (hole org-inlines)
  #:use-module (haunt reader)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (sxml match)
  #:use-module (sxml transform)
  #:use-module (hole sxml)
  #:use-module (hole blocks)
  #:use-module (hole org-blocks)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:use-module (syntax-highlight lisp)
  #:use-module (hole syntax-highlight-python)
  #:export (fox-commonmark-reader
            fox-org-reader
            post-process-commonmark))

(define (maybe-highlight-code lang source)
  (let ((lexer (match lang
                 ('scheme lex-scheme)
                 ('lisp lex-lisp)
                 ('elisp lex-lisp)
                 ('emacs-lisp lex-lisp)
                 ('python lex-python)
                 (_ #f))))
    (if lexer
        (highlights->sxml (highlight lexer source))
        source)))

(define (highlight-code . tree)
  (sxml-match tree
    ((code (@ (class ,class) . ,attrs) ,source)
     (let ((lang (string->symbol
                  (string-drop class (string-length "language-")))))
       `(code (@ ,@attrs)
             ,(maybe-highlight-code lang source))))
    (,other other)))

(define image-suffixes
  '("png" "jpeg" "jpg" "gif" "svg" "webp"))

(define (link-hackery . tree)
  (sxml-match tree
              ((a (@ (href ,src) . ,attrs) . ,body)
               (if (string-prefix? "http" src)
                   (if (any (lambda (suffix) (string-suffix? suffix src))
                            image-suffixes)
                       `(img (@ (src ,src) (alt ,@body) ,@attrs))
                       `(a (@ (href ,src) (class "external_link") ,@attrs) ,@body))
                   tree))
              (,other other)))

(define %commonmark-rules
  `((code . ,highlight-code)
    (a . ,link-hackery)
    (*text* . ,(lambda (tag str) str))
    (*default* . ,(lambda (. arg) arg))))

(define (post-process-commonmark sxml)
  (pre-post-order sxml %commonmark-rules))

(define* (hole/commonmark->sxml #:optional (string-or-port (current-input-port)))
  (let ((port (if (string? string-or-port)
                  (open-input-string string-or-port)
                  string-or-port)))
    (hole/document->sxml (parse-inlines (hole-parse-blocks port)))))

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
    (hole/document->sxml (org/parse-inlines (org/hole-parse-blocks port)))))

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

(define fox-org-reader
  (make-reader (make-file-extension-matcher "org")
               (lambda (file)
                 (call-with-input-file file
                   (lambda (port)
                     (values (org/read-metadata-headers port)
                             (hole/org->sxml port)))))))
