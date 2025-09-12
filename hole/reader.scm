(define-module (hole reader)
  #:use-module (commonmark)
  #:use-module (commonmark inlines)
  #:use-module (haunt reader)
  #:use-module (haunt post)
  #:use-module (ice-9 match)
  #:use-module (sxml match)
  #:use-module (sxml transform)
  #:use-module (hole sxml)
  #:use-module (hole blocks)
  #:export (fox-commonmark-reader))

(define (text-hackery . tree)
  (sxml-match tree
              ((p ",(read more)")
               tree)
              (,other (begin other))))

(define %commonmark-rules
  `((p . ,text-hackery)
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
