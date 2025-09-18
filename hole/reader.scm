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
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:use-module (syntax-highlight lisp)
  #:use-module (hole syntax-highlight-python)
  #:export (fox-commonmark-reader
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

(define %commonmark-rules
  `((code . ,highlight-code)
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
