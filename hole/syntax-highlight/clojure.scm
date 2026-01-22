(define-module (hole syntax-highlight clojure)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (syntax-highlight lexers)
  #:export (lex-clojure))

(define char-set:clojure-delimiters
  (char-set-union char-set:whitespace
                  (char-set #\( #\) #\[ #\] #\{ #\} #\# #\,)))

(define char-set:clojure-symbol
  (char-set-complement char-set:clojure-delimiters))

(define %clojure-special-symbols
  '("def" "defn" "defn-" "defmulti" "defmethod" "defmacro" "defprotocol"
    "defrecord" "deftype" "defonce"
    "let" "letfn" "if" "if-not" "when" "when-not" "cond" "condp" "case"
    "loop" "recur" "do" "fn" "lambda" "λ"
    "->" "->" "as->" "cond->" "some->"
    "for" "doseq" "dotimes" "while"
    "try" "catch" "finally" "throw"
    "ns" "import" "require" "use"))

(define %clojure-special-prefixes
  '("def" "with-" "if-" "when-"))

(define (lex-clojure-dispatch)
  (lex-tag 'special
           (lex-any (lex-string "#{")
                    (lex-string "#\"")
                    (lex-string "#'")
                    (lex-string "#("))))

(define lex-clojure
  (lex-consume
   (lex-any (lex-char-set char-set:whitespace)
            (lex-char-set (char-set #\,))
            (lex-tag 'keyword
                     (lex-filter (lambda (str) (string-prefix? ":" str))
                                 (lex-char-set (char-set-complement char-set:clojure-delimiters))))
            (lex-tag 'open (lex-any* (map lex-string '("(" "[" "{" "#{" "#("))))
            (lex-tag 'close (lex-any* (map lex-string '(")" "]" "}"))))
            (lex-tag 'comment (lex-delimited ";" #:until "\n"))
            (lex-clojure-dispatch)
            (lex-tag 'special (lex-any (lex-string "@")
                                       (lex-string "^")
                                       (lex-string "`")
                                       (lex-string "~@")
                                       (lex-string "~")))
            (lex-tag 'special
                     (lex-filter (lambda (str)
                                   (or (any (cut string=? <> str) %clojure-special-symbols)
                                       (any (cut string-prefix? <> str) %clojure-special-prefixes)))
                                 (lex-char-set char-set:clojure-symbol)))
            (lex-tag 'string (lex-delimited "\""))
            (lex-tag 'symbol (lex-char-set char-set:clojure-symbol)))))
