(define-module (hole syntax-highlight javascript)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (syntax-highlight lexers)
  #:export (lex-javascript))

(define %javascript-reserved-words
  '("break" "case" "catch" "class" "const" "continue" "debugger"
    "default" "delete" "do" "else" "export" "extends" "finally"
    "for" "function" "if" "import" "in" "instanceof" "new"
    "return" "super" "switch" "this" "throw" "try" "typeof"
    "var" "void" "while" "with" "yield" "let" "static"
    "enum" "await" "async" "true" "false" "null" "undefined"))

(define (javascript-reserved-word? str)
  "Return #t if STR is a javascript keyword."
  (any (cut string=? <> str) %javascript-reserved-words))

(define %javascript-builtins
  '("Array" "Boolean" "Date" "Error" "EvalError" "Function"
    "JSON" "Math" "Number" "Object" "RangeError" "ReferenceError"
    "RegExp" "String" "SyntaxError" "TypeError" "URIError"
    "Map" "Set" "WeakMap" "WeakSet" "Promise" "Proxy" "Reflect"
    "Symbol" "console" "window" "document" "process" "global"
    "eval" "parseInt" "parseFloat" "isNaN" "isFinite" "decodeURI"
    "encodeURI" "decodeURIComponent" "encodeURIComponent"))

(define (javascript-builtin? str)
  "Return #t if STR is a javascript built-in object/function."
  (any (cut string=? <> str) %javascript-builtins))

(define %javascript-operators
  '("+"  "-"  "*"  "/"  "%"  "**"  "++"  "--"
    "=="  "===" "!="  "!==" ">"   "<"   ">="  "<="
    "&&"  "||"  "!"   "&"   "|"   "^"   "~"   "<<"  ">>"  ">>>"
    "="   "+="  "-="  "*="  "/="  "%="  "**=" "<<=" ">>=" ">>>="
    "&="  "|="  "^="  "&&=" "||=" "??="
    "?"   ":"   "?."  "??"  "..." "=>"
    "."   ","   ";"))

(define lex-javascript-operator
  (lex-any* (map lex-string %javascript-operators)))

(define char-set:javascript-identifier
  (char-set-adjoin char-set:letter+digit #\_ #\$))

(define lex-javascript-identifier
  (lex-char-set char-set:javascript-identifier))

(define lex-javascript
  (lex-consume
   (lex-any (lex-char-set char-set:whitespace)
            (lex-tag 'open (lex-any* (map lex-string '("(" "[" "{"))))
            (lex-tag 'close (lex-any* (map lex-string '(")" "]" "}"))))
            (lex-tag 'comment (lex-any (lex-delimited "//" #:until "\n")
                                       (lex-delimited "/*" #:until "*/")))
            (lex-tag 'special (lex-filter javascript-reserved-word? lex-javascript-identifier))
            (lex-tag 'operator lex-javascript-operator)
            (lex-tag 'keyword (lex-filter javascript-builtin? lex-javascript-identifier))
            (lex-tag 'symbol lex-javascript-identifier)
            (lex-tag 'string (lex-any (lex-delimited "\"")
                                      (lex-delimited "'")
                                      (lex-delimited "`"))))))
