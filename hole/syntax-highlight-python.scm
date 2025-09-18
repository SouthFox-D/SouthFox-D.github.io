;;; guile-syntax-highlight --- General-purpose syntax highlighter
;;; Copyright © 2023 Skylar Chan <schan12@terpmail.umd.edu>
;;;
;;; Guile-syntax-highlight is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; Guile-syntax-highlight is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-syntax-highlight.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Syntax highlighting for Python.
;;
;;; Code:

(define-module (hole syntax-highlight-python)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (syntax-highlight lexers)
  #:export (lex-python))

(define %python-reserved-words
  '("False"   "await"     "else"     "import"    "pass"
    "None"    "break"     "except"   "in"        "raise"
    "True"    "class"     "finally"  "is"        "return"
    "and"     "continue"  "for"      "lambda"    "try"
    "as"      "def"       "from"     "nonlocal"  "while"
    "assert"  "del"       "global"   "not"       "with"
    "async"   "elif"      "if"       "or"        "yield"))

(define (python-reserved-word? str)
  "Return #t if STR is a python keyword."
  (any (cut string=? <> str) %python-reserved-words))

(define %python-builtin-functions
  '("abs"          "delattr"    "hash"        "memoryview"  "set"
    "all"          "dict"       "help"        "min"         "setattr"
    "any"          "dir"        "hex"         "next"        "slice"
    "ascii"        "divmod"     "id"          "object"      "sorted"
    "bin"          "enumerate"  "input"       "oct"         "staticmethod"
    "bool"         "eval"       "int"         "open"        "str"
    "breakpoint"   "exec"       "isinstance"  "ord"         "sum"
    "bytearray"    "filter"     "issubclass"  "pow"         "super"
    "bytes"        "float"      "iter"        "print"       "tuple"
    "callable"     "format"     "len"         "property"    "type"
    "chr"          "frozenset"  "list"        "range"       "vars"
    "classmethod"  "getattr"    "locals"      "repr"        "zip"
    "compile"      "globals"    "map"         "reversed"    "__import__"
    "complex"      "hasattr"    "max"         "round"))

(define (python-builtin-function? str)
  "Return #t if STR is a python built-in function."
  (any (cut string=? <> str) %python-builtin-functions))

(define %python-operators
  '("+"   "-"   "*"   "**"   "/"    "//"  "%"  "@"
    "<<"  ">>"  "&"   "|"    "^"    "~"   ":="
    "<"   ">"   "<="  ">="   "=="   "!="
    ;;  delimiters
    ","   ":"   "."   ";"    "@"    "="   "->"
    "+="  "-="  "*="  "/="   "//="  "%="  "@="
    "&="  "|="  "^="  ">>="  "<<="  "**="))

(define lex-python-operator
  (lex-any* (map lex-string %python-operators)))

(define char-set:python-identifier
  (char-set-adjoin char-set:letter+digit #\_))

(define lex-python-identifier
  (lex-char-set char-set:python-identifier))

(define lex-python
  (lex-consume
   (lex-any (lex-char-set char-set:whitespace)
            (lex-tag 'open (lex-any* (map lex-string '("(" "[" "{"))))
            (lex-tag 'close (lex-any* (map lex-string '(")" "]" "}"))))
            (lex-tag 'comment (lex-any (lex-delimited "#" #:until "\n")
                                       (lex-delimited "\"\"\"" #:until "\"\"\"")
                                       (lex-delimited "'''" #:until "'''")))
            (lex-tag 'special (lex-filter python-reserved-word? lex-python-identifier))
            (lex-tag 'operator lex-python-operator)
            (lex-tag 'keyword (lex-filter python-builtin-function? lex-python-identifier))
            (lex-tag 'symbol lex-python-identifier)
            (lex-tag 'string (lex-any (lex-delimited "\"")
                                      (lex-delimited "'"))))))
