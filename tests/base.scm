(define-module (tests base)
  #:use-module (hole reader)
  #:use-module (hole sxml)
  #:use-module (srfi srfi-64))


;;;
;;; Test suite.
;;;

(test-begin "sxml-test")


(test-equal (hole/org->sxml "233") '((p "233")))

(test-equal (hole/org->sxml "* H1") '((h1 (@ (id "H1")) "H1")))

(test-equal
    (hole/org->sxml "- 1\n- 2\n- 3")
  '((ul (li "1") (li "2") (li "3"))))

(test-equal
    (hole/org->sxml "- 1\n- 2\n-3")
  '((ul (li "1") (li "2" "\n" "-3"))))

(test-equal
    (hole/org->sxml "1. 1\n2. 2\n3. 3")
  '((ol (li "1") (li "2") (li "3"))))

(test-equal
    (hole/org->sxml "1. 1\n2. 2\n3.3")
  '((ol (li "1") (li "2" "\n" "3.3"))))

(test-equal
    (sxml-attribute-ref 'id '(h1 (@ (id "H1")) "H1"))
  "H1")

(test-end "sxml-test")
