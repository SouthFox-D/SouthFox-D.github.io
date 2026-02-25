(define-module (tests base)
  #:use-module (hole reader)
  #:use-module (srfi srfi-64))


;;;
;;; Test suite.
;;;

(test-begin "sxml-test")


(test-equal (hole/org->sxml "233") '((p "233")))

(test-equal (hole/org->sxml "* H1") '((h1 (@ (id "H1")) "H1")))


(test-end "sxml-test")
