(use-modules (guix packages)
             (guix build-system copy)
             (guix profiles)
             (guix licenses)
             (gnu packages base)
             (gnu packages guile-xyz))

(define %minimal-glibc-locales
  (make-glibc-utf8-locales
   glibc
   #:locales (list "en_US")
   #:name "glibc-utf8-locales"))

(define-public minimal-glibc-locales
  (package
   (name "minimal-glibc-locales")
   (version "master")
   (source (plain-file "hello" "Hello World!"))
   (build-system copy-build-system)
   (arguments (list
               #:install-plan #~'(("hello" "share/"))
               #:phases
               #~(modify-phases
                  %standard-phases
                  (add-after 'unpack 'move-locales
                             (lambda _
                               (mkdir-p (string-append #$output "/share"))
                               (invoke
                                "cp" "-r"
                                (string-append #$(this-package-input "glibc-utf8-locales")
                                               "/lib")
                                (string-append #$output "/")))))))
   (inputs (list %minimal-glibc-locales))
   (synopsis "x")
   (description "x")
   (home-page "x")
   (license expat)))

(packages->manifest
 (list minimal-glibc-locales
       guile-syntax-highlight
       haunt))
