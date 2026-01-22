(use-modules (guix packages)
             (guix download)
             (guix build-system copy)
             (guix profiles)
             (guix licenses)
             (guix utils)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages python)
             (gnu packages python-compression)
             (gnu packages python-web)
             (gnu packages python-xyz))

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

(define-public pagefind-bin
  (package
   (name "pagefind-bin")
   (version "1.5.0-beta.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/Pagefind/pagefind/releases/download/v"
                  version "/pagefind_extended-v" version "-"
                  (cond ((target-aarch64?)
                         "aarch64")
                        ((target-x86-64?)
                         "x86_64")
                        (else ""))
                  "-unknown-linux-musl.tar.gz"))
            (sha256
             (base32
              (cond ((target-aarch64?)
                     "1y22cx9cxkg231g7m99kjkxpi1fcc4zh2hdk0hsr4601qfifcg1j")
                    ((target-x86-64?)
                     "094m8vrbzskc223jbhj2n2bzacavbhcpsgjqdlz5bycrvibycl7c")
                    (else ""))))))
   (build-system copy-build-system)
   (arguments
    (list #:install-plan #~'(("pagefind_extended" "bin/"))
          #:phases
          #~(modify-phases %standard-phases
                           (delete 'strip))))
   (supported-systems (list "aarch64-linux" "x86_64-linux"))
   (home-page "https://pagefind.app/")
   (synopsis "Static low-bandwidth search at scale ")
   (description
    "Pagefind is a fully static search library that aims to perform well on large
sites, while using as little of your users’ bandwidth as possible, and without
hosting any infrastructure.")
   (license expat)
   (properties '((upstream-name . "pagefind")))))

(packages->manifest
 (list bash
       minimal-glibc-locales
       guile-3.0
       guile-syntax-highlight
       haunt
       pagefind-bin
       python
       python-hy
       python-requests
       python-brotli
       python-fonttools))
