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
       guile-3.0
       guile-syntax-highlight
       haunt
       pagefind-bin
       python
       python-hy
       python-requests
       python-brotli
       python-fonttools))
