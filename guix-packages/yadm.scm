(define-module (yadm)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gawk))

(define-public yadm
  (package
   (name "yadm")
   (version "3.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/TheLocehiliosan/yadm/")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0ga0p28nvqilswa07bzi93adk7wx6d5pgxlacr9wl9v1h6cds92s"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f
      #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'patch-file-names
                                (lambda _
                                  (substitute* "yadm"
                                               (("(.*)_PROGRAM=\"(.*)\"" all var prog)
                                                (format #f "~a_PROGRAM=~s" var (which prog))))
                                  (substitute* "yadm"
                                               (("AWK_PROGRAM=(.*)")
                                                (format #f "AWK_PROGRAM=(~s)\n" (which "gawk"))))
                                  (substitute* "yadm" (("/bin/sh") "/bin/bash"))))
                     (delete 'bootstrap)
                     (delete 'configure)
                     (delete 'build))))
   (propagated-inputs
    `(("git" ,git)
      ("gnupg" ,gnupg)
      ("gawk" ,gawk)
      ("pinentry" ,pinentry)))
   (synopsis "Yet Another Dotfile Manager")
   (home-page "https://yadm.io")
   (license gpl3)
   (description "When you live in a command line, configurations are a deeply personal thing. They are often crafted over years of experience, battles lost, lessons learned, advice followed, and ingenuity rewarded. When you are away from your own configurations, you are an orphaned refugee in unfamiliar and hostile surroundings. You feel clumsy and out of sorts. You are filled with a sense of longing to be back in a place you know. A place you built. A place where all the short-cuts have been worn bare by your own travels. A place you proudly call… $HOME.")))
