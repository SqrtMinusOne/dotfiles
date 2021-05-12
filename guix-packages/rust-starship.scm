(define-module (rust-starship)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics))


(define-public rust-either-1
  (package
   (name "rust-either")
   (version "1.6.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "either" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0mwl9vngqf5jvrhmhn9x60kr5hivxyjxbmby2pybncxfqhf4z3g7"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/bluss/either")
   (synopsis
    "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
   (description
    "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
   (license (list license:expat license:asl2.0))))

(define-public rust-which-4
  (package
    (name "rust-which")
    (version "4.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zixp9631knhnvd8c3si4wn01fldq063s86jxlmwxwmx5kj52mdm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-either" ,rust-either-1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/harryfei/which-rs.git")
    (synopsis
      "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
      "This package provides a Rust equivalent of Unix command \"which\".  Locate installed executable in cross platforms.")
    (license license:expat)))

(define-public rust-versions-3
  (package
    (name "rust-versions")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "versions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qdimx0gqmyybg2j2qpynq2zw202glv01zh0x4chdrhgwphayq4c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-itertools" ,rust-itertools-0.10)
         ("rust-nom" ,rust-nom-6)
         ("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/fosskers/rs-versions")
    (synopsis
      "A library for parsing and comparing software version numbers.")
    (description
      "This package provides a library for parsing and comparing software version numbers.")
    (license license:expat)))

(define-public rust-sys-info-0.9
  (package
    (name "rust-sys-info")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sys-info" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fiqhnj1rk69rahz4077lzs8x72gv4zcyknqdg7k359k97pfrz1k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/FillZpp/sys-info-rs")
    (synopsis
      "Get system information in Rust.

For now it supports Linux, Mac OS X, illumos, Solaris, FreeBSD, OpenBSD, and Windows.
")
    (description
      "Get system information in Rust.

For now it supports Linux, Mac OS X, illumos, Solaris, FreeBSD, OpenBSD, and Windows.
")
    (license license:expat)))

(define-public rust-strsim-0.10
  (package
    (name "rust-strsim")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis
      "Implementations of string similarity metrics. Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.
")
    (description
      "Implementations of string similarity metrics.  Includes Hamming, Levenshtein,
OSA, Damerau-Levenshtein, Jaro, Jaro-Winkler, and SÃ¸rensen-Dice.
")
    (license license:expat)))

(define-public rust-starship-module-config-derive-0.2
  (package
    (name "rust-starship-module-config-derive")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri
               "starship_module_config_derive"
               version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09327jvaf2b4xwg0s8l3afrrg140illa0ff6wkwfi4i8pl7dpacp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://starship.rs")
    (synopsis
      "The minimal, blazing-fast, and infinitely customizable prompt for any shell! â\x98\x84ð\x9f\x8c\x8cï¸\x8f
")
    (description
      "The minimal, blazing-fast, and infinitely customizable prompt for any shell! â\x98\x84ð\x9f\x8c\x8cï¸\x8f
")
    (license license:isc)))

(define-public rust-shadow-rs-0.5
  (package
    (name "rust-shadow-rs")
    (version "0.5.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shadow-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0193qj586xpbz2c1aa7g3vpxfvhz1z251wl488m0ygrxvrxhw3hi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-git2" ,rust-git2-0.13))))
    (home-page
      "https://github.com/baoyachi/shadow-rs")
    (synopsis
      "A build-time information stored in your rust project")
    (description
      "This package provides a build-time information stored in your rust project")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.64")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y9gk3yikncrc0zajmwc0pidr7zfwafawb4gidf6mqyskzf9g7kr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-indexmap" ,rust-indexmap-1)
         ("rust-itoa" ,rust-itoa-0.4)
         ("rust-ryu" ,rust-ryu-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
      "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.125")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vfhndim4sa1i1x38dyvrxyq5v8zxjs0av05ldfkn82qpfibg4xh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.125")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w8i0f4wsq4zd9vz1k6lq00066rjrgzlxkm25h8sfpss387cb3am"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis
      "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-dlv-list-0.2
  (package
    (name "rust-dlv-list")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dlv-list" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06r1nskj3x56p5wqz2bgl6q3rpyymrb0k0zpbvk8c6qcd4mkzpv8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rand" ,rust-rand-0.8))))
    (home-page
      "https://github.com/sgodwincs/dlv-list-rs")
    (synopsis
      "Semi-doubly linked list implemented using a vector")
    (description
      "Semi-doubly linked list implemented using a vector")
    (license license:expat)))

(define-public rust-ordered-multimap-0.3
  (package
    (name "rust-ordered-multimap")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ordered-multimap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1194q7sb2d6chbllsn7237dhhvx04iqr3sq0ii16w1pcv5x2qrqw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-dlv-list" ,rust-dlv-list-0.2)
         ("rust-hashbrown" ,rust-hashbrown-0.9)
         ("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/sgodwincs/ordered-multimap-rs")
    (synopsis "Insertion ordered multimap")
    (description "Insertion ordered multimap")
    (license license:expat)))

(define-public rust-rust-ini-0.17
  (package
    (name "rust-rust-ini")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-ini" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08hfh6p2svznza3m07vavsc4c8x4g6d715sz58rzh73sm551qiv3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-ordered-multimap"
          ,rust-ordered-multimap-0.3)
         ("rust-unicase" ,rust-unicase-2))))
    (home-page
      "https://github.com/zonyitoo/rust-ini")
    (synopsis
      "An Ini configuration file parsing library in Rust")
    (description
      "An Ini configuration file parsing library in Rust")
    (license license:expat)))

(define-public rust-regex-syntax-0.6
  (package
    (name "rust-regex-syntax")
    (version "0.6.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16y87hz1bxmmz6kk360cxwfm3jnbsxb3x4zw9x1gzz7khic2i5zl"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      "This package provides a regular expression parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p478fqf4nia2ma0kv4npb8x1hli0zz6k16517ikb51jkryx8sxi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2))))
    (home-page
      "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))

(define-public rust-aho-corasick-0.7
  (package
    (name "rust-aho-corasick")
    (version "0.7.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vv50b3nvkhyy7x7ip19qnsq11bqlnffkmj2yx2xlyk5wzawydqy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-memchr" ,rust-memchr-2))))
    (home-page
      "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching.")
    (description
      "Fast multiple substring searching.")
    (license (list license:unlicense license:expat))))

(define-public rust-regex-1
  (package
    (name "rust-regex")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qf479kjbmb582h4d1d6gfl75h0j8aq2nrdi5wg6zdcy6llqcynh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-aho-corasick" ,rust-aho-corasick-0.7)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-regex-syntax" ,rust-regex-syntax-0.6))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
finite automata and guarantees linear time matching on all inputs.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-quick-xml-0.22
  (package
    (name "rust-quick-xml")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-xml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ssk30ymrd1724g36qjnnql225i6p31jm09cb46sval2hd6g2cw5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-encoding-rs" ,rust-encoding-rs-0.8)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tafia/quick-xml")
    (synopsis
      "High performance xml reader and writer")
    (description
      "High performance xml reader and writer")
    (license license:expat)))

(define-public rust-process-control-3
  (package
    (name "rust-process-control")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "process_control" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zyv7amca12rw69xfwiglmpzyimagcpkmp3984k64n7c1cjhqxvj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crossbeam-channel"
          ,rust-crossbeam-channel-0.5)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/dylni/process_control")
    (synopsis
      "Methods for ergonomically running processes with timeouts
")
    (description
      "Methods for ergonomically running processes with timeouts
")
    (license (list license:expat license:asl2.0))))

(define-public rust-pest-2
  (package
    (name "rust-pest")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lry80bm90x47nq71wxq83kjrm9ashpz4kbm92p90ysdx4m8gx0h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-ucd-trie" ,rust-ucd-trie-0.1))))
    (home-page "https://pest-parser.github.io/")
    (synopsis "The Elegant Parser")
    (description "The Elegant Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-path-slash-0.1
  (package
    (name "rust-path-slash")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "path-slash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06dnnmd3fvmr9ngwgj0xrfj9s8h09m9dgf3zlqsbalzk9wybpb1w"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/rhysd/path-slash")
    (synopsis
      "Conversion to/from a file path from/to slash path")
    (description
      "Conversion to/from a file path from/to slash path")
    (license license:expat)))

(define-public rust-os-info-3
  (package
    (name "rust-os-info")
    (version "3.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "os_info" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vkrzsv9ajz201slgj90l4s49rbqanb18ywqfcswskyhi5z6iamg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-serde" ,rust-serde-1)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/stanislav-tkach/os_info")
    (synopsis
      "Detect the operating system type and version.")
    (description
      "Detect the operating system type and version.")
    (license license:expat)))

(define-public rust-open-1
  (package
    (name "rust-open")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "open" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "083vcabairxahinl8j6407b7lp7ylkxqsv9i1xdx6jnf655yn48p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-which" ,rust-which-4)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Byron/open-rs")
    (synopsis
      "Open a path or URL using the program configured on the system")
    (description
      "Open a path or URL using the program configured on the system")
    (license license:expat)))

(define-public rust-once-cell-1
  (package
    (name "rust-once-cell")
    (version "1.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "once_cell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18qmpyfigg4ibdhjy5mwcjhzk9adwlgfaqv7nj430ivm86q0i2xg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-parking-lot" ,rust-parking-lot-0.11))))
    (home-page
      "https://github.com/matklad/once_cell")
    (synopsis
      "Single assignment cells and lazy values.")
    (description
      "Single assignment cells and lazy values.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zvariant-derive-2
  (package
    (name "rust-zvariant-derive")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zvariant_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h8j2j9js0n455a1bafc8p18l3qy774rqf238fwj0l534m1w7mr7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-crate"
          ,rust-proc-macro-crate-0.1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description
      "D-Bus & GVariant encoding & decoding")
    (license license:expat)))

(define-public rust-zvariant-2
  (package
    (name "rust-zvariant")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zvariant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y7qzrqzqxskzkhq1jwawrzbwjb6a8851rpcn54my4raa1i753k7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-enumflags2" ,rust-enumflags2-0.6)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-bytes" ,rust-serde-bytes-0.11)
         ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page
      "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description
      "D-Bus & GVariant encoding & decoding")
    (license license:expat)))

(define-public rust-zbus-macros-1
  (package
    (name "rust-zbus-macros")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zbus_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14sziniysx55501jq1fiimg93c3d8hydpdcjkfw831p455hcb0m4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro-crate"
          ,rust-proc-macro-crate-0.1)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "proc-macros for zbus")
    (description "proc-macros for zbus")
    (license license:expat)))

(define-public rust-serde-xml-rs-0.4
  (package
    (name "rust-serde-xml-rs")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-xml-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ykx1xkfd59gf0ijnp93xhpd457xy4zi8xv2hrr0ikvcd6h1pgzh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-serde" ,rust-serde-1)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page
      "https://github.com/RReverser/serde-xml-rs")
    (synopsis
      "xml-rs based deserializer for Serde (compatible with 0.9+)")
    (description
      "xml-rs based deserializer for Serde (compatible with 0.9+)")
    (license license:expat)))

(define-public rust-enumflags2-derive-0.6
  (package
    (name "rust-enumflags2-derive")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enumflags2_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kkcwi4n76bi1c16ms00dyk4d393gdf29kpr4k9zsn5z7m7fjvll"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/NieDzejkob/enumflags2")
    (synopsis
      "Do not use directly, use the reexport in the `enumflags2` crate. This allows for better compatibility across versions.")
    (description
      "Do not use directly, use the reexport in the `enumflags2` crate.  This allows for better compatibility across versions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-enumflags2-0.6
  (package
    (name "rust-enumflags2")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enumflags2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "182xd6cxxmadx1axnz6x73d12pzgwkc712zq2lxd4z1k48lxij43"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-enumflags2-derive"
          ,rust-enumflags2-derive-0.6)
         ("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/NieDzejkob/enumflags2")
    (synopsis "Enum-based bit flags")
    (description "Enum-based bit flags")
    (license (list license:expat license:asl2.0))))

(define-public rust-zbus-1
  (package
    (name "rust-zbus")
    (version "1.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "zbus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bcx5vf75iscgfdn3pszkbr6n20pvddry24lnws4xb53g71sq9i3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-io" ,rust-async-io-1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-derivative" ,rust-derivative-2)
         ("rust-enumflags2" ,rust-enumflags2-0.6)
         ("rust-fastrand" ,rust-fastrand-1)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-nb-connect" ,rust-nb-connect-1)
         ("rust-nix" ,rust-nix-0.17)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-polling" ,rust-polling-2)
         ("rust-scoped-tls" ,rust-scoped-tls-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.4)
         ("rust-serde-repr" ,rust-serde-repr-0.1)
         ("rust-zbus-macros" ,rust-zbus-macros-1)
         ("rust-zvariant" ,rust-zvariant-2))))
    (home-page
      "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "API for D-Bus communication")
    (description "API for D-Bus communication")
    (license license:expat)))

(define-public rust-xml-rs-0.6
  (package
    (name "rust-xml-rs")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xml-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18g7krn8zx8ifml83w91w2hvw437j5q3vaw4cvx3kryccj5860pl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/netvl/xml-rs")
    (synopsis "An XML library in pure Rust")
    (description "An XML library in pure Rust")
    (license license:expat)))

(define-public rust-winrt-0.4
  (package
    (name "rust-winrt")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winrt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13dl7i2w2mqc6s0wj38s4wkpkp10gvkjwb22bbf87c125slcnc3y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/microsoft/windows-rs")
    (synopsis "Windows Runtime language projection")
    (description
      "Windows Runtime language projection")
    (license (list license:expat license:asl2.0))))

(define-public rust-strum-macros-0.8
  (package
    (name "rust-strum-macros")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strum_macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f3xv8x1f93kv2wi24h9gqg8446hw894914f28xsn37bg045k11k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-quote" ,rust-quote-0.3)
         ("rust-syn" ,rust-syn-0.11))))
    (home-page
      "https://github.com/Peternator7/strum")
    (synopsis
      "Helpful macros for working with enums and strings")
    (description
      "Helpful macros for working with enums and strings")
    (license license:expat)))

(define-public rust-strum-0.8
  (package
    (name "rust-strum")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1np7ymlq2402l3i6ljxgfdmxxf5akp927zkzahg08zji1xry99jc"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/Peternator7/strum")
    (synopsis
      "Helpful macros for working with enums and strings")
    (description
      "Helpful macros for working with enums and strings")
    (license license:expat)))

(define-public rust-winrt-notification-0.2
  (package
    (name "rust-winrt-notification")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winrt-notification" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i013b5zwx9y3hlwnwdx6cr3fy4bpahz4kdlva14d2k8h6r0wyap"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-strum" ,rust-strum-0.8)
         ("rust-strum-macros" ,rust-strum-macros-0.8)
         ("rust-winapi" ,rust-winapi-0.3)
         ("rust-winrt" ,rust-winrt-0.4)
         ("rust-xml-rs" ,rust-xml-rs-0.6))))
    (home-page
      "https://github.com/allenbenz/winrt-notification")
    (synopsis
      "An incomplete wrapper over the WinRT toast api")
    (description
      "An incomplete wrapper over the WinRT toast api")
    (license license:expat)))

(define-public rust-mac-notification-sys-0.3
  (package
    (name "rust-mac-notification-sys")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mac-notification-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xnsrr4102rbka57198v7gkfhivl54a456ax765x7758m5qnpyrx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-dirs" ,rust-dirs-1)
         ("rust-objc-foundation"
          ,rust-objc-foundation-0.1))))
    (home-page
      "https://github.com/h4llow3En/mac-notification-sys")
    (synopsis
      "Thin wrapper around macOS Notifications.")
    (description
      "Thin wrapper around macOS Notifications.")
    (license license:expat)))

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libdbus-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w06ycq2mw8zfp9j73macgdl8d2881bnxbzdyyxys90ljyya64nw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-dbus-0.9
  (package
    (name "rust-dbus")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dbus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ny01n0gzfdmcy5ydn4q78pamidj4c5q9ixz7gr97dbrza6y15zm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-futures-channel"
          ,rust-futures-channel-0.3)
         ("rust-futures-executor"
          ,rust-futures-executor-0.3)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libdbus-sys" ,rust-libdbus-sys-0.2))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
      "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-notify-rust-4
  (package
    (name "rust-notify-rust")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "notify-rust" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0837mkz9x31r7ckhhfn6ga3jchrzxxdb7pa29jkq3vvkavlcfkr2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-chrono" ,rust-chrono-0.4)
         ("rust-dbus" ,rust-dbus-0.9)
         ("rust-image" ,rust-image-0.23)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-mac-notification-sys"
          ,rust-mac-notification-sys-0.3)
         ("rust-serde" ,rust-serde-1)
         ("rust-winrt-notification"
          ,rust-winrt-notification-0.2)
         ("rust-zbus" ,rust-zbus-1)
         ("rust-zvariant" ,rust-zvariant-2)
         ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page
      "https://github.com/hoodie/notify-rust")
    (synopsis
      "Show desktop notifications (linux, bsd, mac). Pure Rust dbus client and server.")
    (description
      "Show desktop notifications (linux, bsd, mac).  Pure Rust dbus client and server.")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-1
  (package
    (name "rust-indexmap")
    (version "1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wxfh55zlrlpdxfcvvvj6wwc46f23cnb0j9q71190yl9pyh4aj42"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-autocfg" ,rust-autocfg-1)
         ("rust-hashbrown" ,rust-hashbrown-0.9)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis
      "A hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys. It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index. A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (description
      "This package provides a hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the key-value
pairs is independent of the hash values of the keys.  It has the usual
hash table functionality, it preserves insertion order except after
removals, and it allows lookup of its elements by either hash table key
or numerical index.  A corresponding hash set type is also provided.

This crate was initially published under the name ordermap, but it was renamed to
indexmap.
")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libgit2-sys-0.12
  (package
    (name "rust-libgit2-sys")
    (version "0.12.20+1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jv60axsz8nv3pll66hpv0bclb6sbaxp487798csvf80gs8hjbqy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cc" ,rust-cc-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libssh2-sys" ,rust-libssh2-sys-0.2)
         ("rust-libz-sys" ,rust-libz-sys-1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page
      "https://github.com/rust-lang/git2-rs")
    (synopsis
      "Native bindings to the libgit2 library")
    (description
      "Native bindings to the libgit2 library")
    (license (list license:expat license:asl2.0))))

(define-public rust-git2-0.13
  (package
    (name "rust-git2")
    (version "0.13.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "git2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "067sfa0pb9qjrkzpahiiq4j7nklpnhm4py8lm9l9zslx4gkrv4hp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libgit2-sys" ,rust-libgit2-sys-0.12)
         ("rust-log" ,rust-log-0.4)
         ("rust-openssl-probe" ,rust-openssl-probe-0.1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-url" ,rust-url-2))))
    (home-page
      "https://github.com/rust-lang/git2-rs")
    (synopsis
      "Bindings to libgit2 for interoperating with git repositories. This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.
")
    (description
      "Bindings to libgit2 for interoperating with git repositories.  This library is
both threadsafe and memory safe and allows both reading and writing git
repositories.
")
    (license (list license:expat license:asl2.0))))

(define-public rust-gethostname-0.2
  (package
    (name "rust-gethostname")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gethostname" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a609j9dhk816il2f2a01avvi5sqzxh0p38nxwrja7dcpybf54p6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/lunaryorn/gethostname.rs")
    (synopsis "gethostname for all platforms")
    (description "gethostname for all platforms")
    (license license:asl2.0)))

(define-public rust-byte-unit-4
  (package
    (name "rust-byte-unit")
    (version "4.0.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byte-unit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n5w327g55ly5r4yjh71rhv2ifw615xdxv8d2rj5nxsbxgk9fc86"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1)
         ("rust-utf8-width" ,rust-utf8-width-0.1))))
    (home-page "https://magiclen.org/byte-unit")
    (synopsis
      "A library for interaction with units of bytes.")
    (description
      "This package provides a library for interaction with units of bytes.")
    (license license:expat)))

(define-public rust-uom-0.30
  (package
    (name "rust-uom")
    (version "0.30.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vg59hnb7hh0p8kjjhgmrsnn3597722lkfdkp481wksq6vk06rg7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-bigint" ,rust-num-bigint-0.3)
         ("rust-num-rational" ,rust-num-rational-0.3)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-serde" ,rust-serde-1)
         ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/iliekturtles/uom")
    (synopsis "Units of measurement")
    (description "Units of measurement")
    (license (list license:asl2.0 license:expat))))

(define-public rust-mach-0.3
  (package
   (name "rust-mach")
   (version "0.3.2")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "mach" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1yksa8lwzqh150gr4417rls1wk20asy9vhp8kq5g9n7z58xyh8xq"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-libc" ,rust-libc-0.2)
       ("rust-rustc-std-workspace-core"
        ,rust-rustc-std-workspace-core-1))))
   (home-page "https://github.com/fitzgen/mach")
   (synopsis
    "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
   (description
    "This package provides a Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
   (license (list license:asl2.0 license:bsd-2))))

(define-public rust-lazycell-1
  (package
   (name "rust-lazycell")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "lazycell" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))))
   (build-system cargo-build-system)
   (arguments
    `(#:skip-build?
      #t
      #:cargo-inputs
      (("rust-clippy" ,rust-clippy-0.0)
       ("rust-serde" ,rust-serde-1))))
   (home-page "https://github.com/indiv0/lazycell")
   (synopsis
    "A library providing a lazily filled Cell struct")
   (description
    "This package provides a library providing a lazily filled Cell struct")
   (license (list license:expat license:asl2.0))))

(define-public rust-battery-0.7
  (package
    (name "rust-battery")
    (version "0.7.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "battery" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r1641dyks76p39i1iihswhc6iz5z51pihmpxniy1h1pi4k29dml"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if-1)
         ("rust-core-foundation"
          ,rust-core-foundation-0.7)
         ("rust-lazycell" ,rust-lazycell-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-mach" ,rust-mach-0.3)
         ("rust-nix" ,rust-nix-0.19)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-uom" ,rust-uom-0.30)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/svartalf/rust-battery")
    (synopsis
      "Cross-platform information about the notebook batteries")
    (description
      "Cross-platform information about the notebook batteries")
    (license (list license:asl2.0 license:expat))))

(define-public rust-wildmatch-1
  (package
    (name "rust-wildmatch")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wildmatch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02i7qwjy1rkhzp80v9i9khzf09rhr4d534wcap7i6hfkc9gvji3z"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/becheran/wildmatch")
    (synopsis
      "Simple string matching  with questionmark and star wildcard operator.")
    (description
      "Simple string matching  with questionmark and star wildcard operator.")
    (license license:expat)))

(define-public rust-attohttpc-0.17
  (package
    (name "rust-attohttpc")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "attohttpc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bg0d95smcd5nrp5d3h1c3w1vlizdrvnq412jcrjn9jpahqdm2ws"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-encoding-rs" ,rust-encoding-rs-0.8)
         ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
         ("rust-flate2" ,rust-flate2-1)
         ("rust-http" ,rust-http-0.2)
         ("rust-log" ,rust-log-0.4)
         ("rust-mime" ,rust-mime-0.3)
         ("rust-multipart" ,rust-multipart-0.17)
         ("rust-native-tls" ,rust-native-tls-0.2)
         ("rust-openssl" ,rust-openssl-0.10)
         ("rust-rustls" ,rust-rustls-0.18)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-urlencoded"
          ,rust-serde-urlencoded-0.6)
         ("rust-url" ,rust-url-2)
         ("rust-webpki" ,rust-webpki-0.21)
         ("rust-webpki-roots" ,rust-webpki-roots-0.19)
         ("rust-wildmatch" ,rust-wildmatch-1))))
    (home-page "https://github.com/sbstp/attohttpc")
    (synopsis "Small and lightweight HTTP client")
    (description "Small and lightweight HTTP client")
    (license license:mpl2.0)))

(define-public rust-starship-0.53
  (package
   (name "rust-starship")
   (version "0.53.0")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "starship" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1l4yr4b02nm7l199yfx7m3li3kl7sl4mrv5ah8pnvrfr4gbmpa0x"))))
   (build-system cargo-build-system)
   (arguments
    `(#:tests? #f
      #:rust ,rust-1.51
      #:cargo-inputs
      (("rust-ansi-term" ,rust-ansi-term-0.12)
       ("rust-attohttpc" ,rust-attohttpc-0.17)
       ("rust-battery" ,rust-battery-0.7)
       ("rust-byte-unit" ,rust-byte-unit-4)
       ("rust-chrono" ,rust-chrono-0.4)
       ("rust-clap" ,rust-clap-2)
       ("rust-dirs-next" ,rust-dirs-next-2)
       ("rust-gethostname" ,rust-gethostname-0.2)
       ("rust-git2" ,rust-git2-0.13)
       ("rust-indexmap" ,rust-indexmap-1)
       ("rust-log" ,rust-log-0.4)
       ("rust-native-tls" ,rust-native-tls-0.2)
       ("rust-nix" ,rust-nix-0.20)
       ("rust-notify-rust" ,rust-notify-rust-4)
       ("rust-once-cell" ,rust-once-cell-1)
       ("rust-open" ,rust-open-1)
       ("rust-os-info" ,rust-os-info-3)
       ("rust-path-slash" ,rust-path-slash-0.1)
       ("rust-pest" ,rust-pest-2)
       ("rust-pest-derive" ,rust-pest-derive-2)
       ("rust-process-control" ,rust-process-control-3)
       ("rust-quick-xml" ,rust-quick-xml-0.22)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-rayon" ,rust-rayon-1)
       ("rust-regex" ,rust-regex-1)
       ("rust-rust-ini" ,rust-rust-ini-0.17)
       ("rust-semver" ,rust-semver-0.11)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-shadow-rs" ,rust-shadow-rs-0.5)
       ("rust-shadow-rs" ,rust-shadow-rs-0.5)
       ("rust-shell-words" ,rust-shell-words-1)
       ("rust-starship-module-config-derive"
        ,rust-starship-module-config-derive-0.2)
       ("rust-strsim" ,rust-strsim-0.10)
       ("rust-sys-info" ,rust-sys-info-0.9)
       ("rust-term-size" ,rust-term-size-0.3)
       ("rust-toml" ,rust-toml-0.5)
       ("rust-unicode-segmentation"
        ,rust-unicode-segmentation-1)
       ("rust-unicode-width" ,rust-unicode-width-0.1)
       ("rust-urlencoding" ,rust-urlencoding-1)
       ("rust-versions" ,rust-versions-3)
       ("rust-which" ,rust-which-4)
       ("rust-winapi" ,rust-winapi-0.3)
       ("rust-yaml-rust" ,rust-yaml-rust-0.4))
      #:cargo-development-inputs
      (("rust-tempfile" ,rust-tempfile-3))))
   (home-page "https://starship.rs")
   (inputs
    `(("zlib" ,zlib)
      ("pkg-config" ,pkg-config)
      ("openssl" ,openssl)))
   (synopsis
    "The minimal, blazing-fast, and infinitely customizable prompt for any shell! â\x98\x84ð\x9f\x8c\x8cï¸\x8f
")
   (description
    "The minimal, blazing-fast, and infinitely customizable prompt for any shell! â\x98\x84ð\x9f\x8c\x8cï¸\x8f
")
   (license license:isc)))
