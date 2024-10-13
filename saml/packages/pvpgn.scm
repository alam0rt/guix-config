(define-module (saml packages pvpgn)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement))

(define-public pvpgn-server
  (package
    (name "pvpgn-server")
    (version "1.99.7.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pvpgn/pvpgn-server")
              (commit "1.99.7.2.1")))
        (file-name (git-file-name name version))
        (sha256 (base32 "1z2b5sfarkfmhr6hn91yqphw8c5zp31bqrgdq2ncsmnzdx1krip9"))))
    (native-inputs
     (list gnu-make gcc-toolchain perl zlib))
    (inputs
     (list lua-5.1 sqlite))
    (build-system cmake-build-system)
    (arguments (list #:configure-flags #~(list "-D WITH_SQLITE3=true" "-D WITH_LUA=true")
                     #:build-type "Release"))
    (home-page "https://github.com/pvpgn/pvpgn-server")
    (synopsis "Player vs Player Gaming Network - PRO")
    (description "PvPGN is a free and open source cross-platform server software that supports Battle.net and and Westwood Online game clients. PvPGN-PRO is a fork of the official PvPGN project, whose development stopped in 2011, and aims to provide continued maintenance and additional features for PvPGN.")
    (license license:gpl2)))
