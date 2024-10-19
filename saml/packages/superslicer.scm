(define-module (saml packages superslicer)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools))

(define-public superslicer
  (package
    (name "superslicer")
    (version "2.5.59.0")
    (source
      (origin
        (method git-fetch)
        (uri
	  (git-reference
            (url "https://github.com/supermerill/SuperSlicer")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "181y4cl3181f6574i1q2frp21f2fm1xvscpjm2362pb6jh6im7c9"))
        (modules '((guix build utils)))
        (snippet
         '(begin
	    ;; Prusa slicer bundles a lot of dependencies in src/ directory.
	    ;; Most of them contain prusa-specific modifications (e.g. avrdude),
	    ;; but others do not. Here we replace the latter with Guix packages.
	    ;; Remove bundled libraries that were not modified by Prusa Slicer developers.
	    (delete-file-recursively "src/hidapi")
	    (delete-file-recursively "src/eigen")
	    (delete-file-recursively "src/libigl/igl")
	    (substitute* "src/CMakeLists.txt"
		(("add_subdirectory\\(libigl\\)" all)
		(string-append
		all "\ninclude_directories(libigl INTERFACE libigl::core)"))
		(("add_subdirectory\\(hidapi\\)")
		"pkg_check_modules(HIDAPI REQUIRED hidapi-hidraw)")
		(("include_directories\\(hidapi/include\\)")
		"include_directories()"))
	    (substitute* "src/slic3r/CMakeLists.txt"
		(("add_library\\(libslic3r_gui.*" all)
		(string-append
		all
		"\ntarget_include_directories(libslic3r_gui PUBLIC ${HIDAPI_INCLUDE_DIRS})\n"))
		(("\\bhidapi\\b") "${HIDAPI_LIBRARIES}"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DSLIC3R_FHS=1" ;; Use The Filesystem Hierarchy Standard.
         "-DSLIC3R_GTK=3" ;; Use GTK+
	 "-DSLIC3R_GUI=no" ;; Do not build the GUI.
         ;; Use wxWidgets 3.0.x.x to prevent GUI crashes when adding support enforcers.
         "-DSLIC3R_WX_STABLE=1")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list boost
           cereal
           cgal
           curl
           dbus
           eigen
           expat
           glew
           glib
           gmp
           gtk+
           hidapi
           ilmbase
           libigl
           libjpeg-turbo
           libpng
           mesa
           mpfr
           nlopt
           opencascade-occt
           openvdb
           pango
           tbb
           eudev
           ;; prusa-slicer 2.5 segfaults on startup with wxwidgets 3.2
           ;; See https://github.com/prusa3d/PrusaSlicer/issues/8299
           wxwidgets-3.0
           zlib))
    (home-page "https://github.com/supermerill/SuperSlicer")
    (synopsis "A PrusaSlicer fork (which is a slic3r fork)")
    (description "G-code generator for 3D printers (Prusa, Voron, Creality, etc.)")
    (license license:agpl3)
    (properties '((tunable? . #t)))))

;; This allows you to run guix shell -f guix-packager.scm.
;; Remove this line if you just want to define a package.
superslicer
