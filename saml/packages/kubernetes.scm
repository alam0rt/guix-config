; stolen from https://codeberg.org/allana/guix-system/src/branch/main/allana/packages/kubernetes.scm
(define-module (saml packages kubernetes)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (rnrs lists)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public containerd
  (package
    (name "containerd")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/containerd/containerd/releases/download/v" version "/containerd-" version "-linux-amd64.tar.gz"))
       (sha256 (base32 "19r21qwcj4z8d0lghyzbam9mzq9hslnrqm5l21jqah5ba80wfm97"))))
    (build-system copy-build-system)
    (native-inputs
    `(("patchelf" ,patchelf)
      ("glibc" ,glibc)))
    (arguments
	`(#:substitutable? #f
	  #:phases (modify-phases %standard-phases
			(add-after 'unpack 'chmod
				(lambda* (#:key #:allow-other-keys)
					 (let* ((patchelf (assoc-ref %build-inputs "patchelf"))
						(patchelf (string-append patchelf "/bin/patchelf"))
						(containerd "containerd")
						(glibc (assoc-ref %build-inputs "glibc")))
					    (chmod "containerd" #o755)
					    (invoke patchelf "--set-interpreter"
						(string-append glibc "/lib/ld-linux-x86-64.so.2")
						    containerd)))))
          #:install-plan '(("containerd" "/bin/"))))
    (home-page "")
    (synopsis "")
    (description
     "")
    (license license:asl2.0)))


(define-public kops
  (package
    (name "kops")
    (version "1.22.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kubernetes/kops")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1346w3hiidnk4w838ymp5lmq6v7r88h48m7bpmz3r3l8n9g41km9"))))
    (build-system go-build-system)
    (native-inputs `(("git" ,git)
                     ("gawk" ,gawk)
                     ("sed" ,sed)))
    (inputs `(("python" ,python)
              ("perl" ,perl)
              ("ruby" ,ruby)
              ("node" ,node)))
    (arguments
     '(#:import-path "k8s.io/kops/cmd/kops"
       #:unpack-path "k8s.io/kops"
       #:install-source? #f
       #:tests? #f))
    (home-page "https://kops.sigs.k8s.io/")
    (synopsis "The easiest way to get a production grade Kubernetes
cluster up and running")
    (description
     "kops helps you create, destroy, upgrade and maintain
production-grade, highly available, Kubernetes clusters from the
command line. AWS (Amazon Web Services) is currently officially
supported, with GCE and OpenStack in beta support, and VMware vSphere
in alpha, and other platforms planned")
    (license license:asl2.0)))

(define-public kubectl
  (package
    (name "kubectl")
    (version "1.25.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.k8s.io/release/v" version "/bin/linux/amd64/kubectl"))
       (sha256 (base32 "046bqy1swn3if6qcf1vj5ih58xxl2k5wrwwfi8mhswrh6qxym9da"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
		  (add-after 'unpack 'chmod
		    (lambda* (#:key #:allow-other-keys)
		      (chmod "kubectl" #o755))))
       #:substitutable? #f
       #:install-plan '(("kubectl" "/bin/"))))
    (synopsis "kubectl binary")
    (description "kubectl binary")
    (home-page "https://github.com/kubernetes/kubernetes")
    (license license:asl2.0)))


(define-public kustomize
  (package
    (name "kustomize")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/kubernetes-sigs/" name "/releases/download/" name "/v" version "/" name "_v" version "_linux_amd64.tar.gz"))
       (sha256 (base32 "13dp5yqss3a2smrgihaa890zcpvhnglh57gp53gzp17gdarj79nw"))))
    (build-system copy-build-system)
    (arguments
     `(#:substitutable? #f
       #:install-plan '(("kustomize" "/bin/"))))
    (synopsis "Customization of kubernetes YAML configurations")
    (description "kustomize lets you customize raw, template-free YAML files for
multiple purposes, leaving the original YAML untouched and usable as
is.")
    (home-page "https://kustomize.io/")
    (license license:asl2.0)))

(define-public helm
  (package
    (name "helm")
    (version "3.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://get.helm.sh/" name "-v" version "-linux-amd64.tar.gz"))
       (sha256 (base32 "12aajxkvds97q4k5agpis85z1pn8bcxq1p85c8x9myydsi05sbfa"))))
    (build-system copy-build-system)
    (arguments
     `(#:substitutable? #f
       #:install-plan '(("helm" "/bin/"))))
    (synopsis "The Kubernetes Package Manager.")
    (description "Helm is a tool for managing Charts. Charts are packages of
pre-configured Kubernetes resources.")
    (home-page "https://helm.sh/")
    (license license:asl2.0)))

(define-public terraform
  (package
    (name "terraform")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.hashicorp.com/" name "/" version "/" name "_" version "_linux_amd64.zip"))
       (sha256 (base32 "1a86yk2n7f5xc79rjmc6zw6w5aq53hch1kc9l4alnm8irm0yj46f"))))
    (build-system copy-build-system)
    (arguments
     `(#:substitutable? #f
       #:install-plan '(("terraform" "/bin/"))))
    (native-inputs (list unzip))
    (synopsis "Terraform is a tool for building, changing, and versioning
infrastructure safely and efficiently.")
    (description "Terraform enables you to safely and predictably create, change, and
improve infrastructure. It is an open source tool that codifies APIs
into declarative configuration files that can be shared amongst team
members, treated as code, edited, reviewed, and versioned.")
    (home-page "https://helm.sh/")
    (license license:mpl2.0)))

(define-public argocd
  (package
    (name "argocd")
    (version "2.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/argoproj/argo-cd/releases/download/v" version "/argocd-linux-amd64"))
       (sha256 (base32 "0bgxg4br531pjs9galvsz8001rvapf3f2wc89n1rzazpf8dmbxw1"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
		  (add-after 'unpack 'chmod
		    (lambda* (#:key #:allow-other-keys)
		      (chmod "argocd-linux-amd64" #o755))))
       #:substitutable? #f
       #:install-plan '(("argocd-linux-amd64" "/bin/argocd"))))
    (synopsis "argocd binary")
    (description "argocd binary")
    (home-page "https://argo-cd.readthedocs.io")
    (license license:asl2.0)))

(define-public argo
  (package
    (name "argo")
    (version "3.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/argoproj/argo-workflows/releases/download/v" version "/argo-linux-amd64.gz"))
       (sha256 (base32 "0nsxkrjdlmyzs9y6xn626d6fzy0i2fj1jcflims7l6bb3mva09m5"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
		  (add-after 'unpack 'chmod
		    (lambda* (#:key #:allow-other-keys)
		      (chmod "argo-linux-amd64" #o755))))
       #:substitutable? #f
       #:install-plan '(("argo-linux-amd64" "/bin/argo"))))
    (synopsis "argo binary")
    (description "argo binary")
    (home-page "https://argoproj.github.io/argo-workflows/")
    (license license:asl2.0)))


(define-public stern
  (package
    (name "stern")
    (version "1.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stern/stern/releases/download/v" version "/stern_" version "_linux_amd64.tar.gz"))
       (sha256 (base32 "0chyiqypb1sh719vfspj79kmwhrxzn7pkhcmqavyf480b90avj9z"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
		  (add-after 'unpack 'chmod
		    (lambda* (#:key #:allow-other-keys)
		      (chmod "stern" #o755))))
       #:substitutable? #f
       #:install-plan '(("stern" "/bin/stern"))))
    (synopsis "stern binary")
    (description "stern binary")
    (home-page "https://github.com/stern/stern")
    (license license:asl2.0)))

(define-public flux
  (package
    (name "flux")
    (version "2.0.0-rc.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fluxcd/flux2/releases/download/v" version "/flux_" version "_linux_amd64.tar.gz"))
       (sha256 (base32 "187kga8yz2qmfqjz50kwg35575mv5n926qbr7jyybdicxvzq31rn"))))
    (build-system copy-build-system)
    (arguments
     `(#:substitutable? #f
       #:install-plan '(("flux" "/bin/"))))
    (synopsis "Open and extensible continuous delivery solution for Kubernetes.")
    (description "Flux is a tool for keeping Kubernetes clusters in sync with sources of
configuration (like Git repositories and OCI artifacts), and automating updates to
configuration when there is new code to deploy.")
    (home-page "https://fluxcd.io/")
    (license license:asl2.0)))

