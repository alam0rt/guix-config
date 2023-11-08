;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (saml home home-configuration))

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
	     (ice-9 popen)
	     (ice-9 receive)
	     (ice-9 rdelim)
	     (gnu home services)
	     (gnu home services shepherd)
	     (gnu packages syncthing)
             (gnu home services shells))

(define %logdir
  (or (getenv "XDG_LOG_HOME")
      (format #f "~/.local/var/log")))

(define %syncthing-user-service
  (shepherd-service
    (documentation "Run `syncthing' without calling the browser")
    (provision '(syncthing))
    (start #~(make-forkexec-constructor
               (list #$(file-append syncthing "/bin/syncthing") "-no-browser")
               #:log-file (string-append #$%logdir "/syncthing.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t)))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "neovim"
                                            "emacs"
                                            "emacs-evil"
					    "emacs-company"
					    "emacs-geiser"
					    "emacs-geiser-guile"
					    "emacs-magit"
					    "emacs-flycheck"
					    "emacs-smartparens"
					    "emacs-evil-smartparens"
					    "emacs-highlight-indent-guides"
					    "emacs-lsp-mode"
					    "emacs-exec-path-from-shell"
					    "emacs-company-lsp"
					    "emacs-company-quickhelp"
					    "emacs-go-mode"
					    "emacs-rustic"
					    "emacs-which-key"
					    "emacs-markdown-mode"
					    "emacs-projectile"
					    "emacs-slime"
					    "emacs-straight-el"
					    "emacs-use-package"
					    "emacs-spacemacs-theme"
					    "emacs-emacsql"
					    "emacs-slime-company"
					    "emacs-rustic"
					    "emacs-org-roam"
					    "emacs-org-roam-ui"
					    "emacs-direnv"
					    "node" ;; for copilot.el
					    "calibre"
					    "direnv"
					    "rust"
					    "kubectl"
					    "flux"
					    "stern"
					    "gawk"
					    "font-inconsolata"
					    "guile"
					    "gdb"
                                            "keepassxc"
					    "syncthing"
					    "ripgrep"
					    "curl"
					    "rsync"
					    "tor"
					    "proxychains-ng"
					    "unzip"
                                            "firefox"
                                            "irssi")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list 
     (service home-shepherd-service-type
	      (home-shepherd-configuration
		(services (list %syncthing-user-service))))
     (simple-service 'emacs-configuration
		    home-xdg-configuration-files-service-type
		      `(("emacs", (local-file "./files/emacs" #:recursive? #t))))
     (simple-service 'configz
		    home-xdg-configuration-files-service-type
		    `(("tmux/tmux.conf" ,(local-file "./files/tmux.conf"))
		      ("containers/registries.conf" ,(local-file "./files/containers/registries.conf"))
		      ("containers/policy.json" ,(local-file "./files/containers/policy.json"))
		      ("proxychains/proxychains.conf" ,(plain-file "proxychains.conf" "
# proxychains.conf  VER 4
#
#        HTTP, SOCKS4, SOCKS5 tunneling proxifier with DNS.
#	
strict_chain
proxy_dns
tcp_read_time_out 15000
tcp_connect_time_out 8000

[ProxyList]
socks4 	127.0.0.1 9050 # tor
"))
		      ("git/config" ,(local-file "./files/gitconfig"))))
     (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("grep" . "grep --color=auto")
			      ("ll" . "ls -l")
			      ("vim" . "nvim")
			      ("gst" . "git status")
			      ("glog" . "git log")
			      ("gco" . "git checkout")
			      ("k" . "kubectl")
			      ("kgp" . "kubectl get pods")
                              ("ls" . "ls -p --color=auto")))
		   (environment-variables '(("KUBECONFIG" . "/home/sam/.config/kube")
					    ("JAVA_HOME" . "`guix build openjdk@17 | awk '/-jdk$/'`")
					    ("PROXYCHAINS_CONF_FILE" . "/home/sam/.config/proxychains/proxychains.conf")
                                            ("EDITOR" . "nvim")))
                   (bashrc
		    (list (local-file "./files/bashrc" "bashrc")))
                   (bash-profile
		    (list (local-file "./files/bash_profile" "bash_profile"))))))))

