;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

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
      (format #f "~a/.local/var/log")))

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
  (packages (specifications->packages (list "ripgrep"
                                            "neovim"
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
					    "emacs-spacemacs-theme"
					    "emacs-slime-company"
					    "guile"
					    "gdb"
                                            "emacs"
                                            "keepassxc"
					    "syncthing"
					    "curl"
					    "rsync"
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
		    `(("tmux/tmux.conf" ,(local-file "./files/tmux.conf"))))
     (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
			      ("vim" . "nvim")
			      ("gst" . "git status")
			      ("glog" . "git log")
                              ("ls" . "ls -p --color=auto")))
		   (environment-variables '(("KUBECONFIG" . "/home/sam/.config/kube")
					    ("JAVA_HOME" . "`guix build openjdk@17 | awk '/-jdk$/'`")
                                            ("EDITOR" . "nvim")))
                   (bashrc (list (local-file
                                  "/home/sam/guix/home/.bashrc"
                                  "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/sam/guix/home/.bash_profile"
                                        "bash_profile"))))))))

