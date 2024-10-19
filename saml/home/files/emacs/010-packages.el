(exec-path-from-shell-initialize) ;; use $PATH

(which-key-mode)

(global-company-mode)
(company-quickhelp-mode)

(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)

;; enable rg bindings for emacs
(rg-enable-default-bindings)

(setq inferior-lisp-program "guile")

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :ensure t)

;; change lsp cache from r/o config dir
(setq lsp-session-file (concat (getenv "XDG_CACHE_HOME") "/.lsp-session-v1"))

;; HOOKZ
(add-hook 'scheme-mode-hook
	  (lambda () geiser))
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'after-init-hook 
          (lambda () (load-theme 'doom-sourcerer t)))


;; a lambda which executes the command `guix pull` and prints the output
