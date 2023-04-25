(exec-path-from-shell-initialize) ;; use $PATH

(which-key-mode)

(global-company-mode)
(company-quickhelp-mode)

(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)

(setq inferior-lisp-program "guile")

;; HOOKZ
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'after-init-hook 
  (lambda () (load-theme 'spacemacs t)))
