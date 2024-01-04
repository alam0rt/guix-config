(setq package-enable-at-startup nil)

;; stop those pesky native-comp warnings on boot
(setq native-comp-async-report-warnings-errors 'silent)

;; since guix has a read only $XDG_HOME_DIR, use a different directory for straight-el
(setq straight-base-dir (concat (getenv "HOME") "/.straight-el"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar utils/sorted-absolute-init-scripts)
(let* ((emacs-dir (file-name-as-directory (concat (getenv "XDG_CONFIG_HOME") "/emacs" )))
       (default-directory emacs-dir)
       (init-scripts (file-expand-wildcards "???-*.el"))
       (sorted-init-scripts (sort init-scripts 'string=)))
  (setq utils/sorted-absolute-init-scripts
	(mapcar (lambda (name) (concat emacs-dir name)) sorted-init-scripts)))

(mapcar #'load utils/sorted-absolute-init-scripts)
