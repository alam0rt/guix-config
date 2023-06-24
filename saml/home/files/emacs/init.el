(setq package-enable-at-startup nil)

;; stop those pesky native-comp warnings on boot
(setq native-comp-async-report-warnings-errors 'silent)


(defvar utils/sorted-absolute-init-scripts)
(let* ((emacs-dir (file-name-as-directory (concat (getenv "XDG_CONFIG_HOME") "/emacs" )))
       (default-directory emacs-dir)
       (init-scripts (file-expand-wildcards "???-*.el"))
       (sorted-init-scripts (sort init-scripts 'string=)))
  (setq utils/sorted-absolute-init-scripts
	(mapcar (lambda (name) (concat emacs-dir name)) sorted-init-scripts)))

(mapcar #'load utils/sorted-absolute-init-scripts)
