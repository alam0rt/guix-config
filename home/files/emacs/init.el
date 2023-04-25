(setq package-enable-at-startup nil)

(defvar utils/sorted-absolute-init-scripts)
(let* ((emacs-dir (file-name-as-directory "."))
       (default-directory emacs-dir)
       (init-scripts (file-expand-wildcards "???-*.el"))
       (sorted-init-scripts (sort init-scripts 'string=)))
  (setq utils/sorted-absolute-init-scripts
	(mapcar (lambda (name) (concat emacs-dir name)) sorted-init-scripts)))

(mapcar #'load utils/sorted-absolute-init-scripts)
