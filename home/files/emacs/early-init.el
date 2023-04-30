;; Set eln-cache dir
(defvar %eln-cache (concat (getenv "XDG_CACHE_HOME") "/emacs/eln-cache/"))

(make-directory %eln-cache t)

;;  https://emacs.stackexchange.com/questions/70449/set-custom-location-for-eln-cache
;(when (boundp 'native-comp-eln-load-path)
;  (startup-redirect-eln-cache (expand-file-name %eln-cache user-emacs-directory)))

(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name (convert-standard-filename %eln-cache)
                            user-emacs-directory)))
