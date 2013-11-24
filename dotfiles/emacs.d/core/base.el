;; base.el --- Default package selection.

(setq user-name "atomd")

(cd "~")

(setq gc-cons-threshold 50000000)

(setq message-log-max 16384)

(setq auto-save-list-file-prefix emacs-savefile-dir)
(setq pcache-directory
      (let ((dir (concat emacs-savefile-dir "pcache/")))
	(make-directory dir t)
	dir))

(defconst emacs-start-time (current-time))

(provide 'base)
;; base.el ends here
