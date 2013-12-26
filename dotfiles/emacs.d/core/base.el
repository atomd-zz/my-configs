;;; base.el --- Default package selection.

(require 'server)

(setq user-name "atomd")

(cd "~")

(defvar emacs-savefile-dir (expand-file-name "savefile/" emacs-home-dir))
(defvar emacs-snippets-dir (expand-file-name "snippets/" emacs-home-dir))
(defvar emacs-packages-dir (expand-file-name "packages/" emacs-home-dir))

(defconst emacs-start-time (current-time))

(setq gc-cons-threshold 50000000)
(setq message-log-max 16384)
(setq auto-save-list-file-prefix emacs-savefile-dir)
(setq pcache-directory (expand-file-name "pcache/" emacs-savefile-dir))
(setq url-configuration-directory (expand-file-name "url" emacs-savefile-dir))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir))
(unless (file-exists-p emacs-snippets-dir)
  (make-directory emacs-snippets-dir))
(unless (file-exists-p emacs-packages-dir)
  (make-directory emacs-packages-dir))
(unless (file-exists-p pcache-directory)
  (make-directory pcache-directory))

(defun utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (utf8-locale-p (getenv "LC_ALL"))
      (utf8-locale-p (getenv "LC_CTYPE"))
      (utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  (setq utf-translate-cjk-mode 'utf-8)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

;;; server
(unless (server-running-p) (server-start))

(provide 'base)
;;; base.el ends here
