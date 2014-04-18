;;; base.el --- Default package selection.

(cd "~")
(setq user-name "atomd")
(defconst emacs-start-time (current-time))

(setq gc-cons-threshold 50000000)
(setq message-log-max 16384)
(setq default-major-mode 'text-mode)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defun check-subdirectory (name dir)
  "Make sure that the subdirectory exists"
  (let ((subdir (expand-file-name name dir)))
    (unless (file-exists-p subdir) (make-directory subdir)) subdir))

;; custom file
(setq custom-file (expand-file-name "custom.el" emacs-home-dir))
(load custom-file)

;; create directories
(setq emacs-cache-dir (check-subdirectory "cache/" emacs-home-dir)
      emacs-snippets-dir (check-subdirectory "snippets/" emacs-home-dir)
      emacs-packages-dir (check-subdirectory "packages/" emacs-home-dir)
      ;; cache directory for each mode
      pcache-directory (check-subdirectory "pcache/" emacs-cache-dir)
      url-configuration-directory (check-subdirectory "url/" emacs-cache-dir)
      projectile-directory (check-subdirectory "projectile/" emacs-cache-dir)
      smex-directory (check-subdirectory "smex/" emacs-cache-dir)
      auto-complete-directory (check-subdirectory "auto-complete/" emacs-cache-dir)
      ido-directory (check-subdirectory "ido/" emacs-cache-dir)
      session-directory (check-subdirectory "session/" emacs-cache-dir)
      eshell-directory (check-subdirectory "eshell/" emacs-cache-dir)
      auto-save-list-directory (check-subdirectory "auto-save-list/" emacs-cache-dir))

;; auto save files
(setq auto-save-list-file-prefix (expand-file-name "saves-" auto-save-list-directory)
      auto-save-file-name-transforms `((".*" ,auto-save-list-directory t)))

;; backup files
(setq backup-directory-alist `((".*" . ,auto-save-list-directory))
      make-backup-files t ;; do make backups
      backup-by-copying t ;; and copy them here
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" emacs-cache-dir)
      bookmark-save-flag 1)


;; abbrevs (abbreviations)
(setq abbrev-file-name (expand-file-name "abbrev_defs" emacs-home-dir))
; (abbrev-mode t)                        ;; enable abbrevs (abbreviations)
; (setq default-abbrev-mode t            ;; turn it on
;       save-abbrevs t)                  ;; don't ask
; (when (file-exists-p abbrev-file-name)
;   (quietly-read-abbrev-file))          ;;  don't tell
; (add-hook 'kill-emacs-hook             ;; write when
;           'write-abbrev-file)          ;; exiting emacs


;; overrride the default function....
(defun emacs-session-filename (session-id)
  (concat session-directory session-id))

(defun utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (utf8-locale-p (getenv "LC_ALL"))
      (utf8-locale-p (getenv "LC_CTYPE"))
      (utf8-locale-p (getenv "LANG"))))

;; set the character set
(when (or window-system (locale-is-utf8-p))
  (setq utf-translate-cjk-mode 'utf-8)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

;; server
(require 'server)
(setq server-use-tcp t)
(unless (server-running-p) (server-start))

(provide 'base)
;;; base.el ends here
