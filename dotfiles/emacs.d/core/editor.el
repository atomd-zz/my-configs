;;; editor.el --- Enhanced editing experience.

;;;
(setq-default indent-tabs-mode nil)
(setq default-tab-width 8)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; highlight matching parentheses when the point is on them.
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(transient-mark-mode t)
(mouse-avoidance-mode 'animate)

(setq kill-ring-max 200)
(auto-image-file-mode)
(setq mouse-yank-at-point t)
(global-font-lock-mode t)
;(setq enable-recursive-minibuffers t)

;; version control
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)

;; Run at full power please
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq scroll-margin 5
      scroll-conservatively 10000)

(setq confirm-kill-emacs 'yes-or-no-p)

(provide 'editor)

;;; editor.el ends here
