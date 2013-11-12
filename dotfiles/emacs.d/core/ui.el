;;; ui.el --- UI optimizations and tweaks.

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode 0)

;; Disable Scrollbar
(set-scroll-bar-mode 'nil)

;; Don't use OSX Native fullscreen mode
(setq ns-use-native-fullscreen nil)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; Customize line numbers
(setq linum-format " %d ")

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(global-linum-mode t)
(global-visual-line-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; use monokai as the default theme
(load-theme 'monokai t)

(provide 'ui)
;;; ui.el ends here
