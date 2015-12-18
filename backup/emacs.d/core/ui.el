;;; ui.el --- UI optimizations and tweaks.

(require 'cl)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; hl-line: highlight the current line
(global-hl-line-mode t)

;; Disable Scrollbar
(set-scroll-bar-mode 'nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; disable startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(column-number-mode t)
(size-indication-mode t)
(diminish 'visual-line-mode)

;;; powerline

(use-package
  powerline
  :disabled t  ;; disable
  :ensure t
  :init (powerline-default-theme))

;;; themes

(use-package
  zenburn-theme
  :disabled t  ;; disable
  :ensure t
  :init
  (load-theme 'zenburn t))

(use-package
  color-theme-sanityinc-tomorrow
  :ensure t
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package
 monokai-theme
  :disabled t  ;; disable
 :ensure t
 :init (load-theme 'monokai t))

(use-package
  sublime-themes
  :disabled t  ;; disable
  :ensure t
  :init
  ;  (load-theme 'hickey t)
  ;  (load-theme 'brin t)
  ;  (load-theme 'dorsey t)
  ;  (load-theme 'fogus t)
  ;  (load-theme 'graham  t)
  ;  (load-theme 'granger t)
  (load-theme 'junio t)
  ;  (load-theme 'spolsky t)
  ;  (load-theme 'wilson t)
  )

(provide 'ui)
;;; ui.el ends here
