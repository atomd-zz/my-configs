;;; ui.el --- UI optimizations and tweaks.

(require 'cl)

(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;(menu-bar-mode 0)

;; Disable Scrollbar
(set-scroll-bar-mode 'nil)

;; Don't use OSX Native fullscreen mode
;(setq ns-use-native-fullscreen nil)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; Customize line numbers
(setq linum-format 'dynamic)
(add-hook 'linum-before-numbering-hook
          (lambda ()
            (unless (display-graphic-p)
              (setq linum-format
                    (lambda (line)
                      (propertize
                        (format
                          (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d ")) line)
                        'face
                        'linum))))))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(global-linum-mode t)
(global-visual-line-mode t)
(column-number-mode t)
(size-indication-mode t)

;;; the default theme

; (load-theme 'zenburn t)
(load-theme 'sanityinc-tomorrow-night t)

(provide 'ui)
;;; ui.el ends here
