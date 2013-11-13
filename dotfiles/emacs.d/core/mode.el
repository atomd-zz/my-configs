;;; mode.el --- Emacs minor mode


;; projectile is a project management mode
(use-package projectile
  :diminish projectile-mode
  :init (projectile-global-mode)
  :config
  (progn
    ;; (setq projectile-enable-caching t)
    ;; (setq projectile-require-project-root nil)
    ;; (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (setq projectile-completion-system 'grizzl)
    (setq projectile-cache-file (expand-file-name  "projectile.cache" emacs-savefile-dir))
    (setq projectile-known-projects-file (expand-file-name  "projectile.bookmarks" emacs-savefile-dir))))

;;  Minor mode for visual feedback on some operations
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init (volatile-highlights-mode t))

;; anzu-mode enhances isearch by showing total matches and current match position
(use-package anzu
  :diminish anzu-mode
  :init (global-anzu-mode t))

;; Guru mode disables some common keybindings
;; and suggests the use of the established Emacs alternatives instead.
(use-package guru-mode
  :diminish guru-mode
  :init (guru-global-mode t))

(use-package smart-mode-line
  :config
  (progn
    (setq sml/theme 'dark)
    (sml/setup)))

(provide 'mode)
;;; mode.el ends here
