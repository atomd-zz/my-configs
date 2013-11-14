;;; packages.el --- Emacs minor mode

;;; projectile

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

;;; volatile-highlights

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init (volatile-highlights-mode t))

;;; anzu

(use-package anzu
  :diminish anzu-mode
  :init (global-anzu-mode t))

;;; guru

(use-package guru-mode
  :diminish guru-mode
  :init (guru-global-mode t))

;;; smart-mode-line

(use-package smart-mode-line
  :disabled t
  :config
  (progn
    (setq sml/theme 'dark)
    (sml/setup)))

;;; key chords

(use-package key-chord
  :diminish key-chord-mode
  :init (key-chord-mode t))

;;; undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t)
  :config (key-chord-define-global "uu" 'undo-tree-visualize))

;;; ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;;; ace-jump-mode

(use-package ace-jump-mode
  :bind
  (("M-j" . ace-jump-mode)
   ("M-h" . ace-jump-mode-pop-mark))
  :config
  (progn
    (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))))

;;; winner

(use-package winner
  :if (not noninteractive)
  :diminish winner-mode
  :init (winner-mode 1)
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo)))

;;; helm

(use-package helm-config
  :init
  (progn ())
  :bind (("C-c M-x" . helm-M-x)
         ("C-h a" . helm-c-apropos)
         ("M-s a" . helm-do-grep)
         ("M-s b" . helm-occur)
         ("M-s F" . helm-for-files)))

;;; ido

(use-package ido
  :init
  (progn
    (use-package ido-ubiquitous)
    (use-package flx-ido))
  :config
  (progn
    (setq
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.history" emacs-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-use-faces nil)
    (ido-mode 1)
    (ido-ubiquitous-mode 1)
    (flx-ido-mode 1)))

;;; css-mode

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

;;; js2-mode

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;;; cmake-mode

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;;; log4j-mode

(use-package log4j-mode
  :disabled t
  :mode ("\\.log\\'" . log4j-mode))

;;; lua-mode

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

;;; markdown-mode

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;; puppet-mode

(use-package puppet-mode
  :mode ("\\.pp\\'" . puppet-mode))

;;; yaml-mode

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;;; python-mode

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (defvar python-mode-initialized nil)
    (defun my-python-mode-hook ()
      (message "execute my-python-mode-hook.")
      (unless python-mode-initialized
        (setq python-mode-initialized t)

    (add-hook 'python-mode-hook 'my-python-mode-hook)))))

(provide 'packages)
;;; packages.el ends here
