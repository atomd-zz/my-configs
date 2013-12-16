;;; packages.el --- Emacs minor mode

;;; projectile

(use-package
  projectile
  :diminish projectile-mode
  :init (projectile-global-mode)
  :config
  (progn
    ;; (setq projectile-enable-caching t)
    ;; (setq projectile-require-project-root nil)
    (setq
      projectile-completion-system 'grizzl
      projectile-cache-file (expand-file-name  "projectile.cache" emacs-savefile-dir)
      projectile-known-projects-file (expand-file-name  "projectile.bookmarks" emacs-savefile-dir))))

;;; anzu

(use-package
  anzu
  :diminish anzu-mode
  :init (global-anzu-mode t))

;;; guru

(use-package
  guru-mode
  :diminish guru-mode
  :init (guru-global-mode t))

;;; powerline
(use-package
  powerline
  :init (powerline-default-theme))

;;; key chords

(use-package
  key-chord
  :diminish key-chord-mode
  :init (key-chord-mode t))

;;; yasnippet

(use-package
  yasnippet
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (add-to-list 'yas-snippet-dirs emacs-snippets-dir)
  :init (yas-global-mode t))

;;; undo-tree

(use-package
  undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t)
  :config (key-chord-define-global "uu" 'undo-tree-visualize))

;;; fill-column-indicator

(use-package
  fill-column-indicator
  :config
  (progn
    (define-globalized-minor-mode global-fci-mode fci-mode 
                                  (lambda () (fci-mode t)))
    (setq-default fci-rule-column 80)
    (setq-default fci-rule-width 2)
    (setq-default fci-dash-pattern 0.75)
    (setq-default fci-rule-use-dashes 1)
    (global-fci-mode t)))

;;; ibuffer

(use-package
  ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    ;; (use-package ibuffer-vc)
    (use-package ibuffer-git)

    (setq
      ibuffer-show-empty-filter-groups nil
      ibuffer-marked-char ?✓)

    (defvar my-ibuffer-filter-groups
      `(
        ("Terminals" (mode . term-mode))
        ("emacs.d" (filename . ,(expand-file-name "~/.emacs.d/")))
        ("Help" (or (mode . Man-mode)
                    (mode . woman-mode)
                    (mode . Info-mode)
                    (mode . Help-mode)
                    (mode . help-mode)))
        ("Emacs internal" (or (name . "*Messages*")
                              (name . "*Completions*")
                              (name . "*Helm log*")
                              (name . "*helm recentf*")
                              (name . "*ESS*")
                              (name . "*Compile-Log*")))
        ("Gnus" (or
                  (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.bbdb$")
                  (name . "^\\.newsrc-dribble")))
        ("Gtags" (name . "^\\*GTAGS SELECT\\*"))
        ("Special" (name . "^\\*.*\\*$"))
        ("Dired" (mode . dired-mode)))
      "My custom pre-set groups for ibuffer.")

    ;; Clean automatically created buffers
    (defun my-ibuffer-clean ()
      "Clean automatically created buffers"
      (interactive)
      (ibuffer-unmark-all ?*)
      (ibuffer-mark-by-mode 'help-mode)
      (ibuffer-mark-by-mode 'magit-mode)
      (ibuffer-mark-by-mode 'occur-mode)
      (ibuffer-mark-by-mode 'grep-mode)
      (ibuffer-mark-by-mode 'dired-mode)
      (ibuffer-mark-by-mode 'completion-list-mode)
      (ibuffer-mark-by-mode 'compilation-mode)
      (ibuffer-mark-by-mode 'Man-mode)
      (ibuffer-mark-by-mode 'browse-kill-ring-mode)
      (ibuffer-mark-by-name-regexp "*anything*")
      (ibuffer-mark-by-name-regexp "*ESS*")
      (ibuffer-mark-by-name-regexp "*Shell Command Output*")
      (ibuffer-mark-by-name-regexp "*Compile-Log*")
      (ibuffer-mark-by-name-regexp "*vc-diff*")
      (ibuffer-do-delete))

    (defun my-ibuffer-filter-groups ()
      "Generate my very own grouping of buffers. Just for fun."
      (append my-ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root)))

    (define-ibuffer-column
      size-h
      (:name "Size" :inline t)
      (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))

    (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
               "Open ibuffer with cursor pointed to most recent buffer name"
               (let ((recent-buffer-name (buffer-name)))
                 ad-do-it
                 (ibuffer-jump-to-buffer recent-buffer-name)))
    (ad-activate 'ibuffer)

    (defun my-ibuffer-hook ()
      (interactive)
      (ibuffer-do-sort-by-vc-status)
      (setq ibuffer-filter-groups (my-ibuffer-filter-groups))
      (setq ibuffer-formats
            '((mark modified read-only " "
                    (name 18 18 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " " filename-and-process)
              (mark modified read-only vc-status-mini git-status-mini " "
                    (name 18 18 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " "
                    (vc-status 16 16 :left)
                    " "
                    (git-status 8 8 :left)
                    " " filename-and-process)))
      (ibuffer-update nil t))

    (add-hook 'ibuffer-hook #'my-ibuffer-hook)
    (define-key ibuffer-mode-map (kbd "M-D") 'my-ibuffer-clean)
    (define-key ibuffer-mode-map [?s ?v] #'ibuffer-do-sort-by-vc-status)))

;;; ace-jump-mode

(use-package
  ace-jump-mode
  :bind
  (("M-j" . ace-jump-mode)
   ("M-h" . ace-jump-mode-pop-mark))
  :config
  (progn
    (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))))

;;; smex
(use-package
  smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (setq smex-save-file (expand-file-name ".smex.items" emacs-savefile-dir))
    (smex-initialize)))

;;; winner

(use-package
  winner
  :diminish winner-mode
  :if (not noninteractive)
  :init (winner-mode 1)
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo)))

;;; helm

(use-package
  helm-config
  :bind (("C-c M-x" . helm-M-x)
         ("C-h a" . helm-c-apropos)
         ("M-s a" . helm-do-grep)
         ("M-s b" . helm-occur)
         ("M-s F" . helm-for-files)))

;;; ido

(use-package
  ido
  :init (ido-mode t)
  :config
  (progn
    (use-package ido-ubiquitous)
    (use-package flx-ido)
    (setq
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name ".ido.history" emacs-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-use-faces nil)
    (ido-ubiquitous-mode t)
    (flx-ido-mode t)))


;;; auto-complete

(use-package
  auto-complete-config
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip)
    (ac-config-default))
  :config
  (progn
    (ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)
    (setq ac-quick-help-prefer-pos-tip t)
    (setq ac-use-quick-help t)
    (setq ac-quick-help-delay 1.0)
    (setq ac-fuzzy-enable t)
    (setq ac-trigger-commands
          (cons 'backward-delete-char-untabify ac-trigger-commands))
    (setq ac-comphist-file (concat emacs-savefile-dir "ac-comphist.dat"))
    ;(ac-linum-workaround)
    ;(setq popup-use-optimized-column-computation nil)
    (unbind-key "C-s" ac-completing-map)))

;;; auctex

(use-package
  tex-site
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn
    ;;set xetex mode in tex/latex
    (add-to-list 'ac-modes 'latex-mode)
    (add-hook
      'LaTeX-mode-hook
      (lambda()

        (use-package ac-math)
        (setq ac-math-unicode-in-math-p t)
        (nconc ac-sources
               '(ac-source-math-unicode ac-source-math-latex
                                        ac-source-latex-commands))

        (add-to-list
          'TeX-command-list
          '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))

        (setq TeX-command-default "XeLaTeX"
              TeX-auto-save t
              TeX-save-query nil
              ; TeX-parse-self t
              ; TeX-auto-untabify t
              ; TeX-global-PDF-mode t
              TeX-show-compilation t)
        ))))

;;; css-mode

(use-package
  css-mode
  :mode ("\\.css\\'" . css-mode))

;;; js2-mode

(use-package
  js2-mode
  :mode ("\\.js\\'" . js2-mode))

;;; cmake-mode

(use-package
  cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;;; log4j-mode

(use-package
  log4j-mode
  :disabled t
  :mode ("\\.log\\'" . log4j-mode))

;;; lua-mode

(use-package
  lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

;;; markdown-mode

(use-package
  markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;; puppet-mode

(use-package
  puppet-mode
  :mode ("\\.pp\\'" . puppet-mode))

;;; yaml-mode

(use-package
  yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))

;;; python-mode

(use-package
  python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (defvar python-mode-initialized nil)
    (defun my-python-mode-hook ()
      (message "Execute my-python-mode-hook.")

      (unless python-mode-initialized
        (setq python-mode-initialized t)
        ))
    (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;; elpy

(use-package
  elpy
  :init (elpy-enable)
  :config
  (progn
    (elpy-clean-modeline)
    (elpy-use-ipython)))

;;; end

(provide 'packages)
;;; packages.el ends here
