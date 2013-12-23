;;; modes.el --- Emacs minor mode

;;; projectile

(use-package
  projectile
  :diminish projectile-mode
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; (setq projectile-require-project-root nil)
    (setq projectile-enable-caching t)
    (setq
      projectile-completion-system 'grizzl
      projectile-cache-file (expand-file-name  "projectile.cache" emacs-savefile-dir)
      projectile-known-projects-file (expand-file-name  "projectile.bookmarks" emacs-savefile-dir))))

;;; anzu

(use-package
  anzu
  :diminish anzu-mode
  :ensure t
  :init (global-anzu-mode t))

;;; guru

(use-package
  guru-mode
  :diminish guru-mode
  :ensure t
  :init (guru-global-mode t))

;;; powerline
(use-package
  powerline
  :ensure t
  :init (powerline-default-theme))

;;; key chords

(use-package
  key-chord
  :diminish key-chord-mode
  :ensure t
  :init (key-chord-mode t))

;;; yasnippet

(use-package
  yasnippet
  :if (not noninteractive)
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t)
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config (add-to-list 'yas-snippet-dirs emacs-snippets-dir))

;;; undo-tree

(use-package
  undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t))

;;; fill-column-indicator

(use-package
  fill-column-indicator
  :ensure t
  :config
  (progn
    (define-globalized-minor-mode
      global-fci-mode fci-mode
      (lambda () (fci-mode t)))
    (setq-default fci-rule-column 80)
    (setq-default fci-rule-width 2)
    (setq-default fci-dash-pattern 0.75)
    (setq-default fci-rule-use-dashes 1)
    (global-fci-mode t)))

;;; quickrun

(use-package
  quickrun
  :ensure t
  :bind ("C-c r" . quickrun))

;;; eshell

(use-package
  eshell
  :pre-load (setq eshell-directory-name (expand-file-name "eshell" emacs-savefile-dir))
  :config
  (progn
    (defun eshell/clear ()
      "04Dec2001 - sailor, to clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    ))

;;; ibuffer

(use-package
  ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (use-package ibuffer-vc :ensure t)
    (use-package ibuffer-git :ensure t)
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
  :ensure t
  :bind
  (("M-j" . ace-jump-mode)
   ("M-h" . ace-jump-mode-pop-mark))
  :config
  (progn
    (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))))

;;; smex
(use-package
  smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (setq smex-save-file (expand-file-name ".smex.items" emacs-savefile-dir))
    (smex-initialize)))

;;; window-numbering

(use-package
  window-numbering
  :ensure t
  :init (window-numbering-mode t))

;;; maxframe

(use-package
  maxframe
  :ensure t
  :init (add-hook 'window-setup-hook 'maximize-frame t))

;;; mic-paren

(use-package
  mic-paren
  :ensure t
  :init (paren-activate))

;;; winner

(use-package
  winner
  :diminish winner-mode
  :ensure t
  :if (not noninteractive)
  :init (winner-mode 1)
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo)))

;;; helm

(use-package
  helm-config
  :ensure helm
  :bind (("C-c h" . helm-mini)
         ("C-c M-x" . helm-M-x)
         ("C-h a" . helm-c-apropos)
         ("M-s a" . helm-do-grep)
         ("M-s b" . helm-occur)
         ("M-s F" . helm-for-files)))

;;; ido

(use-package
  ido
  :ensure t
  :init (ido-mode t)
  :config
  (progn
    (use-package ido-ubiquitous :ensure t)
    (use-package flx-ido :ensure t)
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
  :ensure auto-complete
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip :ensure t)
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
  :ensure auctex
  :config
  (progn
    ;;set xetex mode in tex/latex
    (add-to-list 'ac-modes 'latex-mode)
    (add-hook
      'LaTeX-mode-hook
      (lambda()

        (use-package ac-math :ensure t)
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

;;; douban-music-mode

(use-package douban-music-mode :commands douban-music)

;;; ibus-mode

(use-package
  ibus
  :init (add-hook 'after-init-hook 'ibus-mode-on)
  :config
  (progn
    (setq ibus-agent-file-name (expand-file-name "ibus/ibus-el-agent" emacs-packages-dir))
    ;; Use C-/ for Undo command
    (ibus-define-common-key ?\C-/ nil)
    (global-unset-key (kbd "C-SPC"))
    (global-set-key (kbd "C-SPC") 'ibus-toggle)
    (setq ibus-cursor-color '("#d28445" "#6a9fb5" "#90a959"))))

;;; css-mode

(use-package
  css-mode
  :ensure t
  :mode ("\\.css\\'" . css-mode))

;;; js3-mode

(use-package
  js3-mode
  :ensure t
  :mode ("\\.js\\'" . js3-mode))

;;; cmake-mode

(use-package
  cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;;; lua-mode

(use-package
  lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

;;; markdown-mode

(use-package
  markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;;; puppet-mode

(use-package
  puppet-mode
  :ensure t
  :mode ("\\.pp\\'" . puppet-mode))

;;; yaml-mode

(use-package
  yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" . yaml-mode))

;;; python-mode

(use-package
  python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    ; use IPython
    (setq-default py-shell-name "ipython")
    (setq-default py-which-bufname "IPython")
    (setq py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
    (setq py-force-py-shell-name-p t)
    (setq py-shell-switch-buffers-on-execute-p t)
    (setq py-switch-buffers-on-execute-p t)
    (setq py-split-windows-on-execute-p nil)
    (setq py-smart-indentation t)

    (defvar python-mode-initialized nil)
    (defun my-python-mode-hook ()
      (message "Execute my-python-mode-hook.")

      (unless python-mode-initialized
        (setq python-mode-initialized t)))

    (add-hook 'python-mode-hook 'my-python-mode-hook)))

;;; scala-mode

(use-package
  ensime
  :ensure t
  :load-path "ensime/src/main/elisp/"
  :pre-load (key-chord-define-global ".." 'ensime-expand-selection-command)
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :config
  (progn ()))

;;; themes

(use-package zenburn-theme :ensure t :disabled t)
(use-package solarized-theme :ensure t :disabled t)
(use-package color-theme-sanityinc-tomorrow :ensure t)

;;; end

(provide 'modes)
;;; modes.el ends here
