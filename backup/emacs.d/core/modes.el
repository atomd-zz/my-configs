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
      projectile-cache-file (expand-file-name  "cache" projectile-directory)
      projectile-known-projects-file (expand-file-name  "bookmarks" projectile-directory))))

;;; Customize line numbers

(use-package
  nlinum
  :ensure t
  :init
  (progn
    (global-nlinum-mode t)
    (setq nlinum-format-function
          (lambda (line)
            (let* ((fmt (format " %%%dd " (- nlinum--width 2)))
                   (str (propertize (format fmt line) 'face 'linum)))
              str)))))

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
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)  ;; TODO
  :config (add-to-list 'yas-snippet-dirs emacs-snippets-dir))

;;; expand-region

(use-package
  expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;; undo-tree

(use-package
  undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode t))


;;; quickrun

(use-package
  quickrun
  :ensure t
  :bind ("C-c r" . quickrun))

;;; eshell

(use-package
  eshell
  :pre-load (setq eshell-directory-name eshell-directory)
  :config
  (progn
    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))))

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

    (defvar ff/ibuffer-filter-groups
      `(
        ("Terminals" (mode . term-mode))
        ("emacs.d" (filename . ,(expand-file-name emacs-home-dir)))
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
    (defun ff/ibuffer-clean ()
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

    (defun ff/ibuffer-filter-groups ()
      "Generate my very own grouping of buffers. Just for fun."
      (append ff/ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root)))

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

    (defun ff/ibuffer-hook ()
      (interactive)
      (ibuffer-do-sort-by-vc-status)
      (setq ibuffer-filter-groups (ff/ibuffer-filter-groups))
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

    (add-hook 'ibuffer-hook #'ff/ibuffer-hook)
    (define-key ibuffer-mode-map (kbd "M-D") 'ff/ibuffer-clean)
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
;;; M-x interface with Ido-style fuzzy matching

(use-package
  smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (progn
    (setq smex-save-file (expand-file-name "save-items" smex-directory))
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
 ; :config
 ; (progn
 ;  (setq mf-offset-x 55)
 ;  (setq mf-max-width 1380)))

;;; mic-paren

(use-package
  mic-paren
  :disabled t
  :ensure t
  :init (paren-activate))

;;; smartparens

(use-package
  smartparens-config
  :ensure smartparens
  :init (smartparens-global-mode t))

;;; rainbow-delimiters

(use-package
  rainbow-delimiters
  :ensure t
  :init (global-rainbow-delimiters-mode))

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
  :config
  (progn
    (ido-mode t)
    (ido-everywhere 1)

    (use-package
      ido-ubiquitous
      :ensure t
      :config (ido-ubiquitous-mode t))
    ;; try out flx-ido for better flex matching between words
    (use-package flx-ido
                 :ensure t
                 :config
                 (progn
                   (flx-ido-mode 1)
                   (setq ido-use-faces nil)))

    ;; flx-ido looks better with ido-vertical-mode
    (use-package ido-vertical-mode
                 :ensure t
                 :config (ido-vertical-mode))

    ;; ido at point
    (use-package ido-at-point
                 :ensure t
                 :config (ido-at-point-mode))

    (setq
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ;ido-use-filename-at-point 'guess
      ido-save-directory-list-file (expand-file-name "history" ido-directory)
      ido-max-prospects 10)))

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
    (setq ac-comphist-file (expand-file-name "ac-comphist.dat" auto-complete-directory))
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

;;; css-mode

(use-package
  css-mode
  :ensure t
  :mode ("\\.css\\'" . css-mode))

;;; js3-mode

(use-package
  js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq-default
    ;js2-bounce-indent-p t
    js2-include-browser-externs t
    ; js2-allow-keywords-as-property-names nil
    ; ;; disable error highliting
    ;js2-mode-show-parse-errors nil
    js2-strict-missing-semi-warning nil
    ; js2-highlight-external-variables nil
    js-indent-level 2
    js2-basic-offset 2))

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
    (defun ff/python-mode-hook ()
      (message "Execute ff/python-mode-hook.")

      (unless python-mode-initialized
        (setq python-mode-initialized t)))

    (add-hook 'python-mode-hook 'ff/python-mode-hook)))

;;; scala-mode

(use-package
  ensime
  :ensure t
  :load-path "ensime/src/main/elisp/"
  :pre-load (key-chord-define-global ".." 'ensime-expand-selection-command)
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :config
  (progn ()))

(use-package
  cal-china-x  ;; TODO
  :config
  (progn
    (setq mark-holidays-in-calendar t)
    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq calendar-holidays cal-china-x-important-holidays)))

;;; org

(use-package
  org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (setq org-agenda-files (list "~/Documents/org/gtd.org"))
    (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")))
    (setq org-tag-alist '(("blog" . ?b) ("daily" . ?d) ("errands" . ?e) ("family" . ?f) ("house" . ?h) ("ideas" . ?i) ("money" . ?n) ("monthly" . ?m)  ("repeating" . ?r) ("softwaredev" . ?s) ("someday" . ?o) ("uncategorized" . ?u) ("weekly" . ?w) ("yearly" . ?y)))
    (setq org-archive- (expand-file-name "~/Documents/org/gtd.org::* Archive"))

    (use-package org-fstree :ensure t)
    (use-package org-mac-link :ensure t)

    (setq org-log-done t
          org-completion-use-ido t
          org-edit-timestamp-down-means-later t
          org-agenda-start-on-weekday nil
;          org-startup-indented t
;          org-indent-mode t
          org-startup-folded 'content
          org-src-fontify-natively t
          org-link-file-path-type 'relative
          org-agenda-span 14
          org-agenda-include-diary t
          org-agenda-window-setup 'current-window
          org-fast-tag-selection-single-key 'expert
          org-export-kill-product-buffer-when-displayed t
          org-use-speed-commands t
          org-tags-column 80)


    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c g") 'org-mac-grab-link)
    (global-set-key (kbd "C-c r") 'org-remember)
    (global-set-key (kbd "C-c b") 'org-iswitchb)
    (global-set-key (kbd "C-c a") 'org-agenda)))

;;; fill-column-indicator

(use-package
  fill-column-indicator
  :ensure t
  :config
  (progn
    (define-globalized-minor-mode
      global-fci-mode
      fci-mode
      (lambda ()
        (fci-mode t)))
    (setq-default fci-rule-column 80)
    (setq-default fci-rule-width 2)
    (setq-default fci-dash-pattern 0.75)
    (setq-default fci-rule-use-dashes 1)
    (global-fci-mode t)))

;;; popwin

(use-package
  popwin
  :ensure t
  :config
  (progn
    (push '("helm" :regexp t) popwin:special-display-config)
    (push '("undo-tree" :width 0.4 :position right :regexp t) popwin:special-display-config)
    (push '("*Buffer List*") popwin:special-display-config)
    (push '("*Backtrace*" :noselect t) popwin:special-display-config)
    (push '("*helm imenu*" :width 0.3 :position right) popwin:special-display-config)
    (push '(" *auto-async-byte-compile*" :noselect t) popwin:special-display-config)
    (push '(" *command-log*" :width 0.3 :position right :noselect t) popwin:special-display-config)
    (popwin-mode t)))

;;; uniquify: unique buffer names

(use-package
  uniquify
  :init
  (setq
    uniquify-buffer-name-style 'post-forward
    uniquify-separator ":"
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*"))

;;;  saveplace: save location in file when saving files

(use-package
  saveplace
  :init
  (progn
    (setq save-place-file (expand-file-name "saveplace" emacs-cache-dir))
    (setq-default save-place t)))

;;; savehist: save some history

(use-package
  savehist
  :init
  (progn
    (setq savehist-additional-variables
          '(search ring regexp-search-ring)
          savehist-autosave-interval 60    ;; save every minute (default: 5 min)
          savehist-file (expand-file-name "savehist" emacs-cache-dir))    ;; keep my home clean
    (savehist-mode t)))

;; recentf

(use-package
  recentf
  :init
  (progn
    (setq
      recentf-save-file (expand-file-name "recentf" emacs-cache-dir)    ;; keep my home clean
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
    (recentf-mode t)))

(provide 'modes)
;;; modes.el ends here
