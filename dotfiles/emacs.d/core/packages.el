;; packages.el --- Default package selection.

(require 'cl)
(require 'package)


(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; set package-user-dir to be relative to install path
(setq package-user-dir (expand-file-name "elpa" emacs-home-dir))
(package-initialize)


(defvar my-packages
  '(use-package
    ace-jump-mode
    ack-and-a-half
    anzu
    dash
    diminish
    elisp-slime-nav
    smart-mode-line
    epl
    expand-region
    flx-ido
    flycheck
    gist
    gitconfig-mode
    gitignore-mode
    grizzl
    guru-mode
    projectile
    ido-ubiquitous
    magit
    move-text
    rainbow-mode
    smartparens
    smex
    undo-tree
    volatile-highlights
    zenburn-theme
    monokai-theme)
  "A list of packages to ensure are installed at launch.")

(defvar my-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ;;("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;; Auto Install package
(defun install-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
    t
    (if (or (assoc package package-archive-contents) no-refresh)
      (package-install package)
      (progn
        (package-refresh-contents)
        (install-package package min-version t)))))

(defun require-package(package &optional min-version no-refresh)
  "Install the PACKAGE and require it."
  (install-package package min-version t)
  (require package))

(defun install-packages (packages)
  "Check if all packages are installed."
  (mapc #'install-package packages))

(defun refresh-packages (packages)
  "Refreshing its package database"
  (unless (every #'package-installed-p packages)
    (package-refresh-contents)
    (install-packages my-packages)))

(defun my-list-foreign-packages ()
  "Browse third-party packages not bundled."
  (interactive)
  (package-show-package-list
    (set-difference package-activated-list my-packages)))

(defmacro my-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
  PACKAGE is installed only if not already present.The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))



;; build auto-install mappings
(mapc
  (lambda (entry)
    (let ((extension (car entry))
          (package (cadr entry))
          (mode (cadr (cdr entry))))
      (unless (package-installed-p package)
        (my-auto-install extension package mode))))
  my-auto-install-alist)

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

(refresh-packages my-packages)
(require 'use-package)
(provide 'packages)
;; packages.el ends here
