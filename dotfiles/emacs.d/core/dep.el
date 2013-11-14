;; dep.el --- Default package selection.

(require 'cl)
(require 'package)


(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; set package-user-dir to be relative to install path
(setq package-user-dir (expand-file-name "elpa" emacs-home-dir))
(package-initialize)


(defvar my-packages
  '(use-package
    ace-jump-mode
    ack-and-a-half
    anzu
    dash
    s
    helm
    helm-projectile
    flx-ido
    ido-ubiquitous
    key-chord
    diminish
    elisp-slime-nav
    smart-mode-line
    epl
    expand-region
    flycheck
    gist
    gitconfig-mode
    gitignore-mode
    grizzl
    guru-mode ;;
    projectile
    magit
    move-text
    rainbow-mode
    smartparens
    smex
    undo-tree ;;
    volatile-highlights
    monokai-theme ;;
    ;; programming lang
    js2-mode
    csv-mode
    cmake-mode
    log4j-mode
    css-mode
    python-mode
    feature-mode
    go-mode
    lua-mode
    puppet-mode
    pkgbuild-mode
    yaml-mode
    markdown-mode
    textile-mode
    auctex
    erlang
    scala-mode2)
  "A list of packages to ensure are installed at launch.")

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

(refresh-packages my-packages)
(require 'use-package)

(provide 'dep)
;; dep.el ends here
