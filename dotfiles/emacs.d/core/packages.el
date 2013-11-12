;;; packages.el --- Default package selection.
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

;; Auto Install package
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
    t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t))))
 ;; (require package)
)

(defun require-packages (packages)
  "Ensure PACKAGES are installed.
  Missing packages are installed automatically."
  (mapc #'require-package packages))

(defvar base-packages
  '(dash
    diminish
;;    ace-jump-mode
;;    ack-and-a-half
;;    elisp-slime-nav
;;    epl
;;    expand-region
;;    flx-ido
;;    flycheck
;;    gist
;;    gitconfig-mode
;;    gitignore-mode
;;    grizzl
;;    guru-mode
;;    projectile
;;    ido-ubiquitous
;;    magit
;;    move-text
;;    rainbow-mode
;;    smartparens
;;    smex
;;    undo-tree
;;    volatile-highlights
    monokai-theme)
  "A list of packages to ensure are installed at launch.")

(require-packages base-packages)

(provide 'packages)
;; packages.el ends here
