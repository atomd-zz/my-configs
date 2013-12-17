;;; dep.el --- Default package selection.

(require 'cl)
(require 'package)

;;; set package-user-dir to be relative to install path
(setq package-user-dir (expand-file-name "elpa" emacs-home-dir))
(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;;; auto-install the melpa package, since it's used to filter packages.
(when (and (not (boundp 'package-pinned-packages))
           (not (package-installed-p 'melpa)))
  (progn
    (switch-to-buffer
      (url-retrieve-synchronously
        "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
    (package-install-from-buffer (package-buffer-info) 'single)))

;;; blacklist some non-melpa packages
(setq package-archive-exclude-alist '(("melpa" melpa yasnippet)))

;;; install and require use-package
(unless (package-installed-p 'use-package)
   (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

(provide 'dep)
;;; dep.el ends here
