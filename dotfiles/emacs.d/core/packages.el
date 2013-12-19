;;; packages.el --- Default package selection.

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

(defun install-from-url (name url)
  (when (and (not (boundp 'package-pinned-packages))
             (not (package-installed-p name)))
    (progn
      (switch-to-buffer (url-retrieve-synchronously url))
      (package-install-from-buffer (package-buffer-info) 'single))))

;;; auto-install the melpa package, since it's used to filter packages.
(install-from-url 'melpa "https://raw.github.com/milkypostman/melpa/master/melpa.el")
(setq package-archive-exclude-alist '(("melpa" melpa yasnippet)))

;;; install and require use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

(provide 'packages)

;;; packages.el ends here
