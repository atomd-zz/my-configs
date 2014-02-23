;;; packages.el --- Default package selection.

(require 'cl)
(require 'package)

;(require 'melpa)
;(setq package-archive-exclude-alist '(("melpa" melpa yasnippet)))
(setq package-user-dir emacs-packages-dir)
(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

(provide 'packages)
;;; packages.el ends here
