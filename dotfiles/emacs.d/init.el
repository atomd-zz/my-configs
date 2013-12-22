;;; init.el main Emacs Settings File

(defvar emacs-home-dir (file-name-directory load-file-name))
(defvar emacs-core-dir (expand-file-name "core/" emacs-home-dir))
(add-to-list 'load-path emacs-home-dir)
(add-to-list 'load-path emacs-core-dir)

;; the core stuff
(require 'base)
(require 'packages)
(require 'modes)

(require 'ui)
(require 'editor)
(require 'kdbs)
(require 'fonts)
;;; init.el ends here
