;;; init.el main Emacs Settings File

(defvar emacs-home-dir (file-name-directory load-file-name))
(defvar emacs-core-dir (expand-file-name "core/" emacs-home-dir))
(add-to-list 'load-path emacs-core-dir)

(setq custom-file (expand-file-name "custom.el" emacs-home-dir))
(load custom-file)

;; the core stuff
(require 'base)
(require 'packages)
(require 'modes)

(require 'editor)
(require 'fonts)
(require 'kdbs)
(require 'ui)
;;; init.el ends here
