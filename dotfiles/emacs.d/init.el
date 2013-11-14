;;==============================================================================
;;; init.el
;;; Main Emacs Settings File
;;==============================================================================

;; make directories
(defvar emacs-home-dir (file-name-directory load-file-name)
  "The root dir of the Emacs distribution.")
(defvar emacs-core-dir (expand-file-name "core" emacs-home-dir)
  "The home of the core functionality.")
(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-home-dir)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path emacs-home-dir)
(add-to-list 'load-path emacs-core-dir)


;; the core stuff
(require 'dep)
(require 'packages)

(require 'base)
(require 'ui)
(require 'editor)
(require 'keybindings)
