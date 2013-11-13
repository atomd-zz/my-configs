;;==============================================================================
;;; init.el
;;; Main Emacs Settings File
;;==============================================================================

;; set default user name
(setq user-name "atomd")
;; cd to home
(cd "~")

;; make dir
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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)


;; the core stuff
(require 'packages)
(require 'ui)
(require 'mode)
(require 'editor)
(require 'keybindings)
