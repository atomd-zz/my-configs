;;==============================================================================
;;; init.el
;;; Main Emacs Settings File
;;==============================================================================

(when (version< emacs-version "24.1")
  (error "Requires at least GNU Emacs 24.1"))

;; set default user name
(setq user-name "atomd")

;; cd to home
(cd "~")

(defvar emacs-home-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar emacs-core-dir (expand-file-name "core" emacs-home-dir)
  "The home of Prelude's core functionality.")

(message ";;; init --> General Settings" emacs-core-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path emacs-home-dir)
(add-to-list 'load-path emacs-core-dir)


;; the core stuff
(require 'packages)
(require 'base)
(require 'ui)
(require 'mode)
(require 'editor)
(require 'keybindings)
