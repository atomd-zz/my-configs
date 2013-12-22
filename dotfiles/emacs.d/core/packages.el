;;; packages.el --- Default package selection.

(require 'cl)
(require 'package)

(setq el-get-dir emacs-packages-dir)
(add-to-list 'load-path (expand-file-name "el-get" emacs-packages-dir))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; el-get should not use elpa
(setq
  el-get-sources
  '(el-get
     (:name ibus)
     (:name melpa
            :type http
            :description "Allows creating a whitelist or blacklist of packages"
            :url "https://raw.github.com/milkypostman/melpa/master/melpa.el"
            :load "melpa.el")
     (:name douban-music-mode
            :type http
            :description "Douban FM client for emacs"
            :url "https://raw.github.com/zhengyuli/DoubanMusic/master/douban-music-mode.el"
            :before 
            (defvar douban-music-cache-directory
              (expand-file-name "douban-music" emacs-savefile-dir)))
     ))
(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

(require 'melpa)
(setq package-archive-exclude-alist '(("melpa" melpa yasnippet)))
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
