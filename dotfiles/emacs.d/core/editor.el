; editor.el --- Enhanced editing experience.

;; anzu-mode enhances isearch by showing total matches and current match position
(require-package 'anzu)
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(provide 'editor)
;;; editor.el ends here
