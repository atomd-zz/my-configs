;;; editor.el --- Enhanced editing experience.

;; highlight matching parentheses when the point is on them.
(show-paren-mode t)

(transient-mark-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'editor)
;;; editor.el ends here
