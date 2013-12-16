; editor.el --- Enhanced editing experience.

;; highlight the current line
(global-hl-line-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'editor)
;;; editor.el ends here
