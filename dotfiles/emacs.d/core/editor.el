; editor.el --- Enhanced editing experience.

;; highlight the current line
(global-hl-line-mode +1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'editor)
;;; editor.el ends here
