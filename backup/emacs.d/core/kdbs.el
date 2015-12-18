;;; kbds.el --- Some useful keybindings.

(global-set-key (kbd "M-g") 'goto-line)
;(global-set-key (kbd "M-m") 'set-mark-command)
;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; newline-withoug-break-of-line
(defun newline-without-break-of-line ()
  "1. remove to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

(provide 'kdbs)
;;; kbds.el ends here
