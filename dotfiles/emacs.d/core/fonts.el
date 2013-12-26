;;; fonts.el

;; (set-default-font "Ubuntu Mono")
;; (dolist (charset '(kana han symbol cjk-misc gb18030 bopomofo))
;;   (set-fontset-font "fontset-default"
;;                     charset
;;                     '("WenQuanYi Zen Hei" . "unicode-bmp")))

(defun font-exists-p (font)
  (if (null (x-list-fonts font))
    nil t))

(defun make-font-spec-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
    (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun set-cjk-font (english-fonts
                      english-font-size
                      chinese-fonts
                      &optional chinese-font-size)
  ; English-font-size could be set to \":pixelsize=10\" or a integer.
  ; If set/leave chinese-font-size to nil, it will follow english-font-size
  (require 'cl) ;; for find if
  (let ((en-font (make-font-spec-string
                   (find-if #'font-exists-p english-fonts)
                   english-font-size))
        (zh-font (font-spec :family (find-if #'font-exists-p chinese-fonts)
                            :size chinese-font-size)))

    ;; Set the default English font
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=13")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=13"))
    ;; We have to use set-face-attribute
    (set-face-attribute
      'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (dolist (charset '(kana han symbol cjk-misc gb18030 bopomofo))
      ;; (set-fontset-font (frame-parameter nil 'font)
      (set-fontset-font "fontset-default"
                        charset
                        zh-font))))

(defun ff/set-cjk-font ()
  "set the fonts for cjk"
  (set-cjk-font
    '("Ubuntu Mono") ":pixelsize=16"
    '("WenQuanYi Zen Hei Mono")))

(defun ff/set-frame-font(&optional frame)
  "set the frame fonts"
  (when (eq 'x (window-system frame))
    (select-frame frame)
    (ff/set-cjk-font)))

(add-hook 'after-make-frame-functions 'ff/set-frame-font)
(if (display-graphic-p)
  (ff/set-cjk-font))

(provide 'fonts)
;;; fonts.el ends here
