;;; fonts

(defconst
  my-ascii-font-candidates
  '("DejaVu Sans Mono" "Cousine" "Consolas" "WenQuanYi Micro Hei Mono"))

(defconst
  my-non-ascii-font-candidates
  '("文泉驿等宽微米黑" "WenQuanYi Micro Hei Mono" "Microsoft YaHei" "MS Gothic"))

(defconst my-font-size 15)

;;; http://netlab.cse.yzu.edu.tw/~statue/freebsd/zh-tut/xlfd.html
(defun font-existp (font-name)
  "Check if the font with FONT-NAME exist in current system"
  (null (null (x-list-fonts font-name))))

(defun font-customize ()
  (let ((ascii-font (find-if #'font-existp my-ascii-font-candidates))
        (non-ascii-font (find-if #'font-existp my-non-ascii-font-candidates)))
    (message "Select non-ascii font: %s" non-ascii-font)
    (message "Select ascii font: %s" ascii-font)
    (when (and ascii-font non-ascii-font)
      (let ((font-spec
              (concat
                "-*-" ascii-font
                "-*-*-*-*-" (number-to-string my-font-size)
                "-*-*-*-*-0-fontset-myfontset")))
        (message "Font Spec: %s" font-spec)
        (setq non-ascii-fontc (font-spec :family non-ascii-font :size 15))
        (create-fontset-from-fontset-spec font-spec)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font "fontset-myfontset" charset non-ascii-font))
        (add-to-list 'default-frame-alist '(font . "fontset-myfontset"))))))

(font-customize)

(provide 'fonts)

;;; fonts.el ends here
