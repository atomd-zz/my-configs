;;; fonts

(defconst
  my-ascii-font-candidates
  '("WenQuanYi Micro Hei Mono" "DejaVu Sans Mono" "Cousine" "Consolas"))

(defconst
  my-non-ascii-font-candidates
  '("WenQuanYi Micro Hei Mono" "Microsoft YaHei" "MS Gothic"))

(setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))

(defconst my-font-size 14)

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
        (create-fontset-from-fontset-spec font-spec)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font "fontset-myfontset" charset non-ascii-font))
        (add-to-list 'default-frame-alist '(font . "fontset-myfontset"))))))

(font-customize)

(provide 'fonts)

;;; fonts.el ends here
