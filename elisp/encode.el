;; https://gist.github.com/kosh04/568800#file-emacs-xyzzy-unicode-un-escape-region
;; ユニコードエスケープシーケンスを解除する
(defun my/unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

;; ユニコードエスケープシーケンスをする
(defun my/unicode-escape-region (&optional start end)
  "指定した範囲の文字をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (format "\\u%04x"
                             (char-unicode
                              (char (match-string 0) 0)))
                     nil t))))

(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

;; http://hins11.jugem.jp/?eid=34
(defun url-encode-string (str &optional sys)
  (let ((sys (or sys 'utf-8)))
    (url-hexify-string (encode-coding-string str sys))))

(defun url-decode-string (str &optional sys)
  (let ((sys (or sys 'utf-8)))
    (decode-coding-string (url-unhex-string str) sys)))

(defun my/url-decode-region (beg end)
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-decode-string str 'utf-8))))

(defun my/url-encode-region (beg end)
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-encode-string str 'utf-8))))

(provide 'encode)
