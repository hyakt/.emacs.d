;;; my-translate.el --- 翻訳系のeslip
;;; Commentary:
;;; テスト用のeslip

;;; Code:
(leaf my-translate
  :ensure (request)
  :require 'request
  :config
  (defcustom my/deepl-api-auth-key ""
    "DeepLの無料のauth key"
    :type 'string)

  (defun my/deepl-region (beg end)
    "BEGからENDの範囲をDeepLで翻訳する"
    (interactive "r")
    (let ((str (buffer-substring beg end)))
      (request
        "https://api-free.deepl.com/v2/translate"
        :type "GET"
        :data `(("auth_key" . ,(url-hexify-string my/deepl-api-auth-key)) ("text" . ,str) ("target_lang" . "JA"))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (let ((text (decode-coding-string (url-unhex-string (assoc-default 'text (aref (assoc-default 'translations data) 0))) 'utf-8)))
                      (kill-new text)
                      (message "%s" text))
                    )))))

  (defun my/trans-deepl (beg end)
    (interactive "r")
    (let ((str (buffer-substring beg end)))
      (browse-url
       (concat "https://www.deepl.com/translator#en/ja/" (url-hexify-string str))))))

(provide 'my-translate)

;;; my-translate.el ends here
