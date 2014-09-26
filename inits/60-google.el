(require 'google-translate)
(require 'google-this)

(custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja"))

(setq google-this-location-suffix "co.jp")
(defun google-this-url () "URL for google searches."
  ;; 100件/日本語ページ/5年以内ならこのように設定する
  (concat google-this-base-url google-this-location-suffix
          "/search?q=%s&hl=ja&num=100&as_qdr=y5&lr=lang_ja"))
