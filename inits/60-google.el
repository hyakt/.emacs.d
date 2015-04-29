(use-package google-translate
  :bind (("C-x t" . google-translate-at-point))
  :init
  (custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja")))

(use-package google-this)
