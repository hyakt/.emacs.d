(use-package google-translate
  :bind (("C-x l" . google-translate-at-point)
         ("C-x L" . google-translate-at-point-reverse))
  :init
  (custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja")))

(use-package google-this)
