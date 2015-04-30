(use-package langtool
  :bind (("C-x c w" . langtool-check)
         ("C-x c W" . langtool-check-done)
         ("C-x c l" . langtool-switch-default-language)
         ("C-x c 4" . langtool-show-message-at-point)
         ("C-x c c" . langtool-correct-buffer))
  :init
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.8/libexec/languagetool-commandline.jar")
  )
