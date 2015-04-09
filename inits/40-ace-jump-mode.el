(use-package ace-jump-mode
  :bind
  (("C-;" . ace-jump-word-mode)
   ("C-:" . ace-jump-line-mode))

  :config
  ;; ヒント文字に使う文字を指定する
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
  )
