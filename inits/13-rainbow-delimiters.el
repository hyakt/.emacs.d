; rainbow-delimitersの設定

(use-package rainbow-delimiters
  :config
  (custom-set-faces '(rainbow-delimiters-depth-2-face ((t (:foreground "#E0FFB0")))))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )
