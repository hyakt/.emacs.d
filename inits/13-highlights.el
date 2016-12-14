(use-package smartparens-config
  :config
  (smartparens-global-mode t))

;; 括弧の色付け
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; 色の色付け
(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

;; 変数などの色付け
(use-package highlight-symbol
  :bind (("C-x C-l" . highlight-symbol-at-point)))
