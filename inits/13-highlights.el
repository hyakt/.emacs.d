
(use-package smartparens-config
  :config
  (smartparens-global-mode t))

;; シンボルの色付け
;; (use-package rainbow-identifers
;;   :init
;;   (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;; 括弧の色付け
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
