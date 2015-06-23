(use-package grammar
  :init
  (setq grammar-program-name "~/.emacs.d/grammar")
  (add-hook 'org-mode-hook 'grammar-mode)
  (add-hook 'TeX-mode-hook 'grammar-mode)
  :config
   (set-face-attribute 'grammar-error-face nil
                       :foreground "#FFFFFF"
                       :background "#F36c60"))
