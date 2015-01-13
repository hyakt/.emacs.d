; flycheckの設定

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
)
