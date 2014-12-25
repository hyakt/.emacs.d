(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)

;; flycheck-pos-tip
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
