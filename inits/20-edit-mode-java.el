(use-package meghanada
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              ;; meghanada-mode on
              (meghanada-mode t)
              (add-hook 'before-save-hook 'delete-trailing-whitespace))))
