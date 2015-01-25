(use-package js2-mode
  :mode (("\.js$" . js2-mode))
  :config
  (setq js2-strict-missing-semi-warning nil)
  )

(use-package tern
  :if (eq system-type 'darwin)
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (tern-mode t)
  (add-to-list 'company-backends 'company-tern)
  )
