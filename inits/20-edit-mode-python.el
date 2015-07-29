(use-package python
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))
