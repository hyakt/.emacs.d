(use-package recentf
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))
