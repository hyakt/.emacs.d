(use-package recentf
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))
