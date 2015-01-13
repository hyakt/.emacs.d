(use-package anzu
  :config
  (global-anzu-mode t)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)

  (global-set-key (kbd "C-%") 'anzu-query-replace)
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-at-cursor)
  )
