(use-package anzu
  :bind (("C-%" . anzu-query-replace)
         ("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace-at-cursor))

  :config
  (global-anzu-mode t)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  )
