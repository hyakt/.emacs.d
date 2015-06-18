(use-package elscreen
  :config
  (elscreen-start)
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-display-tab nil)
  ;(setq elscreen-tab-display-kill-screen nil)
  ;(setq elscreen-tab-display-control nil)

  (use-package elscreen-persist
    :config
    (elscreen-persist-mode 1))
  (use-package elscreen-separate-buffer-list
    :config
    (elscreen-separate-buffer-list-mode 1)))
