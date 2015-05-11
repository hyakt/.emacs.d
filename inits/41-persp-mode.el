(use-package persp-mode
  :init
  (global-unset-key "\C-z")
  (setq persp-keymap-prefix (kbd "C-z")) ;prefix
  (setq wg-morph-on nil)
  :config
  (define-key persp-key-map (kbd "n") #'persp-add-new)
  (setq persp-add-on-switch-or-display t) ;バッファを切り替えたら見えるようにする
  (persp-mode 1))
