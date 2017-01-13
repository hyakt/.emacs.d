(use-package helm-config
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("M-y"     . helm-show-kill-ring)
   ("C-c i"   . helm-imenu)
   ("C-x b"   . helm-buffers-list)
   ("C-x C-b" . helm-buffers-list)
   ("C-c e"   . helm-do-ag)
   ("M-x"     . helm-M-x)
   ("C-o" . helm-swoop)
   ("C-z b" . helm-elscreen))
  :config
  (helm-mode 1)
  (helm-migemo-mode +1))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode 1))

(use-package helm-ag
  :config
  (custom-set-variables
   '(helm-ag-base-command "rg --vimgrep --no-heading")))
