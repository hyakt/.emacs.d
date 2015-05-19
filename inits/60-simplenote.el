(use-package simplenote2
  :bind (("C-`" . simplenote2-browse))
  :config
  (require 'netrc)
  (let* ((credentials (netrc-credentials "app.simplenote.com"))
         (login (nth 0 credentials))
         (password (nth 1 credentials)))
    (setq simplenote-email login)
    (setq simplenote-password password))
  (simplenote2-setup)
  (setq simplenote2-notes-mode 'org-mode))
