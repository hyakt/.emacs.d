(use-package jdee
  :init (add-hook 'jdee-mode-hook
          '(lambda ()
             (define-key jdee-mode-map "\C-j" 'jdee-complete-minibuf)))
  :config
  (setq jdee-server-dir "~/.emacs.d/jdee-server/target/")
  (setq jdee-maven-init-hook nil))


