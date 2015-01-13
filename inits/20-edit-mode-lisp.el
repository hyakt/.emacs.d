(setq inferior-lisp-program "clisp")

(use-package slime-autoloads
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner))
  (require 'ac-slime)
  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
)
