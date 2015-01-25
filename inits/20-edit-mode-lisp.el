(setq inferior-lisp-program "clisp")

(use-package slime-autoloads
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner))
)
