(use-package flyspell
  :init
  (mapc                                   ;; flyspell-mode
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '(
     emacs-lisp-mode-hook
     python-mode-hook
     ))

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
