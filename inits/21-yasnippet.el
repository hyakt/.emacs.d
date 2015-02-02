(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/my-snippets"
          "~/.emacs.d/yasnippet-snippets"))

  ;; yas起動
  (yas-global-mode 1)
  )
