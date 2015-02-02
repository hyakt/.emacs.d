(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/yasnippet-snippets"
          "~/.emacs.d/my-snippets"))

  ;; yas起動
  (yas-global-mode 1)
  )
