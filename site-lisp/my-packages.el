;;; my-packages.el --- packageのリスト
;;; Commentary:

;;; Code:
(require 'quelpa)

;; misc
(quelpa 'use-package)
(quelpa 'use-package-ensure-system-package)
(quelpa 'bind-key)
(quelpa 'init-loader)
(quelpa 'exec-path-from-shell)
(quelpa 'ov)

;; 01-appearance
(quelpa 'rainbow-mode)
(quelpa 'all-the-icons)
(quelpa 'telephone-line)
(quelpa 'dashboard)
(quelpa 'volatile-highlights)
(quelpa 'highlight-defined)
(quelpa 'beacon)
(quelpa 'dimmer)
;;; theme
(quelpa 'panda-theme)

;; 10-coding
(quelpa 'yasnippet)
(quelpa 'yasnippet-snippets)
(quelpa 'company)
(quelpa 'company-flx)
(quelpa 'company-quickhelp)
(quelpa 'company-tern)
(quelpa 'company-anaconda)
(quelpa 'quickrun)
(quelpa 'dumb-jump)
(quelpa 'migemo)
(quelpa 'flycheck)
(quelpa 'flycheck-pos-tip)
(quelpa 'symbol-overlay)
(quelpa 'pcre2el)
(quelpa 'rainbow-delimiters)
(quelpa 'smartparens)
(quelpa 'git-gutter)

;; 20-edit-mode-prog
(quelpa 'swift-mode)
(quelpa 'haskell-mode)
(quelpa 'ansible)
(quelpa 'web-mode)
(quelpa 'js2-mode)
(quelpa 'web-beautify)
(quelpa 'tern)
(quelpa 'add-node-modules-path)
(quelpa 'yaml-mode)
(quelpa 'anaconda-mode)
(quelpa 'csharp-mode)
(quelpa 'robe)
(quelpa 'inf-ruby)
(quelpa 'stylus-mode)
(quelpa 'dockerfile-mode)
(quelpa 'docker-compose-mode)
(quelpa 'ssh-config-mode)
(quelpa 'gitconfig-mode)
(quelpa 'gitignore-mode)
(quelpa 'lispxmp)
(quelpa 'sqlup-mode)

;; 21-edit-mode-doc
(quelpa 'markdown-mode)
(quelpa 'ox-hugo)
(quelpa 'ox-gfm)
(quelpa 'org-bullets)
(quelpa 'ob-sql-mode)

;; 30-edit
(quelpa 'anzu)
(quelpa 'undo-tree)
(quelpa 'undohist)
(quelpa 'multiple-cursors)
(quelpa 'expand-region)
(quelpa 'wdired)
(quelpa 'wgrep)
(quelpa 'open-junk-file)
(quelpa 'codic)

;; 40-interface
(quelpa 'counsel)
(quelpa 'ivy)
(quelpa 'swiper)
(quelpa 'avy)
(quelpa 'avy-migemo)
(quelpa 'magit :stable t)
(quelpa 'which-key)
(quelpa 'git-timemachine)
(quelpa 'projectile)
(quelpa 'docker)
(quelpa 'shell-pop)
(quelpa 'fish-completion)
(quelpa 'esh-autosuggest)
(quelpa 'eshell-prompt-extras)
(quelpa 'atomic-chrome)
(quelpa 'ein :stable t)

;; 50-frame
(quelpa 'neotree)
(quelpa 'swap-buffers)
(quelpa '(other-window-or-split :fetcher github :repo "conao/other-window-or-split"))
(quelpa 'perspeen)
(quelpa 'popwin)
(quelpa 'yafolding)

;;; my-packages.el ends here