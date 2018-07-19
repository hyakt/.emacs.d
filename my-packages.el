;;; my-packages.el --- packageのリスト
;;; Commentary:

;;; Code:
(require 'quelpa)

;; misc
(quelpa 'use-package)
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
(quelpa 'nord-theme)

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
(quelpa 'stylus-mode)
(quelpa 'dockerfile-mode)
(quelpa 'docker-compose-mode)
(quelpa 'ssh-config-mode)
(quelpa 'gitconfig-mode)
(quelpa 'gitignore-mode)

;; 20-edit-mode-doc
(quelpa 'markdown-mode)
(quelpa 'ox-hugo)
(quelpa 'org-bullets)

;; 30-interface
(quelpa 'counsel)
(quelpa 'ivy)
(quelpa 'swiper)
(quelpa 'avy)
(quelpa 'avy-migemo)
(quelpa 'multi-term)
(quelpa 'magit)
(quelpa 'which-key)
(quelpa 'git-timemachine)
(quelpa 'projectile)
(quelpa 'docker)

;; 31-edit
(quelpa 'anzu)
(quelpa 'undo-tree)
(quelpa 'undohist)
(quelpa 'multiple-cursors)
(quelpa 'expand-region)
(quelpa 'wdired)
(quelpa 'wgrep)
(quelpa 'open-junk-file)

;; 40-view
(quelpa 'codic)
(quelpa 'neotree)
(quelpa 'swap-buffers)
(quelpa '(other-window-or-split :fetcher github :repo "conao/other-window-or-split"))
(quelpa 'perspeen)
(quelpa 'popwin)
(quelpa 'origami)

;;; my-packages.el ends here
