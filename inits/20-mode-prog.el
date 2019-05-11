;;; 20-mode-prog.el --- プログラム用メジャーモード設定
;;; Commentary:

;;; Code:

;; lsp-mode
(use-package lsp-mode
  :commands lsp
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil)
           (lsp-prefer-flymake nil))
  :hook   ((prog-major-mode . lsp-prog-major-mode-enable)
           (dart-mode . lsp )))

(use-package lsp-ui
  :after lsp-mode
  :custom ((scroll-margin 0)
           (lsp-ui-doc-enable nil)
           (lsp-ui-peek-enable nil)
           (lsp-ui-sideline-enable nil)
           (lsp-ui-imenu-enable nil)
           (lsp-ui-flycheck-enable t))
  :hook   (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode company yasnippet)
  :defines company-backends
  :init (push 'company-lsp company-backends))

;; emacs-lisp
(use-package lispxmp :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . lispxmp )))

;; Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)))

;; HTML
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :custom
  (web-mode-engines-alist
   '(("php"    . "\\.phtml\\'")
     ("blade"  . "\\.blade\\.")))
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 2)
  (web-mode-indent-style 4)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;; javascript
(use-package js2-mode :defer t
  :mode (("\.js$" . js2-mode))
  :custom
  ((js2-basic-offset 2)
   (js-switch-indent-offset 2)
   (js2-strict-missing-semi-warning nil)))

;; jsx (react)
(use-package rjsx-mode
  :custom ((indent-tabs-mode nil)
           (js-indent-level 2))
  :mode (("components\\/.*\\.js\\'" . rjsx-mode)
         ("containers\\/.*\\.js\\'" . rjsx-mode)
         ("screens\\/.*\\.js\\'" . rjsx-mode)
         ("navigation\\/.*\\.js\\'" . rjsx-mode)))

(use-package tern :defer t
  :after company
  :ensure-system-package ((tern . "npm install -g tern"))
  :hook ((js2-mode . tern-mode)
         (rjsx-mode . tern-mode))
  :config
  (use-package company-tern)
  (add-to-list 'company-backends 'company-tern))

(use-package web-beautify)

(use-package add-node-modules-path :config (add-hook 'js2-mode-hook #'add-node-modules-path))

;; Dart
(use-package dart-mode
  :custom
  (dart-format-on-save nil)
  (dart-enable-analysis-server nil)
  (dart-sdk-path "~/git/github.com/flutter/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "~/git/github.com/flutter/flutter/"))

;; Python
(use-package python :defer t
  :custom((indent-tabs-mode nil)
          (tab-width 4)))

(use-package anaconda-mode
  :after company
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-to-list 'company-backends 'company-anaconda))

;; Swift
(use-package swift-mode :defer t
  :after flycheck
  :config
  (add-to-list 'flycheck-checkers 'swift)
  (use-package company-sourcekit)
  (add-to-list 'company-backends 'company-sourcekit))

;; Ruby
(use-package ruby-mode :defer t
  :mode (("\\.rb\\'" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("[Rr]akefile$" . ruby-mode))
  :interpreter "pry"
  :config (require 'smartparens-ruby))

(use-package inf-ruby
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-l" . ruby-send-line))
  :init
  (defalias 'pry 'inf-ruby)
  :custom
  (inf-ruby-default-implementation "pry")
  (inf-ruby-eval-binding "Pry.toplevel_binding"))

(use-package robe :defer t
  :after company
  :hook ((ruby-mode . robe-mode)
         (inf-ruby-mode . robe-mode))
  :config
  (add-to-list 'company-backends 'company-robe))

(use-package php-mode)

;; SQL
(use-package sql
  :mode (("\.sql$" . sql-mode))
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (buffer-face-set 'variable-pitch)
              (toggle-truncate-lines t))))

(use-package sqlup-mode
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package sqlformat
  :ensure-system-package ((sqlformat . "brew install sqlparse")))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (message "hook")
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (setq c-basic-offset 4)))

;; CSS
(use-package sws-mode)

;; Docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Git
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; Nginx
(use-package nginx-mode)

;; go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (use-package company-go))

;; elixir
(use-package elixir-mode
  :config
  (use-package alchemist)
  (use-package flycheck-elixir))

;; R
(use-package ess)

;; Scala
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command)

(use-package ensime
  :custom ((ensime-startup-notification nil)))

;; fish
(use-package fish-mode)

;;; 20-mode-prog ends here
