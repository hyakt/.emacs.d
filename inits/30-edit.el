;;; 30-edit.el --- 編集機能の設定
;;; Commentary:

;;; Code:
(use-package ivy
  :bind(( "C-c C-r" . ivy-resume)
        ( "M-x" . counsel-M-x)
        ( "M-y" . counsel-yank-pop)
        ( "C-x C-f" . counsel-find-file)
        ( "C-x C-r" . counsel-recentf)
        ( "C-x C-b" . switch-to-buffer)
        ( "<f1> f" . counsel-describe-function)
        ( "<f1> v" . counsel-describe-variable)
        ( "<f1> l" . counsel-find-library)
        ( "<f2> i" . counsel-info-lookup-symbol)
        ( "<f2> u" . counsel-unicode-char)
        ( "C-x C-g" . counsel-git)
        ( "C-x C-d" . counsel-fzf)
        ( "C-c j" . counsel-git-grep)
        ( "C-c e" . counsel-rg))
  :config
  (ivy-mode 1)
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper :bind ((( "\C-s" . swiper))))

(use-package avy
  :bind (("C-:" . avy-goto-char))
  :config (avy-setup-default))

(use-package avy-migemo
  :config (avy-migemo-mode 1)
  (use-package avy-migemo-e.g.ivy)
  (use-package avy-migemo-e.g.swiper)
  (use-package avy-migemo-e.g.counsel))

(use-package anzu
  :bind (("C-%" . anzu-query-replace)
         ("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace-at-cursor))
  :config
  (global-anzu-mode t)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold))

(use-package pcre2el :config (setq rxt-global-mode t))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("<C-M-return>" . mc/mark-all-dwim)))

(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)

  (load-library "migemo")
  (migemo-init))

(use-package wdired
  :config
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd ")") 'dired-hide-details-mode))

(use-package projectile
  :config
  (projectile-global-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package which-key
  :config
  (which-key-mode))

;; undo-treeモードの設定
(use-package undo-tree
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t))

(use-package tramp
  :if (eq system-type 'darwin)
  :config
  ;; http://qiita.com/l3msh0/items/6b84082541cbbf7d00f8
  (setenv "TMPDIR" "/tmp"))

(use-package smartparens-config
  :config
  (smartparens-global-mode t))

(use-package multi-term
  :bind (("M-\\" . multi-term-dedicated-toggle))
  :config
  (add-to-list 'term-unbind-key-list "C-f")
  (add-to-list 'term-unbind-key-list "C-v"))

(use-package open-junk-file :bind (("M-`" . open-junk-file)))
