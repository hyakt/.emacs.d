;;; 30-interface.el --- インターフェースについての設定
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

(use-package company
  :config
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-shell)
  (global-company-mode 1))

(use-package quickrun
  :bind (("C-x q" . quickrun)
         ("C-x a" . quickrun-with-arg)))

(use-package dumb-jump
  :bind (("M-." . dumb-jump-quick-look)
         ("M-n" . dumb-jump-go)
         ("M-p" . dumb-jump-back))
  :config (dumb-jump-mode)
  (setq dumb-jump-default-project "")
  (setq dumb-jump-max-find-time 10)
  (setq dumb-jump-selector 'ivy))

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

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("<C-M-return>" . mc/mark-all-dwim)))

(use-package undo-tree
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t))

(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package wdired
  :config
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
  (define-key dired-mode-map (kbd ")") 'dired-hide-details-mode))

(use-package tramp
  :if (eq system-type 'darwin)
  :config
  ;; http://qiita.com/l3msh0/items/6b84082541cbbf7d00f8
  (setenv "TMPDIR" "/tmp"))

(use-package multi-term
  :bind (("M-\\" . multi-term-dedicated-toggle))
  :config
  (add-to-list 'term-unbind-key-list "C-f")
  (add-to-list 'term-unbind-key-list "C-v"))

(use-package open-junk-file :bind (("M-`" . open-junk-file)))

(use-package docker
  :bind(( "C-x c" . docker))
  :ensure t)

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session))

;;; 30-interface ends here
