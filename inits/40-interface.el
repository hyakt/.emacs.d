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

(use-package multi-term
  :bind (("M-\\" . multi-term-dedicated-toggle))
  :config
  (add-to-list 'term-unbind-key-list "C-f")
  (add-to-list 'term-unbind-key-list "C-v"))

(use-package eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (bind-key "C-r" 'counsel-esh-history eshell-mode-map))))

(use-package which-key
  :config
  (which-key-mode))

(use-package docker
  :bind(( "C-x c" . docker))
  :ensure t)

(use-package magit
  :bind ("C-x g" . magit-status)
  (:map magit-status-mode-map ("q" . my/magit-quit-session))
  :config
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window)))

;;; 30-interface ends here
