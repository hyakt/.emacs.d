;;; 40-interface.el --- インターフェースについての設定
;;; Commentary:

;;; Code:
(use-package ivy
  :ensure-system-package ((rg . "brew install ripgrep")
                          (fzf . "brew install fzf"))
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
        ( "C-c e" . counsel-rg)
        :map read-expression-map
        ("C-r" . counsel-expression-history))
  :config
  (ivy-mode 1)
  (setq ivy-height 20)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../"))))

(use-package swiper :bind ((( "\C-s" . swiper))))

(use-package avy
  :bind (("C-;" . avy-goto-char))
  :config (avy-setup-default))

(use-package avy-migemo
  :config (avy-migemo-mode 1)
  (use-package avy-migemo-e.g.ivy)
  (use-package avy-migemo-e.g.swiper)
  (use-package avy-migemo-e.g.counsel))

(use-package eshell
  :defer t
  :config
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history 'always)
  (add-hook 'eshell-mode-hook
            (lambda () (bind-key "C-r" 'counsel-esh-history eshell-mode-map))))

(use-package shell-pop
  :bind (("M-t" . shell-pop))
  :config
  (custom-set-variables
   '(shell-pop-shell-type '("eshell" " *eshell*" (lambda () (eshell))))))

(use-package eshell-prompt-extras
  :after eshell
  :config
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package fish-completion
  :if (executable-find "fish")
  :config (global-fish-completion-mode))
  
(use-package esh-autosuggest :hook (eshell-mode . esh-autosuggest-mode))

(use-package which-key :config (which-key-mode))

(use-package docker
  :bind(( "C-x c" . docker)))

(use-package magit
  :ensure-system-package ((git . "brew install git"))
  :bind ("C-x g" . magit-status)
  (:map magit-status-mode-map ("q" . my/magit-quit-session))
  :config
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window)))

(use-package atomic-chrome :defer t
  :init (atomic-chrome-start-server))

(use-package ein :defer t
  :config
  (setq ein:completion-backend 'ein:use-company-backend)
  (add-to-list 'company-backends #'user-company-ein-backend))

;;; 40-interface ends here
