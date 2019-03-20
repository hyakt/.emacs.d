;;; 40-interface.el --- インターフェースについての設定
;;; Commentary:

;;; Code:
(use-package projectile)

(use-package dashboard
  :custom
  (dashboard-items '((hackernews . 10)
                     (recents  . 10)
                     (projects . 10)))
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook)
  (use-package dashboard-hackernews))

(use-package counsel)

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
  :custom
  (ivy-height 20)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((t . ivy--regex-plus) (read-file-name-internal . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1)
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../" ".DS_Store" ".tern-port")))
  (use-package all-the-icons-ivy :config (all-the-icons-ivy-setup))
  (use-package ivy-rich :config (ivy-rich-mode 1))
  (use-package ivy-hydra)
  (defun ivy-yank-action (x) (kill-new x))
  (defun ivy-copy-to-buffer-action (x) (with-ivy-window (insert x)))
  (ivy-set-actions t
                   '(("i" ivy-copy-to-buffer-action "insert")
                     ("y" ivy-yank-action "yank"))))

(use-package swiper :bind ((( "\C-s" . swiper))))

(use-package avy
  :bind (("C-;" . avy-goto-char))
  :config (avy-setup-default))

(use-package avy-migemo
  :config
  (avy-migemo-mode t)
  (require 'avy-migemo-e.g.ivy)
  (require 'avy-migemo-e.g.swiper))

(use-package eshell  :defer t
  :custom
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history 'always)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (bind-key "C-r" 'counsel-esh-history eshell-mode-map))))

(use-package shell-pop
  :bind (("M-t" . shell-pop))
  :config
  (custom-set-variables
   '(shell-pop-shell-type '("eshell" " *eshell*" (lambda () (eshell))))))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

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

(use-package twittering-mode
  :ensure-system-package ((gpg . "brew install gpg"))
  :custom
  (twittering-use-master-password t)
  (twittering-timer-interval 120)
  (twittering-convert-fix-size 24)
  (twittering-status-format
   "%FOLD{%RT{%FACE[bold]{RT}} %i%s %r %C{%m/%d %H:%M}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y/%m/%d %H:%M}} \n}")
  (epa-pinentry-mode 'loopback)
  :config
  (twittering-enable-unread-status-notifier))

(use-package ein :defer t
  :custom
  (ein:completion-backend 'ein:use-company-backend)
  :config
  (add-to-list 'company-backends #'user-company-ein-backend))
;;; 40-interface ends here
