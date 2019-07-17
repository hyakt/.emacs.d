
;;; 40-interface.el --- インターフェースについての設定
;;; Commentary:

;;; Code:
(use-package projectile)

(use-package dashboard
  :custom
  (dashboard-items '((recents  . 10)
                     (projects . 10)))
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

(use-package counsel)

(use-package ivy
  :ensure-system-package ((rg . "brew install ripgrep")
                          (fzf . "brew install fzf"))
  :bind(( "M-x" . counsel-M-x)
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
        ( "C-x f" . counsel-fzf)
        ( "C-x e" . counsel-rg)
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
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop-separator "\n-------\n")
  :config
  (ivy-mode 1)
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../" ".DS_Store" ".tern-port")))
  (defun ivy-yank-action (x) (kill-new x))
  (defun ivy-copy-to-buffer-action (x) (with-ivy-window (insert x)))
  (ivy-set-actions t
                   '(("i" ivy-copy-to-buffer-action "insert")
                     ("y" ivy-yank-action "yank"))))

(use-package counsel-ghq
  :straight (:host github :repo "windymelt/counsel-ghq" :branch "master")
  :bind(( "C-x C-j" . counsel-ghq)))

(use-package counsel-tramp
  :bind (( "C-x C-t" . counsel-tramp)))

(use-package ivy-rich
  :defines all-the-icons-mode-icon-alist
  :functions (all-the-icons-icon-family-for-mode all-the-icons-icon-family-for-file)
  :init (ivy-rich-mode 1)
  :preface
  (with-eval-after-load 'all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Show buffer icons in `ivy-rich'."
    (when-let* ((buffer (get-buffer candidate))
                (major-mode (buffer-local-value 'major-mode buffer))
                (icon (all-the-icons-icon-for-mode major-mode)))
      (propertize
       (if (symbolp icon)
           (all-the-icons-icon-for-mode 'text-mode)
         icon)
       'face `(
               :height 1.1
               :family ,(all-the-icons-icon-family-for-mode
                         (if (symbolp icon)
                             'text-mode
                           major-mode))
               :inherit
               ))))

  (defun ivy-rich-file-icon (candidate)
    "Show file icons in `ivy-rich'."
    (let ((icon (all-the-icons-icon-for-file candidate)))
      (propertize
       (if (symbolp icon)
           (all-the-icons-icon-for-mode 'text-mode)
         icon)
       'face `(
               :height 1.1
               :family ,(all-the-icons-icon-family-for-file candidate)
               :inherit
               ))))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon :width 2)
            (ivy-rich-candidate)
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))))

(use-package ivy-hydra
  :after ivy)

(use-package swiper
  :after ivy
  :bind ((( "\C-s" . swiper))))

(use-package avy
  :bind ((( "C-;" . avy-goto-char))))

(use-package wdired
  :after dired
  :bind (:map dired-mode-map (("r" . wdired-change-to-wdired-mode)
                              ("C-t" . nil))))

(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package all-the-icons-dired
  :after dired )

(use-package dired-sidebar
  :after dired
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar))
  :commands (qdired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom ((dired-sidebar-use-term-integration t)
           (dired-sidebar-use-custom-modeline nil)))

(defcustom dired-sidebar-mode-line-format
  '("%e" mode-line-front-space
    mode-line-buffer-identification
    " "  mode-line-end-spaces)
  "Mode line format for `dired-sidebar'."
  :type 'list
  :group 'dired-sidebar)

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

(use-package forge  :after magit)

(use-package git-timemachine)

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
