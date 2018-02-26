;;; 10-coding.el --- コーディングのサポート設定
;;; Commentary:

;;; Code:
(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/my-snippets"
          "~/.emacs.d/yasnippet-snippets"))
  (yas-global-mode 1))

(use-package dumb-jump
  :config (dumb-jump-mode))

(use-package company
  :config
  (setq company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  ;;Addrevの時のdowncaseをやめる

  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'company-backends 'company-edbi)
  (add-to-list 'company-backends 'company-shell)
  (global-company-mode 1))

(use-package quickrun
  :bind (("C-x q" . quickrun)
         ("C-x a" . quickrun-with-arg)))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-pos-tip-mode))

(use-package flyspell
  :init
  (mapc                                   ;; flyspell-mode
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '(emacs-lisp-mode-hook
     python-mode-hook))

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package ispell
  :config
  (setq-default ispell-program-name "aspell")
  (eval-after-load "ispell"
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

;; 括弧の色付け
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; 変数などの色付け
(use-package symbol-overlay
  :bind ("M-i" . symbol-overlay-put)
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode))
