;;; 21-coding-support.el --- コーディングのサポート設定
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
