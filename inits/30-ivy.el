;;; 30-ivy.el --- a generic completion frontend for Emacs
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
        ( "C-c g" . counsel-git)
        ( "C-c j" . counsel-git-grep)
        ( "C-c e" . counsel-rg))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package swiper :bind ((( "\C-s" . swiper))))

