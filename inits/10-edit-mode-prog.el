;;; 20-edit-mode-prog.el --- プログラム用メジャーモード設定
;;; Commentary:

;;; Code:

;; Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)))

;; javascript
(use-package js2-mode
  :mode (("\.js$" . js2-mode))
  :config
  (setq js2-strict-missing-semi-warning nil))

(use-package tern
  :if (eq system-type 'darwin)
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (tern-mode t)
  (add-to-list 'company-backends 'company-tern))

;; Python
(use-package python
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
  (use-package anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

;; Swift
(use-package swift-mode :config (add-to-list 'flycheck-checkers 'swift))

;; HTML
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))
