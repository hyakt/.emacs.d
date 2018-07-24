;;; 20-mode-prog.el --- プログラム用メジャーモード設定
;;; Commentary:

;;; Code:

;; Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)))

;; HTML
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

;; javascript
(use-package js2-mode
  :mode (("\.js$" . js2-mode))
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-basic-offset 2)
  (use-package tern
    :after company
    :config
    (tern-mode t)
    (add-hook 'js2-mode-hook 'tern-mode)
    (add-to-list 'company-backends 'company-tern)))

(use-package web-beautify
  :defer t
  :init
  ;; (add-hook 'js2-mode-hook (lambda ()(add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'json-mode-hook (lambda ()(add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'css-mode (lambda ()(add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(use-package add-node-modules-path :config (add-hook 'js2-mode-hook #'add-node-modules-path))

;; Python
(use-package python :defer t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
  (use-package anaconda-mode
    :after company
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (add-to-list 'company-backends 'company-anaconda)))

;; Swift
(use-package swift-mode :defer t
  :after flycheck
  :config (add-to-list 'flycheck-checkers 'swift))

;; Ruby
(use-package enh-ruby-mode :defer t
  :mode (("\\.rb\\'" . enh-ruby-mode))
  :interpreter "pry"
  :config
  (use-package robe
    :after company
    :config
    (add-to-list 'company-backends 'company-tern))
  (yard-mode t))

(use-package inf-ruby :defer t
  :config
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.toplevel_binding")))

;;; 20-mode-prog ends here
