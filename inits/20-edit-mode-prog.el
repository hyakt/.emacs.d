;;; 20-edit-mode-prog.el --- プログラム用メジャーモード設定
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
  (setq js2-basic-offset 2))

(use-package web-beautify
  :defer t
  :init
  ;; (add-hook 'js2-mode-hook (lambda ()(add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'json-mode-hook (lambda ()(add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'css-mode (lambda ()(add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(use-package tern
  :if (eq system-type 'darwin)
  :defer t
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (tern-mode t)
  (add-to-list 'company-backends 'company-tern))

(use-package add-node-modules-path :config (add-hook 'js2-mode-hook #'add-node-modules-path))

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
