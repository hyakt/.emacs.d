;;; 20-mode-prog.el --- プログラム用メジャーモード設定
;;; Commentary:

;;; Code:

;; emacs-lisp
(use-package xmplisp :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . lispxmp )))

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
(use-package js2-mode :defer t
  :mode (("\.js$" . js2-mode))
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-basic-offset 2))

(use-package tern :defer t
  :after company
  :ensure-system-package ((tern . "npm install -g tern"))
  :hook ((js2-mode . tern-mode))
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package web-beautify :defer t
  :ensure-system-package ((js-beautify . "npm install -g js-beautify"))
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
              (setq tab-width 4))))

(use-package anaconda-mode
  :after company
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-to-list 'company-backends 'company-anaconda))

;; Swift
(use-package swift-mode :defer t
  :after flycheck
  :config (add-to-list 'flycheck-checkers 'swift))

;; Ruby
(use-package ruby-mode :defer t
  :mode (("\\.rb\\'" . ruby-mode)
         ("Capfile$" . ruby-mode)
	     ("Gemfile$" . ruby-mode)
	     ("[Rr]akefile$" . ruby-mode))
  :interpreter "pry"
  :config
  (use-package smartparens-ruby))

(use-package inf-ruby :defer t
  :ensure-system-package ((pry . "gem install pry; gem install pry-doc;"))
  :init (defalias 'irb 'inf-ruby))
  :config
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.toplevel_binding"))

(use-package robe :defer t
    :after company
    :hook ((ruby-mode . robe-mode)
           (inf-ruby-mode . robe-mode))
    :config
    (add-to-list 'company-backends 'company-robe))

;;; 20-mode-prog ends here
