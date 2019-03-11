;;; 10-coding.el --- コーディングのサポート設定
;;; Commentary:

;;; Code:
(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/site-lisp/my-snippets"
          "~/.emacs.d/quelpa/build/yasnippet-snippets/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package company
  :defer t
  :init (global-company-mode)
  :bind (("C-j" . company-complete)
         :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer))
  :config
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet)))))

(use-package company-flx
  :hook (company-mode . company-flx-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay .1)
  (setq pos-tip-use-relative-coordinates t))

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (use-package flycheck-pos-tip :config (flycheck-pos-tip-mode)))

(use-package flyspell
  :ensure-system-package ((aspell . "brew install aspell"))
  :hook (LaTeX-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-," . nil))
  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package quickrun
  :bind (("C-x q" . quickrun)
         ("C-x a" . quickrun-with-arg)))

(use-package dumb-jump
  :bind (("M-." . dumb-jump-quick-look)
         ("M-n" . dumb-jump-go)
         ("M-p" . dumb-jump-back))
  :config
  (dumb-jump-mode)
  (setq dumb-jump-default-project "")
  (setq dumb-jump-max-find-time 10)
  (setq dumb-jump-selector 'ivy))

;; 括弧の色付け
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; カラーコードの表示
(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; 変数などの色付け
(use-package symbol-overlay
  :bind ("M-i" . symbol-overlay-put)
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode))

(use-package pcre2el :config (setq rxt-global-mode t))

(use-package smartparens
  :init (smartparens-global-mode t)
  :bind (("C-M-n" . sp-forward-sexp)
         ("C-M-p" . sp-backward-sexp))
  :config
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "~" "~"))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign    " ")
  (git-gutter:deleted-sign  " ")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#A6E22E"))))
  (git-gutter:deleted  ((t (:background "#D2527F"))))
  :config
  (global-git-gutter-mode +1))

(use-package yafolding
  :init (add-hook 'prog-mode-hook
                  (lambda () (yafolding-mode))))

;;; 10-coding ends here
