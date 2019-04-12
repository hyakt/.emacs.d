;;; 10-coding.el --- コーディングのサポート設定
;;; Commentary:

;;; Code:
(use-package yasnippet
  :custom
  (yas-snippet-dirs
   '("~/.emacs.d/site-lisp/my-snippets"
     "~/.emacs.d/quelpa/build/yasnippet-snippets/snippets"))
  :config
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
  :custom
  (company-transformers '(company-sort-by-occurrence))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  :config
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
  :custom
  (company-quickhelp-delay .1)
  (pos-tip-use-relative-coordinates t))

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
  :custom
  (dumb-jump-default-project "")
  (dumb-jump-max-find-time 10)
  (dumb-jump-selector 'ivy)
  :config
  (dumb-jump-mode))

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

(use-package pcre2el
  :custom
  (rxt-global-mode t))

(use-package smartparens
  :init (smartparens-global-mode t)
  :bind (("C-M-n" . sp-forward-sexp)
         ("C-M-p" . sp-backward-sexp))
  :config
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "~" "~")
  (sp-local-pair 'org-mode "「" "」"))

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

(use-package google-translate
  :bind(("C-." . google-translate-at-point))
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(use-package google-this
  :config (google-this-mode 1))

;;; 10-coding ends here
