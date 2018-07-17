;;; 10-coding.el --- コーディングのサポート設定
;;; Commentary:

;;; Code:
(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/my-snippets"
          "~/.emacs.d/quelpa/build/yasnippet-snippets/snippets"))
  (yas-global-mode 1))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-pos-tip-mode))

(use-package flyspell
  :init
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

(use-package pcre2el :config (setq rxt-global-mode t))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)

  (load-library "migemo")
  (migemo-init))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package undohist :config (undohist-initialize))

(use-package smartparens-config
  :config
  (smartparens-global-mode t))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; 10-coding ends here
