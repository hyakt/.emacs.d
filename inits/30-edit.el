;;; 30-edit.el --- 編集機能についての設定
;;; Commentary:

;;; Code:
(use-package migemo
  :ensure-system-package ((cmigemo . "brew install cmigemo"))
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :config
  (setq vr/engine 'pcre2el))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M-." . mc/mark-all-dwim)))

(use-package undo-tree
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t))

(use-package undohist :config (undohist-initialize))

(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package wdired
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("(" . dired-hide-details-mode)
              (")" . dired-hide-details-mode)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package tramp
  :if (eq system-type 'darwin)
  :config
  ;; http://qiita.com/l3msh0/items/6b84082541cbbf7d00f8
  (setenv "TMPDIR" "/tmp"))

(use-package open-junk-file
  :bind (("C-`" . open-junk-file))
  :config
  (setq open-junk-file-format "~/Documents/junk/%Y-%m-%d-%H%M%S."))

(use-package aggressive-inden
  :config (global-aggressive-indent-mode 1))

(use-package wgrep)

(use-package codic)

;;; 30-edit ends here
