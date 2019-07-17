;;; 30-edit.el --- 編集機能についての設定
;;; Commentary:

;;; Code:
(use-package visual-regexp
  :bind (("C-r" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :custom
  (vr/engine 'pcre2el))

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

(use-package point-history
  :straight (point-history :type git :host github :repo "blue0513/point-history")
  :config
  (point-history-mode t))

(use-package ivy-point-history
  :after (ivy)
  :bind (("M-p" . ivy-point-history))
  :straight (ivy-point-history :type git :host github :repo "SuzumiyaAoba/ivy-point-history"))

(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package recentf
  :custom
  (recentf-max-saved-items 500)
  (recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package tramp
  :if (eq system-type 'darwin)
  :config
  ;; http://qiita.com/l3msh0/items/6b84082541cbbf7d00f8
  (setenv "TMPDIR" "/tmp"))

(use-package docker-tramp)

(use-package open-junk-file
  :bind (("C-`" . open-junk-file))
  :custom
  (open-junk-file-format "~/Documents/junk/%Y-%m-%d-%H%M%S."))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package codic)

;;; 30-edit ends here
