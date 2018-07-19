;;; 31-edit.el --- 編集機能についての設定
;;; Commentary:

;;; Code:
(use-package anzu
  :bind (("C-%" . anzu-query-replace)
         ("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace-at-cursor))
  :config
  (global-anzu-mode t)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("<C-M-return>" . mc/mark-all-dwim)))

(use-package undo-tree :ensure t
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

(use-package open-junk-file :bind (("M-`" . open-junk-file)))

;;; 31-edit ends here
