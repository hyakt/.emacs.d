;; undo-treeモードの設定
(use-package undo-tree
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t))
