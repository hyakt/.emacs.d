;; undohistの設定
(when(require 'undohist nil t)
  (undohist-initialize))

;; undo-treeモードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))
