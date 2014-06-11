(require 'multiple-cursors)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

;; リージョンと一致する箇所で現在行より下にあるもの1つを追加
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; リージョンと一致する箇所で現在行より上にあるもの1つを追加
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

