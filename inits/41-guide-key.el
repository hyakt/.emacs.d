(use-package guide-key
  :config
;;; guilde-keyを発動させるプレフィクスキー
  (setq guide-key/guide-key-sequence
        '("C-x" "C-c" "C-x 4"                 ;global
          ;; org-modeではC-c C-xも対象にする
          (org-mode "C-c C-x")))

;;; コマンド名にこれらが含まれている場合はハイライトされる
  (setq guide-key/highlight-command-regexp "rectangle\\|register\\|org-clock")
;;; 1秒後にポップアップされる(デフォルト)
;;; つまり1秒以内に操作すればポップアップされずに実行される
  (setq guide-key/idle-delay 1.0)
;;; 下部にキー一覧を表示させる(デフォルトはright)
  (setq guide-key/popup-window-position 'bottom)
;;; 文字の大きさを変更する(正の数で大きく、負の数で小さく)
  (setq guide-key/text-scale-amount -1)
;;; 有効にする
  (guide-key-mode 1))
