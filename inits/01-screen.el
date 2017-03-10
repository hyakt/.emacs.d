;;; 01-screen.el --- screenの設定
;;; Commentary:
;;; https://github.com/seudut/perspeen

;;; Code:
(use-package perspeen
  :config
  (perspeen-mode +1)
  (global-unset-key (kbd "C-z"))
  (setq perspeen-keymap-prefix (kbd "C-z")))
