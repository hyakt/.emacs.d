;;; init.el --- 最初の設定
;;; Commentary:
;;; init-loaderにより、分割した設定ファイルが./initsより読み込まれる

;;; Code:
;;ロードパス追加設定
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; elpa設定
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; ローカル設定
(load "my-proxy" t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Quelpa
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(defun my/quelpa-setup ()
  "Setup Quelpa packages."
  (interactive)
  (load "my-packages"))

;; init-loader
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

;;; init.el ends here
