;;; init.el --- 最初の設定
;;; Commentary:
;;; init-loaderにより、分割した設定ファイルが./initsより読み込まれる

;;; Code:
;; ロードパス追加
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; ローカル設定
(load "my-proxy" t)
(setq custom-file (locate-user-emacs-file "custom.el"))

;; パッケージャ
; elpa
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package use-package-ensure-system-package)

;; init-loader
(use-package init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

;;; init.el ends here
