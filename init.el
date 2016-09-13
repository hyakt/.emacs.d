;;ロードパス追加設定
(add-to-list 'load-path "~/.emacs.d/elisp")

;;cask設定
(require 'cask)
(cask-initialize)

;;elpa設定
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;init-loaderの設定
;;設定ファイルはinits下で管理
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")
