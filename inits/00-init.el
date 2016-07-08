;emacsの基本設定

(require 'use-package)

(when (eq system-type 'darwin)

  ;; 文字コードの設定
  (require 'ucs-normalize)
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  ;----インラインパッチ時の設定---
  ;; (setq mac-pass-control-to-system )

  ;; (setq ns-command-modifier (quote meta))
  ;; (setq ns-alternate-modifier (quote super))  ;; (setq default-input-method "MacOSX")

  ;; emacs 起動時は英数モードから始める
  ;; (add-hook 'after-init-hook 'mac-change-language-to-us)
  ;; minibuffer 内は英数モードにする
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  ;; [migemo]isearch のとき IME を英数モードにする
  ;; (add-hook 'isearch-mode-hook 'mac-change-language-to-us)
  ;----------------------------
)
(when (eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'meta)

  ;; 文字コードの設定
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  )

;; HTMLのMETAタグコーディング無視
(setq auto-coding-functions nil)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; yes-noの選択肢をy-nにする
(fset 'yes-or-no-p 'y-or-n-p)

;; オートセーブのファイルを作らない
(setq auto-save-default nil)

;; Backup fileの場所指定
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))

;; スクロールの設定
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; タブの変わりに半角スペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ログの記録行数を増やす
(setq message-log-max 10000)

;; ファイルの自動再読み込み
(global-auto-revert-mode 1)

;; symlinkは必ず追いかける
(setq vc-follow-symlinks t)

;; サーバ起動
(require 'server)
(unless (server-running-p)
  (server-start))

;; 初期のモードをorg-modeに
(setq initial-major-mode 'org-mode)
