;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;キーの設定;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'darwin)       ; もし、システムが Mac のとき
  (setq mac-pass-control-to-system t)) ; コントロールキーを Mac ではなく Emacs に渡す

(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(when (eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;emacs本体の設定;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;;HTMLのMETAタグコーディング無視
(setq auto-coding-functions nil)

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;;yes-noの選択肢をy-nにする
(fset 'yes-or-no-p 'y-or-n-p)

;;オートセーブファイルを作らない
(setq auto-save-default nil)

;; Backup fileの場所指定
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))

;;; スクロールを一行ずつにする
;; (setq scroll-step 1)
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;タブの代わりに半角スペースを使う
(setq-default tab-width 4 indent-tabs-mode nil)

;;;GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;ログの記録行数を増やす
(setq message-log-max 10000)

;;;クライアントを終了するとき終了するかどうかを聞かない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;自動再読み込み
(global-auto-revert-mode 1)

;;; symlinkは必ず追いかける
(setq vc-follow-symlinks t)

;;; 最後に改行無い時は挿入
(setq require-final-newline t)
(setq mode-require-final-newline t)

;;;サーバ起動
(require 'server)
(unless (server-running-p)
  (server-start))

;;; 文字コード
(set-language-environment "Japanese")
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (prefer-coding-system 'utf-8)
         (setq file-name-coding-system 'sjis)
         (setq locale-coding-system 'utf-8))
        ((eq ws 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))
        ((eq ws 'x)
         (prefer-coding-system 'utf-8)
         (setq file-name-coding-system 'utf-8)
         (setq locale-coding-system 'utf-8))
        )
  )

