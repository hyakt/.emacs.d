; ディスプレイの設定

;; themeを設定
(load-theme 'zerodark t)

;; 対応する括弧を光らせるy
(show-paren-mode t)

;; 選択部分のハイライト
(transient-mark-mode t)

;; 行間
(setq-default line-spacing 0)

;; 同じバッファ名の時 <2> とかではなく、ディレクトリ名で区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; フォントロックモード
(global-font-lock-mode t)

;; windowの設定
(setq default-frame-alist
      (append (list
               '(width . 87)
               '(height . 50)
               '(top . 0)
               '(left . 0)
               '(alpha . (99 99)))
              default-frame-alist))

;; tool-bar使わない
(tool-bar-mode 0)

;; scroll-barを使わない
(set-scroll-bar-mode 'nil)

;; 画面端まで来たら折り返す
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; init-loaderが失敗した時のみエラーメッセージを表示
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))

;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;; 行番号・桁番号を表示
(line-number-mode 1)
(column-number-mode 1)

;; 編集行を目立たせる（現在行をハイライト表示する）
;; (defface hlline-face
;;   '((((class color)
;;       (background dark))
;;      (:background "#00070D"))
;;     (((class color)
;;       (background light))
;;      (:background "#F1FCE3"))
;;     (t
;;      ()))
;;   "*Face used by hl-line.")
;; (setq hl-line-face 'hlline-face)
(global-hl-line-mode)                   

;; フォント設定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Consolas"
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Consolas")))
        ((eq ws 'mac)
         (set-face-attribute 'default nil
                             :family "Source Han Code JP"
                             :height 130)
         (set-face-attribute 'variable-pitch nil  
                             :family "Ricty Diminished"
                             :height 136)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP"))
         ;; org-modeのみ英語と日本語が1:2の比率のものを利用する
         (add-hook 'org-mode-hook
          '(lambda()
             (buffer-face-set 'variable-pitch))))))

;; whitespace-modeの設定
(use-package whitespace
  :config
  (setq whitespace-style '(face           ; faceで可化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           space-mark     ; 表示のマッピング
                           tab-mark
                           ))

  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; The next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  (global-whitespace-mode 1))

;; IMEのディスプレイの設定
(when (eq system-type 'darwin)
  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードが英語の時はカーソルの色をfirebrickに、日本語の時はblackにする
    (set-cursor-color (if (or
                           (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                           (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                           "PaleVioletRed1" "Powder blue")))

  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'mac-selected-keyboard-input-source-change-hook-func))
