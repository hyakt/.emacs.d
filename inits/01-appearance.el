;;; 01-appearance.el --- 外観の設定
;;; Commentary:

;;; Code:
;; 全般
(setq-default line-spacing 0)                    ;; 行間を無しに設定
(setq-default cursor-type 'box)
(setq truncate-lines nil)                        ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)
(setq inhibit-startup-screen 1)                  ;; スタートアップメッセージを非表示
(setq initial-scratch-message "")                ;; scratchの初期メッセージ消去
(setq echo-keystrokes 0.1)                       ;; キーストロークをエコーエリアに早く表示する
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 1)                             ;; スクロールの設定
(setq mouse-highlight nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode 1)                              ;; 対応する括弧を光らせる
(transient-mark-mode 1)                          ;; 選択部分のハイライト
(global-font-lock-mode 1)                        ;; フォントロックモード
(tool-bar-mode 0)                                ;; ツールバーを利用しない
(set-scroll-bar-mode 'nil)                       ;; スクロールバーを使わない
(line-number-mode 1)                             ;; 行番号を表示
(column-number-mode 1)                           ;; 列番号を表示
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only)) ;; init-loaderが失敗した時のみエラーメッセージを表示

;; タイトルバーに何も表示しない
(setq frame-title-format "")

;; ウィンドウサイズの設定
(setq default-frame-alist
      '((top . 0)
        (left . 100)
        (width . (text-pixels . 1280))
        (height . (text-pixels . 800))
        (alpha . (100 100))))

(add-hook 'kill-emacs-hook 'frame-size-save)
(defun frame-size-save ()
  "Save current the frame size and postion."
  (set-buffer (find-file-noselect (expand-file-name "~/.emacs.d/.framesize")))
  (erase-buffer)
  (insert (concat
           "(set-frame-width  (selected-frame) "
           (int-to-string (frame-width))")
            (set-frame-height (selected-frame) "
           (int-to-string (frame-height))")
            (set-frame-position (selected-frame) "
           (int-to-string (car (frame-position))) " "
           (int-to-string (cdr (frame-position))) ")"))
  (save-buffer)
  (kill-buffer))

(add-hook 'window-setup-hook 'frame-size-resume)
(defun frame-size-resume ()
  "Load the saved frame size."
  (let* ((file "~/.emacs.d/.framesize"))
    (if (file-exists-p file) (load-file file))))

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
                             :height 110)
         (set-face-attribute 'variable-pitch nil
                             :family "Ricty Diminished"
                             :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP")))))

;; カーソルの色設定
(when (eq system-type 'darwin)
  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
    (set-cursor-color (if (or
                           (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                           (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                          "#FF5996" "#51AFEF")))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'mac-selected-keyboard-input-source-change-hook-func))

(use-package all-the-icons)

(use-package doom-themes
  :custom-face
  (ivy-current-match ((t (:background "#5C748E"))))
  :init (load-theme 'doom-city-lights t)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; 移動した行にハイライト
(use-package beacon :config (beacon-mode 1))

;; 選択Window以外を暗くする
(use-package dimmer :init (dimmer-mode))

;; 編集した行にハイライト
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;; Whitespaceの設定
(use-package whitespace
  :custom
  (whitespace-style '(face           ; faceで可化
                      trailing       ; 行末
                      tabs           ; タブ
                      spaces         ; スペース
                      space-mark     ; 表示のマッピング
                      tab-mark ))
  (whitespace-display-mappings
   '((space-mark ?\u3000 [?\u25a1])
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  ;; スペースは全角のみを可視化
  (whitespace-space-regexp "\\(\u3000+\\)")
  :config
  (global-whitespace-mode 1))

;; モードラインの設定
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 32)
  (setq doom-modeline-bar-width 3))

;;; 01-appearance ends here
