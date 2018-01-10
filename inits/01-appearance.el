;;; 01-appearance.el --- 外観の設定
;;; Commentary:

;;; Code:
;; 全般
(use-package all-the-icons)
(load-theme 'doom-tomorrow-night t)              ;; themeを設定
(setq-default line-spacing 0)                    ;; 行間を無しに設定
(setq truncate-lines nil)                        ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)        ;; スタートアップメッセージを非表示
(setq inhibit-startup-screen 1)                  ;; scratchの初期メッセージ消去
(setq initial-scratch-message "")
(setq echo-keystrokes 0.1)                       ;; キーストロークをエコーエリアに早く表示する
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)                             ;; スクロールの設定

(use-package uniquify :config                    ;; 同じバッファ名の時 <2> とかではなく、ディレクトリ名で区別
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(show-paren-mode 1)                              ;; 対応する括弧を光らせる
(transient-mark-mode 1)                          ;; 選択部分のハイライト
(global-font-lock-mode 1)                        ;; フォントロックモード
(tool-bar-mode 0)                                ;; ツールバーを利用しない
(set-scroll-bar-mode 'nil)                       ;; スクロールバーを使わない
(line-number-mode 1)                             ;; 行番号を表示
(column-number-mode 1)                           ;; 列番号を表示
(global-hl-line-mode 1)                          ;; 現在行をハイライト
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only)) ;; init-loaderが失敗した時のみエラーメッセージを表示

;; ウィンドウサイズの設定
(setq default-frame-alist
      (append (list
               '(width . 220)
               '(height . 55)
               '(top . 0)
               '(left . 50)
               '(alpha . (97 97)))
              default-frame-alist))

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
                             :height 120)
         (set-face-attribute 'variable-pitch nil
                             :family "Ricty Diminished"
                             :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP"))
         ;; org-modeのみ英語と日本語が1:2の比率のものを利用する
         (add-hook 'org-mode-hook
          '(lambda()
             (buffer-face-set 'variable-pitch))))))

;; Whitespaceの設定
(use-package whitespace
  :config
  (setq whitespace-style '(face           ; faceで可化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           space-mark     ; 表示のマッピング
                           tab-mark ))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (global-whitespace-mode 1))

;; カーソルの色設定
(when (eq system-type 'darwin)
  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
    (set-cursor-color (if (or
                           (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                           (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                          "PaleVioletRed1" "Powder blue")))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'mac-selected-keyboard-input-source-change-hook-func))

;; モードラインの設定
(use-package telephone-line
  :config
  (use-package telephone-line-utils)
  (setq telephone-line-height 20)
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left)
  (setq telephone-line-primary-right-separator 'telephone-line-tan-right)
  (setq telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left)
  (setq telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)

    ;; Exclude some buffers in modeline
  (defvar modeline-ignored-modes nil
    "List of major modes to ignore in modeline")

  (setq modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell"
                                 "Debugger"
                                 "Quickrun"
                                 "REPL"
                                 "IELM"
                                 "Messages"))

  ;; Display buffer name
  (telephone-line-defsegment my-buffer-segment ()
    `(""
      ,(telephone-line-raw mode-line-buffer-identification t)))

  ;; Display current position in a buffer
  (telephone-line-defsegment* my-position-segment ()
    (if (telephone-line-selected-window-active)
        (if (eq major-mode 'paradox-menu-mode)
            (telephone-line-trim (format-mode-line mode-line-front-space))
          '(" %3l,%2c "))))

  ;; Display modified status
  (telephone-line-defsegment my-modified-status-segment ()
    (if (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)))
        (propertize "+" 'face `(:foreground "#85b654")))
    (propertize "-"))

  ;; Display encoding system
  (telephone-line-defsegment my-coding-segment ()
    (let* ((code (symbol-name buffer-file-coding-system))
           (eol-type (coding-system-eol-type buffer-file-coding-system))
           (eol (cond
                 ((eq 0 eol-type) "unix")
                 ((eq 1 eol-type) "dos")
                 ((eq 2 eol-type) "mac")
                 (t ""))))
      (concat eol " ")))

  ;; Hide vc backend in modeline
  (defadvice vc-mode-line (after strip-backend () activate)
      (when (stringp vc-mode)
        (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
          (setq vc-mode my-vc))))

    ;; Display current branch
  (telephone-line-defsegment my-vc-segment ()
    ;; #6fb593 #4a858c
    (let ((fg-color "#6fb593"))
      (when vc-mode
        ;; double format to prevent warnings in '*Messages*' buffer
          (format "%s %s"
                  (propertize (all-the-icons-octicon "git-branch")
                              'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground ,fg-color)
                              'display '(raise 0.0))
                  (propertize
                    (format "%s"
                      (telephone-line-raw vc-mode t))
                    'face `(:foreground ,fg-color))))))

  ;; Left edge
  (setq telephone-line-lhs
        '((accent    . (my-modified-status-segment))
          (nil    . (my-buffer-segment))))

  ;; Right edge
  (setq telephone-line-rhs
        '((nil     . ((my-vc-segment :active)))
          (accent  . (my-position-segment))
          (nil     . (telephone-line-simple-major-mode-segment))
          (accent  . ((my-coding-segment :active)))))

  (telephone-line-mode 1))
