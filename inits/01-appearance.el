;;; 01-appearance.el --- 外観の設定
;;; Commentary:

;;; Code:
;; 全般
(setq-default line-spacing 0)                    ;; 行間を無しに設定
(setq-default cursor-type 'bar)
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
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP")))))

;; カーソルの色設定
(when (eq system-type 'darwin)
  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
    (set-cursor-color (if (or
                           (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                           (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                          "#FF5996" "#60FCEC")))
  (add-hook 'mac-selected-keyboard-input-source-change-hook
            'mac-selected-keyboard-input-source-change-hook-func))

(use-package all-the-icons)

(use-package zeno-theme
  :init (load-theme 'zeno t))                    ;; themeを設定

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

;; モードラインの設定
(use-package telephone-line
  :config
  (setq telephone-line-height 27)
  (setq telephone-line-primary-left-separator 'telephone-line-halfsin-left)
  (setq telephone-line-primary-right-separator 'telephone-line-halfsin-right)
  (setq telephone-line-secondary-left-separator 'telephone-line-halfsin-hollow-left)
  (setq telephone-line-secondary-right-separator 'telephone-line-halfsin-hollow-right)
  (let ((fg-accent "#FF5996")
        (bg-accent "#282A36"))
    (set-face-attribute 'mode-line nil
                        :box nil)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil)
    (set-face-attribute 'telephone-line-accent-active nil
                        :weight 'bold
                        :background bg-accent
                        :foreground fg-accent)
    (set-face-attribute 'telephone-line-accent-inactive nil
                        :background bg-accent))

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

  ;; Hide vc backend in modeline
  (defadvice vc-mode-line (after strip-backend () activate)
      (when (stringp vc-mode)
        (let ((my-vc (replace-regexp-in-string "^ Git." "" vc-mode)))
          (setq vc-mode my-vc))))

  ;; Display current branch
  (telephone-line-defsegment my-vc-segment ()
    (let ((fg "#66D9EF"))
      (when vc-mode
        ;; double format to prevent warnings in '*Messages*' buffer
          (format "%s %s"
                  (propertize (all-the-icons-octicon "git-branch")
                              'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :foreground ,fg)
                              'display '(raise 0.0))
                  (propertize
                    (format "%s"
                      (telephone-line-raw vc-mode t))
                    'face `(:foreground ,fg))))))

  (telephone-line-defsegment my-perspeen-segment ()
    (let ((fg "#A6E22E"))
      (when perspeen-mode
        (let ((index 0)
              (currnet-ws 0))
          (mapcar (lambda (ws)
                    (setq index (1+ index))
                    (if (eq ws perspeen-current-ws)
                        (setq current-ws index)))
                  perspeen-ws-list)
          (format "%s %s"
                  (propertize (all-the-icons-material "desktop_windows")
                              'face `(:family ,(all-the-icons-material-family) :height 1.0 :foreground ,fg))
                  (propertize (format "%s/%s" current-ws index)
                              'face `(:foreground ,fg)))))))

  (telephone-line-defsegment my-flycheck-segment ()
    (let ((error "#D2527F")
          (ok "#5FCA81")
          (other "#BB98FC"))
    (when (bound-and-true-p flycheck-mode)
      (let* ((text (pcase flycheck-last-status-change
                     ('finished (if flycheck-current-errors
                                    (let-alist (flycheck-count-errors flycheck-current-errors)
                                      (if (or .error .warning)
                                          (format "%s %s"
                                                  (propertize (all-the-icons-material "error_outline")
                                                              'face `(:family ,(all-the-icons-material-family) :foreground ,error))
                                                  (propertize (format "%s/%s" (or .error 0) (or .warning 0))
                                                              'face `(:foreground ,error)))
                                        ""))
                                      (propertize ":)" 'face `(:foreground ,ok))))
                     ('running     (propertize "*" 'face `(:foreground ,other)))
                     ('no-checker  (propertize "-" 'face `(:foreground ,other)))
                     ('not-checked (propertize "=" 'face `(:foreground ,other)))
                     ('errored     (propertize "!" 'face `(:foreground ,error)))
                     ('interrupted (propertize "." 'face `(:foreground ,error)))
                     ('suspicious  "?"))))
        (propertize text
                    'help-echo (pcase flycheck-last-status-change
                                 ('finished "Display errors found by Flycheck")
                                 ('running "Running...")
                                 ('no-checker "No Checker")
                                 ('not-checked "Not Checked")
                                 ('errored "Error!")
                                 ('interrupted "Interrupted")
                                 ('suspicious "Suspicious?"))
                    'display '(raise 0.0)
                    'mouse-face '(:box 1)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 #'flycheck-list-errors))))))
  ;; Left edge
  (setq telephone-line-lhs
        '((nil  . (telephone-line-buffer-segment))
          (nil  . (telephone-line-airline-position-segment))))

  ;; Right edge
  (setq telephone-line-rhs
        '((nil  . ((my-vc-segment :active)))
          (nil  . ((my-flycheck-segment :active)))
          (nil  . ((my-perspeen-segment :active)))
          (accent  . (telephone-line-major-mode-segment))))

  (telephone-line-mode 1))

;;; 01-appearance ends here
