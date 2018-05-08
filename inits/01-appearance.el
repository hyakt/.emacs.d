;;; 01-appearance.el --- 外観の設定
;;; Commentary:

;;; Code:
;; 全般
(use-package all-the-icons)
(load-theme 'nord t)              ;; themeを設定
(setq-default line-spacing 0)                    ;; 行間を無しに設定
(setq truncate-lines nil)                        ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)        ;; スタートアップメッセージを非表示
(setq inhibit-startup-screen 1)                  ;; scratchの初期メッセージ消去
(setq initial-scratch-message "")
(setq echo-keystrokes 0.1)                       ;; キーストロークをエコーエリアに早く表示する
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)                             ;; スクロールの設定
(setq mouse-highlight nil)

(use-package uniquify :config                    ;; 同じバッファ名の時 <2> とかではなく、ディレクトリ名で区別
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(show-paren-mode 1)                              ;; 対応する括弧を光らせる
(transient-mark-mode 1)                          ;; 選択部分のハイライト
(global-font-lock-mode 1)                        ;; フォントロックモード
(tool-bar-mode 0)                                ;; ツールバーを利用しない
(set-scroll-bar-mode 'nil)                       ;; スクロールバーを使わない
(line-number-mode 1)                             ;; 行番号を表示
(column-number-mode 1)                           ;; 列番号を表示
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only)) ;; init-loaderが失敗した時のみエラーメッセージを表示

(use-package dashboard :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))))

;; 移動した行にハイライト
(use-package beacon :config (beacon-mode 1))

;; 編集した行にハイライト
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;; ウィンドウサイズの設定
(setq default-frame-alist
      '((top . 0)
        (left . 100)
        (width . (text-pixels . 1280))
        (height . (text-pixels . 800))
        (alpha . (98 98))))

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

  ;; 保存前に自動でクリーンアップ
  (setq whitespace-action '(auto-cleanup))

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
  (setq telephone-line-height 28)
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left)
  (setq telephone-line-primary-right-separator 'telephone-line-tan-right)
  (setq telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left)
  (setq telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
  (set-face-attribute 'telephone-line-accent-inactive nil
                      :background "#3B4252")
  (set-face-attribute 'telephone-line-accent-active nil
                      :background "#434c5e"
                      :foreground "#eceff4")

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
    (let ((fg-color "#EBCB8B"))
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

  (telephone-line-defsegment my-flycheck-segment ()
    (when (bound-and-true-p flycheck-mode)
      (let* ((text (pcase flycheck-last-status-change
                     ('finished (if flycheck-current-errors
                                    (let-alist (flycheck-count-errors flycheck-current-errors)
                                      (if (or .error .warning)
                                          (propertize (format "%s/%s"
                                                              (or .error 0) (or .warning 0))
                                                      'face '(:foreground "#D08770"))
                                        ""))
                                  (propertize ":)" 'face '(:foreground "#A3BE8C"))))
                     ('running     "*")
                     ('no-checker  "-")
                     ('not-checked "=")
                     ('errored     (propertize "!" 'face '(:foreground "#D08770")))
                     ('interrupted (propertize "." 'face '(:foreground "#D08770")))
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
                                'mouse-1 #'flycheck-list-errors)))))

  ;; Left edge
  (setq telephone-line-lhs
        '((nil  . (telephone-line-buffer-segment))))

  ;; Right edge
  (setq telephone-line-rhs
        '((nil  . ((my-vc-segment :active)))
          (nil  . ((my-flycheck-segment :active)))
          (nil  . (telephone-line-misc-info-segment))
          (nil  . (telephone-line-airline-position-segment))
          (accent  . (telephone-line-major-mode-segment))))

  (telephone-line-mode 1))

;;; 01-appearance ends here
