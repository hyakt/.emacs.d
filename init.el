;;; init.el --- My emacs settings. -*- coding: utf-8 ; lexical-binding: t -*-
;; Author: hyakt <https://github.com/hyakt/.emacs.d>

;;; Commentary:
;; This is hyakt's init.el of Emacs :tada: .

;;; Code:
;; デフォルトの shell を bashに変更
(setenv "SHELL" "/bin/bash")
(setq-default shell-file-name "/bin/bash")
(setq-default explicit-shell-file-name "/bin/bash")

;; ロードパス追加
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; ローカル設定
;; custom fileの読み込み
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; straight.elでパッケージ管理
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

;; use-packageとstraight.elを連携
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications 'live)
(use-package use-package-ensure-system-package)

;; 自作elispの読み込み
(load "my-functions")


;;; ---------- 初期設定 ----------
(setq auto-coding-functions nil)                  ;; 文字コードの自動変換保存をしない
(setq completion-ignore-case t)                   ;; file名の補完で大文字小文字を区別しない
(setq auto-save-default nil)                      ;; オートセーブのファイルを作らない
(setq make-backup-files t)                        ;; Backup fileの場所指定
(setq gc-cons-threshold (* 10 gc-cons-threshold)) ;; GCを減らして軽くする
(setq message-log-max 10000)                      ;; ログの記録行数を増やす
(setq vc-follow-symlinks t)                       ;; symlinkは必ず追いかける
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/.backup"))
            backup-directory-alist))              ;; バックアップの設定
(fset 'yes-or-no-p 'y-or-n-p)                     ;; yes-noの選択肢をy-nにする
(global-auto-revert-mode 1)                       ;; ファイルの自動再読み込み

(require 'server)
(unless (server-running-p) (server-start))        ;; サーバ起動

(when (eq system-type 'darwin)
  (mac-auto-ascii-mode 1)
  ;; 文字コードの設定
  (require 'ucs-normalize)
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(when (eq system-type 'gnu/linux)
  (setq x-alt-keysym 'meta)
  (setq x-super-keysym 'meta)
  ;; 文字コードの設定
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom((exec-path-from-shell-variables '("PATH" "GOPATH")))
  :config
  (exec-path-from-shell-initialize))


;;; ---------- 外観設定 ----------
;; 全般
(setq-default tab-width 2)                                    ;; タブの幅は半角スペース 2
(setq-default indent-tabs-mode nil)                           ;; タブの変わりに半角スペースを使う
(setq-default line-spacing 0)                                 ;; 行間を無しに設定
(setq-default cursor-type 'box)
(setq inhibit-startup-screen 1)                               ;; スタートアップメッセージを非表示
(setq initial-scratch-message "")                             ;; scratchの初期メッセージ消去
(setq truncate-lines nil)                                     ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)
(setq echo-keystrokes 0.1)                                    ;; キーストロークをエコーエリアに早く表示する
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 1) ;; スクロールの設定
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq frame-title-format "")                                  ;; タイトルバーに何も表示しない
(show-paren-mode 1)                                           ;; 対応する括弧を光らせる
(transient-mark-mode 1)                                       ;; 選択部分のハイライト
(global-font-lock-mode 1)                                     ;; フォントロックモード
(tool-bar-mode 0)                                             ;; ツールバーを利用しない
(set-scroll-bar-mode 'nil)                                    ;; スクロールバーを使わない
(line-number-mode 1)                                          ;; 行番号を表示
(column-number-mode 1)                                        ;; 列番号を表示
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))              ;; init-loaderが失敗した時のみエラーメッセージを表示

;; ウィンドウサイズの設定
(setq split-height-threshold 120)
(setq split-width-threshold 200)

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
                             :family "Myrica M"
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
  :init
  ;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27
  (setq custom--inhibit-theme-enable nil)
  (load "my-doom-tokyo-night")
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; モードラインの設定
(use-package doom-modeline
  :custom ((doom-modeline-buffer-encoding t)
           (doom-modeline-buffer-file-name-style 'truncate-with-project)
           (doom-modeline-height 32)
           (doom-modeline-bar-width 3))
  :config
  (doom-modeline-mode 1))

(use-package paren
  :straight nil
  :custom((show-paren-style 'mixed)
          (show-paren-when-point-inside-paren t)
          (show-paren-when-point-in-periphery t))
  :bind (("M-o" . my/jump-to-match-parens))
  :config
  (defun my/jump-to-match-parens ()
    "対応する括弧に移動"
    (interactive)
    (let ((paren-point (show-paren--default)))
      (let ((beg (nth 1 paren-point))
            (end (nth 3 paren-point)))
        (if (>= (point) beg)
            (goto-char end)
          (goto-char beg))))))

;; 移動した行にハイライト
(use-package beacon :config (beacon-mode 1))

;; 選択Window以外を暗くする
(use-package dimmer :init (dimmer-mode))

;; 編集した行にハイライト
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; Whitespaceの設定
(use-package whitespace
  :straight nil
  :custom
  (whitespace-style
   '(
     face
     ;; newline
     ;; newline-mark
     spaces
     space-mark
     tabs
     tab-mark
     trailing
     empty
     ))
  (whitespace-display-mappings
   '(
     ;; (space-mark   ?\     [?·]     [?.])          ; space - middle dot
     (space-mark   ?\xA0  [?\u00A4]     [?_])        ; hard space - currency
     (space-mark   ?\u3000 [?\u25a1])                ; Japanese zenkaku space - rect
     (newline-mark ?\n    [?$ ?\n])                  ; eol - dollar sign
     (tab-mark     ?\t    [?» ?\t] [?\\ ?\t])        ; tab - right guillemet
     ))
  (whitespace-action '(auto-cleanup))
  (whitespace-space-regexp "\\(\u3000\\)")
  :config
  (global-whitespace-mode 1))

(use-package indent-guide
  :custom
  (indent-guide-char ">")
  :config
  (indent-guide-global-mode))

(use-package disable-mouse
  :custom (disable-mouse-wheel-events '("wheel-left" "wheel-right"))
  :config (global-disable-mouse-mode))


;;; ---------- キーバインド設定 ----------
;; 通常操作
(keyboard-translate ?\C-h ?\C-?)
(bind-key (kbd "C-h") nil)
(bind-key (kbd "C-h") nil)
(bind-key (kbd "C-m") 'newline-and-indent) ; リターンで改行とインデント
(bind-key (kbd "C-x C-k") 'kill-buffer)
(bind-key (kbd "C-0") 'delete-frame)

;; my/function keybinding
(bind-key (kbd "C-g") 'my/keyboard-quit)
(bind-key (kbd "<f5>") 'my/revert-buffer-no-confirm)
(bind-key (kbd "M-r") 'my/revert-buffer-no-confirm)
(bind-key (kbd "C-x k") 'kill-this-buffer)
(bind-key (kbd "C-x C-k") 'my/close-and-kill-this-pane)
(bind-key (kbd "C-x C-x") 'my/kill-other-buffers)
(bind-key (kbd "C-x i") 'my/buffer-indent)
(bind-key (kbd "C-x d") 'my/dired-this-buffer)
(bind-key (kbd "M-t") 'my/open-hyper-current-buffer)


;;; ---------- 編集機能設定 ----------
(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/site-lisp/my-snippets"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package company
  :defer t
  :init (global-company-mode)
  :bind (("C-j" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("C-o" . company-other-backend))
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  (company-backends
   '((company-elisp)
     (company-yasnippet)))
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode t)
  :custom
  ((flycheck-disabled-checkers '(slim-lint))))

(use-package flyspell
  :ensure-system-package ((aspell . "brew install aspell"))
  :hook (LaTeX-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-," . nil))
  :config
  (setq-default ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package quickrun
  :bind (("C-x q" . quickrun)
         ("C-x a" . quickrun-with-arg)))

(use-package dumb-jump
  :bind (:map dumb-jump-mode-map
              (("C-M-g" . nil)
               ("C-M-p" . nil)
               ("C-M-q" . nil)))
  :custom
  (dumb-jump-default-project "")
  (dumb-jump-max-find-time 10)
  (dumb-jump-selector 'ivy)
  :config
  (dumb-jump-mode))

(use-package smart-jump
  :bind(("M-." . smart-jump-go)
        ("M-," . smart-jump-back)
        ("M-'" . smart-jump-references)
        ("M-P" . smart-jump-peek))
  :custom((smart-jump-bind-keys nil))
  :init
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'js2-mode
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'xref-pop-marker-stack
                       :refs-fn 'xref-find-references
                       :should-jump t
                       :heuristic 'point
                       :async t)
  (smart-jump-register :modes 'robe-mode
                       :jump-fn 'robe-jump
                       :pop-fn 'xref-pop-marker-stack
                       :refs-fn 'xref-find-references
                       :should-jump t
                       :heuristic 'point
                       :async t))

(use-package jumplist
  :defer 0
  :bind
  (("M-p" . jumplist-previous)
   ("M-n" . jumplist-next))
  :custom
  ((jumplist-hook-commands
    '(avy-goto-char
      mouse-set-point
      smart-jump-go smart-jump-ref
      lsp-ui-peek-find-definitions lsp-ui-peek-find-references
      xref-find-definitions xref-find-references
      dump-jump-go
      my/jump-to-match-parens
      swiper counsel-find-file counsel-switch-buffer
      counsel-rg counsel-projectile-switch-project counsel-git counsel-projectile
      end-of-buffer beginning-of-buffer))
   (jumplist-ex-mode t)))

;; 括弧の色付け
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; カラーコードの表示
(use-package rainbow-mode
  :hook ((js2-mode css-mode html-mode web-mode typescript-mode) . rainbow-mode))

;; 変数などの色付け
(use-package symbol-overlay
  :bind ("M-i" . symbol-overlay-put)
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode))

;; 正規表現をPerl(PCRE) likeに
(use-package pcre2el :custom (rxt-global-mode t))

(use-package elec-pair
  :straight nil
  :init (electric-pair-mode t)
  :hook((org-mode . org-add-electric-pairs)
        (web-mode . web-add-electric-pairs))
  :config
  (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (defvar web-electric-pairs '((?< . ?>) (?' . ?') (?` . ?`)) "Electric pairs for web-mode.")
  (defun web-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)))

;; 折りたたみ
(use-package yafolding
  :init (add-hook 'prog-mode-hook
                  (lambda () (yafolding-mode))))

(use-package google-translate
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

(use-package google-this
  :config (google-this-mode 1))

(use-package visual-regexp
  :custom ((case-fold-search  nil))
  :bind (("C-r" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :custom (vr/engine 'pcre2el))

(use-package codic)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("M-/" . undo-fu-only-redo)))

(use-package git-undo
  :bind (("C-x C-/" . git-undo)))

(use-package expand-region
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package string-inflection
  :bind (("M-[" . string-inflection-all-cycle)))


;;; ---------- メジャーモード設定 ----------
;; lsp-mode
(use-package lsp-mode
  :commands lsp
  :bind ((:map lsp-mode-map ("C-c i" . lsp-execute-code-action)))
  :custom ((lsp-enable-indentation nil)
           (lsp-eldoc-render-all t)
           (lsp-signature-auto-activate t)
           (lsp-signature-render-documentation t)
           (lsp-enable-snippet nil)
           (lsp-headerline-breadcrumb-enable nil)))

;; emacs-lisp
(use-package lispxmp
  :defer t
  :bind ((:map emacs-lisp-mode-map ("C-c C-e" . lispxmp-emacs-lisp ))
         (:map lisp-mode-map ("C-c C-e" . lispxmp-emacs-lisp )))
  :preface
  (defun %lispxmp-doit (eval-last-sexp-function)
    (let ((comment-start ";"))
      (comment-kill nil)
      (comment-indent)
      (save-excursion
        (let ((current-prefix-arg t)) (call-interactively eval-last-sexp-function)))
      (insert " => ")))

  (defun lispxmp-emacs-lisp ()
    (interactive)
    (%lispxmp-doit 'eval-last-sexp)))

;; Haskell
(use-package haskell-mode
  :mode (("\\.hs$" . haskell-mode)
         ("\\.lhs$" . literate-haskell-mode)))

;; HTML
(use-package web-mode
  :straight (:host github :repo "hyakt/web-mode" :branch "master")
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[gj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.[jt]sx\\'" . web-mode))
  :custom
  (web-mode-indent-style 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-indentation nil)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-auto-quoting nil)
  (web-mode-comment-formats
   '(("javascript" . "//")
     ("jsx" .  "//")
     ("php" . "/*")))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-content-type "jsx")
                (setq emmet-expand-jsx-className? t)
                (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
                (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
                (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
                (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
                (flycheck-add-mode 'javascript-eslint 'web-mode))
              (when (and (stringp buffer-file-name)
                         (string-match "\\.jsx\\'" buffer-file-name))
                (tern-mode)
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                (set (make-local-variable 'company-backends)
                     '((company-tern :with company-dabbrev-code) company-yasnippet)))
              (when (and (stringp buffer-file-name)
                         (string-match "\\.tsx\\'" buffer-file-name))
                (tide-setup)
                (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
                (set (make-local-variable 'company-backends)
                     '((company-tide) company-css company-yasnippet))

                (defun company-tide-advice (orig-fun &rest args)
                  (if (and (eq (car args) 'prefix) (web-mode-is-css-string (point)))
                      'nil
                    (apply orig-fun args)))
                (advice-add 'company-tide :around #'company-tide-advice)

                (defun web-mode-language-at-pos-advice (orig-fun &rest args)
                  (let ((pos (or (car args) (point))))
                    (or (and (web-mode-is-css-string pos) "css")
                        (apply orig-fun args))))
                (advice-add 'web-mode-language-at-pos :around #'web-mode-language-at-pos-advice))
              )))

(use-package company-web)

(use-package slim-mode
  :after company-web
  :config
  (add-hook 'slim-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-web-slim :with company-dabbrev))))))

(use-package haml-mode)

;; CSS
(use-package css-mode
  :straight nil
  :custom
  ((css-indent-offset 2))
  :config
  (add-hook 'css-mode-hook
            (lambda ()
              (set (make-local-variable 'flycheck-checker)
                   (setq flycheck-checker 'css-stylelint))
              (set (make-local-variable 'company-backends)
                   '((company-css :with company-dabbrev) company-yasnippet)))))

(use-package scss-mode
  :custom ((scss-indent-offset 2))
  :config
  (add-hook 'scss-mode-hook
            (lambda ()
              (set (make-local-variable 'flycheck-checker)
                   (setq flycheck-checker 'scss-stylelint))
              (set (make-local-variable 'company-backends)
                   '((company-css :with company-dabbrev) company-yasnippet)))))

(use-package sass-mode)
(use-package sws-mode) ;; Stylus

(use-package emmet-mode
  :bind (:map emmet-mode-keymap
              ("C-j" . company-complete))
  :hook ((html-mode web-mode css-mode scss-mode) . emmet-mode))

;; javascript
(use-package js2-mode
  :after (tern xref-js2)
  :mode (("\.js$" . js2-mode))
  :custom
  ((js-indent-level 2)
   (js-switch-indent-offset 2)
   (js2-basic-offset 2)
   (js2-strict-missing-semi-warning nil))
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (tern-mode)
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
              (set (make-local-variable 'company-backends)
                   '((company-tern :with company-dabbrev-code) company-yasnippet)))))

(use-package tern)

(use-package company-tern
  :after tern
  :straight (:host github :repo "emacsattic/company-tern" :branch "master"))

(use-package xref-js2
  :custom ((xref-js2-search-program 'rg)))

(use-package typescript-mode
  :custom (typescript-indent-level 2)
  :init
  (add-hook 'typescript-mode-hook
            (lambda()
              (tide-setup)
              (tide-hl-identifier-mode)
              (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append))))

(use-package tide
  :bind (:map tide-mode-map
              ("M-." . nil)
              ("M-," . nil))
  :after (typescript-mode company flycheck)
  :hook ((before-save . tide-format-before-save))
  :config
  (defun my/remove-tide-format-before-save ()
    (interactive)
    (remove-hook 'before-save-hook 'tide-format-before-save))
  (defun my/tide-copy-type ()
    "Copy type to clipbord."
    (interactive)
    (tide-command:quickinfo
     (tide-on-response-success-callback response (:ignore-empty t)
       (kill-new (tide-annotate-display-parts
                  (plist-get (plist-get response :body) :displayParts)))))))

(use-package coffee-mode
  :custom (coffee-tab-width 2))

(use-package nodejs-repl
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package add-node-modules-path
  :hook ((typescript-mode js2-mode web-mode scss-mode graphql-mode) . add-node-modules-path))

(use-package npm-mode
  :hook ((typescript-mode js2-mode web-mode scss-mode) . npm-mode))

(use-package jest
  :bind (:map jest-minor-mode-map ("C-c C-c C-c" . jest-popup))
  :hook ((typescript-mode js2-mode web-mode) . jest-minor-mode))

(use-package prettier-js
  :hook ((graphql-mode js2-mode scss-mode css-mode) . prettier-js-mode))

;; Dart
(use-package dart-mode
  :custom
  (dart-format-on-save nil)
  (dart-enable-analysis-server nil)
  (dart-sdk-path "~/repos/github.com/flutter/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/repos/github.com/flutter/flutter/"))

;; Python
(use-package python)

;; Swift
(use-package company-sourcekit)

(use-package swift-mode
  :defer t
  :after flycheck company-sourcekit
  :config
  (add-hook 'swift-mode-hook
            (lambda ()
              (add-to-list 'flycheck-checkers 'swift)
              (set (make-local-variable 'company-backends)
                   '((company-sourcekit))))))

;; Ruby
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("[Rr]akefile$" . ruby-mode))
  :custom ((ruby-insert-encoding-magic-comment nil))
  :interpreter "pry")

(use-package inf-ruby
  :bind (:map inf-ruby-minor-mode-map
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-l" . ruby-send-line))
  :init
  (defalias 'pry 'inf-ruby)
  :custom
  (inf-ruby-default-implementation "pry")
  (inf-ruby-eval-binding "Pry.toplevel_binding"))

(use-package rspec-mode
  :bind (:map rspec-mode-map ("C-c C-c C-c" . rspec-verify-single)))

(use-package robe
  :bind (:map robe-mode-map ("M-." . smart-jump-go))
  :hook (ruby-mode . robe-mode)
  :config
  (add-hook 'robe-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-robe)))
              (robe-start))))

;; PHP
(use-package php-mode)

;; SQL
(use-package sql
  :mode (("\.sql$" . sql-mode))
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (buffer-face-set 'variable-pitch)
              (toggle-truncate-lines t))))

(use-package sqlup-mode
  :commands (sqlup-mode)
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package sqlformat
  :ensure-system-package ((sqlformat . "brew install sqlparse"))
  :config
  (defun my/sql-indent-region (beg end)
    "Indent the SQL statement in the BEG to END (region)."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (sql-indent-buffer)))))

(use-package graphql-mode)

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (message "hook")
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            (setq c-basic-offset 4)))

;; Docker
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Git
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; Nginx
(use-package nginx-mode)

;; golang
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp))

;; elixir
(use-package elixir-mode
  :config
  (use-package alchemist)
  (use-package flycheck-elixir))

;; R
(use-package ess)

;; Scala
(use-package scala-mode :interpreter ("scala" . scala-mode))
(use-package sbt-mode :commands sbt-start sbt-command)
(use-package scala-bootstrap
  :straight (scala-bootstrap.el :type git :host github :repo "tarao/scala-bootstrap-el")
  :config
  (add-hook 'scala-mode-hook
            '(lambda ()
               (scala-bootstrap:with-metals-installed
                (scala-bootstrap:with-bloop-server-started
                 (lsp))))))

;; Rust
(use-package rustic
  :custom ((lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
           (rustic-format-display-method 'display-buffer)
           (rustic-format-trigger 'on-compile)))

;; fish
(use-package fish-mode)

;; CSV
(use-package csv-mode)

;; Org
(use-package org
  :straight nil
  :mode (("\\.txt$" . org-mode))
  :bind (:map org-mode-map
              (("C-," . nil)))
  :custom
  (org-startup-truncated nil)
  (org-src-fontify-natively t)
  (org-log-done 'time)
  :config
  (defun my-add-custom-id ()
    "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
    (interactive)
    (my-org-custom-id-get nil t))

  (defun my-get-custom-id ()
    "Return a part of UUID with an \"org\" prefix. e.g. \"org3ca6ef0c\"."
    (let* ((id (org-id-new "")))
      (when (org-uuidgen-p id)
        (downcase (concat "org"  (substring (org-id-new "") 0 8))))))

  (defun my-org-custom-id-get (&optional pom create)
    "See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (my-get-custom-id))
          (unless id
            (error "Invalid ID"))
          (org-entry-put pom "CUSTOM_ID" id)
          (message "--- CUSTOM_ID assigned: %s" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))
  (require 'ox-latex)
  (setq org-latex-default-class "cv")
  (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-file-apps
        '(("pdf" . "/usr/bin/open -a Preview.app %s")))
  (setq org-latex-with-hyperref nil)
  (setq org-latex-hyperref-template nil)

  (add-to-list 'org-latex-classes
               '("cv"
                 "\\documentclass[autodetect-engine,dvi=dvipdfmx,10pt,a4wide,ja=standard]{bxjsarticle}
                      \\parindent = 0pt
                      \\usepackage{typearea}
                      \\typearea{18}
                      \\usepackage{longtable}
                      [NO-DEFAULT-PACKAGES]
                      \\usepackage{amsmath}
                      \\usepackage{newtxtext,newtxmath}
                      \\usepackage{graphicx}
                      \\usepackage{hyperref}
                      \\ifdefined\\kanjiskip
                        \\usepackage{pxjahyper}
                        \\hypersetup{colorlinks=true}
                      \\else
                        \\ifdefined\\XeTeXversion
                            \\hypersetup{colorlinks=true}
                        \\else
                          \\ifdefined\\directlua
                            \\hypersetup{pdfencoding=auto,colorlinks=true}
                          \\else
                            \\hypersetup{unicode,colorlinks=true}
                          \\fi
                        \\fi
                      \\fi"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package htmlize)
(use-package ob-sql-mode :after org)
(use-package ox-gfm :after ox)

(use-package org-bullets
  :after org
  :custom
  (org-bullets-bullet-list '("■" "○" "✸" "►" "•" "★"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (defun my/org-bullets-export (path)
    "Export to bullets style text file into PATH."
    (interactive "FExport file: ")
    (let* ((current-buffer-string (buffer-string)))
      (with-temp-buffer
        (insert current-buffer-string)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ " nil t)
          (let ((level (- (match-end 0) (match-beginning 0) 1)))
            (replace-match
             (concat  (make-string (- level 1) ? ) (string (org-bullets-level-char level)) " "))))
        (write-file path))))

  (defun my/org-bullets-export-region-clipboard (start end)
    "Export to bullets style text file into clipbord from START to END."
    (interactive "*r")
    (let* ((current-buffer-string (buffer-substring start end)))
      (with-temp-buffer
        (insert current-buffer-string)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+" nil t)
          (let ((level (- (match-end 0) (match-beginning 0))))
            (replace-match
             (concat  (make-string (- level 1) ? ) (string (org-bullets-level-char level)) " "))))
        (clipboard-kill-ring-save (point-min) (point-max))))))

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.mdown\\'" . gfm-mode))
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'whitespace-action) nil))))


;;; ---------- インターフェース設定 ----------
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))

;; https://stackoverflow.com/questions/1731634/dont-show-uninteresting-files-in-emacs-completion-window
(defadvice completion--file-name-table (after
                                        ignoring-backups-f-n-completion
                                        activate)
  "Filter out results when they match `completion-ignored-extensions'."
  (let ((res ad-return-value))
    (if (and (listp res)
             (stringp (car res))
             (cdr res))                 ; length > 1, don't ignore sole match
        (setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

(use-package ediff
  :straight nil
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package projectile
  :bind (("C-x t" . my/projectile-toggle-between-implementation-and-test-other-window))
  :preface
  (defun my/projectile-toggle-between-implementation-and-test-other-window ()
    "Toggle between an implementation file and its test file."
    (interactive)
    (find-file-other-window
     (projectile-find-implementation-or-test (buffer-file-name))))
  :custom
  ((projectile-add-known-project '("~/repos/")))
  :config
  (projectile-mode +1))

(use-package projectile-rails
  :after projectile
  :commands (projectile-rails-on)
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package dashboard
  :custom
  (dashboard-items '((recents  . 10)
                     (projects . 10)))
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

(use-package counsel
  :ensure-system-package ((rg . "brew install ripgrep")
                          (fzf . "brew install fzf"))
  :bind(( "C-s" . swiper)
        ( "M-x" . counsel-M-x)
        ( "M-y" . counsel-yank-pop)
        ( "C-x C-f" . my/find-file-and-create-directory)
        ( "C-x C-r" . counsel-recentf)
        ( "C-x C-b" . switch-to-buffer)
        ( "<f1> f" . counsel-describe-function)
        ( "<f1> v" . counsel-describe-variable)
        ( "<f1> l" . counsel-find-library)
        ( "<f2> i" . counsel-info-lookup-symbol)
        ( "<f2> u" . counsel-unicode-char)
        ( "C-x f" . counsel-fzf)
        ( "C-x e" . counsel-rg)
        ( "C-c f" . counsel-flycheck)
        ( "C-x C-g" . counsel-git)
        :map read-expression-map
        ("C-r" . counsel-expression-history))
  :custom
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (ivy-height 20)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((t . ivy--regex-plus) (read-file-name-internal . ivy--regex-fuzzy)))
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop-separator "\n-------\n")
  (ivy-sort-matches-functions-alist  '((t)
                                       (ivy-completion-in-region . ivy--shorter-matches-first)
                                       (ivy-switch-buffer . ivy-sort-function-buffer)))
  :config
  (ivy-mode 1)
  ;; :custom ではなぜか反映されないため :config でsetqする
  (setq ivy-initial-inputs-alist nil)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))

  ;; counsel-find-file
  (defun reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun given-file (cmd prompt)
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))

  (defun confirm-delete-file (x)
    (dired-delete-file x 'confirm-each-subdirectory))

  (defun my/open-externally (file-name) ; MacOS
    "Open file with operating system's default application."
    (interactive "fOpen externally: ")
    (let ((process-connection-type nil))
      (start-process "open-externally" nil
                     "open" file-name)))

  (ivy-set-actions
   'my/find-file-and-create-directory
   `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("e" my/open-externally "open externally")
     ("w" find-file-other-window "other window")))

  ;; counsel-rg
  (defun my/counsel-rg-with-extention-and-word (_)
    "Execute counsel-rg with extention and _"
    (let ((word (read-from-minibuffer "Search Word: "))
          (extention (read-from-minibuffer "Extention: ")))
      (counsel-rg (concat word " -- -g'*." extention "'"))))

  (defun my/counsel-rg-from-current-directory (_)
    "Searched by current directory and subdirectories."
    (if (buffer-file-name)
        (counsel-rg nil (file-name-directory buffer-file-name))
      (counsel-rg nil (dired-current-directory))))

  (ivy-set-actions
   'counsel-rg
   '(("e" my/counsel-rg-with-extention-and-word "with-extention")
     ("d" my/counsel-rg-from-current-directory "search-from-current-directroy")))

  ;; counsel-fzf
  (defun my/counsel-fzf-from-current-directory (_)
    "Searched by current directory and subdirectories."
    (if (buffer-file-name)
        (counsel-fzf nil (file-name-directory buffer-file-name))
      (counsel-fzf nil (dired-current-directory))))

  (ivy-set-actions
   'counsel-fzf
   '(("d" my/counsel-fzf-from-current-directory "search-from-current-directroy")))

  ;; geleral action
  (defun my/ivy-yank-action (x) (kill-new x))
  (ivy-set-actions t
                   '(("y" my/ivy-yank-action "yank"))))

(use-package counsel-projectile
  :bind(( "C-x C-j" . counsel-projectile-switch-project)))

(use-package counsel-tramp
  :bind (( "C-x C-t" . counsel-tramp)))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-hydra :after counsel)


(use-package avy
  :custom ((avy-background t))
  :config
  (defun add-keys-to-avy (prefix c &optional mode)
    (define-key global-map
      (read-kbd-macro (concat prefix (string c)))
      `(lambda ()
         (interactive)
         (funcall (if (eq ',mode 'word)
                      #'avy-goto-word-1
                    #'avy-goto-char) ,c))))
  (loop for c from ?! to ?~ do (add-keys-to-avy "C-M-" c)))

(use-package which-key :config (which-key-mode))

(use-package amx)

;; dired
(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map ("P" . peep-dired)))

(use-package all-the-icons-dired
  :after dired )

(use-package dired-sidebar
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar)
         :map dired-sidebar-mode-map (("o" . dired-sidebar-subtree-toggle)))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom ((dired-sidebar-use-term-integration t)
           (dired-sidebar-use-custom-modeline nil))
  :config
  (defcustom dired-sidebar-mode-line-format
    '("%e" mode-line-front-space
      mode-line-buffer-identification
      " "  mode-line-end-spaces)
    "Mode line format for `dired-sidebar'."
    :type 'list
    :group 'dired-sidebar))

(use-package wdired
  :after dired
  :bind (:map dired-mode-map (("e" . wdired-change-to-wdired-mode)
                              ("C-t" . nil))))

;; shell
;; (use-package shell-pop
;;   :bind (("M-t" . shell-pop))
;;   :config
;;   (custom-set-variables
;;    '(shell-pop-shell-type '("eshell" " *eshell*" (lambda () (eshell))))))

(use-package eshell
  :defer t
  :custom
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history 'always)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (bind-key "C-r" 'counsel-esh-history eshell-mode-map))))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(use-package esh-autosuggest :hook (eshell-mode . esh-autosuggest-mode))

;; docker
(use-package docker)
(use-package docker-tramp)

;; git
(use-package magit
  :ensure-system-package ((git . "brew install git"))
  :bind (("C-x g" . magit-status)
         (:map magit-status-mode-map
               ("q" . my/magit-quit-session)
               ("C-o" . magit-diff-visit-file-other-window)))
  :preface
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window))
  :config
  (add-hook 'server-switch-hook 'magit-commit-diff))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign    " ")
  (git-gutter:deleted-sign  " ")
  :custom-face
  (git-gutter:modified ((t (:background "#B4DCE7"))))
  (git-gutter:added    ((t (:background "#74DFC4"))))
  (git-gutter:deleted  ((t (:background "#964C7B"))))
  :config
  (global-git-gutter-mode +1))

(use-package gist
  :custom
  (gist-list-format
   '((files "Filename" 24 nil identity)
     (created "Created" 20 nil "%D %R")
     (visibility "Visibility" 10 nil
                 (lambda
                   (public)
                   (or
                    (and public "public")
                    "private")))
     (description "Description" 0 nil identity))))

(use-package magit-gh-pulls
  :after magit
  :straight (:host github :repo "hyakt/magit-gh-pulls" :branch "master")
  :hook ((magit-mode . turn-on-magit-gh-pulls)))
(use-package git-timemachine)
(use-package git-link
  :custom ((git-link-open-in-browser t)))

(use-package twittering-mode
  :ensure-system-package ((gpg . "brew install gpg"))
  :custom
  (twittering-use-master-password t)
  (twittering-timer-interval 120)
  (twittering-convert-fix-size 24)
  (twittering-status-format
   "%FOLD{%RT{%FACE[bold]{RT}} %i%s %r %C{%m/%d %H:%M}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y/%m/%d %H:%M}} \n}")
  (epa-pinentry-mode 'loopback)
  :config
  (twittering-enable-unread-status-notifier))

(use-package open-junk-file
  :bind (("C-`" . open-junk-file))
  :custom
  (open-junk-file-format "~/Documents/junk/%Y-%m-%d-%H%M%S."))

(use-package view
  :bind (:map view-mode-map
              ("h" . backward-word)
              ("l" . forward-word)
              ("j" . next-line)
              ("k" . previous-line)
              (";" . gene-word)
              ("b" . scroll-down)
              (" " . scroll-up)
              ("n" . (lambda () (interactive) (scroll-up 1)))
              ("p" . (lambda () (interactive) (scroll-down 1)))
              ("." . bm-toggle)
              ("[" . bm-previous)
              ("]" . bm-next)
              ("c" . scroll-other-window-down)
              ("v" . scroll-other-window))
  :config
  (setq view-read-only t)

  ;; 書き込み不能なファイルはview-modeで開くように
  (defadvice find-file
      (around find-file-switch-to-view-file (file &optional wild) activate)
    (if (and (not (file-writable-p file))
             (not (file-directory-p file)))
        (view-file file)
      ad-do-it)))


;;; ---------- フレーム設定 ----------
(use-package swap-buffers
  :bind (("C-x C-o" . swap-buffers)))

(use-package other-window-or-split
  :straight (other-window-or-split :type git :host github :repo "conao/other-window-or-split")
  :bind (("C-t"  . my/ws-other-window-or-split-and-kill-minibuffer)
         ("C-S-t" . ws-previous-other-window-or-split))
  :config
  (setq ws-split-window-width-with-em 130)
  (use-package dired-sidebar)
  (defun my/ws-other-window-or-split ()
    (interactive)
    (when (one-window-p)
      (ws-split-window-dwim))
    (when (and (and
                (eq (length (window-list)) 2)
                (dired-sidebar-showing-sidebar-p))
               (not (eq (current-buffer)
                        (dired-sidebar-buffer (selected-frame)))))
      (ws-split-window-dwim))
    (other-window 1))
  (defun my/ws-other-window-or-split-and-kill-minibuffer ()
    (interactive)
    (if (active-minibuffer-window)
        (progn
          (minibuffer-keyboard-quit)
          (my/ws-other-window-or-split))
      (my/ws-other-window-or-split))))

;; screen
(use-package eyebrowse
  :custom
  (eyebrowse-keymap-prefix (kbd "C-z"))
  (eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode 1)
  (global-unset-key (kbd "C-z")))

(use-package shackle
  :custom
  ((shackle-default-size 0.4)
   (shackle-rules
    '(("*Help*"                   :align right)
      ("*Messages*"               :align right)
      ("*Backtrace*"              :align right)
      ("*Completions*"            :align below :ratio 0.33)
      ("*compilation*"            :align below :ratio 0.33)
      ("*Compile-Log"             :align below :ratio 0.33)
      ("*Kill Ring*"              :align below :ratio 0.33)
      ("*Occur*"                  :align below :ratio 0.33)
      ("*Google Translate*"       :align below :ratio 0.33)
      ("*Codic Result*"           :align below :ratio 0.33)
      ("*quickrun*"               :align below :ratio 0.33)
      ("*xref*"                   :align below :ratio 0.33)
      ("*prettier errors*"        :align below :ratio 0.33)
      (magit-status-mode          :select t :inhibit-window-quit t)
      ;; repl
      ("*Python*"                 :align below :ratio 0.33 :select t)
      ("*pry*"                    :align below :ratio 0.33 :select t)
      ("*ruby*"                   :align below :ratio 0.33 :select t)
      ("*nodejs*"                 :align below :ratio 0.33 :select t)
      ("*shell*"                  :align below :ratio 0.33 :select t)
      ;; excute shell
      ("*Async Shell Command*"    :align right)
      ("*Shell Command Output*"   :align right)
      ("\\`\\*My Mocha .*?\\*\\'" :regexp t :align below :ratio 0.3)
      ("*jest*"                   :regexp t :align below :ratio 0.3)
      ;; rust
      ("*rustic-compilation*"     :align below :ratio 0.33 :select nil)
      ("*rustfmt*"                :align below :ratio 0.33 :select nil)
      )))
  :init
  (shackle-mode 1))

(use-package centaur-tabs
  :bind
  (("M-{" . centaur-tabs-backward)
   ("M-}" . centaur-tabs-forward))
  :custom
  (centaur-tabs-height 34)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-close-button "x")
  :init
  (centaur-tabs-mode t)
  :config
  (centaur-tabs-headline-match))

;;; init.el ends here
