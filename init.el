;;; init.el --- My emacs settings. -*- coding: utf-8; lexical-binding: t -*-
;; Author: Hayato Kajiyama <https://github.com/hyakt/.emacs.d>

;;; Commentary:
;; This is my init.el of Emacs.

;;; Code:
(defconst my-enable-measuring nil
  "If non-nil, mesure start up time.")

(when my-enable-measuring
  ;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/4d0a9dde1043c6eaffad
  (defvar setup-tracker--level 0)
  (defvar setup-tracker--parents nil)
  (defvar setup-tracker--times nil)

  (when load-file-name
    (push load-file-name setup-tracker--parents)
    (push (current-time) setup-tracker--times)
    (setq setup-tracker--level (1+ setup-tracker--level)))

  (add-variable-watcher
   'load-file-name
   (lambda (_ v &rest __)
     (cond ((equal v (car setup-tracker--parents))
            nil)
           ((equal v (cadr setup-tracker--parents))
            (setq setup-tracker--level (1- setup-tracker--level))
            (let* ((now (current-time))
                   (start (pop setup-tracker--times))
                   (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                               (/ (- (nth 2 now) (nth 2 start)) 1000))))
              (with-current-buffer (get-buffer-create "*setup-tracker*")
                (save-excursion
                  (goto-char (point-min))
                  (dotimes (_ setup-tracker--level) (insert "> "))
                  (insert
                   (file-name-nondirectory (pop setup-tracker--parents))
                   " (" (number-to-string elapsed) " msec)\n")))))
           (t
            (push v setup-tracker--parents)
            (push (current-time) setup-tracker--times)
            (setq setup-tracker--level (1+ setup-tracker--level))))))

  (require 'profiler)
  (profiler-start 'cpu))

(defvar my-delayed-configurations nil)
(defvar my-delayed-configuration-timer nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq my-delayed-configuration-timer
                  (run-with-timer
                   0.1 0.1
                   (lambda ()
                     (if my-delayed-configurations
                         (eval (pop my-delayed-configurations))
                       (cancel-timer my-delayed-configuration-timer)))))))

;; shrink title bar
(add-hook 'after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

(defmacro with-deferred-eval (&rest body)
  (declare (indent 0))
  `(push ',(cons 'progn body) my-delayed-configurations))

(defmacro when-macos (&rest body)
  (when (eq system-type 'darwin)
    `(progn ,@body)))

(eval-when-compile
  (require 'package)
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("stable" . "https://stable.melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (package-refresh-contents))

;; 読み込まないと init.elc の場合にエラーになる
(require 'bind-key)

;;; ---------- basic ----------
(setq user-full-name "Hayato Kajiyama")
(setq user-mail-address "me@piginbeer.com")

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(setq native-comp-async-report-warnings-errors 'silent)
(setq native-compile-prune-cache t)

(setq mac-command-modifier 'meta)

(setq make-backup-files t)                                         ;; Backup file を作る
(setq backup-directory-alist '(("\\.*$" .  "~/.emacs.d/.backup"))) ;; バックアップ先
(setq cursor-type 'box)
(setq completion-ignored-extensions '("~" ".o" ".elc" "./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store"))
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((control). 5)))
(setq text-quoting-style 'straight)
(setq echo-keystrokes 0.1)                                         ;; キーストロークをエコーエリアに早く表示する
(setq inhibit-startup-screen 1)                                    ;; スタートアップメッセージを非表示
(setq line-spacing 0)                                              ;; 行間を無しに設定
(setq scroll-conservatively 100)                                   ;; スクロールの設定
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq fast-but-imprecise-scrolling t)
(setq truncate-lines nil)                                          ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)     ;; 同じ名前のバッファを開いたときの設定
(setq split-height-threshold 120)
(setq split-width-threshold 200)
(setq auto-coding-functions nil)                                   ;; 文字コードの自動変換保存をしない
(setq completion-ignore-case t)                                    ;; file 名の補完で大文字小文字を区別しない
(setq auto-save-default nil)                                       ;; オートセーブのファイルを作らない
(setq create-lockfiles nil)                                        ;; ロックファイル(.#filename)のファイルを作らない
(setq confirm-kill-processes nil)                                  ;; 終了時のプロセス確認を出さない
(setq message-log-max 10000)                                       ;; ログの記録行数を増やす
(setq vc-follow-symlinks t)                                        ;; symlink は必ず追いかける
(setq enable-local-variables :all)                                 ;; local variable は全て使用する
(setq init-file-debug t)
(setq frame-resize-pixelwise t)
(setq history-length 3000)
(setq history-delete-duplicates t)
(setq custom-file "~/.emacs.d/custom.el")
(setq initial-scratch-message "")                                  ;; scratch の初期メッセージ消去
(setq initial-major-mode 'fundamental-mode)
(setq package-install-upgrade-built-in t)
(setq use-short-answers t)

;; performance
(setq process-adaptive-read-buffering t)
(setq blink-matching-paren nil)
(setq vc-handled-backends '(Git))
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq ffap-machine-p-known 'reject)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(when-macos
 (setq command-line-ns-option-alist nil))
(setq require-final-newline t)
(setq-default tab-width 2)                                         ;; タブの幅は半角スペース 2
(setq-default indent-tabs-mode nil)                                ;; タブの変わりに半角スペースを使う

(load (locate-user-emacs-file "./lisp/functions/my-functions-autoloads.el") nil t)

(with-deferred-eval
  (if (file-exists-p (expand-file-name custom-file))
      (load-file (expand-file-name custom-file))))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t))

(use-package minibuffer
  :defer t
  :config
  (setq enable-recursive-minibuffers t))

(use-package shell
  :defer t
  :config
  (setq explicit-shell-file-name shell-file-name))

(use-package recentf
  :defer 1
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (setq recentf-auto-cleanup 'never)

  (recentf-mode t))

(use-package savehist
  :defer 1
  :config
  (setq savehist-additional-variables '(kill-ring))
  (savehist-mode t))

(use-package saveplace
  :defer 1
  :init
  (save-place-mode t))

(use-package autorevert
  :defer 1
  :config
  (global-auto-revert-mode))

(use-package so-long
  :defer 1
  :config
  (global-so-long-mode))

(use-package server
  :defer 2
  :config
  (unless (server-running-p)
    (server-start)))

(use-package exec-path-from-shell
  :ensure t
  :defer 1
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (setq exec-path-from-shell-arguments nil)

  (exec-path-from-shell-initialize))

(use-package gcmh
  :ensure t
  :defer 5
  :config
  (gcmh-mode t))

;;; ---------- appearance ----------
(set-face-attribute 'default nil
                    :family "Source Han Code JP"
                    :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "JuliaMono"
                    :height 110)
(face-remap-add-relative 'variable-pitch :background "#0d1117")

(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP"))
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)

(set-frame-parameter nil 'alpha '(98 . 98))

(defun my-special-mode-hook ()
  "Customize background color for special modes."
  (setq buffer-face-mode-face `(:background "#0f0f14"))
  (buffer-face-mode 1))

(add-hook 'special-mode-hook 'my-special-mode-hook)
(mac-get-current-input-source)
(with-deferred-eval
  (when-macos
   (defun mac-selected-keyboard-input-source-change-hook-func ()
     ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
     (set-cursor-color (if (string-match "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" (mac-get-current-input-source))
                           "#FF5996" "#51AFEF")))
   (add-hook 'input-method-activate-hook 'mac-selected-keyboard-input-source-change-hook-func)
   (add-hook 'input-method-deactivate-hook 'mac-selected-keyboard-input-source-change-hook-func)
   (mac-input-method-mode 1))

  (global-font-lock-mode)
  (transient-mark-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (show-paren-mode t)
  (set-scroll-bar-mode nil))

(use-package whitespace
  :defer t
  :hook ((prog-mode org-mode) . whitespace-mode)
  :config
  (setq whitespace-style
        '(face
          spaces
          space-mark
          tabs
          tab-mark
          trailing
          empty))
  (setq whitespace-display-mappings
        '((space-mark
           ?\xA0
           [?\u00A4]
           [?_])
          (space-mark ?\u3000 [?\u25a1])
          (newline-mark ?\n [?$ ?\n])
          (tab-mark
           ?\t
           [?» ?\t]
           [?\\ ?\t])
          ))
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-space-regexp "\\(\u3000\\)"))

(use-package doom-themes
  :ensure t
  :defer 1
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
  (load-theme 'my-doom-tokyo-night t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :defer 1
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-height 32)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-enable-word-count 5)
  (setq doom-modeline-vcs-max-length 30)

  (doom-modeline-mode t))

;;; ---------- edit ----------
(with-deferred-eval
  (defun my-keyboard-quit()
    "Escape the minibuffer or cancel region consistently using 'Control-g'."
    (interactive)
    (if (not(window-minibuffer-p (selected-window)))
        (if (or mark-active (active-minibuffer-window))
            (keyboard-escape-quit))
      (keyboard-quit)))

  (defun my-buffer-indent ()
    "Indent whole current buffer."
    (interactive)
    (let ((current (point)))
      (push-mark (point-max) nil t)
      (goto-char (point-min))
      (indent-region (region-beginning)(region-end))
      (goto-char current)))

  (defun my-revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

  (defun my-close-and-kill-this-pane ()
    "If there are multiple windows, then close this pane and kill the buffer in it also."
    (interactive)
    (kill-this-buffer)
    (if (not (one-window-p))
        (delete-window)))

  (defun my-kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (cl-loop for buf in (buffer-list)
             unless (or
                     (get-buffer-window buf)
                     (string= (substring (buffer-name buf) 0 1) " ")
                     (get-buffer-process buf)
                     (member (buffer-name buf) ;; 消さないバッファ名を指定
                             '("*Messages*" "*Compile-Log*" "*Help*" "*scratch*" "*init log*")))
             do (kill-buffer buf)))

  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (keymap-global-set "C-m" #'newline-and-indent)
  (keymap-global-set "C-0" #'delete-frame)
  (keymap-global-set "C-g" #'my-keyboard-quit)
  (keymap-global-set "M-r" #'my-revert-buffer-no-confirm)
  (keymap-global-set "C-x k" #'kill-this-buffer)
  (keymap-global-set "C-x C-k" #'my-close-and-kill-this-pane)
  (keymap-global-set "C-x C-x" #'my-kill-other-buffers)
  (keymap-global-set "C-x i" #'my-buffer-indent)
  (keymap-global-set "M-<up>" #'windmove-up)
  (keymap-global-set "M-<down>" #'windmove-down)
  (keymap-global-set "M-<left>" #'windmove-left)
  (keymap-global-set "M-<right>" #'windmove-right)
  (keymap-global-set "M-+" #'text-scale-increase)
  (keymap-global-set "M-_" #'text-scale-decrease)
  (keymap-global-set "C-`" #'open-today-org-file)
  (keymap-global-set "C-\\" #'scratch-buffer)
  (keymap-global-set "C-o" #'my-other-window-or-split-and-kill-minibuffer)
  (keymap-global-unset "C-z")
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right]))

(use-package paren
  :defer t
  :bind (("M-o" . my-jump-to-match-parens))
  :config
  (defun my-jump-to-match-parens nil
    "対応する括弧に移動"
    (interactive)
    (if (and (eq major-mode 'web-mode)
             (string= (web-mode-language-at-pos) "html"))
        (web-mode-navigate)
      (ignore-errors
        (let ((paren-point (show-paren--default)))
          (let ((beg (nth 1 paren-point))
                (end (nth 3 paren-point)))
            (if (>=
                 (point)
                 beg)
                (goto-char end)
              (goto-char beg)))) t)))

  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(use-package tagedit
  :ensure t
  :defer t)

(use-package puni
  :ensure t
  :defer t
  :bind
  (:map puni-mode-map
        (("M-d" . nil)
         ("<DEL>" . puni-force-delete)
         ("C-k" . kill-line)
         ("C-M-k" . puni-kill-line)))
  :hook ((prog-mode . puni-mode)
         ((tsx-ts-mode vue-mode) . (lambda () (my-puni-jsx-setup))))
  :preface
  (defun my-puni-jsx-setup ()
    "Setup puni bindings for jsx."
    (interactive)
    (local-set-key [remap puni-kill-line] #'my-puni-jsx-kill-line))

  (defun my-puni-jsx-end-of-soft-kill ()
    (cond
     ((eolp)
      (forward-char))
     ;; Kill content inside a tag (i.e. between "<" and ">")
     ((and (looking-back (rx "<" (* (not (any "{>"))))
                         (line-beginning-position)))
      (if (re-search-forward (rx (? "/") ">") (line-end-position) t)
          (goto-char (match-beginning 0))
        (end-of-line)))
     ;; Kill content inside a tag pair (i.e. between an open tag and end tag)
     ((looking-back (rx ">" (* (not (any "<"))))
                    (line-beginning-position))
      (if (re-search-forward "<" (line-end-position) t)
          (goto-char (match-beginning 0))
        (end-of-line)))
     (t
      (end-of-line))))

  (defun my-puni-jsx-kill-line ()
    (interactive)
    (if (looking-at (rx (* blank) "<"))
        (tagedit-kill)
      (puni-soft-delete-by-move #'my-puni-jsx-end-of-soft-kill
                                nil
                                'beyond
                                ;; 'within
                                'kill
                                'delete-one))))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode)
  :config
  (defvar web-electric-pairs '((?< . ?>) (?' . ?') (?` . ?`)) "Electric pairs for web-mode.")
  (defun web-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (add-hook 'web-mode-hook #'web-add-electric-pairs)
  (add-hook 'typescript-ts-base-mode-hook #'web-add-electric-pairs)

  (defvar markdown-electric-pairs '((?` . ?`)) "Electric pairs for markdown-mode.")
  (defun markdown-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook #'markdown-add-electric-pairs)

  (defun my-inhibit-electric-pair-mode (char)
    (minibufferp))
  (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode))

(use-package ediff
  :defer t
  :config
  (setopt ediff-diff-program "diff")
  (setopt ediff-diff-options "-w")
  (setopt ediff-split-window-function #'split-window-horizontally)
  (setopt ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

(use-package flymake
  :defer t
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5)
  (setq flymake-fringe-indicator-position nil))

(use-package flymake-diagnostic-at-point
  :ensure t
  :defer t
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
    "Name of the flymake posframe buffer.")
  (defun flymake-diagnostic-at-point-display-posframe (text)
    "Display the flymake diagnostic TEXT inside a child frame."
    (posframe-show
     flymake-posframe-buffer
     :string (propertize
              (concat flymake-diagnostic-at-point-error-prefix text)
              'face (if-let ((type (get-char-property (point) 'flymake-diagnostic)))
                        (pcase (flymake--diag-type type)
                          (:error 'error)
                          (:warning 'warning)
                          (:note 'success)
                          (_ 'default))
                      'default))
     :left-fringe 4
     :right-fringe 4
     :max-width (round (* (frame-width) 0.62))
     :max-height (round (* (frame-height) 0.62))
     :internal-border-width 1
     :internal-border-color "#0f0f14"
     :background-color (face-background 'tooltip nil t))
    (unwind-protect
        (push (read-event) unread-command-events)
      (progn
        (posframe-hide flymake-posframe-buffer)
        (other-frame 0))))
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        #'flymake-diagnostic-at-point-display-posframe))

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package pulsar
  :ensure t
  :defer 1
  :config
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-face 'pulsar-cyan)
  (setq pulsar-window-change-face 'pulsar-cyan)

  (setq pulsar-pulse-functions
        (append '(avy-goto-char
                  symbol-overlay-jump-next symbol-overlay-jump-prev symbol-overlay-jump-last
                  smart-jump-go smart-jump-back
                  xref-find-definitions xref-find-references
                  dump-jump-go
                  my-jump-to-match-parens
                  end-of-buffer beginning-of-buffer ) pulsar-pulse-functions))
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (pulsar-global-mode))

(use-package indent-bars
  :vc (:url "https://github.com/jdtsmith/indent-bars")
  :defer t
  :config
  (setq indent-bars-width-frac 0.2)
  (setq indent-bars-pattern " .    .  ")
  (setq indent-bars-treesit-support t)
  :hook ((yaml-ts-mode) . indent-bars-mode))

(use-package tempel
  :ensure t
  :defer t
  :config
  (setq tempel-path (expand-file-name "lisp/templates" user-emacs-directory))

  (define-key tempel-map [remap my-tempel-maybe-expand] #'tempel-next)
  (define-key tempel-map "\C-g" #'tempel-done)
  (defun my-tempel-maybe-expand ()
    (interactive)
    (if (tempel-expand)
        (tempel-expand t)
      (indent-for-tab-command)))

  (add-hook 'before-save-hook 'tempel-done))

(use-package corfu
  :ensure t
  :defer t
  :hook
  ((prog-mode . corfu-mode)
   (prog-mode . corfu-popupinfo-mode)
   (org-mode . corfu-mode)
   (eshell-mode . corfu-mode))
  :bind (("C-j" . completion-at-point)
         (:map corfu-map
               ("<tab>" . nil)))
  :config
  (setq corfu-min-width 30)
  (setq corfu-auto t)
  (setq corfu-preview-current nil)
  (setq corfu-scroll-margin 0)
  (setq corfu-quit-at-boundary nil)
  (setq corfu-popupinfo-delay 0.2))

(use-package cape
  :ensure t
  :defer t
  :after corfu)

(use-package kind-icon
  :ensure t
  :defer t
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dumb-jump
  :ensure t
  :defer t
  :commands (dumb-jump-xref-activate)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package smart-jump
  :ensure t
  :defer t
  :bind
  (("M-." . my-custom-smart-jump)
   ("M-," . smart-jump-back)
   ("M-'" . smart-jump-references))
  :config
  (defun my-custom-smart-jump ()
    (interactive)
    (cond
     ((thing-at-point 'url)
      (browse-url (thing-at-point 'url)))
     ((thing-at-point 'existing-filename)
      (ffap))
     (t
      ;; smart-jump 起点として認識させる
      (let ((this-command 'smart-jump-go)
            (real-this-command 'smart-jump-go))
        (call-interactively #'smart-jump-go)))))

  (setq smart-jump-bind-keys nil)
  (smart-jump-setup-default-registers)
  (smart-jump-register :modes 'js-ts-mode
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
  :defer 5
  :ensure t
  :bind
  ("M-n" . jumplist-next)
  ("M-p" . jumplist-previous)
  :config
  (setq jumplist-hook-commands
        '(avy-goto-char
          mouse-set-point
          my-custom-smart-jump smart-jump-go smart-jump-ref
          xref-find-definitions xref-find-references
          dump-jump-go
          vr/query-replace
          my-jump-to-match-parens
          consult-line consult-ripgrep consult-find consult-ghq-find consult-fd consult-flymake
          expreg-expand
          end-of-buffer beginning-of-buffer))
  (setq jumplist-ex-mode t))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((js-ts-mode css-mode html-mode typescript-ts-base-mode) . rainbow-mode))

(use-package symbol-overlay
  :ensure t
  :defer t
  :bind ("C-." . symbol-overlay-put)
  :config
  ;; https://github.com/wolray/symbol-overlay/issues/82
  (setq symbol-overlay-priority 100))

(use-package visual-regexp
  :ensure t
  :defer t
  :bind ("C-r" . vr/query-replace)
  :config
  (setq case-fold-search nil))

(use-package pcre2el
  :ensure t
  :after visual-regexp)

(use-package visual-regexp-steroids
  :ensure t
  :after pcre2el
  :config
  (setq vr/engine 'pcre2el))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(use-package expreg
  :ensure t
  :defer t
  :bind
  ("C-," . expreg-expand)
  ("C-M-," . expreg-contract))

(use-package undo-fu
  :ensure t
  :defer t
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package vundo
  :ensure t
  :after undo-fu)

(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-enable-key "e")
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package grep
  :bind (:map grep-mode-map ("C-o" . nil)))

(use-package string-inflection
  :ensure t
  :defer t
  :bind ("M-[" . string-inflection-all-cycle))

(use-package rg
  :ensure t
  :defer t
  :config
  (setq rg-group-result t)
  (setq rg-custom-type-aliases '(("graphql" . "*.gql *.graphql"))))

(use-package pangu-spacing
  :ensure t
  :defer 5
  :config
  (setq pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp
        (rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
                         (group-n 1 (or (category chinse-two-byte)
                                        (category japanese-hiragana-two-byte)
                                        (category japanese-katakana-two-byte))))
                     (group-n 2 (in "a-zA-Z")))
                (and (group-n 1 (in "a-zA-Z"))
                     (or (group-n 3 (any "。，！？；：「」（）、"))
                         (group-n 2 (or (category chinse-two-byte)
                                        (category japanese-hiragana-two-byte)
                                        (category japanese-katakana-two-byte))))))))

  (defun my-pangu-spacing-region (beg end)
    "Replace regexp with match in region."
    (interactive "r")
    (pangu-spacing-search-buffer
     pangu-spacing-include-regexp beg (+ end 8) (replace-match "\\1 \\2" nil nil))))

(use-package avy
  :ensure t
  :defer t
  :bind ("C-;" . avy-goto-char))

(use-package mwim
  :ensure t
  :defer t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package request
  :ensure t
  :defer t)

(use-package posframe
  :defer t
  :ensure t)

;; ウィンドウ幅に応じて動的に幅を決定する関数
(defun my-adaptive-window-width ()
  "Return appropriate window width based on frame width."
  (let ((frame-width (frame-width)))
    (cond
     ((> frame-width 300) 0.3)   ; 広い画面では30%
     ((> frame-width 200) 0.4)   ; 中程度では40% (現在の設定)
     (t 0.5))))                  ; 狭い画面では50%

;; 動的幅計算付きサイドウィンドウ表示関数
(defun my-display-buffer-in-side-window-adaptive (buffer alist)
  "Display BUFFER in side window with adaptive width."
  (let* ((side (cdr (assq 'side alist)))
         (slot (cdr (assq 'slot alist)))
         (window-width (my-adaptive-window-width))
         (new-alist (cons `(window-width . ,window-width)
                          (assq-delete-all 'window-width alist))))
    (display-buffer-in-side-window buffer new-alist)))

(use-package copilot
  :ensure t
  :hook ((prog-mode
          . (lambda ()
              (when (and buffer-file-name
                         (file-readable-p buffer-file-name)
                         (< (nth 7 (file-attributes buffer-file-name)) 100000))
                (copilot-mode t))))
         ((json-ts-mode yaml-ts-mode eshell-mode) . copilot-mode))
  :bind (("<tab>" . copilot-accept-completion)
         ("M-P" . copilot-next-completion)
         ("M-N" . copilot-previous-completion))
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 100000))

(use-package copilot-chat
  :ensure t
  :demand t
  :bind* (("M-q" . copilot-chat-toggle))
  :after magit
  :init
  (setq copilot-chat-frontend 'shell-maker)
  :hook ((git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setopt shell-maker-prompt-before-killing-buffer nil)
  (setopt shell-maker-display-function #'display-buffer)
  (setopt copilot-chat-default-model "gpt-5.1")

  (defun copilot-chat-toggle()
    "Copilot chat toggle."
    (interactive)
    (if (string-prefix-p "*Copilot Chat" (buffer-name))
        (copilot-chat-hide)
      (let* ((major-mode-str (symbol-name major-mode))
             (lang (replace-regexp-in-string "\\(?:-ts\\)?-mode$" "" major-mode-str))
             (region (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       ""))
             (buffer (current-buffer)))
        (deactivate-mark)
        (copilot-chat-switch-to-buffer)
        (with-current-buffer buffer
          (copilot-chat-add-current-buffer))
        (when (not (string= region ""))
          (save-excursion
            (goto-char (point-max))
            (insert "\n```" lang "\n" region "\n```"))))))

  (defun copilot-chat-hide()
    "Hide buffer."
    (interactive)
    (if (window-deletable-p)
        (delete-window)))

  (defun copilot-chat-close()
    "Hide buffer."
    (interactive)
    (if (window-deletable-p)
        (progn
          (copilot-chat-list-clear-buffers)
          (kill-buffer)
          (delete-window))))

  (defun my-copilot-chat-use-current-directory (orig-fun &rest args)
    "Advice to automatically use current directory instead of prompting."
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (prompt &optional default-dir &rest _)
                 (or default-dir default-directory))))
      (apply orig-fun args)))

  (advice-add 'copilot-chat--create-instance :around #'my-copilot-chat-use-current-directory)

  (add-to-list 'display-buffer-alist
               '("\\*Copilot Chat"
                 (display-buffer-reuse-window my-display-buffer-in-side-window-adaptive)
                 (side . right)
                 (reusable-frames . visible))))

(use-package opencode
  :vc (:url "https://codeberg.org/sczi/opencode.el")
  :bind ("M-1" . my-opencode-toggle)
  :config
  (setq opencode-api-log-max-lines 1000
        opencode-event-log-max-lines 1000)
  (setq opencode-auto-start-server t)

  (defun my-opencode--major-mode-lang (mode)
    "Return language name for MODE."
    (replace-regexp-in-string "\\(?:-ts\\)?-mode$" "" (symbol-name mode)))

  (defun my-opencode--session-buffer-p (buffer)
    "Return non-nil if BUFFER is an OpenCode session buffer."
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (bound-and-true-p opencode-session-id))))

  (defun my-opencode--project-root (&optional buffer)
    "Return project root for BUFFER or nil."
    (let ((default-directory (with-current-buffer (or buffer (current-buffer))
                               default-directory)))
      (when-let ((project (project-current nil default-directory)))
        (expand-file-name (project-root project)))))

  (defun my-opencode--most-recent-session-buffer (&optional source-buffer)
    "Return the most recently used OpenCode session buffer.
If SOURCE-BUFFER has a project, prefer a session in that project."
    (let ((root (my-opencode--project-root source-buffer)))
      (cl-loop for buffer in (buffer-list)
               when (and (my-opencode--session-buffer-p buffer)
                         (or (not root)
                             (string= root (my-opencode--project-root buffer))))
               return buffer)))

  (defun my-opencode-add-current-buffer (buffer)
    "Add BUFFER to the current OpenCode session context."
    (let ((name (buffer-name buffer)))
      (cl-letf (((symbol-function 'read-buffer)
                 (lambda (&rest _) name)))
        (opencode-add-buffer))))

  (defun my-opencode--with-session (fn)
    "Call FN with an OpenCode session buffer."
    (if-let ((buffer (my-opencode--most-recent-session-buffer (current-buffer))))
        (funcall fn buffer)
      (call-interactively 'opencode)))

  (defun my-opencode-toggle ()
    "Toggle OpenCode session window.
If a region is active, insert it as a fenced code block."
    (interactive)
    (if (string-prefix-p "*OpenCode" (buffer-name))
        (my-opencode-hide)
      (let* ((source-buffer (current-buffer))
             (lang (my-opencode--major-mode-lang major-mode))
             (region (if (use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))
                       "")))
        (deactivate-mark)
        (my-opencode--with-session
         (lambda (session-buffer)
           (pop-to-buffer session-buffer)
           (with-current-buffer session-buffer
             (when (not (string= region ""))
               (my-opencode-add-current-buffer source-buffer)
               (goto-char (point-max))
               (insert "\n```" lang "\n" region "\n```"))))))))

  (defun my-opencode-hide ()
    "Hide current window."
    (interactive)
    (when (window-deletable-p)
      (delete-window)))

  (defun my-opencode--format-tool-call-with-apply-patch (orig-fun tool input)
    "Format apply_patch tool call for display."
    (if (string= tool "apply_patch")
        (let-alist input
          (let ((patch (or .patchText "")))
            (if (string= patch "")
                (funcall orig-fun tool input)
              (concat "apply_patch:\n"
                      (my-opencode--fontify-apply-patch patch)
                      "\n"))))
      (funcall orig-fun tool input)))

  (defun my-opencode--fontify-apply-patch (patch)
    "Return PATCH with diff-mode font-lock properties."
    (with-temp-buffer
      (insert patch)
      (delay-mode-hooks (diff-mode))
      (font-lock-ensure)
      (buffer-string)))

  (advice-add 'opencode--format-tool-call :around #'my-opencode--format-tool-call-with-apply-patch)

  (add-to-list 'display-buffer-alist
               '("\\*OpenCode"
                 (display-buffer-reuse-window my-display-buffer-in-side-window-adaptive)
                 (side . right)
                 (reusable-frames . visible))))

(use-package comint
  :defer t
  :hook
  (comint-mode . (lambda ()
                   (progn
                     (setq buffer-face-mode-face `(:background "#0f0f14"))
                     (buffer-face-mode 1))))
  :config
  (setopt comint-scroll-to-bottom-on-input t)
  (setopt comint-scroll-to-bottom-on-output t))

(use-package gt
  :defer t
  :ensure t
  :bind ("C-c C-t" . gt-translate)
  :config
  (setq gt-langs '(en ja))
  (setq gt-default-translator
        (gt-translator
         :engines (list (gt-deepl-engine) (gt-google-engine) (gt-bing-engine))
         :render  (gt-posframe-pop-render
                   :width 100  :frame-params (list :cursor 'box :timeout nil)))))

(use-package google-this
  :bind ("C-c C-k" . google-this-noconfirm)
  :ensure t
  :defer t)

(use-package vlf
  :ensure t
  :defer t)

;;; ---------- interface ----------
(use-package mise
  :ensure t
  :defer t
  :hook (prog-mode-hook . mise-mode))

(use-package hydra
  :ensure t
  :defer t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        '(
          :internal-border-width 10
          :internal-border-color "#0f0f14"
          :background-color "#0f0f14"
          :foreground-color "white"
          :lines-truncate t
          :poshandler posframe-poshandler-window-center)))

(use-package major-mode-hydra
  :ensure t
  :defer t
  :bind ("M-a" . major-mode-hydra)
  :config
  (setq major-mode-hydra-invisible-quit-key "q"))

(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode t))

(use-package swap-buffers
  :ensure t
  :defer t
  :bind ("C-x C-o" . swap-buffers))

(use-package project
  ;; :init
  ;; (setq project-vc-extra-root-markers '("package.json"))
  )

(use-package projectile
  :ensure t
  :defer t
  :bind (("C-x t" . my-projectile-toggle-between-implementation-and-test-other-window))
  :config
  (setq projectile-add-known-project '("~/repos/"))

  (defun my-projectile-toggle-between-implementation-and-test-other-window ()
    "Toggle between an implementation file and its test file."
    (interactive)
    (find-file-other-window
     (projectile-find-implementation-or-test
      (buffer-file-name)))))

(use-package disproject
  :ensure t
  :defer t
  :bind (("C-x C-p" . disproject-dispatch)))

(use-package tab-bar
  :defer t
  :bind (("M-t" . tab-bar-new-tab-to)
         ("M-W" . tab-bar-close-tab)
         ("M-}" . tab-bar-switch-to-next-tab)
         ("M-{" . tab-bar-switch-to-prev-tab))
  :hook (tab-bar-mode . (lambda ()
                          (setq tab-bar-close-button-show nil)
                          (setq tab-bar-show 1)
                          (setq tab-bar-separator "")
                          (setq tab-bar-new-tab-to 'rightmost)))
  :config
  (tab-bar-mode t))

(use-package desktop
  :config
  (setq desktop-load-locked-desktop t)
  (setq desktop-save t)
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.gz\\|\\.bbdb"
                "\\)$"))
  (setq desktop-restore-eager 5)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'special-mode)
  (desktop-save-mode t))

(use-package eldoc
  :defer t
  :bind
  ("C-M-." . eldoc-doc-buffer-toggle)
  :config
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  (defun eldoc-doc-buffer-toggle()
    "Eldoc toggle."
    (interactive)
    (if (eldoc-doc-buffer-toggle--get-window)
        (eldoc-doc-buffer-toggle-hide)
      (eldoc-doc-buffer t)))

  (defun eldoc-doc-buffer-toggle-hide ()
    "Hide buffer."
    (interactive)
    (or (string-prefix-p "*eldoc" (buffer-name))
        (select-window (eldoc-doc-buffer-toggle--get-window)))
    (quit-window))

  (defun eldoc-doc-buffer-toggle--get-window()
    "Get the eldoc window which is visible (active or inactive)."
    (cl-find-if #'(lambda(w)
                    (string-prefix-p "*eldoc" (buffer-name (window-buffer w))))
                (window-list)))

  (add-to-list 'display-buffer-alist
               '("^\\*eldoc" display-buffer-at-bottom
                 (window-height . 0.3))))

(use-package dired
  :defer t
  :bind (("C-x C-d" . my-dired-this-buffer)
         :map dired-mode-map
         ("e" . wdired-change-to-wdired-mode)
         ("C-o" . nil)
         ("M-s" . nil)
         ("c" . my-dired-do-copy-with-filename)
         ("M-<up>" . nil)
         ("M-<down>" . nil)
         ("M-<left>" . nil)
         ("M-<right>" . nil))
  :config
  (setq dired-dwim-target t)

  (defun my-dired-this-buffer ()
    "Open dired in this buffer."
    (interactive)
    (dired
     (file-name-directory (expand-file-name (buffer-name)))))

  ;; https://y0m0r.hateblo.jp/entry/20120219/1329657774
  (defun my-dired-view-file-other-window ()
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (or (and (cdr dired-subdir-alist)
                   (dired-goto-subdir file))
              (dired file))
        (view-file-other-window file)
        )))

  (defun my-dired-do-copy-with-filename (&optional arg)
    (interactive "P")
    (let* ((filename
            (or (dired-get-subdir)
                (mapconcat #'identity
                           (if arg
                               (cond ((zerop (prefix-numeric-value arg))
                                      (dired-get-marked-files))
                                     ((consp arg)
                                      (dired-get-marked-files t))
                                     (t
                                      (dired-get-marked-files
                                       'no-dir (prefix-numeric-value arg))))
                             (dired-get-marked-files 'no-dir))
                           " ")))
           (new-filename (read-string (format "Copy %s to: " filename) (dired-get-filename))))
      (copy-file (dired-get-filename) new-filename))
    (revert-buffer))

  (major-mode-hydra-define dired-mode ()
    ("Mark"
     (("m" dired-mark)
      ("t" dired-toggle-marks)
      ("U" dired-unmark-all-marks)
      ("u" dired-unmark))
     "Manipulate"
     (("+" dired-create-directory :exit t)
      ("M" dired-do-chmod :exit t)
      ("D" dired-do-delete :exit t)
      ("c" my-dired-do-copy-with-filename :exit t)
      ("C" dired-do-copy :exit t)
      ("R" dired-do-rename :exit t)
      ("e" wdired-change-to-wdired-mode :exit t)
      ("w" dired-copy-filename-as-kill :exit t)
      ("W" dired-get-fullpath-filename :exit t))
     "Open"
     (("o" dired-find-file-other-window :exit t)
      ("v" dired-view-file :exit t)
      ("V" my-dired-view-file-other-window :exit t)
      ("s" dired-sort-toggle-or-edit)
      ("g" revert-buffer)))))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :defer t)

(use-package nerd-icons
  :ensure t
  :defer t
  :config
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (nerd-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-sucicon (icon str &optional height v-adjust)
    (s-concat (nerd-icons-sucicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-codicon (icon str &optional height v-adjust)
    (s-concat (nerd-icons-codicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(use-package dired-sidebar
  :ensure t
  :defer t
  :bind
  (("M-d" . dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
   ("o" . dired-sidebar-subtree-toggle)
   ("q" . kill-this-buffer))
  :hook
  (dired-sidebar-mode . (lambda ()
                          (setq buffer-face-mode-face `(:background "#0f0f14"))
                          (buffer-face-mode 1)
                          (setq-local mode-line-format nil)))
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-modeline nil))

(use-package consult
  :ensure t
  :pin stable
  :defer t
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x C-b" . consult-buffer)
         ("C-x f" . consult-fd)
         ;; consult-ripgrepの検索構文メモ
         ;;
         ;; 1. 基本的な使い方:
         ;;    - 「pattern options」形式で検索オプションを指定できる
         ;;
         ;; 2. コンテキスト行表示:
         ;;    - 「検索語 -C3」     前後3行表示
         ;;    - 「検索語 -B2 -A4」 前2行・後4行表示
         ;;
         ;; 3. 複数キーワード検索:
         ;;    - 「word1 word2」      word1とword2の両方を含む（順不同）
         ;;    - 「"exact phrase"」   完全一致フレーズを検索
         ;;
         ;; 4. 正規表現:
         ;;    - 「pattern.*another」  patternの後にanotherがある行
         ;;    - 「\bword\b」          単語境界のwordを検索
         ;;
         ;; 5. 特定ファイル検索:
         ;;    - 「pattern -g "*.js"」   JavaScriptファイルのみ検索
         ;;    - 「pattern -g "!*.min.*"」最小化ファイルを除外
         ;;    - 「pattern -g "!*.test.*"」テストファイルを除外
         ;;    - 「pattern -g "*.js" -g "!*.test.*"」JSファイルからテストを除外
         ;;
         ;; 6. 固定文字列検索（正規表現でなく）:
         ;;    - 「検索語 -F」      正規表現記号をただの文字として扱う
         ;;
         ;; 7. 大文字小文字:
         ;;    - 「検索語 -s」      大文字小文字を区別する
         ;;    - 「検索語 -i」      大文字小文字を無視する
         ;;
         ;; 8. その他便利なオプション:
         ;;    - 「検索語 -l」      一致したファイル名のみ表示
         ;;    - 「検索語 --hidden」隠しファイルも検索
         ("C-x e" . consult-ripgrep)
         ("C-x C-r" . my-tab-bar-filtered-consult-recent-file)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("C-s" . consult-line)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu))
  :config
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)

  ;; https://github.com/minad/consult/issues/837#issuecomment-1703762384
  (consult-customize
   consult-fd
   :state (consult--file-preview))

  (consult-customize
   find-file
   consult-fd
   consult-ripgrep
   consult-git-grep
   consult-recent-file
   consult-ls-git
   my-tab-bar-filtered-consult-recent-file
   :preview-key '(:debounce 1 any))

  (defun my-tab-bar-filtered-consult-recent-file ()
    "Show recentf files filtered by current tab-bar tab name."
    (interactive)
    (let* ((current-tab (tab-bar--current-tab))
           (tab-name (alist-get 'name current-tab))
           (explicit-name (alist-get 'explicit-name current-tab))
           (recentf-list-copy (bound-and-true-p recentf-list))
           (filtered-files
            (if (and tab-name explicit-name)
                (seq-filter (lambda (file)
                              (string-match-p (regexp-quote tab-name) file))
                            recentf-list-copy)
              recentf-list-copy)))

      ;; Find file using consult--read
      (find-file
       (consult--read
        (mapcar #'consult--fast-abbreviate-file-name filtered-files)
        :prompt (if (and tab-name explicit-name)
                    (format "Find recent file (filtered by '%s'): " tab-name)
                  "Find recent file: ")
        :sort nil
        :require-match t
        :category 'file
        :state (consult--file-preview)
        :history 'file-name-history)))))

(use-package consult-ghq
  :ensure t
  :defer t
  :bind ("C-x C-g" . consult-ghq-find))

(use-package consult-ls-git
  :ensure t
  :defer t
  :bind ("C-x g" . consult-ls-git))

(use-package vertico
  :ensure t
  :defer 1
  :bind (:map vertico-map
              ("C-l" . vertico-directory-up))
  :init
  (vertico-mode)
  :config
  (setq vertico-count 30))

(use-package orderless
  :ensure t
  :defer 1
  :config
  (setq completion-styles '(orderless)))

(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode))

(use-package embark
  :ensure t
  :after which-key
  :defer 5
  :bind
  ("M-e" . embark-act)
  :config
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
    The which-key help message will show the type and value of the
    current target followed by an ellipsis if there are further
    targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-hook . consult-preview-at-point-mode)
  :after (embark consult))

(use-package affe
  :ensure t
  :pin stable
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (setq affe-find-command "fd --color=never --full-path")
  (consult-customize affe-grep :preview-key "M-."))

(use-package eshell
  :defer t
  :bind (:map eshell-mode-map
              ("C-r" . consult-history)
              ("C-l" . my-eshell/clear-scrollback)
              ("C-d" . eshell-life-is-too-much))
  :init
  (defun my-eshell/clear-scrollback ()
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input))

  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history 'always)

  (defun eshell/magit (&rest args)
    "Magit for eshell."
    (if (null args)
        (magit-status)
      (pcase (car args)
        ("log" (magit-log))
        ("diff" (magit-diff-dwim))
        ("ci" (magit-commit))
        ("commit" (magit-commit))
        ("pull" (magit-pull))
        ("push" (magit-push))
        (_ (shell-command-to-string (concat "git " (eshell-flatten-and-stringify args)))))
      )
    )

  (defalias 'g 'eshell/magit)
  (defalias 'd (lambda () (dired ".")))
  (defalias 'e 'find-file-other-window))

(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

(use-package eshell-syntax-highlighting
  :defer t
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package fish-completion
  :defer t
  :ensure t
  :hook (eshell-mode . fish-completion-mode))

(use-package mistty
  :ensure t
  :defer t
  :bind (("C-t" . mistty-toggle)
         :map mistty-prompt-map
         ("C-d" . mistty-toggle-hide))
  :custom-face
  (mistty-fringe-face ((t (:foreground "#bbc2e0"))))
  :hook
  (mistty-mode . (lambda ()
                   (variable-pitch-mode 1)
                   (face-remap-add-relative 'variable-pitch :background "#0b0e11")))
  :config
  (defun mistty-toggle()
    "Mistty toggle."
    (interactive)
    (if (string-prefix-p "*mistty" (buffer-name))
        (mistty-toggle-hide)
      (progn
        (mistty))))

  (defun mistty-toggle-hide ()
    "Hide buffer."
    (interactive)
    (if (window-deletable-p)
        (progn
          (mistty-send-string "exit\n")
          (kill-buffer)
          (delete-window))))

  (add-to-list 'display-buffer-alist
               '("\\*mistty"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package gh
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer 5
  :bind (("M-S" . git/body)
         ("M-s" . magit-status-toggle)
         (:map magit-status-mode-map
               ("q" . my-magit-quit-session))
         (:map git-commit-mode-map
               ("C-c C-t" . nil)
               ("M-p" . my-consult-git-commit-messages)
               ("M-i" . my-consult-git-conventional-commit-prefix)))
  :config
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-diff-refine-hunk nil)
  (setq magit-no-confirm
        '(discard stage-all-changes unstage-all-changes set-and-push))
  (setq git-commit-major-mode 'git-commit-elisp-text-mode)

  (defun my-magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window))

  (defun magit-status-toggle()
    "magit toggle."
    (interactive)
    (if (or (derived-mode-p 'magit-status-mode)
            (magit-status-toggle--get-window))
        (magit-status-toggle-hide)
      (magit-status-setup-buffer)
      (magit-status)))

  (defun magit-status-toggle-hide ()
    "Hide buffer."
    (interactive)
    (or (derived-mode-p 'magit-mode)
        (select-window (magit-status-toggle--get-window)))
    (if (window-deletable-p)
        (delete-window)))

  (defun magit-status-toggle--get-window()
    "Get the magit window which is visible (active or inactive)."
    (cl-find-if #'(lambda(w)
                    (provided-mode-derived-p
                     (buffer-local-value 'major-mode (window-buffer w))
                     'magit-mode))
                (window-list)))

  (defun my-magit-find-file-current ()
    (interactive)
    (let ((rev (consult--read
                (magit-list-refnames nil t)
                :require-match t
                :prompt "branch: "
                :sort nil)))
      (magit-find-file-other-window rev (expand-file-name (buffer-name)))))

  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname
                                      (equal major-mode 'magit-status-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.6)))

  (defun my-magit-git-push-advice (&rest _args)
    "Run `magit-process` after `magit-git-push`."
    (magit-process-buffer))

  ;; Add the advice to `magit-git-push`
  (advice-add 'magit-git-push :after #'my-magit-git-push-advice)

  (pretty-hydra-define
    git
    (:title (with-faicon "nf-fa-git" "Git commands" 1 -0.05) :quit-key "q")
    ("Magit"
     (("m" magit-status "status" :exit t)
      ("b" magit-blame "blame" :exit t)
      ("f" my-magit-find-file-current "view another branch" :exit t))
     "Timemachine"
     (("t" git-timemachine "timemachine" :exit t))
     "Gutter"
     (("p" diff-hl-previous-hunk "previous")
      ("n" diff-hl-next-hunk "next")
      ("s" diff-hl-stage-hunk "stage")
      ("r" diff-hl-revert-hunk "revert")
      ("SPC" diff-hl-show-hunk "toggle hunk"))
     "Link"
     (("l" git-link "link" :exit t)
      ("h" git-link-homepage "homepage" :exit t))
     "GH"
     (("v" my-gh-pr-view "view pr" :exit t)
      ("c" my-gh-pr-create "create pr" :exit t)
      ("o" my-git-open-pr-from-current-line "open pr from current line" :exit t))
     "Misc"
     (("w" my-git-wip "wip" :exit t)
      ("u" my-git-unwip "unwip" :exit t)))))

(use-package magit-delta
  :ensure t
  :defer t
  :hook (magit-mode . magit-delta-mode))

(use-package transient-posframe
  :ensure t
  :defer t
  :custom-face
  (transient-posframe ((t (:inherit tooltip))))
  (transient-posframe-border ((t (:inherit posframe-border :background "#0f0f14"))))
  :hook (after-init . transient-posframe-mode)
  :init
  (setq transient-posframe-border-width 10
        transient-posframe-min-height nil
        transient-posframe-min-width 80
        transient-posframe-poshandler nil
        transient-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8)
                                        (lines-truncate . t)))
  :config
  (defun my-transient-posframe--hide ()
    "Hide transient posframe."
    (posframe-hide transient--buffer-name))
  (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide))

(use-package diff-hl
  :ensure t
  :defer t
  :custom-face
  (diff-hl-change ((t (:background "#B4DCE7" :foreground "#B4DCE7"))))
  (diff-hl-insert ((t (:background "#74DFC4" :foreground "#74DFC4"))))
  (diff-hl-change ((t (:background "#964C7B" :foreground "#964C7B"))))
  :hook ((prog-mode . diff-hl-mode)
         (prog-mode . diff-hl-show-hunk-mouse-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  )

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-use-commit t))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook (lsp-completion-mode . (lambda ()
                                 (setq-local completion-at-point-functions
                                             (list (cape-capf-super
                                                    #'lsp-completion-at-point
                                                    #'tempel-expand)))))
  :bind ("C-c l" . lsp-code-actions-hydra/body)
  :init
  (defhydra lsp-code-actions-hydra (:color blue :hint nil)
    "
 ^Code Actions^        ^Refactoring^         ^Formatting^         ^Other^
 ^^-----------------   ^^------------------  ^^-----------------  ^^--------
 _a_: Execute action   _r_: Rename           _f_: Format buffer    _h_: Help
 _i_: Implementation   _o_: Organize imports _e_: Format eslint
"
    ;; コードアクション
    ("a" lsp-execute-code-action)
    ("i" lsp-find-implementation)
    ;; リファクタリング
    ("r" lsp-rename)
    ("o" lsp-organize-imports)
    ;; フォーマット
    ("f" lsp-format-buffer)
    ("e" lsp-eslint-fix-all)
    ;; その他
    ("h" lsp-describe-thing-at-point)
    ;; 終了
    ("q" nil :color blue))
  :config
  (setq lsp-completion-provider :none)
  (setq lsp-diagnostics-provider :flymake)
  (setq lsp-log-io nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive t)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-text-document-color t)
  (setq lsp-enable-snippet nil)

  (setq lsp-clients-typescript-prefer-use-project-ts-server t))

(use-package lsp-tailwindcss
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package imenu-list
  :ensure t
  :defer 5
  :bind
  ("M-i" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :config
  (setopt imenu-list-focus-after-activation t)
  (setopt imenu-list-auto-resize nil))

(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  ;; https://genehack.blog/2024/02/fixing-an-emacs-typescript-ts-mode-problem/
  (defvar tsx-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'tsx
     :ts-mode 'tsx-ts-mode
     :remap '(typescript-tsx-mode)
     :requires 'typescript
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "tsx/src"
     :ext "\\.tsx\\'")
    "Recipe for libtree-sitter-tsx.dylib")
  (add-to-list 'treesit-auto-recipe-list tsx-treesit-auto-recipe)

  (defvar typescript-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'typescript
     :ts-mode 'typescript-ts-mode
     :remap 'typescript-mode
     :requires 'tsx
     :url "https://github.com/tree-sitter/tree-sitter-typescript"
     :revision "v0.20.3"
     :source-dir "typescript/src"
     :ext "\\.ts\\'")
    "Recipe for libtree-sitter-typescript.dylib")
  (add-to-list 'treesit-auto-recipe-list typescript-treesit-auto-recipe)

  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode)
  :bind (:map treesit-fold-mode-map
              ("C-<return>" . treesit-fold-toggle)
              ("C-M-<return>" . treesit-fold-close-all)))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("M-a" . smerge-hydra/body))
  :init
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
    ^Move^       ^Keep^               ^Diff^                 ^Other^
    ^^-----------^^-------------------^^---------------------^^-------
    _n_ext       _b_ase               _<_: upper/base        _C_ombine
    _p_rev       _u_pper              _=_: upper/lower       _r_esolve
    ^^           _l_ower              _>_: base/lower        _k_ill current
    ^^           _a_ll                _R_efine
    ^^           _RET_: current       _E_diff
    "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

(use-package restclient :ensure t :defer t)

;; formatter
(use-package apheleia
  :ensure t
  :defer t
  :hook ((prog-mode text-mode) .
         (lambda ()
           (when-let* ((p-root (car (last (project-current))))
                       (file-name (buffer-file-name))
                       (ext (file-name-extension file-name)))
             (when
                 (or
                  (file-exists-p (concat p-root "deno.json"))
                  (file-exists-p (concat p-root "deno.jsonc")))
               (cond
                ((member ext '("js" "jsx" "ts" "tsx"))
                 (setq-local apheleia-mode-alist '((prog-mode . denofmt))))
                ((member ext '("md" "mkd" "mkdn" "mdwn" "mdown" "markdown"))
                 (setq-local apheleia-mode-alist '((text-mode . denofmt-md))))
                ((member ext '("json" "jsonc"))
                 (setq-local apheleia-mode-alist '((json-ts-mode . denofmt-json))))
                ((member ext '("css" "scss" "sass" "less"))
                 (setq-local apheleia-mode-alist '((prog-mode . denofmt-css))))
                ((member ext '("html" "svelte" "vue" "astro" "vto" "njk"))
                 (setq-local apheleia-mode-alist '((prog-mode . denofmt-html))))
                ((member ext '("yml" "yaml"))
                 (setq-local apheleia-mode-alist '((prog-mode . denofmt-yaml))))
                ((string= ext "sql")
                 (setq-local apheleia-mode-alist '((prog-mode . denofmt-sql)))
                 )))
             (when
                 (or
                  (file-exists-p (concat p-root "dprint.json"))
                  (file-exists-p (concat p-root "dprint.jsonc")))
               (setq-local apheleia-mode-alist '((prog-mode . dprint))))

             (apheleia-mode))))
  :config
  (add-to-list 'apheleia-formatters '(denofmt-html . ("deno" "fmt" "-" "--ext" "html" "--unstable-component")))
  (add-to-list 'apheleia-formatters '(denofmt-css . ("deno" "fmt" "-" "--ext" "css")))
  (add-to-list 'apheleia-formatters '(denofmt-sql . ("deno" "fmt" "-" "--ext" "sql" "--unstable-sql"))))

;;; ---------- major mode ----------
(use-package elisp-mode
  :defer t
  :config
  (setq-default tab-width 2)
  :hook
  ((emacs-lisp-mode
    . (lambda ()
        (major-mode-hydra-define emacs-lisp-mode
          (:quit-key "q" :title (with-sucicon "nf-custom-emacs" "Emacs Lisp"))
          ("Eval"
           (("b" eval-buffer "buffer")
            ("e" eval-defun "defun")
            ("r" eval-region "region"))
           "REPL"
           (("I" ielm "ielm"))
           "Test"
           (("t" ert "prompt")
            ("T" (ert t)
             "all")
            ("F" (ert :failed) "failed"))
           "Doc"
           (("d" describe-foo-at-point "thing-at-pt")
            ("f" describe-function "function")
            ("v" describe-variable "variable")
            ("i" info-lookup-symbol "info lookup"))
           "Macrostep"
           (("m" macrostep-expand "macrostep-expand"))))))))

(use-package macrostep
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.p?html\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.njk" "\\.tt" "\\.vento" "\\.vto")
  :bind (:map web-mode-map ("C-c C-t" . gt-translate) ("C-c C-a" . nil))
  :init
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (define-derived-mode astro-mode web-mode "astro")
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode))
  :hook ((vue-mode .
                   (lambda ()
                     (add-node-modules-path)
                     (lsp-deferred)))
         (astro-mode .
                     (lambda ()
                       (add-node-modules-path)
                       (lsp-deferred))))
  :config
  (setq web-mode-engines-alist
        '(("django"    .  "\\.vento")
          ("django"    .  "\\.vto")))
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-comment-formats '(("javascript" . "//")
                                   ("jsx" .  "//")
                                   ("php" . "/*")))
  (setq web-mode-enable-front-matter-block t) ;ignore Front Matter Data

  (major-mode-hydra-define web-mode
    (:quit-key "q" :title (with-sucicon "nf-seti-html" "Web mode"))
    ("Navigation"
     (("m" web-mode-navigate "navigate" :color red)
      ("g" web-mode-element-beginning "beginning" :color red)
      (";" web-mode-element-end "end" :color red)
      ("j" web-mode-element-next "next" :color red)
      ("k" web-mode-element-previous "previous" :color red)
      ("l" web-mode-element-child "child" :color red)
      ("h" web-mode-element-parent "parent" :color red))
     "Editing"
     (("w" web-mode-element-wrap "wrap")
      ("c" web-mode-element-clone "clone")
      ("i" web-mode-element-insert "insert")
      ("r" web-mode-element-rename "rename")
      ("s" web-mode-element-select "select")
      ("v" web-mode-element-vanish "vanish"))
     )))

(use-package emmet-mode
  :ensure t
  :defer t
  :bind (:map emmet-mode-keymap ("C-j" . completion-at-point))
  :hook ((html-mode
          web-mode
          css-mode
          scss-mode) . emmet-mode))

(use-package slim-mode
  :ensure t
  :defer t)

(use-package css-mode
  :defer t
  :hook ((css-mode scss-mode) . lsp-deferred)
  :config
  (setq css-indent-offset 2))

(use-package sass-mode
  :ensure t
  :defer t)

(use-package sws-mode
  :ensure t
  :defer t)

(use-package js
  :defer t
  :mode ("\\.[mc]?js$" . js-ts-mode)
  :hook
  (js-ts-mode . lsp-deferred)
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2))

(use-package typescript-ts-mode
  :defer t
  :mode (("\\.m?ts$" . typescript-ts-mode)
         ("\\.tsx$" . tsx-ts-mode))
  :hook
  (typescript-ts-base-mode . lsp-deferred)
  :config
  (major-mode-hydra-define typescript-ts-mode
    (:quit-key "q" :title (with-sucicon "nf-seti-typescript" "TypeScript"))
    ("REPL"
     (("n" nodejs-repl "node")
      ("t" run-ts "ts-node"))
     "Test"
     (("jc" my-jest-copy-command-watch-current-buffer)
      ("vc" my-vitest-copy-command-watch-current-buffer)
      ("vt" my-vitest-command-watch-tmux)))))

(use-package json-ts-mode
  :defer t
  :mode ("\\.jsonc\\'"))

(use-package yaml-ts-mode
  :defer t
  :mode ("\\.ya?ml\\'")
  :hook (yaml-ts-mode . lsp-deferred))

(use-package jq-mode
  :ensure t
  :defer t)

(use-package add-node-modules-path
  :ensure t
  :defer t
  :hook
  ((typescript-ts-base-mode
    js-ts-mode
    scss-mode
    graphql-mode
    ts-comint-mode
    json-ts-mode) . #'add-node-modules-path)
  :config
  ;; https://github.com/codesuki/add-node-modules-path/issues/23#issuecomment-1312961383
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\"")))

(use-package nodejs-repl
  :ensure t
  :defer t)

(use-package ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb\\'" "Capfile$" "Gemfile$" "[Rr]akefile$")
  :hook
  (ruby-mode . lsp-deferred)
  (ruby-mode . inf-ruby-minor-mode)
  (ruby-mode . inf-ruby-switch-setup)
  :interpreter ("pry")
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package inf-ruby
  :ensure t
  :defer t
  :bind
  (:map inf-ruby-minor-mode-map
        ("C-c C-b" . ruby-send-buffer)
        ("C-c C-l" . ruby-send-line))
  :init
  (defalias 'pry 'inf-ruby)
  :config
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding"))

(use-package rubocop
  :ensure t
  :defer t)

(use-package rspec-mode
  :ensure t
  :defer t
  :bind
  (:map rspec-mode-map
        ("C-c C-c C-c" . rspec-verify-single)))

(use-package php-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package swift-mode
  :ensure t
  :defer t)

(use-package dart-mode
  :ensure t
  :defer t
  :config
  (setq dart-format-on-save nil)
  (setq dart-enable-analysis-server nil)
  (setq dart-sdk-path "~/repos/github.com/flutter/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :ensure t
  :defer t
  :config
  (setq flutter-sdk-path "~/repos/github.com/flutter/flutter/"))

(use-package go-mode
  :ensure t
  :defer t
  :hook (go-mode . lsp-deferred))

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package java-ts-mode
  :defer t
  :mode (".gradle$"))

(use-package kotlin-mode
  :ensure t
  :defer t)

(use-package groovy-mode
  :ensure t
  :defer t)

(use-package scala-mode
  :ensure t
  :defer t
  :interpreter ("scala"))

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-format-on-save t)

  (major-mode-hydra-define rust-mode
    (:quit-key "q" :title (concat (nerd-icons-devicon "nf-dev-rust") " " "Rust"))
    ("Build/Run"
     (("b" cargo-process-build "build")
      ("l" cargo-process-clean "clean")
      ("i" cargo-process-init "init")
      ("r" cargo-process-run "run")
      ("c" my-cargo-process-build-and-run-current-bin "run current bin")
      ("u" cargo-process-update "update"))
     "Test"
     (("t" my-cargo-process-build-and-test "build and test")
      ("f" cargo-process-current-test "current")
      ("o" cargo-process-current-file-tests "file"))
     "Lint/Format"
     (("k" cargo-process-check "check")
      ("q" cargo-process-clippy "clippy")
      ("<RET>" cargo-process-fmt "fmt"))
     "Doc"
     (("d" cargo-process-doc "doc")))))

(use-package cargo
  :ensure t
  :defer t
  :bind (:map cargo-mode-map
              ("C-c C-c C-c" . my-cargo-process-build-and-test))
  :hook (rust-mode . cargo-minor-mode)
  :config
  (defun my-cargo-process-build-and-test ()
    (interactive)
    (cargo-process-build)
    (cargo-process-current-file-tests))

  (defun my-cargo-process-run-bin-current-buffer ()
    (interactive)
    (let ((command (file-name-sans-extension (buffer-name))))
      (cargo-process--start (concat "Run " command)
                            (concat cargo-process--command-run-bin " " command))))

  (defun my-cargo-process-build-and-run-current-bin ()
    (interactive)
    (cargo-process-build)
    (my-cargo-process-run-bin-current-buffer)))

(use-package csharp-mode
  :defer t
  :hook
  (csharp-ts-mode . lsp-deferred))

(use-package sql
  :defer t
  :hook
  (sql-mode . sql-interactive-mode)
  (sql-interactive-mode
   . (lambda ()
       (buffer-face-set 'variable-pitch)
       (toggle-truncate-lines t)))
  :mode (".sql$")
  :config
  (defun my-sql-indent-region (beg end)
    "Indent the SQL statement in the BEG to END (region)."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (sql-indent-buffer)))))

(use-package sqlup-mode
  :ensure t
  :defer t)

(use-package sqlformat
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package nginx-mode
  :ensure t
  :defer t)

(use-package fish-mode
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t
  :defer t)

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package org
  :pin gnu
  :defer t
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-j" . nil)
              ("C-c C-t" . nil)
              ("C-c C-k" . nil)
              ("C-c C-a" . nil)
              ("M-{" . nil)
              ("M-}" . nil))
  :hook (org-mode . (lambda ()
                      (require 'ob-js)
                      (require 'ob-async)
                      (setq-local completion-at-point-functions
                                  (list (cape-capf-super
                                         #'cape-elisp-block
                                         #'tempel-complete)))))
  :config
  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)
  (setq org-log-done 'time)
  (setq org-confirm-babel-evaluate nil)
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                         "[:space:]"
                                         "."
                                         1))
  (setq org-match-substring-regexp
        (concat
         "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
         "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)

  (setq org-src-lang-modes '(("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("emacs-lisp" . emacs-lisp)
                             ("shell" . sh)
                             ("bash" . sh)
                             ("fish" . fish)
                             ("typescript" . typescript-ts)
                             ("ts" . typescript-ts)
                             ("tsx" . tsx-ts)
                             ("html" . web)
                             ("vue" . vue)
                             ("javascript" . js)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (sql . t)
     (emacs-lisp . t)
     (shell . t)
     (js . t)
     (org . t)
     (ruby . t)
     (restclient . t)
     (typescript . t)))

  (setq org-file-apps
        (append '(("\\.png\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file))))
                  ("\\.jpg\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file))))
                  ("\\.gif\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file))))))))

(use-package ox-latex
  :defer t
  :config
  (setq org-latex-pdf-process '("latexmk %f"))
  (setq org-file-apps '(("pdf" "/usr/bin/open -a Preview.app %s")))
  (setq org-latex-with-hyperref nil)
  (setq org-latex-hyperref-template nil))

(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-gfm
  :ensure t
  :defer t
  :hook (org-mode . (lambda ()
                      (require 'ox-gfm nil t)
                      (add-to-list 'org-export-backends 'gfm))))

(use-package org-modern
  :ensure t
  :defer t
  :hook ((org-mode . org-modern-mode))
  :config
  (setq org-modern-fold-stars
        '(("▶" . "▼") ("▷" . "▽") ("⏷" . "⏵") ("▹" . "▿") ("▸" . "▾"))))

(use-package ob-restclient
  :ensure t
  :defer t)

(use-package ob-async
  :ensure t
  :defer t)

(use-package ob-typescript
  :ensure t
  :defer t)

(use-package mermaid-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :hook
  (markdown-mode
   . (lambda nil
       (set
        (make-local-variable 'whitespace-action)
        nil)))
  :bind (:map markdown-mode-map (("C-c C-t" . nil)
                                 ("C-c C-a" . nil)))
  :mode
  ("\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.mdown\\'" . gfm-mode)
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-content-type "application/xhtml+xml")
  (setq markdown-command "multimarkdown")
  (setq markdown-gfm-additional-languages '("Mermaid"))
  (setq markdown-css-paths
        '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown-light.min.css"
          "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/default.min.css"))
  (setq markdown-live-preview-window-function 'markdown-live-preview-window-xwidget)
  (setq markdown-xhtml-body-preamble "<article class='markdown-body'>")
  (setq markdown-xhtml-body-epilogue "</article>")
  (setq markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body { max-width: 740px; padding: 1em 2em; }
.markdown-body { margin: 0 auto; }
</style>
<script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre code').forEach((code) => {
    if (code.className != 'mermaid') {
      hljs.highlightBlock(code);
    }
  });
});
</script>
<script src='https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js'></script>
<script>mermaid.initialize({ startOnLoad: true });</script>")

  (defun markdown-live-preview-window-xwidget (file)
    "Preview file with xwidget browser"
    (xwidget-webkit-browse-url (concat "file://" file))
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (pop-to-buffer buf)))))

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable))

(setq gc-cons-threshold 1073741824)

(when my-enable-measuring
  (profiler-report)
  (profiler-stop))

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
