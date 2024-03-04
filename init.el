;;; init.el --- My emacs settings. -*- coding: utf-8; lexical-binding: t -*-
;; Author: Hayato Kajiyama <https://github.com/hyakt/.emacs.d>

;;; Commentary:
;; This is my init.el of Emacs.

;;; Code:
(defconst my-enable-measuring nil
  "If non-nil, mesure start up time.")
;;
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
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (package-refresh-contents)
  ;; TODO: Remove when https://github.com/slotThe/vc-use-package#installation
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package")))

;; 読み込まないと init.elc の場合にエラーになる
(require 'bind-key)

;;; ---------- basic ----------
(setq user-full-name "Hayato Kajiyama")
(setq user-mail-address "me@piginbeer.com")

(setq native-comp-async-report-warnings-errors 'nil)
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
(setq scroll-conservatively 35)                                    ;; スクロールの設定
(setq scroll-margin 0)                                             ;; スクロールの設定
(setq tab-width 2)                                                 ;; タブの幅は半角スペース 2
(setq truncate-lines nil)                                          ;; 画面端まで来たら折り返す
(setq truncate-partial-width-windows nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)     ;; 同じ名前のバッファを開いたときの設定
(setq split-height-threshold 120)
(setq split-width-threshold 200)
(setq auto-coding-functions nil)                                   ;; 文字コードの自動変換保存をしない
(setq completion-ignore-case t)                                    ;; file 名の補完で大文字小文字を区別しない
(setq auto-save-default nil)                                       ;; オートセーブのファイルを作らない
(setq create-lockfiles nil)                                        ;; ロックファイル(.#filename)のファイルを作らない
(setq garbage-collection-message nil)                              ;; GC 実行のメッセージを表示しない
(setq message-log-max 10000)                                       ;; ログの記録行数を増やす
(setq vc-follow-symlinks t)                                        ;; symlink は必ず追いかける
(setq enable-local-variables :all)                                 ;; local variable は全て使用する
(setq init-file-debug t)
(setq frame-resize-pixelwise t)
(setq history-length 3000)
(setq history-delete-duplicates t)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 100)
(setq custom-file "~/.emacs.d/custom.el")
(setq initial-scratch-message "")                                  ;; scratch の初期メッセージ消去
(setq initial-major-mode 'fundamental-mode)
(setq package-install-upgrade-built-in t)
(setq use-short-answers t)

(setq-default indent-tabs-mode nil)                                ;; タブの変わりに半角スペースを使う
(setq-default shell-file-name "/bin/bash")

(load (locate-user-emacs-file "./lisp/functions/my-functions-autoloads.el") nil t)

(with-deferred-eval
  (if (file-exists-p (expand-file-name custom-file))
      (load-file (expand-file-name custom-file)))
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

  (defun frame-size-resume ()
    "Load the saved frame size."
    (let ((file "~/.emacs.d/.framesize"))
      (if (file-exists-p file) (load-file file))))

  (frame-size-resume)
  (add-hook 'kill-emacs-hook 'frame-size-save))

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
  (setq explicit-shell-file-name "/bin/bash"))

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
  (savehist-mode t))

(use-package mac-win
  :defer 1
  :if (eq system-type 'darwin)
  :config
  (mac-auto-ascii-mode t))

(use-package autorevert
  :defer 1
  :config
  (global-auto-revert-mode))

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
  (setq gcmh-verbose t)
  (defvar my-gcmh-status nil)
  (advice-add #'garbage-collect
              :before
              (defun my-gcmh-log-start (&rest _)
                (when gcmh-verbose
                  (setq my-gcmh-status "Running GC..."))))
  (advice-add #'gcmh-message
              :override
              (defun my-gcmh-message (format-string &rest args)
                (setq my-gcmh-status
                      (apply #'format-message format-string args))
                (run-with-timer 2 nil
                                (lambda ()
                                  (setq my-gcmh-status nil)))))
  (gcmh-mode t))

;;; ---------- appearance ----------
(set-face-attribute 'default nil
                    :family "Source Han Code JP"
                    :height 110)
(set-face-attribute 'variable-pitch nil
                    :family "Myrica M"
                    :height 120)
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP"))

(with-deferred-eval
  (when-macos
   (defun mac-selected-keyboard-input-source-change-hook-func ()
     ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
     (set-cursor-color (if (or
                            (string-match "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" (mac-input-source))
                            (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                            (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                           "#FF5996" "#51AFEF")))
   (add-hook 'mac-selected-keyboard-input-source-change-hook 'mac-selected-keyboard-input-source-change-hook-func))

  (global-font-lock-mode)
  (transient-mark-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (show-paren-mode t)
  (set-scroll-bar-mode nil))

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
         ("C-h" . puni-force-delete)
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

(use-package direnv
  :ensure t
  :defer 1
  :config
  (direnv-mode))

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

  (keymap-global-set "C-h" #'delete-backward-char)
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
  (keymap-global-set "M-t" #'my-open-alacritty-tmux-current-buffer)
  (keymap-global-unset "C-z"))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode)
  :config
  (defvar web-electric-pairs '((?< . ?>) (?' . ?') (?` . ?`)) "Electric pairs for web-mode.")
  (defun web-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun my-inhibit-electric-pair-mode (char)
    (minibufferp))

  (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

  (add-hook 'web-mode-hook #'web-add-electric-pairs)
  (add-hook 'typescript-ts-base-mode-hook #'web-add-electric-pairs))

(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

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

(use-package flymake-eslint
  :ensure t
  :after flymake
  :init
  ;; Need npm i -g eslint
  (setq flymake-eslint-project-root (file-name-directory (shell-command-to-string "npm root")))
  (defun enable-flymake-eslint-without-eglot ()
    (setq-local eglot-stay-out-of '(flymake))
    (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
    (ignore-errors (flymake-eslint-enable))))

(use-package beacon
  :ensure t
  :defer 5
  :config
  (beacon-mode t))

(use-package volatile-highlights
  :ensure t
  :defer 5
  :config
  (volatile-highlights-mode t))

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

;; for some lsp
(use-package yasnippet
  :ensure t
  :defer t)

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
  (("M-." . my-goto-address-or-smart-jump)
   ("M-," . smart-jump-back)
   ("M-'" . smart-jump-references))
  :config
  (defun my-goto-address-or-smart-jump ()
    (interactive)
    (let ((url (thing-at-point 'url)))
      (if url
          (browse-url url)
        (smart-jump-go))))

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
          my-goto-address-or-smart-jump smart-jump-go smart-jump-ref
          xref-find-definitions xref-find-references
          dump-jump-go
          vr/query-replace
          my-jump-to-match-parens
          consult-line consult-ripgrep consult-find consult-ghq-find consult-fd consult-flymake
          er/expand-region
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

(use-package yafolding
  :ensure t
  :defer t
  :hook (prog-mode . yafolding-mode))

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

(use-package expand-region
  :ensure t
  :defer t
  :bind
  ("C-," . er/expand-region)
  ("C-M-," . er/contract-region))

(use-package undo-fu
  :ensure t
  :defer t
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-enable-key "e")
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

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

(use-package unicode-escape
  :ensure t
  :defer 5
  :commands (unicode-escape-region unicode-unescape-region))

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

(use-package copilot.el
  :vc (:fetcher github :repo copilot-emacs/copilot.el)
  :hook ((prog-mode
          . (lambda ()
              (when (and buffer-file-name
                         (file-readable-p buffer-file-name)
                         (< (nth 7 (file-attributes buffer-file-name)) 100000))
                (copilot-mode t))))
         (eshell-mode . copilot-mode))
  :bind (("<tab>" . copilot-accept-completion)
         ("M-P" . copilot-next-completion)
         ("M-N" . copilot-previous-completion))
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 100000))

(use-package go-translate
  :defer t
  :ensure t
  :bind ("C-c C-l" . gts-do-translate)
  :init
  (defcustom gts-deepl-key ""
    "DeepL API key"
    :type 'string
    :group 'go-translate)
  :config
  (setq go-translate-buffer-follow-p t)
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-posframe-pop-render-timeout nil)

  (setq gts-default-translator
        (gts-translator
         :picker
         (gts-noprompt-picker :texter (gts-current-or-selection-texter))
         :engines
         (list
          (gts-deepl-engine :auth-key gts-deepl-key :pro nil)
          (gts-google-engine)
          )
         :render
         (gts-posframe-pop-render))))

(use-package google-this
  :bind ("C-c C-k" . google-this-noconfirm)
  :ensure t
  :defer t)

(use-package vlf
  :ensure t
  :defer t)

;;; ---------- interface ----------
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
  :defer 5
  :config
  (which-key-mode t))

(use-package swap-buffers
  :ensure t
  :defer t
  :bind ("C-x C-o" . swap-buffers))

(use-package other-window-or-split
  :vc (:fetcher github :repo conao3/other-window-or-split)
  :bind
  ("C-t" . my-ws-other-window-or-split-and-kill-minibuffer)
  ("C-S-t" . ws-previous-other-window-or-split)
  :config
  (setq ws-split-window-width-with-em 130)
  (defun minibuffer-keyboard-quit () ;; esc quits
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (defun my-ws-other-window-or-split-and-kill-minibuffer ()
    (interactive)
    (if (active-minibuffer-window)
        (progn
          (minibuffer-keyboard-quit)
          (ws-other-window-or-split))
      (ws-other-window-or-split))))

(use-package project
  :init
  (setq project-vc-extra-root-markers '("package.json")))

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

(use-package eyebrowse
  :ensure t
  :defer 5
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-z"))
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-mode t))

(use-package eldoc
  :defer t
  :config
  (defvar eldoc-buffer-name "*ElDoc*")
  (setq eldoc-idle-delay 0.75)

  (defun eldoc-buffer-message (format-string &rest args)
    "Display messages in the mode-line when in the ElDoc buffer."
    (when (and (stringp format-string) (not (equal format-string "")))
      (display-buffer
       (with-current-buffer (get-buffer-create eldoc-buffer-name)
         (view-mode -1)
         (erase-buffer)
         (insert (apply #'format format-string args))
         (goto-char (point-min))
         (setq-local kill-buffer-hook 'delete-window)
         (view-mode t)
         (current-buffer))
       )))

  (defun my-switch-eldoc-display-mode ()
    (interactive)
    "Switch eldoc mode between minibuffer and buffer."
    (if (eq eldoc-message-function #'eldoc-buffer-message)
        (progn
          (setq eldoc-message-function #'eldoc-minibuffer-message)
          (message "minibuffer mode")
          (when-let
              ((eldoc-window
                (cl-find-if
                 (lambda (win)
                   (string-match eldoc-buffer-name (buffer-name (window-buffer win))))
                 (window-list))))
            (and
             (select-window eldoc-window)
             (window-deletable-p)
             (delete-window))))
      (progn
        (setq eldoc-message-function #'eldoc-buffer-message)
        (message "buffer mode")))))

(use-package dired
  :defer t
  :bind (("C-x C-d" . my-dired-this-buffer)
         :map dired-mode-map
         ("e" . wdired-change-to-wdired-mode)
         ("C-t" . nil)
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
    (s-concat (nerd-icons-sucicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(use-package dired-sidebar
  :ensure t
  :defer t
  :bind
  (("M-d" . dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
   ("o" . dired-sidebar-subtree-toggle)
   ("q" . kill-this-buffer))
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-modeline nil))

(use-package consult
  :ensure t
  :defer t
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x C-b" . consult-buffer)
         ("C-x f" . consult-fd)
         ("C-x e" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
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
  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --ignore-case -e ARG OPTS")

  ;; https://github.com/minad/consult/issues/837#issuecomment-1703762384
  (consult-customize
   consult-find
   consult-fd
   :state (consult--file-preview))

  (consult-customize
   find-file
   consult-fd
   consult-ripgrep
   consult-recent-file
   consult--source-buffer
   consult-ls-git
   :preview-key
   '("C-." :debounce 0.5 any)))

(use-package consult-ghq
  :ensure t
  :defer t
  :bind ("C-x C-g" . consult-ghq-find)
  :config
  (setq consult-ghq-find-function 'find-file))

(use-package consult-ls-git
  :ensure t
  :defer t
  :bind ("C-x g" . consult-ls-git))

(use-package vertico
  :ensure t
  :defer 1
  :bind (:map vertico-map
              ("C-l" . vertico-directory-up))
  :config
  (setq vertico-count 30)
  (vertico-mode t))

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
  ("M-q" . embark-act)
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
  (defalias 'e 'find-file-other-window)
  )

(use-package shell-pop
  :defer t
  :ensure t
  :bind (("M-e" . shell-pop))
  :init
  (setq
   shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
   shell-pop-term-shell "eshell"
   shell-pop-window-size 50
   shell-pop-full-span nil
   shell-pop-window-position "bottom"
   shell-pop-autocd-to-working-dir t
   shell-pop-restore-window-configuration t
   shell-pop-cleanup-buffer-at-process-exit t))

(use-package fish-completion
  :defer t
  :ensure t
  :hook (eshell-mode . fish-completion-mode))

(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

(use-package eshell-syntax-highlighting
  :defer t
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell-z
  :ensure t
  :defer t
  :hook (eshell-mode . (lambda () (require 'eshell-z)))
  :config
  (setq eshell-z-freq-dir-hash-table-file-name "~/.local/share/z/data"))

(use-package eat
  :ensure t
  :defer t
  :hook (eshell-mode . eat-eshell-mode))
(use-package consult-tramp
  :vc (:fetcher github :repo Ladicle/consult-tramp)
  :defer t)

(use-package gh
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer 5
  :bind (("M-S" . git/body)
         ("M-s" . magit-status-toggle)
         (:map magit-status-mode-map
               ("q" . my-magit-quit-session)
               ("C-o" . magit-diff-visit-file-other-window))
         (:map git-commit-mode-map
               ("M-i" . my-consult-git-commit-messages)
               ("M-p" . my-consult-git-conventional-commit-prefix)))
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
    "Hide the magit-statsu buffer."
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
     (("p" git-gutter:previous-hunk "previous")
      ("n" git-gutter:next-hunk "next")
      ("s" git-gutter:stage-hunk "stage")
      ("r" git-gutter:revert-hunk "revert")
      ("SPC" my-git-gutter:toggle-popup-hunk "toggle hunk"))
     "Link"
     (("l" git-link "link" :exit t)
      ("h" git-link-homepage "homepage" :exit t))
     "GH"
     (("v" my-gh-pr-view "view pr" :exit t)
      ("c" my-gh-pr-create "create pr" :exit t)
      ("o" my-git-open-pr-from-current-line "open pr from current line" :exit t))
     "Misc"
     (("w" my-git-wip "wip" :exit t)))))

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
  :hook (magit-mode . transient-posframe-mode)
  :init
  (setq transient-posframe-border-width 3
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

(use-package git-gutter
  :ensure t
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:ask-p nil)

  (defun my-git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter hunk window."
    (interactive)
    (if (and (get-buffer git-gutter:popup-buffer) (git-gutter:popup-buffer-window))
        (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))

  (set-face-background 'git-gutter:modified "#B4DCE7")
  (set-face-background 'git-gutter:added "#74DFC4")
  (set-face-background 'git-gutter:deleted "#964C7B")
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign    " ")
  (setq git-gutter:deleted-sign  " ")

  (add-to-list 'git-gutter:update-commands #'my-revert-buffer-no-confirm)
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-use-commit t))

(use-package eglot
  :defer t
  :commands (eglot-ensure)
  :hook (eglot--managed-mode . (lambda ()
                                 (setq-local completion-at-point-functions
                                             (list (cape-capf-super
                                                    #'eglot-completion-at-point
                                                    #'tempel-expand)))))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t)
  ;; https://github.com/joaotavora/eglot/issues/43#issuecomment-1132605973
  (setq eglot-events-buffer-size 0)

  (defun advice-eglot--xref-make-match (old-fn name uri range)
    (cond
     ((string-prefix-p "deno:/" uri)
      (let ((contents (jsonrpc-request (eglot--current-server-or-lose)
                                       :deno/virtualTextDocument
                                       (list :textDocument (list :uri uri))))
            (filepath (concat (temporary-file-directory)
                              (replace-regexp-in-string "^deno:/\\(.*\\)$" "\\1" (url-unhex-string uri)))))
        (unless (file-exists-p filepath)
          (make-empty-file filepath 't)
          (write-region contents nil filepath nil 'silent nil nil))
        (apply old-fn (list name filepath range))))
     (t
      (apply old-fn (list name uri range)))))

  (advice-add 'eglot--xref-make-match :around #'advice-eglot--xref-make-match)

  ;; https://github.com/joaotavora/eglot/discussions/999
  (defun es-server-program (_)
    "Decide which server to use for ECMA Script based on project characteristics."
    (cond ((my-deno-project-p) '("deno" "lsp" :initializationOptions (:enable t :lint t :unstable t)))
          ((my-node-project-p) '("typescript-language-server" "--stdio"))
          (t                nil)))

  (add-to-list 'eglot-server-programs '(((js-ts-mode :language-id "javascript")
                                         (typescript-ts-mode :language-id "typescript")
                                         (tsx-ts-mode :language-id "typescriptreact")) . es-server-program))

  ;; npm i -g @vue/language-server
  (add-to-list 'eglot-server-programs '(vue-mode . ("vue-language-server" "--stdio"
                                                    :initializationOptions
                                                    (:typescript
                                                     (:tsdk "node_modules/typescript/lib")
                                                     :serverMode 0
                                                     :diagnosticModel 1
                                                     :textDocumentSync 2))))

  ;; npm install -g vscode-langservers-extracted
  (add-to-list 'eglot-server-programs '((html-mode) . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((css-mode scss-mode) . ("vscode-css-language-server" "--stdio"))))

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
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil))

(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

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

;;; ---------- major mode ----------
(with-deferred-eval
  (defun my-deno-project-p ()
    "Predicate for determining if the open project is a Deno one."
    (let ((p-root (car (last (project-current)))))
      (or
       (file-exists-p (concat p-root "deno.json"))
       (file-exists-p (concat p-root "deno.jsonc")))))

  (defun my-node-project-p ()
    "Predicate for determining if the open project is a Node one."
    (let ((p-root (file-name-directory (shell-command-to-string "npm root"))))
      (file-exists-p (concat p-root "package.json")))))

(use-package emacs-lisp-mode
  :defer t
  :config
  (setq-default tab-width 2)
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
     (("m" macrostep-expand "macrostep-expand")))))

(use-package macrostep
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.p?html\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.astro" "\\.njk")
  :bind (:map web-mode-map ("C-c C-l" . nil))
  :init
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  :hook ((vue-mode . (lambda ()
                       (message "called-web-mode-vuehook")
                       (add-node-modules-path)
                       (yas-minor-mode t)
                       (eglot-ensure)
                       (enable-flymake-eslint-without-eglot))))
  :config
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
     "Format"
     (("p" prettier-js)
      ("d" deno-fmt))
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
  :hook ((css-mode scss-mode) . eglot-ensure)
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
  ((js-ts-mode . (lambda ()
                   (eglot-ensure)
                   (when (my-node-project-p)
                     (enable-flymake-eslint-without-eglot))))
   (js-ts-mode . subword-mode))
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2))

(use-package typescript-ts-mode
  :defer t
  :mode (("\\.m?ts$" . typescript-ts-mode)
         ("\\.tsx$" . tsx-ts-mode))
  :hook
  (typescript-ts-base-mode . (lambda ()
                               (eglot-ensure)
                               (when (my-node-project-p)
                                 (enable-flymake-eslint-without-eglot))))
  (typescript-ts-base-mode . subword-mode)
  :config
  (major-mode-hydra-define typescript-ts-mode
    (:quit-key "q" :title (with-sucicon "nf-seti-typescript" "TypeScript"))
    ("REPL"
     (("n" nodejs-repl "node")
      ("t" run-ts "ts-node"))
     "Test"
     (("jc" my-jest-copy-command-watch-current-buffer)
      ("vc" my-vitest-copy-command-watch-current-buffer)
      ("vt" my-vitest-command-watch-tmux))
     "Format"
     (("p" prettier-js)
      ("d" deno-fmt)))))

(use-package json-mode
  :ensure t
  :defer t
  :mode
  ("\\.jsonc\\'" . json-mode))

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
    json-mode) . #'add-node-modules-path)
  :config
  ;; https://github.com/codesuki/add-node-modules-path/issues/23#issuecomment-1312961383
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\"")))

(use-package nodejs-repl
  :ensure t
  :defer t)

(use-package prettier-js
  :ensure t
  :defer t
  :hook
  (((typescript-ts-base-mode
     web-mode
     js-ts-mode
     json-mode) .
     (lambda ()
       (when (my-node-project-p) (prettier-js-mode))))
   html-mode
   css-mode
   scss-mode
   graphql-mode)
  :config
  (setq prettier-js-show-errors nil))

(use-package eslintd-fix
  :ensure t
  :defer t)

(use-package deno-fmt
  :ensure t
  :defer t
  :hook
  ((typescript-ts-base-mode
    js-ts-mode
    json-mode) .
    (lambda ()
      (when (my-deno-project-p) (deno-fmt-mode)))))

(use-package ruby-mode
  :ensure t
  :defer t
  :mode ("\\.rb\\'" "Capfile$" "Gemfile$" "[Rr]akefile$")
  :hook
  (ruby-mode . eglot-ensure)
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
  :hook (go-mode . eglot-ensure))

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package java-ts-mode
  :defer t
  :mode (".gradle$"))

(use-package eglot-java
  :defer t
  :hook (java-ts-mode . eglot-java-mode))

(use-package scala-mode
  :ensure t
  :defer t
  :interpreter ("scala"))

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . eglot-ensure)
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

(use-package csharp-ts-mode
  :defer t
  :hook
  (csharp-ts-mode . eglot-ensure))

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

(use-package docker-compose-mode
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
  :defer t
  :bind (:map org-mode-map
              ("C-," . nil)
              ("C-j" . nil)
              ("C-c C-l" . nil)
              ("C-c C-k" . nil))
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
  (setq org-src-lang-modes '(("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("emacs-lisp" . emacs-lisp)
                             ("shell" . sh)
                             ("bash" . sh)
                             ("fish" . fish)
                             ("typescript" . typescript)
                             ("html" . web)
                             ("vue" . vue)
                             ("javascript" . js)))
  (setq org-file-apps
        (append '(("\\.png\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file))))
                  ("\\.jpg\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file))))
                  ("\\.gif\\'" . (lambda (file link) (shell-command-to-string (concat "open -R " file)))))))

  (setq org-babel-load-languages
        '((plantuml . t)
          (sql . t)
          (emacs-lisp . t)
          (shell . t)
          (js . t)
          (org . t)
          (ruby . t)
          (restclient . t))))

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
  :hook ((org-mode . org-modern-mode)))

(use-package org-ai
  :ensure t
  :defer t
  :hook
  (org-mode . org-ai-mode)
  :config
  (setq org-ai-default-chat-model "gpt-3.5-turbo"))

(use-package ob-restclient
  :ensure t
  :defer t)

(use-package ob-async
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
  (markdown-mode . orgtbl-mode)
  :bind (:map markdown-mode-map ("C-c C-l" . nil))
  :mode
  ("\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.mdown\\'" . gfm-mode)
  :config
  (setq markdown-command "marked")
  (setq markdown-hide-urls nil)
  (setq markdown-hide-markup nil)
  (setq markdown-fontify-code-block-natively t)
  (setq markdown-gfm-additional-languages '("Mermaid"))
  (setq markdown-css-paths '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown-light.min.css"
                             "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/default.min.css"))
  (setq markdown-live-preview-window-function 'markdown-live-preview-window-xwidget-webkit)
  (setq markdown-xhtml-body-preamble "<article class='markdown-body'>")
  (setq markdown-xhtml-body-epilogue "</article>")
  (setq markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1'>
<style>
	.markdown-body {
		box-sizing: border-box;
		min-width: 200px;
		max-width: 980px;
		margin: 0 auto;
		padding: 45px;
	}
</style>
<script src='https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js'></script>
<script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js'></script>
<script>
mermaid.initialize({startOnLoad:true});
hljs.highlightAll();
</script>
")

  (defun markdown-live-preview-window-xwidget-webkit (file)
    "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
    (let ((uri (format "file://%s" file)))
      (xwidget-webkit-browse-url uri)
      xwidget-webkit-last-session-buffer)))

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
