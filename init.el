;;; init.el --- My emacs settings. -*- coding: utf-8 ; lexical-binding: t -*-
;; Author: hyakt <https://github.com/hyakt/.emacs.d>

;;; Commentary:
;; This is hyakt's init.el of Emacs.

;;; Code:
;; leaf-install-code
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf
    :config
    (leaf leaf-keywords
      :ensure t
      :init
      (leaf el-get :ensure t)
      (leaf system-packages :ensure t)
      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))
    (leaf leaf-tree
      :ensure t
      :custom ((imenu-list-size . 30)
               (imenu-list-position . 'left)))))

;;; ---------- 初期設定 ----------
(leaf *basic
  :setq-default
  (shell-file-name . "/bin/bash")
  (explicit-shell-file-name . "/bin/bash")
  :pre-setq
  (custom-file . "~/.emacs.d/custom.el")
  :custom
  (
   (auto-coding-functions . nil)                                                              ;; 文字コードの自動変換保存をしない
   (completion-ignore-case . t)                                                               ;; file名の補完で大文字小文字を区別しない
   (auto-save-default . nil)                                                                  ;; オートセーブのファイルを作らない
   (make-backup-files . t)                                                                    ;; Backup fileを作る
   (backup-directory-alist . '(("\\.*$" . "~/.emacs.d/.backup")))                             ;; バックアップ先
   (create-lockfiles . nil)                                                                   ;; ロックファイル(.#filename)のファイルを作らない
   (gc-cons-threshold . 1073741824)                                                           ;; GCの閾値を設定
   (garbage-collection-messages . nil)                                                        ;; GC実行のメッセージを表示しない
   (message-log-max . 10000)                                                                  ;; ログの記録行数を増やす
   (vc-follow-symlinks . t)                                                                   ;; symlinkは必ず追いかける
   (completion-ignored-extensions . '("~" ".o" ".elc" "./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store"))
   (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt)) ;; minibufferをマウスカーソルで選択できないようにする
   (enable-recursive-minibuffers . t)                                                         ;; minibufferの再帰的使用を許可する
   (enable-local-variables . :all)                                                            ;; local variableは全て使用する
   )
  :global-minor-mode global-auto-revert-mode
  :hook (minibuffer-setup-hook . cursor-intangible-mode)                                       ;; minibufferをマウスカーソルで選択できないようにする
  :init
  (fset 'yes-or-no-p 'y-or-n-p)                                                                ;; yes-noの選択肢をy-nにする
  (setenv "SHELL" "/bin/bash")                                                                 ;; デフォルトの shell を bashに変更
  (setenv "LANG" "ja_JP.UTF-8")                                                                ;; デフォルトのLANGをUTF-8に設定 ruby/flyceck対策
  (run-with-idle-timer 60.0 t #'garbage-collect)                                                ;; Run GC every 60 seconds if emacs is idle.

  (leaf custom-file
    :when (file-exists-p custom-file)
    :config (load custom-file))

  (leaf my-functions
    :load-path "~/.emacs.d/site-lisp/my-functions/"
    :config
    (require 'my-util)
    (require 'my-prog)
    (require 'my-git))

  (leaf server
    :require t
    :config
    (unless (server-running-p) (server-start)))

  (leaf for-macos
    :require (ucs-normalize)
    :when (eq system-type 'darwin)
    :setq
    (file-name-coding-system . 'utf-8-hfs)
    (locale-coding-system . 'utf-8-hfs)
    :global-minor-mode mac-auto-ascii-mode
    :config
    (prefer-coding-system 'utf-8))

  (leaf for-linux
    :when (eq system-type 'gnu/linux)
    :custom
    (x-alt-keysym . 'meta)
    (x-super-keysym . 'meta)
    :setq
    (file-name-coding-system . 'utf-8)
    (locale-coding-system . 'utf-8)
    :config
    (prefer-coding-system 'utf-8))

  (leaf exec-path-from-shell
    :ensure t
    :if (eq system-type 'darwin)
    :custom((exec-path-from-shell-variables '("PATH" "GOPATH")))
    :config
    (exec-path-from-shell-initialize))

  (leaf savehist
    :init
    (savehist-mode)))


;;; ---------- 外観設定 ----------
(leaf *appearance
  :ensure all-the-icons
  :custom
  (cursor-type .'box)
  (echo-keystrokes . 0.1)                                     ;; キーストロークをエコーエリアに早く表示する
  (frame-title-format . "")                                   ;; タイトルバーに何も表示しない
  (indent-tabs-mode . nil)                                    ;; タブの変わりに半角スペースを使う
  (inhibit-startup-screen . 1)                                ;; スタートアップメッセージを非表示
  (init-loader-show-log-after-init . 'error-only)             ;; init-loaderが失敗した時のみエラーメッセージを表示
  (initial-scratch-message . "")                              ;; scratchの初期メッセージ消去
  (line-spacing . 0)                                          ;; 行間を無しに設定
  (scroll-conservatively . 35)                                ;; スクロールの設定
  (scroll-margin . 0)                                         ;; スクロールの設定
  (scroll-step . 1)
  (scroll-bar-mode . nil)                                     ;; スクロールバーを使わない
  (tab-width . 2)                                             ;; タブの幅は半角スペース 2
  (tool-bar-mode . nil)                                       ;; ツールバーを利用しない
  (truncate-lines . nil)                                      ;; 画面端まで来たら折り返す
  (truncate-partial-width-windows . nil)
  (uniquify-buffer-name-style . 'post-forward-angle-brackets) ;; 同じ名前のバッファを開いたときの設定
  (split-height-threshold . 120)
  (split-width-threshold . 200)
  (default-frame-alist .
    '((top . 0)
      (left . 100)
      (width . (text-pixels . 1280))
      (height . (text-pixels . 800))
      (alpha . (100 100))))
  :global-minor-mode
  show-paren-mode                                             ;; 対応する括弧を光らせる
  transient-mark-mode                                         ;; 選択部分のハイライト
  global-font-lock-mode                                       ;; フォントロックモード
  line-number-mode                                            ;; 行番号を表示
  column-number-mode                                          ;; 列番号を表示
  :preface
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
    (let* ((file "~/.emacs.d/.framesize"))
      (if (file-exists-p file) (load-file file))))
  :hook
  (window-setup-hook . frame-size-resume)
  (kill-emacs-hook . frame-size-save)
  :init
  (leaf font
    :config
    (set-face-attribute 'default nil
                        :family "Source Han Code JP"
                        :height 110)
    (set-face-attribute 'variable-pitch nil
                        :family "Myrica M"
                        :height 120)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP")))

  (leaf cursor
    :when (eq system-type 'darwin)
    :config
    (defun mac-selected-keyboard-input-source-change-hook-func ()
      ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
      (set-cursor-color (if (or
                             (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                             (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                            "#FF5996" "#51AFEF")))
    (add-hook 'mac-selected-keyboard-input-source-change-hook
              'mac-selected-keyboard-input-source-change-hook-func))

  (leaf doom-themes
    :ensure t
    :init
    (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/my-themes")
    :config
    (load-theme 'my-doom-tokyo-night t)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (leaf doom-modeline
      :ensure t
      :custom ((doom-modeline-buffer-encoding . nil)
               (doom-modeline-buffer-file-name-style . 'auto)
               (doom-modeline-height . 32)
               (doom-modeline-bar-width . 3)
               (doom-modeline-enable-word-count . 5)
               (doom-modeline-vcs-max-length . 50))
      :config
      (doom-modeline-mode 1)))

  (leaf paren
    :after paren
    :bind (("M-o" . my/jump-to-match-parens))
    :custom((show-paren-style . 'mixed)
            (show-paren-when-point-inside-paren . t)
            (show-paren-when-point-in-periphery . t))
    :config
    (defun my/jump-to-match-parens nil
      "対応する括弧に移動"
      (interactive)
      (let ((paren-point (show-paren--default)))
        (let ((beg (nth 1 paren-point))
              (end (nth 3 paren-point)))
          (if (>=
               (point)
               beg)
              (goto-char end)
            (goto-char beg))))))

  (leaf beacon
    :ensure t
    :global-minor-mode beacon-mode)

  ;; (leaf dimmer
  ;;   :ensure t
  ;;   :global-minor-mode dimmer-mode)

  (leaf volatile-highlights
    :ensure t
    :global-minor-mode volatile-highlights-mode)

  (leaf whitespace
    :custom
    ((whitespace-style . '(
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
     (whitespace-display-mappings . '(
                                      ;; (space-mark   ?\     [?·]     [?.])          ; space - middle dot
                                      (space-mark   ?\xA0  [?\u00A4]     [?_])        ; hard space - currency
                                      (space-mark   ?\u3000 [?\u25a1])                ; Japanese zenkaku space - rect
                                      (newline-mark ?\n    [?$ ?\n])                  ; eol - dollar sign
                                      (tab-mark     ?\t    [?» ?\t] [?\\ ?\t])        ; tab - right guillemet
                                      ))
     (whitespace-action . '(auto-cleanup))
     (whitespace-space-regexp . "\\(\u3000\\)"))
    :global-minor-mode global-whitespace-mode)

  (leaf indent-guide
    :ensure t
    :custom (indent-guide-char . ">")
    :global-minor-mode indent-guide-global-mode))


;;; ---------- 編集機能設定 ----------
(leaf *edit
  :config
  (leaf keybind
    :bind
    ("C-h" . nil)
    ("C-m" . newline-and-indent) ; リターンで改行とインデント
    ("C-0" . delete-frame)
    ;; my/function keybinding
    ("C-g" . my/keyboard-quit)
    ("<f5>" . my/revert-buffer-no-confirm)
    ("M-r" . my/revert-buffer-no-confirm)
    ("C-x k" . kill-this-buffer)
    ("C-x C-k" . my/close-and-kill-this-pane)
    ("C-x C-x" . my/kill-other-buffers)
    ("C-x i" . my/buffer-indent)
    ("C-x d" . my/dired-this-buffer)
    :bind*
    ("M-c M-c M-c" . my/deepl-region)
    :init
    (keyboard-translate ?\C-h ?\C-?)
    (global-unset-key (kbd "C-z")))

  (leaf disable-mouse
    :ensure t
    :custom (disable-mouse-wheel-events . '("wheel-left" "wheel-right"))
    :global-minor-mode global-disable-mouse-mode)

  (leaf yasnippet
    :ensure (yasnippet yasnippet-snippets)
    :custom (yas-snippet-dirs . '("~/.emacs.d/site-lisp/my-snippets"))
    :config
    (yas-global-mode 1))

  (leaf company
    :ensure t
    :custom ((company-dabbrev-downcase . nil)
             (company-dabbrev-ignore-case . nil)
             (company-dabbrev-other-buffers . t)
             (company-dabbrev-code-other-buffers . t)
             (company-backends . '((company-elisp)
                                   (company-yasnippet)))
             (company-transformers .'(company-sort-by-backend-importance))
             (company-idle-delay . 0.1)
             (company-minimum-prefix-length . 2)
             (company-selection-wrap-around . t)
             (company-tooltip-align-annotations . t))
    :bind (("C-j" . company-complete)
           (company-active-map
            ("C-n" . company-select-next)
            ("C-p" . company-select-previous)
            ("C-d" . company-show-doc-buffer)
            ("C-o" . company-other-backend)))
    :config
    (global-company-mode)
    (leaf company-box :ensure t :config (company-box-mode t))
    (leaf company-quickhelp :ensure t :config (company-quickhelp-mode t)))

  (leaf flycheck
    :ensure t
    :custom
    (flycheck-disabled-checkers . '(slim-lint))
    :config
    (global-flycheck-mode t))

  (leaf quickrun
    :ensure t
    :bind (("C-x q" . quickrun)
           ("C-x a" . quickrun-with-arg)))

  (leaf smart-jump
    :ensure (smart-jump dumb-jump)
    :bind (("M-." . smart-jump-go)
           ("M-," . smart-jump-back)
           ("M-'" . smart-jump-references)
           ("M-P" . smart-jump-peek))
    :custom (smart-jump-bind-keys . nil)
    :config
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

  (leaf jumplist
    :require t
    :ensure t
    :bind (("M-p" . jumplist-previous)
           ("M-n" . jumplist-next))
    :custom
    ((jumplist-hook-commands .
                             '(avy-goto-char
                               mouse-set-point
                               smart-jump-go smart-jump-ref
                               lsp-ui-peek-find-definitions lsp-ui-peek-find-references
                               xref-find-definitions xref-find-references
                               dump-jump-go
                               my/jump-to-match-parens
                               consult-line consult-ripgrep consult-find consult-ghq-find consult-fd
                               end-of-buffer beginning-of-buffer))
     (jumplist-ex-mode . t)))

  (leaf rainbow-delimiters
    :ensure t
    :hook (prog-mode-hook))

  (leaf rainbow-mode
    :ensure t
    :hook (js2-mode-hook css-mode-hook html-mode-hook web-mode-hook typescript-mode-hook))

  (leaf symbol-overlay
    :ensure t
    :hook (prog-mode-hook markdown-mode-hook)
    :bind (("M-i" . symbol-overlay-put)))

  (leaf pcre2el
    :ensure t
    :custom (rxt-global-mode . t))

  (leaf elec-pair
    :init (electric-pair-mode)
    :commands org-add-electric-pairs web-add-electric-pairs
    :hook ((org-mode-hook . org-add-electric-pairs)
           ((web-mode-hook typescript-mode-hook) . web-add-electric-pairs))
    :config
    (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
    (defun org-add-electric-pairs ()
      (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
      (setq-local electric-pair-text-pairs electric-pair-pairs))
    (defvar web-electric-pairs '((?< . ?>) (?' . ?') (?` . ?`)) "Electric pairs for web-mode.")
    (defun web-add-electric-pairs ()
      (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs))
      (setq-local electric-pair-text-pairs electric-pair-pairs)))

  (leaf yafolding
    :ensure t
    :hook prog-mode-hook)

  (leaf visual-regexp
    :ensure t
    :bind (("C-r" . vr/query-replace))
    :custom ((case-fold-search . nil))
    :config
    (leaf visual-regexp-steroids
      :ensure t
      :require t
      :custom (vr/engine . 'pcre2el)))

  (leaf multiple-cursors
    :ensure t
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)))

  (leaf undo-fu
    :ensure t
    :bind (("C-/" . undo-fu-only-undo)
           ("M-/" . undo-fu-only-redo)))

  (leaf expand-region
    :ensure t
    :bind (("C-," . er/expand-region)
           ("C-M-," . er/contract-region)))

  (leaf recentf
    :ensure t
    :custom
    (recentf-max-saved-items . 1000)
    (recentf-exclude . '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
    (recentf-auto-cleanup . 'never)
    :config
    (recentf-mode 1))

  (leaf wgrep
    :ensure t
    :custom
    (wgrep-enable-key . "e")
    (wgrep-auto-save-buffer . t)
    (wgrep-change-readonly-file . t))

  (leaf string-inflection
    :ensure t
    :bind (("M-[" . string-inflection-all-cycle)))

  (leaf unicode-escape :ensure t :require t)

  (leaf rg
    :ensure t
    :custom ((rg-group-result . t)
             (rg-custom-type-aliases . '(("graphql" . "*.gql *.graphql"))))))

;;; ---------- インターフェース設定 ----------
(leaf *interface
  :config
  (leaf ediff
    :custom
    (ediff-split-window-function . 'split-window-horizontally))

  (leaf projectile
    :ensure t
    :bind (("C-x t" . my/projectile-toggle-between-implementation-and-test-other-window))
    :custom
    ((projectile-add-known-project . '("~/repos/")))
    :preface
    (defun my/projectile-toggle-between-implementation-and-test-other-window nil
      "Toggle between an implementation file and its test file."
      (interactive)
      (find-file-other-window
       (projectile-find-implementation-or-test
        (buffer-file-name))))
    :config
    (projectile-mode 1)
    (leaf projectile-rails
      :ensure t
      :hook projectile-mode-hook))

  (leaf dashboard
    :ensure t
    :custom
    (dashboard-items . '((recents  . 10)
                         (projects . 10)))
    (dashboard-startup-banner . 'logo)
    :config
    (dashboard-setup-startup-hook))

  (leaf consult
    :ensure-system-package ((rg . ripgrep) (fd))
    :ensure t consult-flycheck consult-ghq
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c b" . consult-bookmark)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x C-b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x f" . consult-fd)
           ("C-x e" . consult-ripgrep)
           ("C-x C-r" . consult-recent-file)
           ("C-x C-g" . consult-ghq-find)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("<help> a" . consult-apropos)            ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("C-s" . consult-line)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flycheck)
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-project-imenu)
           ("M-e" . consult-isearch))
    :custom
    ((xref-show-xrefs-function . 'consult-xref)
     (xref-show-definitions-function . 'consult-xref)
     (consult-ghq-find-function . 'magit-status)
     (consult-project-root-function . #'projectile-project-root)
     (consult-ripgrep-command . "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --ignore-case . -e ARG OPTS"))
    :config
    ;; see: https://github.com/minad/consult/wiki#find-files-using-fd
    (defvar consult--fd-command "fd")

    (defun consult--fd-builder (input)
      (unless consult--fd-command
        (setq consult--fd-command
              (if (eq 0 (call-process-shell-command "fdfind"))
                  "fdfind"
                "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended)))
        (when re
          (list :command (append
                          (list consult--fd-command
                                "--color=never" "--full-path"
                                (consult--join-regexps re 'extended))
                          opts)
                :highlight hl))))

    (defun consult-fd (&optional dir initial)
      (interactive "P")
      (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
             (default-directory (cdr prompt-dir)))
        (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (list :debounce 0.8 'any))
    )

  (leaf vertico
    :ensure t
    :custom ((vertico-count . 30))
    :init
    (vertico-mode))

  (leaf orderless
    :ensure t
    :custom
    ((completion-styles . '(orderless))
     (completion-category-defaults . nil)))

  (leaf marginalia
    :ensure t
    :bind (("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (leaf embark
    :ensure t
    :bind
    (("M-O" . embark-act))
    :preface
    (defun embark-action-indicator-which-key (map _target)
      (which-key--show-keymap "Embark" map nil nil 'no-paging)
      #'which-key--hide-popup-ignore-command)
    :custom
    ((embark-action-indicator . 'embark-action-indicator-which-key)
     (embark-become-indicator . 'embark-action-indicator-which-key))
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (leaf embark-consult
    :ensure t
    :after (embark consult)
    :require t)

  (leaf dired
    :ensure (wdired all-the-icons-dired)
    :custom ((dired-dwim-target . t))
    :bind (dired-mode-map (("e" . wdired-change-to-wdired-mode)
                           ("C-t" . nil)
                           ("M-s" . nil)))
    :config
    (leaf dired-sidebar
      :ensure t
      :bind (("M-d" . dired-sidebar-toggle-sidebar)
             (dired-sidebar-mode-map
              ("o" . dired-sidebar-subtree-toggle)))
      :custom ((dired-sidebar-use-term-integration . t)
               (dired-sidebar-use-custom-modeline . nil))
      :config
      (defcustom dired-sidebar-mode-line-format
        '("%e" mode-line-front-space
          mode-line-buffer-identification
          " "  mode-line-end-spaces)
        "Mode line format for `dired-sidebar'."
        :type 'list
        :group 'dired-sidebar)))

  (leaf shell
    :config
    (leaf eshell
      :custom
      (eshell-cmpl-ignore-case . t)
      (eshell-ask-to-save-history . 'always)
      :config
      (leaf eshell-prompt-extras
        :ensure t
        :custom
        (eshell-highlight-prompt . nil)
        (eshell-prompt-function . 'epe-theme-lambda))
      (leaf esh-autosuggest
        :ensure t
        :hook (eshell-mode-hook)))

    (leaf vterm
      :ensure t
      :ensure-system-package (cmake libtool)
      :custom
      (vterm-max-scrollback . 10000)
      (vterm-buffer-name-string . "vterm: %s")
      ;; delete "C-h", add <f1> and <f2>
      (vterm-keymap-exceptions
       . '("<f1>" "<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "C-t" "M-t" "M-s"))
      :config
      ;; Workaround of not working counsel-yank-pop
      ;; https://github.com/akermu/emacs-libvterm#counsel-yank-pop-doesnt-work
      (defun my/vterm-counsel-yank-pop-action (orig-fun &rest args)
        (if (equal major-mode 'vterm-mode)
            (let ((inhibit-read-only t)
                  (yank-undo-function (lambda (_start _end) (vterm-undo))))
              (cl-letf (((symbol-function 'insert-for-yank)
                         (lambda (str) (vterm-send-string str t))))
                (apply orig-fun args)))
          (apply orig-fun args)))
      (advice-add 'counsel-yank-pop-action :around #'my/vterm-counsel-yank-pop-action)
      (leaf vterm-toggle
        :ensure t
        :bind ("M-t" . vterm-toggle)
        :custom
        ((vterm-toggle-reset-window-configration-after-exit . t)
         (vterm-toggle-scope . 'project)))))

  (leaf git
    :config
    (leaf magit
      :require t
      :ensure (magit gh)
      :ensure-system-package git
      :custom ((magit-save-repository-buffers . 'dontask))
      :bind (("M-s" . magit-status-toggle)
             (magit-status-mode-map
              ("q" . my/magit-quit-session)
              ("C-o" . magit-diff-visit-file-other-window)))
      :preface
      (defun my/magit-quit-session ()
        (interactive)
        (kill-buffer)
        (delete-window))

      (defun magit-status-toggle()
        "magit toggle."
        (interactive)
        (if (or (derived-mode-p 'magit-status-mode)
                (magit-status-toggle--get-window))
            (magit-status-toggle-hide)
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
                    (window-list))))

    (leaf magit-delta
      :after magit
      :ensure t
      :ensure-system-package (delta . git-delta)
      :config
      (defun my/magit-delta-toggle ()
        "Toggle magit-delta-mode and refresh magit."
        (interactive)
        (progn
          (call-interactively 'magit-delta-mode)
          (magit-refresh))))

    (leaf git-gutter
      :ensure t
      :custom
      (git-gutter:modified-sign . " ")
      (git-gutter:added-sign    . " ")
      (git-gutter:deleted-sign  . " ")
      :custom-face
      (git-gutter:modified . '((t (:background "#B4DCE7"))))
      (git-gutter:added    . '((t (:background "#74DFC4"))))
      (git-gutter:deleted  . '((t (:background "#964C7B"))))
      :global-minor-mode global-git-gutter-mode)

    (leaf git-timemachine :ensure t)

    (leaf git-link
      :ensure t
      :custom (git-link-open-in-browser . t)))

  (leaf docker :ensure t
    :config
    (leaf docker-tramp :ensure t))

  (leaf window
    :config
    (leaf swap-buffers
      :ensure t
      :bind ("C-x C-o" . swap-buffers))

    (leaf other-window-or-split
      :el-get (other-window-or-split :url "https://github.com/conao/other-window-or-split.git")
      :bind (("C-t" . my/ws-other-window-or-split-and-kill-minibuffer)
             ("C-S-t" . ws-previous-other-window-or-split))
      :custom (ws-split-window-width-with-em . 130)
      :config
      (require 'dired-sidebar)
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

    (leaf eyebrowse
      :ensure t
      :custom
      (eyebrowse-new-workspace . t)
      (eyebrowse-keymap-prefix . "\C-z")
      :global-minor-mode eyebrowse-mode)

    (leaf shackle
      :ensure t
      :custom
      ((shackle-default-size . 0.4)
       (shackle-rules .
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
                        (magit-status-mode          :align below :ratio 0.7 :select t :inhibit-window-quit t)
                        ;; repl
                        ("*Python*"                 :align below :ratio 0.5 :select t)
                        ("*pry*"                    :align below :ratio 0.5 :select t)
                        ("*ruby*"                   :align below :ratio 0.5 :select t)
                        ("*nodejs*"                 :align below :ratio 0.5 :select t)
                        ("*shell*"                  :align below :ratio 0.5 :select t)
                        ("*Typescript*"             :align below :ratio 0.5)
                        ;; excute shell
                        ("*Async Shell Command*"    :align right)
                        ("*Shell Command Output*"   :align right)
                        ("\\`\\*My Mocha .*?\\*\\'" :regexp t :align below :ratio 0.5)
                        ("*jest*"                   :regexp t :align below :pratio 0.5)
                        (vterm-mode                 :align below :ratio 0.7)
                        ;; rust
                        ("*rustic-compilation*"     :align below :ratio 0.33 :select nil)
                        ("*rustfmt*"                :align below :ratio 0.33 :select nil)
                        ;; ruby
                        ("*rspec-compilation*"      :align below :ratio 0.5 :select nil)
                        )))
      :config
      (shackle-mode 1)))

  (leaf which-key
    :ensure t
    :global-minor-mode which-key-mode)

  (leaf amx :ensure t)

  (leaf open-junk-file
    :ensure t
    :bind (("C-`" . open-junk-file))
    :init
    (defvaralias 'open-junk-file-format 'open-junk-file-directory "Temporary alias for Emacs27")
    :custom
    (open-junk-file-format . "~/Documents/junk/%Y-%m-%d-%H%M%S."))

  (leaf tree-sitter
    :ensure (t tree-sitter-langs)
    :require tree-sitter-langs
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
    ;; TSXの対応
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
    ;; ハイライトの追加
    (tree-sitter-hl-add-patterns 'tsx
      [
       ;; styled.div``
       (call_expression
        function: (member_expression
                   object: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ;; styled(Component)``
       (call_expression
        function: (call_expression
                   function: (identifier) @function.call
                   (.eq? @function.call "styled"))
        arguments: ((template_string) @property.definition
                    (.offset! @property.definition 0 1 0 -1)))
       ])
    )
  )


;;; ---------- メジャーモード設定 ----------
(leaf *major-mode
  :config
  (leaf lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :bind ((lsp-mode-map
            ("C-c i" . lsp-execute-code-action)))
    :custom ((lsp-enable-indentation . nil)
             (lsp-eldoc-render-all . t)
             (lsp-signature-auto-activate .t)
             (lsp-signature-render-documentation . t)
             (lsp-enable-snippet . nil)
             (lsp-enable-xref . t)
             (lsp-headerline-breadcrumb-enable . nil)
             (lsp-enable-file-watchers . nil)
             (lsp-clients-deno-import-map . "./import_map.json"))
    :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
    )

  (leaf web
    :config
    (leaf web-mode
      :ensure t
      :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.jsx\\'")
      :custom
      ((web-mode-indent-style . 2)
       (web-mode-markup-indent-offset . 2)
       (web-mode-css-indent-offset . 2)
       (web-mode-code-indent-offset . 2)
       (web-mode-enable-auto-pairing . t)
       (web-mode-enable-auto-quoting . nil)
       (web-mode-enable-auto-indentation . nil)
       (web-mode-enable-css-colorization . t)
       (web-mode-enable-current-element-highlight . t)
       (web-mode-enable-current-column-highlight . t)
       (web-mode-enable-auto-quoting . nil)
       (web-mode-comment-formats .
                                 '(("javascript" . "//")
                                   ("jsx" .  "//")
                                   ("php" . "/*"))))
      :config
      (add-hook 'web-mode-hook
                (lambda ()
                  (when (equal web-mode-content-type "jsx")
                    (setq emmet-expand-jsx-className? t)
                    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
                    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
                    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
                    (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
                    (flycheck-add-mode 'javascript-eslint 'web-mode)
                    (tern-mode)
                    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                    (set (make-local-variable 'company-backends)
                         '((company-tern :with company-dabbrev-code) company-yasnippet))))))

    (leaf emmet-mode
      :ensure t
      :bind ((emmet-mode-keymap
              ("C-j" . company-complete)))
      :hook (html-mode-hook
             web-mode-hook
             css-mode-hook
             scss-mode-hook))

    (leaf add-node-modules-path
      :ensure t
      :hook ((typescript-mode-hook
              js2-mode-hook
              web-mode-hook
              scss-mode-hook
              graphql-mode-hook
              ts-comint-mode-hook
              json-mode-hook) . add-node-modules-path))

    (leaf prettier-js
      :ensure t
      :hook ((typescript-mode-hook
              js2-mode-hook
              web-mode-hook
              css-mode-hook
              scss-mode-hook
              graphql-mode-hook)))

    (leaf html
      :config
      (leaf slim-mode :ensure t)
      (leaf haml-mode :ensure t))

    (leaf css
      :config
      (leaf css-mode
        :custom (css-indent-offset . 2)
        :hook
        (css-mode-hook . (lambda ()
                           (set
                            (make-local-variable 'flycheck-checker)
                            (setq flycheck-checker 'css-stylelint))
                           (set
                            (make-local-variable 'company-backends)
                            '((company-css :with company-dabbrev)
                              company-yasnippet)))))
      (leaf scss-mode
        :ensure t
        :custom (scss-indent-offset . 2)
        :init
        ;; see: https://github.com/flycheck/flycheck/issues/1912
        (flycheck-define-checker general-stylelint
          "A checker for CSS and related languages using Stylelint"
          :command ("stylelint"
                    (eval flycheck-stylelint-args)
                    (option-flag "--quiet" flycheck-stylelint-quiet)
                    (config-file "--config" flycheck-general-stylelintrc))
          :standard-input t
          :error-parser flycheck-parse-stylelint
          :predicate flycheck-buffer-nonempty-p
          :modes (scss-mode))
        (flycheck-def-config-file-var flycheck-general-stylelintrc
            (general-stylelint) nil)
        (add-to-list 'flycheck-checkers 'general-stylelint)
        :hook
        (scss-mode-hook . (lambda ()
                            (set
                             (make-local-variable 'flycheck-checker)
                             (setq flycheck-checker 'general-stylelint)
                             )
                            (set
                             (make-local-variable 'company-backends)
                             '((company-css :with company-dabbrev) company-yasnippet)))))
      (leaf sass-mode :ensure t)
      (leaf sws-mode :ensure t))

    (leaf javascript
      :config
      (leaf js2-mode
        :mode (("\\.js$"))
        :ensure (js2-mode tern xref-js2)
        :el-get (company-tern :url "https://github.com/emacsattic/company-tern.git")
        :ensure-system-package (tern . "npm i -g tern")
        :custom
        ((js-indent-level . 2)
         (js-switch-indent-offset . 2)
         (js2-basic-offset . 2)
         (js2-strict-missing-semi-warning . nil)
         (xref-js2-search-program . 'rg))
        :hook
        (js2-mode-hook . (lambda ()
                           (tern-mode)
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                           (set
                            (make-local-variable 'company-backends)
                            '((company-tern :with company-dabbrev-code)
                              company-yasnippet)))))

      (leaf typescript-mode
        :ensure t
        :custom (typescript-indent-level . 2)
        :hook ((typescript-mode-hook . lsp-deferred)
               (typescript-mode-hook . subword-mode))
        :init
        (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
        (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

      (leaf ts-comint
        :ensure t typescript-mode
        :after typescript-mode
        :ensure-system-package (ts-node . "npm i -g ts-node")
        :commands (run-ts)
        :custom (ts-comint-program-command . "ts-node")
        :bind (typescript-mode-map
               (("C-x C-e" . ts-send-last-sexp)
                ("C-c b" . ts-send-buffer)
                ("C-c r" . ts-send-region))))

      (leaf deno-fmt :ensure t)

      (leaf coffee-mode
        :ensure t
        :custom (coffee-tab-width . 2))

      (leaf nodejs-repl :ensure t)

      (leaf npm-mode
        :ensure t
        :hook (typescript-mode-hook js2-mode-hook web-mode-hook scss-mode-hook))

      (leaf jest
        :ensure t
        :bind ((jest-minor-mode-map
                ("C-c C-c C-c" . jest-file-dwim)))
        :hook ((typescript-mode-hook . jest-minor-mode)
               (js2-mode-hook . jest-minor-mode)
               (web-mode-hook . jest-minor-mode)))))

  (leaf ruby-mode
    :ensure t
    :mode ("\\.rb\\'" "Capfile$" "Gemfile$" "[Rr]akefile$")
    :interpreter ("pry")
    :custom ((ruby-insert-encoding-magic-comment . nil))
    :hook ((ruby-mode-hook . lsp-deferred))
    :config
    (leaf inf-ruby
      :ensure t
      :bind ((inf-ruby-minor-mode-map
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-l" . ruby-send-line)))
      :init
      (defalias 'pry 'inf-ruby)
      :custom
      (inf-ruby-default-implementation . "pry")
      (inf-ruby-eval-binding . "Pry.toplevel_binding"))

    (leaf rubocop  :ensure t)

    (leaf rspec-mode
      :ensure t
      :bind ((rspec-mode-map
              ("C-c C-c C-c" . rspec-verify-single)))))

  (leaf swift-mode
    :ensure t
    :hook (swift-mode-hook . (lambda ()
                               (add-to-list 'flycheck-checkers 'swift)
                               (set
                                (make-local-variable 'company-backends)
                                '((company-sourcekit)))))
    :config
    (leaf company-sourcekit :ensure t))

  (leaf dart-mode
    :ensure t
    :custom
    (dart-format-on-save . nil)
    (dart-enable-analysis-server . nil)
    (dart-sdk-path . "~/repos/github.com/flutter/flutter/bin/cache/dart-sdk/"))

  (leaf flutter
    :ensure t
    :custom
    (flutter-sdk-path . "~/repos/github.com/flutter/flutter/"))

  (leaf sql
    :ensure t
    :mode (".sql$")
    :hook
    (sql-interactive-mode-hook .
                               (lambda ()
                                 (buffer-face-set 'variable-pitch)
                                 (toggle-truncate-lines t)))
    :config
    (leaf sqlup-mode
      :ensure t
      :hook (sql-mode-hook sql-interactive-mode-hook))
    (leaf sqlformat
      :ensure t
      :ensure-system-package sqlparse
      :preface
      (defun my/sql-indent-region (beg end)
        "Indent the SQL statement in the BEG to END (region)."
        (interactive "*r")
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (sql-indent-buffer))))))

  (leaf python :ensure t)

  (leaf php-mode :ensure t)

  (leaf haskell-mode :ensure t)

  (leaf graphql-mode :ensure t)

  (leaf java-mode
    :hook
    (java-mode-hook . (lambda ()
                        (setq tab-width 4)
                        (setq indent-tabs-mode t)
                        (setq c-basic-offset 4))))

  (leaf dockerfile-mode :ensure t)

  (leaf docker-compose-mode :ensure t)

  (leaf nginx-mode :ensure t)

  (leaf gitconfig-mode :ensure t)

  (leaf gitignore-mode :ensure t)

  (leaf go-mode
    :ensure t
    :hook ((go-mode-hook . lsp)))

  (leaf elixir-mode
    :ensure t
    :config
    (leaf alchemist :ensure t)
    (leaf flycheck-elixir :ensure t))

  (leaf scala-mode
    :ensure t
    :interpreter ("scala")
    :config
    (leaf sbt-mode
      :ensure t
      :commands sbt-start sbt-command)
    (leaf scala-bootstrap
      :el-get (scala-bootstrap :url "https://github.com/tarao/scala-bootstrap-el")
      :hook (scala-mode-hook . (lambda ()
                                 (scala-bootstrap:with-metals-installed
                                  (scala-bootstrap:with-bloop-server-started
                                   (lsp)))))))

  (leaf rustic
    :ensure t
    :custom ((lsp-rust-analyzer-server-command . '("~/.cargo/bin/rust-analyzer"))
             (rustic-format-display-method . 'display-buffer)
             (rustic-format-trigger . 'on-compile)))

  (leaf fish-mode :ensure t)

  (leaf csv-mode :ensure t)

  (leaf jq-mode
    :ensure t
    :ensure-system-package jq)

  (leaf org
    :bind ((org-mode-map
            ("C-," . nil)))
    :mode ("\\.txt$")
    :custom
    (org-startup-truncated . nil)
    (org-src-fontify-natively . t)
    (org-log-done . 'time)
    :config
    (defun my-add-custom-id nil
      "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
      (interactive)
      (my-org-custom-id-get nil t))

    (defun my-get-custom-id nil
      "Return a part of UUID with an \"org\" prefix. e.g. \"org3ca6ef0c\"."
      (let* ((id (org-id-new "")))
        (when (org-uuidgen-p id)
          (downcase
           (concat "org"
                   (substring
                    (org-id-new "")
                    0 8))))))

    (defun my-org-custom-id-get (&optional pom create)
      "See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
      (interactive)
      (org-with-point-at pom
                         (let ((id (org-entry-get nil "CUSTOM_ID")))
                           (cond
                            ((and id
                                  (stringp id)
                                  (string-match "\\S-" id))
                             id)
                            (create
                             (setq id (my-get-custom-id))
                             (unless id
                               (error "Invalid ID"))
                             (org-entry-put pom "CUSTOM_ID" id)
                             (message "--- CUSTOM_ID assigned: %s" id)
                             (org-id-add-location id
                                                  (buffer-file-name
                                                   (buffer-base-buffer)))
                             id)))))

    (leaf ox-latex
      :custom
      (org-latex-default-class . "cv")
      (org-latex-pdf-process . '("latexmk %f"))
      (org-file-apps . '(("pdf" . "/usr/bin/open -a Preview.app %s")))
      (org-latex-with-hyperref . nil)
      (org-latex-hyperref-template . nil)
      :defer-config
      (add-to-list 'org-latex-classes
                   '("cv"
                     "\\documentclass[autodetect-engine,dvi=dvipdfmx,10pt,a4wide,ja=standard]{bxjsarticle}
                      \\parindent = 0pt
                      \\usepackage{typearea}
                      \\typearea{18}
                      \\usepackage{longtable}
                      [NO-DEFAULT-PACKAGES]
                      \\usepackage{amsmath}
                      \\usepackage[dvipdfmx]{hyperref}
                      \\usepackage{url}
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
    (leaf htmlize :ensure t)
    (leaf ob-sql-mode :ensure t)
    (leaf ox-gfm :ensure t :require t)
    (leaf org-bullets :ensure t
      :config
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

    (leaf markdown-mode
      :ensure t
      :mode (("\\.markdown\\'" . gfm-mode)
             ("\\.md\\'" . gfm-mode)
             ("\\.mdown\\'" . gfm-mode))
      :hook
      (markdown-mode-hook .
                          (lambda nil
                            (set
                             (make-local-variable 'whitespace-action)
                             nil)))))

  (leaf plantuml-mode
    :ensure t
    ;; :ensure-system-package (plantuml graphviz)
    :custom
    ((plantuml-executable-path . "plantuml")
     (plantuml-default-exec-mode . 'executable)))
  )

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
