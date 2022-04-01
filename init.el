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
      (leaf hydra :ensure t)
      (leaf major-mode-hydra
        :ensure t
        :custom
        ((major-mode-hydra-title-generator
          . '(lambda (mode)
               (s-concat (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                         " "
                         (symbol-name mode)
                         " commands")))
         (major-mode-hydra-invisible-quit-key . "q"))
        :bind ("M-a" . major-mode-hydra))
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
   (completion-ignore-case . t)                                                               ;; file 名の補完で大文字小文字を区別しない
   (auto-save-default . nil)                                                                  ;; オートセーブのファイルを作らない
   (make-backup-files . t)                                                                    ;; Backup file を作る
   (backup-directory-alist . '(("\\.*$" . "~/.emacs.d/.backup")))                             ;; バックアップ先
   (create-lockfiles . nil)                                                                   ;; ロックファイル(.#filename)のファイルを作らない
   (gc-cons-threshold . 1073741824)                                                           ;; GC の閾値を設定
   (garbage-collection-messages . nil)                                                        ;; GC 実行のメッセージを表示しない
   (message-log-max . 10000)                                                                  ;; ログの記録行数を増やす
   (vc-follow-symlinks . t)                                                                   ;; symlink は必ず追いかける
   (completion-ignored-extensions . '("~" ".o" ".elc" "./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store"))
   (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt)) ;; minibuffer をマウスカーソルで選択できないようにする
   (enable-recursive-minibuffers . t)                                                         ;; minibuffer の再帰的使用を許可する
   (enable-local-variables . :all)                                                            ;; local variable は全て使用する
   (display-warning-minimum-level . :error)                                                   ;; init.el読み込み時のWarningを抑制
   )
  :global-minor-mode global-auto-revert-mode
  :hook (minibuffer-setup-hook . cursor-intangible-mode)                                      ;; minibuffer をマウスカーソルで選択できないようにする
  :init
  (fset 'yes-or-no-p 'y-or-n-p)                                                               ;; yes-no の選択肢を y-n にする
  (setenv "SHELL" "/bin/bash")                                                                ;; デフォルトの shell を bash に変更
  (setenv "LANG" "ja_JP.UTF-8")                                                               ;; デフォルトの LANG を UTF-8 に設定 ruby/flyceck 対策
  (run-with-idle-timer 60.0 t #'garbage-collect)                                              ;; Run GC every 60 seconds if emacs is idle.

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
    (savehist-mode))

  (leaf paradox
    :ensure t async
    :config
    (paradox-enable))

  (leaf recentf
    :ensure t
    :custom
    (recentf-max-saved-items . 1000)
    (recentf-exclude . '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
    (recentf-auto-cleanup . 'never)
    :global-minor-mode recentf-mode))


;;; ---------- 外観設定 ----------
(leaf *appearance
  :ensure all-the-icons
  :custom
  (cursor-type .'box)
  (echo-keystrokes . 0.1)                                     ;; キーストロークをエコーエリアに早く表示する
  (frame-title-format . "")                                   ;; タイトルバーに何も表示しない
  (indent-tabs-mode . nil)                                    ;; タブの変わりに半角スペースを使う
  (inhibit-startup-screen . 1)                                ;; スタートアップメッセージを非表示
  (init-loader-show-log-after-init . 'error-only)             ;; init-loader が失敗した時のみエラーメッセージを表示
  (initial-scratch-message . "")                              ;; scratch の初期メッセージ消去
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
               (doom-modeline-vcs-max-length . 30))
      :config
      (doom-modeline-mode 1)))

  (leaf paren
    :require t
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

  (leaf dashboard
    :ensure t
    :custom
    (dashboard-items . '((recents  . 10)
                         (projects . 10)))
    (dashboard-startup-banner . 'logo)
    :config
    (dashboard-setup-startup-hook)))


;;; ---------- 編集機能設定 ----------
(leaf *edit
  :preface
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :pretty-hydra
  ((:title (with-faicon "code" "Window & Edit" 1 -0.05) :quit-key "q")
   (
    "Resize"
    (("j" shrink-window "  ↑  ")
     ("k" enlarge-window "  ↓  ")
     ("l" enlarge-window-horizontally "  →  ")
     ("h" shrink-window-horizontally "  ←  "))
    "Font"
    (("+" text-scale-increase "increase")
     ("-" text-scale-decrease "decrease"))
    "Align"
    (("a" align "align")
     ("r" align-regexp "regex" :exit t))
    "Convert"
    (("p"  my/pangu-spacing-region "spacing jp")
     ("tu" my/url-decode-region "url decode")
     ("tn" unicode-unescape-region "unicode decode"))
    "Template"
    (("i" tempel-insert :exit t))
    "Yafolding"
    (("ya" yafolding-show-all "show all")
     ("yh" yafolding-hide-all "hide all"))
    "Symbol"
    (("sr" symbol-overlay-remove-all "remove all" :exit t))
    "Browse"
    (("o" (call-process-shell-command "open .") "open finder" :exit t)
     ("b" browse-url-at-point "browse url" :exit t)
     ("g" google-this-symbol "google this" :exit t))
    "File"
    (("c" my/copy-this-file "copy" :exit t)
     ("m" my/move-or-rename-this-file "rename" :exit t)
     ("d" my/delete-or-remove-this-file "delete" :exit t))))
  :config
  (leaf keybind
    :bind
    (("C-h" . nil)
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
     ("M-e" . *edit/body))
    :init
    (keyboard-translate ?\C-h ?\C-?)
    (global-unset-key (kbd "C-z")))

  (leaf disable-mouse
    :ensure t
    :custom (disable-mouse-wheel-events . '("wheel-left" "wheel-right")))

  (leaf tempel
    :ensure t
    :bind ("<tab>" . my/tempel-maybe-expand)
    :custom (tempel-path . "~/.emacs.d/site-lisp/templates")
    :config
    (define-key tempel-map [remap my/tempel-maybe-expand] #'tempel-next)
    (define-key tempel-map "\C-g" #'tempel-done)
    (defun my/tempel-maybe-expand ()
      (interactive)
      (if (tempel-expand)
          (tempel-expand t)
        (indent-for-tab-command))))

  (leaf company
    :ensure t
    :custom ((company-dabbrev-downcase . nil)
             (company-dabbrev-ignore-case . nil)
             (company-dabbrev-other-buffers . t)
             (company-dabbrev-code-other-buffers . t)
             (company-backends . '((company-elisp)))
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
    :hook
    (after-init-hook . global-flycheck-mode))

  (leaf smart-jump
    :ensure (smart-jump dumb-jump)
    :bind (("M-." . smart-jump-go)
           ("M-," . smart-jump-back)
           ("M-'" . smart-jump-references))
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
                               er/expand-region
                               end-of-buffer beginning-of-buffer))
     (jumplist-ex-mode . t)))

  (leaf rainbow-delimiters
    :ensure t
    :hook (prog-mode-hook))

  (leaf rainbow-mode
    :ensure t
    :hook (js2-mode-hook css-mode-hook html-mode-hook typescript-mode-hook))

  (leaf symbol-overlay
    :ensure t
    :hook (prog-mode-hook markdown-mode-hook)
    :bind (("C-." . symbol-overlay-put)))

  (leaf pcre2el
    :ensure t
    :custom (rxt-global-mode . t))

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
             (rg-custom-type-aliases . '(("graphql" . "*.gql *.graphql")))))

  (leaf pangu-spacing
    :require t
    :ensure t
    :config
    (defun my/pangu-spacing-region (beg end)
      "Replace regexp with match in region."
      (interactive "r")
      (pangu-spacing-search-buffer
       pangu-spacing-include-regexp beg (+ end 8) (replace-match "\\1 \\2" nil nil)))
    :custom ((pangu-spacing-real-insert-separtor . t)))

  (leaf emojify
    :ensure t
    :if (display-graphic-p)
    :hook (after-init-hook . global-emojify-mode)
    :custom  (emojify-emoji-styles . '(github unicode)))

  (leaf google-this :ensure t)

  (leaf elec-pair
    :global-minor-mode electric-pair-mode
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

  (leaf avy
    :ensure t
    :bind ("C-;" . avy-goto-char))

  (leaf ediff
    :custom
    (ediff-split-window-function . 'split-window-horizontally))

  (leaf smerge-mode
    :custom
    (smerge-command-prefix . "\C-c\C-m")
    :hydra
    ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
    (my/hydra-smerge
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
    :hook (magit-diff-visit-file-hook . (lambda ()
                                          (when smerge-mode
                                            (my/hydra-smerge/body))))))


;;; ---------- インターフェース設定 ----------
(leaf *interface
  :config
  (leaf window
    :config
    (leaf swap-buffers
      :ensure t
      :bind (("M-u" . my/hydra-window/body)
             ("C-x C-o" . swap-buffers)))

    (leaf other-window-or-split
      :el-get (other-window-or-split :url "https://github.com/conao3/other-window-or-split.git")
      :bind (("C-t" . my/ws-other-window-or-split-and-kill-minibuffer)
             ("C-S-t" . ws-previous-other-window-or-split))
      :custom (ws-split-window-width-with-em . 130)
      :config
      (defun my/ws-other-window-or-split-and-kill-minibuffer ()
        (interactive)
        (if (active-minibuffer-window)
            (progn
              (minibuffer-keyboard-quit)
              (ws-other-window-or-split))
          (ws-other-window-or-split))))

    (leaf eyebrowse
      :ensure t
      :custom
      (eyebrowse-new-workspace . t)
      (eyebrowse-keymap-prefix . "\C-z")
      :global-minor-mode eyebrowse-mode)

    (leaf shackle
      :ensure t
      :preface
      (defun my/shackle--get-cargo-window ()
        (cl-find-if
         (lambda (win)
           (string-match "*Cargo .*?\\*" (buffer-name (window-buffer win))))
         (window-list)))

      (defun my/shackle-cargo-custom (buffer alist plist)
        (and
         (my/shackle--get-cargo-window)
         (select-window (my/shackle--get-cargo-window))
         (window-deletable-p)
         (delete-window))
        (display-buffer-below-selected buffer alist))
      :custom
      ((shackle-select-reused-windows . t)
       (shackle-default-size . 0.5)
       (shackle-rules .
                      '(("*Help*"                   :align right)
                        ("*Messages*"               :align right)
                        ("*Backtrace*"              :align right)
                        ("*Completions*"            :align below :ratio 0.33)
                        ("*compilation*"            :align below :ratio 0.33)
                        ("*Compile-Log"             :align below :ratio 0.33)
                        ("*Kill Ring*"              :align below :ratio 0.33)
                        ("*Occur*"                  :align below :ratio 0.33)
                        ("*xref*"                   :align below :ratio 0.33)
                        ("*prettier errors*"        :align below :ratio 0.33)
                        (magit-status-mode          :align below :ratio 0.7 :select t)
                        ;; repl
                        ("*Python*"                 :align below :select t)
                        ("*pry*"                    :align below :select t)
                        ("*ruby*"                   :align below :select t)
                        ("*nodejs*"                 :align below :select t)
                        ("*shell*"                  :align below :select t)
                        ("*Typescript*"             :align below :select t)
                        ;; excute shell
                        ("*Async Shell Command*"    :align right)
                        ("*Shell Command Output*"   :align right)
                        ("\\`\\*My Mocha .*?\\*\\'" :regexp t :align below :ratio 0.5)
                        ("*jest*"                   :regexp t :align below :ratio 0.5)
                        (vterm-mode                 :align below :ratio 0.7)
                        ;; rust
                        ("\\`\\*Cargo .*?\\*\\'"    :align below :regexp t :custom my/shackle-cargo-custom)
                        ("*Evcxr*"                  :align below :select t)
                        ;; ruby
                        ("*rspec-compilation*"      :align below :ratio 0.5)
                        )))
      :config
      (shackle-mode 1)))

  (leaf eldoc
    :preface
    (defvar eldoc-buffer-name "*ElDoc*")

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

    (defun my/switch-eldoc-display-mode ()
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

  (leaf projectile
    :ensure t
    :bind (("C-x t" . my/projectile-toggle-between-implementation-and-test-other-window))
    :global-minor-mode projectile-mode
    :custom
    ((projectile-add-known-project . '("~/repos/")))
    :preface
    (defun my/projectile-toggle-between-implementation-and-test-other-window ()
      "Toggle between an implementation file and its test file."
      (interactive)
      (find-file-other-window
       (projectile-find-implementation-or-test
        (buffer-file-name))))
    (leaf projectile-rails
      :ensure t
      :hook projectile-mode-hook))

  (leaf consult
    :ensure-system-package ((rg . ripgrep) (fd))
    :ensure t consult-flycheck consult-ghq
    :bind (;; C-x bindings (ctl-x-map)
           ("C-x C-b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x f" . consult-fd)
           ("C-x e" . consult-ripgrep)
           ("C-x C-r" . consult-recent-file)
           ("C-x C-g" . consult-ghq-find)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings (goto-map)
           ("C-s" . consult-line)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flycheck)
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)
           ("M-g i" . consult-imenu))
    :custom
    ((xref-show-xrefs-function . 'consult-xref)
     (xref-show-definitions-function . 'consult-xref)
     (consult-ghq-find-function . 'magit-status)
     (consult-project-root-function . #'projectile-project-root)
     (consult-ripgrep-command . "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --ignore-case . -e ARG OPTS"))
    :config
    ;; https://github.com/minad/consult/wiki#find-files-using-fd
    (defvar consult--fd-command nil)
    (defun consult--fd-builder (input)
      (unless consult--fd-command
        (setq consult--fd-command
              (if (eq 0 (call-process-shell-command "fdfind"))
                  "fdfind"
                "fd")))
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (`(,re . ,hl) (funcall consult--regexp-compiler
                                          arg 'extended t)))
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
     consult-ripgrep
     consult-recent-file
     :preview-key (kbd "C-."))
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
    (("M-q" . embark-act))
    :preface
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
    :custom
    ((embark-indicators . '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator)))
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (leaf embark-consult
    :ensure t
    :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
    :after (embark consult)
    :require t)

  (leaf dired
    :ensure all-the-icons-dired
    :custom (dired-dwim-target . t)
    :bind (("C-x C-d" . my/dired-this-buffer)
           (dired-mode-map (("C-t" . nil)
                            ("M-s" . nil)
                            ("c" . my/dired-do-copy-with-filename))))
    :config
    (leaf dired-mode
      :custom ((dired-dwim-target . t))
      :preface
      (defun my/dired-this-buffer ()
        "Open dired in this buffer."
        (interactive)
        (dired
         (file-name-directory (expand-file-name (buffer-name)))))
      ;; https://y0m0r.hateblo.jp/entry/20120219/1329657774
      (defun my/dired-view-file-other-window ()
        (interactive)
        (let ((file (dired-get-file-for-visit)))
          (if (file-directory-p file)
              (or (and (cdr dired-subdir-alist)
                       (dired-goto-subdir file))
                  (dired file))
            (view-file-other-window file)
            )))
      (defun my/dired-do-copy-with-filename (&optional arg)
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
      :mode-hydra
      ("Mark"
       (("m" dired-mark)
        ("t" dired-toggle-marks)
        ("U" dired-unmark-all-marks)
        ("u" dired-unmark))
       "Manipulate"
       (("+" dired-create-directory :exit t)
        ("M" dired-do-chmod :exit t)
        ("D" dired-do-delete :exit t)
        ("c" my/dired-do-copy-with-filename :exit t)
        ("C" dired-do-copy :exit t)
        ("R" dired-do-rename :exit t)
        ("e" wdired-change-to-wdired-mode :exit t)
        ("w" dired-copy-filename-as-kill :exit t)
        ("W" dired-get-fullpath-filename :exit t))
       "Open"
       (("o" dired-find-file-other-window :exit t)
        ("v" dired-view-file :exit t)
        ("V" my/dired-view-file-other-window :exit t)
        ("s" dired-sort-toggle-or-edit)
        ("g" revert-buffer))))

    (leaf wdired
      :require t
      :bind (dired-mode-map (("e" . wdired-change-to-wdired-mode))))

    (leaf dired-x
      :require t
      :hook (dired-mode-hook . dired-omit-mode)
      :custom (dired-omit-files . "^\\.DS_Store$"))

    (leaf dired-sidebar
      :ensure t
      :bind (("M-d" . dired-sidebar-toggle-sidebar)
             (dired-sidebar-mode-map
              ("o" . dired-sidebar-subtree-toggle)))
      :custom ((dired-sidebar-use-term-integration . t)
               (dired-sidebar-use-custom-modeline . nil))))

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
    :pretty-hydra
    ((:title (with-faicon "git" "Git commands" 1 -0.05) :quit-key "q")
     ("Magit"
      (("m" magit-status "status" :exit t)
       ("b" magit-blame "blame" :exit t))
      "Timemachine"
      (("t" git-timemachine "timemachine" :exit t))
      "Gutter"
      (("p" git-gutter:previous-hunk "previous")
       ("n" git-gutter:next-hunk "next")
       ("s" git-gutter:stage-hunk "stage")
       ("r" git-gutter:revert-hunk "revert")
       ("SPC" my/git-gutter:toggle-popup-hunk "toggle hunk"))
      "Browse"
      (("l" git-link "link" :exit t)
       ("h" git-link-homepage "homepage" :exit t)
       ("v" my/gh-pr-view "view pr" :exit t)
       ("o" my/gh-pr-create "open pr" :exit t)
       ("c" my/git-open-pr-from-commit-hash "open pr from hash" :exit t))))
    :config
    (leaf magit
      :require t
      :ensure (magit gh)
      :ensure-system-package git
      :custom ((magit-save-repository-buffers . 'dontask))
      :bind (("M-s" . magit-status-toggle)
             ("M-S" . git/body)
             (magit-status-mode-map
              ("q" . my/magit-quit-session)
              ("C-o" . magit-diff-visit-file-other-window))
             (git-commit-mode-map
              ("M-i" . my/consult-git-commit-messages)
              ("M-p" . my/consult-git-commit-prefix)))
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
                    (window-list)))
      :config
      (leaf magit-delta
        :ensure t
        :ensure-system-package (delta . git-delta)
        :after magit
        :hook (magit-mode-hook)))

    (leaf git-gutter
      :ensure t
      :preface
      (defun my/git-gutter:toggle-popup-hunk ()
        "Toggle git-gutter hunk window."
        (interactive)
        (if (and (get-buffer git-gutter:popup-buffer) (git-gutter:popup-buffer-window))
            (delete-window (git-gutter:popup-buffer-window))
          (git-gutter:popup-hunk)))
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
      :custom ((git-link-open-in-browser . t)
               (git-link-use-commit . t))))

  (leaf docker :ensure t
    :config
    (leaf docker-tramp :ensure t))

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
    (tree-sitter-load 'vue "vue")
    (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
    ;; TSX の対応
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
    ))


;;; ---------- メジャーモード設定 ----------
(leaf *major-mode
  :after major-mode-hydra
  :custom ((compilation-scroll-output . t))
  :config
  (leaf emacs-lisp-mode
    :ensure macrostep
    :mode-hydra
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Test"
     (("t" ert "prompt")
      ("T" (ert t) "all")
      ("F" (ert :failed) "failed"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" describe-function "function")
      ("v" describe-variable "variable")
      ("i" info-lookup-symbol "info lookup"))
     "Macrostep"
     (("m" macrostep-mode "macrostep-mode"))))

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
             (lsp-modeline-diagnostics-enable . nil)
             (lsp-clients-deno-import-map . "./import_map.json")
             ;; https://github.com/johnsoncodehk/volar/discussions/471
             (lsp-volar-take-over-mode . t))
    :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
    )


  (leaf web
    :config
    (leaf web-mode
      :ensure t
      :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
             "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.jsx\\'" "\\.vue")
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
                  (when (equal web-mode-engine "vue")
                    (lsp-deferred)))
                ))


    (leaf emmet-mode
      :ensure t
      :bind ((emmet-mode-keymap
              ("C-j" . company-complete)))
      :hook (html-mode-hook
             web-mode-hook
             css-mode-hook
             scss-mode-hook))

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
                            '((company-css :with company-dabbrev))))))
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
                             '((company-css :with company-dabbrev))))))
      (leaf sass-mode :ensure t)
      (leaf sws-mode :ensure t)))

  (leaf javascript
    :config
    (leaf js2-mode
      :mode (("\\.js$"))
      :hook ((js2-mode-hook . lsp-deferred)
             (js2-mode-hook . subword-mode))
      :ensure (js2-mode xref-js2)
      :custom
      ((js-indent-level . 2)
       (js-switch-indent-offset . 2)
       (js2-basic-offset . 2)
       (js2-strict-missing-semi-warning . nil)
       (xref-js2-search-program . 'rg)))

    (leaf typescript-mode
      :ensure t
      :mode-hydra
      ("REPL"
       (("n" nodejs-repl "node")
        ("t" run-ts "ts-node"))
       "Test"
       (("jf" jest-file)
        ("jp" jest-popup)
        ("jb" my/jest-current-buffer)
        ("jw" my/jest-watch-current-buffer)
        ("jcb" my/jest-copy-command-current-buffer)
        ("jcw" my/jest-copy-command-watch-current-buffer))
       "Format"
       (("p" prettier-js)
        ("d" deno-fmt)))
      :custom (typescript-indent-level . 2)
      :hook ((typescript-mode-hook . lsp-deferred)
             (typescript-mode-hook . subword-mode))
      :init
      (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))

    (leaf add-node-modules-path
      :ensure t
      :hook ((typescript-mode-hook
              js2-mode-hook
              web-mode-hook
              scss-mode-hook
              graphql-mode-hook
              ts-comint-mode-hook
              json-mode-hook) . add-node-modules-path))

    (leaf nodejs-repl :ensure t)

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

    (leaf npm-mode
      :ensure t
      :hook (typescript-mode-hook js2-mode-hook web-mode-hook scss-mode-hook))

    (leaf jest
      :ensure t
      :bind ((jest-minor-mode-map
              ("C-c C-c C-c" . jest-file-dwim)))
      :hook ((typescript-mode-hook . jest-minor-mode)
             (js2-mode-hook . jest-minor-mode)
             (web-mode-hook . jest-minor-mode)))

    (leaf prettier-js
      :ensure t
      :hook ((typescript-mode-hook
              js2-mode-hook
              web-mode-hook
              css-mode-hook
              scss-mode-hook
              graphql-mode-hook)))

    (leaf deno-fmt :require t :ensure t))

  (leaf ruby-mode
    :ensure t
    :mode ("\\.rb\\'" "Capfile$" "Gemfile$" "[Rr]akefile$")
    :interpreter ("pry")
    :custom ((ruby-insert-encoding-magic-comment . nil))
    :hook ((ruby-mode-hook . lsp-deferred)
           (ruby-mode-hook . inf-ruby-minor-mode)
           (ruby-mode-hook . inf-ruby-switch-setup))
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

  (leaf php-mode :ensure t)

  (leaf haskell-mode :ensure t)

  (leaf graphql-mode :ensure t)

  (leaf java-mode
    :hook
    (java-mode-hook . (lambda ()
                        (setq tab-width 4)
                        (setq indent-tabs-mode t)
                        (setq c-basic-offset 4))))

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

  (leaf dockerfile-mode :ensure t)

  (leaf docker-compose-mode :ensure t)

  (leaf nginx-mode :ensure t)

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

  (leaf rust-mode
    :ensure t
    :hook ((rust-mode-hook . lsp))
    :custom ((lsp-rust-server . 'rust-analyzer)
             (rust-format-on-save . t))
    :mode-hydra
    ("REPL"
     (("e" evcxr "evcxr")
      ("v" evcxr-eval-region "region")
      ("a" evcxr-eval-buffer "buffer"))
     "Build/Run"
     (("b" cargo-process-build "build")
      ("l" cargo-process-clean "clean")
      ("i" cargo-process-init "init")
      ("r" cargo-process-run "run")
      ("c" my/cargo-process-build-and-run-current-bin "run current bin")
      ("u" cargo-process-update "update"))
     "Test"
     (("t" my/cargo-process-build-and-test "build and test")
      ("f" cargo-process-current-test "current")
      ("o" cargo-process-current-file-tests "file"))
     "Lint/Format"
     (("k" cargo-process-check "check")
      ("q" cargo-process-clippy "clippy")
      ("<RET>" cargo-process-fmt "fmt"))
     "Doc"
     (("d" cargo-process-doc "doc")))
    :config
    (leaf cargo
      :preface
      (defun my/cargo-process-build-and-test ()
        (interactive)
        (cargo-process-build)
        (cargo-process-current-file-tests))

      (defun my/cargo-process-run-bin-current-buffer ()
        (interactive)
        (let ((command (file-name-sans-extension (buffer-name))))
          (cargo-process--start (concat "Run " command)
                                (concat cargo-process--command-run-bin " " command))))

      (defun my/cargo-process-build-and-run-current-bin ()
        (interactive)
        (cargo-process-build)
        (my/cargo-process-run-bin-current-buffer))
      :bind ((cargo-mode-map
              ("C-c C-c C-c" . my/cargo-process-build-and-test)))
      :ensure t
      :hook (rust-mode-hook . cargo-minor-mode))

    (leaf evcxr
      :ensure (parsec)
      :custom (evcxr-shell-enable-font-lock . nil)
      :el-get (evcxr :url "https://github.com/hyakt/evcxr-mode.git"))
    )

  (leaf fish-mode :ensure t)

  (leaf csv-mode :ensure t)

  (leaf jq-mode
    :ensure t
    :ensure-system-package jq)

  (leaf protobuf-mode :ensure t)

  (leaf org
    :bind ((org-mode-map
            ("C-," . nil)))
    :mode ("\\.txt$")
    :custom
    (org-startup-truncated . nil)
    (org-src-fontify-natively . t)
    (org-log-done . 'time)
    :config
    (leaf ox-latex
      :custom
      (org-latex-pdf-process . '("latexmk %f"))
      (org-file-apps . '(("pdf" . "/usr/bin/open -a Preview.app %s")))
      (org-latex-with-hyperref . nil)
      (org-latex-hyperref-template . nil))
    (leaf htmlize :ensure t)
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
            (clipboard-kill-ring-save (point-min) (point-max)))))))

  (leaf markdown-mode
    :ensure t
    :setq
    (markdown-command . "marked")
    :mode (("\\.markdown\\'" . gfm-mode)
           ("\\.md\\'" . gfm-mode)
           ("\\.mdown\\'" . gfm-mode))
    :hook
    (markdown-mode-hook .
                        (lambda nil
                          (set
                           (make-local-variable 'whitespace-action)
                           nil))))

  (leaf plantuml-mode
    :ensure t
    ;; :ensure-system-package (plantuml graphviz)
    :custom
    ((plantuml-executable-path . "plantuml")
     (plantuml-default-exec-mode . 'executable))))

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
