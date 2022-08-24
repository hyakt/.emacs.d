;;; init.el --- My emacs settings. -*- coding: utf-8; lexical-binding: t -*-
;; Author: hyakt <https://github.com/hyakt/.emacs.d>

;;; Commentary:
;; This is hyakt's init.el of Emacs.

;;; Code:

;; (require 'profiler)
;; (profiler-start 'cpu)

(setq gc-cons-threshold most-positive-fixnum)

(eval-and-compile
  (setq package-archives
        '(("org" . "https://orgmode.org/elpa/")
          ("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  
  (leaf el-get :ensure t))

;;; ---------- 初期設定 ----------
(leaf *basic
  :leaf-defer nil
  :leaf-autoload nil
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  (after-init-hook . global-auto-revert-mode)  ;; minibuffer をマウスカーソルで選択できないようにする
  :setq-default
  (shell-file-name . "/bin/bash")
  (explicit-shell-file-name . "/bin/bash")
  :setq
  (auto-coding-functions . nil)                                                              ;; 文字コードの自動変換保存をしない
  (completion-ignore-case . t)                                                               ;; file 名の補完で大文字小文字を区別しない
  (auto-save-default . nil)                                                                  ;; オートセーブのファイルを作らない
  (make-backup-files . t)                                                                    ;; Backup file を作る
  (backup-directory-alist . '(("\\.*$" . "~/.emacs.d/.backup")))                             ;; バックアップ先
  (create-lockfiles . nil)                                                                   ;; ロックファイル(.#filename)のファイルを作らない
  (garbage-collection-messages . nil)                                                        ;; GC 実行のメッセージを表示しない
  (message-log-max . 10000)                                                                  ;; ログの記録行数を増やす
  (vc-follow-symlinks . t)                                                                   ;; symlink は必ず追いかける
  (completion-ignored-extensions . '("~" ".o" ".elc" "./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store"))
  (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt)) ;; minibuffer をマウスカーソルで選択できないようにする
  (enable-recursive-minibuffers . t)                                                         ;; minibuffer の再帰的使用を許可する
  (enable-local-variables . :all)                                                            ;; local variable は全て使用する
  (display-warning-minimum-level . :error)                                                   ;; init.el 読み込み時の Warning を抑制
  (init-file-debug . t)
  (frame-resize-pixelwise . t)
  (history-length . 3000)
  (history-delete-duplicates . t)
  (scroll-preserve-screen-position . t)
  (scroll-conservatively . 100)
  (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
  (ring-bell-function . 'ignore)
  (text-quoting-style . 'straight)
  (custom-file . "~/emacs.d/custom.el")
  :init
  (setenv "SHELL" "/bin/bash")                                                               ;; デフォルトの shell を bash に変更
  (setenv "LANG" "ja_JP.UTF-8")                                                              ;; デフォルトの LANG を UTF-8 に設定 ruby/flyceck 対策
  (run-with-idle-timer 60.0 t #'garbage-collect)                                             ;; Run GC every 60 seconds if emacs is idle.
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf my-functions
  :load-path "~/.emacs.d/site-lisp/my-functions/"
  :hook (after-init-hook
         . (lambda ()
             (require 'my-util)
             (require 'my-prog)
             (require 'my-git)))
  :config
  (defun my/native-comp-packages ()
    (interactive)
    (native-compile-async "~/.emacs.d/init.el")
    (native-compile-async "~/.emacs.d/site-lisp" 'recursively)))

(leaf server
  :require t
  :hook (emacs-startup-hook
         . (lambda () (unless (server-running-p) (server-start)))))

(leaf for-macos
  :require ucs-normalize
  :when (eq system-type 'darwin)
  :hook (after-init-hook . mac-auto-ascii-mode)
  :setq
  (file-name-coding-system . 'utf-8-hfs)
  (locale-coding-system . 'utf-8-hfs)
  :config
  (prefer-coding-system 'utf-8))

(leaf for-linux
  :when (eq system-type 'gnu/linux)
  :setq
  (file-name-coding-system . 'utf-8)
  (locale-coding-system . 'utf-8)
  (x-alt-keysym . 'meta)
  (x-super-keysym . 'meta)
  :config
  (prefer-coding-system 'utf-8))

(leaf exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :setq
  (exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-arguments . nil)
  :config
  (exec-path-from-shell-initialize))

(leaf savehist
  :init
  (savehist-mode))

(leaf recentf
  :hook (after-init-hook . recentf-mode)
  :setq
  (recentf-max-saved-items . 1000)
  (recentf-exclude . '("/\\.emacs\\.d/recentf" "COMMIT_EDITMSG" "^/sudo:" "/\\.emacs\\.d/elpa/"))
  (recentf-auto-cleanup . 'never))

;;; ---------- 外観設定 ----------
(leaf *appearance
  :leaf-defer nil
  :leaf-autoload nil
  :hook
  (window-setup-hook . frame-size-resume)
  (kill-emacs-hook . frame-size-save)
  :setq
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
  (compilation-scroll-output . t)
  (tab-width . 2)                                             ;; タブの幅は半角スペース 2
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
  transient-mark-mode                                         ;; 選択部分のハイライト
  global-font-lock-mode                                       ;; フォントロックモード
  line-number-mode                                            ;; 行番号を表示
  column-number-mode                                          ;; 列番号を表示
  :init
  (set-scroll-bar-mode nil)                                   ;; スクロールバーを使わない
  
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
  
  :config
  (set-face-attribute 'default nil
                      :family "Source Han Code JP"
                      :height 110)
  (set-face-attribute 'variable-pitch nil
                      :family "Myrica M"
                      :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Source Han Code JP")))

(leaf cursor
  :leaf-autoload nil
  :when (eq system-type 'darwin)
  :hook (mac-selected-keyboard-input-source-change-hook
         . mac-selected-keyboard-input-source-change-hook-func)
  :init
  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードが英語の時はカーソルの色を青に、日本語の時は青にする
    (set-cursor-color (if (or
                           (string-match "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese" (mac-input-source))
                           (string-match "com.apple.inputmethod.Kotoeri.Japanese" (mac-input-source))
                           (string-match "com.google.inputmethod.Japanese.Roman" (mac-input-source)))
                          "#FF5996" "#51AFEF"))))

(leaf paren
  :hook (after-init . show-paren-mode)
  :bind ("M-o" . my/jump-to-match-parens)
  :setq
  (show-paren-style . 'mixed)
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t)
  :defer-config
  (defun my/jump-to-match-parens nil
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
              (goto-char beg)))) t))))

(leaf whitespace
  :hook after-init-hook
  :setq
  (whitespace-style
   . '(face
       spaces
       space-mark
       tabs
       tab-mark
       trailing
       empty))
  (whitespace-display-mappings
   . '(
       (space-mark
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
  (whitespace-action . '(auto-cleanup))
  (whitespace-space-regexp . "\\(\u3000\\)"))

(leaf all-the-icons :ensure t)

(leaf doom-themes
  :ensure t
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/my-themes")
  :config
  (load-theme 'my-doom-tokyo-night t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :setq
  (doom-modeline-buffer-encoding . nil)
  (doom-modeline-buffer-file-name-style . 'auto)
  (doom-modeline-height . 32)
  (doom-modeline-bar-width . 3)
  (doom-modeline-enable-word-count . 5)
  (doom-modeline-vcs-max-length . 30))

(leaf beacon
  :ensure t
  :hook after-init-hook)

(leaf volatile-highlights
  :ensure t
  :hook after-init-hook)

(leaf dashboard
  :ensure t
  :setq
  (dashboard-items . '((recents  . 10)
                       (projects . 10)))
  (dashboard-startup-banner . 'logo)
  :config
  (dashboard-setup-startup-hook))

;;; ---------- 編集機能設定 ----------
(leaf *edit
  :leaf-defer nil
  :leaf-autoload nil
  :ensure unicode-escape
  :bind ("M-e" . edit/body)
  :config
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  
  (pretty-hydra-define
    edit
    (:title (with-faicon "code" "Window & Edit" 1 -0.05) :quit-key "q")
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
      ("d" my/delete-or-remove-this-file "delete" :exit t)))))

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
  :init
  (global-unset-key (kbd "C-z")))

(leaf elec-pair
  :hook
  (prog-mode-hook . electric-pair-mode)
  (org-mode-hook . org-add-electric-pairs)
  ((web-mode-hook typescript-mode-hook) . web-add-electric-pairs)
  :commands org-add-electric-pairs web-add-electric-pairs
  :defer-config
  (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
  (defun org-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defvar web-electric-pairs '((?< . ?>) (?' . ?') (?` . ?`)) "Electric pairs for web-mode.")
  (defun web-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs web-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (defun my/inhibit-electric-pair-mode (char)
    (minibufferp))

  (setq electric-pair-inhibit-predicate #'my/inhibit-electric-pair-mode))

(leaf ediff
  :setq
  (ediff-split-window-function . 'split-window-horizontally))

(leaf smerge-mode
  :setq
  (smerge-command-prefix . "\C-c\C-m")
  :hook (magit-diff-visit-file-hook . (lambda ()
                                        (when smerge-mode
                                          (my/hydra-smerge/body))))
  :config
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (defhydra unpackaged/smerge-hydra
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
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(leaf disable-mouse
  :ensure t
  :setq (disable-mouse-wheel-events . '("wheel-left" "wheel-right")))

(leaf tempel
  :ensure t
  :bind ("<tab>" . my/tempel-maybe-expand)
  :setq (tempel-path . "~/.emacs.d/site-lisp/templates")
  :defer-config
  (define-key tempel-map [remap my/tempel-maybe-expand] #'tempel-next)
  (define-key tempel-map "\C-g" #'tempel-done)
  (defun my/tempel-maybe-expand ()
    (interactive)
    (if (tempel-expand)
        (tempel-expand t)
      (indent-for-tab-command))))

;; for lsp-mode
(leaf yasnippet
  :ensure t
  :hook (lsp-mode-hook . (lambda () (yas-minor-mode t))))

(leaf corfu
  :ensure t
  :hook prog-mode-hook
  :bind ("C-j" . completion-at-point)
  :setq
  (corfu-min-width . 30)
  (corfu-auto . t)
  (corfu-preview-current . nil)
  (corfu-scroll-margin . 0)
  (corfu-quit-at-boundary . nil))

(leaf cape
  :ensure t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(leaf corfu-doc
  :ensure t
  :after corfu
  :hook corfu-mode-hook
  :setq
  (corfu-doc-auto . t)
  (corfu-doc-delay . 5))

(leaf kind-icon
  :ensure t
  :after corfu
  :setq
  (kind-icon-default-face . 'corfu-default) ; to compute blended backgrounds correctly)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf flycheck
  :ensure t
  :hook prog-mode-hook)

(leaf smart-jump
  :ensure (t dumb-jump)
  :bind
  ("M-." . smart-jump-go)
  ("M-," . smart-jump-back)
  ("M-'" . smart-jump-references)
  :setq (smart-jump-bind-keys . nil)
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
  :ensure t
  :bind
  ("M-p" . jumplist-previous)
  ("M-n" . jumplist-next)
  :setq
  (jumplist-hook-commands
   . '(avy-goto-char
       mouse-set-point
       smart-jump-go smart-jump-ref
       lsp-ui-peek-find-definitions lsp-ui-peek-find-references
       xref-find-definitions xref-find-references
       dump-jump-go
       my/jump-to-match-parens
       consult-line consult-ripgrep consult-find consult-ghq-find consult-fd
       er/expand-region
       end-of-buffer beginning-of-buffer))
  (jumplist-ex-mode . t))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook))

(leaf rainbow-mode
  :ensure t
  :hook (js2-mode-hook css-mode-hook html-mode-hook typescript-mode-hook))

(leaf symbol-overlay
  :ensure t
  :hook (prog-mode-hook markdown-mode-hook)
  :bind ("C-." . symbol-overlay-put))

(leaf yafolding
  :ensure t
  :hook prog-mode-hook)

(leaf visual-regexp
  :hook (visual-regexp-mode-hook . (lambda () (require 'visual-regexp-steroids)))
  :ensure (t visual-regexp-steroids pcre2el)
  :bind ("C-r" . vr/query-replace)
  :setq
  (case-fold-search . nil)
  (vr/engine . 'pcre2el))

(leaf multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this))

(leaf expand-region
  :ensure t
  :bind
  ("C-," . er/expand-region)
  ("C-M-," . er/contract-region))

(leaf undo-fu
  :ensure t
  :bind
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

(leaf wgrep
  :ensure t
  :setq
  (wgrep-enable-key . "e")
  (wgrep-auto-save-buffer . t)
  (wgrep-change-readonly-file . t))

(leaf string-inflection
  :ensure t
  :bind ("M-[" . string-inflection-all-cycle))

(leaf rg
  :ensure t
  :setq
  (rg-group-result . t)
  (rg-custom-type-aliases . '(("graphql" . "*.gql *.graphql"))))

(leaf pangu-spacing
  :hook (after-init-hook . (lambda () (require 'pangu-spacing)))
  :ensure t
  :setq (pangu-spacing-real-insert-separtor . t)
  :config
  (defun my/pangu-spacing-region (beg end)
    "Replace regexp with match in region."
    (interactive "r")
    (pangu-spacing-search-buffer
     pangu-spacing-include-regexp beg (+ end 8) (replace-match "\\1 \\2" nil nil))))

(leaf avy
  :ensure t
  :bind ("C-;" . avy-goto-char))

(leaf google-this :ensure t)

;;; ---------- インターフェース設定 ----------
(leaf hydra :ensure t)

(leaf major-mode-hydra
  :ensure t
  :setq
  (major-mode-hydra-invisible-quit-key . "q")
  :bind ("M-a" . major-mode-hydra))

(leaf eldoc
  :config
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

(leaf dired
  :bind (("C-x C-d" . my/dired-this-buffer)
         (dired-mode-map (("C-t" . nil)
                          ("M-s" . nil)
                          ("c" . my/dired-do-copy-with-filename))))
  :ensure all-the-icons-dired
  :setq ((dired-dwim-target . t))
  :config
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
      ("g" revert-buffer)))))

(leaf wdired
  :hook (dired-mode-hook . (lambda () (require 'wdired)))
  :bind (dired-mode-map (("e" . wdired-change-to-wdired-mode))))

(leaf dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :setq (dired-omit-files . "^\\.DS_Store$"))

(leaf dired-sidebar
  :ensure t
  :bind
  ("M-d" . dired-sidebar-toggle-sidebar)
  (dired-sidebar-mode-map
   ("o" . dired-sidebar-subtree-toggle))
  :setq
  (dired-sidebar-use-term-integration . t)
  (dired-sidebar-use-custom-modeline . nil))

(leaf which-key
  :ensure t
  :hook prog-mode-hook)

(leaf swap-buffers
  :ensure t
  :bind ("C-x C-o" . swap-buffers))

(leaf other-window-or-split
  :init
  (el-get-bundle other-window-or-split
    :url "https://github.com/conao3/other-window-or-split.git")
  :bind
  ("C-t" . my/ws-other-window-or-split-and-kill-minibuffer)
  ("C-S-t" . ws-previous-other-window-or-split)
  :setq (ws-split-window-width-with-em . 130)
  :config
  (defun minibuffer-keyboard-quit () ;; esc quits
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  
  (defun my/ws-other-window-or-split-and-kill-minibuffer ()
    (interactive)
    (if (active-minibuffer-window)
        (progn
          (minibuffer-keyboard-quit)
          (ws-other-window-or-split))
      (ws-other-window-or-split))))

(leaf eyebrowse
  :ensure t
  :hook prog-mode-hook
  :setq
  (eyebrowse-new-workspace . t)
  (eyebrowse-keymap-prefix . "\C-z"))

(leaf shackle
  :ensure t
  :config
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
  :setq
  (shackle-select-reused-windows . t)
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
                   (magit-status-mode          :align below :ratio 0.6 :select t)
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
                   ;; rust
                   ("\\`\\*Cargo .*?\\*\\'"    :align below :regexp t :setq my/shackle-cargo-custom)
                   ("*Evcxr*"                  :align below :select t)
                   ;; ruby
                   ("*rspec-compilation*"      :align below :ratio 0.5)
                   )))

(leaf projectile
  :ensure t
  :hook prog-mode-hook
  :bind (("C-x t" . my/projectile-toggle-between-implementation-and-test-other-window))
  :setq
  (projectile-add-known-project . '("~/repos/"))
  :preface
  (defun my/projectile-toggle-between-implementation-and-test-other-window ()
    "Toggle between an implementation file and its test file."
    (interactive)
    (find-file-other-window
     (projectile-find-implementation-or-test
      (buffer-file-name)))))

(leaf consult
  :ensure t consult-flycheck consult-ghq consult-ls-git
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x C-b" . consult-buffer)
         ("C-x f" . consult-fd)
         ("C-x e" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ("C-x C-g" . consult-ghq-find)
         ("C-x g" . consult-ls-git)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("C-s" . consult-line)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu))
  :setq
  (xref-show-xrefs-function . 'consult-xref)
  (xref-show-definitions-function . 'consult-xref)
  (consult-ghq-find-function . 'find-file)
  (consult-project-root-function . #'projectile-project-root)
  (consult-ripgrep-command . "rg --null --line-buffered --color=ansi --max-columns=1000 --no-heading --line-number --ignore-case . -e ARG OPTS")
  :defer-config
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
   find-file
   consult-ripgrep
   consult-recent-file
   consult-ls-git
   :preview-key (kbd "C-.")))

(leaf vertico
  :ensure t
  :setq (vertico-count . 30)
  :init
  (vertico-mode))

(leaf orderless
  :ensure t
  :commands (orderless-filter)
  :config
  (if (fboundp #'migemo-get-pattern)
      (defun orderless-migemo (component)
        "Match COMPONENT as `migemo'."
        (let ((pattern (migemo-get-pattern component)))
          (condition-case nil
              (progn (string-match-p pattern "") pattern)
            (invalid-regexp nil)))))

  (if (and (boundp 'orderless-matching-styles)
           (fboundp #'orderless-migemo))
      (add-to-list 'orderless-matching-styles #'orderless-migemo t)))

(leaf fussy
  :ensure t
  :setq
  (completion-styles . '(fussy))
  (completion-category-defaults . nil)
  (completion-category-overrides . nil)
  (fussy-filter-fn . 'fussy-filter-orderless)
  (fussy-score-fn . 'fussy-fzf-native-score)
  :config
  (el-get-bundle fzf-native
    :url "https://github.com/dangduc/fzf-native.git")
  :defer-config
  (fzf-native-load-dyn))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode))

(leaf embark
  :ensure t
  :bind
  ("M-q" . embark-act)
  :setq
  (embark-indicators
   . '(embark-which-key-indicator
       embark-highlight-indicator
       embark-isearch-highlight-indicator))
  :config
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

(leaf embark-consult
  :ensure t
  :require t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
  :after (embark consult))

(leaf migemo
  :ensure t
  :require t
  :setq
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs" "-i" "\a"))
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  (migemo-dictionary . "/usr/local/share/migemo/utf-8/migemo-dict")
  :config
  (migemo-init))

(leaf eshell
  :setq
  (eshell-cmpl-ignore-case . t)
  (eshell-ask-to-save-history . 'always))

(leaf vterm
  :ensure (t vterm-toggle)
  :bind (("M-t" . vterm-toggle)
         (vterm-mode-map (("C-t" . nil))))
  :setq
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  ;; delete "C-h", add <f1> and <f2>
  (vterm-keymap-exceptions
   . '("<f1>" "<f2>" "C-c" "C-x" "C-u" "C-g" "C-l" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "C-t" "M-t" "M-s"))
  (vterm-toggle-reset-window-configration-after-exit . t)
  (vterm-toggle-scope . 'project)
  (vterm-toggle-fullscreen-p . nil)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname
                                      (or (equal major-mode 'vterm-mode)
                                          (string-prefix-p vterm-buffer-name bufname))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

(leaf magit
  :ensure (magit gh)
  :setq ((magit-save-repository-buffers . 'dontask))
  :bind (("M-S" . git/body)
         ("M-s" . magit-status-toggle)
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
  (pretty-hydra-define
    git
    (:title (with-faicon "git" "Git commands" 1 -0.05) :quit-key "q")
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
     "Link"
     (("l" git-link "link" :exit t)
      ("h" git-link-homepage "homepage" :exit t))
     "GH"
     (("v" my/gh-pr-view "view pr" :exit t)
      ("c" my/gh-pr-create "create pr" :exit t)
      ("o" my/git-open-pr-from-commit-hash "open pr from hash" :exit t))))
  )

(leaf magit-delta
  :ensure t
  :after magit
  :hook (magit-mode-hook))

(leaf git-gutter
  :ensure t
  :hook prog-mode-hook
  :preface
  (defun my/git-gutter:toggle-popup-hunk ()
    "Toggle git-gutter hunk window."
    (interactive)
    (if (and (get-buffer git-gutter:popup-buffer) (git-gutter:popup-buffer-window))
        (delete-window (git-gutter:popup-buffer-window))
      (git-gutter:popup-hunk)))
  :setq
  (git-gutter:modified-sign . " ")
  (git-gutter:added-sign    . " ")
  (git-gutter:deleted-sign  . " ")
  :custom-face
  (git-gutter:modified . '((t (:background "#B4DCE7"))))
  (git-gutter:added    . '((t (:background "#74DFC4"))))
  (git-gutter:deleted  . '((t (:background "#964C7B")))))

(leaf git-timemachine :ensure t)

(leaf git-link
  :ensure t
  :setq
  (git-link-open-in-browser . t)
  (git-link-use-commit . t))

(leaf docker
  :ensure (t docker-tramp))

(leaf open-junk-file
  :ensure t
  :bind (("C-`" . open-junk-file))
  :init
  (defvaralias 'open-junk-file-format 'open-junk-file-directory "Temporary alias for Emacs27")
  :setq
  (open-junk-file-format . "~/Documents/junk/%Y-%m-%d-%H%M%S."))

(leaf tree-sitter
  :ensure (t tree-sitter-langs)
  :defer-config
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
     ]))

;;; ---------- メジャーモード設定 ----------
(leaf lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind
  (lsp-mode-map
   ("C-c i" . lsp-execute-code-action))
  :setq
  (lsp-enable-indentation . nil)
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
  (lsp-volar-take-over-mode . t)
  (lsp-completion-provider . :none)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'"))

(leaf emacs-lisp-mode
  :ensure macrostep
  :config
  (major-mode-hydra-define emacs-lisp-mode
    (:quit-key "q" :title (concat (all-the-icons-fileicon "elisp") " Emacs Lisp"))
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
     (("m" macrostep-mode "macrostep-mode")))))

(leaf web-mode
  :ensure t
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
         "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.jsx\\'" "\\.vue" "\\.astro")
  :setq
  (web-mode-indent-style . 2)
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
                              ("php" . "/*")))
  (web-mode-enable-front-matter-block . t) ;ignore Front Matter Data
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (equal web-mode-engine "vue")
                (lsp-deferred))))

  (defun my/emmet-change-at-point ()
    (interactive)
    (let ((web-mode-cur-language
           (web-mode-language-at-pos)))
      (if (string= web-mode-cur-language "css")
          (and
           (setq emmet-use-css-transform t)
           (message "css transform"))
        (progn
          (setq emmet-use-css-transform nil)
          (message "html transform")
          ))))

  (major-mode-hydra-define web-mode
    (:quit-key "q" :title (concat (all-the-icons-alltheicon "html5") " Web mode"))
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
      ("v" web-mode-element-vanish "vanish")
      ("r" lsp-rename "rename"))
     "Test"
     (("tf" jest-file)
      ("tp" jest-popup)
      ("tb" my/jest-current-buffer)
      ("tw" my/jest-watch-current-buffer)
      ("tcb" my/jest-copy-command-current-buffer)
      ("tcw" my/jest-copy-command-watch-current-buffer))
     "Format"
     (("p" prettier-js)
      ("d" deno-fmt))
     "Misc"
     (("e" my/emmet-change-at-point)))
    ))

(leaf emmet-mode
  :ensure t
  :bind ((emmet-mode-keymap
          ("C-j" . completion-at-point)))
  :hook (html-mode-hook
         web-mode-hook
         css-mode-hook
         scss-mode-hook))

(leaf slim-mode :ensure t)

(leaf haml-mode :ensure t)

(leaf css-mode
  :setq (css-indent-offset . 2))

(leaf scss-mode
  :ensure t
  :hook
  (scss-mode-hook
   . (lambda ()
       (set
        (make-local-variable 'flycheck-checker)
        (setq flycheck-checker 'general-stylelint)
        )))
  :setq (scss-indent-offset . 2)
  :defer-config
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
  (add-to-list 'flycheck-checkers 'general-stylelint))

(leaf sass-mode :ensure t)

(leaf sws-mode :ensure t)

(leaf js2-mode
  :ensure (js2-mode xref-js2)
  :mode ("\\.[mc]?js$" )
  :hook
  (js2-mode-hook . lsp-deferred)
  (js2-mode-hook . subword-mode)
  :setq
  (js-indent-level . 2)
  (js-switch-indent-offset . 2)
  (js2-basic-offset . 2)
  (js2-strict-missing-semi-warning . nil)
  (xref-js2-search-program . 'rg))

(leaf typescript-mode
  :ensure t
  :hook
  (typescript-mode-hook . lsp-deferred)
  (typescript-mode-hook . subword-mode)
  (typescript-mode-hook . tree-sitter-mode)
  :setq (typescript-indent-level . 2)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  :config
  (major-mode-hydra-define typescript-mode
    (:quit-key "q" :title (concat (all-the-icons-fileicon "typescript") " TypeScript"))
    ("REPL"
     (("n" nodejs-repl "node")
      ("t" run-ts "ts-node"))
     "Editing"
     (("r" lsp-rename "rename"))
     "Test"
     (("jf" jest-file)
      ("jp" jest-popup)
      ("jb" my/jest-current-buffer)
      ("jw" my/jest-watch-current-buffer)
      ("jcb" my/jest-copy-command-current-buffer)
      ("jcw" my/jest-copy-command-watch-current-buffer))
     "Format"
     (("p" prettier-js)
      ("d" deno-fmt)))))

(leaf add-node-modules-path
  :ensure t
  :hook
  ((typescript-mode-hook
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
  :bind (typescript-mode-map
         (("C-x C-e" . ts-send-last-sexp)
          ("C-c b" . ts-send-buffer)
          ("C-c r" . ts-send-region)))
  :commands (run-ts)
  :setq (ts-comint-program-command . "ts-node"))

(leaf jest
  :ensure t
  :bind
  (jest-minor-mode-map
   ("C-c C-c C-c" . jest-file-dwim))
  :hook
  (typescript-mode-hook . jest-minor-mode)
  (js2-mode-hook . jest-minor-mode)
  (web-mode-hook . jest-minor-mode)
  :setq (jest-executable . "npx jest"))

(leaf prettier-js
  :ensure t
  :hook
  (typescript-mode-hook
   js2-mode-hook
   web-mode-hook
   css-mode-hook
   scss-mode-hook
   graphql-mode-hook)
  :setq (prettier-js-show-errors . nil))

(leaf deno-fmt :ensure t)

(leaf ruby-mode
  :ensure t
  :mode ("\\.rb\\'" "Capfile$" "Gemfile$" "[Rr]akefile$")
  :hook
  (ruby-mode-hook . lsp-deferred)
  (ruby-mode-hook . inf-ruby-minor-mode)
  (ruby-mode-hook . inf-ruby-switch-setup)
  :interpreter ("pry")
  :setq ((ruby-insert-encoding-magic-comment . nil)))

(leaf inf-ruby
  :ensure t
  :bind
  (inf-ruby-minor-mode-map
   ("C-c C-b" . ruby-send-buffer)
   ("C-c C-l" . ruby-send-line))
  :init
  (defalias 'pry 'inf-ruby)
  :setq
  (inf-ruby-default-implementation . "pry")
  (inf-ruby-eval-binding . "Pry.toplevel_binding"))

(leaf rubocop  :ensure t)

(leaf rspec-mode
  :ensure t
  :bind
  (rspec-mode-map
   ("C-c C-c C-c" . rspec-verify-single)))

(leaf php-mode :ensure t)

(leaf haskell-mode :ensure t)

(leaf graphql-mode :ensure t)

(leaf java-mode
  :hook
  (java-mode-hook
   . (lambda ()
       (setq tab-width 4)
       (setq indent-tabs-mode t)
       (setq c-basic-offset 4))))

(leaf swift-mode :ensure t)

(leaf dart-mode
  :ensure t
  :setq
  (dart-format-on-save . nil)
  (dart-enable-analysis-server . nil)
  (dart-sdk-path . "~/repos/github.com/flutter/flutter/bin/cache/dart-sdk/"))

(leaf flutter
  :ensure t
  :setq
  (flutter-sdk-path . "~/repos/github.com/flutter/flutter/"))

(leaf go-mode
  :ensure t
  :hook (go-mode-hook . lsp-deferred))

(leaf elixir-mode
  :ensure t)

(leaf scala-mode
  :ensure t
  :interpreter ("scala"))

(leaf rust-mode
  :ensure t
  :hook (rust-mode-hook . lsp)
  :setq
  (lsp-rust-server . 'rust-analyzer)
  (rust-format-on-save . t)
  :config
  (major-mode-hydra-define rust-mode
    (:quit-key "q" :title (concat (all-the-icons-alltheicon "rust") "Rust"))
    ("Build/Run"
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
     (("d" cargo-process-doc "doc")))))

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

(leaf csharp-mode
  :ensure t
  :hook
  (csharp-mode-hook . lsp-deferred)
  (csharp-mode-hook . unity-mode)
  :config
  (el-get-bundle unity
    :url "https://github.com/elizagamedev/unity.el.git"))

(leaf sql-mode
  :ensure (sqlup-mode sqlformat)
  :hook
  (sql-interactive-mode-hook
   . (lambda ()
       (buffer-face-set 'variable-pitch)
       (toggle-truncate-lines t)))
  (sql-mode-hook sql-interactive-mode-hook)
  :mode (".sql$")
  :config
  (defun my/sql-indent-region (beg end)
    "Indent the SQL statement in the BEG to END (region)."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (sql-indent-buffer)))))

(leaf dockerfile-mode :ensure t)

(leaf docker-compose-mode :ensure t)

(leaf nginx-mode :ensure t)

(leaf fish-mode :ensure t)

(leaf csv-mode :ensure t)

(leaf jq-mode
  :ensure t)

(leaf protobuf-mode :ensure t)

(leaf org
  :bind ((org-mode-map
          ("C-," . nil)))
  :mode ("\\.txt$")
  :setq
  (org-startup-truncated . nil)
  (org-src-fontify-natively . t)
  (org-log-done . 'time))

(leaf ox-latex
  :setq
  (org-latex-pdf-process . '("latexmk %f"))
  (org-file-apps . '(("pdf" . "/usr/bin/open -a Preview.app %s")))
  (org-latex-with-hyperref . nil)
  (org-latex-hyperref-template . nil))

(leaf htmlize :ensure t)

(leaf ox-gfm :ensure t)

(leaf org-bullets :ensure t
  :defer-config
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
  :hook
  (markdown-mode-hook
   . (lambda nil
       (set
        (make-local-variable 'whitespace-action)
        nil)))
  :setq
  (markdown-command . "marked")
  :mode
  ("\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.mdown\\'" . gfm-mode)
  :setq
  (markdown-hide-urls . nil)
  (markdown-hide-markup . nil)
  (markdown-fontify-code-block-natively . t)
  (markdown-gfm-additional-languages . '("Mermaid"))
  (markdown-css-paths . '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown-light.min.css"
                          "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/default.min.css"))
  (markdown-live-preview-window-function . 'markdown-live-preview-window-xwidget-webkit)
  (markdown-xhtml-body-preamble . "<article class='markdown-body'>")
  (markdown-xhtml-body-epilogue . "</article>")
  (markdown-xhtml-header-content . "
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
  :config
  (defun markdown-live-preview-window-xwidget-webkit (file)
    "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
    (let ((uri (format "file://%s" file)))
      (xwidget-webkit-browse-url uri)
      xwidget-webkit-last-session-buffer)))

(leaf plantuml-mode
  :ensure t
  :setq
  (plantuml-executable-path . "plantuml")
  (plantuml-default-exec-mode . 'executable))

(setq gc-cons-threshold 1073741824)

;; (profiler-report)
;; (profiler-stop)

;; Local Variables:
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; init.el ends here
