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

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf el-get :ensure t)
    (leaf system-packages :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))


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
(leaf *basic
  :config
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
    :ensure t
    :if (eq system-type 'darwin)
    :custom((exec-path-from-shell-variables '("PATH" "GOPATH")))
    :config
    (exec-path-from-shell-initialize))

  (leaf *keybinds
    :config
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
    (bind-key (kbd "M-t") 'my/open-hyper-current-buffer)))


;;; ---------- 外観設定 ----------
(leaf *appearance
  :config
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

  (leaf
    :ensure t
    all-the-icons)

  (leaf doom-themes
    :ensure t
    :init
    (load "my-doom-tokyo-night")
    :config
    (load-theme 'doom-vibrant t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))

  (leaf doom-modeline
    :ensure t
    :custom ((doom-modeline-buffer-encoding . t)
             (doom-modeline-buffer-file-name-style . 'truncate-with-project)
             (doom-modeline-height . 32)
             (doom-modeline-bar-width . 3))
    :config
    (doom-modeline-mode 1))

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
    :config
    (beacon-mode 1))

  (leaf dimmer
    :ensure t
    :config
    (dimmer-mode))

  (leaf volatile-highlights
    :ensure t
    :config
    (volatile-highlights-mode t))

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
    :config
    (global-whitespace-mode 1))

  (leaf indent-guide
    :ensure t
    :custom (indent-guide-char . ">")
    :config
    (indent-guide-global-mode))

  (leaf disable-mouse
    :ensure t
    :custom (disable-mouse-wheel-events . '("wheel-left" "wheel-right"))
    :config
    (global-disable-mouse-mode)))


;;; ---------- 編集機能設定 ----------
(leaf *edit
  :config
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
    (global-flycheck-mode t)
    (leaf flycheck-posframe :ensure t :hook flycheck-mode-hook))

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
                               swiper counsel-find-file counsel-switch-buffer
                               counsel-rg counsel-projectile-switch-project counsel-git counsel-projectile
                               end-of-buffer beginning-of-buffer))
     (jumplist-ex-mode . t)))

  (leaf rainbow-delimiters
    :ensure t
    :hook (prog-mode-hook))

  (leaf rainbow-mode
    :ensure
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
           (web-mode-hook . web-add-electric-pairs))
    :config
    (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
    (defun org-add-electric-pairs ()p
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
    :bind (("M-[" . string-inflection-all-cycle))))


;;; ---------- メジャーモード設定 ----------
(leaf *major-mode
  :config
  (leaf lsp-mode
    :ensure t
    :commands lsp
    :bind ((lsp-mode-map
            ("C-c i" . lsp-execute-code-action)))
    :custom ((lsp-enable-indentation . nil)
             (lsp-eldoc-render-all . t)
             (lsp-signature-auto-activate .t)
             (lsp-signature-render-documentation . t)
             (lsp-enable-snippet . nil)
             (lsp-headerline-breadcrumb-enable . nil)))

  (leaf *web
    :config
    (leaf web-mode
      :straight (web-mode :type git :host github :repo "hyakt/web-mode")
      :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.[jt]sx\\'")
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
      :hook
      (web-mode-hook .
                     (lambda ()
                       ((when (equal web-mode-content-type "jsx")
                          (setq emmet-expand-jsx-className\? t)
                          (add-to-list 'web-mode-indentation-params
                                       '("lineup-args"))
                          (add-to-list 'web-mode-indentation-params
                                       '("lineup-calls"))
                          (add-to-list 'web-mode-indentation-params
                                       '("lineup-concats"))
                          (add-to-list 'web-mode-indentation-params
                                       '("lineup-ternary"))
                          (flycheck-add-mode 'javascript-eslint 'web-mode))
                        (when (and
                               (stringp buffer-file-name)
                               (string-match "\\.jsx\\'" buffer-file-name))
                          (tern-mode)
                          (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                          (set
                           (make-local-variable 'company-backends)
                           '((company-tern :with company-dabbrev-code)
                             company-yasnippet)))
                        (when (and
                               (stringp buffer-file-name)
                               (string-match "\\.tsx\\'" buffer-file-name))
                          (tide-setup)
                          (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
                          (set
                           (make-local-variable 'company-backends)
                           '((company-tide)
                             company-css company-yasnippet))
                          (defun company-tide-advice (orig-fun &rest args)
                            (if (and
                                 (eq
                                  (car args)
                                  'prefix)
                                 (web-mode-is-css-string
                                  (point)))
                                'nil
                              (apply orig-fun args)))

                          (advice-add 'company-tide :around #'company-tide-advice)
                          (defun web-mode-language-at-pos-advice (orig-fun &rest args)
                            (let ((pos (or
                                        (car args)
                                        (point))))
                              (or
                               (and
                                (web-mode-is-css-string pos)
                                "css")
                               (apply orig-fun args))))
                          (advice-add 'web-mode-language-at-pos :around #'web-mode-language-at-pos-advice))))))

    (leaf emmet-mode
      :ensure t
      :bind ((emmet-mode-keymap
              ("C-j" . company-complete)))
      :hook (html-mode-hook web-mode-hook css-mode-hook scss-mode-hook))

    (leaf add-node-modules-path
      :ensure t
      :commands add-node-modules-path
      :hook (typescript-mode-hook js2-mode-hook web-mode-hook scss-mode-hook graphql-mode-hook))

    (leaf prettier-js
      :ensure t
      :hook (graphql-mode-hook js2-mode-hook scss-mode-hook css-mode-hook))

    (leaf *html
      :config
      (leaf slim-mode :ensure t)
      (leaf haml-mode :ensure t))

    (leaf *css
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
        :hook
        (css-mode-hook . ((lambda ()
                            (set
                             (make-local-variable 'flycheck-checker)
                             (setq flycheck-checker 'scss-stylelint))
                            (set
                             (make-local-variable 'company-backends)
                             '((company-css :with company-dabbrev) company-yasnippet))))))
      (leaf sass-mode :ensure t)
      (leaf sws-mode :ensure t))

    (leaf *javascript
      :config
      (leaf js2-mode
        :ensure (js2-mode tern xref-js2)
        :straight (company-tern :host github :repo "emacsattic/company-tern" :branch "master")
        :custom
        ((js-indent-level . 2)
         (js-switch-indent-offset . 2)
         (js2-basic-offset . 2)
         (js2-strict-missing-semi-warning . nil)
         (xref-js2-search-program . 'rg))
        :hook
        (js2-mode-hook . ((lambda ()
                            (tern-mode)
                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                            (set
                             (make-local-variable 'company-backends)
                             '((company-tern :with company-dabbrev-code)
                               company-yasnippet))))))

      (leaf typescript-mode
        :ensure t
        :after tide
        :custom (typescript-indent-level . 2)
        :hook (typescript-mode-hook . ((lambda ()
                                         (tide-setup)
                                         (tide-hl-identifier-mode)
                                         (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)))))

      (leaf tide
        :ensure t
        :after (company flycheck)
        :bind (tide-mode-map
               ("M-." . nil)
               ("M-," . nil))
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
    :config
    (leaf inf-ruby
      :ensure t
      :bind ((inf-ruby-minor-mode-map
              ("C-c C-b" . ruby-send-buffer)
              ("C-c C-l" . ruby-send-line)))
      :custom
      (inf-ruby-default-implementation . "pry")
      (inf-ruby-eval-binding . "Pry.toplevel_binding")
      :config
      (defalias 'pry 'inf-ruby))

    (leaf rspec-mode
      :ensure t
      :bind ((rspec-mode-map
              ("C-c C-c C-c" . rspec-verify-single))))

    (leaf robe
      :ensure t
      :bind ((robe-mode-map
              ("M-." . smart-jump-go)))
      :hook
      (ruby-mode-hook)
      (robe-mode-hook . (lambda ()
                          (advice-add 'company-box--get-buffer :around #'company-box-set-current-buffer)
                          (advice-add 'company-box-doc :around #'hack-company-box-doc)
                          (setq-local company-box-doc-enable nil)
                          (company-box-mode nil)
                          (set (make-local-variable 'company-backends)
                               '((company-robe)))
                          (robe-start)))
      :config
      (defun company-box-set-current-buffer (orig-fun &rest args)
        (let ((company-box-buffer (apply orig-fun args))
              (from-buffer (current-buffer)))
          (with-current-buffer company-box-buffer
            (setq-local company-box--from-buffer from-buffer))
          company-box-buffer))

      (defun hack-company-box-doc (orig-fun &rest args)
        (with-current-buffer company-box--from-buffer
          (apply orig-fun args)))))

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
      :straight (scala-bootstrap :type git :host github :repo "tarao/scala-bootstrap-el")
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
      (org-latex-hyperref-template . nil))
    (leaf htmlize :ensure t)
    (leaf ob-sql-mode :ensure t)
    (leaf ox-gfm :ensure t)
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
                             nil))))))

;;; ---------- インターフェース設定 ----------
(leaf *interface
  :config
  (setq completion-ignored-extensions (append completion-ignored-extensions
                                              '("./" "../" ".xlsx" ".docx" ".pptx" ".DS_Store")))
  (defadvice completion--file-name-table (after ignoring-backups-f-n-completion activate)
    "Filter out results when they match `completion-ignored-extensions'."
    (let ((res ad-return-value))
      (if (and
           (listp res)
           (stringp
            (car res))
           (cdr res))
          (setq ad-return-value (completion-pcm--filename-try-filter res)))))

  (leaf ediff
    :custom
    (ediff-split-window-function . 'split-window-horizontally))

  (leaf projectile
    :bind (("C-x t" . my/projectile-toggle-between-implementation-and-test-other-window))
    :config
    (straight-use-package 'projectile)
    (eval-and-compile
      (defun my/projectile-toggle-between-implementation-and-test-other-window nil
        "Toggle between an implementation file and its test file."
        (interactive)
        (find-file-other-window
         (projectile-find-implementation-or-test
          (buffer-file-name)))))

    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(projectile-add-known-project
                                    '("~/repos/")
                                    nil nil "Customized with use-package projectile")))
    (with-eval-after-load 'projectile
      (projectile-mode 1)))

  (leaf projectile-rails
    :config
    (straight-use-package 'projectile-rails)
    (with-eval-after-load 'projectile
      (unless (fboundp 'projectile-rails-on)
        (autoload #'projectile-rails-on "projectile-rails" nil t))
      (add-hook 'projectile-mode-hook 'projectile-rails-on)))

  (leaf dashboard
    :init
    (straight-use-package 'dashboard)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(dashboard-items
                                    '((recents . 10)
                                      (projects . 10))
                                    nil nil "Customized with use-package dashboard")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(dashboard-startup-banner 'logo nil nil "Customized with use-package dashboard")))
    :require t
    :config
    (dashboard-setup-startup-hook))

  (leaf counsel
    :ensure t
    :bind (("C-s" . swiper)
           ("M-x" . counsel-M-x)
           ("M-y" . counsel-yank-pop)
           ("C-x C-f" . my/find-file-and-create-directory)
           ("C-x C-r" . counsel-recentf)
           ("C-x C-b" . switch-to-buffer)
           ("<f1> f" . counsel-describe-function)
           ("<f1> v" . counsel-describe-variable)
           ("<f1> l" . counsel-find-library)
           ("<f2> i" . counsel-info-lookup-symbol)
           ("<f2> u" . counsel-unicode-char)
           ("C-x f" . counsel-fzf)
           ("C-x e" . counsel-rg)
           ("C-c f" . counsel-flycheck)
           ("C-x C-g" . counsel-git)
           (read-expression-map
            ("C-r" . counsel-expression-history)))
    :init
    (straight-use-package 'counsel)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s" nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-height 20 nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-use-virtual-buffers t nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(enable-recursive-minibuffers t nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-count-format "(%d/%d) " nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-extra-directories nil nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-re-builders-alist
                                    '((t . ivy--regex-plus)
                                      (read-file-name-internal . ivy--regex-fuzzy))
                                    nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-format-function 'ivy-format-function-arrow nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(counsel-yank-pop-separator "\n-------\n" nil nil "Customized with use-package counsel")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(ivy-sort-matches-functions-alist
                                    '((t)
                                      (ivy-completion-in-region . ivy--shorter-matches-first)
                                      (ivy-switch-buffer . ivy-sort-function-buffer))
                                    nil nil "Customized with use-package counsel")))
    (with-eval-after-load 'counsel
      (ivy-mode 1)
      (setq ivy-initial-inputs-alist nil)
      (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
      (defun reloading (cmd)
        (lambda (x)
          (funcall cmd x)
          (ivy--reset-state ivy-last)))

      (defun given-file (cmd prompt)
        (lambda (source)
          (let ((target (let ((enable-recursive-minibuffers t))
                          (read-file-name
                           (format "%s %s to:" prompt source)))))
            (funcall cmd source target 1))))

      (defun confirm-delete-file (x)
        (dired-delete-file x 'confirm-each-subdirectory))

      (defun my/open-externally (file-name)
        "Open file with operating system's default application."
        (interactive "fOpen externally: ")
        (let ((process-connection-type nil))
          (start-process "open-externally" nil "open" file-name)))

      (ivy-set-actions 'my/find-file-and-create-directory
                       `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
                         ("c" ,(given-file #'copy-file "Copy")
                          "copy")
                         ("d" ,(reloading #'confirm-delete-file)
                          "delete")
                         ("m" ,(reloading
                                (given-file #'rename-file "Move"))
                          "move")
                         ("e" my/open-externally "open externally")
                         ("w" find-file-other-window "other window")))
      (defun my/counsel-rg-with-extention-and-word (_)
        "Execute counsel-rg with extention and _"
        (let ((word (read-from-minibuffer "Search Word: "))
              (extention (read-from-minibuffer "Extention: ")))
          (counsel-rg
           (concat word " -- -g'*." extention "'"))))

      (defun my/counsel-rg-from-current-directory (_)
        "Searched by current directory and subdirectories."
        (if (buffer-file-name)
            (counsel-rg nil
                        (file-name-directory buffer-file-name))
          (counsel-rg nil
                      (dired-current-directory))))

      (ivy-set-actions 'counsel-rg
                       '(("e" my/counsel-rg-with-extention-and-word "with-extention")
                         ("d" my/counsel-rg-from-current-directory "search-from-current-directroy")))
      (defun my/counsel-fzf-from-current-directory (_)
        "Searched by current directory and subdirectories."
        (if (buffer-file-name)
            (counsel-fzf nil
                         (file-name-directory buffer-file-name))
          (counsel-fzf nil
                       (dired-current-directory))))

      (ivy-set-actions 'counsel-fzf
                       '(("d" my/counsel-fzf-from-current-directory "search-from-current-directroy")))
      (defun my/ivy-yank-action (x)
        (kill-new x))

      (ivy-set-actions t
                       '(("y" my/ivy-yank-action "yank")))
      (unless (use-package-ensure-system-package-exists\? 'rg)
        (async-shell-command "brew install ripgrep"))
      (unless (use-package-ensure-system-package-exists\? 'fzf)
        (async-shell-command "brew install fzf"))))

  (leaf counsel-projectile
    :bind (("C-x C-j" . counsel-projectile-switch-project))
    :config
    (straight-use-package 'counsel-projectile))

  (leaf counsel-tramp
    :bind (("C-x C-t" . counsel-tramp))
    :config
    (straight-use-package 'counsel-tramp))

  (leaf ivy-rich
    :init
    (straight-use-package 'ivy-rich)
    (ivy-rich-mode 1)
    :require t)

  (leaf all-the-icons-ivy-rich
    :init
    (straight-use-package 'all-the-icons-ivy-rich)
    (all-the-icons-ivy-rich-mode 1)
    :require t)

  (leaf ivy-hydra
    :config
    (straight-use-package 'ivy-hydra)
    (with-eval-after-load 'counsel
      (require 'ivy-hydra nil nil)))

  (leaf avy
    :preface
    (defun add-keys-to-avy (prefix c &optional mode)
      (define-key global-map
        (read-kbd-macro
         (concat prefix
                 (string c)))
        `(lambda nil
           (interactive)
           (funcall
            (if (eq ',mode 'word)
                #'avy-goto-word-1 #'avy-goto-char)
            ,c))))

    :init
    (straight-use-package 'avy)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(avy-background t nil nil "Customized with use-package avy")))
    :require t
    :config
    (loop for c from 33 to 126 do
          (add-keys-to-avy "C-M-" c)))

  (leaf which-key
    :init
    (straight-use-package 'which-key)
    :require t
    :config
    (which-key-mode))

  (leaf amx
    :init
    (straight-use-package 'amx)
    :require t)

  (leaf peep-dired
    :config
    (straight-use-package 'peep-dired)
    (with-eval-after-load 'dired
      (unless (fboundp 'peep-dired)
        (autoload #'peep-dired "peep-dired" nil t))
      (bind-keys :package peep-dired :map dired-mode-map
                 ("P" . peep-dired))))

  (leaf all-the-icons-dired
    :config
    (straight-use-package 'all-the-icons-dired)
    (with-eval-after-load 'dired
      (require 'all-the-icons-dired nil nil)))

  (leaf dired-sidebar
    :bind (("C-x C-d" . dired-sidebar-toggle-sidebar)
           (dired-sidebar-mode-map
            ("o" . dired-sidebar-subtree-toggle)))
    :config
    (straight-use-package 'dired-sidebar)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(dired-sidebar-use-term-integration t nil nil "Customized with use-package dired-sidebar")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(dired-sidebar-use-custom-modeline nil nil nil "Customized with use-package dired-sidebar")))
    (add-hook 'dired-sidebar-mode-hook
              (lambda nil
                (unless (file-remote-p default-directory)
                  (auto-revert-mode))))
    (with-eval-after-load 'dired-sidebar
      (defcustom dired-sidebar-mode-line-format '("%e" mode-line-front-space mode-line-buffer-identification " " mode-line-end-spaces)
        "Mode line format for `dired-sidebar'." :type 'list :group 'dired-sidebar)))

  (leaf wdired
    :config
    (straight-use-package 'wdired)
    (with-eval-after-load 'dired
      (unless (fboundp 'wdired-change-to-wdired-mode)
        (autoload #'wdired-change-to-wdired-mode "wdired" nil t))
      (bind-keys :package wdired :map dired-mode-map
                 ("e" . wdired-change-to-wdired-mode)
                 ("C-t"))))

  (leaf eshell
    :config
    (straight-use-package 'eshell)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(eshell-cmpl-ignore-case t nil nil "Customized with use-package eshell")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(eshell-ask-to-save-history 'always nil nil "Customized with use-package eshell")))
    (with-eval-after-load 'eshell
      (add-hook 'eshell-mode-hook
                (lambda nil
                  (bind-key "C-r" 'counsel-esh-history eshell-mode-map)))))

  (leaf eshell-prompt-extras
    :config
    (straight-use-package 'eshell-prompt-extras)
    (with-eval-after-load 'eshell
      (let ((custom--inhibit-theme-enable nil))
        (custom-theme-set-variables 'use-package
                                    '(eshell-highlight-prompt nil nil nil "Customized with use-package eshell-prompt-extras")))
      (let ((custom--inhibit-theme-enable nil))
        (custom-theme-set-variables 'use-package
                                    '(eshell-prompt-function 'epe-theme-lambda nil nil "Customized with use-package eshell-prompt-extras")))
      (require 'eshell-prompt-extras nil nil)))

  (leaf esh-autosuggest
    :hook (eshell-mode-hook)
    :config
    (straight-use-package 'esh-autosuggest))

  (leaf docker
    :init
    (straight-use-package 'docker)
    :require t)

  (leaf docker-tramp
    :init
    (straight-use-package 'docker-tramp)
    :require t)

  (leaf magit
    :bind (("C-x g" . magit-status)
           (magit-status-mode-map
            ("q" . my/magit-quit-session)
            ("C-o" . magit-diff-visit-file-other-window)))
    :config
    (straight-use-package 'magit)
    (eval-and-compile
      (defun my/magit-quit-session nil
        (interactive)
        (kill-buffer)
        (delete-window)))

    (with-eval-after-load 'magit
      (add-hook 'server-switch-hook 'magit-commit-diff)
      (unless (use-package-ensure-system-package-exists\? 'git)
        (async-shell-command "brew install git"))))

  (leaf git-gutter
    :custom-face ((git-gutter:modified quote
                                       ((t
                                         (:background "#B4DCE7"))))
                  (git-gutter:added quote
                                    ((t
                                      (:background "#74DFC4"))))
                  (git-gutter:deleted quote
                                      ((t
                                        (:background "#964C7B")))))
    :init
    (straight-use-package 'git-gutter)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(git-gutter:modified-sign " " nil nil "Customized with use-package git-gutter")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(git-gutter:added-sign " " nil nil "Customized with use-package git-gutter")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(git-gutter:deleted-sign " " nil nil "Customized with use-package git-gutter")))
    :require t
    :config
    (global-git-gutter-mode 1))

  (leaf gist
    :init
    (straight-use-package 'gist)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(gist-list-format
                                    '((files "Filename" 24 nil identity)
                                      (created "Created" 20 nil "%D %R")
                                      (visibility "Visibility" 10 nil
                                                  (lambda (public)
                                                    (or
                                                     (and public "public")
                                                     "private")))
                                      (description "Description" 0 nil identity))
                                    nil nil "Customized with use-package gist")))
    :require t)

  (leaf magit-gh-pulls
    :config
    (straight-use-package
     '(magit-gh-pulls :host github :repo "hyakt/magit-gh-pulls" :branch "master"))
    (with-eval-after-load 'magit
      (unless (fboundp 'turn-on-magit-gh-pulls)
        (autoload #'turn-on-magit-gh-pulls "magit-gh-pulls" nil t))
      (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)))

  (leaf git-timemachine
    :init
    (straight-use-package 'git-timemachine)
    :require t)

  (leaf git-link
    :init
    (straight-use-package 'git-link)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(git-link-open-in-browser t nil nil "Customized with use-package git-link")))
    :require t)

  (leaf twittering-mode
    :init
    (straight-use-package 'twittering-mode)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(twittering-use-master-password t nil nil "Customized with use-package twittering-mode")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(twittering-timer-interval 120 nil nil "Customized with use-package twittering-mode")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(twittering-convert-fix-size 24 nil nil "Customized with use-package twittering-mode")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(twittering-status-format "%FOLD{%RT{%FACE[bold]{RT}} %i%s %r %C{%m/%d %H:%M}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y/%m/%d %H:%M}} \n}" nil nil "Customized with use-package twittering-mode")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(epa-pinentry-mode 'loopback nil nil "Customized with use-package twittering-mode")))
    :require t
    :config
    (twittering-enable-unread-status-notifier)
    (unless (use-package-ensure-system-package-exists\? 'gpg)
      (async-shell-command "brew install gpg")))

  (leaf open-junk-file
    :ensure t
    :bind (("C-`" . open-junk-file))
    :custom
    (open-junk-file-format . "~/Documents/junk/%Y-%m-%d-%H%M%S."))

  (leaf view
    :commands backward-word forward-word next-line previous-line gene-word scroll-down scroll-up bm-toggle bm-previous bm-next scroll-other-window-down scroll-other-window
    :config
    (straight-use-package 'view)
    (with-eval-after-load 'view
      (setq view-read-only t)
      (defadvice find-file (around find-file-switch-to-view-file
                                   (file &optional wild)
                                   activate)
        (if (and
             (not (file-writable-p file))
             (not (file-directory-p file)))
            (view-file file)
          ad-do-it)))

    (bind-keys :package view :map view-mode-map
               ("h" . backward-word)
               ("l" . forward-word)
               ("j" . next-line)
               ("k" . previous-line)
               (";" . gene-word)
               ("b" . scroll-down)
               (" " . scroll-up)
               ("n" lambda nil
                (interactive)
                (scroll-up 1))
               ("p" lambda nil
                (interactive)
                (scroll-down 1))
               ("." . bm-toggle)
               ("[" . bm-previous)
               ("]" . bm-next)
               ("c" . scroll-other-window-down)
               ("v" . scroll-other-window)))

  (leaf swap-buffers
    :bind (("C-x C-o" . swap-buffers))
    :config
    (straight-use-package 'swap-buffers))

  (leaf other-window-or-split
    :straight (other-window-or-split :type git :host github :repo "conao/other-window-or-split")
    :bind (("C-t" . my/ws-other-window-or-split-and-kill-minibuffer)
           ("C-S-t" . ws-previous-other-window-or-split))
    :config
    (with-eval-after-load 'other-window-or-split
      (setq ws-split-window-width-with-em 130)
      (use-package dired-sidebar)

      (defun my/ws-other-window-or-split nil
        (interactive)
        (when (one-window-p)
          (ws-split-window-dwim))
        (when (and
               (and
                (eq
                 (length
                  (window-list))
                 2)
                (dired-sidebar-showing-sidebar-p))
               (not (eq
                     (current-buffer)
                     (dired-sidebar-buffer
                      (selected-frame)))))
          (ws-split-window-dwim))
        (other-window 1))

      (defun my/ws-other-window-or-split-and-kill-minibuffer nil
        (interactive)
        (if (active-minibuffer-window)
            (progn
              (minibuffer-keyboard-quit)
              (my/ws-other-window-or-split))

          (my/ws-other-window-or-split)))))

  (leaf eyebrowse
    :bind (("C-z"))
    :init
    (straight-use-package 'eyebrowse)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(eyebrowse-keymap-prefix
                                    (kbd "C-z")
                                    nil nil "Customized with use-package eyebrowse")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(eyebrowse-new-workspace t nil nil "Customized with use-package eyebrowse")))
    :require t
    :config
    (eyebrowse-mode 1))

  (leaf shackle
    :init
    (straight-use-package 'shackle)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(shackle-default-size 0.4 nil nil "Customized with use-package shackle")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(shackle-rules
                                    '(("*Help*" :align right)
                                      ("*Messages*" :align right)
                                      ("*Backtrace*" :align right)
                                      ("*Completions*" :align below :ratio 0.33)
                                      ("*compilation*" :align below :ratio 0.33)
                                      ("*Compile-Log" :align below :ratio 0.33)
                                      ("*Kill Ring*" :align below :ratio 0.33)
                                      ("*Occur*" :align below :ratio 0.33)
                                      ("*Google Translate*" :align below :ratio 0.33)
                                      ("*Codic Result*" :align below :ratio 0.33)
                                      ("*quickrun*" :align below :ratio 0.33)
                                      ("*xref*" :align below :ratio 0.33)
                                      ("*prettier errors*" :align below :ratio 0.33)
                                      (magit-status-mode :select t :inhibit-window-quit t)
                                      ("*Python*" :align below :ratio 0.33 :select t)
                                      ("*pry*" :align below :ratio 0.33 :select t)
                                      ("*ruby*" :align below :ratio 0.33 :select t)
                                      ("*nodejs*" :align below :ratio 0.33 :select t)
                                      ("*shell*" :align below :ratio 0.33 :select t)
                                      ("*Async Shell Command*" :align right)
                                      ("*Shell Command Output*" :align right)
                                      ("\\`\\*My Mocha .*?\\*\\'" :regexp t :align below :ratio 0.3)
                                      ("*jest*" :regexp t :align below :ratio 0.3)
                                      ("*rustic-compilation*" :align below :ratio 0.33 :select nil)
                                      ("*rustfmt*" :align below :ratio 0.33 :select nil)
                                      ("*rspec-compilation*" :align below :ratio 0.33 :select nil))
                                    nil nil "Customized with use-package shackle")))
    (shackle-mode 1)
    :require t)

  (leaf centaur-tabs
    :bind (("M-{" . centaur-tabs-backward)
           ("M-}" . centaur-tabs-forward))
    :config
    (straight-use-package 'centaur-tabs)
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(centaur-tabs-height 34 nil nil "Customized with use-package centaur-tabs")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(centaur-tabs-set-icons t nil nil "Customized with use-package centaur-tabs")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(centaur-tabs-set-modified-marker t nil nil "Customized with use-package centaur-tabs")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(centaur-tabs-modified-marker "*" nil nil "Customized with use-package centaur-tabs")))
    (let ((custom--inhibit-theme-enable nil))
      (custom-theme-set-variables 'use-package
                                  '(centaur-tabs-close-button "x" nil nil "Customized with use-package centaur-tabs")))
    (centaur-tabs-mode t)
    (with-eval-after-load 'centaur-tabs
      (centaur-tabs-headline-match))))

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
