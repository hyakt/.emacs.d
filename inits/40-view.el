;;; 40-view.el --- 表示機能についての設定
;;; Commentary:

;;; Code:
(use-package neotree :defer t
  :bind (("M-=" . neotree-toggle))
  :config
  (defun my/neotree-kill-filename-at-point ()
    "Kill full path of note at point."
    (interactive)
    (message "Copy %s"
             (kill-new (neo-buffer--get-filename-current-line))))
  (bind-key "M-w" 'my/neotree-kill-filename-at-point neotree-mode-map)

  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char))
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package swap-buffers
  :bind (("C-x C-o" . swap-buffers)))

(use-package other-window-or-split
  :bind* (("C-t"  . ws-other-window-or-split)
         ("C-S-t" . ws-previous-other-window-or-split)
         ("M-t"   . ws-split-window-dwim)
         ("C-c j" . ws-adjust-windows-size)))

;; screen
(use-package perspeen
  :init (custom-set-variables '(perspeen-modestring-dividers '("" "" "|")))
  :config
  (perspeen-mode +1)
  (global-unset-key (kbd "C-z"))
  (setq perspeen-keymap-prefix (kbd "C-z"))
  (set-face-attribute 'perspeen-selected-face nil
                      :foreground "#b48ead" :background nil))

(use-package popwin
  :commands (popwin-mode)
  :config
  (popwin-mode 1)
  (setq popwin:close-popup-window-timer-interval 0.5)

  ;; popwin settings
  (push '("*Help*" :height 30 :stick t) popwin:special-display-config)
  (push '("*Completions*" :noselect t) popwin:special-display-config)
  (push '("*compilation*" :noselect t) popwin:special-display-config)
  (push '("*Messages*") popwin:special-display-config)
  (push '("*Backtrace*" :noselect t) popwin:special-display-config)
  (push '("*Kill Ring*" :height 30) popwin:special-display-config)
  (push '("*Compile-Log" :height 20 :stick t) popwin:special-display-config)

  (push '("*quickrun*" :height 10 :stick t) popwin:special-display-config)

  (push '("\*grep\*" :regexp t :height 0.5 :stick t) popwin:special-display-config)
  (push '("*Occur*" :noselect t) popwin:special-display-config)

  (push '("*eshell*" :height 30) popwin:special-display-config)
  (push '("*ansi-term" :regexp t :height 30) popwin:special-display-config)
  (push '("*shell*" :height 30) popwin:special-display-config)
  (push '("*Shell Command Output*" :noselect t) popwin:special-display-config)

  (push '("*Python*" :stick t) popwin:special-display-config)
  (push '("*jedi:doc*" :noselect t) popwin:special-display-config)

  (push '("*slime-apropos*") popwin:special-display-config)
  (push '("*slime-macroexpansion*") popwin:special-display-config)
  (push '("*slime-description*") popwin:special-display-config)
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
  (push '("*slime-xref*") popwin:special-display-config)
  (push '(sldb-mode :stick t) popwin:special-display-config)
  (push '(slime-repl-mode) popwin:special-display-config)
  (push '(slime-connection-list-mode) popwin:special-display-config)

  (push '("*undo-tree*" :width 0.2 :position right) popwin:special-display-config)
  (push '("*Google Translate*" :noselect t) popwin:special-display-config)
  (push '("*Codic Result*" :noselect t) popwin:special-display-config)

  (push '("*magit-commit*" :noselect t :height 30 :width 80 :stick t) popwin:special-display-config)
  (push '("*magit-diff*" :noselect t :height 30 :width 80) popwin:special-display-config)
  (push '("*magit-edit-log*" :noselect t :height 15 :width 80) popwin:special-display-config)
  (push '("*magit-process*" :noselect t :height 15 :width 80) popwin:special-display-config)
  (push '("^\*magit: .+\*$" :regexp t :height 0.5) popwin:special-display-config)

  (push '("*Remember*" :noselect t :height 30 :width 80 :stick t) popwin:special-display-config))

;; read onlyの設定
(setq view-read-only t)
(defvar pager-keybind
      `( ;; vi-like
        ("h" . backward-word)
        ("l" . forward-word)
        ("j" . next-line)
        ("k" . previous-line)
        (";" . gene-word)
        ("b" . scroll-down)
        (" " . scroll-up)
        ("n" . ,(lambda () (interactive) (scroll-up 1)))
        ("p" . ,(lambda () (interactive) (scroll-down 1)))
        ;; bm-easy
        ("." . bm-toggle)
        ("[" . bm-previous)
        ("]" . bm-next)
        ;; langhelp-like
        ("c" . scroll-other-window-down)
        ("v" . scroll-other-window)))

(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (hl-line-mode 1)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)

;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))

;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

(use-package origami
  :config
  ;; (makunbound 'origami-view-mode-map)
  (define-minor-mode origami-view-mode
    "TABにorigamiの折畳みを割り当てる"
    nil "折紙"
    '(("\C-i" . origami-cycle))
    (or origami-mode (origami-mode 1)))
  (defun origami-cycle (recursive)
    "origamiの機能をorg風にまとめる"
    (interactive "P")
    (call-interactively
     (if recursive 'origami-toggle-all-nodes 'origami-toggle-node)))
  (defun view-mode-hook--origami ()
    (when (memq major-mode (mapcar 'car origami-parser-alist))
      (origami-view-mode (if view-mode 1 -1))))
  (add-hook 'view-mode-hook 'view-mode-hook--origami))
