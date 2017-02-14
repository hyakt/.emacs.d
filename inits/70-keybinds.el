;;; 70-keybinds.el ---  キーバインドの設定
;;; Commentary:

;; Code:
;; 通常操作
(keyboard-translate ?\C-h ?\C-?)

(bind-key (kbd "C-h") nil)
(bind-key (kbd "C-m") 'newline-and-indent) ; リターンで改行とインデント

;; 複数行移動
(bind-key (kbd "M-n") (kbd "C-u 5 C-n"))
(bind-key (kbd "M-p") (kbd "C-u 5 C-p"))

;; helm-projectile
(bind-key (kbd "C-x C-p") 'helm-projectile)

;; kill-this-buffer
(bind-key (kbd "C-x C-k") 'kill-buffer)

;; window-split
(bind-key (kbd "C-0") 'delete-window)
(bind-key (kbd "C-1") 'delete-other-windows)
(bind-key (kbd "C-2") 'split-window-below)
(bind-key (kbd "C-3") 'split-window-right)

;; helm-find-files-other-window
(bind-key (kbd "C-x C-a") 'find-file-other-window)

;; expand-region
(bind-key* (kbd "C-,") 'er/expand-region)

;; other-window-or-split
(bind-key* (kbd "C-t") 'other-window-or-split)

;; avy
(bind-key* (kbd "C-;") 'avy-goto-char)
(bind-key* (kbd "C-:") 'avy-goto-line)

;; company-complete
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'company-complete)

;; reload buffer
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(bind-key (kbd "<f5>") 'revert-buffer-no-confirm)

;; clonse-buffer
(defun close-and-kill-this-pane ()
      "If there are multiple windows, then close this pane and kill the buffer in it also."
      (interactive)
      (kill-this-buffer)
      (if (not (one-window-p))
          (delete-window)))

(bind-key (kbd "C-x k") 'close-and-kill-this-pane)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.06)

  ;; magit
  (key-chord-define-global "gt" 'magit-status)
  ;; dired-toggle
  ;;  (key-chord-define-global "dr" 'dired-toggle)
  ;; view-mode
  (key-chord-define-global "jk" 'view-mode)
  (key-chord-define-global "zx" 'org-capture)
  (key-chord-define-global "qw" 'org-agenda))
