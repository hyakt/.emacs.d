; キーバインドの設定

;; 通常操作
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key (kbd "C-x ?") 'help-command)

;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;;; 少しずつスクロール
(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;; redo
(global-set-key "\M-/" 'redo)

;; shell
(global-set-key (kbd "C-q") 'eshell)

;; helm-projectile
(define-key global-map (kbd "C-x C-p") 'helm-projectile)

;; kill-this-buffer
(define-key global-map (kbd "C-x C-k") 'kill-buffer)

(defun close-and-kill-this-pane ()
      "If there are multiple windows, then close this pane and kill the buffer in it also."
      (interactive)
      (kill-this-buffer)
      (if (not (one-window-p))
          (delete-window)))

(define-key global-map (kbd "C-x k") 'close-and-kill-this-pane)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.06)

  ;; magit
  (key-chord-define-global "gt" 'magit-status)
  ;; dired-toggle
  (key-chord-define-global "dr" 'dired-toggle)
  )

