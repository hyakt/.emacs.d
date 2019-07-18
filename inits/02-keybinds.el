;;; 02-keybinds.el ---  基本的なキーバインドの設定
;;; Commentary:

;;; Code:
;; 通常操作
(keyboard-translate ?\C-h ?\C-?)

(bind-key (kbd "C-h") nil)
(bind-key (kbd "C-m") 'newline-and-indent) ; リターンで改行とインデント

(bind-key (kbd "C-x C-k") 'kill-buffer)

;; window-split
(bind-key (kbd "C-0") 'delete-window)
(bind-key (kbd "C-1") 'delete-other-windows)
(bind-key (kbd "C-2") 'split-window-below)
(bind-key (kbd "C-3") 'split-window-right)

(bind-key (kbd "C-g") '(lambda ()
                         (interactive)
                         (if (active-minibuffer-window)
                             (minibuffer-keyboard-quit)
                           (keyboard-quit))))

(defun my/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))
(bind-key (kbd "C-x k") 'my/close-and-kill-this-pane)

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (loop for buf in (buffer-list)
        unless (or
                (get-buffer-window buf)
                (string= (substring (buffer-name buf) 0 1) " ")
                (get-buffer-process buf)
                (member (buffer-name buf) ;; 消さないバッファ名を指定
                        '("*Messages*" "*Compile-Log*" "*Help*"
                          "*scratch*" "*init log*")))
        do (kill-buffer buf)))
(bind-key (kbd "C-x C-x") 'my/kill-other-buffers)

;; reload buffer
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(bind-key (kbd "<f5>") 'revert-buffer-no-confirm)

(bind-key (kbd "C-x i") 'my/buffer-indent)

;;; 02-keybinds.el ends here
