;;; 70-keybinds.el ---  キーバインドの設定
;;; Commentary:

;;; Code:
;; 通常操作
(keyboard-translate ?\C-h ?\C-?)

(bind-key (kbd "C-h") nil)
(bind-key (kbd "C-m") 'newline-and-indent) ; リターンで改行とインデント

;; kill-this-buffer
(bind-key (kbd "C-x C-k") 'kill-buffer)

;; find-file-other-window
(bind-key (kbd "C-x f") 'find-file-other-window)

;; window-split
(bind-key (kbd "C-0") 'delete-window)
(bind-key (kbd "C-1") 'delete-other-windows)
(bind-key (kbd "C-2") 'split-window-below)
(bind-key (kbd "C-3") 'split-window-right)

;; expand-region
(bind-key* (kbd "C-,") 'er/expand-region)

;; avy
(bind-key* (kbd "C-;") 'avy-goto-char)
(bind-key* (kbd "C-:") 'avy-goto-line)

;; other-window
(bind-key* (kbd "C-t") 'ws-other-window-or-split)

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

;; kill-other-buffers
(defun kill-other-buffers ()
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

(bind-key (kbd "C-x C-x") 'kill-other-buffers)

;;; 60-keybinds.el ends here
