;;; my-util.el --- 汎用的なのユーティリティ
;;; Commentary:

;;; Code:
(leaf my-util
  :config
  (defun my/set-alpha (alpha-num)
    "Set frame parameter ALPHA-NUM."
    (interactive "nAlpha: ")
    (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

  (defun my/buffer-face-set-variable-pitch-font ()
    "Change the current buffer font to variable pitch font."
    (interactive)
    (buffer-face-set 'variable-pitch))

  (defvar my/current-screen-geometry
    (cl-loop for x in (display-monitor-attributes-list)
             when (> (length (assoc 'frames x)) 1)
             return (cons (nth 3 (assoc 'geometry x)) (nth 4 (assoc 'geometry x)))))

  (defun my/resize-frame (w h x y frame)
    "Set frame W (width), H (height), X (position left) and Y (position top) on FRAME."
    (set-frame-width frame (- w 20) nil 'pixelwise)
    (set-frame-height frame (- h 10) nil 'pixelwise)
    (set-frame-position frame x y))

  (defun my/fullscreen ()
    "Set frame maxmize."
    (interactive)
    (let ((frame (selected-frame))
          (width (car my/current-screen-geometry))
          (height (cdr my/current-screen-geometry)))
      (my/resize-frame width height 0 0 frame)))

  (defun my/halfscreen ()
    "Set frame half."
    (interactive)
    (let ((frame (selected-frame))
          (width (car my/current-screen-geometry))
          (height (cdr my/current-screen-geometry)))
      (my/resize-frame (/ width 2) height 0 0 frame)))

  (defun my/halfscreen-right ()
    "Set frame half."
    (interactive)
    (let ((frame (selected-frame))
          (width (car my/current-screen-geometry))
          (height (cdr my/current-screen-geometry)))
      (my/resize-frame (/ width 2) height (/ width 2) 0 frame)))

  (defun url-encode-string (str &optional sys)
    "STRをSYSタイプにエンコードする."
    (let ((sys (or sys 'utf-8)))
      (url-hexify-string (encode-coding-string str sys))))

  (defun url-decode-string (str &optional sys)
    "SYSタイプのSTRをデコードする."
    (let ((sys (or sys 'utf-8)))
      (decode-coding-string (url-unhex-string str) sys)))

  (defun my/url-decode-region (beg end)
    "BEGからENDの範囲の文字をURLデコードする."
    (interactive "r")
    (let ((pos beg)
          (str (buffer-substring beg end)))
      (goto-char beg)
      (delete-region beg end)
      (insert (url-decode-string str 'utf-8))))

  (defun my/url-encode-region (beg end)
    "BEGからENDの範囲の文字をURLエンコードする."
    (interactive "r")
    (let ((pos beg)
          (str (buffer-substring beg end)))
      (goto-char beg)
      (delete-region beg end)
      (insert (url-encode-string str 'utf-8))))

  (defun my/reverse-chars-region (beg end)
    "BEGからENDの範囲の文字反転する."
    (interactive "r")
    (let ((pos beg)
          (reverse-str (mapconcat 'identity (reverse (split-string (buffer-substring beg end) "")) "")))
      (goto-char beg)
      (message reverse-str)
      (delete-region beg end)
      (insert reverse-str)))

  (defun my/uniq-lines (beg end)
    "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (not (eobp))
          (kill-line 1)
          (yank)
          (let ((next-line (point)))
            (while
                (re-search-forward
                 (format "^%s" (regexp-quote (car kill-ring))) nil t)
              (replace-match "" nil nil))
            (goto-char next-line))))))

  (defun my/reopen-with-sudo-tramp ()
    "Reopen current buffer-file with sudo using tramp."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (string-match ":\\(.*\\):" file-name)
      (if file-name
          (find-alternate-file (replace-regexp-in-string ":.*:" (concat ":" (match-string 1 file-name) "|sudo:root" ":") file-name))
        (error "Cannot get a file name"))))

  (defun my/copy-buffer-name-clipboard ()
    "Copy buffer name to clipbord."
    (interactive)
    (kill-new buffer-file-name))

  (defun my/move-or-rename-this-file (newfile)
    "Move or Rename current buffer file to NEWFILE."
    (interactive "Fnewfile name: ")
    (let* ((current-file-name (buffer-name)))
      (rename-file current-file-name newfile)
      (find-file newfile)
      (kill-buffer current-file-name)))

  (defun my/delete-or-remove-this-file ()
    "Delete current buffer file."
    (interactive)
    (let* ((current-file-name (buffer-name)))
      (move-file-to-trash current-file-name)
      (kill-buffer current-file-name)))

  (defun my/dired-this-buffer ()
    "Open dired in this buffer."
    (interactive)
    (dired
     (file-name-directory (expand-file-name (buffer-name)))))

  (defun my/close-and-kill-this-pane ()
    "If there are multiple windows, then close this pane and kill the buffer in it also."
    (interactive)
    (kill-this-buffer)
    (if (not (one-window-p))
        (delete-window)))

  (defun my/kill-other-buffers ()
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

  (defun my/revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

  (defun my/buffer-indent ()
    "Indent whole current buffer."
    (interactive)
    (let ((current (point)))
      (push-mark (point-max) nil t)
      (goto-char (point-min))
      (indent-region (region-beginning)(region-end))
      (goto-char current)))

  (defun my/keyboard-quit()
    "Escape the minibuffer or cancel region consistently using 'Control-g'."
    (interactive)
    (if (not(window-minibuffer-p (selected-window)))
        (if (or mark-active (active-minibuffer-window))
            (keyboard-escape-quit))
      (keyboard-quit)))

  (defun my/find-file-and-create-directory(filename &optional wildcards)
    "Find a file, and then create FILENAME (WILDCARDS)
the folder if it doesn't exist."
    (interactive (find-file-read-args "Find file: " nil))
    (let ((value (find-file-noselect filename nil nil wildcards)))
      (if (listp value)
          (mapc 'switch-to-buffer (nreverse value))
        (switch-to-buffer value)))
    (when (not (file-exists-p default-directory))
      (message (format "Creating  %s" default-directory))
      (make-directory default-directory t)))

  (defun my/generate-slack-reminder (content)
    "Generate slack reminder with CONTENT and copy to clipboard."
    (interactive "sContent: ")
    (require 'org)
    (let* ((date (org-read-date t 'to-time nil "Date:  "))
           (time-string (format-time-string "%H:%M" date))
           (date-string (format-time-string "%Y-%m-%d" date))
           (channel (read-from-minibuffer "Channel: " "me")))
      (kill-new (concat "/remind " channel " " content " at " time-string " on " date-string))))

  (defun my/open-hyper-current-buffer ()
    "Open current buffer with Hyper term and fish."
    (interactive)
    (let ((default-env-shell (getenv "SHELL")))
      (setenv "SHELL" "/usr/local/bin/fish")
      (call-process-shell-command (concat "hyper " (file-name-directory buffer-file-name)))
      (setenv "SHELL" default-env-shell))))

(provide 'my-util)

;;; my-util.el ends here
