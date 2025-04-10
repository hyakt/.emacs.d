;;; my-util.el --- 汎用的な関数 -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'package)
(require 'org)

;;;###autoload
(defun my-generate-autoloads ()
  "Generates autoloads."
  (interactive)
  (package-generate-autoloads "my-functions" "~/.emacs.d/lisp/functions"))

;;;###autoload
(defun my-set-alpha (alpha-num)
  "Set frame parameter ALPHA-NUM."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;;;###autoload
(defun my-buffer-face-set-variable-pitch-font ()
  "Change the current buffer font to variable pitch font."
  (interactive)
  (buffer-face-set 'variable-pitch))

(defvar my-current-screen-geometry
  (cl-loop for x in (display-monitor-attributes-list)
           when (> (length (assoc 'frames x)) 1)
           return (cons (nth 3 (assoc 'geometry x)) (nth 4 (assoc 'geometry x)))))

(defun my-resize-frame (w h x y frame)
  "Set frame W (width), H (height), X (position left) and Y (position top) on FRAME."
  (set-frame-width frame (- w 20) nil 'pixelwise)
  (set-frame-height frame (- h 10) nil 'pixelwise)
  (set-frame-position frame x y))

;;;###autoload
(defun my-fullscreen ()
  "Set frame maxmize."
  (interactive)
  (let ((frame (selected-frame))
        (width (car my-current-screen-geometry))
        (height (cdr my-current-screen-geometry)))
    (my-resize-frame width height 0 0 frame)))

;;;###autoload
(defun my-halfscreen ()
  "Set frame half."
  (interactive)
  (let ((frame (selected-frame))
        (width (car my-current-screen-geometry))
        (height (cdr my-current-screen-geometry)))
    (my-resize-frame (/ width 2) height 0 0 frame)))

;;;###autoload
(defun my-halfscreen-right ()
  "Set frame half."
  (interactive)
  (let ((frame (selected-frame))
        (width (car my-current-screen-geometry))
        (height (cdr my-current-screen-geometry)))
    (my-resize-frame (/ width 2) height (/ width 2) 0 frame)))

(defun url-encode-string (str &optional sys)
  "STRをSYSタイプにエンコードする."
  (let ((sys (or sys 'utf-8)))
    (url-hexify-string (encode-coding-string str sys))))

(defun url-decode-string (str &optional sys)
  "SYSタイプのSTRをデコードする."
  (let ((sys (or sys 'utf-8)))
    (decode-coding-string (url-unhex-string str) sys)))

;;;###autoload
(defun my-url-decode-region (beg end)
  "BEGからENDの範囲の文字をURLデコードする."
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-decode-string str 'utf-8))))

;;;###autoload
(defun my-url-encode-region (beg end)
  "BEGからENDの範囲の文字をURLエンコードする."
  (interactive "r")
  (let ((pos beg)
        (str (buffer-substring beg end)))
    (goto-char beg)
    (delete-region beg end)
    (insert (url-encode-string str 'utf-8))))

(defun char-unicode (char) (encode-char char 'ucs))
(defun unicode-char (code) (decode-char 'ucs code))

;;;###autoload
(defun unicode-unescape-region (start end)
  "指定した範囲のUnicodeエスケープ文字(\\uXXXX)をデコードする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\u\\([[:xdigit:]]\\{4\\}\\)" nil t)
      (replace-match (string (unicode-char
                              (string-to-number (match-string 1) 16)))
                     nil t))))

;;;###autoload
(defun unicode-escape-region (&optional start end)
  "指定した範囲の文字をUnicodeエスケープする."
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "." nil t)
      (replace-match (format "\\u%04x"
                             (char-unicode
                              (string-to-char (match-string 0))))
                     nil t))))


;;;###autoload
(defun my-reverse-chars-region (beg end)
  "BEGからENDの範囲の文字反転する."
  (interactive "r")
  (let ((pos beg)
        (reverse-str (mapconcat 'identity (reverse (split-string (buffer-substring beg end) "")) "")))
    (goto-char beg)
    (message reverse-str)
    (delete-region beg end)
    (insert reverse-str)))

;;;###autoload
(defun my-uniq-lines (beg end)
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

;;;###autoload
(defun my-reopen-with-sudo-tramp ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (string-match ":\\(.*\\):" file-name)
    (if file-name
        (find-alternate-file (replace-regexp-in-string ":.*:" (concat ":" (match-string 1 file-name) "|sudo:root" ":") file-name))
      (error "Cannot get a file name"))))

;;;###autoload
(defun my-copy-buffer-name-clipboard ()
  "Copy buffer name to clipbord."
  (interactive)
  (kill-new buffer-file-name))

;;;###autoload
(defun my-move-or-rename-this-file (newfile)
  "Move or Rename current buffer file to NEWFILE."
  (interactive "Fnewfile name: ")
  (let* ((current-file-name (buffer-name)))
    (rename-file current-file-name newfile)
    (find-file newfile)
    (kill-buffer current-file-name)))

;;;###autoload
(defun my-copy-this-file ()
  "Copy current buffer file to new file."
  (interactive)
  (let* ((current-file-name (buffer-name))
         (new-filename (read-string (format "Copy %s to: " current-file-name) current-file-name)))
    (copy-file current-file-name new-filename)
    (find-file new-filename)
    (kill-buffer current-file-name)))

;;;###autoload
(defun my-delete-or-remove-this-file ()
  "Delete current buffer file."
  (interactive)
  (let* ((current-file-name (buffer-name)))
    (move-file-to-trash current-file-name)
    (kill-buffer current-file-name)))

;;;###autoload
(defun my-find-file-and-create-directory(filename &optional wildcards)
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

;;;###autoload
(defun my-generate-slack-reminder (content)
  "Generate slack reminder with CONTENT and copy to clipboard."
  (interactive "sContent: ")
  (let* ((date (org-read-date t 'to-time nil "Date:  "))
         (time-string (format-time-string "%H:%M" date))
         (date-string (format-time-string "%Y-%m-%d" date))
         (channel (read-from-minibuffer "Channel: " "me")))
    (kill-new (concat "/remind " channel " " content " at " time-string " on " date-string))))

;;;###autoload
(defun my-open-hyper-current-buffer ()
  "Open current buffer with Hyper term and fish."
  (interactive)
  (let ((default-env-shell (getenv "SHELL")))
    (setenv "SHELL" "/usr/local/bin/fish")
    (call-process-shell-command (concat "hyper " (file-name-directory buffer-file-name)))
    (setenv "SHELL" default-env-shell)))

;;;###autoload
(defun my-open-alacritty-tmux-current-buffer ()
  "Open current buffer with tmux."
  (interactive)
  (shell-command
   (concat "tmux new-window -c " (file-name-directory buffer-file-name)))
  (shell-command "open -a Alacritty"))

;;;###autoload
(defun my-open-scratch ()
  "Open *scratch* buffer ."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))

(defcustom open-today-org-file-format "~/Documents/junk/%Y-%m-%d.org"
  "File format to put junk files with directory.
It can include `format-time-string' format specifications."
  :type 'string
  :group 'open-junk-file)

;;;###autoload
(defun open-today-org-file ()
  "Open a today org file."
  (interactive)
  (let* ((file (format-time-string open-today-org-file-format (current-time)))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file file)))

;;;###autoload
(defun my-open ()
  "Open finder."
  (interactive)
  (shell-command "open ."))

;;;###autoload
(defun my-other-window-or-split ()
  "Switch to other window or split window."
  (interactive)
  (if (one-window-p)
      (split-window-horizontally)
    (other-window 1)))

(defun minibuffer-keyboard-quit () ;; esc quits
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;;###autoload
(defun my-other-window-or-split-and-kill-minibuffer ()
  "Switch to other window or split window and kill minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (minibuffer-keyboard-quit)
        (my-other-window-or-split))
    (my-other-window-or-split)))

;;;###autoload
(defun my-create-ics-file-from-text (text)
  "テキストからICSファイルを作成し、デスクトップに保存します。"
  (interactive "sICSテキストを入力してください（複数行は C-q C-j で改行）: ")
  (let* ((desktop-dir (expand-file-name "~/Desktop/"))
         (date-str (format-time-string "%Y%m%d-%H%M%S"))
         (filename (concat desktop-dir "calendar-" date-str ".ics")))
    ;; テキストからイベント名を抽出しようとする
    (when (string-match "SUMMARY:\\(.*\\)" text)
      (setq filename (concat desktop-dir
                             (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_"
                                                       (match-string 1 text))
                             "-" date-str ".ics")))
    ;; ファイル保存
    (with-temp-file filename
      (insert text))
    (message "ICSファイルを保存しました: %s" filename)))

(provide 'my-util)

;;; my-util.el ends here
