;;; my-functions.el --- 自作elisp
;;; Commentary:
;;; 自作elispを乱雑に配置する。今後整理するかも。

;;; Code:
(leaf my-functions
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

  (defvar sql-connection-alist)

  (defun my/connect-db-via-ssh-tunneling(db host dbport)
    "Connect DB:DBPORT@HOST via ssh tunneling."
    (let ((port (car (cdr (assoc 'sql-port (assoc db sql-connection-alist)))))
          (password (car (cdr (assoc 'sql-password (assoc db sql-connection-alist))))))
      (start-process-shell-command "DBProxy" "*Proxy*" (concat "ssh -N -L " (number-to-string port) ":localhost:" dbport " " host))
      (setenv "PGPASSWORD" password))
    (sleep-for 1.5)
    (sql-connect db))

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

  (defun my/eslint-fix-file ()
    "Run eslint for current file."
    (interactive)
    (message "eslint --fix %s" (buffer-file-name))
    (call-process-shell-command
     (concat "npx eslint --fix " (buffer-file-name))
     nil "*Shell Command Output*" t)
    (revert-buffer t t))

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

  (leaf my/test
    :ensure (projectile vterm vterm-toggle)
    :config
    (defun my/copy-project-name-clipboard ()
      "Copy project name to clipbord."
      (interactive)
      (kill-new (projectile-project-root)))

    (defcustom my/mocha-config-path nil
      "Mocha config path")

    (defun my/projectile-run-async-shell-command-in-root (command &optional output-buffer)
      "Invoke `async-shell-command' COMMAND in the project's root."
      (projectile-with-default-dir
          (projectile-ensure-project (projectile-project-root))
        (async-shell-command command output-buffer)))

    (defun my/projectile-run-vterm-command-in-root (command)
      "Invoke `async-shell-command' COMMAND in the project's root."
      (projectile-with-default-dir
          (projectile-ensure-project (projectile-project-root))
        (my/run-in-vterm command)))

    (defun my/run-in-vterm (command)
      "Execute string COMMAND in a new vterm."
      (interactive
       (list
        (let* ((f (cond (buffer-file-name)
                        ((eq major-mode 'dired-mode)
                         (dired-get-filename nil t))))
               (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
          (read-shell-command "Terminal command: "
                              (cons filename 0)
                              (cons 'shell-command-history 1)
                              (list filename)))))
      (with-current-buffer (vterm-toggle)
        (vterm-send-string command)
        (vterm-send-return)))

    (defun my/mocha-exec-current-buffer ()
      "Run mocha for current file."
      (interactive)
      (setenv "NODE_ENV" "test")
      (my/projectile-run-async-shell-command-in-root
       (concat "npx mocha -c"
               (when my/mocha-config-path
                 (concat " --config " my/mocha-config-path))
               (concat " " (buffer-file-name)))
       "*My Mocha Exec Command*"))

    (defun my/mocha-copy-command-exec-current-buffer ()
      "Run mocha for current file for paste."
      (interactive)
      (let ((mocha-command
             (concat "env NODE_PATH=test npx mocha -c"
                     (when my/mocha-config-path
                       (concat " --config " my/mocha-config-path))
                     (concat " " (buffer-file-name)))))
        (kill-new (concat "cd " (projectile-project-root) "; " mocha-command "; "))
        (message (concat "cd " (projectile-project-root) "; " mocha-command "; "))))

    (defun my/mocha-watch-current-buffer ()
      "Watch mocha for current file."
      (interactive)
      (setenv "NODE_ENV" "test")
      (my/projectile-run-async-shell-command-in-root
       (concat "npx mocha -c -w --extension js,ts,jsx,tsx"
               (when my/mocha-config-path
                 (concat " --config " my/mocha-config-path))
               (concat " " (buffer-file-name)))
       "*My Mocha Watch Command*"))

    (defun my/mocha-copy-command-watch-current-buffer ()
      "Watch mocha for current file for paste."
      (interactive)
      (let ((mocha-command
             (concat "env NODE_PATH=test npx mocha -c -w --extension js,ts,jsx,tsx"
                     (when my/mocha-config-path
                       (concat " --config " my/mocha-config-path))
                     (concat " " (buffer-file-name)))))
        (kill-new (concat "cd " (projectile-project-root) "; " mocha-command "; "))
        (message (concat "cd " (projectile-project-root) "; " mocha-command "; "))))

    (defun my/mocha-exec-add-save-hook ()
      "Add save hook exec mocha."
      (interactive)
      (add-hook 'before-save-hook 'my/mocha-exec-current-buffer))

    (defun my/mocha-exec-remove-save-hook ()
      "Remove save hook exec mocha."
      (interactive)
      (remove-hook 'before-save-hook 'my/mocha-exec-current-buffer))

    (defun my/jest-copy-command-current-buffer ()
      "Watch jest for current file for paste."
      (interactive)
      (let ((jest-command (concat "env DEBUG_PRINT_LIMIT=100000 npx jest --color " (buffer-file-name))))
        (kill-new (concat "cd " (projectile-project-root) "; " jest-command ";"))
        (message (concat "cd " (projectile-project-root) "; " jest-command ";"))))

    (defun my/jest-copy-command-watch-current-buffer ()
      "Watch jest for current file for paste."
      (interactive)
      (let ((jest-command (concat "npx jest --watch --color " (buffer-file-name))))
        (kill-new (concat "cd " (projectile-project-root) "; " jest-command "; "))
        (message (concat "cd " (projectile-project-root) "; " jest-command "; "))))

    (defun my/jest-current-buffer ()
      "Watch mocha for current file."
      (interactive)
      (setenv "NODE_ENV" "test")
      (let ((jest-command (concat "env DEBUG_PRINT_LIMIT=100000 npx jest --color " (buffer-file-name))))
        (my/projectile-run-vterm-command-in-root jest-command)))

    (defun my/jest-watch-current-buffer ()
      "Watch mocha for current file."
      (interactive)
      (setenv "NODE_ENV" "test")
      (let ((jest-command (concat "npx jest --watch --color " (buffer-file-name))))
        (my/projectile-run-vterm-command-in-root jest-command)))

    (defun my/rspec-copy-command-current-buffer ()
      "Watch RSpec for current file for paste."
      (interactive)
      (let ((rspec-command (concat "bundle exec rspec " (buffer-file-name))))
        (kill-new (concat "cd " (projectile-project-root) "; " rspec-command "; "))
        (message (concat "cd " (projectile-project-root) "; " rspec-command "; "))))

    (defun my/create-test-file-for-jest ()
      "Create test file for jest in test directory."
      (interactive)
      (let* ((test-file (replace-regexp-in-string "\\." ".test." (file-name-nondirectory (buffer-file-name))))
             (test-dir (replace-regexp-in-string "src/" "__tests__/" (file-name-directory (buffer-file-name)))))
        (unless (file-exists-p (expand-file-name test-file test-dir))
          (progn (unless (file-exists-p test-dir)
                   (make-directory test-dir :create-parents))
                 (find-file-other-window (concat test-dir test-file))
                 (save-buffer)))))

    (defun my/create-test-file-for-rspec ()
      "Create test file for RSpec in test directory."
      (interactive)
      (let* ((test-file (replace-regexp-in-string "\\." "_spec." (file-name-nondirectory (buffer-file-name))))
             (test-dir (replace-regexp-in-string "app/" "spec/" (file-name-directory (buffer-file-name)))))
        (unless (file-exists-p (expand-file-name test-file test-dir))
          (progn (unless (file-exists-p test-dir)
                   (make-directory test-dir :create-parents))
                 (find-file-other-window (concat test-dir test-file))
                 (save-buffer))))))

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
      (setenv "SHELL" default-env-shell)))

  (defun my/trans-deepl (beg end)
    (interactive "r")
    (let ((str (buffer-substring beg end)))
      (browse-url
       (concat "https://www.deepl.com/translator#en/ja/" (url-hexify-string str)))))

  (leaf my/translate
    :ensure (request)
    :require 'request
    :config
    (defcustom my/deepl-api-auth-key ""
      "DeepLの無料のauth key"
      :type 'string)

    (defun my/deepl-region (beg end)
      "BEGからENDの範囲をDeepLで翻訳する"
      (interactive "r")
      (let ((str (buffer-substring beg end)))
        (request
          "https://api-free.deepl.com/v2/translate"
          :type "GET"
          :data `(("auth_key" . ,(url-hexify-string my/deepl-api-auth-key)) ("text" . ,str) ("target_lang" . "JA"))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let ((text (decode-coding-string (url-unhex-string (assoc-default 'text (aref (assoc-default 'translations data) 0))) 'utf-8)))
                        (kill-new text)
                        (message "%s" text))
                      )))))

    (defun my/codic (str)
      "STRの値をdeepLで翻訳する"
      (interactive "smy/Codic: ")
        (request
          "https://api-free.deepl.com/v2/translate"
          :type "GET"
          :data `(("auth_key" . ,(url-hexify-string my/deepl-api-auth-key)) ("target_lang" . "EN-US") ("text" . ,str))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let ((text (downcase (decode-coding-string (url-unhex-string (assoc-default 'text (aref (assoc-default 'translations data) 0))) 'utf-8))))
                        (kill-new text)
                        (message "%s" text))
                      )))))
  )

(provide 'my-functions)

;;; my-functions.el ends here
