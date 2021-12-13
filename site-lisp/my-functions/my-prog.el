;;; my-prog.el --- prog mode用のユーティリティ
;;; Commentary:

;;; Code:
(leaf my-prog
  :ensure (projectile vterm vterm-toggle)
  :config
  (defun my/copy-project-name-clipboard ()
    "Copy project name to clipbord."
    (interactive)
    (kill-new (projectile-project-root)))

  (defcustom my/mocha-config-path nil
    "Mocha config path")

  (defun my/projectile-run-shell-command-in-root (command)
    "Invoke `shell-command' COMMAND in the project's root."
    (projectile-with-default-dir
        (projectile-ensure-project (projectile-project-root))
      (shell-command command)))

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

  (defun my/tsc-error-find-file-buffer ()
    "Show tsc error on buffer."
    (interactive)
    (my/projectile-run-async-shell-command-in-root
     "npx tsc --noEmit --pretty false | sed -E \"s/^ +.*//g\" | sed -E \"s/\\([0-9]+,[0-9]+\\):.*//g\" | sort | uniq | sed -E \"s/(.*)/\\(find-file-other-window \\\"\\1\\\"\\)/g\""
     "*My TSC Errors*"))

  (defun my/eslint-error-find-file-buffer ()
    "Show eslint error on buffer."
    (interactive)
    (my/projectile-run-async-shell-command-in-root
     "npx eslint --quiet --format compact . | sed -E 's/^([\\/\\._a-zA-Z0-9]+):.*\\((.*)\\)$/[\\2] (find-file-other-buffer \"\\1\")/g'"
     "*My Eslint Errors*"))

  (defun my/eslint-warning-sorted-by-error-find-file-buffer ()
    "Show sorted eslint warning on buffer."
    (interactive)
    (my/projectile-run-async-shell-command-in-root
     "npx eslint --format compact . | sort -k 2 -t \"(\" | sed -E 's/^([\\/\\._a-zA-Z0-9]+):.*\\((.*)\\)$/[\\2] (find-file-other-buffer \"\\1\")/g'"
     "*My Eslint Errors*"))

  (defun my/eslint-spefic-error-find-file-buffer (error-name)
    "Show eslint ERROR-NAME error on buffer."
    (interactive "sError name: ")
    (my/projectile-run-async-shell-command-in-root
     (concat
      "echo Error: "
      error-name
      ";echo -----------------------------------------\n\n;"
      "npx eslint --format json . | jq '.[] | {filePath: .filePath, ruleId: .messages[].ruleId}' | jq -s '.[] | select (.ruleId ==\""
      error-name
      "\")' | grep filePath | sed -E 's/ +\"filePath\": \"(.*)\".*/\\(find-file-other-window \\\"\\1\\\"\\)/g' | uniq"
      )
     "*My Eslint Specific Errors*"))

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
               (save-buffer)))))

  (defun my/eslint-fix-file ()
    "Run eslint for current file."
    (interactive)
    (message "eslint --fix %s" (buffer-file-name))
    (call-process-shell-command
     (concat "npx eslint --fix " (buffer-file-name))
     nil "*Shell Command Output*" t)
    (revert-buffer t t)))

(provide 'my-prog)

;;; my-prog.el ends here
