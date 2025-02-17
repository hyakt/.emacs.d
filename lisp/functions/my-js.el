;;; my-js.el --- javascript用のユーティリティ
;;; Commentary:

;;; Code:
(eval-when-compile (package-initialize))

(require 'projectile)

;;;###autoload
(defun my-deno-project-p ()
  "Predicate for determining if the open project is a Deno one."
  (let ((p-root (car (last (project-current)))))
    (or
     (file-exists-p (concat p-root "deno.json"))
     (file-exists-p (concat p-root "deno.jsonc")))))

;;;###autoload
(defun my-node-project-p ()
  "Predicate for determining if the open project is a Node one."
  (let ((p-root (file-name-directory (shell-command-to-string "npm root"))))
    (file-exists-p (concat p-root "package.json"))))

;;;###autoload
(defun my-copy-project-name-clipboard ()
  "Copy project name to clipbord."
  (interactive)

  (kill-new (projectile-project-root)))

(defcustom my-mocha-config-path nil
  "Mocha config path")

;;;###autoload
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-projectile-run-shell-command-in-root (command)
  "Invoke `shell-command' COMMAND in the project's root."
  (projectile-with-default-dir
   (projectile-ensure-project (projectile-project-root))
   (shell-command command)))

(defun my-projectile-run-async-shell-command-in-root (command &optional output-buffer)
  "Invoke `async-shell-command' COMMAND in the project's root."
  (projectile-with-default-dir
   (projectile-ensure-project (projectile-project-root))
   (async-shell-command command output-buffer)))

;;;###autoload
(defun my-mocha-exec-current-buffer ()
  "Run mocha for current file."
  (interactive)
  (setenv "NODE_ENV" "test")
  (my-projectile-run-async-shell-command-in-root
   (concat "npx mocha -c"
           (when my-mocha-config-path
             (concat " --config " my-mocha-config-path))
           (concat " " (buffer-file-name)))
   "*My Mocha Exec Command*"))

;;;###autoload
(defun my-mocha-copy-command-exec-current-buffer ()
  "Run mocha for current file for paste."
  (interactive)
  (let ((mocha-command
         (concat "env NODE_PATH=test npx mocha -c"
                 (when my-mocha-config-path
                   (concat " --config " my-mocha-config-path))
                 (concat " " (buffer-file-name)))))
    (kill-new (concat "cd " (projectile-project-root) "; " mocha-command "; "))
    (message (concat "cd " (projectile-project-root) "; " mocha-command "; "))))

;;;###autoload
(defun my-mocha-watch-current-buffer ()
  "Watch mocha for current file."
  (interactive)
  (setenv "NODE_ENV" "test")
  (my-projectile-run-async-shell-command-in-root
   (concat "npx mocha -c -w --extension js,ts,jsx,tsx"
           (when my-mocha-config-path
             (concat " --config " my-mocha-config-path))
           (concat " " (buffer-file-name)))
   "*My Mocha Watch Command*"))

;;;###autoload
(defun my-mocha-copy-command-watch-current-buffer ()
  "Watch mocha for current file for paste."
  (interactive)
  (let ((mocha-command
         (concat "env NODE_PATH=test npx mocha -c -w --extension js,ts,jsx,tsx"
                 (when my-mocha-config-path
                   (concat " --config " my-mocha-config-path))
                 (concat " " (buffer-file-name)))))
    (kill-new (concat "cd " (projectile-project-root) "; " mocha-command "; "))
    (message (concat "cd " (projectile-project-root) "; " mocha-command "; "))))

;;;###autoload
(defun my-mocha-exec-add-save-hook ()
  "Add save hook exec mocha."
  (interactive)
  (add-hook 'before-save-hook 'my-mocha-exec-current-buffer))

;;;###autoload
(defun my-mocha-exec-remove-save-hook ()
  "Remove save hook exec mocha."
  (interactive)
  (remove-hook 'before-save-hook 'my-mocha-exec-current-buffer))

;;;###autoload
(defun my-jest-copy-command-current-buffer ()
  "Watch jest for current file for paste."
  (interactive)
  (let ((jest-command (concat "env DEBUG_PRINT_LIMIT=100000 npx jest --color " (buffer-file-name))))
    (kill-new (concat "cd " (projectile-project-root) "; " jest-command ";"))
    (message (concat "cd " (projectile-project-root) "; " jest-command ";"))))

;;;###autoload
(defun my-jest-copy-command-watch-current-buffer ()
  "Watch jest for current file for paste."
  (interactive)
  (let ((jest-command (concat "npx jest --watch --color " (buffer-file-name))))
    (kill-new (concat "cd " (projectile-project-root) "; " jest-command "; "))
    (message (concat "cd " (projectile-project-root) "; " jest-command "; "))))

;;;###autoload
(defun my-vitest-copy-command-current-buffer ()
  "Watch vitest for current file for paste."
  (interactive)
  (let ((vitest-command (concat "env NODE_OPTIONS='--no-experimental-fetch' DEBUG_PRINT_LIMIT=100000 pnpm vitest " (buffer-file-name))))
    (kill-new (concat "cd " (projectile-project-root) "; " vitest-command ";"))
    (message (concat "cd " (projectile-project-root) "; " vitest-command ";"))))

;;;###autoload
(defun my-vitest-copy-command-watch-current-buffer ()
  "Watch vitest for current file for paste."
  (interactive)
  (let ((vitest-command (concat "NODE_OPTIONS='--no-experimental-fetch' pnpm vitest --watch " (buffer-file-name))))
    (kill-new (concat "cd " (projectile-project-root) "; " vitest-command "; "))
    (message (concat "cd " (projectile-project-root) "; " vitest-command "; "))))

;;;###autoload
(defun my-vitest-command-watch-tmux ()
  "Watch vitest for other tmux."
  (interactive)
  (let ((vitest-command (concat "NODE_OPTIONS='--no-experimental-fetch' pnpm vitest --watch " (buffer-file-name))))
    (my-open-alacritty-tmux-current-buffer)
    (shell-command
     (concat "tmux send-keys '" vitest-command "' Enter"))
    (message (concat "cd " (projectile-project-root) "; " vitest-command "; "))))

;;;###autoload
(defun my-tsc-error-find-file-buffer ()
  "Show tsc error on buffer."
  (interactive)
  (my-projectile-run-async-shell-command-in-root
   "npx tsc --noEmit --pretty false | sed -E \"s/^ +.*//g\" | sed -E \"s/\\([0-9]+,[0-9]+\\):.*//g\" | sort | uniq | sed -E \"s/(.*)/\\(find-file-other-window \\\"\\1\\\"\\)/g\""
   "*My TSC Errors*"))

;;;###autoload
(defun my-eslint-error-find-file-buffer ()
  "Show eslint error on buffer."
  (interactive)
  (my-projectile-run-async-shell-command-in-root
   "npx eslint --quiet --format compact . | sed -E 's/^([\\/\\._a-zA-Z0-9]+):.*\\((.*)\\)$/[\\2] (find-file-other-buffer \"\\1\")/g'"
   "*My Eslint Errors*"))

;;;###autoload
(defun my-eslint-warning-sorted-by-error-find-file-buffer ()
  "Show sorted eslint warning on buffer."
  (interactive)
  (my-projectile-run-async-shell-command-in-root
   "npx eslint --format compact . | sort -k 2 -t \"(\" | sed -E 's/^([\\/\\._a-zA-Z0-9]+):.*\\((.*)\\)$/[\\2] (find-file-other-buffer \"\\1\")/g'"
   "*My Eslint Errors*"))

;;;###autoload
(defun my-eslint-spefic-error-find-file-buffer (error-name)
  "Show eslint ERROR-NAME error on buffer."
  (interactive "sError name: ")
  (my-projectile-run-async-shell-command-in-root
   (concat
    "echo Error: "
    error-name
    ";echo -----------------------------------------\n\n;"
    "npx eslint --format json . | jq '.[] | {filePath: .filePath, ruleId: .messages[].ruleId}' | jq -s '.[] | select (.ruleId ==\""
    error-name
    "\")' | grep filePath | sed -E 's/ +\"filePath\": \"(.*)\".*/\\(find-file-other-window \\\"\\1\\\"\\)/g' | uniq"
    )
   "*My Eslint Specific Errors*"))

;;;###autoload
(defun my-eslint-fix-file ()
  "Run eslint for current file."
  (interactive)
  (message "eslint --fix %s" (buffer-file-name))
  (call-process-shell-command
   (concat "npx eslint --fix " (buffer-file-name))
   nil "*Shell Command Output*" t)
  (revert-buffer t t))

(provide 'my-js)
;;; my-js.el ends here
