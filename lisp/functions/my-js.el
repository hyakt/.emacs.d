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

(defun my-js-detect-package-manager ()
  "Detect the package manager used in the current project."
  (let* ((project-root (projectile-project-root)))
    (cond
     ;; ロックファイルを確認
     ((file-exists-p (concat project-root "pnpm-lock.yaml"))
      "pnpm")
     ((file-exists-p (concat project-root "yarn.lock"))
      "yarn")
     ((file-exists-p (concat project-root "package-lock.json"))
      "npm")
     ;; デフォルト
     (t "npm"))))

(defun my-js-get-package-runner (pkg-manager)
  "Get the appropriate command prefix for running binaries with PKG-MANAGER."
  (cond
   ((string= pkg-manager "npm") "npx")
   ((string= pkg-manager "pnpm") "pnpm")
   ((string= pkg-manager "yarn") "yarn")
   (t "npx"))) ; デフォルト

(defun my-js-create-vitest-command (watch-flag env-options)
  "Create a vitest command with appropriate package manager.
WATCH-FLAG if non-nil adds --watch to the command.
ENV-OPTIONS can be used to pass additional environment variables."
  (let* ((pkg-manager (my-js-detect-package-manager))
         (runner (my-js-get-package-runner pkg-manager))
         (watch-option (if watch-flag " --watch " " "))
         ;; pnpmの場合だけNODE_OPTIONSを設定
         (node-options (if (string= pkg-manager "pnpm")
                           "NODE_OPTIONS='--no-experimental-fetch' "
                         ""))
         (env-vars (if env-options (concat env-options " ") "")))
    (concat env-vars node-options runner " vitest" watch-option (buffer-file-name))))

(defun my-js-handle-vitest-command (watch-flag env-options action)
  "Handle vitest command creation and specified ACTION.
WATCH-FLAG determines if --watch should be added.
ENV-OPTIONS specifies any environment variables.
ACTION is a function that takes project-root and vitest-command as arguments."
  (let* ((project-root (projectile-project-root))
         (vitest-command (my-js-create-vitest-command watch-flag env-options)))
    (funcall action project-root vitest-command)))

(defun my-js-copy-command-to-clipboard (project-root command &optional trailing-space)
  "Copy COMMAND prefixed with cd PROJECT-ROOT to clipboard.
Add TRAILING-SPACE if specified."
  (let ((full-command (concat "cd " project-root "; " command
                              (if trailing-space " ; " ";"))))
    (kill-new full-command)
    (message full-command)))

;;;###autoload
(defun my-vitest-copy-command-current-buffer ()
  "Run vitest for current file and copy command to clipboard."
  (interactive)
  (my-js-handle-vitest-command
   nil "DEBUG_PRINT_LIMIT=100000"
   (lambda (root cmd) (my-js-copy-command-to-clipboard root cmd))))

;;;###autoload
(defun my-vitest-copy-command-watch-current-buffer ()
  "Watch vitest for current file and copy command to clipboard."
  (interactive)
  (my-js-handle-vitest-command
   t nil
   (lambda (root cmd) (my-js-copy-command-to-clipboard root cmd t))))

;;;###autoload
(defun my-vitest-command-watch-tmux ()
  "Watch vitest for current file in another tmux pane."
  (interactive)
  (my-js-handle-vitest-command
   t nil
   (lambda (root cmd)
     (my-open-alacritty-tmux-current-buffer)
     (let ((full-command (concat "cd " root "; " cmd)))
       (shell-command (concat "tmux send-keys '" full-command "' Enter"))
       (message full-command)))))

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
