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
(defun my-tsc-error-find-file-buffer (&optional tsc-command)
  "Show tsc error on buffer.
Optional argument TSC-COMMAND is the TypeScript check command to use.
If not provided, it will detect and use the appropriate command based on the project."
  (interactive
   (list (let* ((pkg-manager (my-js-detect-package-manager))
                (runner (my-js-get-package-runner pkg-manager))
                (default-command (concat runner " tsc --noEmit"))
                (project-root (projectile-project-root))
                (tsconfig (and project-root
                               (file-exists-p (concat project-root "tsconfig.json"))))
                (has-type-check (and project-root
                                     (with-temp-buffer
                                       (insert-file-contents (concat project-root "package.json"))
                                       (goto-char (point-min))
                                       (when (re-search-forward "\"type-check\"\\s-*:" nil t)
                                         t))))
                (suggested-command (cond
                                    (has-type-check (concat runner " type-check"))
                                    (tsconfig default-command)
                                    (t default-command))))
           (read-string "TypeScript check command: " suggested-command nil default-command))))
  (let ((command (or tsc-command
                     (let ((pkg-manager (my-js-detect-package-manager)))
                       (concat (my-js-get-package-runner pkg-manager) " tsc --noEmit")))))
    (my-projectile-run-async-shell-command-in-root
     (concat command " --pretty false | "
             ;; 重要なエラー情報を抽出（簡略化したアプローチ）
             "grep -E '^[^[:space:]].*\\([0-9]+,[0-9]+\\):' | "
             "grep -v 'node_modules' | "
             "sed -E 's/^([^(]+)\\(([0-9]+,[0-9]+)\\): (.*)/\\1 - \\3/' | "
             "sort | uniq | "
             ;; 実際に存在するファイルのみをフィルタリング（awkを使用）
             "awk 'BEGIN { "
             "RED=\"\\033[1;31m\"; " ; 赤色（エラー用）
             "RESET=\"\\033[0m\"; " ; リセット
             "} "
             "{ split($0, parts, \" - \"); file=parts[1]; msg=substr($0, length(file) + 4); "
             "cmd=\"test -f \\\"\" file \"\\\" && echo 1 || echo 0\"; cmd | getline exists; close(cmd); "
             "if (exists==\"1\") print \"\\n\" RED \"[ERROR]\" RESET \" \" msg \"\\n    => (find-file-other-window \\\"\" file \"\\\")\" }' ")
     "*My TSC Errors*")))

;;;###autoload
(defun my-eslint-error-find-file-buffer (&optional eslint-command)
  "Show sorted eslint warning on buffer.
Optional argument ESLINT-COMMAND is the eslint command to use.
If not provided, it will detect and use the appropriate command based on the project."
  (interactive
   (list (let* ((pkg-manager (my-js-detect-package-manager))
                (runner (my-js-get-package-runner pkg-manager))
                (default-command (concat runner " eslint")))
           (read-string "ESLint command: " default-command nil default-command))))
  (let ((command (or eslint-command
                     (let ((pkg-manager (my-js-detect-package-manager)))
                       (concat (my-js-get-package-runner pkg-manager) " eslint")))))
    (my-projectile-run-async-shell-command-in-root
     (concat command " --format json . | "
             "jq '.[] | select(.messages | length > 0) | .filePath + \" (\" + (.messages[0].ruleId // \"error\") + \") \" + .messages[0].message' | "
             "grep -v \"node_modules\" | "
             "sort | "
             "sed -E 's/(.*) \\((.*)\\) (.*)/\\n\\1\\n[\\2] \\3\\n(find-file-other-buffer \"\\1\")/g' | "
             "awk 'BEGIN { "
             "YELLOW=\"\\033[1;33m\"; " ; 黄色（警告用）
             "RESET=\"\\033[0m\"; " ; リセット
             "} "
             "{ if ($0 ~ /^\\[/) { "
             "  gsub(/^\\[(.*?)\\]/, YELLOW \"&\" RESET); "
             "} "
             "print $0 }"
             "'")
     "*My Eslint Warnings*")))

;;;###autoload
(defun my-eslint-spefic-error-find-file-buffer (error-name &optional eslint-command)
  "Show eslint ERROR-NAME error on buffer.
Optional argument ESLINT-COMMAND is the eslint command to use."
  (interactive
   (list
    (read-string "Error name: ")
    (let* ((pkg-manager (my-js-detect-package-manager))
           (runner (my-js-get-package-runner pkg-manager))
           (default-command (concat runner " eslint")))
      (read-string "ESLint command: " default-command nil default-command))))
  (let ((command (or eslint-command
                     (let ((pkg-manager (my-js-detect-package-manager)))
                       (concat (my-js-get-package-runner pkg-manager) " eslint")))))
    (my-projectile-run-async-shell-command-in-root
     (concat
      "RED=\"\\033[1;31m\"; " ; 赤色（エラー用）
      "RESET=\"\\033[0m\"; " ; リセット
      "echo \"Rule: ${RED}" error-name "${RESET}\"; "
      "echo \"-----------------------------------------\"; "
      "echo; "
      command " --format json . | "
      "jq '.[] | select(.messages | map(select(.ruleId == \"" error-name "\")) | length > 0) | "
      "\".filePath + \\\" (line: \\\" + (.messages[0].line | tostring) + \\\")\\\"\"' | "
      "grep -v \"node_modules\" | "
      "xargs -I{} bash -c 'eval \"echo ${BLUE}$1${RESET}\"' -- {} | "
      "sed -E 's/(.*)/\\1\\n(find-file-other-window \"\\1\")/g' | "
      "xargs -I{} bash -c 'if [[ \"{}\" == \"(find-file\"* ]]; then eval \"echo ${YELLOW}{}${RESET}\"; else echo \"{}\"; fi' | "
      "sort | uniq")
     "*My Eslint Specific Errors*")))

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
