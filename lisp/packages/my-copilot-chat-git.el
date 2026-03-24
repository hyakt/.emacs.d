;;; my-copilot-chat-git.el --- Copilot Chat commit message helper -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'copilot-chat)

(defcustom my-copilot-chat-commit-max-diff-bytes 12000
  "Maximum bytes from staged diff sent to Copilot Chat."
  :type 'integer
  :group 'copilot)

(defcustom my-copilot-chat-commit-system-prompt
  (concat
   "You are a commit message generator.\n"
   "Your ONLY task is to produce a Git commit message.\n"
   "Generate a message that follows Conventional Commits 1.0.0.\n"
   "\n"
   "INPUTS PROVIDED:\n"
   "- git diff --cached output of staged changes\n"
   "\n"
   "PRIMARY GOAL:\n"
   "- Produce one short commit message for the staged changes.\n"
   "\n"
   "FORMAT RULES:\n"
   "- Output exactly one line.\n"
   "- Use: <type>(<scope>): <description> or <type>: <description>.\n"
   "- Allowed types include: feat, fix, docs, style, refactor, perf, test, build, ci, chore.\n"
   "- Use imperative mood.\n"
   "- Keep the line within 72 characters.\n"
   "- No body, no footer, no trailers, no extra explanation.\n"
   "- No markdown, no code fences, no quotes.\n"
   "\n"
   "SELECTION GUIDANCE:\n"
   "- Choose the most meaningful type based on intent, not file extension.\n"
   "- Prefer specific scope when obvious (e.g. parser, ui, lsp, build).\n"
   "- If uncertain, use chore.\n"
   "\n"
   "Return only the final commit message line.")
  "Prompt prefix used to generate commit messages from staged diff."
  :type 'string
  :group 'copilot)

(defvar-local my-copilot-chat--commit-generation-running nil
  "Non-nil while Copilot Chat is generating commit message.")

(defun my-copilot-chat--git-root (&optional start-dir)
  "Return git root from START-DIR or nil."
  (let ((default-directory (or start-dir default-directory)))
    (or (and (fboundp 'magit-toplevel)
             (ignore-errors (magit-toplevel)))
        (locate-dominating-file default-directory ".git"))))

(defun my-copilot-chat--staged-diff (git-root)
  "Return staged diff string from GIT-ROOT, or nil if empty."
  (with-temp-buffer
    (let ((default-directory git-root)
          (status
           (process-file
            "git" nil (current-buffer) nil
            "diff" "--cached" "--no-color" "--no-ext-diff" "--")))
      (when (zerop status)
        (let ((diff (buffer-string)))
          (unless (string-empty-p diff)
            (if (> (length diff) my-copilot-chat-commit-max-diff-bytes)
                (substring diff 0 my-copilot-chat-commit-max-diff-bytes)
              diff)))))))

(defun my-copilot-chat--commit-message-buffer-p ()
  "Return non-nil when current buffer is a git commit message buffer."
  (or (derived-mode-p 'git-commit-mode)
      (derived-mode-p 'git-commit-elisp-text-mode)
      (and buffer-file-name
           (string-match-p "COMMIT_EDITMSG\\'"
                           (file-name-nondirectory buffer-file-name)))))

(defun my-copilot-chat--commit-buffer-has-message-p ()
  "Return non-nil if commit buffer already has a non-comment line."
  (save-excursion
    (goto-char (point-min))
    (let ((bound
           (save-excursion
             (if (re-search-forward "^# -+ >8 -+$" nil t)
                 (line-beginning-position)
               (point-max)))))
      (catch 'found
        (while (< (point) bound)
          (let ((line
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
            (unless (or (string-empty-p (string-trim line))
                        (string-prefix-p "#" (string-trim-left line)))
              (throw 'found t)))
          (forward-line 1))
        nil))))

(defun my-copilot-chat--merge-commit-p (git-root)
  "Return non-nil when merge commit is in progress at GIT-ROOT."
  (my-copilot-chat--git-path-exists-p git-root "MERGE_HEAD"))

(defun my-copilot-chat--git-path-exists-p (git-root git-path)
  "Return non-nil if GIT-PATH exists in repository at GIT-ROOT."
  (with-temp-buffer
    (let ((default-directory git-root))
      (and (zerop (process-file "git" nil (current-buffer) nil
                                "rev-parse" "--git-path" git-path))
           (let ((resolved-path
                  (string-trim (buffer-string))))
             (file-exists-p (if (file-name-absolute-p resolved-path)
                                resolved-path
                              (expand-file-name resolved-path git-root))))))))

(defun my-copilot-chat--replace-commit-message (message)
  "Replace current commit message area with MESSAGE.
Lines starting with `#' are preserved."
  (let ((end
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward "^#" nil t)
               (line-beginning-position)
             (point-max))))
        (inhibit-read-only t))
    (save-excursion
      (delete-region (point-min) end)
      (goto-char (point-min))
      (let ((text (string-trim message)))
        (unless (string-empty-p text)
          (insert text "\n\n"))))))

(defun my-copilot-chat--cleanup-commit-message (text)
  "Normalize Copilot commit TEXT for insertion."
  (let* ((without-follow-up
          (replace-regexp-in-string
           "\n\nFollow-up:.*\\'" "" text))
         (trimmed (string-trim without-follow-up))
         (without-fence
          (replace-regexp-in-string
           "\\````[[:alnum:]_-]*\n\\|\n```\\'" "" trimmed)))
    (string-trim without-fence)))

(defun my-copilot-chat--request-commit-message (diff on-success on-error)
  "Request commit message from DIFF and call ON-SUCCESS/ON-ERROR."
  (let* ((chat-buf (get-buffer-create "*copilot-chat*"))
         (message
          (format
           "%s\n\nStaged diff:\n```diff\n%s\n```\n\nReturn only the commit message."
           my-copilot-chat-commit-system-prompt
           diff))
         (start-time (float-time))
         (timeout-seconds 45)
         (started nil)
         marker
         timer)
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-chat-mode)
        (copilot-chat-mode))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local copilot-chat--source-buffer nil)
      (copilot-chat--insert-prompt message)
      (setq marker (copy-marker (point))))
    (condition-case err
        (progn
          (copilot-chat--create message (lambda (_result) nil))
          (setq timer
                (run-with-timer
                 0.2 0.2
                 (lambda ()
                   (if (not (buffer-live-p chat-buf))
                       (cancel-timer timer)
                     (with-current-buffer chat-buf
                       (let ((raw (buffer-substring-no-properties marker (point-max))))
                         (when (string-match-p "\\[Error:" raw)
                           (cancel-timer timer)
                           (funcall on-error (my-copilot-chat--cleanup-commit-message raw))))
                       (when copilot-chat--streaming-p
                         (setq started t))
                       (when (> (- (float-time) start-time) timeout-seconds)
                         (cancel-timer timer)
                         (funcall on-error "copilot-chat timed out"))
                       (when (and started (not copilot-chat--streaming-p))
                         (cancel-timer timer)
                         (let* ((raw (buffer-substring-no-properties marker (point-max)))
                                (msg (my-copilot-chat--cleanup-commit-message raw)))
                           (if (or (string-empty-p msg)
                                   (string-prefix-p "[Error:" msg))
                               (funcall on-error (if (string-empty-p msg)
                                                     "copilot-chat returned empty response"
                                                   msg))
                             (funcall on-success msg))))))))))
      (error
       (funcall on-error (format "%s" err))))))

;;;###autoload
(defun my-copilot-chat-generate-commit-message ()
  "Auto insert commit message from staged diff via Copilot Chat."
  (interactive)
  (cond
   ((not (my-copilot-chat--commit-message-buffer-p))
    (user-error "Run this in a git commit message buffer (git-commit-mode)"))
   (my-copilot-chat--commit-generation-running
    (message "Copilot Chat commit message is already being generated."))
   (t
    (let* ((commit-buf (current-buffer))
           (git-root (my-copilot-chat--git-root default-directory))
           (diff (and git-root (my-copilot-chat--staged-diff git-root))))
      (cond
       ((not git-root)
        (message "Not inside a git repository."))
       ((my-copilot-chat--merge-commit-p git-root)
        (message "Skipping Copilot generation for merge commits."))
       ((my-copilot-chat--commit-buffer-has-message-p)
        (message "Skipping Copilot generation for amend commits."))
       ((not diff)
        (message "No staged changes. Stage files before generating message."))
       (t
        (setq-local my-copilot-chat--commit-generation-running t)
        (let ((status-message "Generating commit message with Copilot Chat..."))
          (my-copilot-chat--replace-commit-message status-message))
        (my-copilot-chat--request-commit-message
         diff
         (lambda (message)
           (when (buffer-live-p commit-buf)
             (with-current-buffer commit-buf
               (setq-local my-copilot-chat--commit-generation-running nil)
               (my-copilot-chat--replace-commit-message message))))
         (lambda (err)
           (when (buffer-live-p commit-buf)
             (with-current-buffer commit-buf
               (setq-local my-copilot-chat--commit-generation-running nil)
               (my-copilot-chat--replace-commit-message "")
               (message "Copilot Chat commit message skipped: %s" err)))))))))))

;;;###autoload
(defun my-copilot-chat--git-commit-setup ()
  "Generate commit message shortly after commit buffer opens."
  (let ((commit-buf (current-buffer)))
    (run-with-timer 0.8 nil
                    (lambda ()
                      (when (buffer-live-p commit-buf)
                        (with-current-buffer commit-buf
                          (my-copilot-chat-generate-commit-message)))))))

(provide 'my-copilot-chat-git)
;;; my-copilot-chat-git.el ends here
