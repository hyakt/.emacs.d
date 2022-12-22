;;; my-git.el --- git用のユーティリティ
;;; Commentary:

;;; Code:

;;;###autoload
(defun my-projectile-run-shell-command-in-root (command)
  (require 'projectile)
  "Invoke `shell-command' COMMAND in the project's root."
  (projectile-with-default-dir
      (projectile-ensure-project (projectile-project-root))
    (shell-command cnnommand)))

;;;###autoload
(defun my-gh-repo-view ()
  "gh open."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh repo view --web"))

;;;###autoload
(defun my-gh-co (target)
  "gh open."
  (interactive "sname/number: ")
  (my-projectile-run-shell-command-in-root (concat "gh co " target))
  (my-revert-buffer-no-confirm))

;;;###autoload
(defun my-gh-pr-view ()
  "gh open."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh pr view --web"))

;;;###autoload
(defun my-gh-pr-create ()
  "gh pr create."
  (interactive)
  (my-projectile-run-shell-command-in-root "git push -u origin HEAD; gh pr create --web --fill"))

;;;###autoload
(defun my-gh-pr-list ()
  "gh open."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh pr list --web"))

;;;###autoload
(defun my-git-open-pr-from-commit-hash (hash)
  "gh open."
  (interactive "sHash: ")
  (my-projectile-run-shell-command-in-root (concat "git openpr " hash)))

;;;###autoload
(defun my-consult-git-commit-messages ()
  (interactive)
  (require 'consult)
  (insert (consult--read
           (split-string
            (shell-command-to-string
             "git log --format=\"%s\"")
            "\n" t)
           :require-match t
           :prompt "Commit message: "
           :sort nil)))

;;;###autoload
(defun my-consult-git-commit-prefix ()
  (interactive)
  (require 'consult)
  (insert (consult--read
           (list
            "feat: "
            "fix: "
            "test: "
            "build: "
            "chore: "
            "ci: "
            "docs: "
            "perf: "
            "refactor: "
            "revert: "
            "style: ")
           :require-match t
           :prompt "prefix: "
           :sort nil)))

(provide 'my-git)

;;; my-git.el ends here
