;;; my-git.el --- git用のユーティリティ
;;; Commentary:

;;; Code:
(eval-when-compile (package-initialize))

(require 'projectile)

;;;###autoload
(defun my-projectile-run-shell-command-in-root (command)
  "Invoke `shell-command' COMMAND in the project's root."
  (projectile-with-default-dir
      (projectile-ensure-project (projectile-project-root))
    (shell-command command)))

;;;###autoload
(defun my-gh-repo-view ()
  "Gh repo view."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh repo view --web"))

;;;###autoload
(defun my-gh-co (target)
  "Gh checkout TARGET."
  (interactive "sname/number: ")
  (my-projectile-run-shell-command-in-root (concat "gh co " target))
  (my-revert-buffer-no-confirm))

;;;###autoload
(defun my-gh-pr-view ()
  "Gh open pr."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh pr view --web"))

;;;###autoload
(defun my-gh-pr-create ()
  "Gh pr create."
  (interactive)
  (my-projectile-run-shell-command-in-root "git push -u origin HEAD; gh pr create --web --fill"))

;;;###autoload
(defun my-gh-pr-list ()
  "Gh pull request list."
  (interactive)
  (my-projectile-run-shell-command-in-root "gh pr list --web"))

;;;###autoload
(defun my-git-open-pr-from-commit-hash (hash)
  "Git openpr HASH."
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
(defun my-consult-git-conventional-commit-prefix ()
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

;;;###autoload
(defun my-git-wip ()
  (interactive)
  (my-projectile-run-shell-command-in-root "git wip"))

(provide 'my-git)

;;; my-git.el ends here
