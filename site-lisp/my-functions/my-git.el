;;; my-git.el --- git用のユーティリティ
;;; Commentary:

;;; Code:
(leaf my-git
  :ensure-system-package gh
  :ensure projectile
  :config
  (defun my/projectile-run-shell-command-in-root (command)
    "Invoke `shell-command' COMMAND in the project's root."
    (projectile-with-default-dir
        (projectile-ensure-project (projectile-project-root))
      (shell-command command)))

  (defun my/gh-repo-view ()
    "gh open."
    (interactive)
    (my/projectile-run-shell-command-in-root "gh repo view --web"))

  (defun my/gh-co (target)
    "gh open."
    (interactive "sname/number: ")
    (my/projectile-run-shell-command-in-root (concat "gh co " target))
    (my/revert-buffer-no-confirm))

  (defun my/gh-pr-view ()
    "gh open."
    (interactive)
    (my/projectile-run-shell-command-in-root "gh pr view --web"))

  (defun my/gh-pr-create ()
    "gh open."
    (interactive)
    (my/projectile-run-shell-command-in-root "gh pr create --web --base (git parent)"))

  (defun my/gh-pr-list ()
    "gh open."
    (interactive)
    (my/projectile-run-shell-command-in-root "gh pr list --web"))
  )

(provide 'my-git)

;;; my-git.el ends here
