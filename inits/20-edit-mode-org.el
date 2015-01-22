(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (setq org-startup-folded t)
  (setq org-src-fontify-natively t)

  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "remember.org"))

  ;; Org-Agenda
  (setq org-agenda-files
        '(
          "~/Dropbox/org/todo.org"
          "~/Dropbox/org/schedule.org"
          "~/Dropbox/org/mobile.org"
          "~/Dropbox/org/remember.org"))

  ;; Todo状態
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))

  ;; DONEの時刻を記録
  (setq org-log-done 'time)

  ;; ToDoをDoneにした時の挙動
  (require 'org-archive nil 'noerror)
  (setq org-archive-location "archive.org::")
  (defun my:org-archive-done-tasks ()
    (interactive)
    ;; ARCHIVE タグを付けるだけなら以下
    ;;   (org-map-entries 'org-archive-set-tag "/DONE" 'file))
    ;; org-archive-location に refile したいなら以下
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (add-hook 'org-todo-statistics-hook 'my:org-archive-done-tasks)
  (add-hook 'org-todo-after-statistics-hook 'my:org-archive-done-tasks)

  ;; MobileOrg
  (setq org-mobile-directory "~/Dropbox/アプリ/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/mobile.org")
  )

;; Org-Capture
(use-package org-capture
  :bind (((kbd "C-`") . org-capture))
  :config
  (setq org-capture-templates
        `(
          ("t" "Todo" entry
           (file (concat org-directory "todo.org"))
           "* TODO <%<%Y-%m-%d>> %?\n  %i\n  %a\n"
           :prepend nil
           :unnarrowed nil
           :kill-buffer t
           )
          ("s" "Schedule" entry
           (file (concat org-directory "schedule.org"))
           "* <%<%Y-%m-%d>> %?\n  %i\n  %a\n"
           :prepend nil
           :unnarrowed nil
           :kill-buffer t
           )
          ("r" "Remember" entry
           (file (concat org-directory "remember.org"))
           "* %?\n %U\n %i"
           :prepend
           :unnarrowed nil
           :kill-buffer t
           )
          ))
  )
