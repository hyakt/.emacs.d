;;; 21-mode-doc.el --- 文章編集用メジャーモード設定
;;; Commentary:

;;; Code:

;; CSV
(use-package csv-mode)

;; Org
(use-package org
  :straight nil
  :mode (("\\.txt$" . org-mode))
  :bind (("C-M-]" . org-cycle-list-bullet)
         :map org-mode-map ("C-," . nil))
  :custom
  (org-startup-truncated nil)
  (org-src-fontify-natively t)
  (org-log-done 'time)
  :config
  (defun my-add-custom-id ()
    "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
    (interactive)
    (my-org-custom-id-get nil t))

  (defun my-get-custom-id ()
    "Return a part of UUID with an \"org\" prefix. e.g. \"org3ca6ef0c\"."
    (let* ((id (org-id-new "")))
      (when (org-uuidgen-p id)
        (downcase (concat "org"  (substring (org-id-new "") 0 8))))))

  (defun my-org-custom-id-get (&optional pom create)
    "See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (my-get-custom-id))
          (unless id
            (error "Invalid ID"))
          (org-entry-put pom "CUSTOM_ID" id)
          (message "--- CUSTOM_ID assigned: %s" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (use-package ob-sql-mode)
  (use-package ox-gfm :after ox)
  (use-package org-bullets
    :custom
    (org-bullets-bullet-list '("■" "○" "✸" "►" "•" "★"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.mdown\\'" . gfm-mode)))

;;; 21-mode-doc ends here
