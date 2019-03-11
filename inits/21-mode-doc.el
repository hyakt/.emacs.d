;;; 21-mode-doc.el --- 文章編集用メジャーモード設定
;;; Commentary:

;;; Code:

;; CSV
(use-package csv-mode)

;; Org
(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind (("C-M-]" . org-cycle-list-bullet)
         :map org-mode-map ("C-," . nil))
  :custom
  (org-startup-truncated nil)
  (org-src-fontify-natively t)
  (org-log-done 'time)
  :config
  (use-package ob-sql-mode)
  (use-package ox-gfm :after ox)
  (use-package ox-qmd :after ox
    :straight (ox-qmd :type git :host github :repo "0x60df/ox-qmd"))
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
