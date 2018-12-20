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
  :config
  (use-package ob-sql-mode)
  (use-package ox-gfm :after ox)
  (use-package ox-qmd :after ox
    :straight (ox-qmd :type git :host github :repo "0x60df/ox-qmd"))

  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)

  (use-package org-bullets
    :init
    (setq org-bullets-bullet-list '("■" "○" "✸" "►" "•" "★"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; Todo状態
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOIN(i)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)" "SMDY(s)")))

  ;; DONEの時刻を記録
  (setq org-log-done 'time))

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.mdown\\'" . gfm-mode))
  :config
  (add-hook 'gfm-mode-hook '(lambda ()
                              (custom-set-faces
                               '(markdown-header-face-1 ((t (:inherit outline-1 markdown-header-face))))
                               '(markdown-header-face-2 ((t (:inherit outline-2 markdown-header-face))))
                               '(markdown-header-face-3 ((t (:inherit outline-3 markdown-header-face))))
                               '(markdown-header-face-4 ((t (:inherit outline-4 markdown-header-face))))
                               '(markdown-header-face-5 ((t (:inherit outline-5 markdown-header-face))))
                               '(markdown-header-face-6 ((t (:inherit outline-6 markdown-header-face)))))

                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-1-atx '((1 markdown-header-face-1)
                                                                               (2 markdown-header-face-1)
                                                                               (3 markdown-header-face-1))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-2-atx '((1 markdown-header-face-2)
                                                                               (2 markdown-header-face-2)
                                                                               (3 markdown-header-face-2))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-3-atx '((1 markdown-header-face-3)
                                                                               (2 markdown-header-face-3)
                                                                               (3 markdown-header-face-3))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-4-atx '((1 markdown-header-face-4)
                                                                               (2 markdown-header-face-4)
                                                                               (3 markdown-header-face-4))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-5-atx '((1 markdown-header-face-5)
                                                                               (2 markdown-header-face-5)
                                                                               (3 markdown-header-face-5))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-6-atx '((1 markdown-header-face-6)
                                                                               (2 markdown-header-face-6)
                                                                               (3 markdown-header-face-6)))))))

;;; 21-mode-doc ends here
