;;; 20-edit-mode-other.el --- その他メジャーモード設定
;;; Commentary:

;;; Code:
;; EDBI
(use-package edbi)

;; PDF
(use-package pdf-tools
  :commands (pdf-tools-install)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (use-package pdf-annot)
  (use-package pdf-links)
  (use-package pdf-info)
  (use-package pdf-misc)
  (use-package pdf-sync)
  (use-package pdf-occur)
  (use-package pdf-outline)

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-misc-size-indication-minor-mode)
              (pdf-links-minor-mode)
              (pdf-isearch-minor-mode))))

;; Git
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (delete-window))

  (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  ;; 読んだ
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))
