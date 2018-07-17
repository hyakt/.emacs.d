;;; 20-edit-mode-other.el --- その他メジャーモード設定
;;; Commentary:

;;; Code:
;; EDBI
(use-package edbi)

;; docker
(use-package docker
  :bind(( "C-x c" . docker))
  :ensure t)

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
