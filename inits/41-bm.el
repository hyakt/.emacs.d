(use-package bm
  :bind(((kbd "M-[") . bm-previous)
        ((kbd "M-]") . bm-next))

  :config
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  )

(use-package helm-bm
  :bind(((kbd "M-@") . bm-toggle-or-helm))
  :config
  ;; migemoくらいつけようね
  (push '(migemo) helm-source-bm)
  ;; annotationはあまり使わないので仕切り線で表示件数減るの嫌
  (setq helm-source-bm (delete '(multiline) helm-source-bm))

  (defun bm-toggle-or-helm ()
    "2回連続で起動したらhelm-bmを実行させる"
    (interactive)
    (bm-toggle)
    (when (eq last-command 'bm-toggle-or-helm)
      (helm-bm)))

  ;; これがないとemacs -Qでエラーになる。おそらくバグ。
  (require 'compile)
  )

