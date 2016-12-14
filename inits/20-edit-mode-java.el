(use-package eclim
  :config
  (require 'eclimd)
  (custom-set-variables
  '(eclim-eclipse-dirs '("/usr/local/sb/eclipse"))
  '(eclim-executable "/usr/local/sb/eclipse/eclim")
  '(eclimd-executable "/usr/local/sb/eclipse/eclimd")
  '(eclimd-default-workspace "/usr/local/sb/workspace")
  ;; eclimd の起動が完了するのを待たない（*eclimd* バッファで起動の状況を確認できます）
  '(eclimd-wait-for-process nil))

  (global-eclim-mode)
  (add-hook 'java-mode-hook 'eclim-mode)

  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 1)
  (help-at-pt-set-timer)

  (use-package company
    :config
    (require 'company-emacs-eclim)
    (company-emacs-eclim-setup)))

;; (use-package jdee
;;   :init (add-hook 'jdee-mode-hook
;;           '(lambda ()
;;              (define-key jdee-mode-map "\C-j" 'jdee-complete-minibuf)))
;;   :config
;;   (setq jdee-server-dir "~/.emacs.d/jdee-server/target/"))
