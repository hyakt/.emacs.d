;;; package --- Early initialization file -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-message t)
(setq frame-title-format "")
(setq truncate-lines nil)

(setq gc-cons-threshold most-positive-fixnum)

;; Suppress file-name-handler-alist during startup for faster loading
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))

(provide 'early-init)
;;; early-init.el ends here
