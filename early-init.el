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

(provide 'early-init)
;;; early-init.el ends here
