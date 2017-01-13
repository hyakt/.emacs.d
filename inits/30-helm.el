;;; 30-helm.el --- helm周りの設定
;;; Commentary:

;;; Code:
(use-package helm-config
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("M-y"     . helm-show-kill-ring)
   ("C-c i"   . helm-imenu)
   ("C-x b"   . helm-buffers-list)
   ("C-x C-b" . helm-buffers-list)
   ("C-c e"   . helm-do-ag)
   ("M-x"     . helm-M-x)
   ("C-o" . helm-swoop)
   ("C-z b" . helm-elscreen))
  :config
  (helm-mode 1)
  (helm-migemo-mode +1))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode 1))

(use-package helm-ag
  :config
  (custom-set-variables
   '(helm-ag-base-command "rg --vimgrep --no-heading")))

(use-package helm-gtags
  :bind
  (("M-t" . helm-gtags-find-tag)
   ("M-r" . helm-gtags-find-rtag)
   ("M-s" . helm-gtags-find-symbol)
   ("M-g M-p" . helm-gtags-parse-file)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history)
   ("M-," . helm-gtags-pop-stack)
   ("M-." . helm-gtags-dwim))
  :config
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t)))

(use-package helm-dash
  :bind (("C-x C-d" . helm-dash))
  :config
  (setq helm-dash-browser-func 'eww)

  ;; 適応させたいモードとdocsetを記入
  (defun my-set-doctype-for-helm-dash ()
    (setq-local helm-dash-docsets
                (case major-mode
                  (js2-mode '("JavaScript" "jQuery"))
                  (web-mode '("CSS" "HTML"))
                  (java-mode '("Java"))
                  (python-mode '("Python")))))
  (dolist (hook (list 'js2-mode-hook 'web-mode-hook 'java-mode-hook 'python-mode-hook))
    (add-hook hook 'my-set-doctype-for-helm-dash)))
