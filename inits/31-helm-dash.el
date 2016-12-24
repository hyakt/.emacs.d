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
