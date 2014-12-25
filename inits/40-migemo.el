;; C/Migemo
(when (eq system-type 'darwin)          ;mac
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))

    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs" "-i" "\g"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)

    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1000)

    (load-library "migemo")
    (migemo-init)

    ;;helm-migemoの設定　http://rubikitch.com/2014/12/19/helm-migemo/
    (require 'helm-migemo)
    (eval-after-load "helm-migemo"
      '(defun helm-compile-source--candidates-in-buffer (source)
         (helm-aif (assoc 'candidates-in-buffer source)
             (append source
                     `((candidates
                        . ,(or (cdr it)
                               (lambda ()
                                 ;; Do not use `source' because other plugins
                                 ;; (such as helm-migemo) may change it
                                 (helm-candidates-in-buffer (helm-get-current-source)))))
                       (volatile) (match identity)))
           source)))

    )
  )
