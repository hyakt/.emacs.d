;;
;; C/Migemo
;;

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
  )
  )

(when (eq system-type 'windows-nt)      ;windows
  (setq migemo-command (concat (getenv "INST_DIR")
                               "~\\cmigemo\\cmigemo"))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary (concat (getenv "INST_DIR")
                                  "~\\cmigemo\\dict\\utf-8\\migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  )

