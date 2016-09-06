(use-package org
  :mode (("\\.org$" . org-mode)
	 ("\\.txt$" . org-mode))
  :config
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (setq org-startup-folded t)
  (setq org-src-fontify-natively t)

  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "remember.org"))

  ;; Org-Exportの読み込み
  (use-package ox)
  (use-package ox-bibtex)
  (use-package ox-pandoc)

  ;; Org-Agenda
  (setq org-agenda-files
        '(
          "~/Dropbox/org/todo.org"
          "~/Dropbox/org/schedule.org"
          "~/Dropbox/org/remember.org"))

  ;; Todo状態
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))

  ;; DONEの時刻を記録
  (setq org-log-done 'time)

  ;; ToDoをDoneにした時の挙動
  (require 'org-archive nil 'noerror)
  (setq org-archive-location "archive.org::")
  (defun my:org-archive-done-tasks ()
    (interactive)
    ;; ARCHIVE タグを付けるだけなら以下
    ;;   (org-map-entries 'org-archive-set-tag "/DONE" 'file))
    ;; org-archive-location に refile したいなら以下
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (add-hook 'org-todo-statistics-hook 'my:org-archive-done-tasks)
  (add-hook 'org-todo-after-statistics-hook 'my:org-archive-done-tasks))

;; Org-Capture
(use-package org-capture
;  :bind (("C-`" . org-capture))
  :config
  (setq org-capture-templates
        `(
          ("t" "Todo" entry
           (file (concat org-directory "todo.org"))
           "* TODO <%<%Y-%m-%d>> %?\n  %i\n  %a\n"
           :prepend nil
           :unnarrowed nil
           :kill-buffer t
           )
          ("s" "Schedule" entry
           (file (concat org-directory "schedule.org"))
           "* <%<%Y-%m-%d>> %?\n  %i\n  %a\n"
           :prepend nil
           :unnarrowed nil
           :kill-buffer t
           )
          ("r" "Remember" entry
           (file (concat org-directory "remember.org"))
           "* %?\n %U\n %i"
           :prepend
           :unnarrowed nil
           :kill-buffer t
           )
          ))
  )


;; Org-Latex
(use-package ox-latex
  :config
  (setq org-latex-default-class "jsarticle")
  (setq org-latex-pdf-process '("latexmk -e '$latex=q/platex %S/' -e '$bibtex=q/pbibtex/' -e '$makeindex=q/mendex -U -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))

  (setq org-file-apps
        '(("pdf" . "/usr/bin/open -a Preview.app %s")))

  (setq org-latex-with-hyperref nil)
  (setq org-latex-hyperref-template nil)

  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[dvipdfmx,12pt,a4paper]{jsarticle}
                  [NO-DEFAULT-PACKAGES]
                  \\usepackage{amsmath}
                  \\usepackage[dvipdfmx]{graphicx}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("thesis"
                 "\\documentclass[a4j, 11pt]{ujreport}
                  [NO-DEFAULT-PACKAGES]
                  \\usepackage{amsmath}
                  \\usepackage[dvipdfmx]{graphicx}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("ipsj"
                 "\\documentclass[submit]{ipsj}
                  [NO-DEFAULT-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-export-latex-packages-alist
      '(("AUTO" "inputenc"  t)   ; automatically replaced with a coding system
        ("T1"   "fontenc"   t)
        ("deluxe,expert,multi"     "otf"   nil)
        (""     "txfonts"   nil)
        (""     "graphicx"  t)
        ("dvipdfmx"     "color"  nil)
        ("setpagesize=false,dvipdfmx"     "hyperref"  nil)
        )))
