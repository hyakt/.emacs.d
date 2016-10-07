(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind (("C-\\" . org-agenda))
  :config
  ;; Org-Exportの読み込み
  (use-package ox)
  (use-package ox-bibtex)
  (use-package ox-pandoc)

  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)

  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "note.org"))

  ;; Todo状態
;; TODO状態
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" "SOMEDAY(s)")))

  ;; DONEの時刻を記録
  (setq org-log-done 'time)

  ;; -- Org-Agenda -- 
  (setq org-agenda-files (list org-directory))

  (setq org-agenda-custom-commands 
        '(("d" "Daily Report"
           ((agenda "" ((org-agenda-ndays 1)
                        (org-agenda-sorting-strategy
                         (quote ((agenda time-up priority-down tag-up))))
                        (org-deadline-warning-days 0)
                        (org-agenda-clockreport-mode t))))))))

;; Org-Capture
(use-package org-capture
  :bind (("C-`" . org-capture))
  :config
  (setq org-capture-templates
        `(("p" "Project" entry (file+headline "~/org/project.org" "Project")
           "* TODO %^{content}\n DEADLINE: %^{Deadline}t\n%?"
           :unnarrowed t
           :empty-lines 1 )
          ("c" "Chore" entry (file+headline "~/org/chore.org" "Chore")
           "* TODO %^{content}\n DEADLINE: %^{Deadline}t\n%?"
           :prepend t
           :empty-lines t
           :kill-buffer t)
          ("n" "Note" entry (file+headline "~/org/note.org" "Note")
           "* %? %U %i"
           :prepend t
           :empty-lines 1
           :kill-buffer t ))))

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
          ("setpagesize=false,dvipdfmx"     "hyperref"  nil))))
