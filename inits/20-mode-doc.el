;;; 20-edit-mode-doc.el --- 文章編集用メジャーモード設定
;;; Commentary:

;;; Code:
;; Org
(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind (("C-\\" . org-agenda)
         ("C-M-]" . org-cycle-list-bullet))
  :config
  ;; Org-Exportの読み込み
  (use-package ox)

  (setq org-startup-truncated nil)
  (setq org-src-fontify-natively t)

  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "note.org"))

  (use-package org-bullets
    :ensure t
    :init
    (setq org-bullets-bullet-list '("■" "◎" "○" "●" "►" "◇"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; Todo状態
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOIN(i)" "WAIT(w)" "|" "DONE(d)" "CNCL(c)" "SMDY(s)")))

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

(use-package org-capture
  :bind (("C-`" . org-capture))
  :config
  (setq org-capture-templates
        `(("p" "Project" entry (file+headline "~/org/project.org" "Project")
           "* TODO %^{content}\n %i SCHEDULED: %^{Scheduled}t DEADLINE: %^{Deadline}t\n %i %?"
           :unnarrowed t
           :empty-lines 1 )
          ("c" "Chore" entry (file+headline "~/org/chore.org" "Chore")
           "* TODO %^{content} %u\n %i SCHEDULED: %^{Scheduled}t DEADLINE: %^{Deadline}t\n %i %?"
           :prepend t
           :empty-lines 1
           :kill-buffer t)
          ("n" "Note" entry (file+headline "~/org/note.org" "Note")
           "* %? %U %i"
           :prepend t
           :empty-lines 1
           :kill-buffer t ))))

(use-package ox-hugo :after ox)
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

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.mdown\\'" . gfm-mode))
  :config
  (add-hook 'gfm-mode-hook '(lambda ()
                              (custom-set-faces
                               '(markdown-header-face-1 ((t (:inherit outline-1 markdown-header-face))))
                               '(markdown-header-face-2 ((t (:inherit outline-2 markdown-header-face))))
                               '(markdown-header-face-3 ((t (:inherit outline-3 markdown-header-face))))
                               '(markdown-header-face-4 ((t (:inherit outline-4 markdown-header-face))))
                               '(markdown-header-face-5 ((t (:inherit outline-5 markdown-header-face))))
                               '(markdown-header-face-6 ((t (:inherit outline-6 markdown-header-face)))))

                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-1-atx '((1 markdown-header-face-1)
                                                                               (2 markdown-header-face-1)
                                                                               (3 markdown-header-face-1))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-2-atx '((1 markdown-header-face-2)
                                                                               (2 markdown-header-face-2)
                                                                               (3 markdown-header-face-2))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-3-atx '((1 markdown-header-face-3)
                                                                               (2 markdown-header-face-3)
                                                                               (3 markdown-header-face-3))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-4-atx '((1 markdown-header-face-4)
                                                                               (2 markdown-header-face-4)
                                                                               (3 markdown-header-face-4))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-5-atx '((1 markdown-header-face-5)
                                                                               (2 markdown-header-face-5)
                                                                               (3 markdown-header-face-5))))
                              (add-to-list 'markdown-mode-font-lock-keywords-basic
                                           (cons markdown-regex-header-6-atx '((1 markdown-header-face-6)
                                                                               (2 markdown-header-face-6)
                                                                               (3 markdown-header-face-6)))))))

