(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

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
                                       (3 markdown-header-face-6))))
))
