(use-package company
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)

  ;;Addrevの時のdowncaseをやめる
  (setq company-dabbrev-downcase nil)

  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-omnisharp)
  (add-to-list 'company-backends 'company-edbi))
