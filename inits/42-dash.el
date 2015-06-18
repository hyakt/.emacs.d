(use-package dash-at-point
  :bind (("C-x j" . dash-at-point))
  :config
  (add-to-list 'dash-at-point-mode-alist '(csharp-mode . "cs")))
