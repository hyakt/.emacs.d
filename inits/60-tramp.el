(use-package tramp
  :if (eq system-type 'darwin)
  :config
  ;; http://qiita.com/l3msh0/items/6b84082541cbbf7d00f8
  (setenv "TMPDIR" "/tmp")
  )
