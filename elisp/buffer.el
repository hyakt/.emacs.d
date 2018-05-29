(use-package ov
  :init
  (defun my/buffer-ricty-face ()
    "Minchoize current buffer."
    (interactive)
    (ov (point-min) (point-max) 'face '(:family "Ricty Diminished"))))

(provide 'buffer)
