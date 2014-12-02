(require 'dired)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

(define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd ")") 'dired-hide-details-mode)
