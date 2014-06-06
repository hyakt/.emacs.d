(require 'shell-pop)

(custom-set-variables
 '(shell-pop-set-internal-mode "multi-term")
 '(shell-pop-set-internal-mode-shell "/bin/bash")
 '(shell-pop-set-window-height 45)
 '(shell-pop-set-window-position "bottom")
 '(shell-pop-shell-type (quote ("multi-term" "*pop-terminal*" (lambda nil (multi-term)))))
 '(shell-pop-term-shell "/bin/zsh")
; '(shell-pop-universal-key "C-t")
 '(shell-pop-window-height 30))

(global-set-key (kbd "C-t") 'shell-pop)
