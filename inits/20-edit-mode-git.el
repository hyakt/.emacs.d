(require 'magit)

(set-face-background 'magit-item-highlight "#202020")
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")
(set-face-foreground 'magit-diff-file-header "blue")

(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

(require 'gitconfig-mode)
(require 'gitignore-mode)
