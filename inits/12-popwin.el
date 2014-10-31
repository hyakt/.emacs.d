;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin-conf.el

(autoload 'popwin-mode "popwin" nil t)
(popwin-mode 1)

(setq popwin:close-popup-window-timer-interval 0.5)

;; popwin settings
(push '("*Help*" :height 30 :stick t) popwin:special-display-config)
(push '("*Completions*" :noselect t) popwin:special-display-config)
(push '("*compilation*" :noselect t) popwin:special-display-config)
(push '("*Messages*") popwin:special-display-config)
(push '("*Backtrace*" :noselect t) popwin:special-display-config)

(push '("helm" :regexp t :height 0.5) popwin:special-display-config)
(push '("*Kill Ring*" :height 30) popwin:special-display-config)
(push '("*Compile-Log" :height 20 :stick t) popwin:special-display-config)

(push '("*quickrun*" :height 10 :stick t) popwin:special-display-config)

(push '("\*grep\*" :regexp t :height 0.5 :stick t) popwin:special-display-config)
(push '("*Occur*" :noselect t) popwin:special-display-config)

(push '("*eshell*" :height 30) popwin:special-display-config)
(push '("*ansi-term" :regexp t :height 30) popwin:special-display-config)
(push '("*shell*" :height 30) popwin:special-display-config)
(push '("*Shell Command Output*" :noselect t) popwin:special-display-config)

(push '("*Python*" :stick t) popwin:special-display-config)
(push '("*jedi:doc*" :noselect t) popwin:special-display-config)

(push '("*slime-apropos*") popwin:special-display-config)
(push '("*slime-macroexpansion*") popwin:special-display-config)
(push '("*slime-description*") popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push '("*slime-xref*") popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push '(slime-repl-mode) popwin:special-display-config)
(push '(slime-connection-list-mode) popwin:special-display-config)

(push '("*undo-tree*" :width 0.2 :position right) popwin:special-display-config)
(push '("*Google Translate*" :noselect t) popwin:special-display-config)
(push '("*Codic Result*" :noselect t) popwin:special-display-config)

(push '("*magit-commit*" :noselect t :height 30 :width 80 :stick t) popwin:special-display-config)
(push '("*magit-diff*" :noselect t :height 30 :width 80) popwin:special-display-config)
(push '("*magit-edit-log*" :noselect t :height 15 :width 80) popwin:special-display-config)
(push '("*magit-process*" :noselect t :height 15 :width 80) popwin:special-display-config)
(push '("^\*magit: .+\*$" :regexp t :height 0.5) popwin:special-display-config)
