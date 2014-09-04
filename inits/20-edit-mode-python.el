(require 'python)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)

            (require 'jedi)
            (jedi:ac-setup)
            (setq jedi:complete-on-dot t)
            
            )
          )
