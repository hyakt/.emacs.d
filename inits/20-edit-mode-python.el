(require 'python)
(require 'py-autopep8)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (add-hook 'before-save-hook 'py-autopep8-before-save)

            ;; Macならjediを読み込む
            (when (eq system-type 'darwin)
              (require 'jedi)
              (add-hook 'python-mode-hook
                        (jedi:setup)
                        (setq jedi:complete-on-dot t)
                        )
              )
            )
          )



