(when (eq system-type 'darwin)       ;Mac
  (require 'multi-term)
  (setq multi-term-program shell-file-name)
  (setq multi-term-program "/bin/bash")

  (global-set-key (kbd "C-q") '(lambda ()
                                 (interactive)
                                 (if (get-buffer "*terminal<1>*")
                                     (switch-to-buffer "*terminal<1>*")
                                   (multi-term))))

  (global-set-key (kbd "C-c n") 'multi-term-next)
  (global-set-key (kbd "C-c p") 'multi-term-prev)

  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "C-\\") 'other-window)
              (define-key term-raw-map (kbd "C-y") 'term-paste)
              (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
              (define-key term-raw-map (kbd "<backspace>") 'term-send-backspace)
              (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
              (define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
              (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
              (define-key term-raw-map (kbd "C-v") nil)
              (define-key term-raw-map (kbd "ESC ESC") 'term-send-raw))
            )
  )

(when (eq system-type 'windows-nt)      ;windows
  (global-set-key (kbd "C-q") 'eshell))


