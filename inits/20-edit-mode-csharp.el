(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook
            '(lambda ()
               (setq indent-tabs-mode nil)
               (setq c-basic-offset 4)
               (c-set-offset 'substatement-open 0)
               (flycheck-mode 1)
               (omnisharp-mode)))
  
  (use-package omnisharp
    :config
    (setq omnisharp-server-executable-path (expand-file-name "/Users/hayato/Documents/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))))
