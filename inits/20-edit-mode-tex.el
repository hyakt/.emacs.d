;; latexの設定(auctex)
(use-package tex-site
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; auctex用のlatexmkを追加
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (require 'auctex-latexmk nil 'noerror)
                        (auctex-latexmk-setup)))
            (use-package company-auctex
              :config
              (company-auctex-init)))

  ;; reftexの設定
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))
