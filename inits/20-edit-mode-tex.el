;; latexの設定(auctex)
(when (require 'tex-site nil t)
  (require 'tex-site)

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; auctex用のlatexmkを追加
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (require 'auctex-latexmk nil 'noerror)
                        (auctex-latexmk-setup)
                        )))

  ;; auctex用のauto-completeを読み込み
  (eval-after-load 'latex
    '(require 'auto-complete-auctex))

  ;; reftexの設定
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  )
