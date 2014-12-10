;;auto-complete-modeの読み込み
(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)
(ac-config-default)

(setq ac-delay 0.05)
(setq ac-auto-start 3)                         ; 4 文字以上で起動
(setq ac-auto-show-menu 0.8)                   ; 0.8秒でメニュー表示
(setq ac-use-comphist t)                       ; 補完候補をソート
(setq ac-candidate-limit nil)                  ; 補完候補表示を無制限に
(setq ac-quick-help-delay 0.5)
(setq ac-use-fuzzy t)

(setq ac-use-menu-map t)                       ; キーバインド
(define-key ac-menu-map (kbd "C-n")         'ac-next)
(define-key ac-menu-map (kbd "C-p")         'ac-previous)
(define-key ac-completing-map (kbd "M-/")   'ac-stop)

(add-to-list 'ac-modes 'arduino-mode)

(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))
