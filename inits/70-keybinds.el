;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;キーバインドの設定;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.08)

;; (require 'key-combo)
;; (key-combo-load-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; 通常操作 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
(global-set-key "\C-x\C-i" 'indent-region) ; 選択範囲をインデント
(global-set-key "\C-m" 'newline-and-indent) ; リターンで改行とインデント
(global-set-key "\C-j" 'newline) ; 改行

;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

(global-set-key (kbd "C-x ?") 'help-command)

;;; 少しずつスクロール
(define-key global-map (kbd "C-s-n") 'scroll-down-in-place)
(define-key global-map (kbd "C-s-p") 'scroll-up-in-place)

;; redo
(global-set-key "\M-/" 'redo)
;;
;; shell
(global-set-key (kbd "C-q") 'eshell)

;; goto
(key-chord-define-global "gl" 'goto-line)

;; magit
(key-chord-define-global "gt" 'magit-status)

;; ace-jump-buffer
(define-key global-map (kbd "C-'") 'ace-jump-buffer)
