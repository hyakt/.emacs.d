;; ヒント文字に使う文字を指定する
(setq ace-jump-mode-move-keys
       (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))

;; ace-jump-word-modeのとき文字を尋ねないようにする
;(setq ace-jump-word-mode-use-query-char nil)
(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "C-:") 'ace-jump-line-mode)
