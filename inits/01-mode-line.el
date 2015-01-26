; modelineの設定

;; milkypostman/powerline · GitHub
(use-package powerline
  :config
  (powerline-default-theme)
  
;; (set-face-attribute 'powerline-active2 nil
;;                     :foreground "#467BE7"
;;                     :background "#75715e"
;;                     :inherit 'mode-line)

;; (set-face-attribute 'powerline-active1 nil
;;                     :foreground "#AF2B12"
;;                     :background "#75715e"
;;                     :inherit 'mode-line)
)

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (undo-tree-mode . "")
    (git-gutter-mode . "")
    (anzu-mode . "")
    (yas-minor-mode . "")
    (guide-key-mode . "")
    (auto-complete-mode . "")
    (global-whitespace-mode . "")
    (magit-auto-revert-mode . "")
    (smartparens-mode . "")
    (helm-mode . "")
    (company-mode . "")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          )))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
