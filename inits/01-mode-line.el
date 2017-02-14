;;; 01-mode-line.el --- modelineの設定
;;; Commentary:

;;; Code:
;; custom-modeline
;; (use-package amitp-mode-line)

(use-package all-the-icons
  :config
  (zerodark-setup-modeline-format))

(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (undo-tree-mode . "/Ut")
    (git-gutter-mode . "")
    (anzu-mode . "")
    (yas-minor-mode . "/Ys")
    (guide-key-mode . "")
    (auto-complete-mode . "/Ac")
    (global-whitespace-mode . "")
    (magit-auto-revert-mode . "")
    (smartparens-mode . "")
    (helm-mode . "")
    (helm-gtags-mode . "/Gt")
    (company-mode . "/Co")
    (flyspell-mode . "/Fs")
    (projectile-mode . "")
    (tern-mode . "/Tn")
    (omnisharp-mode . "/Om")
    (abbrev-mode . "")
    (grammar-mode . "/Gm")
    (golden-ratio-mode . "")
    (rainbow-mode . "/Rm")

    ;; Mafor modes
    (emacs-lisp-mode . "El")
    (python-mode . "Py")
    (csharp-mode . "C#")
    (js2-mode . "Js2")
    (shell-mode . "Sh")
    (swift-mode . "Sw")
    (markdown-mode . "Md")
    (fundamental-mode . "Fn")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          (when (eq mode major-mode)
            (setq mode-name mode-str)))
        ))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)
