(require 'python)
(require 'epc)
(require 'jedi)
(require 'py-autopep8)

;; タブ幅の設定
(add-hook 'python-mode-hook '(lambda () 
                               (setq-default indent-tabs-mode nil)
                               (setq-default tab-width 4)
                               (define-key python-mode-map "\"" 'electric-pair)
                               (define-key python-mode-map "\'" 'electric-pair)
                               (define-key python-mode-map "(" 'electric-pair)
                               (define-key python-mode-map "[" 'electric-pair)
                               (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; jediの設定
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(add-hook 'before-save-hook 'py-autopep8-before-save)
