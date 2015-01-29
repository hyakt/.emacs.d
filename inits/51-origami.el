(require 'origami)
;; (makunbound 'origami-view-mode-map)
(define-minor-mode origami-view-mode
  "TABにorigamiの折畳みを割り当てる"
  nil "折紙"
  '(("\C-i" . origami-cycle))
  (or origami-mode (origami-mode 1)))
(defun origami-cycle (recursive)
  "origamiの機能をorg風にまとめる"
  (interactive "P")
  (call-interactively
   (if recursive 'origami-toggle-all-nodes 'origami-toggle-node)))
(defun view-mode-hook--origami ()
  (when (memq major-mode (mapcar 'car origami-parser-alist))
    (origami-view-mode (if view-mode 1 -1))))
(add-hook 'view-mode-hook 'view-mode-hook--origami)
