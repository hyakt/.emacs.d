;;; my-herdr-review.el --- Annotate diffs and send comments to a herdr agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Collect phantom-style inline comments on a diff-mode / magit diff buffer
;; (or any file-visiting buffer) and send them as structured text to a
;; running herdr agent (e.g. claude) through the herdr socket API.
;;
;; Toggle review mode.  While it is on, single keys drive the
;; comment commands:
;;   c/f add or edit / file-level, d delete, n/p jump, l list,
;;   t fold, C clear all, q quit, ? transient menu, C-c C-c send.
;;
;; Built for terminal Emacs (-nw): comments render via after-string overlays
;; and editing happens in a split-window buffer (no posframe / child frames).

;;; Code:
(eval-when-compile (package-initialize))

(require 'json)
(require 'subr-x)
(require 'seq)
(require 'transient)

(declare-function magit-current-file "magit")
(declare-function magit-toplevel "magit")
(declare-function diff-find-file-name "diff-mode")
(declare-function projectile-project-root "projectile")
(declare-function mac-ime-deactivate "ext:macfns")

(defgroup my-herdr-review nil
  "Send diff comments to a herdr agent."
  :group 'tools)

(defcustom my-herdr-review-herdr-executable "herdr"
  "Path to the herdr executable."
  :type 'string
  :group 'my-herdr-review)

(defcustom my-herdr-review-prompt-header
  "The following are code review comments. For each one, check the referenced location and address it."
  "Instruction prepended to the text sent to the agent."
  :type 'string
  :group 'my-herdr-review)

(defcustom my-herdr-review-confirm-before-send nil
  "When non-nil, ask for confirmation before sending."
  :type 'boolean
  :group 'my-herdr-review)

(defcustom my-herdr-review-submit-with-enter t
  "When non-nil, send Enter after the text so the agent runs immediately."
  :type 'boolean
  :group 'my-herdr-review)

(defcustom my-herdr-review-quit-after-send t
  "When non-nil, leave review mode after a successful send."
  :type 'boolean
  :group 'my-herdr-review)

(defcustom my-herdr-review-lgtm-text "LGTM 👍"
  "Overall note sent by `my-herdr-review-lgtm'."
  :type 'string
  :group 'my-herdr-review)

(defcustom my-herdr-review-marker "💬"
  "Marker shown in front of a comment.
Change it to e.g. \">>\" if the terminal renders emoji poorly."
  :type 'string
  :group 'my-herdr-review)

(defface my-herdr-review-annotation
  '((((class color) (min-colors 16777216)) :foreground "#ffd700")
    (((class color) (min-colors 256)) :foreground "color-220")
    (t :foreground "yellow"))
  "Face for the phantom comment text."
  :group 'my-herdr-review)

(defvar my-herdr-review--last-terminal nil
  "Terminal id of the last agent sent to; used as the next default.")

(defvar-local my-herdr-review--annotations nil
  "Comments in the current buffer.
Each element is a plist: :file(absolute) :line :end-line :type :text
:file-level :overlay.")

(defvar-local my-herdr-review--folded nil
  "Non-nil while comments are folded.")

(defvar-local my-herdr-review--saved-read-only nil
  "Saved `buffer-read-only' value, restored when review mode turns off.")

(defun my-herdr-review--deactivate-ime ()
  "Switch the input source back to ASCII so single keys work.
Use the Mac port in a GUI frame; in a terminal shell out to `macism'
if available.  Calling `mac-ime-deactivate' under -nw would spawn a
GUI Emacs, so it is restricted to graphic frames."
  (cond
   ((and (display-graphic-p) (fboundp 'mac-ime-deactivate))
    (mac-ime-deactivate))
   ((executable-find "macism")
    (call-process "macism" nil 0 nil "com.apple.keylayout.ABC"))))

;;;; Project and diff position

(defun my-herdr-review--git-root ()
  "Return the git repository root."
  (or (and (fboundp 'magit-toplevel) (ignore-errors (magit-toplevel)))
      (locate-dominating-file default-directory ".git")
      (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))))

(defun my-herdr-review--diff-buffer-p ()
  "Return non-nil when the current buffer shows a diff."
  (derived-mode-p 'diff-mode 'magit-mode))

(defun my-herdr-review--file-at-point ()
  "Return the absolute path of the file at point."
  (cond
   ((and (derived-mode-p 'magit-mode) (fboundp 'magit-current-file))
    (let ((file (magit-current-file))
          (root (and (fboundp 'magit-toplevel) (ignore-errors (magit-toplevel)))))
      (when file (if root (expand-file-name file root) file))))
   ((derived-mode-p 'diff-mode)
    (ignore-errors (diff-find-file-name)))
   (buffer-file-name buffer-file-name)))

(defun my-herdr-review--line-at (pos)
  "Return diff line info for the line containing POS.
Value is (:old OLD :new NEW :type CHAR) or nil."
  (save-excursion
    (goto-char pos)
    (let ((target (line-beginning-position)))
      (when (re-search-backward
             "^@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
             nil t)
        (let ((old (string-to-number (match-string 1)))
              (new (string-to-number (match-string 2))))
          (forward-line 1)
          (while (< (point) target)
            (pcase (char-after (line-beginning-position))
              (?+ (setq new (1+ new)))
              (?- (setq old (1+ old)))
              (_  (setq old (1+ old) new (1+ new))))
            (forward-line 1))
          (list :old old :new new :type (char-after target)))))))

(defun my-herdr-review--location-at-point ()
  "Return location info for point or the active region.
Uses hunk headers in a diff buffer, or buffer line numbers otherwise.
Value is (:file F :line L :end-line E :type CHAR) or nil."
  (let* ((beg (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (1- (region-end)) (point)))
         (file (my-herdr-review--file-at-point)))
    (when file
      (if (my-herdr-review--diff-buffer-p)
          (let ((start (my-herdr-review--line-at beg)))
            (when start
              (let* ((type (plist-get start :type))
                     (line (if (eq type ?-) (plist-get start :old) (plist-get start :new)))
                     (end-info (and (use-region-p) (my-herdr-review--line-at end)))
                     (end-line (and end-info
                                    (let ((et (plist-get end-info :type)))
                                      (if (eq et ?-) (plist-get end-info :old)
                                        (plist-get end-info :new))))))
                (list :file file :line line
                      :end-line (and end-line (/= end-line line) end-line)
                      :type type))))
        (let ((line (line-number-at-pos beg))
              (eline (line-number-at-pos end)))
          (list :file file :line line
                :end-line (and (/= eline line) eline)
                :type nil))))))

;;;; Overlays (phantom display)

(defun my-herdr-review--render-string (annotation)
  "Build the after-string for ANNOTATION.
File-level comments render at the top with their file name; line
comments render just below their line."
  (let ((lines (split-string (plist-get annotation :text) "\n")))
    (if (plist-get annotation :file-level)
        (let* ((name (file-name-nondirectory (plist-get annotation :file)))
               (head (format "%s[file: %s] %s" my-herdr-review-marker name (car lines)))
               (rest (mapcar (lambda (l) (concat "  " l)) (cdr lines))))
          (propertize (concat (string-join (cons head rest) "\n") "\n")
                      'face 'my-herdr-review-annotation))
      (let* ((end (plist-get annotation :end-line))
             (range (if end (format "[L%d-%d] " (plist-get annotation :line) end) ""))
             (first (concat "  " my-herdr-review-marker " " range (car lines)))
             (rest (mapcar (lambda (l) (concat "     " l)) (cdr lines))))
        (propertize (concat "\n" (string-join (cons first rest) "\n"))
                    'face 'my-herdr-review-annotation)))))

(defun my-herdr-review--render-overlay (annotation)
  "Refresh ANNOTATION's overlay for the current fold state."
  (let ((ov (plist-get annotation :overlay)))
    (when (overlayp ov)
      (overlay-put
       ov 'after-string
       (cond
        ((not my-herdr-review--folded) (my-herdr-review--render-string annotation))
        ((plist-get annotation :file-level)
         (propertize (format "%s[file: %s]\n" my-herdr-review-marker
                             (file-name-nondirectory (plist-get annotation :file)))
                     'face 'my-herdr-review-annotation))
        (t (propertize (concat "  " my-herdr-review-marker)
                       'face 'my-herdr-review-annotation)))))))

(defun my-herdr-review--overlay-at-point ()
  "Return the comment overlay on the current line, if any."
  (seq-find (lambda (ov) (overlay-get ov 'my-herdr-review))
            (overlays-in (line-beginning-position) (1+ (line-end-position)))))

(defun my-herdr-review--annotation-by-overlay (ov)
  "Return the annotation plist whose overlay is OV."
  (seq-find (lambda (a) (eq (plist-get a :overlay) ov))
            my-herdr-review--annotations))

(defun my-herdr-review--sorted-overlays ()
  "Return comment overlays ordered by buffer position."
  (sort (delq nil (mapcar (lambda (a) (plist-get a :overlay))
                          my-herdr-review--annotations))
        (lambda (x y) (< (overlay-start x) (overlay-start y)))))

(defun my-herdr-review--clear-overlays ()
  "Delete all comment overlays."
  (dolist (a my-herdr-review--annotations)
    (when-let ((ov (plist-get a :overlay)))
      (when (overlayp ov) (delete-overlay ov)))))

(defun my-herdr-review--remove (annotation)
  "Remove ANNOTATION."
  (when-let ((ov (plist-get annotation :overlay)))
    (when (overlayp ov) (delete-overlay ov)))
  (setq my-herdr-review--annotations
        (delq annotation my-herdr-review--annotations)))

;;;; Edit buffer (multi-line input)

(defvar my-herdr-review--edit-context nil
  "Plist for the in-progress edit: :review-buffer :marker :loc :annotation
:file :mode.")

(defconst my-herdr-review--edit-buffer-name "*herdr review comment*")

(defvar my-herdr-review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my-herdr-review-edit-commit)
    (define-key map (kbd "C-c C-k") #'my-herdr-review-edit-cancel)
    map)
  "Keymap for the comment edit buffer.")

(define-minor-mode my-herdr-review-edit-mode
  "Minor mode for the comment edit buffer."
  :lighter " ✎Rev"
  :keymap my-herdr-review-edit-mode-map)

(defun my-herdr-review--open-edit (context &optional initial)
  "Open the edit buffer with CONTEXT, optionally seeded with INITIAL."
  (let ((buf (get-buffer-create my-herdr-review--edit-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (when initial (insert initial))
      (setq-local my-herdr-review--edit-context context)
      (my-herdr-review-edit-mode 1))
    (pop-to-buffer buf)
    (message "C-c C-c to add / C-c C-k to cancel")))

(defun my-herdr-review-edit-commit ()
  "Commit the comment being edited."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (ctx my-herdr-review--edit-context)
        (edit-buf (current-buffer)))
    (when (string-empty-p text)
      (user-error "Empty comment; nothing added"))
    (let ((review-buf (plist-get ctx :review-buffer)))
      (unless (buffer-live-p review-buf)
        (user-error "Review buffer is gone"))
      (with-current-buffer review-buf
        (pcase (plist-get ctx :mode)
          ('add (my-herdr-review--add (plist-get ctx :loc)
                                      (plist-get ctx :marker) text))
          ('add-file (my-herdr-review--add-file (plist-get ctx :file) text))
          ('edit (my-herdr-review--update (plist-get ctx :annotation) text)))
        (deactivate-mark))
      (quit-window t (get-buffer-window edit-buf))
      (kill-buffer edit-buf)
      (when (buffer-live-p review-buf)
        (pop-to-buffer review-buf))
      (my-herdr-review--deactivate-ime))))

(defun my-herdr-review-edit-cancel ()
  "Cancel the comment being edited."
  (interactive)
  (let ((buf (current-buffer)))
    (quit-window t (get-buffer-window buf))
    (kill-buffer buf)
    (my-herdr-review--deactivate-ime)
    (message "Comment canceled")))

(defun my-herdr-review--add (loc marker text)
  "Add a comment with TEXT at MARKER for LOC."
  (save-excursion
    (goto-char marker)
    (let* ((eol (line-end-position))
           (ov (make-overlay eol eol))
           (annotation (list :file (plist-get loc :file)
                             :line (plist-get loc :line)
                             :end-line (plist-get loc :end-line)
                             :type (plist-get loc :type)
                             :text text :file-level nil :overlay ov)))
      (overlay-put ov 'my-herdr-review t)
      (push annotation my-herdr-review--annotations)
      (my-herdr-review--render-overlay annotation)))
  (message "Comment added"))

(defun my-herdr-review--add-file (file text)
  "Add a file-level comment TEXT for FILE at the top of the buffer."
  (let* ((ov (make-overlay (point-min) (point-min)))
         (annotation (list :file file :line nil :end-line nil :type nil
                           :text text :file-level t :overlay ov)))
    (overlay-put ov 'my-herdr-review t)
    (push annotation my-herdr-review--annotations)
    (my-herdr-review--render-overlay annotation))
  (message "File comment added"))

(defun my-herdr-review--update (annotation text)
  "Set ANNOTATION's body to TEXT."
  (plist-put annotation :text text)
  (my-herdr-review--render-overlay annotation)
  (message "Comment updated"))

;;;; Commands

(defun my-herdr-review-annotate ()
  "Comment on the current line or region; edit an existing one."
  (interactive)
  (let ((existing (my-herdr-review--overlay-at-point)))
    (if existing
        (let ((annotation (my-herdr-review--annotation-by-overlay existing)))
          (my-herdr-review--open-edit
           (list :review-buffer (current-buffer) :mode 'edit :annotation annotation)
           (plist-get annotation :text)))
      (let ((loc (my-herdr-review--location-at-point)))
        (unless loc
          (user-error "Place point on a line inside a diff hunk"))
        (my-herdr-review--open-edit
         (list :review-buffer (current-buffer) :mode 'add :loc loc
               :marker (copy-marker (line-beginning-position))))))))

(defun my-herdr-review-annotate-file ()
  "Add a file-level comment for the file at point."
  (interactive)
  (let ((file (my-herdr-review--file-at-point)))
    (unless file
      (user-error "Could not determine the file"))
    (my-herdr-review--open-edit
     (list :review-buffer (current-buffer) :mode 'add-file :file file))
    (message "Comment on file %s. C-c C-c to add"
             (file-name-nondirectory file))))

(defun my-herdr-review-delete ()
  "Delete the comment on the current line."
  (interactive)
  (let ((ov (my-herdr-review--overlay-at-point)))
    (unless ov (user-error "No comment on this line"))
    (my-herdr-review--remove (my-herdr-review--annotation-by-overlay ov))
    (message "Comment deleted")))

(defun my-herdr-review-next ()
  "Jump to the next comment."
  (interactive)
  (let* ((ovs (my-herdr-review--sorted-overlays))
         (next (seq-find (lambda (ov) (> (overlay-start ov) (point))) ovs)))
    (cond (next (goto-char (overlay-start next)))
          (ovs (goto-char (overlay-start (car ovs)))
               (message "Wrapped to first comment"))
          (t (user-error "No comments")))))

(defun my-herdr-review-previous ()
  "Jump to the previous comment."
  (interactive)
  (let* ((ovs (reverse (my-herdr-review--sorted-overlays)))
         (prev (seq-find (lambda (ov) (< (overlay-start ov) (point))) ovs)))
    (cond (prev (goto-char (overlay-start prev)))
          (ovs (goto-char (overlay-start (car ovs)))
               (message "Wrapped to last comment"))
          (t (user-error "No comments")))))

(defun my-herdr-review-list ()
  "Pick a comment and jump to it."
  (interactive)
  (unless my-herdr-review--annotations
    (user-error "No comments"))
  (let* ((candidates
          (mapcar (lambda (a)
                    (cons (format "%s%s  %s"
                                  (file-name-nondirectory (plist-get a :file))
                                  (if (plist-get a :file-level) ""
                                    (format ":%d" (plist-get a :line)))
                                  (car (split-string (plist-get a :text) "\n")))
                          a))
                  my-herdr-review--annotations))
         (choice (completing-read "Comment: " candidates nil t))
         (annotation (cdr (assoc choice candidates)))
         (ov (plist-get annotation :overlay)))
    (when (overlayp ov)
      (goto-char (overlay-start ov)))))

(defun my-herdr-review-toggle-fold ()
  "Toggle folding of all comments."
  (interactive)
  (setq my-herdr-review--folded (not my-herdr-review--folded))
  (dolist (a my-herdr-review--annotations)
    (my-herdr-review--render-overlay a))
  (message (if my-herdr-review--folded "Comments folded" "Comments expanded")))

(defun my-herdr-review-clear ()
  "Delete all comments."
  (interactive)
  (when (and my-herdr-review--annotations
             (y-or-n-p (format "Discard all %d comments? "
                               (length my-herdr-review--annotations))))
    (my-herdr-review--clear-overlays)
    (setq my-herdr-review--annotations nil)
    (message "All comments cleared")))

(defun my-herdr-review-quit ()
  "Turn review mode off."
  (interactive)
  (my-herdr-review-mode -1))

(transient-define-prefix my-herdr-review-menu ()
  "herdr review commands."
  [["Comment"
    ("c" "add / edit" my-herdr-review-annotate)
    ("f" "file-level" my-herdr-review-annotate-file)
    ("d" "delete" my-herdr-review-delete)
    ("C" "clear all" my-herdr-review-clear)]
   ["Move / view"
    ("n" "next" my-herdr-review-next :transient t)
    ("p" "previous" my-herdr-review-previous :transient t)
    ("l" "list" my-herdr-review-list)
    ("t" "fold" my-herdr-review-toggle-fold :transient t)]
   ["Send / quit"
    ("C-c C-c" "send" my-herdr-review-send)
    ("C-c C-l" "send LGTM" my-herdr-review-lgtm)
    ("q" "quit review" my-herdr-review-quit)]])

;;;; Formatting the outgoing text

(defun my-herdr-review--relativize (file agent-cwd)
  "Return FILE relative to AGENT-CWD."
  (if (and agent-cwd (file-name-absolute-p file))
      (file-relative-name file (file-name-as-directory agent-cwd))
    file))

(defun my-herdr-review--type-suffix (type)
  "Return the marker for diff TYPE."
  (pcase type
    (?+ " (+)")
    (?- " (-)")
    (_  "")))

(defun my-herdr-review--comment-blocks (agent-cwd)
  "Render the comment blocks as structured text relative to AGENT-CWD."
  (let ((sorted
         (sort (copy-sequence my-herdr-review--annotations)
               (lambda (a b)
                 (let ((fa (plist-get a :file)) (fb (plist-get b :file)))
                   (if (string= fa fb)
                       (< (or (plist-get a :line) 0) (or (plist-get b :line) 0))
                     (string< fa fb)))))))
    (mapconcat
     (lambda (a)
       (let* ((file (my-herdr-review--relativize (plist-get a :file) agent-cwd))
              (line (plist-get a :line))
              (end (plist-get a :end-line))
              (head (cond
                     ((plist-get a :file-level) (format "## %s (file-level)" file))
                     (end (format "## %s:%d-%d%s" file line end
                                  (my-herdr-review--type-suffix (plist-get a :type))))
                     (t (format "## %s:%d%s" file line
                                (my-herdr-review--type-suffix (plist-get a :type)))))))
         (concat head "\n" (string-trim (plist-get a :text)))))
     sorted "\n\n")))

(defun my-herdr-review--compose (agent-cwd overall)
  "Build the outgoing text relative to AGENT-CWD, with optional OVERALL note."
  (let ((blocks (and my-herdr-review--annotations
                     (my-herdr-review--comment-blocks agent-cwd))))
    (cond
     ((and blocks overall)
      (concat my-herdr-review-prompt-header "\n\n"
              "## (overall)\n" overall "\n\n" blocks))
     (blocks (concat my-herdr-review-prompt-header "\n\n" blocks))
     (t overall))))

;;;; herdr integration

(defun my-herdr-review--herdr (&rest args)
  "Run herdr with ARGS and return stdout, signalling on failure."
  (with-temp-buffer
    (let ((status (apply #'process-file my-herdr-review-herdr-executable
                         nil t nil args)))
      (unless (zerop status)
        (error "herdr %s failed: %s" (car args) (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun my-herdr-review--list-agents ()
  "Return the herdr agents as a list of alists."
  (let* ((out (my-herdr-review--herdr "agent" "list"))
         (json (json-parse-string out :object-type 'alist :array-type 'list)))
    (alist-get 'agents (alist-get 'result json))))

(defun my-herdr-review--status-rank (agent)
  "Rank AGENT so that waiting agents sort first."
  (pcase (alist-get 'agent_status agent)
    ("blocked" 0) ("idle" 1) ("working" 2) (_ 3)))

(defun my-herdr-review--select-agent ()
  "Pick the target agent."
  (let* ((agents (my-herdr-review--list-agents))
         (root (my-herdr-review--git-root))
         (matched
          (and root
               (seq-filter
                (lambda (a)
                  (let ((cwd (alist-get 'cwd a)))
                    (and cwd
                         (let ((r (file-name-as-directory (expand-file-name root)))
                               (c (file-name-as-directory (expand-file-name cwd))))
                           (string-prefix-p r c)))))
                agents)))
         (candidates (sort (or matched agents)
                           (lambda (a b) (< (my-herdr-review--status-rank a)
                                            (my-herdr-review--status-rank b))))))
    (cond
     ((null candidates) (user-error "No running herdr agent found"))
     ((= (length candidates) 1) (car candidates))
     (t (let* ((alist
                (mapcar (lambda (a)
                          (cons (format "%s  %s  %s  [%s]"
                                        (alist-get 'agent a)
                                        (alist-get 'agent_status a)
                                        (alist-get 'cwd a)
                                        (alist-get 'terminal_id a))
                                a))
                        candidates))
               (default (seq-find (lambda (c)
                                    (equal (alist-get 'terminal_id (cdr c))
                                           my-herdr-review--last-terminal))
                                  alist))
               (choice (completing-read "Target agent: " alist nil t
                                        nil nil (car default))))
          (cdr (assoc choice alist)))))))

(defun my-herdr-review--send (overall)
  "Send the collected comments, plus an optional OVERALL note, to an agent."
  (when (and (null my-herdr-review--annotations) (not overall))
    (user-error "No comments to send"))
  (let* ((agent (my-herdr-review--select-agent))
         (terminal (alist-get 'terminal_id agent))
         (pane (alist-get 'pane_id agent))
         (cwd (alist-get 'cwd agent))
         (count (length my-herdr-review--annotations))
         (text (my-herdr-review--compose cwd overall)))
    (when (and my-herdr-review-confirm-before-send
               (not (y-or-n-p
                     (format "Send to %s (%s)? " (alist-get 'agent agent) cwd))))
      (user-error "Send canceled"))
    (my-herdr-review--herdr "agent" "send" terminal text)
    (when my-herdr-review-submit-with-enter
      (my-herdr-review--herdr "pane" "send-keys" pane "Enter"))
    (setq my-herdr-review--last-terminal terminal)
    (kill-new text)
    (if my-herdr-review-quit-after-send
        (my-herdr-review-mode -1)
      (my-herdr-review--clear-overlays)
      (setq my-herdr-review--annotations nil))
    (message (if (> count 0)
                 (format "Sent %d comments to %s (text on kill-ring)" count terminal)
               (format "Sent overall note to %s (text on kill-ring)" terminal)))))

(defun my-herdr-review-send ()
  "Send the collected comments; if there are none, prompt for an overall note."
  (interactive)
  (if my-herdr-review--annotations
      (my-herdr-review--send nil)
    (let ((note (string-trim (read-string "Overall note (no comments): "))))
      (when (string-empty-p note)
        (user-error "Nothing to send"))
      (my-herdr-review--send note))))

(defun my-herdr-review-lgtm ()
  "Send an overall LGTM note, along with any collected comments."
  (interactive)
  (my-herdr-review--send my-herdr-review-lgtm-text))

;;;; Minor mode

(defvar my-herdr-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'my-herdr-review-annotate)
    (define-key map "f" #'my-herdr-review-annotate-file)
    (define-key map "d" #'my-herdr-review-delete)
    (define-key map "n" #'my-herdr-review-next)
    (define-key map "p" #'my-herdr-review-previous)
    (define-key map "l" #'my-herdr-review-list)
    (define-key map "t" #'my-herdr-review-toggle-fold)
    (define-key map "C" #'my-herdr-review-clear)
    (define-key map "q" #'my-herdr-review-quit)
    (define-key map "?" #'my-herdr-review-menu)
    (define-key map (kbd "C-c C-c") #'my-herdr-review-send)
    (define-key map (kbd "C-c C-l") #'my-herdr-review-lgtm)
    map)
  "Single-key map active while review mode is on.")

;;;###autoload
(define-minor-mode my-herdr-review-mode
  "Modal minor mode to annotate a diff and send comments to a herdr agent."
  :lighter " 💬Rev"
  :keymap my-herdr-review-mode-map
  :group 'my-herdr-review
  (if my-herdr-review-mode
      (if (or (my-herdr-review--diff-buffer-p) buffer-file-name)
          (progn
            (my-herdr-review--deactivate-ime)
            (setq my-herdr-review--saved-read-only buffer-read-only
                  buffer-read-only t
                  my-herdr-review--annotations nil
                  my-herdr-review--folded nil))
        (setq my-herdr-review-mode nil)
        (user-error "Enable in a file-visiting or diff buffer"))
    (my-herdr-review--clear-overlays)
    (setq buffer-read-only my-herdr-review--saved-read-only
          my-herdr-review--annotations nil
          my-herdr-review--folded nil)))

;;;; doom-modeline integration

;; `eval' defers the doom-modeline macro past byte-compile; remove-before-add avoids dupes on reload.
(with-eval-after-load 'doom-modeline
  (eval
   '(progn
      (doom-modeline-def-segment my-herdr-review
        (when (bound-and-true-p my-herdr-review-mode)
          (propertize "💬 REVIEW "
                      'face '(:background "#e0af68" :foreground "#1a1a1a"
                                          :weight bold))))
      (doom-modeline-remove-segment 'my-herdr-review 'all)
      (doom-modeline-add-segment 'my-herdr-review 'buffer-info :before))
   t))

(provide 'my-herdr-review)
;;; my-herdr-review.el ends here
