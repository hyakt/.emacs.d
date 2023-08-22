;;; my-functions-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "my-git" "my-git.el" (0 0 0 0))
;;; Generated autoloads from my-git.el

(autoload 'my-projectile-run-shell-command-in-root "my-git" "\
Invoke `shell-command' COMMAND in the project's root.

\(fn COMMAND)" nil nil)

(autoload 'my-gh-repo-view "my-git" "\
Gh repo view." t nil)

(autoload 'my-gh-co "my-git" "\
Gh checkout TARGET.

\(fn TARGET)" t nil)

(autoload 'my-gh-pr-view "my-git" "\
Gh open pr." t nil)

(autoload 'my-gh-pr-create "my-git" "\
Gh pr create." t nil)

(autoload 'my-gh-pr-list "my-git" "\
Gh pull request list." t nil)

(autoload 'my-git-open-pr-from-commit-hash "my-git" "\
Git openpr HASH.

\(fn HASH)" t nil)

(autoload 'my-consult-git-commit-messages "my-git" nil t nil)

(autoload 'my-consult-git-conventional-commit-prefix "my-git" nil t nil)

(autoload 'my-git-wip "my-git" nil t nil)

;;;***

;;;### (autoloads nil "my-js" "my-js.el" (0 0 0 0))
;;; Generated autoloads from my-js.el

(autoload 'my-copy-project-name-clipboard "my-js" "\
Copy project name to clipbord." t nil)

(autoload 'my-reload-dir-locals-for-current-buffer "my-js" "\
reload dir locals for the current buffer" t nil)

(autoload 'my-run-in-vterm "my-js" "\
Execute string COMMAND in a new vterm.

\(fn COMMAND)" t nil)

(autoload 'my-mocha-exec-current-buffer "my-js" "\
Run mocha for current file." t nil)

(autoload 'my-mocha-copy-command-exec-current-buffer "my-js" "\
Run mocha for current file for paste." t nil)

(autoload 'my-mocha-watch-current-buffer "my-js" "\
Watch mocha for current file." t nil)

(autoload 'my-mocha-copy-command-watch-current-buffer "my-js" "\
Watch mocha for current file for paste." t nil)

(autoload 'my-mocha-exec-add-save-hook "my-js" "\
Add save hook exec mocha." t nil)

(autoload 'my-mocha-exec-remove-save-hook "my-js" "\
Remove save hook exec mocha." t nil)

(autoload 'my-jest-copy-command-current-buffer "my-js" "\
Watch jest for current file for paste." t nil)

(autoload 'my-jest-copy-command-watch-current-buffer "my-js" "\
Watch jest for current file for paste." t nil)

(autoload 'my-jest-current-buffer "my-js" "\
Watch mocha for current file." t nil)

(autoload 'my-jest-watch-current-buffer "my-js" "\
Watch mocha for current file." t nil)

(autoload 'my-tsc-error-find-file-buffer "my-js" "\
Show tsc error on buffer." t nil)

(autoload 'my-eslint-error-find-file-buffer "my-js" "\
Show eslint error on buffer." t nil)

(autoload 'my-eslint-warning-sorted-by-error-find-file-buffer "my-js" "\
Show sorted eslint warning on buffer." t nil)

(autoload 'my-eslint-spefic-error-find-file-buffer "my-js" "\
Show eslint ERROR-NAME error on buffer.

\(fn ERROR-NAME)" t nil)

(autoload 'my-eslint-fix-file "my-js" "\
Run eslint for current file." t nil)

(register-definition-prefixes "my-js" '("my-"))

;;;***

;;;### (autoloads nil "my-util" "my-util.el" (0 0 0 0))
;;; Generated autoloads from my-util.el

(autoload 'my-generate-autoloads "my-util" "\
Generates autoloads." t nil)

(autoload 'my-set-alpha "my-util" "\
Set frame parameter ALPHA-NUM.

\(fn ALPHA-NUM)" t nil)

(autoload 'my-buffer-face-set-variable-pitch-font "my-util" "\
Change the current buffer font to variable pitch font." t nil)

(autoload 'my-fullscreen "my-util" "\
Set frame maxmize." t nil)

(autoload 'my-halfscreen "my-util" "\
Set frame half." t nil)

(autoload 'my-halfscreen-right "my-util" "\
Set frame half." t nil)

(autoload 'my-url-decode-region "my-util" "\
BEGからENDの範囲の文字をURLデコードする.

\(fn BEG END)" t nil)

(autoload 'my-url-encode-region "my-util" "\
BEGからENDの範囲の文字をURLエンコードする.

\(fn BEG END)" t nil)

(autoload 'my-reverse-chars-region "my-util" "\
BEGからENDの範囲の文字反転する.

\(fn BEG END)" t nil)

(autoload 'my-uniq-lines "my-util" "\
Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort).

\(fn BEG END)" t nil)

(autoload 'my-reopen-with-sudo-tramp "my-util" "\
Reopen current buffer-file with sudo using tramp." t nil)

(autoload 'my-copy-buffer-name-clipboard "my-util" "\
Copy buffer name to clipbord." t nil)

(autoload 'my-move-or-rename-this-file "my-util" "\
Move or Rename current buffer file to NEWFILE.

\(fn NEWFILE)" t nil)

(autoload 'my-copy-this-file "my-util" "\
Copy current buffer file to new file." t nil)

(autoload 'my-delete-or-remove-this-file "my-util" "\
Delete current buffer file." t nil)

(autoload 'my-find-file-and-create-directory "my-util" "\
Find a file, and then create FILENAME (WILDCARDS)
the folder if it doesn't exist.

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'my-generate-slack-reminder "my-util" "\
Generate slack reminder with CONTENT and copy to clipboard.

\(fn CONTENT)" t nil)

(autoload 'my-open-hyper-current-buffer "my-util" "\
Open current buffer with Hyper term and fish." t nil)

(autoload 'my-open-scratch "my-util" "\
Open *scratch* buffer ." t nil)

(register-definition-prefixes "my-util" '("my-" "url-"))

;;;***


;;; Generated autoloads from my-git.el

(autoload 'my-projectile-run-shell-command-in-root "my-git" "\
Invoke `shell-command' COMMAND in the project's root.

(fn COMMAND)")
(autoload 'my-gh-repo-view "my-git" "\
Gh repo view." t)
(autoload 'my-gh-co "my-git" "\
Gh checkout TARGET.

(fn TARGET)" t)
(autoload 'my-gh-pr-view "my-git" "\
Gh open pr." t)
(autoload 'my-gh-pr-create "my-git" "\
Gh pr create." t)
(autoload 'my-gh-pr-list "my-git" "\
Gh pull request list." t)
(autoload 'my-git-open-pr-from-commit-hash "my-git" "\
Git openpr HASH.

(fn HASH)" t)
(autoload 'my-git-open-pr-from-current-line "my-git" "\
Git openpr current line." t)
(autoload 'my-consult-git-commit-messages "my-git" nil t)
(autoload 'my-consult-git-conventional-commit-prefix "my-git" nil t)
(autoload 'my-git-wip "my-git" nil t)


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-functions-autoloads.el ends here
