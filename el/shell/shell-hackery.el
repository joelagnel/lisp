(eval-after-load "sh-script"
 '(progn
(defun sh-mode ()
  "Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:

\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-while-getopts]	 while getopts loop
\\[sh-repeat]	 repeat loop
\\[sh-select]	 select loop
\\[sh-until]	 until loop
\\[sh-while]	 while loop

\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sh-mode-map)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'skeleton-end-hook)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'sh-header-marker)
  (make-local-variable 'sh-shell-file)
  (make-local-variable 'sh-shell)
  (make-local-variable 'skeleton-pair-alist)
  (make-local-variable 'skeleton-pair-filter)
  (make-local-variable 'comint-dynamic-complete-functions)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'skeleton-filter)
  (make-local-variable 'skeleton-newline-indent-rigidly)
  (make-local-variable 'sh-shell-variables)
  (make-local-variable 'sh-shell-variables-initialized)
  (make-local-variable 'imenu-generic-expression)
  (setq major-mode 'sh-mode
	mode-name "Shell-script"
	indent-line-function 'sh-indent-line
	;; not very clever, but enables wrapping skeletons around regions
	indent-region-function (lambda (b e)
				 (save-excursion
				   (goto-char b)
				   (skip-syntax-backward "-")
				   (setq b (point))
				   (goto-char e)
				   (skip-syntax-backward "-")
				   (indent-rigidly b (point) sh-indentation)))
	skeleton-end-hook (lambda ()
			    (or (eolp) (newline) (indent-relative)))
	paragraph-start (concat page-delimiter "\\|$")
	paragraph-separate paragraph-start
	comment-start "# "
	comint-dynamic-complete-functions sh-dynamic-complete-functions
	;; we can't look if previous line ended with `\'
	comint-prompt-regexp "^[ \t]*"
	font-lock-defaults
	'((sh-font-lock-keywords
	   sh-font-lock-keywords-1 sh-font-lock-keywords-2)
	  nil nil
	  ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
	  (font-lock-syntactic-keywords . sh-font-lock-syntactic-keywords))
	skeleton-pair-alist '((?` _ ?`))
	skeleton-pair-filter 'sh-quoted-p
	skeleton-further-elements '((< '(- (min sh-indentation
						(current-column)))))
	skeleton-filter 'sh-feature
	skeleton-newline-indent-rigidly t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Parse or insert magic number for exec, and set all variables depending
  ;; on the shell thus determined.
  (let ((interpreter
	 (save-excursion
	   (goto-char (point-min))
	   (cond ((looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)")
		  (match-string 2))
		 ((and buffer-file-name
		       (string-match "\\.m?spec$" buffer-file-name))
		  "rpm")))))
    (if interpreter
	(sh-set-shell interpreter nil nil)
      (progn
        ;; If we don't know the shell for this file, set the syntax
        ;; table anyway, for the user's normal choice of shell.
	(set-syntax-table (or (sh-feature sh-mode-syntax-table)
			      (standard-syntax-table)))
        ;; And avoid indent-new-comment-line (at least) losing.
        (setq comment-start-skip "#+[\t ]*")))
(run-hooks 'sh-mode-hook)
)
  )
(setq sh-shell-file "/usr/bin/csh")
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (unless interpreter (sh-set-shell sh-shell-file nil t))))
))

(eval-after-load "comint"
 '(progn
    (setq comint-completion-autolist t)
    (setq comint-completion-addsuffix t)
    (setq comint-input-ignoredups t)
    (setq comint-input-autoexpand 'input)
    ))

(eval-after-load "shell" '(define-key shell-mode-map " " 'comint-magic-space))

(provide 'shell-hackery)
