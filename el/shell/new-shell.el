;; Modified version of shell-send-input by rwells, 29-oct-86
;; Also contains new-shell, to create multiple shell buffers. rwells, 13-jan-87.
;; Fixed so it deletes input when it sends it.
;; The last command is saved in a variable.
;; It would be neat to have a ring of saved commands.

;; Run subshell under Emacs
;; Copyright (C) 1985 Richard M. Stallman.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;;; ----- new-shell -------------------------------------------------------

(defun new-shell (shellname)
  "Create inferior shell in buffer of given name.  
Otherwise just like shell command."
  (interactive "sShell name: ")
  (let* ((prog (or explicit-shell-file-name
		   (getenv "ESHELL")
		   (if (eq system-type 'hpux) "sh"
		     ;; On hpux people normally use csh,
		     ;; but the csh in hpux has stty sanity checking
		     ;; so it does not work under emacs.
		     (getenv "SHELL"))
		   "/bin/sh"))		     
	 (name (file-name-nondirectory prog)))
    (switch-to-buffer
     (make-shell shellname prog
		 (if (file-exists-p (concat "~/.emacs_" name))
		     (concat "~/.emacs_" name))
		 "-i"))))

;;; ----- stuff related to better shell-send-input -----------------------

(defvar last-input-save nil
  "In a shell-mode buffer, saved copy of last input.")

(defun shell-mode-hook ()
  "Fix to shell-mode for single echo of commands -rwells, 29-oct-86."
  (make-local-variable 'last-input-save)
  (setq last-input-save ""))


(defun shell-send-input ()
  "Send input to subshell.
At end of buffer, sends all text after last output
 as input to the subshell, including a newline inserted at the end.
Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of shell-prompt-pattern if possible.
This regexp should start with \"^\"."
  (interactive)
  (end-of-line)
    (if (eobp)
	(progn
	  (move-marker last-input-start
		       (process-mark (get-buffer-process (current-buffer))))
	  (insert ?\n)
	  (move-marker last-input-end (point)))
    (beginning-of-line)
    (re-search-forward shell-prompt-pattern nil t)
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker last-input-start (point))
      (insert copy)
      (move-marker last-input-end (point))))
    ;; Even if we get an error trying to hack the working directory,
    ;; still send the input to the subshell.
    (condition-case ()
	(save-excursion
	  (goto-char last-input-start)
	  (cond ((and (looking-at shell-popd-regexp)
		      (memq (char-after (match-end 0)) '(?\; ?\n)))
		 (if shell-directory-stack
		     (progn
		       (cd (car shell-directory-stack))
		       (setq shell-directory-stack (cdr shell-directory-stack)))))
		((looking-at shell-pushd-regexp)
		 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
			(if shell-directory-stack
			    (let ((old default-directory))
			      (cd (car shell-directory-stack))
			      (setq shell-directory-stack
				    (cons old (cdr shell-directory-stack))))))
		       ((memq (char-after (match-end 0)) '(?\  ?\t))
			(let (dir)
			  (skip-chars-forward "^ ")
			  (skip-chars-forward " \t")
			  (if (file-directory-p
				(setq dir
				      (expand-file-name
					(substitute-in-file-name
					 (buffer-substring
					  (point)
					  (progn
					    (skip-chars-forward "^\n \t;")
					    (point)))))))
			      (progn
				(setq shell-directory-stack
				      (cons default-directory shell-directory-stack))
				(cd dir)))))))
		((looking-at shell-cd-regexp)
		 (cond ((memq (char-after (match-end 0)) '(?\; ?\n))
			(cd (getenv "HOME")))
		       ((memq (char-after (match-end 0)) '(?\  ?\t))
			(let (dir)
			  (forward-char 3)
			  (skip-chars-forward " \t")
			  (if (file-directory-p
				(setq dir 
				      (expand-file-name
					(substitute-in-file-name
					 (buffer-substring
					  (point)
					  (progn
					    (skip-chars-forward "^\n \t;")
					    (point)))))))
			      (cd dir))))))))
      (error nil))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region process last-input-start last-input-end)
    (setq last-input-save (buffer-substring last-input-end last-input-start))
    (delete-region last-input-start last-input-end)
    (set-marker (process-mark process) (point))))

(defun copy-last-shell-input ()
  "Copy previous shell input, sans newline, and insert before point."
  (interactive)
  (insert last-input-save)
  (delete-char -1))
