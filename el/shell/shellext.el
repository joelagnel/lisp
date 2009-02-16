;From arpa-unix-emacs-request@ALEXANDER.BBN.COM Tue Jan  5 17:52:24 1988
;Received: from alexander by ALEXANDER.BBN.COM id aa02677; 5 Jan 88 16:30 EST
;Received: from [128.89.0.122] by ALEXANDER.BBN.COM id aa02665;
;          5 Jan 88 16:29 EST
;Received: from ucbvax.berkeley.edu by BBN.COM id aa03966; 5 Jan 88 16:26 EST
;Received: by ucbvax.Berkeley.EDU (5.58/1.26)
;	id AA29705; Tue, 5 Jan 88 12:54:48 PST
;Received: from USENET by ucbvax.Berkeley.EDU with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com)
;	(contact usenet@ucbvax.Berkeley.EDU if you have questions)
;Date: 5 Jan 88 19:43:33 GMT
;From: Wolfgang Rupprecht <wolfgang@MGM.MIT.EDU>
;Organization: Independent Software Consultant
;Subject: ksh for GnuEmacs
;Message-Id: <2140@bloom-beacon.MIT.EDU>
;Sender: unix-emacs-request@BBN.COM
;To: unix-emacs@BBN.COM
;
;This is a ksh-like extention to shell.el.  These extentiions implement
;command history (backwards, forwards, back-search, forward-srearch),
;filename completion, and history printout for an emacs shell window.
;The one glaring difference between this and ksh, is that all of the
;shell-mode commands are bound to the Control-C prefix map. (Eg.
;previous command is C-c C-p).
;
;The full list of shell commands is:
;
;	C-c C-a         shell-beginning-of-line
;	C-c C-c         interrupt-shell-subjob
;	C-c C-d         shell-send-eof
;	C-c C-h         Prefix Command
;	C-c TAB         shell-filename-expand
;	C-c RET         shell-push-input
;	C-c C-n         shell-next-command
;	C-c C-o         kill-output-from-shell
;	C-c C-p         shell-previous-command
;	C-c C-r         shell-history-search-backward
;	C-c C-s         shell-history-search-forward
;	C-c C-u         kill-shell-input
;	C-c C-w         backward-kill-word
;	C-c C-x         Prefix Command
;	C-c C-y         copy-last-shell-input
;	C-c C-z         stop-shell-subjob
;	C-c ESC         Prefix Command
;	C-c C-\         quit-shell-subjob
;	C-c [           show-output-from-shell
;	C-c h           shell-list-history
;
;This is a diff for shell.el(18.49) (to add a few hooks, and some
;personal bug-fixes/hacks) and a file shellext.el, which does the real
;work.
;
;This basic code has been running on 18.26 as well as 18.49, so you
;shouldn't have much trouble, even on older emacses.
;
;			-enjoy
;			wolfgang
;
;----cut here---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     shellext.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;      Address:  wolfgang@mgm.mit.edu (IP addr 18.82.0.114)                 ;;
;;	Created:  Mon Oct 12 18:52:34 EDT 1987				     ;;
;;	Contents: ksh-like extensions to shell.el		             ;;
;;									     ;;
;;	Copyright (c) 1987 Wolfgang Rupprecht.				     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs and this file "shellext.el", is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY.  No author or
;; distributor accepts responsibility to anyone for the consequences
;; of using it or for whether it serves any particular purpose or
;; works at all, unless he says so in writing.  Refer to the GNU Emacs
;; General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs and shellext.el, but only under the conditions described in
;; the GNU Emacs General Public License.  A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you can
;; know your rights and responsibilities.  It should be in a file
;; named COPYING.  Among other things, the copyright notice and this
;; notice must be preserved on all copies.

;; If you like this shell hack, and would like other custom
;; non-proprietary GnuEmacs extensions, let me know. I may be
;; interested in doing it for you on a contract basis. -wsr

(provide 'shellext)			; for require 'shellext in 
					; patched shell.el

(defvar shell-last-search "" "Last shell search string.")
(defvar shell-max-history 60
  "*Max shell history retained")
(defvar shell-history-list nil
  "History list of past shell commands.")
(defvar shell-history-index -1
  "Where we are on the history list. It is -1 unless currently
walking up/down the list")
(defvar shell-history-list-order nil
  "*If t, list shell history with most recent command last." nil)
(defvar shell-read-history t
  "*If t, the emacs shell will read in the user's ~/.history file.
This somewhat slows down shell startup.")

(define-key shell-mode-map "\C-c\t" 'shell-filename-expand)
(define-key shell-mode-map "\C-c\C-p" 'shell-previous-command)
(define-key shell-mode-map "\C-c\C-n" 'shell-next-command)
(define-key shell-mode-map "\C-c\C-r" 'shell-history-search-backward)
(define-key shell-mode-map "\C-c\C-s" 'shell-history-search-forward)
(define-key shell-mode-map "\C-ch" 'shell-list-history)
(define-key shell-mode-map "\C-c\C-a" 'shell-beginning-of-line)
(define-key shell-mode-map "\C-c\r" 'shell-push-input)
; and now rebind 'show-output-from shell (which was on C-cC-r)
(define-key shell-mode-map "\C-c[" 'show-output-from-shell)

(defun shell-filename-expand ()
  "Complete the filename before (point) as far as possible."
	(interactive)	       
	  (let* ((end (point))
		 (beg (progn
			(re-search-backward "[ \t\n;]")
			(forward-char 1)
			(point)))
		 (new (path-name-completion (buffer-substring beg end))))
	    (cond ((eq new t)
		   (progn
		     (goto-char end)
		     (message "File name is already complete")))
		  ((null new)
		   (progn
		     (goto-char end)
		     (error "No completion possible")))
		   (t
		    (progn
		      (delete-region beg end)
		      (insert new))))))

(defun path-name-completion (path-name)
  "Complete PATHNAME as far as possible, return this string."
  (let* ((dir (file-name-directory path-name))
	 (file (file-name-nondirectory path-name))
	 (completion
	   (file-name-completion file (or dir ""))))
    (if (string-equal file completion)	; we are at a branch point
	(let ((list (sort (file-name-all-completions (or file "") (or dir ""))
			  'string-lessp)))
	  (with-output-to-temp-buffer " *Completions*"
		    (princ "Possible completions are:\n")
		    (while list
		      ;; -40 padding spec doesn't work !!!
		      ;; (princ (format "%-40s %s" (car list)
		      ;;                (or (car (cdr list)) "")))
		      (princ (car list))
		      (if (cdr list)
			  (progn
			    (princ
			      (make-string
				(- 35 (length (car list)))
				?\ ))
			    (princ (car (cdr list)))))
		      (terpri)
		      (setq list (cdr (cdr list)))))
		  path-name)		; give 'em back their input
      ;; BUG?: emacs outputs to wrong-buffer (inserted text destined for
      ;; the *shell* buffer goes to next buffer on buffer list.)
      ;; Seemingly unneeded save-excursion around kill-buffer solves
      ;; this problem. -wsr
      (save-excursion
	(let ((buffer (get-buffer " *Completions*")))
	  (if buffer (kill-buffer buffer))))
      (if dir
	  (and completion (or (eq completion t)	; leave t or nil alone
			      (concat dir completion)))
	completion))))

(defun shell-previous-command ()
  "Insert the previous command into the shell buffer."
  (interactive)
  (let ((history (nthcdr (1+ shell-history-index) shell-history-list)))
    (if history
	(progn
	  (delete-region (process-mark (get-buffer-process (current-buffer)))
			 (point-max))
	  (goto-char (point-max))
	  (insert (car history))
	  (setq shell-history-index (1+ shell-history-index)))
;	  (message "history %d" shell-history-index)
      (error "End of history list, (history %d)" shell-history-index))))

(defun shell-next-command ()
  "Insert the next command into the shell buffer."
  (interactive)
  (delete-region (process-mark (get-buffer-process (current-buffer)))
	       (point-max))
  (goto-char (point-max))
  (if (<= 0 shell-history-index)  
      (setq shell-history-index (1- shell-history-index))
    (error "Top of history list (history %d)" shell-history-index))
  (if (<= 0 shell-history-index)
      (progn 
;	(message "history %d" shell-history-index)
	(insert (nth shell-history-index shell-history-list)))))

(defun shell-history-search-backward (string)
  "Search the history list for an occurance of STRING."
  (interactive (list (setq shell-last-search
			   (read-string
			    "History search for: " shell-last-search))))
  (let* ((index (1+ shell-history-index)) ; start at next command
	 (history (nthcdr index shell-history-list)))
    (while (and history
		(null (string-match string (car history))))
      (setq index (1+ index)
	    history (cdr history)))
    (if history
	(progn
	  (setq shell-history-index index)
	  (delete-region (process-mark (get-buffer-process (current-buffer)))
			 (point-max))
	  (goto-char (point-max))
	  (insert (car history))
;	  (message "history %d" shell-history-index)
	  )
      (error "No match found, history %d" shell-history-index))))


(defun shell-history-search-forward (string)
  "Search the history list for an occurance of STRING."
  (interactive (list (setq shell-last-search
			   (read-string
			    "History search for: " shell-last-search))))
  (let ((index shell-history-index))
    (while (and (<= 0 index)		; not as effecient as backwards hum...
		(null (string-match
		       string (nth (setq index (1- index))
				   shell-history-list)))))
    ;; index is bounded by: (-1 <= index <= shell-history-index) 
    (if (<= 0 index)
	(progn
	  (setq shell-history-index index)
	  (delete-region (process-mark (get-buffer-process (current-buffer)))
			 (point-max))
	  (goto-char (point-max))
	  (insert (or (nth index shell-history-list) ""))
;	  (message "history %d" shell-history-index)
	  )
      (error "No match found, history %d" shell-history-index))))

(defun shell-list-history ()
  "List the history in the *History* buffer. A '*' indicates current
position on the history list."
  (interactive)
  (with-output-to-temp-buffer "*History*"
    (if shell-history-list-order
	(let ((history (reverse (cons "<none>" shell-history-list)))
	      (index (1- (length shell-history-list))))
	  (while history
	    (princ (format "%c [%d] %s\n" 
			   (if (= index shell-history-index)
			       ?* ?\ )
			   index (car history)))
	    (setq history (cdr history)
		  index (1- index))))
      (let ((history (cons "<none>" shell-history-list))
	    (index -1))
	(while history
	  (princ (format "%c [%d] %s\n" 
			 (if (= index shell-history-index)
			     ?* ?\ )
			 index (car history)))
	  (setq history (cdr history)
		index (1+ index)))))))

(defun shell-beginning-of-line ()
  "Goto the beggining of the commad line. (ie. just after the prompt)"
  (interactive)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun shell-input-history-file ()
  "Read in the user's ~/.history file."
  (if shell-history-list		; if non-nil, its only a shell restart,
      nil				; so don't trash existing history list.
    (let (list)
      (save-excursion
	(set-buffer (get-buffer-create "*history-temp*"))
	(erase-buffer)
	(insert-file (expand-file-name "~/.history"))
	(goto-char (point-min))
	(while (re-search-forward "[^\^@\n]+$" nil t)
	  (setq list
		(cons (buffer-substring (match-beginning 0)
					(match-end 0))
		      list)))
	(kill-buffer (current-buffer)))
      (let ((prune-pt (nthcdr shell-max-history list)))
	(and prune-pt (rplacd prune-pt nil)))
      (make-local-variable 'shell-history-index)
      (make-local-variable 'shell-history-list)
      (setq shell-history-list list)
      (setq shell-history-index -1))))

(defun shell-output-history-file ()
  "Write out the user's ~/.history file from the internal history list."
  (save-excursion
    (let ((list shell-history-list))	; hold onto that slippery local var
      (set-buffer (get-buffer-create "*history-temp*"))
      (erase-buffer)
      (while list
	(insert (car list) "\n\^@")
	(setq list (cdr list))))
    (write-region (point-min) (point-max)
		  (expand-file-name "~/.history") nil 'nomsg)))

(defun shell-save-history ()
  "Save this command on the shell-history-list."
  (let ((command (buffer-substring last-input-start (1- last-input-end))))
    (if (or (string-match "^[ \t]*$" command)
	    (string-equal command (car shell-history-list)))
	nil				; don't hang dups on list
      (setq shell-history-list (cons command shell-history-list))
      (let ((prune-pt (nthcdr shell-max-history shell-history-list)))
	(and prune-pt (rplacd prune-pt nil)))))
  (setq shell-history-index -1))

(defun shell-push-input ()
  "Pushes all pending shell input to shell. Like \\[shell-send-input], only
it doesn't append a newline. Useful for programs that expect to talk 
to a tty in raw mode (eg. tip(1)). The pushed input doesn't get recorded
on the shell's history list."
  (interactive)
  (goto-char (point-max))
  (move-marker last-input-start
	       (process-mark (get-buffer-process (current-buffer))))
  (move-marker last-input-end (point))
  (let ((process (get-buffer-process (current-buffer))))
    (process-send-region process last-input-start last-input-end)
    (set-marker (process-mark process) (point))))

