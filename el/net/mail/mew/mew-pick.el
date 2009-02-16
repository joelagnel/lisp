;;; mew-pick.el --- Picking up messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick commands
;;;

(defun mew-summary-pick-mark (&optional arg)
  "Pick messages according to a pick pattern which you input,
then put the '*' mark onto them. 'mewl' is called as a picking
command. If called with '\\[universal-argument]', the target is
the region."
  (interactive "P")
  (mew-pickable
   (let ((func 'mew-summary-pick-with-mewl))
     (if (and mew-prog-mewl
	      (not (string= mew-prog-mewl ""))
	      (mew-which-exec mew-prog-mewl))
	 (mew-summary-pick-body func arg t)
       (message "No command")))))

(defun mew-summary-grep-mark (&optional arg)
  "Grep messages according to a grep pattern which you input,
then put the '*' mark onto them. 'mew-prog-grep' is called as a
grep command. If called with '\\[universal-argument]', the target
is the region."
  (interactive "P")
  (mew-pickable
   (let ((func 'mew-summary-pick-with-grep))
     (if (and mew-prog-grep
	      (not (string= mew-prog-grep ""))
	      (mew-which-exec mew-prog-grep))
	 (mew-summary-pick-body func arg nil)
       (message "No command")))))

(defvar mew-input-cmd-hist (list mew-prog-grep))

(defun mew-summary-cmd-mark (&optional arg)
  "Grep messages according to a grep pattern which you input,
then put the '*' mark onto them. You can asked to specify a grep
command.  If called with '\\[universal-argument]', the target is
the region."
  (interactive "P")
  (mew-pickable
   (let ((func 'mew-summary-pick-with-cmd)
	 (mew-inherit-grep-cmd (read-string "Command opts pattern: "
                                            (car mew-input-cmd-hist)
                                            'mew-input-cmd-hist)))
     (mew-summary-pick-body func arg nil 'nopattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick body
;;;

(defun mew-summary-pick-body (func regionp internalp &optional nopattern)
  (let* ((folder (mew-summary-folder-name))
	 (dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
	 msg msgs beg end region doit pattern)
    (if (mew-mark-active-p) (setq regionp t))
    (cond
     ((and internalp (not regionp))
      (setq doit t)
      (setq msgs nil))
     (regionp
      (setq region (mew-summary-get-region))
      (setq beg (car region))
      (setq end (cdr region))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward mew-regex-msg-or-part end t) ;; for thread
	  (setq msg (mew-summary-message-number))
	  (setq msgs (cons msg msgs))
	  (forward-line)))
      (setq msgs (delq nil msgs))
      (setq msgs (nreverse msgs))
      (if msgs (setq doit t)))
     (t
      (setq msgs (mew-dir-messages ".")) ;; all messages
      (setq msgs (delq nil msgs))
      (if msgs (setq doit t))))
    (if (not doit)
	(message "No message")
      (if internalp
	  (progn
	    (setq pattern (mew-input-pick-pattern))
	    (mew-sinfo-set-find-key nil)) ;; force to ask a user
	(unless nopattern
	  (setq pattern (mew-input-grep-pattern)))
	(mew-sinfo-set-find-key pattern))
      (mew-summary-pick-body1 func msgs pattern))))

(defun mew-summary-pick-body1 (func src-msgs pattern)
  (let ((folder (mew-summary-folder-name))
	(preline 0)
	msgs msgsback threadmsgs total linenum n)
    (message "Picking messages in %s..." folder)
    (setq msgs (funcall func folder pattern src-msgs))
    (setq msgsback msgs)
    (message "Picking messages in %s...done" folder)
    (if (null msgs)
	(message "No message to be marked")
      (setq n (length msgs))
      (if (= n 1)
	  (message "Marking 1 message...")
	(message "Marking %d messages..." n))
      (save-excursion
	(cond
	 ((mew-thread-p)
	  ;; Thread mode
	  (setq threadmsgs (mew-summary-thread-get-msglst
			    (mew-vinfo-get-top) 'separator))
	  (setq total (length threadmsgs))
	  (goto-char (point-min))
	  (while (and msgs (not (eobp)))
	    (setq linenum (member (car msgs) threadmsgs))
	    (if (null linenum)
		(setq msgsback (delete (car msgs) msgsback))
	      (setq linenum (- total (length linenum)))
	      (forward-line (- linenum preline))
	      (setq preline linenum)
	      (mew-summary-mark-as mew-mark-review))
	    (setq msgs (cdr msgs)))
	  (set-buffer-modified-p nil))
	 ((mew-virtual-p)
	  ;; Virtual mode
	  (goto-char (point-min))
	  (setq msgsback nil)
	  (while (and msgs (not (eobp)))
	    (when (re-search-forward (mew-regex-sumsyn-msg (car msgs)) nil t)
	      (setq msgsback (cons (car msgs) msgsback))
	      (mew-summary-mark-as mew-mark-review)
	      (forward-line))
	    (setq msgs (cdr msgs)))
	  (setq msgsback (nreverse msgsback))
	  (set-buffer-modified-p nil)))
	;; Summary mode (physical)
	(when (get-buffer folder)
	  (set-buffer folder)
	  (save-excursion
	    (goto-char (point-min))
	    (setq msgs msgsback)
	    (while (and msgs (not (eobp)))
	      (when (re-search-forward (mew-regex-sumsyn-msg (car msgs)) nil t)
		(mew-summary-mark-as mew-mark-review)
		(forward-line))
	      (setq msgs (cdr msgs)))
	    (set-buffer-modified-p nil))))
      (if (= n 1)
	  (message "Marking 1 message...done")
	(message "Marking %d messages...done" n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick internal
;;;

(defun mew-summary-pick-with-mewl (folder pattern src-msgs)
  "A function to pick messages matching PATTERN with 'mewl'"
  (mew-summary-with-mewl
   (let ((fld (mew-expand-folder2 folder))
	 i min max msgs range)
     (when src-msgs
       (setq min (string-to-int (car src-msgs)))
       (setq max min)
       (setq src-msgs (cdr src-msgs))
       (while src-msgs
	 (setq i (string-to-int (car src-msgs)))
	 (if (< i min) (setq min i))
	 (if (> i max) (setq max i))
	 (setq src-msgs (cdr src-msgs)))
       (setq range (list (format "%d-%d" min max))))
     (with-temp-buffer
       (mew-set-buffer-multibyte t)
       (mew-piolet
	mew-cs-text-for-read mew-cs-text-for-write
	(apply 'call-process
	       mew-prog-mewl nil t nil
	       "-b" mew-mail-path
	       "-l" (int-to-string mew-scan-max-field-length)
	       "-p" pattern fld range))
       (goto-char (point-min))
       (while (not (eobp))
	 (if (looking-at mew-regex-message-files)
	     (setq msgs (cons (mew-match-string 0) msgs)))
	 (forward-line)))
     (nreverse msgs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grep
;;;

(defun mew-summary-pick-with-grep (folder pattern src-msgs)
  "A function to pick messages matching PATTERN."
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
	 msgs nxt)
    ;; no sort here
    (if (= (length src-msgs) 1) (setq src-msgs (cons null-device src-msgs)))
    (setq pattern (mew-cs-encode-arg pattern))
    (with-temp-buffer
      (cd dir)
      (mew-piolet
       mew-cs-text-for-read mew-cs-text-for-write
       (while src-msgs
	 (goto-char (point-max))
	 (setq nxt (nthcdr mew-prog-grep-max-msgs src-msgs))
	 (if nxt (setcdr (nthcdr (1- mew-prog-grep-max-msgs) src-msgs) nil))
	 (apply 'call-process
		mew-prog-grep nil t nil
		(append mew-prog-grep-opts (list pattern) src-msgs))
	 (setq src-msgs nxt)))
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at mew-regex-message-files2)
	    (setq msgs (cons (mew-match-string 0) msgs)))
	(forward-line)))
    (setq msgs (mew-uniq-list msgs))
    (setq msgs (mapcar 'string-to-int msgs))
    (setq msgs (sort msgs '<))
    (mapcar 'int-to-string msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cmd
;;;

(defun mew-summary-pick-with-cmd (folder pattern src-msgs)
  "A function to pick messages matching PATTERN."
  ;; pattern is ignored
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
	 (cmd mew-inherit-grep-cmd)
	 msgs nxt arg)
    ;; no sort here
    (if (= (length src-msgs) 1) (setq src-msgs (cons null-device src-msgs)))
    (with-temp-buffer
      (cd dir)
      (mew-piolet
       mew-cs-text-for-read mew-cs-text-for-write
       (while src-msgs
	 (goto-char (point-max))
	 (setq nxt (nthcdr mew-prog-grep-max-msgs src-msgs))
	 (if nxt (setcdr (nthcdr (1- mew-prog-grep-max-msgs) src-msgs) nil))
	 (setq arg (mapconcat 'identity (cons cmd src-msgs) " "))
	 (setq arg (mew-cs-encode-arg arg))
	 ;; call shell to handle quote stuff
	 (call-process shell-file-name nil t nil shell-command-switch arg)
	 (setq src-msgs nxt)))
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at mew-regex-message-files2)
	    (setq msgs (cons (mew-match-string 0) msgs)))
	(forward-line)))
    (setq msgs (mew-uniq-list msgs))
    (setq msgs (mapcar 'string-to-int msgs))
    (setq msgs (sort msgs '<))
    (mapcar 'int-to-string msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying keyword
;;;

(defun mew-summary-find-keyword-down (&optional arg)
  "Display a message marked with '*' and find a keyword and highlight
it in the forward direction. The keyword is stored in a buffer local
variable in Summary mode. If no keyword is set to the variable, this
command first asks you for a keyword. If you want to change the stored
keyword, execute this command with '\\[universal-argument]'."
  (interactive "P")
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (ofld (mew-current-get-fld (mew-frame-id)))
	 (omsg (mew-current-get-msg (mew-frame-id)))
	 (cwin (get-buffer-window (current-buffer)))
	 (mbuf (mew-buffer-message))
	 (mwin (get-buffer-window mbuf))
	 (key (mew-sinfo-get-find-key))
	 (mark (mew-summary-get-mark))
	 (search t) end top)
    (when (or arg (not (stringp key)))
      (setq key (read-string
		 "Keyword: " (or (car mew-input-pick-pattern-hist) key)))
      (mew-sinfo-set-find-key key))
    (cond
     ((and (equal mew-mark-review mark)
	   (or (not (string= fld ofld)) (not (string= msg omsg))))
      (mew-summary-display-asis)
      (setq top t))
     ((or (null mwin)
	  (not (equal mew-mark-review mark))
	  (or (not (string= fld ofld)) (not (string= msg omsg)))
	  (save-excursion (set-buffer mbuf) (eobp)))
      (if (not (mew-summary-down-mark mew-mark-review))
	  (setq search nil)
	(mew-summary-display-asis)
	(setq top t))))
    (setq mwin (get-buffer-window mbuf))
    (if (not search)
	(message "No more marked messages")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (if top (goto-char (point-min)))
	    (if (setq end (re-search-forward key nil t))
		(progn
		  (isearch-highlight (- end (length key)) end)
		  (recenter (/ (window-height) 2)))
	      (goto-char (point-max))
	      (message "Keyword '%s' is not found" key)
	      (recenter -1)))
	(select-window cwin)))))

(defun mew-summary-find-keyword-up (&optional arg)
  "Display a message marked with '*' and find a keyword and highlight
it in the backward direction. The keyword is stored in a buffer local
variable in Summary mode. If no keyword is set to the variable, this
command first asks you for a keyword. If you want to change the stored
keyword, execute this command with '\\[universal-argument]'."
  (interactive "P")
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (ofld (mew-current-get-fld (mew-frame-id)))
	 (omsg (mew-current-get-msg (mew-frame-id)))
	 (cwin (get-buffer-window (current-buffer)))
	 (mbuf (mew-buffer-message))
	 (mwin (get-buffer-window mbuf))
	 (key (mew-sinfo-get-find-key))
	 (mark (mew-summary-get-mark))
	 (search t) beg bottom)
    (when (or arg (not (stringp key)))
      (setq key (read-string
		 "Keyword: " (or (car mew-input-pick-pattern-hist) key)))
      (mew-sinfo-set-find-key key))
    (cond
     ((and (equal mew-mark-review mark)
	   (or (not (string= fld ofld)) (not (string= msg omsg))))
      (mew-summary-display-asis)
      (setq bottom t))
     ((or (null mwin)
	  (not (equal mew-mark-review mark))
	  (or (not (string= fld ofld)) (not (string= msg omsg)))
	  (save-excursion (set-buffer mbuf) (bobp)))
      (if (not (mew-summary-up-mark mew-mark-review))
	  (setq search nil)
	(mew-summary-display-asis)
	(setq bottom t))))
    (setq mwin (get-buffer-window mbuf))
    (if (not search)
	(message "No more marked messages")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (if bottom (goto-char (point-max)))
	    (if (setq beg (re-search-backward key nil t))
		(progn
		  (isearch-highlight beg (+ beg (length key)))
		  (recenter (/ (window-height) 2)))
	      (goto-char (point-min))
	      (message "Keyword '%s' is not found" key)
	      (recenter 0)))
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick macro
;;;

(defvar mew-pick-macro-alist nil)

(defun mew-pick-define-macro (str1 str2)
  "Define pick macro."
  (interactive (list (read-string "Pick pattern: ")
		     (read-string "Macro body: ")))
  ;; macro-pattern is a string including no #, or
  ;; a string in a form FIELD=#1 #2 #3...#n.
  ;; #1 can be replaced by #.
  (let (args body asc value)
    (while (string-match "\\(#[0-9]*\\)[, ]*" str1)
      (setq args (cons (intern (match-string 1 str1)) args))
      (setq str1 (replace-match "" nil t str1)))
    (while (string-match "#[0-9]*" str2)
      (setq body (cons (substring str2 0 (match-beginning 0)) body))
      (setq body (cons (intern (match-string 0 str2)) body))
      (setq str2 (substring str2 (match-end 0))))
    (setq body (cons str2 body))
    (setq asc (assoc str1 mew-pick-macro-alist))
    (setq value (cons (nreverse args) (nreverse body)))
    (if asc
	(setcdr asc value)
      (setq mew-pick-macro-alist
	    (cons (cons str1 value) mew-pick-macro-alist)))))
    
(defun mew-pick-macro-expand (name args)
  (let ((asc (assoc name mew-pick-macro-alist))
	alist args2 body body-copy assq)
    (if (not asc)
	name
      (setq args2 (nth 1 asc))
      (setq body (nthcdr 2 asc))
      (while (and args args2)
	(setq alist (cons (cons (car args2) (car args)) alist))
	(setq args (cdr args))
	(setq args2 (cdr args2)))
      (while body
	(if (stringp (car body))
	    (setq body-copy (cons (car body) body-copy))
	  (setq assq (assq (car body) alist))
	  (if assq (setq body-copy (cons (cdr assq) body-copy))))
	(setq body (cdr body)))
      (concat "("
	      (mew-pick-macro-expand-string
	       (apply 'concat (nreverse body-copy)))
	      ")"))))

(defun mew-pick-macro-expand-string (str)
  (if (string= str "") 
      ""
    (let ((first (string-to-char str))
	  asc key rest eq-flag val args)
      (if (memq first (list ?\( ?\! ?\& ?\| ?= mew-sp ?\)))
	  (concat (char-to-string first)
		  (mew-pick-macro-expand-string (substring str 1)))
	(if (string-match "=\\| \\|)\\|&\\||" str)
	    (if (string= (match-string 0 str) "=")
		(progn
		  (setq eq-flag t)
		  (setq key (substring str 0 (match-end 0)))
		  (setq rest (substring str (match-end 0))))
	      (setq key (substring str 0 (match-beginning 0)))
	      (setq rest (substring str (match-beginning 0))))
	  (setq key str)
	  (setq rest ""))
	(setq asc (assoc key mew-pick-macro-alist))
	(cond
	 (asc
	  (setq args (nth 1 asc))
	  (while args
	    (if (string-match ",\\| \\|)\\|&\\||" rest)
		(progn
		  (setq val (cons (substring rest 0 (match-beginning 0)) val))
		  (setq rest (substring rest (match-beginning 0))))
	      (setq val (cons rest val))
	      (setq rest ""))
	    (setq args (cdr args)))
	  (concat
	   (mew-pick-macro-expand key (nreverse val))
	   (mew-pick-macro-expand-string rest)))
	 (eq-flag
	  (if (string-match " \\|)\\|&\\||" rest)
	      (progn
		(setq val (substring rest 0 (match-beginning 0)))
		(setq rest (substring rest (match-beginning 0))))
	    (setq val rest)
	    (setq rest ""))
	  (concat key val (mew-pick-macro-expand-string rest)))
	 (t
	  (concat key (mew-pick-macro-expand-string rest))))))))

(provide 'mew-pick)

;;; Copyright Notice:

;; Copyright (C) 1996-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-pick.el ends here
