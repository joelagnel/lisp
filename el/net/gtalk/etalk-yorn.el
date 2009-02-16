;;; etalk-yorn --- yes or no process-filter safe query
;;
;; Copyright (C) 1994, 1996 Free Software Foundation
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.

;;; Commentary:
;;  Because EMACS gets upset when you abort a recursive edit from
;;  within a filter, this was written to compensate for y-or-n-p so
;;  that G-g simply says NO instead of exiting.

;;; $Id: etalk-yorn.el,v 1.6 1997/10/08 18:19:25 zappo Exp $

;;; Code:
(defcustom etalk-bufferize-yorn nil
  "*Non nil use buffer query instead of minibuffer query.
Some systems can cause severe problems if the other version is used."
  :group 'etalk
  :type 'boolean)

(defvar etalk-stored-savefunct nil
  "Used locally to remember what to do after someone answers Y to some request.")


(defvar etalk-yorn-map nil
  "Keymap for etalk-y-or-n-p functions.")

(if etalk-yorn-map
    ()
  (setq etalk-yorn-map (make-keymap))
  (etalk-superbind-alpha etalk-yorn-map 'etalk-yorn-wrong)
  (define-key etalk-yorn-map "y" 'etalk-yorn-yes)
  (define-key etalk-yorn-map "n" 'etalk-yorn-no)
  (define-key etalk-yorn-map "Y" 'etalk-yorn-yes)
  (define-key etalk-yorn-map "N" 'etalk-yorn-no)
  (define-key etalk-yorn-map " " 'etalk-yorn-yes)
  (define-key etalk-yorn-map "\C-g" 'etalk-yorn-no))

(defvar etalk-proc-yorn-map nil
  "Keymap for etalk `y-or-n-p' within a buffer.")

(if etalk-proc-yorn-map
    ()
  (setq etalk-proc-yorn-map (make-keymap))
  (etalk-superbind-alpha etalk-proc-yorn-map 'etalk-proc-yorn-wrong)
  (define-key etalk-proc-yorn-map "y" 'etalk-proc-yorn-yes)
  (define-key etalk-proc-yorn-map "n" 'etalk-proc-yorn-no)
  (define-key etalk-proc-yorn-map "Y" 'etalk-proc-yorn-yes)
  (define-key etalk-proc-yorn-map "N" 'etalk-proc-yorn-no)
  (define-key etalk-proc-yorn-map "\C-g" 'etalk-proc-yorn-no))

(defun etalk-bufferized-yorn (savefunct)
  "Place a y/n query into the current talk buffer.
This is safer than using `y-or-n-p'.  SAVEFUNCT is stored in
`etalk-stored-savefunt' and is used later to initiate what this query
is for."
		      
  (let ((m1 (point)))
    (insert (format "Would you like to use %s with %s?"
		    savefunct etalk-remote-who))
    (set-marker etalk-point m1) ; we don't want this in the buffer
			       ; long.
    (use-local-map etalk-proc-yorn-map)	;new map to read y/n stuff
    (setq etalk-stored-savefunct savefunct)
    ))

(defun etalk-proc-yorn-yes ()
  "Yes answer from talk buffer."
  (interactive)
  ;; do enable thing
  (use-local-map etalk-mode-map)
  (delete-region etalk-point (point-max))
  (etalk-send-output (list (get-buffer-process (current-buffer)))
		     "\03\02y\n")
  (etalk-remote-start-function etalk-stored-savefunct)
  (tyrant-player2)
  (run-hooks 'tyrant-player2-hook)
  (setq tyrant-player1-hook nil)
  (setq tyrant-player2-hook nil)
  (setq etalk-filter-message nil)
  ;; toast that message from before!
  )
(defun etalk-proc-yorn-no ()
  (interactive)
  ;; do no thingy
  (delete-region etalk-point (point-max))
  (use-local-map etalk-mode-map)
  (message "Sending refused requrest.")
  (etalk-send-output (list (get-buffer-process (current-buffer)))
		     "\03\02nRequest Refused!\n"))

(defun etalk-proc-yorn-wrong ()
  "Message that something is wrong in bufferized yorn."
  (interactive)
  (ding)
  (message "Please press Y or N to talk prompt!"))

(defun etalk-yorn-p (prompt)
  "Substitute for `y-or-n-p' with PROMPT.
Use this because a `exit-recursive-edit' is unsafe in a filter."

  (interactive)
  (save-excursion
    (let ((ans (read-from-minibuffer (concat prompt " (y or n) ")
				     "" etalk-yorn-map nil)))
      (cond ((or (string= ans "y") (string= ans "y"))
	     t)
	    (t nil)))))

(defun etalk-yorn-yes ()
  "They said yes!"
  (interactive) (insert "y") (sit-for 0) (exit-minibuffer))

(defun etalk-yorn-no ()
  "They said no!"
  (interactive) (exit-minibuffer))

(defun etalk-yorn-wrong ()
  "Say No No No No!"
  (interactive) (ding) (message "Respond with \"y\" or \"n\" please!"))

(provide 'etalk-yorn)
;;; etalk-yorn ends here
