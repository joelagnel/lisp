;;; tyrn-ai --- Play selected games against a specified AI program.
;;
;; Copyright (C) 1994, 1995, 1998 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Keywords: talk, games
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
;;

;;; $Id: tyrn-ai.el,v 1.5 1998/10/11 02:18:16 zappo Exp $
;; History:
;;

;;; Commentary:
;;   TYRANT mode AI interface.  This program will use tyrant mode to
;;   interface a game between a human, and a lisp AI program.  It
;;   requires selected files from from emacs talk.

;;; Code:

;; These 2 are also duplicated, but we need to to make this require as
;; little as possible from the etalk sources.
(defvar etalk-announce-as (user-login-name)
  "*Name passed to the remote talk deamon as your username.
Changing this means you announce yourself with a different name.
Most sysadmins would probably be upset if you did this to unknowing users.")

(defvar etalk-preferred-name (user-full-name)
  "*The user name you wish to use as your prefered name.")

(defvar tyrant-ai-hook nil
  "Hook to be run when starting up tyrant AI.
Good for starting sub-processes and the like")

(require 'sformat)
(require 'etalk-lgame)

;;;###autoload
(defun tyrant-play-computer (function)
  "Main function to start up a computer game against an AI opponant.
The game played is defined in FUNCTION."

  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Game: " etalk-legal-tyrant-ai-functions
			    nil t ))))
  ;; I tried, but don't get why it doesn't work.
  ;;'(tyrant-games-history . 1)))))

  ;; clean up the quit string to prevent flow over from other games
  (makunbound 'etalk-tyrant-quit-string)
  (fmakunbound 'etalk-tyrant-quit-string)

  (let ((lastb (current-buffer))
	(flst (assoc function etalk-legal-tyrant-ai-functions)))
    ;; game fn
    (require (nth 1 flst) (format "games/%s" (nth 1 flst)))
    ;; ai fn
    (require (nth 2 flst) (format "games/%s" (nth 2 flst)))
    ;; run it
    (funcall (nth 1 flst))
    (if (equal (current-buffer) lastb)
	(error "Ooops!  Mode did not start correctly."))
    
    ;; run the startup hook
    (run-hooks 'tyrant-ai-hook)

    ;; make a local variable on where the buffer is.
    (make-local-variable 'etalk-tyrant-return-buffer)
    (setq etalk-tyrant-return-buffer lastb)
	
    (make-local-variable 'etalk-tyrant-imprisoned-process)
    (setq etalk-tyrant-imprisoned-process nil) ;just for the record. emu etalk

    (make-local-variable 'etalk-tyrant-imprisoned-user)
    (setq etalk-tyrant-imprisoned-user "Emacs AI")
    
    (make-local-variable 'etalk-tyrant-imprisoned-preferred-name)
    (setq etalk-tyrant-imprisoned-preferred-name "Emacs AI Version 1.0")

    (make-local-variable 'tyrant-opponent-type)
    ;; we must set the variable part of this symbol as well..
    (setq tyrant-opponent-type nil)
    (fset 'tyrant-opponent-type (car (cdr (cdr flst))))

    (etalk-tyrannical-mode)
    (tyrant-player1)))

(defun tyrant-ai-input-string (str)
  "Take STR and run the string as macro on tyranted game map."

  ;; let user reflect on what s/he has done
  (sit-for 1)

  (let ((cnt 0))
    (while (< cnt (length str))
      ;; buildsequence is null when not given as argument.
      (let* ((tchar (aref str cnt))
	     (sequence (concat etalk-tyrant-local-buildsequence
			       (char-to-string tchar)))
	     (ttfk-sym (lookup-key etalk-borrowed-keymap sequence)))
	(setq cnt (+ cnt 1))
	(if ttfk-sym
	    (progn
	      (cond
	       ;; if ttfk-symbol is a keymap, then read the next char
	       ;; untill done.
	       ((keymapp ttfk-sym)
		(setq etalk-tyrant-local-buildsequence ttfk-sym))
	       ;; if symbol is a function, then get function and send sequence
	       ;; to remote.
	       ((fboundp ttfk-sym)
		(setq etalk-tyrant-local-buildsequence nil)
		(setq last-input-char tchar)
		(funcall ttfk-sym))
	       ;; just in case something weird happens
	       (t
		(setq etalk-tyrant-local-buildsequence nil)
		(etalk-tyrant-help))))
	  (etalk-tyrant-help)))
      (sit-for 2)
      )))


(provide 'tyrn-ai)
;;; tyrn-ai ends here
