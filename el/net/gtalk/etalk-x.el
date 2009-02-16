;;; etalk-x --- special X support for menus etc under etalk
;;
;; Copyright (C) 1994, 1996, 1998, 1999 Free Software Foundation
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
;;   To hide all window specific things into a sub-file to reduce load time
;; for someone using a terminal.
;;
;; $Id: etalk-x.el,v 1.16 1999/11/29 16:56:50 zappo Exp $

(require 'etalk-tyrn)
(require 'easymenu)

;;; Code:

;;; Modify any running menus to add our cool stuff there
;;
(defvar etalk-command-keymap nil
  "Keymap used to submenu different etalk commands.")

(easy-menu-define
 etalk-command-menu etalk-command-keymap "Etalk command menu"
 '("Etalk"
   [ "Talk to..." etalk t ]
   [ "Reply to last reqest" etalk-reply t ]
   ))

;; The menu for ETALK LOG
;;
(easy-menu-define
 etalk-log-mode-menu etalk-log-mode-map "Etalk log mode menu"
 '("Etalk"
   [ "Process Help" etalk-send-help-command etalk-process ]
   [ "Users" etalk-send-users-command etalk-process ]
   [ "Devices" etalk-send-device-command etalk-process ]
   [ "Hosts" etalk-send-host-command etalk-process]
   [ "Clean" etalk-send-clean-command etalk-process ]
   [ "Abort Call" etalk-send-abort-command etalk-process ]
   [ "Quit Process" etalk-send-quit-command etalk-process ]
   [ "Destroy Process" etalk-kill-process etalk-process ]
   ))

;; The menu for etalk connections
;;
(easy-menu-define
 etalk-mode-map-menu etalk-mode-map "Etalk menu"
 '("Connections"
   [ "Call ..." etalk t ]
   [ "Ringer Off" etalk-ringer-off-command t ]
   [ "Ringer On" etalk-ringer-on-command t ]
   [ "TCP Accept Always" etalk-tcpaccept-always-command t]
   [ "TCP Accept Sometimes" etalk-tcpaccept-query-command t]
   [ "TCP Accept Never" etalk-tcpaccept-never-command t]
   [ "Play Game ..." etalk-x-play-game
     (and etalk-remote-is-emacs etalk-tcp-list) ]
   [ "Send Message ..." etalk-send-minibuffer-message
     (and etalk-remote-is-emacs etalk-tcp-list) ]
   [ "Insert File ..." etalk-insert-file etalk-tcp-list]
   [ "Transfer File ..." etalk-insert-file-buffer
     (and etalk-remote-is-emacs etalk-tcp-list) ]
   [ "Start Shared App" etalk-start-shared-app
     (and etalk-remote-is-emacs etalk-tcp-list) ]
   [ "Enable Filter" etalk-enable-user-filter
     (and etalk-remote-is-emacs etalk-tcp-list) ]
   [ "Refresh Windows" etalk-setup-windows t ]
   [ "Show Log" etalk-setup-windows-with-log t ]
   [ "Hangup" etalk-x-hangup t ]
   ))
   
;; Tyrant mode map menu
;;
(easy-menu-define
 etalk-tyrant-menu etalk-tyrant-map "Etalk menu"
 '("Tyrant"
   [ "Game Help" etalk-tyrant-help t ]
   [ "Send Message" tyrant-send-minibuffer-message t ]
   [ "Cancel Game" etalk-usurp-tyrant-keyed t ]
   ))

(if (string-match "XEmacs" emacs-version)
    ;; XEmacs specifics
    nil
  ;; Emacs specifics
  (let ((toolmap (lookup-key global-map [menu-bar tools])))
    (define-key-after toolmap [etalk] (cons "ETalk" etalk-command-keymap)
      [calendar])
    ;; we don't want the edit menu hangin about...
    (define-key etalk-log-mode-map [menu-bar edit] 'undefined)
    ;; we don't want the edit menu hangin about...
    (define-key etalk-mode-map [menu-bar edit] 'undefined)
    ;; we don't want the edit menu hangin about...
    (define-key etalk-tyrant-map [menu-bar edit] 'undefined)
    ))

;; fontlock keywords (if applicable)
;;

;; all we need to do is define the keywords.  font lock handles the rest
(defconst etalk-log-font-lock-keywords
  (list
   '("^ \\*\\* [^\n]+$" . font-lock-function-name-face)
   '("^[^ ][^:\n\t]*:" . font-lock-comment-face)
   '("ETALK version[^\n]+$" . font-lock-type-face)
   '("^Name[^\n]+$" . font-lock-type-face)
   '("<[^>]+>" . font-lock-string-face)
   )
  "Keywords used to highlight the etalk log buffer.")

(defconst etalk-font-lock-keywords
  (list
   '("\\(\\*[A-Z]+\\*\\)" 1 'bold-italic)
   '("\\(\\*[^*]+\\*\\)" 1 'bold)
   '("\\(_[^*]+_\\)" 1 'underline)
   '("\\(/[^*]+/\\)" 1 'italic)
   '("\\([;:][-*]?[()0O]\\)" 1 'font-lock-keyword-face) ;smiley face
   )
  "Keywords used to highlight conversation buffers.
Mostly items that highlight usual ways of exemplifying plain text.")
   

;; Used to support hilit19 here, but it was too anoying to use with
;; compiled code

;;; Special menu function designed for lists of various things.
;;
(defun etalk-x-list-2-menu (event title list &optional max)
  "On EVENT take a list and turn it into a pop-up menu with TITLE.
It returns an index into LIST.  The list may have anything in it,
and they need not be of the same type.  This function must be bound to
a mouse event.  Optional MAX limits the size of the menu."

  (let ((menu))
    (setq menu
	  (cons ""			; single frame
		(list
		 (let ((tail list)
		       (head nil)
		       (i 1))
		   (cons title
			 (progn
			   (while (and tail (or (not max) (<= i max)))
			     (setq head (cons
					 (cons
					  (format "%s"
						  ; go to smallest element
						  (let ((elt (car tail)))
						    (while (listp elt)
						      (setq elt (car elt)))
						    elt))
					  i)
					 head))
			     (setq i (1+ i))
			     (setq tail (cdr tail)))
			   (reverse head)))))))
    (let ((n (x-popup-menu event menu)))
      (if (integerp n)
	  (1- n)			;the nth starts at 0, we must start
					;at 1, or the first elt returns nil
	nil))))

;;; Some functions which use above menu to be bound to menu things.
;;
(defun etalk-x-hangup (event)
  "Hangup on remote processes from a menu EVENT."
  (interactive "e")
  ;; if there isn't much about, just hangup,
  (if (<= (length etalk-tcp-list) 1)
     (progn
       (set-buffer (etalk-format etalk-local-buffer-name))
       (etalk-nuke-connection))
    ;; otherwise, lets pop up a menu to decide who to mangle..
    (let ((c (etalk-x-list-2-menu
	      event "Hanup on who?"
	      (cons (list "Everyone")
		    (reverse
		     (let ((words nil) (lop etalk-tcp-list))
		       (while lop
			 (setq words (cons (list
					    (buffer-name (process-buffer
							  (car (car lop)))))
					   words))
			 (setq lop (cdr lop)))
		       words))))))
      (cond
       ((not c) nil)
       ((= c 0) (etalk-zorch-all-processes)
	(message "Not feeling social anymore?"))
       (t (etalk-nuke-connection (nth (1- c) (car etalk-tcp-list))))))))

(defun etalk-x-play-game (event)
  "Choose a game from a menu EVENT.
Once it is chosen, that game is initiated with the remote process."
  (interactive "e")
  (let* ((fns (reverse etalk-legal-multiuser-functions))
	 (c (etalk-x-list-2-menu event "What to play?" fns)))
    (cond
     ((not c) nil)
     (t (etalk-initiate-special-function
	 (car (nth c fns)))))))

;;; now some replacement mouse functions...
(defun etalk-mouse-yank (event)
  "Yank cut-buffer text with based on a mouse EVENT.
Yanked text is taken from the cut buffer, or from the X clipboard."
  (interactive "e")
  ;; turn of isearch and the like...
  (run-hooks 'mouse-leave-buffer-hook)
  (etalk-yank-text))

(defun etalk-mouse-yank-secondary (event)
  "Yank cut-buffer text with based on a mouse EVENT.
Yanked text is taken from the seondary cut buffer."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (etalk-string-to-remote (x-get-selection 'SECONDARY)))

(provide 'etalk-x)
;;; etalk-x ends here
