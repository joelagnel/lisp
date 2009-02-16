;; This file contains code of questionable quality.
;;
;; I accept no responsibility for damaged machines
;; accounts or egos.
;;
;; To the best of my knowlege, what is contianed here seems
;; to work for myself, and for my friends.
;;
;; All testing of this software so far was done with:
;;   berkely unix V  4.2A
;;   emacs        v  18.58.2
;;   gcc          v  2.1
;;
;; boring blurb dated: July 6th
;;--------------------------------------
;; This file is a part of talk.el
;;--------------------------------------
;;
;;  Eric ludlam
;;
;;--------------------------------------

(provide 'talk-process)

(defun talk-startup-talkprocess ()
  "Startup a process from executable TALK to interface to
emacs windows."

  (talk-error-thing "Talk-startup-talkprocess")


  ;; get the buffer values of who to talk to so you can hangup, and
  ;; recall someone, without retyping...

  (if (not (stringp talk-remote-who))
      (error "Required local variables not found in buffer %s"
	     (get-buffer (current-buffer))))

  (if (get-process (format "talk-process-%s" talk-remote-who))
      (error "You are already talking to them!"))

  ;; restart process, need to get new set.
  (setq talk-edit-chars "") 

  (if talk-remote-tty
      (progn
	(start-process (format "talk-process-%s" talk-remote-who)
		       (current-buffer)
		       talk-process-file
		       talk-edit-characters-mine 
		       talk-remote-who
		       talk-remote-where
		       talk-announce-as 
		       talk-remote-tty))
    (progn
      (start-process (format "talk-process-%s" talk-remote-who) 
		     (current-buffer)
		     talk-process-file
		     talk-edit-characters-mine 
		     talk-remote-who
		     talk-remote-where
		     talk-announce-as )))
  
  (set-process-filter (get-process 
		       (format "talk-process-%s" talk-remote-who)) 
		      'talk-filter-proc)
  (set-process-sentinel (get-process 
			 (format "talk-process-%s" talk-remote-who)) 
			'talk-sentinel-proc)
  (talk-error-thing "Done in creation")

  (setq talk-remote-process-list 
	(cons (get-buffer-process (current-buffer)) 
	      talk-remote-process-list))

  (setq mode-line-process '(" [%s]"))
  )

(defun talk-filter-proc (process output)
  "Filters input from a talk process for printing, or buffer
control."

  (talk-error-thing "Talk-filter-proc")

  (setq oldbuffer (current-buffer))

  ;; if the buffer hosting this process is gone, terminate session!

  (if (not (bufferp (process-buffer process)))
      (progn
	(process-send-string process 
	   "\03Ooops!  Host emacs buffer deleted, terminating call.\n")
	(talk-nuke-TALK process)
	(error "That buffer no longer exists!"))
    (set-buffer (process-buffer process)))

  (setq strlen (length output))
  (setq counter 0)
  (while (< counter strlen)
    (setq tchar (string-to-char (substring output counter (+ 1 counter))))
    (setq counter (+ 1 counter))
    (if (equal 3 tchar) 
	(setq talk-filter-message "") ;; init message sequence
	(if talk-filter-message
	    (if (equal tchar 10)       ;; if end of line,  
		(progn
		  (if talk-message-to-minibuffer
		      (message talk-filter-message)   ;; print the message
		    (save-excursion
		      (goto-char talk-point)
		      (insert talk-filter-message)
		      (move-marker talk-point (point))))
		  (setq talk-filter-message nil)) ;; reset message
		  (setq talk-filter-message       ;; else append new char 
			(concat talk-filter-message 
				(char-to-string tchar))))
	  (if (< (length talk-edit-chars) 3)  ;; if we havn't gotten the 
	      ;; edit chars yet, get em.
	      (progn
		(setq talk-edit-chars 
		      (concat talk-edit-chars (char-to-string tchar)))
		(if (equal talk-edit-chars talk-edit-characters-mine)
		    (progn
		      (setq talk-remote-is-emacs t) ; they use emacs too!
		      (message "%s is using Emacs too!" talk-remote-who)
		      (ding))
		  (message "%s is using regular talk." talk-remote-who)))
	      ;; last but not least, if no messages collecting, and no
	      ;; edit chars being waited for, by golly, do something with the
	      ;; input
	      (cond
	       ;; delete character is character[0] in string	   
	       ((equal (substring talk-edit-chars 0 1) (char-to-string tchar))
		(save-excursion
		  (goto-char talk-point)
		  (talk-backward-delete-not-past-eob)
		  (move-marker talk-point (point))))
	       ;; delete line is character 2
	       ((equal (substring talk-edit-chars 1 2) (char-to-string tchar))
		(save-excursion
		  (goto-char talk-point)
		  (kill-line -1)
		  (move-marker talk-point (point))))
	       ;; delete word is character 3
	       ((equal (substring talk-edit-chars 2 3) (char-to-string tchar))
		(save-excursion
		  (goto-char talk-point)
		  (talk-delete-word-backwards-not-past-eob)
		  (move-marker talk-point (point))))
	       ;; what about ctrl-l?  Throw it away!
	       ((equal tchar 12) 
		(message "%s' screen was dirty. Should be ok now." 
			 talk-remote-who))
	       ;; otherwise, go and print the character to the string.
	       (t
		(save-excursion
		  (goto-char talk-point)
		  (insert (char-to-string tchar))
		  (move-marker talk-point (point)))))))))
  (set-buffer oldbuffer)

  ;; This line will hopefully be changed to some sort of option
  ;; dependant on some sort of criteria.  (variable, point check, etc.)

  (talk-move-realpoint (process-buffer process)))

(defun talk-sentinel-proc (process event)
  "The procedure is called whenever a process changes status.  Used to
detect the remote process hanging up to remove a process from the talk
process list."

  ;; (message "Hangup.")    
  ;; no message. all disconnects seem to have a special message with them.
  (ding)
  (if (or (equal event "finished\n")
	  (equal event "killed\n"))
      (talk-remove-process-from-list process)))

(defun talk-send-where ()
  (talk-error-thing "Talk-send-where")

  (if (equal (current-buffer) (get-buffer talk-local-buffer-name))
      talk-remote-process-list
    (current-buffer)))

(defun talk-send-output (buffer-or-list output-string)
  "Send output to buffer's process only.  If buffer is nil,
then send to list of all open talk processes."

  (interactive)
  (talk-error-thing "Talk-send-ouput")

  (setq pl talk-remote-process-list)
  (if buffer-or-list
      (if (bufferp buffer-or-list)
	  (setq pl (cons (get-buffer-process buffer-or-list) '()))
	(if (listp buffer-or-list)
	    (setq pl buffer-or-list))))
  (while (car pl)
    (process-send-string (car pl) output-string)
    (setq pl (cdr pl))))


(defun talk-nuke-TALK (&optional process)
  "Zap the talk process.  Uses delete. Should change to
signals sometime in the near future."

  (interactive)
  (talk-error-thing "Talk-nuke-TALK")
  
  (if (and (not process) (equal (current-buffer) 
				(get-buffer talk-local-buffer-name)))
      (cond
       ((equal (length talk-remote-process-list) 0)
	(error "No processes to delete."))
       ((equal (length talk-remote-process-list) 1)
	(talk-nuke-TALK (car talk-remote-process-list))
	(setq talk-remote-process-list '()))
       (t
	(error 
	 "Place cursor in remote window to delete selected connection.")))
    (progn
      (setq p (if process process (get-buffer-process (current-buffer))))
      (quit-process p)
      (talk-remove-process-from-list p))))

(defun talk-remove-process-from-list (process)
  "Function internal:  Remove one process object from the process
list."

  (talk-error-thing "Talk-temore-process-from-list")

  (setq n '())
  (setq l talk-remote-process-list)
  (while (car l)
    (if (not (equal (car l) process))
	(setq n (cons (car l) n)))
    (setq l (cdr l)))
  (setq talk-remote-process-list n))
