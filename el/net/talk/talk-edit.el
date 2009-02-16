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

(provide 'talk-edit)

(defun talk-action-to-local (key-or-function)
  "Function internal: User keypress, is interpreted by other function, which
calls this function to 1) send a key or 2) simply edit the local buffer
by applying a function to it at talk-point."

  (talk-error-thing "Talk-action-to-local")

  (if (equal mode-name talk-remote-mode-string)
      ;; we are in a remote window. output goes only to the single
      ;; remote process.
      (setq buffer (current-buffer))
    ;; we are in some other buffer of unknown origin, so we then send
    ;; output to all remote talk processes.
    (setq buffer nil))
  
  (if (integerp key-or-function) ;; check if key, then send it to process.
      (talk-send-output buffer (char-to-string key-or-function)))
  
  (save-excursion   ;; modify the buffer.
    (set-buffer (get-buffer talk-local-buffer-name))
    (goto-char talk-point)
    (if (integerp key-or-function)
	(insert-char key-or-function 1)
      (eval key-or-function))
    (move-marker talk-point (point))
    (if (> (current-column) fill-column)
	(setq af t) (setq af nil)))
  (if af (talk-auto-fill-now))
  (talk-move-realpoint (get-buffer talk-local-buffer-name)))

(defun talk-move-realpoint (buffer)
  "Function internal: Move the real point assocaited with buffer to the
location pointed at by talk-point."

  (talk-error-thing "Talk-move-realpoint")

  (if (not (bufferp buffer))
      (error "That buffer no longer exists!"))

  ;; This is to check to makesure that you aren't in the minibuffer when 
  ;; this command is run.  If so, simply wait till later to move the point.
  ;; Otherwise minibuffer gets all confused.
  (if (not (equal (current-buffer) (get-buffer " *Minibuf-0*")))
      (progn
	
	(if (eq (current-buffer) buffer)
	    (goto-char talk-point)
	  
	  (progn                        ;; Else in some other buffer
	    (setq oldbuffer (current-buffer))
	    (if (get-buffer-window buffer)
		
		(progn              ;; that buffer is in a window
		  (select-window (get-buffer-window buffer))
		  (goto-char talk-point)
		  (select-window (get-buffer-window oldbuffer)))
	      
	      (progn                ;; else it is somewhere else
		(set-buffer buffer)
		(goto-char talk-point)
		(set-buffer oldbuffer))))))))
    
;; -------------------------------------------------------------- ;;
;; The following functions are mostly key functions and functions ;;
;; passed to talk-action-local, and used by talk-filter-proc.     ;;
;; -------------------------------------------------------------- ;;

(defun talk-insert-char ()
  "called by all keypresses, except a few, and sends them to
the remote, which is basically all talk does too."

  (interactive)
  (talk-error-thing "Talk-insert-char")

  (talk-action-to-local last-input-char))

(defun talk-auto-fill-now ()
  "This function will be called to automatically wrap a word
down to the next line for all versions of talk."
  (interactive)
  (talk-error-thing "Talk-auto-fill-now")

  (setq buflist (talk-send-where))
  (if (not (equal last-input-char ?\ ))
      (progn
	(talk-action-to-local '(talk-delete-word-backwards-not-past-eob)) 
	(if (< (length (car kill-ring-yank-pointer)) fill-column)
	    (progn
	      (talk-send-output buflist "")
	      (talk-action-to-local 10)
	      (talk-yank-text))
	  (save-excursion
	    (set-buffer (get-buffer talk-local-buffer-name))
	    (insert-before-markers (car kill-ring-yank-pointer))
	    (talk-action-to-local 10)))
	(setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))
    (talk-action-to-local 10)))

(defun talk-string-to-remote (text)
  " Send the string in text into the local buffer, and send
to remote talk as well. Done one character at a time to help with
autowrap and stuff."

  (interactive)
  (talk-error-thing "Talk-string-to-remote")

  (if (stringp text)
      (while (< 0 (length text) )
	(talk-action-to-local (string-to-char text))
	(setq text (substring text 1 (length text))))))
  

(defun talk-yank-text ()
  "This function copies the contents of the yank buffer into a talk window."

  (interactive)
  (talk-error-thing "Talk-yank-text")

  (talk-string-to-remote (car kill-ring-yank-pointer))
)

(defun talk-backward-delete-not-past-eob ()
  "delete one word backwards, but don't go past beginning of
line"
  (talk-error-thing "Talk-backward-delete-not-past-eob")

  (if (not (bolp))
      (backward-delete-char 1)))

(defun talk-delete-backwards ()
  "Send delete a character in your buffer, and send delete 
character." 

  (interactive)
  (talk-error-thing "Talk-delete-backwards")

  (talk-send-output (talk-send-where) (char-to-string last-input-char))
  (talk-action-to-local '(talk-backward-delete-not-past-eob))

)

(defun talk-RET ()
  "Send carriage return."

  (interactive)
  (talk-error-thing "Talk-RET")

  (talk-action-to-local 10))

(defun talk-delete-backward-to-char-or-bol (DACHAR)
  "Delete from the cursor backwards to CHAR, or to beginning
of line if CHAR is nil.  Store result in the kill ring."

  ;; if DACHAR is nil, it kills to bol. :)
  (talk-error-thing "Talk-backward-to-char-or-bol")

  (setq tmps "")
  (save-excursion
    ;; first go until non-space or bol
    (while (and (equal (preceding-char) DACHAR)
		(not (bolp)))
      (setq tmps (concat (char-to-string (preceding-char)) tmps))
      (backward-delete-char 1))
    ;; now delete that word until space of bol
    (while (not (or (equal (preceding-char) DACHAR)
		    (bolp) ))
      (setq tmps (concat (char-to-string (preceding-char)) tmps))
      (backward-delete-char 1)))
  (setq kill-ring-yank-pointer (cons tmps kill-ring-yank-pointer))
)

(defun talk-delete-word-backwards-not-past-eob ()
  "Delete a word backwards, but don't go past bol."

  (talk-error-thing "Talk-delete-word-backwards-not-past-eob")

  (if (not (bolp))
    (talk-delete-backward-to-char-or-bol (string-to-char " "))))


(defun talk-delete-word-backwards ()
  "Delete character backwards in local and remote."

  (interactive)
  (talk-error-thing "Talk-delete-word-backwards")

  (talk-send-output (talk-send-where) (char-to-string last-input-char))
  (talk-action-to-local '(talk-delete-word-backwards-not-past-eob)))

(defun talk-delete-line ()
  "Delete line backwards in local and remote."

  (interactive)
  (talk-error-thing "Talk-delete-line")

  (talk-send-output (talk-send-where) (char-to-string last-input-char))
  (talk-action-to-local '(talk-delete-backward-to-char-or-bol nil)))

(defun talk-clear-window ()
  "clears the current window, and places the cursor on the
top line of the window with whatever line was being edited."

  (interactive)
  (talk-error-thing "Talk-clear-window")

  (talk-send-output (talk-send-where) "")
  (setq oldbuffer (current-buffer))
  (set-buffer (get-buffer talk-local-buffer-name))
  (recenter 0)
  (set-buffer oldbuffer)
  (redraw-display))

