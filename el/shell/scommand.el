;Date: Sat, 4 Mar 89 03:30:48 EST
;From: Theodore Ts'o <tytso@athena.mit.edu>
;To: otter!ange@hplabs.hp.com
;Cc: info-gnu-emacs@prep.ai.mit.edu
;Subject: [otter!ange@hplabs.hp.com: Help: Process Filters]
;
;>Subject: Help: Process Filters
;>Date: 2 Mar 89 19:05:49 GMT
;>From: otter!ange@hplabs.hp.com  (Andy Norman)
;
;>I have a process created by start-process that is producing output at random
;>intervals.
;
;>Whenever new output occurs, I want the most recent bit of the buffer made
;>visible, i.e. I want to see all the output from the process as it happens.
;
;>Can anyone suggest a better filter that either does what I want in a cleaner
;>way, or that doesn't suffer from the minibuffer popping up?
;
;Try the following:  it's a replacement for shell-command, which I call
;scommand (I can't replace it with shell-command since it'll break
;packages that expect to use it.)  What it does is that it runs a shell
;program in the background, and output appears, it outputs it to a
;minibuffer.  It's fairly robust and as far as I know, never does thing
;wrong thing we regards to minibufers popping up or other gruesome
;problems.  The other win is that if you change buffers into the shell
;output window and hit the delete key, the window disappears and the
;buffer goes back to the it original configuration, more or less.
;
;Parts of this code were originally written by Mark Eichin for
;his "zwgc" mode, which was used to pop up notifications in real-time
;from Zephyr, which is also used by users to send messages to each other.
;When you hit the delete key in the buffer, it would automatically delete
;the "top-most" message and display the second most recent message.  When
;all of the messages had been deleted, it would make the *zwgc* window
;disappear.  If this is close to what you had in mind, let me know and I
;can send you that.  I'm posting scommand instead, since it probably can
;be used by more people.

;;;
;;; shell-command.el -- a new and improved shell-command
;;;
;;; This version of shell-command runs the shell-command in the background
;;; and "pops-up" a window as the program displays its output.
;;;
;;;	$Header:$
;;;	$Source:$
;;;	$Author:$
;;;

(defun scommand (command &optional flag)
  "Execute string COMMAND in inferior shell; display output, if any.
Optional second arg non-nil (prefix arg, if interactive)
means insert output in current buffer after point (leave mark after it)."
  (interactive "sShell command: \nP")
  (if flag
      (progn (barf-if-buffer-read-only)
	     (push-mark)
	     (call-process shell-file-name nil t nil
			   "-c" command)
	     (exchange-point-and-mark))
    (let ((buffer (get-buffer-create "*shell-command*")) proc status)
      (setq proc (get-buffer-process buffer))
      (if proc
	  (if (yes-or-no-p (concat "'" 
			   (car (cdr (cdr (process-command proc))))
			   "' in progress.  Kill it? "))
	      (progn
		(kill-process proc)
		(sit-for 1)
		(setq buffer (get-buffer-create "*shell-command*")))
	    (error "Shell command in progress")))
      (set-buffer buffer)
      (erase-buffer)
      (setq proc (start-process "Shell" buffer 
				shell-file-name "-c" command))
      (message "Starting %s." command)
      (setq mode-name "ShellC")
      (setq mode-line-process '(": %s"))
      (set-process-filter proc 'shell-command-return)
      (set-process-sentinel proc 'shell-command-sentinel)
      (local-set-key "" 'shell-command-punt)
      (local-set-key " " 'scroll-up)
      (make-local-variable 'shell-command-punted)
      (setq shell-command-punted nil)
      )))

(defun shell-command-sentinel (process signal)
  (let ((buffer (process-buffer process)))
    (if (memq (process-status process) '(exit signal))
	(progn
	  (message "%s: %s." 
		   (car (cdr (cdr (process-command process))))
		   (substring signal 0 -1))
	  (if (buffer-name buffer)
	      (progn
		(set-buffer buffer)
		(if shell-command-punted
		    (progn
		      (delete-windows-on buffer)
		      (kill-buffer buffer)))))
	  (delete-process process)))))

(defun shell-command-punt ()
  (interactive)
  (let* ((buffer (current-buffer))
	 (proc (get-buffer-process buffer)))
    (setq shell-command-punted t)
    (delete-windows-on buffer)
    (if (or (not proc)
	    (not (process-status proc)))
	(kill-buffer buffer)
      (bury-buffer buffer))))

(defun shell-command-return (proc string)
  (set-buffer (process-buffer proc))
  (save-excursion
    (goto-char (point-max))
    (insert string)
    (Special-pop-up-window (process-buffer proc))))

;; Taken from zwgc.el

(defun Special-pop-up-window (buffer &optional max-height)
  (interactive "bBuffer to pop: ")
  (let* ((pop-up-windows t)
	 (window-min-height 1))
    (pop-to-buffer buffer)
    (if (not (one-window-p t))
	(progn
	  (setq lines (1+ (count-lines (point-min) (point-max))))
	  (enlarge-window (- lines (window-height (selected-window))))
	  (goto-char (point-min))
	  (other-window 1)))
    ))

