(defun call-process-with-error (command buffer &optional noerror)
  "Call the shell-command in COMMAND in a separate process.
Insert output in BUFFER before point; t means current buffer;
nil for BUFFER means discard it.
Optional fourth arg NOERROR non-nil means don't raise an error but
silently return nil.
This function waits for COMMAND to terminate; if you quit, the process
is killed.
On succesful termination of COMMAND t is returned.
If the command returns a non-zero exit status and NOERROR is not given
or nil, the command output is displayed in a separate window and an 
error is raised.
The implementation is a weird hack since the exit status of the
inferior process is tested by a shell, which inserts a message 
into a buffer.
Based on code from levin@eos.ncsu.edu (Dr. Hal Levin)."
  (let* ((old-buffer (current-buffer))
	 (process-error-string "Process terminated with error")
	 (process-ok-string "OK")
	 (bufname "*Process Error*")
	 (error-buffer (get-buffer-create bufname)))
    (set-buffer error-buffer)
    (erase-buffer)
    (call-process "sh" nil error-buffer nil
		  "-c" (concat command 
			       ";if test $? != 0\nthen\necho "
			       process-error-string
			       "\nelse\necho "
			       process-ok-string
			       "\nfi"))
    (if (string-match (concat process-error-string "\n$")
		      (buffer-string))
	(if noerror 
	    nil
	  (progn
	    (goto-char (point-max))
	    (pop-to-buffer error-buffer)
	    (other-window 1)
	    (error (substitute-command-keys 
		    "Type \\[delete-other-windows] to remove error window"))))
      (if buffer
	  (progn
	    (kill-region (1+ (string-match (concat process-ok-string "\n$")
					   (buffer-string)))
			 (point-max))
	    (let ((contents (buffer-string)))
	      (set-buffer (if (eq buffer 't)
			      old-buffer
			    buffer))
	      (insert contents))))
      (set-buffer old-buffer)
      (kill-buffer error-buffer)
      t)))
