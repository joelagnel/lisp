; process filter taken from finger.el by 
; krulwich@ils.nwu.edu (Bruce Krulwich)
; -Calvin Clark [ckclark:19911205.1646EST]

(defun expn (arg)
  "Prompts user for name to expand at mail hub.  If provided with
numerical arg, prompts for alternate mailhub to contact (default is
mit.edu). Displays results an another buffer when done."
  (interactive "P")
  (let ((who (read-from-minibuffer "name to expand: "))
	(p 
	 (open-network-stream 
	  "expn" "*expn-output*" 
	  (if arg (read-from-minibuffer "mail hub to contact: ") "mit.edu")
	  "smtp")))
    (set-buffer (process-buffer p))
    (erase-buffer)
    (set-process-filter
     p 
     '(lambda (proc s)
	(save-excursion
	  (set-buffer (process-buffer proc))
	  (while (< 0 (length s))
	    (let ((l (string-match "\r" s)))
	      (insert (substring s 0 l))
	      (setq s (cond (l (substring s (1+ l)))
			    (t ""))))))))
    (set-process-sentinel
     p 
     '(lambda (proc s)
	(display-buffer (process-buffer proc))))
    (process-send-string p (concat "expn " who "\r\n"))
    (process-send-string p "quit\r\n")))
