;;;; sysadmin.el -- useful stuff for running system
;;; Time-stamp: <2003-01-06 09:28:59 jcgs>

(provide 'dmesg)

(defvar dmesg-buffer "*System messages*"
  "The name of the system messages buffer")

(defun dmesg ()
  "Run dmesg and display the results."
  (interactive)
  (set-buffer (get-buffer-create dmesg-buffer))
  (erase-buffer)
  (let ((dmesg-process (start-process "dmesg" dmesg-buffer "dmesg")))
    (set-process-sentinel dmesg-process (function (lambda (process state)
						    (message "Process %s %s" process state))))
    (pop-to-buffer dmesg-buffer)))

  
