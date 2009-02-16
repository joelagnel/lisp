;;; Saved through ges-version 0.3.3dev at 2003-03-19 20:54
;;; ;;; From: Peter Breton <pbreton@attbi.com>
;;; ;;; Subject: run-command.el 1.13
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Wed, 12 Mar 2003 12:27:19 GMT
;;; ;;; Reply-To: peter_breton@yahoo.com
;;; ;;; Organization: AT&T Broadband


;;; Run-command is a library to facilitate running external commands,
;;; operating at a higher level than call-process, start-process, 
;;; make-comint, etc.

;;; In addition to simplicity of use, the library includes many common
;;; facilities:

;;; * Show or hide the output buffer via a variable binding
;;; * Echo the commands which are executed to a buffer
;;; * Save existing output in a command buffer
;;; * Callbacks for asynchronous commands

;;; Some example uses of run-command:

;;; ;; Run a command synchronously
(defun mount-list-mounted-filesystems ()
  "List all mounted filesystems."
  (interactive)
  (run-command "*Mount Information*" "mount"))

;; Run a command asynchronously
(defun xlock ()
  "Run the xlock program."
  (interactive)
  (run-command-asynchronously "*XLock*" "xlock"))

;; Run a command as a comint buffer
(defun fdisk  (device)
  "Run fdisk on a device"
  (interactive "sRun Fdisk on device: ")
  (run-command-as-comint
   (concat "*Fdisk [" device "]*")
   "fdisk" device))
  


(defun nmap (host)
  "Run a network map"
  (interactive "snmap on host: ")
  (run-command-as-comint
   (concat "*nmap [" host "]*")
   "sudo" "nmap" "-sS" "-A"  host))

(defun free ()
  "memory usage"
  (interactive)
  (run-command-asynchronously "*Memory Usage*" "free" "-m"))


(defun procsocklist ()
  "procsocklist"
  (interactive)
  (run-command-asynchronously "*Proc Sock List*" "perl" "/home/ike/repository/scripts/process/procsocklist-1.1.pl"))
  

(defun psmem ()
  "Memory usage"
  (interactive)
  (run-command-asynchronously "*psmem*" "sudo" "python" "/home/ike/repository/scripts/process/ps_mem.py"))
  

(defun psmax ()
  "Memory usage"
  (interactive)
  (run-command-asynchronously "*psmax*" "perl" "/home/ike/repository/scripts/process/psmax"))


(defun sv-status ()
  "sv status"
  (interactive)
  (run-command-asynchronously "*sv status*" "sudo" "sv" "s" "/var/service/*"))


(defun ups-status ()
  "APC UPS Status"
  (interactive)
  (run-command-asynchronously "*UPS Status*" "sudo" "apcaccess"))



;;trial
(defun cpan (package) 
  "Install a CPAN package"
  (interactive "sCPAN module please: ")
  (run-command-as-comint
   (concat "*CPAN package : [" package "]*")
   "sudo" "perl" "-MCPAN" "-e" "install" package))

(defun port-forward (host local-port remote-port) 
  "Forward local-port to remote-port"
  (interactive "sHost: \nsLocal port: \nsRemote Port: ")
  (run-command-as-comint
   (concat "*ssh -L "local-port":"host":"remote-port"*")
   "ssh" "-L" local-port":"host":"remote-port))




;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar run-command-display-output-buffer t
  "If non-nil, display the results of the command in a buffer.")

(defvar run-command-echo-commands nil
  "If non-nil, echo the command lines to `run-command-echo-command-buffer'.")

(defvar run-command-keep-output nil
  "If non-nil, do not delete existing output in a buffer.")

(defvar run-command-echo-command-buffer "*Run Command*"
  "Buffer to echo command lines.")

(defvar run-command-callback nil
  "Callback to invoke when a process completes.
The function will be invoked in the output buffer.")

(make-variable-buffer-local 'run-command-callback)

(defvar run-command-completed nil
  "Non-nil if an asynchronous command has completed.")

(defvar run-command-program nil
  "The program that was run.")

(make-variable-buffer-local 'run-command-program)

(defvar run-command-program-arguments nil
  "Arguments to the program that was run.")

(make-variable-buffer-local 'run-command-program-arguments)

(defvar run-command-type nil
  "The type of command that was run.
One of 'synchronous, 'synchronous-region, 'asynchronous, 'comint")

(make-variable-buffer-local 'run-command-type)

(defvar run-command-history-list nil
  "History list for running commands.")

(defvar run-command-argument-history-list nil
  "History list for run command arguments.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-command-process-sentinel (process state)
  "Runs a callback when PROCESS exits."
  (and (buffer-live-p (process-buffer process))
       (with-current-buffer (process-buffer process)
	 (and (or (string= state "finished\n")
		  (string-match "^exited" state)
		  (string-match "^kill" state)
		  (string-match "^SIG" state))
	      (progn
		(setq run-command-completed t)
		(and run-command-callback
		     (if (listp run-command-callback)
			 (mapcar 'funcall run-command-callback)
		       (funcall run-command-callback))))
	      ))))

;; Internal workhorse function
(defun run-command-internal (buffer program type start end callback-or-delete 
        filter &rest args)
  "In BUFFER, run PROGRAM with ARGS. Return BUFFER.
If BUFFER is t, run program in current buffer.

If type is 'synchronous, the command is run as a synchronous process;
START, END, CALLBACK-OR-DELETE and FILTER are ignored.

If type is 'synchronous-region, the command is run as a synchronous process
on the region defined by START and END. If CALLBACK-OR-DELETE is non-nil, then
the current-region is deleted. FILTER is ignored.

If type is 'comint, the command is run as a comint process;
START, END and CALLBACK-OR-DELETE are ignored. If FILTER is present, it is
used as a comint filter, or list of filters.

If type is 'asynchronous, the command is run as an asynchronous process;
if CALLBACK-OR-DELETE is non-nil, it is invoked when the command completes.
START, END and FILTER are ignored.

The run-command-* functions provide convenience methods for running
commands of the correct type.
"
  (let ((options args)
	(orig-buffer (current-buffer))
	(output-buffer
	 (if (eq buffer 't)
	     (current-buffer)
	   (get-buffer-create buffer)))
	original-filter-functions
	)
    ;; Go to the buffer
    (save-excursion
      (set-buffer output-buffer)
      (cond ((eq buffer 't)
	     )
	    (run-command-keep-output
	     (goto-char (point-max)))
	    (t
	     (erase-buffer))
	    )

      (run-hooks 'run-command-hooks)

      ;; Figure out what to do, process-wise
       (cond

	;; Simple synchronous call
	((eq type 'synchronous)
	 (apply 'call-process program nil t nil args))

       ;; Synchronous using region
       ((eq type 'synchronous-region)
	(with-current-buffer orig-buffer
	    (apply 'call-process-region start end program
		   callback-or-delete
		   output-buffer nil options)))

       ;; Asynchronous
       ((eq type 'asynchronous)
	(setq run-command-callback callback-or-delete)
	(make-local-variable 'run-command-completed)
	(setq run-command-completed nil)
	(set-process-sentinel
	 (apply 'start-process (buffer-name output-buffer)
		output-buffer program options)
	 'run-command-process-sentinel))

       ;; Comint
       ((eq type 'comint)
	(or (get-buffer-process (current-buffer))
	    (progn
	      (require 'comint)
	      (comint-mode)
	      (run-hooks 'run-command-comint-hooks)
	      (comint-exec
	       output-buffer
	       (buffer-name output-buffer)
	       program
	       nil
	       options)
	      (and filter
		   (with-current-buffer output-buffer
		     (setq original-filter-functions
			   comint-output-filter-functions)
		     (make-local-hook 'comint-output-filter-functions)
		     (mapcar
		      (function
		       (lambda(element)
			 (or (member element original-filter-functions)
			     (add-hook
			      'comint-output-filter-functions element t t))
			 )
		       )
		      (if (listp filter) filter (list filter)))
		     )))
	    ))
       (t
	(error "Unknown type %s" type)))

      ;; Keep track of how we were invoked
      (setq run-command-program           program)
      (setq run-command-program-arguments options)
      (setq run-command-type              type)

      ;; Show the status of the process
      (and (member type (list 'asynchronous 'comint))
	   (setq mode-line-process '(":%s")))

      ;; Log the invokation
      (and run-command-echo-commands
	   (with-current-buffer
	       (get-buffer-create run-command-echo-command-buffer)
	     (goto-char (point-max))
	     (insert
	      "[Command Line: \""
	      (mapconcat 'identity
			 (append (list program) options) " ")
	      "\"]\n")))

      ;; Display the output
      (and run-command-display-output-buffer
	   (progn
	     (goto-char (point-min))
	     (display-buffer output-buffer)))

      ;; Return the output buffer
      output-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst run-command-read-arguments  (buf)
  "Read the arguments for a run command interactive call.
Use BUF as buffer name."
  (let (arguments)
    (apply
     'list
     (read-buffer "Run command in buffer: " buf)
     (read-from-minibuffer "Run command (no arguments): " nil nil nil 
        'run-command-history-list)
     (progn
       (setq arguments
	     (read-from-minibuffer "With arguments: " nil nil nil
				   'run-command-argument-history-list))
       (and arguments (split-string arguments))))))

;;;###autoload
(defun run-command (buffer program &rest args)
  "In BUFFER, run PROGRAM with ARGS."
  (interactive
   (run-command-read-arguments "*Run Command*"))
  (apply 'run-command-internal buffer program 'synchronous nil nil nil 
        nil args))

;;;###autoload
(defun run-command-region (start end delete buffer program &rest args)
  "In BUFFER, run PROGRAM on region with ARGS.
If DELETE is non-nil, then delete the current region.
Interactively, delete if the prefix arg is non-nil."
  (interactive
   (apply 'list
    (region-beginning)
    (region-end)
    current-prefix-arg
    (run-command-read-arguments "*Run Command Region*")
    ))
   (apply 'run-command-internal buffer program 'synchronous-region start end
	  delete nil args))

;;;###autoload
(defun run-command-as-comint (buffer program &rest args)
  "In BUFFER, run PROGRAM with ARGS as a comint process."
  (interactive
   (run-command-read-arguments "*Run Command Comint*"))
  (if (get-buffer buffer)
      (switch-to-buffer buffer)
  (apply 'run-command-internal buffer program 'comint nil nil 
        nil nil args)))

;;;###autoload
(defun run-command-as-comint-with-filter (buffer program filter &rest args)
  "In BUFFER, run PROGRAM with ARGS as a comint process.
If optional argument FILTER is present, use as comint filter function."
  (apply 'run-command-internal buffer program 'comint nil nil 
        nil filter args))

;;;###autoload
(defun run-command-asynchronously (buffer program &rest args)
  "In BUFFER, run PROGRAM asynchronously with ARGS."
  (interactive
   (run-command-read-arguments "*Run Command Asynchronously*"))
  (apply 'run-command-internal buffer program 'asynchronous
	 nil nil nil nil args))

;;;###autoload
(defun run-command-asynchronously-with-callback (buffer program 
        &optional callback &rest args)
  "In BUFFER, run PROGRAM asynchronously with ARGS.
When the process finishes, invoke CALLBACK if non-nil."
  (apply 'run-command-internal buffer program 'asynchronous
	 nil nil callback nil args))

(defmacro run-command-without-displaying-output-buffer (&rest body)
  "Perform body without displaying any run-command output buffers."
  (`
   (let ((run-command-display-output-buffer nil))
     (,@ body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-command-add-callback (buffer callback)
  "Add CALLBACK to buffer."
  (with-current-buffer buffer
    (setq run-command-callback
	  (if (not run-command-callback)
	      callback
	    (if (listp run-command-callback)
		(append run-command-callback (list callback))
	      (list run-command-callback callback))))))

(defmacro run-command-after (buffer &rest body)
  "Run BODY after the command in BUFFER completes."
     `(run-command-add-callback
       ,buffer
	(lambda()
	  ,@ body)))

(provide 'run-command)

;;; run-command.el ends here

