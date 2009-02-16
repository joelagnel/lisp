;;;
;;; Basic logging
;;;

(require 'el4r-support)

(defun el4r-timestamp ()
  (format-time-string "%j-%H:%M:%S"))

(defun el4r-log-buffer ()
  (or (get-buffer el4r-log-bufname)
      (let ((log-buffer (get-buffer-create el4r-log-bufname)))
	(with-current-buffer log-buffer
	  (setq buffer-read-only t)
	  (buffer-disable-undo)
	  (let (el4r-timestamp-function)
	    (el4r-log 'unnamed "** LOG CREATED %s ***\n" (el4r-timestamp))))
	log-buffer)))

(defconst el4r-timestamp-function 'el4r-timestamp
  "If non-nil, timestamps are enabled in logs. Can be set to a
  custom function.")
(defconst el4r-log-level-names
  '((error . "ERR!") (warning . "WARN") (info . "INFO") (debug . "DEBG"))
  "Alist of text to prefix log-level messages with.")
(defconst el4r-echo-levels
  '(error warning)
  "List of log-levels which will also go to the echo area")

(defun el4r-log-level-name (level)
  "Returns a print-name for the log level LEVEL or nil if none is
found. See `el4r-log-level-names'."
  (cdr (assq level el4r-log-level-names)))

(defun el4r-log (level message &rest rest)
  "Log something.
LEVEL is the log level. Should be something like `error',
`warning', `info' or `debug'. MESSAGE and REST are used as used
as arguments to `format'."
  (let* ((level-name (el4r-log-level-name level))
	 (formatted-message (apply 'format message format-args))
	 (log-message (if level-name 
			  (concat level-name ": " formatted-message)
			formatted-message))
	 (max-echo-size (- (window-width) 8)))
    ;; some levels also trigger an echo-message
    (when (memq level el4r-echo-levels)
      (message (truncate-string-to-width log-message max-echo-size)))
    ;; timestamps don't show up in messages
    (if el4r-timestamp-function
	(setq log-message 
	      (concat (funcall el4r-timestamp-function) ": " log-message)))
    (with-current-buffer (el4r-log-buffer)
      (let (buffer-read-only)
	(buffer-puts-tail (el4r-log-buffer) log-message)))
    (redisplay t) ;; force immediate update
    ))

(defun el4r-log-info (message &rest format-args)
  (apply 'el4r-log 'info message format-args))
(defun el4r-log-debug (message &rest format-args)
  (apply 'el4r-log 'debug message format-args))
(defun el4r-log-warn (message &rest format-args)
  (apply 'el4r-log 'warning message format-args))
(defun el4r-log-error (message &rest format-args)
  (apply 'el4r-log 'error message format-args))


(defun append-string-to-buffer (buffer string)
  "Append a STRING to BUFFER. Does not move point."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(defun add-smart-newline (string)
  "Adds a \n to TEXT if it does not currently end in one"
  (if (string-match "\n" string)
      string
    (concat string "\n")))

(defvar buffer-puts-tail-mode 'always
  "What to do after `buffer-puts'. Values: `always' - always move
to end of window. `maybe' - move to the end of the window if the
window-point was at the end prior to the function. `never' -
nothing is done on nil.")


(provide 'el4r-log)