;;;
;;; Process control
;;;

(defvar el4r-process nil
  "Process handle to currently running el4r Server, if any.")

(defun el4r-attach-process (process)
  (setq el4r-process process))

(defun el4r-process-status ()
  "Returns process status as defined by `process-status'."
  ;; `pocessp' check is to keep `process-status' from using the current
  ;; buffer if el4r-process is nil or otherwise broken
  (and (processp el4r-process)
       (process-status el4r-process)))

(defun el4r-is-alive-p ()
  "Returns non-nil if Ruby process is alive."
  (eq (el4r-process-status) 'run))
(defalias 'el4r-running-p 'el4r-is-alive-p)

(defun el4r-kill-process ()
  "You likely want to use `el4r-shutdown' instead."
  (process-send-signal 'SIGTERM el4r-process)
  (setq el4r-process nil))

(defun el4r-process-name ()
  "Name of process."
  (process-name el4r-process))

(provide 'el4r-process)