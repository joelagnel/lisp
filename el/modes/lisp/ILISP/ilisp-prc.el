;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-prc.el --
;;; ILISP process handling.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-prc.el,v 1.4 2003/07/19 01:28:52 rgrjr Exp $


(defun ilisp-process ()
  "Return the current ILISP process."
  (get-buffer-process (ilisp-buffer)))


(defvar ilisp-buffer-function 'ilisp-recent-buffer
  "A function of no arguments which returns the current ilisp buffer")


;;;%Buffer and process selection
(defun ilisp-buffer ()
  "Return the current ILISP buffer.
This is the buffer to whose process requests are sent."
  (if (memq major-mode ilisp-modes)
      (current-buffer)
    (let ((buffer (funcall ilisp-buffer-function)))
      (or buffer
	  (error "You must start an inferior LISP with run-ilisp.")))))


(defun ilisp-recent-buffer ()
  "Return the most-recently selected ilisp buffer." 
  (if ilisp-buffer 
      (or (get-buffer ilisp-buffer)
	  (get-buffer
	   (setq ilisp-buffers
		 (delete* (substring ilisp-buffer
				     1 
				     (1- (length ilisp-buffer)))
			  ilisp-buffers 
			  :test (function (lambda (s1 s2)
					    (string= s1 (car s2)))))
		 ilisp-buffer
		 (format "*%s*" (car (car ilisp-buffers))))))))

;;;
(defun ilisp-default-ilisp-buffer ()
  ;; Helper for select-ilisp, returns the current buffer name iff it's a dialect
  ;; buffer, else the most-recently-created dialect buffer name that is NOT
  ;; current, else the most-recently-created (in which case there should only be
  ;; one).  Returns nil if no legal candidates exist (though it is still
  ;; possible that the named default buffer might not exist).  [Some LRU
  ;; reordering and/or garbage collection of the ilisp-buffers list would be a
  ;; really good idea, though.  -- rgr, 22-Sep-02.]
  (or (let ((name (buffer-name)))
	(and (string-match "^\\*\\(.*\\)\\*$" name)
	     (member* (match-string 1 name) ilisp-buffers
		      :test (function (lambda (x y)
			      (equal x (car y)))))
	     name))
      (let ((tail (or (and ilisp-buffer
			   (member* (substring ilisp-buffer 1 -1) ilisp-buffers
				    :test (function (lambda (x y)
					    (not (equal x (car y)))))))
		      ;; take the most-recently-created member of ilisp-buffers
		      ;; as the default default.
		      ilisp-buffers)))
	(if tail
	    (concat "*" (car (car tail)) "*")))))

(defun select-ilisp (new-ilisp-buffer)
  "Select a new ILISP dialect buffer.
Prompts for the name of an ILISP dialect; the default is the current one
if in a dialect buffer, else the most-recently-created dialect (other
than the current dialect, if there is more than one).

The selected ILISP dialect is where ILISP sends all eval/compile/query
requests, and is kept in the `ilisp-buffer' emacs lisp variable.  Note
that the actual dialect buffer name is stored there, e.g. \"*cmulisp*\",
even though the user interface requests the dialect name, e.g. \"cmulisp\"."
  (interactive
    (let* ((default
	     (or (ilisp-default-ilisp-buffer)
		 (error "No ILISP buffers; do M-x run-lisp to create one.")))
	   (prompt (format "Buffer [%sdefault %s]: "
			   (if ilisp-buffer
			       (format "currently %s, "
				       (substring ilisp-buffer 1 -1))
			       "")
			   (substring default 1 -1)))
	   (new (completing-read prompt ilisp-buffers nil t)))
      (list (if (zerop (length new))
		default
		(format "*%s*" new)))))
  (cond ((not (stringp new-ilisp-buffer))
	  (error "bug: %S is not a string." new-ilisp-buffer))
	((not (get-buffer new-ilisp-buffer))
	  ;; [should eliminate this before the completing-read call.  -- rgr,
	  ;; 22-Sep-02.]
	  (error "Oops; buffer %S no longer exists." new-ilisp-buffer))
	(t
	  ;; flush M-. cache first.  [it might be cleaner to cache based on the
	  ;; dialect as well, but then we'd also have to provide a separate
	  ;; command to flush the cache explicitly.  -- rgr, 18-Jul-03.]
	  (setq lisp-inferior-source-definitions-cache nil)
	  ;; install new selected dialect.
	  (setq ilisp-buffer new-ilisp-buffer)
	  (message "Selecting %s as the current ILISP dialect buffer."
		   (substring new-ilisp-buffer 1 -1)))))

;;; end of file -- ilisp-prc.el --
