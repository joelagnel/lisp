;;; -*- Mode: Lisp -*-

;;; cmulisp.lisp --
;;; ILISP CMU Common Lisp dialect support definitions.
;;; Author: Todd Kaufmann    May 1990
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: cmulisp.lisp,v 1.15 2003/05/14 20:00:58 kevinrosenberg Exp $


(in-package :ilisp)

;;;% Stream settings, when running connected to pipes.
;;;
;;; This fixes a problem when running piped: When CMU is running as a piped
;;; process, *terminal-io* really is a terminal; ie, /dev/tty.  This means an
;;; error will cause lisp to stop and wait for input from /dev/tty, which it
;;; won't be able to grab, and you'll have to restart your lisp.  But we want
;;; it to use the same input that the user is typing in, ie, the pipe (stdin).
;;; This fixes that problem, which only occurs in the CMU cores of this year.
;;;

(defvar *Fix-pipe-streams* t
  "Set to Nil if you want them left alone.  And tell me you don't get stuck.")

(when (and *Fix-pipe-streams*
	   (lisp::synonym-stream-p *terminal-io*)
	   (eq (lisp::synonym-stream-symbol *terminal-io*)
	       'system::*tty*))
  (setf *terminal-io* (make-two-way-stream system::*stdin* system::*stdout*))
  ;; *query-io* and *debug-io* are synonym streams to this, so this fixes
  ;; everything.
  )

;;;% Debugger extensions

;;;%% Implementation of a :pop command for CMU CL debugger

;;;
;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.
;;;
(setq debug:*flush-debug-errors* nil)  ; allow multiple error levels.

;;; This implementation of "POP" simply looks for the first restart that says
;;; "Return to debug level n" or "Return to top level." and executes it.
;;;
(debug::def-debug-command "POP" ()
  ;; find the first "Return to ..." restart
  (if (not (boundp 'debug::*debug-restarts*))
      (error "You're not in the debugger; how can you call this!?")
      (labels ((find-return-to (restart-list num)
		 (let ((first
			(member-if
			 #'(lambda (restart)
			     (string=
                              (with-output-to-string (s)
			        (funcall
			         (conditions::restart-report-function restart)
			         s))
			      "Return to " :end1 10))
			 restart-list)))
		   (cond ((zerop num) (car first))
			 ((cdr first)
			  (find-return-to (cdr first) (1- num)))))))
	(let* ((level (debug::read-if-available 1))
	       (first-return-to (find-return-to 
				 debug::*debug-restarts* (1- level))))
	  (if (null first-return-to)
	      (format *debug-io* "pop: ~d is too far" level)
	      (debug::invoke-restart-interactively first-return-to)
	      ))))
    )


;;;%% arglist/source-file utils.

(defun get-correct-fn-object (sym)
  "Deduce how to get the \"right\" function object and return it."
  (let ((fun (or (macro-function sym)
		 (and (fboundp sym) (symbol-function sym)))))
    (unless fun
      (error "Unknown function ~a.  Check package." sym))

    (if (and (= (lisp::get-type fun) #.vm:closure-header-type)
	     (not (eval:interpreted-function-p fun)))
	(lisp::%closure-function fun)
	fun)))

;;;%% arglist - return arglist of function
;;;
;;; This function is patterned after DESCRIBE-FUNCTION in the
;;; 'describe.lisp' file of CMUCL.

(defun arglist (symbol package)
  (ilisp-errors
   (let ((x (if (symbolp symbol)
		symbol
		(ilisp-find-symbol symbol
				   (if (packagep package)
				       (package-name package)
				       package)))))
     (flet ((massage-arglist (args)
	      (typecase args
		(string args)
		(null "()")
		(t (format nil "~S" args)))))

       (multiple-value-bind (func kind)
	   (extract-function-info-from-name x)
	 ;; (print func *trace-output*)
	 ;; (print kind *trace-output*)
	 (if (and func kind)
	     (case (lisp::get-type func)
	       ((#.vm:closure-header-type
		 #.vm:function-header-type
		 #.vm:closure-function-header-type)
		(massage-arglist
                 (the-function-if-defined
                  ((#:%function-arglist :lisp) (#:%function-header-arglist :lisp))
                  func)))
	       (#.vm:funcallable-instance-header-type
		(typecase func
		  (kernel:byte-function
		   "Byte compiled function or macro, no arglist available.")
		  (kernel:byte-closure
		   "Byte compiled closure, no arglist available.")
		  ((or generic-function pcl:generic-function)
		   (generic-function-pretty-arglist func))
		  (eval:interpreted-function
		   (massage-arglist (eval::interpreted-function-arglist func)))
		
		  (t (print 99 *trace-output*) "No arglist available.")))
	       (t "No arglist available."))
	     "Unknown function - no arglist available." ; For the time
					; being I just
					; return this
					; value. Maybe
					; an error would
					; be better.
	     ))))))

(defun cmulisp-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

(defun ilisp-callers (name package)
  #-(or cmu18e cmu19) (declare (ignore name package))
  (ilisp-errors
    ;; [this reader conditionalization strategy fails if there's a cmu18f
    ;; release.  -- rgr, 11-Apr-03.]
    #-(or cmu18e cmu19)
    (error "Finding callers is not supported in this version of CMUCL.")
    #+(or cmu18e cmu19)
    (let ((symbol (ilisp-find-symbol name package))
	  (callers nil))
      (unless symbol
	(error "No such symbol '~A' in package '~A'." name package))
      (dolist (caller (xref:who-calls symbol))
	(let ((caller-name (xref:xref-context-name caller)))
	  (when (and (consp caller-name)
		     (eq (car caller-name) :method))
	    ;; standardize method name syntax.
	    (setq caller-name (cons 'method (cdr caller-name))))
	  ;; must use pushnew, because the current release doesn't correctly
	  ;; flush old definitions.  -- rgr, 11-Apr-03.
	  (pushnew caller-name callers :test #'equal)))
      ;; print callers afterwards, to minimize GC messages interference.  bind
      ;; *package* so that all symbols are printed with a suitable prefix.
      (let ((*package* (find-package :ilisp))
	    (*print-pretty* nil) (*print-circle* nil))
	(dolist (caller callers)
	  (print caller)))
      t)))

(defun generic-function-pretty-arglist (gf)
  (if (fboundp 'pcl::generic-function-lambda-list)
      (pcl::generic-function-lambda-list gf))
  (mop:generic-function-lambda-list gf))


;;; end of file -- cmulisp.lisp --
