;;; -*- Mode: Lisp -*-

;;; sbcl.lisp --
;;;
;;; This init file was last tested with SBCL 0.6.13 and
;;; SBCL 0.7pre.71

;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: sbcl.lisp,v 1.11 2003/12/28 22:50:56 rgrjr Exp $


(in-package :ilisp)

(ignore-errors (require :sb-introspect))

;; ILISP-specifics for SBCL. Since version 0.7 introduced lots of changes,
;; e.g.(bytecode-)interpreter goes away, and lots of other 'renaming'-changes,
;; take care of that, by testing via the 'magic'-macros:
;; THE-SYMBOL-IF-DEFINED, and THE-FUNCTION-IF-DEFINED.
;;
;; MNA: 2001-10-20
;; Some annotations:
;; <1> - interpreter related changes (interpreter missing in sbcl-0.7.x)
;; <2> - byte-compiler related changes (sbcl-0.7.x)
;; <3> - renamings in sbcl-0.7.x., where in general this is accounted for
;;       using THE-SYMBOL-IF-DEFINED and THE-FUNCTION-IF-DEFINED macros.
;;       In general, the "new" symbol comes before the "old" symbol.

;;;% CMU CL does not define defun as a macro
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (ilisp-errors
   (ilisp-eval
    (format nil "(funcall (compile nil '(lambda () ~A)))" form)
    package filename)))

;;;% Stream settings, when running connected to pipes.
;;;
;;; When SBCL is running as a piped process, it still manages to open
;;; /dev/tty to use it for the *terminal-io* stream.  This means that an
;;; error will cause lisp to stop and wait for input from /dev/tty, which is
;;; probably not available and certainly not what you were expecting.
;;;
;;; We want it to use the same input that the user is typing at, ie,
;;; the pipe (stdin).

(defvar *Fix-pipe-streams* T
  "Set to Nil if you want them left alone.  And tell me you don't get stuck.")

(when (and *Fix-pipe-streams*
	   (sb-impl::synonym-stream-p *terminal-io*)
	   (eq (sb-impl::synonym-stream-symbol *terminal-io*)
	       'sb-impl::*tty*))
  (setf *terminal-io* (make-two-way-stream sb-impl::*stdin* sb-impl::*stdout*))
  ;; *query-io* and *debug-io* are synonym streams to this, so this fixes
  ;; everything.
  )

;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.

(setq sb-debug:*flush-debug-errors* nil)  ; allow multiple error levels.

;;; 2000-04-02: Martin Atzmueller
;;; better (more bulletproof) arglist code adapted from cmulisp.lisp:

(defun massage-arglist (args)
  (typecase args
    (string args)
    (cons (let ((*print-pretty* t)
		(*print-escape* t)
		(*print-base* 10)
		(*print-radix* nil))
	    (format nil "~A" args)))
    (null "()")
    (t "")))

(defun arglist (symbol-name package)
  (ilisp-errors
   (let* ((package-name (if (packagep package)
                            (package-name package)
                          package))
	  (symbol (ilisp-find-symbol symbol-name package-name)))
     (flet ()
       (multiple-value-bind (func kind)
	   (extract-function-info-from-name symbol)
	 (if (and func kind)
             ;; Instruments of darkness
             (macrolet ((madness ()
                          (let ((function-arglist-sym
                                 (maybe-function '#:function-arglist '#:sb-introspect)))
                            (if function-arglist-sym
                                `(massage-arglist
                                  (,function-arglist-sym func))
                              `(case (the-function-if-defined
                                      ((#:widetag-of :sb-impl)
                                       (#:get-type :sb-impl))
                                      func)
                                (,(symbol-value
				   (the-symbol-if-defined
				    ((#:simple-fun-header-widetag :sb-vm)
				     (#:function-header-type :sb-vm))))
                                 (massage-arglist
                                  (the-function-if-defined
                                   ((#:%simple-fun-arglist :sb-impl)
                                    (#:%function-arglist :sb-impl))
                                   func)))
				((,(symbol-value
				    (the-symbol-if-defined
				     ((#:closure-header-widetag :sb-vm)
				      (#:closure-header-type :sb-vm))))
                                  ,(symbol-value
                                    (the-symbol-if-defined
                                     ((#:closure-fun-header-widetag :sb-vm)
                                      (#:closure-function-header-type :sb-vm)))))
                                 (massage-arglist
                                  (the-function-if-defined
                                   ((#:%simple-fun-arglist :sb-impl)
                                    (#:%function-arglist :sb-impl))
                                   (the-function-if-defined
				    ((#:%closure-fun :sb-impl) ())
				    func))))
                                (,(symbol-value
                                   (the-symbol-if-defined
                                    ((#:funcallable-instance-header-widetag :sb-vm)
                                     (#:funcallable-instance-header-type :sb-vm))))
                                 (typecase func
                                   (,(the-symbol-if-defined
                                      ((#:byte-function :sb-kernel) ()))
                                    "Byte compiled function or macro, no arglist available.")
                                   (,(the-symbol-if-defined
                                      ((#:byte-closure :sb-kernel) ()))
                                    "Byte compiled closure, no arglist available.")
                                   ((or generic-function sb-pcl::generic-function)
                                    (sb-pcl::generic-function-pretty-arglist func))
                                   (,(the-symbol-if-defined
                                      ((#:interpreted-function :sb-eval) ()))
                                    (the-function-if-defined
                                     ((#:interpreted-function-arglist :sb-eval) ()
                                      :function-binding-p t)
                                     (massage-arglist (funcall the-function func))))
                                   (t (print 99 *trace-output*)
                                      "No arglist available.")))
                                (t "No arglist available."))))))
               (madness))               ; 
           "Unknown function - no arglist available." ; For the time
					; being I just
					; return this
					; value. Maybe
					; an error would
					; be better.
           ))))))

(defun sbcl-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

;;; end of file -- sbcl.lisp --

