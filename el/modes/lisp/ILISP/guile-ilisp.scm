;;;; guile-ilisp.scm --- ILISP support functions for GUILE
;;;; Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de> 
;;;
;;; Copyright (C) 2000, 2001, 2002, 2003, 2004 Matthias Koeppe
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: guile-ilisp.scm,v 1.25 2004/08/02 10:14:54 kevinrosenberg Exp $


(define-module (guile-ilisp)
  :use-module (ice-9 debug)
  :use-module (ice-9 session)
  :use-module (ice-9 documentation)
  :use-module (ice-9 regex)
  :use-module (oop goops))

;; This file tries hard to be compatible with Guile versions
;; 1.3.4 through 1.8.x

(define eval-in-package
  ;; A two-argument version of `eval'
  (if (= (car (procedure-property eval 'arity)) 2)
      (lambda (expression environment)	; we have a R5RS eval
	(save-module-excursion
	 (lambda ()
	   (eval expression environment))))
      (lambda (expression environment)	; we have a one-arg eval (Guile <= 1.4)
	(save-module-excursion
	 (lambda ()
	   (set-current-module environment)
	   (eval expression))))))  

(define is-a-generic?
  (if (false-if-exception (eval-in-package '(use-modules (oop goops))
					   (current-module)))
      (lambda (object)
	(is-a? object <generic>))
      (lambda (object)
	#f)))

(define (read-from-string str)
  (call-with-input-string str read))

(define (read-from-string/source str filename line column)
  "Read from string STR, pretending the source is the given FILENAME, LINE, COLUMN."
  (call-with-input-string
   str
   (lambda (port)
     (set-port-filename! port filename)
     (set-port-line! port (- line 1))
     (set-port-column! port (- column 1))
     (read port))))

(define (string->module str)
  (let ((v (call-with-input-string str read)))
    (cond
     ((eq? 'nil v) (current-module))
     ((pair? v) (resolve-module v))
     (else (resolve-module (list v))))))

(define (first-line s)
  (let ((i (string-index s #\newline)))
    (if i
	(substring s 0 i)
	s)))

(define (doc->arglist doc with-procedure?)
  "Parse DOC to find the arglist and return it as a string.  If
WITH-PROCEDURE?, include the procedure symbol."
  (letrec ((texinfo-doc->arglist
	    ;; Guile >= 1.4.1 primitive procedure documentation, passed through
	    ;; TeXinfo:
	    ;;
	    ;;  - primitive: assoc key alist
	    ;;     Behaves like `assq' but uses `equal?' for key comparison.
	    ;;
	    ;; Newer versions of Guile (or is it TeXinfo?) use "Scheme
	    ;; Procedure" rather than "primitive".
	    ;;
	    ;; Continuation lines of arglists have an indentation of
	    ;; 10 chars.
	    (lambda (pattern)
	      (and (>= (string-length doc)
		       (string-length pattern))
		   (string=? (substring doc 0 (string-length pattern))
			     pattern)
		   (let ((start-index
			  (if with-procedure?
			      (string-length pattern)
			      (min (1+ (or (string-index doc #\space
							 (string-length pattern))
					   (string-length pattern)))
				   (or (string-index doc #\newline
						     (string-length pattern))
				       (string-length pattern))))))
		     (let ((eol-index (or (string-index doc #\newline start-index)
					  (string-length doc))))
		       (string-append 
			"("
			(let loop ((bol-index (+ 1 eol-index))
				   (arglist (substring doc start-index eol-index)))
			  (cond 
			   ((and bol-index (>= bol-index (string-length doc)))
			    arglist)
			   ((and (>= (string-length doc) (+ bol-index 10))
				 (string=? (substring doc bol-index (+ bol-index 10))
					   "          "))
			    (let ((eol-index (string-index doc #\newline bol-index)))
			      (loop (and eol-index (+ 1 eol-index))
				    (string-append arglist " " 
						   (substring doc (+ bol-index 10)
							      eol-index)))))
			   (else
			    arglist)))
			")")))))))
    (cond
     ((texinfo-doc->arglist " - primitive: "))
     ((texinfo-doc->arglist " - Scheme Procedure: "))
     ((string=? (substring doc 0 1) "(")
      ;; Guile <= 1.4 primitive procedure documentation and other
      ;; conventions:
      ;;
      ;; (help [NAME])
      ;; Prints useful information.  Try `(help)'.
      ;;
      (if with-procedure?
	  (first-line doc)
	  (let* ((f-l (first-line doc))
		 (index (string-index f-l #\space)))
	    (if index
		(string-append "("
			       (substring f-l
					  (+ index 1)))
		"()"))))     
     (else (string-append "CAN'T PARSE THIS DOCUMENTATION:\n"
			  doc)))))

(define (write-truncated obj port max-length)
  "Write OBJ on PORT, truncated to MAX-LENGTH characters.  Indicate
truncation by an ellipsis."
  (call-with-current-continuation
   (lambda (return)
     (let ((trunc-port
	    (make-soft-port
	     (vector (lambda (c)
		       (display c port)
		       (set! max-length (- max-length 1))
		       (if (not (positive? max-length))
			   (begin
			     (display "..." port)
			     (return #t))))
		     (lambda (s)
		       (cond
			((<= (string-length s) max-length)
			 (display s port)
			 (set! max-length (- max-length (string-length s))))
			(else
			 (display (substring s max-length))
			 (display "...")
			 (return #t))))
		     #f #f #f)
	     "w")))
       (write obj trunc-port)))))

(define (list-length* l)
  "Return two values: The length of the possibly improper list L,
and a boolean that indicates whether the list is proper."
  (let loop ((l l) (len 0))
    (cond
     ((null? l) (values len #t))
     ((pair? l) (loop (cdr l) (1+ len)))
     (else (values len #f)))))

(define (info-message sym obj expensive? arglist-only?)
  "Return an informational message about OBJ, which is the value of SYM.
For procedures, return procedure symbol and arglist, or
fall back to a message on the arity; if ARGLIST-ONLY?, return the
arglist only.  If EXPENSIVE?, take some more effort."
  ;; The code here is so lengthy because we want to return a
  ;; meaningful result even if we aren't allowed to read the
  ;; documentation files (EXPENSIVE? = #f).
  (cond
   ((and (procedure? obj)
	 (procedure-property obj 'arglist))
    => (lambda (arglist)
	 (let ((required-args (car arglist))
	       (optional-args (cadr arglist))
	       (keyword-args (caddr arglist))
	       (allow-other-keys? (cadddr arglist))
	       (rest-arg (car (cddddr arglist))))
	   (with-output-to-string
	     (lambda ()
	       (define (arg-only arg/default)
		 (if (pair? arg/default) (car arg/default) arg/default))
	       (if arglist-only?
		   (begin (write sym)
			  (display ": ")))
	       (write
		(append
		 (if arglist-only?
		     '()
		     (list sym))
		 required-args
		 (if (not (null? optional-args))
		     (cons #:optional (map arg-only optional-args))
		     '())
		 (if (not (null? keyword-args))
		     (cons #:key (map arg-only keyword-args))
		     '())
		 (if allow-other-keys?
		     (list #:allow-other-keys)
		     '())
		 (if rest-arg rest-arg '()))))))))
   ((and (procedure-with-setter? obj)
	 (closure? (procedure obj)))
    (let ((formals (cadr (procedure-source (procedure obj)))))
      (if arglist-only? formals (cons sym formals))))
   ((closure? obj)
    (let ((formals (cadr (procedure-source obj))))
      (if arglist-only?
	  (begin (write sym)
		 (display ": ")))
      (if arglist-only? formals (cons sym formals))))
   ((or
     (and expensive?
	  (false-if-exception
	   ;; object-documentation was introduced in Guile 1.4,
	   ;; There is no documentation for primitives in earlier
	   ;; versions.
	   (object-documentation obj)))
     (and (procedure? obj)
	  (procedure-property obj 'documentation)
	  ;; The documentation property is attached to a primitive
	  ;; procedure when it was read from the documentation file
	  ;; before.
	  ))
    => (lambda (doc)
	 (if arglist-only?
	     (begin (write sym)
		    (display ": ")))
	 (doc->arglist doc (not arglist-only?))))
   ((and (macro? obj)
	 (macro-transformer obj)
	 (closure? (macro-transformer obj))
	 (procedure-documentation (macro-transformer obj)))
    ;; Documentation may be in the doc string of the transformer, as
    ;; is in session.scm (help).
    => (lambda (doc)
	 (doc->arglist doc (not arglist-only?))))
   ((is-a-generic? obj)
    (let ((methods (generic-function-methods obj)))
      (cond
       ((null? methods)
	(string-append "`" (symbol->string sym) "'"
		       " is a generic function with no methods."))
       ((= 1 (length methods))
	(info-message sym (method-procedure (car methods))
		      expensive? arglist-only?))
       (else
	(let loop ((methods methods)
		   (min-arity most-positive-fixnum)
		   (max-arity 0)
		   (rest-argument? #f))
	  (cond
	   ((null? methods)
	    (with-output-to-string
	      (lambda ()
		(display sym) (display ": ")
		(cond
		 (rest-argument?
		  (display min-arity)
		  (display " or more arguments"))
		 ((= min-arity max-arity)
		  (display min-arity)
		  (display " argument")
		  (if (not (= min-arity 1))
		      (display "s")))
		 (else
		  (display min-arity)
		  (display " to ")
		  (display max-arity)
		  (display " arguments")))
		(display "."))))
	   (else
	    (call-with-values
		(lambda ()
		  (list-length* (method-specializers (car methods))))
	      (lambda (len proper?)
		(loop (cdr methods)
		      (min min-arity len)
		      (max max-arity len)
		      (or rest-argument? (not proper?))))))))))))
   ((procedure? obj)
    ;; Return a message about the arity of the procedure.
    (with-output-to-string
      (lambda () (display sym) (display ": ") (arity obj))))
   (else
    (string-append "`" (symbol->string sym) "'"
		   " is bound to "
		   (with-output-to-string
		     (lambda ()
		       (write-truncated obj (current-output-port) 800)))
		   "."))))
  

(define-public (ilisp-print-info-message sym package)
  "Evaluate SYM in PACKAGE and print an informational message about
the value.  For procedures, the arglist is printed.
This procedure is invoked by the electric space key."
  (if (symbol? sym)
      (let ((obj (catch #t
			(lambda ()
			  (eval-in-package sym
					   (string->module package)))
			(lambda args #f))))
		     
	(cond
	 ((and obj
	       (info-message sym obj #f #f))
	  => (lambda (message)
	       (display message)
	       (newline)))))))

(define (if-defined symbol package
			   defined-procedure undefined-procedure)
  (let ((obj (catch #t
		    (lambda ()
		      (list (eval-in-package symbol
					     (string->module package))))
		    (lambda args #f))))
    (if obj
	(defined-procedure (car obj))
	(undefined-procedure))))

(define (strip-parens s)
  (if (and (string=? (substring s 0 1) "(")
	   (string=? (substring s (- (string-length s) 1)) ")"))
      (substring s 1 (- (string-length s) 1))
      s))      

(define (symbol-not-present symbol package)
  (display "Symbol `")
  (display symbol)
  (display "' not present in ")
  (cond
   ((string=? "nil" package)
    (display "the current module `")
    (for-each display (module-name (current-module)))
    (display "'"))
   (else
    (display "module `")
    (display (strip-parens package))
    (display "'")))
  (display ".\n"))

(define-public (ilisp-arglist symbol package)
  "Evaluate SYMBOL in PACKAGE and print the arglist if we have a
procedure. This procedure is invoked by `arglist-lisp'."
  (if-defined symbol package
	      (lambda (obj)
		(cond
		 ((info-message symbol obj #t #t)
		  => (lambda (message)
		       (display message)
		       (newline)))
		 (else
		  (display "Can't get arglist.")
		  (newline))))
	      (lambda ()
		(symbol-not-present symbol package))))

(define-public (ilisp-help symbol package)
  "Evaluate SYMBOL in PACKAGE and print help for it."
  (if-defined symbol package
	      (lambda (obj)
		(let ((doc (object-documentation obj)))
		  (if doc
		      (display doc)
		      (display "No documentation."))
		  (newline)))
	      (lambda ()
		(symbol-not-present symbol package))))

(define (word-separator? ch)
  (or (char=? ch #\-)
      (char=? ch #\:)
      (char=? ch #\_)
      (char=? ch #\/)))

(define (string-pred-rindex str pred)
  (let loop ((index (- (string-length str) 1)))
    (cond
     ((negative? index) #f)
     ((pred (string-ref str index)) index)
     (else (loop (- index 1))))))

(define (separate-fields-before-predicate pred str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-pred-rindex str pred)
      => (lambda (w) (loop (cons (make-shared-substring str w) fields)
			   (make-shared-substring str 0 w))))
     (else (apply ret str fields)))))

(define (make-word-regexp str)
  (apply string-append
	 (cons "^"
	       (map (lambda (word)
		      (string-append (regexp-quote word) "[^-:/_]*"))
		    (separate-fields-before-predicate word-separator?
						      str list)))))	      

(define-public (ilisp-matching-symbols string package function? external? prefix?)
  (write (map (lambda (sym) (list (symbol->string sym)))
       (let ((regexp (if (eq? prefix? 't)
			 (string-append "^" (regexp-quote string))
			 (make-word-regexp string)))
	     (a-i apropos-internal))
	 (save-module-excursion
	  (lambda ()
	    (set-current-module (string->module package))
	    (a-i regexp))))))
  (newline))

(define (last l)
  (cond ((and (pair? l) (not (null? (cdr l))))
	 (last (cdr l)))
	(else (car l))))

(define-public (ilisp-get-package sequence-of-defines)
  "Get the last module name defined in the sequence of define-module forms."
  ;; First eval the sequence-of-defines.  This will register the
  ;; module with the Guile interpreter if it isn't there already.
  ;; Otherwise `resolve-module' will give us a bad environment later,
  ;; which just makes trouble.
  (let ((name
	 (eval-in-package 
	  (append sequence-of-defines
		  '((module-name (current-module))))
	  (string->module "(guile-user)"))))
    (cond
     ((pair? name)
      ;; This version of Guile has a module-name procedure that
      ;; returns the full module name.  Good.
      (write name))
     (else 
      ;; Now we have the name of the module -- but only the last
      ;; component.  We need to "parse" the sequence-of-defines
      ;; ourselves.
      (let ((last-form (last sequence-of-defines)))
	(cond ((and (pair? last-form)
		    (eq? (car last-form) 'define-module))
	       (write (cadr last-form)))
	      (else (write '(guile-user))))))))
  (newline))

(define-public (ilisp-in-package package)
  (set-current-module (string->module package))
  (process-use-modules '(((guile-ilisp))))
  *unspecified*)

(define-public (ilisp-eval form package filename line)
  "Evaluate FORM in PACKAGE recording FILENAME as the source file
and LINE as the source code line there."
  (eval-in-package
   (read-from-string/source form filename line 1)
   (string->module package)))

(define-public (ilisp-trace symbol package breakp)
  (trace (eval-in-package symbol (string->module package)))
  *unspecified*)

(define-public (ilisp-untrace symbol package)
  (untrace (eval-in-package symbol (string->module package)))
  *unspecified*)

(define (or-map* f list)
  "Apply f to successive elements of l until exhaustion or improper end
or while f returns #f. If returning early, return the return value of f."
  (let loop ((result #f)
	     (l list))
    (or result
	(and (pair? l)
	     (loop (f (car l)) (cdr l))))))

(define-public (ilisp-source-file symbol package)
  "Find the source file of SYMBOL's definition in PACKAGE."
  (catch #t
	 (lambda ()
	   (let ((value (eval-in-package (read-from-string symbol)
					 (string->module package))))
	     (cond
	      ((and (procedure? value)
		    (procedure-source value))
	       => (lambda (source)
		    (and=>
		     (or-map* (lambda (s)
				(false-if-exception
				 (source-property s 'filename)))
			      source)
		     (lambda (filename) (throw 'result filename))))))
	     (write 'nil)))
	 (lambda (key . args)
	   (if (eq? key 'result)
	       (begin (write (car args)) (newline) (write #t))
	       (begin (write 'nil)))))
  (newline))

(define-public (ilisp-macroexpand-1 expression package)
  (write (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (macroexpand-1 (read-from-string expression)))))
  (newline))

(define-public (ilisp-macroexpand expression package)
  (write (save-module-excursion
   (lambda ()
     (set-current-module (string->module package))
     (macroexpand (read-from-string expression)))))
  (newline))

(define-public (ilisp-describe symbol package)
  "Evaluate SYMBOL in PACKAGE and describe its value."
  (let* ((module (resolve-module '(oop goops describe)))
	 (describe (false-if-exception
		    (module-ref module 'describe))))
    (if describe
	(describe (eval-in-package symbol (string->module package)))
	"Need GOOPS for describe.")))

;  Init
; ======
;
; We would like to use ilisp-* functions from all modules. Since functions
; are defined withing (guile-ilisp) module, we have to:
;
;  1) explicitly :use-module (guile-ilisp) in each module in which we
;     want to use ilisp
;
;  2) cheat and put all public symbols to interface of (guile) module.
;     This makes ilisp-* function accessible to all modules without any
;     further effort.
;
; Because I am lazy programmer, I chose 2) - writing (use-modules (...))
; every time I work on some module is very boring and all public functions
; have ilisp-* prefix so name clash is very unlikely.

(module-for-each (lambda (name var)
		   (module-add! the-scm-module name var))
		 (module-public-interface (current-module)))

