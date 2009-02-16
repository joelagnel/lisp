;;; Elpp | Ali Rahimi | ali@xcf.berkeley.edu
;;; Elisp PreProcessor |
;;; Time-stamp: <1998-10-17 13:04:12 ali> | 0.9 |

;;; Commentary:

;; Allows arbitrary elisp code to be embedded within your source code.
;; The main function, elpp-process, takes an input buffer, finds the lisp
;; expressions therein, evaluates them, and substitutes them an output buffer
;; by their value.

;; Expressions must be declared before they can be recognized in a source
;; file. You can do this with either elpp-defun or elpp-macro. If you don't
;; wish to declare new functions or macros, you can use elpp-eval.


;; Usage:

;; This package is most useful when run by emacs in batch mode. I typically
;; define some file "codegen.el" which contains something like:
;;
;;    (defun process-all-my-code ()
;;       (load "elpp.el")
;;       (find-file "foo.C") (find-file "foo.c") (elpp-process "foo.C" "foo.c")
;;       (find-file "bar.C") (find-file "bar.c") (elpp-process "bar.C" "bar.c")
;;     )
;;
;; An example C program that uses Elpp:
;;
;;  (elpp-defvar elements '(0 1 2 3))
;;
;;  (elpp-defun arrangements (lst &optional prn)
;;   "Computes all the permutations of the elements in LST. Returns a list of
;;  the permutations."
;;   (if (null lst) (list prn)
;;     (mapcan (lambda (x) (arrangements (remove x lst) (cons x prn))) lst)))
;;
;;  (elpp-defun list-to-array (lst)
;;    (cond ((symbolp lst) (symbol-name lst))
;;  	    ((numberp lst) (number-to-string lst))
;;  	    (t (concat "\n{ "
;;  		       (apply 'concat
;;  			      (list-to-array  (car lst))
;;  			      (mapcar (lambda (l)
;;  				        (concat ", " (list-to-array l)))
;;  				      (cdr lst)))
;;  		       " }"))))
;;
;;  (elpp-defun perm-tab-list () (list-to-array (arrangements elements)))
;;
;;  (elpp-defun n-perms (&optional n) (length (arrangements elements)))
;;
;;
;;  void
;;  permute(const char *lst, char *out, int mode)
;;  {
;;    int i;
;;    int permutation_tab[(n-perms)][(elpp-eval (length elements))] =
;;  				 (perm-tab-list);
;;    for(i=0; i<(elpp-eval (length elements)); ++i)
;;      out[i] = lst[permutation_tab[mode][i]];
;;  }
;;
;;
;; See cgen.el if you are interested in having your Lisp expression
;; autogenerate C or C++ code. I have trouble justifying the use of
;; elpp in C programming without using cgen.el.

;;; Bugs and Requests:

;; There will be lots, and there will be features that you will want that
;; will be lacking. Mail ali@xcf.berkeley.edu.

;;; Code:


(require 'cl)



(defvar *elpp-macro-list* '("elpp-defun")
  "List of Lisp function names recognized by ELPP.
This starts off with only `elpp-defun', and is added to by
`elpp-defun' as ELPP and user defined functions are created. I know
Emacs people don't like stars bracketting global function names, but
that's how we did it when I was a boy, and that's how we're gonna do
it for a while longer.")


(defun elpp-function-p (fname)
  "Returns true if string FNAME is an ELPP recognized function name."
  (member fname *elpp-macro-list*))


(defmacro elpp-defmacro (name arglist &rest body)
  "Used in source to define an ELPP macro for use in source code.
The name of the macro is added to `*elpp-macro-list*'."
  (add-to-list '*elpp-macro-list* (symbol-name name))
  `(progn ,(append (list 'defmacro name arglist) body) ""))


(defmacro elpp-defun (name arglist &rest body)
  "Used in source to define an ELPP function for use in source code.
The name of the function is added to `*elpp-macro-list*'."
  (add-to-list '*elpp-macro-list* (symbol-name name))
  `(progn ,(append (list 'defun name arglist) body) ""))


(elpp-defun elpp-eval (&rest r)
	    "Used in source to evaluate arbirary Lisp expressions.
The return value of this expression is substituted for the expression
in the source."
	    (car (last r)))

(elpp-defmacro elpp-defvar (vname value &optional docstring)
	       "Used in source to define a variable."
	       `(progn (defvar ,vname ,value ,docstring) ""))


(defun eval-exp-1 ()
  "A useful eval which evaluates the expression at point. Unlike
eval-last-sexp (which evaluates the sexp before point) returns the
result of the expression."
  (let ((ts (point)))
      (eval (read (buffer-substring ts (forward-list))))))


(defun elpp-process (inbuf outbuf)
  "Preprocess a source file in buffer INBUF and dump the result into OUTBUF.
Normally, the elpp source functions elpp-defun, elpp-macro, elpp-defvar,
and elpp-eval are defined. These can be used in INBUF to create new elpp
symbols."
  (interactive "bTemplate buffer: \nbOutput buffer: ")
  (save-excursion
    (set-buffer outbuf)
    (erase-buffer)

    (set-buffer inbuf)
    (lisp-mode)                ; needed for eval-exp-1.
    (goto-char (point-min))

    (while (not (equal (point) (point-max)))
      ; Copy everything from here to the next '(' to the output buffer.
      (let ((ts (point))
	    (tm (point-max))
	    (te (search-forward "(" nil t)))

	(set-buffer outbuf)

	(if (null te) (insert-buffer-substring inbuf ts tm)

	   ; everything up to the potential sexp to outbuf.
	  (insert-buffer-substring inbuf ts (1- te)) (goto-char (point-max))

	  (set-buffer inbuf) (point)
	  ; see whether the "(" is actually a sexp we can use.
	  (let ((ts (point))
		(te (re-search-forward "[^ \n\t()]+" nil t)))
	    (if (and te (elpp-function-p (buffer-substring ts te)))
		; need to go to the end of the expression now.
		(progn (goto-char (1- ts))
		       (princ (prog1 (eval-exp-1) (set-buffer outbuf)) #'insert)
		       (set-buffer inbuf))

	      ; It was not a sexp afterall. Output the (. The rest will
	      ; be taken care of. Just make sure the input is rolled back
	      ; to the (.
	      (set-buffer outbuf)
	      (insert "(")
	      (set-buffer inbuf)
	      (goto-char ts)
	      ))))
      )))


(elpp-defmacro elpp-template-begin (tname)
  "Defines a template called TNAME.
Everything from the end of this funcall to the beginning of the matching
call to `elpp-template-end' is treated as the content of TNAME.
Elpp expressions in the template are not evaluated at this time.
Substitutions will happen when the template is instantiated.

There is a lameness herein, because `elpp-template-end' is actually not an
elpp symbol. It's actually just searched for as a dumb string."
;  `(progn (setq ,tname `(,(current-buffer) ,(point) nil nil)) ""))
  (let ((tts (point)))
    ; Skip to the matching elpp-template-end.
    (if (re-search-forward (concat
			    "(elpp-template-end[ \t$]+"
			    (symbol-name tname)
			    "[ \t$]*)") nil nil)
	       `(progn (setq ,tname
			     '(,(current-buffer) ,tts ,(search-backward "(")
			       nil))
		       (setf (cadddr ,tname) (buffer-substring (cadr ,tname)
							    (caddr ,tname)))
		       "")
      )))

(elpp-defun elpp-template-end (tname)
  "This is a noop, since elpp-template-begin did all the work already."
  "")



(defun define-template-macros (substs)
  "Creates a function for each of the substitutions in substs. The name
of each function will be the car of each element in SUBSTS. Each of these
functions is defined with elpp-defmacro and evaluates to the cadr of each
element in SUBSTS."
  (mapcar (lambda (sub)
	    (eval (list 'elpp-defun (car sub) nil (cadr sub))))
	  substs))


(elpp-defun elpp-template-instantiate (tname substs)
  "Instantiates the template TNAME by performing the substitutions defined
by SUBSTS.
The substitutions take on the same form they do in `let', with each element
of SUBST being a list, the `car' of each of which issymbol name to substitute
for, and the `cadr' of which is an expression to that the `car' should
evaluate to.
The template then `elpp-process'ed, with each symbol in SUBST defining an
elpp source function which evaluates to the value of the symbol.

Verbose rambling aside, here's what you do to get 'foo bar moof bafbaf baz':

(elpp-template-begin TT)
foo bar (fluff) (flap) baz
(elpp-template-end TT)

(elpp-template-instantiate TT '((fluff moof) (flap bafbaf)))"
  (let ((*elpp-macro-list* *elpp-macro-list*)
	(tempbuf           (generate-new-buffer "*template*"))
	(instbuf           (generate-new-buffer "*template-instance*"))
	(oldbuf            (current-buffer)))

    (define-template-macros substs)

    (set-buffer tempbuf)
    (insert (cadddr tname))

    (elpp-process tempbuf instbuf)

    (set-buffer instbuf)
    (prog1
	(buffer-string)
      (kill-buffer tempbuf)
      (kill-buffer instbuf)
      (set-buffer oldbuf))
    ))


(elpp-defun elpp-annotate-table (titles tab)
	    "Annotates the columns of TAB with elements of TITLES.
TAB is of the form ((A1 A2 A3 ...)
                    (B1 B2 B3 ...)
                    ...)
Titles is an array of symbols with which to annotate the elements of TAB.
The result of annotating the above table with the titles (C1 C2 C2) is

                   (((C1 A1) (C2 A2) (C3 A3))
                    ((C1 B1) (C2 B2) (C3 B3))
                    ...)

This function is often useful when lists of template substitution values
are being created, and `elpp-template-instatiate' is about to be called
on each element of the annotated list."
	    (mapcar (lambda (entry) (mapcar* 'list titles entry)) tab))
