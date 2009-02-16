;;; czscm.el --- cheezy scheme interpreter for emacs

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: interpreters, languages, scheme, lisp
;; Created: 1997-06-19

;; $Id: czscm.el,v 1.6 1997/06/30 12:55:25 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; "It's not merely cheezy, it's full-on spray-can cheez."

;; To use this package, load it and run the command `czscm-initialize'.
;; To evaluate scheme expressions within emacs lisp, use
;;      (in-czscm <scheme expressions>)

;; Some features of this implementation:
;;    * lexical scope
;;    * general tail-call elimination (not just tail recursion)
;;    * first-class environments

;; Not yet implemented:
;;    * misc primitives and i/o routines
;;    * continuations
;;    * R4RS-conformant read/print routines
;;    * exception handlers (including dynamic-wind)
;;    * macros (let-syntax, etc.)
;;    * compilation for a scheme VM
;;    * debugging facilities

;; Since this implementation does not yet have its own reader, the read
;; syntax for characters, etc. match those of the underlying lisp, not R4RS
;; Scheme.  For example, symbols are case-sensitive and symbols starting
;; with '#' must be escaped with `\' (e.g. `\#t').

;; Currently, the empty list is treated equivalently to the boolean false
;; value.  This will be fixed eventually as various primitive functions
;; (e.g. `null?') get rewritten so as not to return nil from the underlying
;; emacs-lisp subr.

;;; Code:

;; None of these variables are user options.  This intrepreter (as yet) has
;; none.  So don't change these unless you know what you're doing.

(defvar czscm-top-level-environment nil)
(defvar czscm-env-default-obarray-size 3)
(defvar czscm-procedure-creation-count nil)

;; This is a special "object" to be bound to variables which do not yet
;; have a value, and as the return value of primitive functions which have
;; no other meaningful return value.  The symbol is uninterned to
;; distinguish it from regular objects that happen to have the same name.
(defvar czscm-unspecified-symbol (make-symbol "#<unspecified>"))

;; This variable (when initialized) provides an "infinite pool" of
;; unspecified values to bind to variable lists when they have no other
;; value to bind.
(defvar czscm-circular-unspecified-list nil)

;; Internal tag.
(defvar czscm-env-null-frame-symbol (make-symbol "#<null-frame>"))

(defvar czscm-boolean-true '\#t)
(defvar czscm-boolean-false '\#f)

(defvar czscm-procedure-types
  '(special-form primitive-procedure elisp-procedure compound-procedure))

(defvar czscm-procedure-type-eval-methods
  '((special-form        czscm-eval-procedure-special-form)
    (primitive-procedure czscm-eval-procedure-primitive)
    (elisp-procedure     czscm-eval-procedure-elisp)
    (compound-procedure  czscm-eval-procedure-compound)))

;; Special forms receive their arguments unevaluated.
;; They must evaluate their arguments explicitly and pass back tail-calls.
(defvar czscm-procedure-special-form-names
  '(and begin case cond define do if
    lambda let let* letrec or quote set!
    the-environment))

;; Like special forms, but arguments are already evaluated.
(defvar czscm-procedure-primitive-names
  '(apply eval))

;; Mapping from scheme procedures to emacs lisp functions.
;; Note that these functions know nothing about environment context or
;; tail calls, and do not get these arguments from the scheme evaluator.
;; Define "primitive" procedures when you need this information.
;;
;; If an elt is a symbol, then the scheme name is the same as the elisp name.
;; If the elt is a list, then the second member denotes the elisp name.
;; If the second name is not specified, it defaults to
;;   czscm-procedure-elisp-<name>
;;
(defvar czscm-procedure-elisp-names
  '(not
    (boolean\?)
    (eqv\? eq)  ; fixme
    (eq\? eq)
    (equal\? equal)

    (pair\? consp)
    cons car cdr
    (set-car! setcar)
    (set-cdr! setcdr)
    (null\? null)
    (list\? listp)  ; fixme
    list length append reverse
    memq member
    (memv memq)  ; fixme
    assq assoc
    (assv assq) ; fixme
    (list-ref nth)
    ;; (list-tail)

    (symbol\? symbolp)
    (symbol->string symbol-name)
    (string->symbol intern)

    (number\? numberp)
    (complex\? ignore)
    (real\? floatp)
    (rational\? numberp)
    (integer\? integerp)
    (exact\? integerp)
    (inexact\? floatp)

    = < > <= >=
    (zero\? zerop)
    ;; (positive\?)
    ;; (negative\?)
    ;;; (odd\?)
    ;; (even\?)

    max min
    + - * / 1+ 1-
    abs
    (remainder %)
    (modulo mod)
    (quotient /)
    ;; (gcd)
    ;; (lcm)
    floor ceiling truncate round
    lsh ash logand logior logxor lognot
    sin cos tan asin acos atan exp log log10 expt sqrt
    (exact->inexact float)
    (exact->inexact truncate)

    (number->string number-to-string)
    (string->number string-to-number)
    (char-integer identity)  ; may change in xemacs 20
    (integer-char identity)  ; may change in xemacs 20

    (char-upcase upcase)
    (char-downcase downcase)
    (char=? char-equal)
    (char<? <)
    (char>? >)
    (char<=? <=)
    (char>=? >=)
    ;; char-ci=?
    ;; char-ci<?
    ;; char-ci>?
    ;; char-ci<=?
    ;; char-ci>=?
    ;; char-alphabetic?
    ;; char-numeric?
    ;; char-whitespace?
    ;; char-upper-case?
    ;; char-lower-case?

    (string\? stringp)
    make-string
    ;; (string)
    (string-length length)
    (string-ref aref)
    (string-set! aset)
    (string=\? string-equal)
    ;; (string-ci=\?)
    ;; string<?
    ;; string>?
    ;; string<=?
    ;; string>=?
    ;; string-ci<?
    ;; string-ci>?
    ;; string-ci<=?
    ;; string-ci>=?
    substring
    (string-append concat)

    ;; string->list
    ;; list->string
    (string-copy copy-sequence)
    ;; (string-fill!)
    format

    (procedure\?)
    (procedure-environment)
    ))


(defun czscm-initialize ()
  (interactive)
  (czscm-indent-like 'save-excursion 'in-czscm)
  (czscm-indent-like 'let 'letrec)

  (setq czscm-procedure-creation-count 0)
  (setq czscm-circular-unspecified-list
        (cons czscm-unspecified-symbol nil))
  (setcdr czscm-circular-unspecified-list czscm-circular-unspecified-list)

  (setq czscm-top-level-environment
        (czscm-env-make-new-environment nil nil nil))

  (czscm-initialize-special-forms)
  (czscm-initialize-primitive-procedures)
  (czscm-initialize-elisp-procedures))

;; There is no need to make this macro "hygienic" because it does not
;; evaluate arbitrary elisp expressions, only scheme expressions.  The
;; binding of local variables in the eval loop cannot have any side effects
;; on either the lisp or scheme world.
(defmacro in-czscm (&rest bodyforms)
  (` (let ((result nil)
           (bodyforms '(, bodyforms)))
       (while bodyforms
         (setq result (czscm-eval (car bodyforms)))
         (setq bodyforms (cdr bodyforms)))
       result)))


;;; Top-level evaluator routines

(defun czscm-eval (expr &optional env tail)
  (or env (setq env czscm-top-level-environment))
  (or tail (setq tail (czscm-make-tail-call)))
  (let ((result nil))
    (while expr
      (czscm-tail-call-set-expression!  tail nil)
      (czscm-tail-call-set-environment! tail nil)
      (cond ((czscm-type-immediate-p expr)
             (setq result expr)
             (setq expr nil))
            ((czscm-type-variable-p expr)
             (setq result (czscm-variable-lookup expr env))
             (setq expr nil))
            ((czscm-type-compound-p expr)
             (setq result (czscm-eval-compound expr env tail))
             (setq expr (czscm-tail-call-expression tail))
             (setq env (czscm-tail-call-environment tail)))))
    result))

(defun czscm-eval-compound (expr env tail)
  (let* ((procedure (czscm-eval (car expr) env tail))
         (args (cdr expr))
         (method nil))
    (or (czscm-type-procedure-p procedure)
        (czscm-signal 'czscm-applying-non-procedure
                      (list procedure expr env)))
    (setq method (nth 1 (assq (czscm-procedure-type procedure)
                              czscm-procedure-type-eval-methods)))
    (funcall method procedure args env tail)))

(defun czscm-eval-procedure-special-form (procedure args env tail)
  (funcall (czscm-procedure-lookup-special-form procedure)
           args env tail))

(defun czscm-eval-procedure-primitive (procedure args env tail)
  (funcall (czscm-procedure-lookup-primitive procedure)
           (czscm-eval-arglist args env tail)
           env tail))

(defun czscm-eval-procedure-elisp (procedure args env tail)
  (apply (czscm-procedure-lookup-elisp procedure)
         (czscm-eval-arglist args env tail)))

(defun czscm-eval-procedure-compound (procedure args env tail)
  (czscm-eval-sequence
   (czscm-procedure-get-bodyforms procedure)
   (czscm-env-make-new-environment
    (czscm-procedure-get-arguments procedure)
    (czscm-eval-arglist args env tail)
    (czscm-procedure-get-environment procedure))
   tail))

;; Evaluate expressions in env, returning the final expression unevaluated
;; in TAIL, unless ALLP is non-nil.  RESULT is the initial value to
;; consider as the return value if there are no expressions to evaluate.
;; UNTIL is a predicate which, if it returns true for the most recently
;; evaluated expression, aborts the sequence of evaluations and returns the
;; most recent result with no tail call.
(defun czscm-eval-sequence (expressions env tail &optional result until allp)
  (while expressions
    (cond ((and (not allp)
                (czscm-last-expression-p expressions))
           (czscm-tail-call-set-expression!
            tail (czscm-first-expression expressions))
           (czscm-tail-call-set-environment! tail env))
          (t
           (setq result
                 (czscm-eval (czscm-first-expression expressions) env tail))
           (and until
                (czscm-boolean-true-p (funcall until result))
                (setq expressions nil))))
    (setq expressions (czscm-rest-expressions expressions)))
  result)

(defun czscm-eval-arglist (args env tail)
  (mapcar (function (lambda (arg) (czscm-eval arg env tail))) args))


;;; Special forms (i.e. syntax)

;; Note that although this interpreter does not rewrite derived expression
;; types (which might make it it easier to perform some kinds of optimization),
;; that would not inhibit a compiler from performing those rewrites.
;; So I am not going to worry about this for now.

(defun czscm-procedure-special-form-and (args env tail)
  (czscm-eval-sequence args env tail
                       czscm-boolean-true 'czscm-boolean-false-p))

(defun czscm-procedure-special-form-begin (args env tail)
  (czscm-eval-sequence args env tail))

(defun czscm-procedure-special-form-case (args env tail)
  (let ((key (czscm-eval (car args) env tail))
        (clauses (cdr args))
        datum expressions
        (result czscm-boolean-false))
    (while clauses
      (setq datum (car (car clauses)))
      (cond ((or (eq datum 'else)
                 ;; FIXME: this should use memqv instead of memq
                 ;; once eqv is implemented.
                 (memq key datum))
             (setq result
                   (czscm-eval-sequence (cdr (car clauses)) env tail))
             (setq clauses nil))
            (t
             (setq clauses (cdr clauses)))))
    result))

(defun czscm-procedure-special-form-cond (clauses env tail)
  (let ((result czscm-boolean-false)
        condition body)
    (while clauses
      (setq condition (car (car clauses)))
      (setq body (cdr (car clauses)))
      (cond ((or (eq condition 'else)
                 (czscm-boolean-true-p (czscm-eval condition env tail)))
             (setq result (czscm-eval-sequence body env tail))
             (setq clauses nil))
            (t
             (setq clauses (cdr clauses)))))
    result))

(defun czscm-procedure-special-form-define (args env tail)
  (let ((var (car args))
        (val (cdr args)))
    (cond ((consp var)
           (setq val (append (list 'lambda (cdr var)) val))
           (setq var (car var)))
          (t
           (setq val (car val))))
    (or (symbolp var)
        (czscm-signal 'czscm-wrong-type-argument
                      (list (list 'symbolp var) args env)))
    (czscm-variable-define var (czscm-eval val env tail) env)))

(defun czscm-procedure-special-form-do (args env tail)
  (let* ((bindings (nth 0 args))
         (clause (nth 1 args))
         (body (nthcdr 2 args))
         (test (nth 0 clause))
         (final-expressions (nthcdr 1 clause))
         (iter-env (czscm-env-make-new-environment nil nil env))
         (next-env (czscm-env-make-new-environment nil nil env)))
    (czscm-internal-let-bind
     bindings (czscm-env-first-frame iter-env) env tail)
    (while (czscm-boolean-false-p (czscm-eval test iter-env tail))
      (czscm-eval-sequence body iter-env tail nil nil t)
      (czscm-internal-let-bind
       bindings (czscm-env-first-frame next-env) iter-env tail t)
      (setq iter-env (prog1 next-env (setq next-env iter-env))))
    (czscm-eval-sequence final-expressions iter-env tail
                         czscm-unspecified-symbol)))

(defun czscm-procedure-special-form-if (args env tail)
  (if (czscm-boolean-true-p (czscm-eval (nth 0 args) env tail))
      (setcar tail (nth 1 args))
    (setcar tail (nth 2 args)))
  (setcdr tail env))

;; TODO: Add some checking here for `define' forms in the body.
;; They should only be allowed when specified before any other expressions.
;; At the very least, create the new variables in the environment before
;; actually evaluating anything.   Otherwise, it's possible to change the
;; scope of variable references mid-procedure.
(defun czscm-procedure-special-form-lambda (args env tail)
  (czscm-procedure-create-compound (car args) (cdr args) env))

(defun czscm-procedure-special-form-let (args env tail)
  (let ((frame (czscm-env-make-new-frame nil nil)))
    (czscm-internal-let-bind (car args) frame env tail)
    (czscm-eval-sequence
     (cdr args)
     (czscm-env-link-frame-into-environment frame env)
     tail)))

;; This function must, in principle, put every local binding in its own
;; frame, because lambda expressions defined earlier should not reference
;; free variables defined later.  That's what would happen if all local
;; variables were defined in the same frame (see letrec).
;; We optimize this somewhat by only creating new frames for subsequent
;; bindings when the result of the previous binding was a procedure object
;; created in the "current" environment.
(defun czscm-procedure-special-form-let* (args env tail)
  (setq env (czscm-env-make-new-environment nil nil env))
  (let ((bindings (car args))
        (new-val nil))
    (while bindings
      (setq new-val
            (czscm-internal-let-binding
             (car bindings) (czscm-env-first-frame env) env tail))
      (and (czscm-type-compound-procedure-p new-val)
           (eq env (czscm-procedure-get-environment new-val))
           (setq env (czscm-env-make-new-environment nil nil env)))
      (setq bindings (cdr bindings))))
  (czscm-eval-sequence (cdr args) env tail))

;; This is similar to let*, but instead of evaluating bindings in the
;; incrementally growing new environment, binding all the local variables
;; in the top frame of the environment, and then evaluate the bindings in
;; *that* environment.  This allows for mutual references in the bindings,
;; particularly useful if those bindings are lambda expressions.
(defun czscm-procedure-special-form-letrec (args env tail)
  (let* ((bindings (car args))
         (namelist (mapcar (function (lambda (binding)
                                       (if (symbolp binding)
                                           binding
                                         (car binding))))
                           bindings))
         (frame (czscm-env-make-new-frame namelist nil)))
    (setq env (czscm-env-link-frame-into-environment frame env))
    (czscm-internal-let-bind bindings frame env tail)
    (czscm-eval-sequence (cdr args) env tail)))

(defun czscm-procedure-special-form-or (args env tail)
  (czscm-eval-sequence args env tail
                       czscm-boolean-false 'czscm-boolean-true-p))

(defun czscm-procedure-special-form-quote (args env tail)
  (nth 0 args))

(defun czscm-procedure-special-form-set! (args env tail)
  (let ((var (nth 0 args))
        (val (czscm-eval (nth 1 args) env tail)))
    (or (symbolp var)
        (czscm-signal 'czscm-wrong-type-argument
                      (list (list 'symbolp var) args env)))
    (czscm-variable-set! var val env)))

(defun czscm-procedure-special-form-the-environment (args env tail)
  env)

;;; Common subroutines for some special forms.

(defun czscm-internal-let-bind (varlist frame env tail &optional step)
  (let (binding)
    (while varlist
      (setq binding (car varlist))
      (setq varlist (cdr varlist))
      (czscm-internal-let-binding binding frame env tail step))))

(defun czscm-internal-let-binding (binding frame env tail &optional step)
  (cond
   ((symbolp binding)
    (czscm-variable-define-in-frame binding czscm-unspecified-symbol frame)
    czscm-unspecified-symbol)
   ((not (consp binding))
    (czscm-signal 'czscm-wrong-type-argument
                  (list 'symbol-or-list-p binding)))
   (t
    (if (and step
             (nthcdr 2 binding))
        (setq step 2)
      (setq step 1))
    (let ((val (czscm-eval (nth step binding) env tail)))
      (czscm-variable-define-in-frame (nth 0 binding) val frame)
      val))))


;;; Primitive scheme procedures
;; These functions are applicative. i.e. their arguments are already
;; evaluated, but they may do tail-call elimination or manipulate the
;; environment.

(defun czscm-procedure-primitive-eval (args env tail)
  (czscm-tail-call-set-expression! tail (nth 0 args))
  (czscm-tail-call-set-environment! tail (nth 1 args)))

(defun czscm-procedure-primitive-apply (args env tail)
  (let ((p args))
    (while (nthcdr 2 p)
      (setq p (cdr p)))
    (setcdr p (nth 1 p)))
  (czscm-tail-call-set-expression! tail args)
  (czscm-tail-call-set-environment! tail env))


;;; Primitive procedures with elisp function call conventions
;; The arguments to these functions are the fully-evaluated argument lists
;; from scheme, with no environment context or tail-call slots for callbacks.
;; These functions are expected always to return a value (assuming they halt).

(defun czscm-procedure-elisp-boolean\? (obj)
  (or (eq obj czscm-boolean-true)
      (eq obj czscm-boolean-false)))

(defun czscm-procedure-elisp-procedure\? (obj)
  (if (czscm-type-procedure-p obj)
      czscm-boolean-true
    czscm-boolean-false))

(defun czscm-procedure-elisp-procedure-environment (procedure)
  (czscm-procedure-get-environment procedure))


;;; Type predicates, used internally by the evaluator.

(defun czscm-type-immediate-p (obj)
  (or (numberp obj)
      (stringp obj)
      (vectorp obj)
      (memq obj '(t nil \#t \#f))))

(defun czscm-type-variable-p (obj)
  (and (symbolp obj)
       ;; Procedure objects are also symbols, but they are never
       ;; interned in any obarray.
       (intern-soft (symbol-name obj))))

;; Conses and lists are not considered an immediate type.
;; They are used to form sexps, which must be recursively evaluated.
;; Only `quote' can avoid this recursion.
(defun czscm-type-compound-p (obj)
  (consp obj))

(defun czscm-type-boolean-p (obj)
  (memq obj '(\#t \#f)))

(defun czscm-type-procedure-p (obj)
  (and (symbolp obj)
       (not (intern-soft (symbol-name obj)))
       (eq 'procedure (czscm-procedure-get-property obj 'type))))

(defun czscm-boolean-false-p (val)
  (or (eq val czscm-boolean-false)
      ;; This may change someday.
      (null val)))

(defun czscm-boolean-true-p (val)
  (not (czscm-boolean-false-p val)))


;;; Procedure creation and decomposition routines

;; Procedures (of any type) are bound to an uninterned symbol.
;; The object actually accessible in the scheme environment is that
;; symbol.  This is avoid making it possible to decompose the function body
;; in a way that would allow the forgery of new primitive procedures,
;; e.g. ones which can be destructive to emacs.

(defun czscm-initialize-class (initializer names)
  (while names
    (if (symbolp (car names))
        (funcall initializer (car names) czscm-unspecified-symbol)
      (apply initializer (car names)))
    (setq names (cdr names))))

(defun czscm-initialize-special-forms ()
  (czscm-initialize-class 'czscm-define-special-form
                          czscm-procedure-special-form-names))

(defun czscm-initialize-primitive-procedures ()
  (czscm-initialize-class 'czscm-define-primitive-procedure
                          czscm-procedure-primitive-names))

(defun czscm-initialize-elisp-procedures ()
  (czscm-initialize-class 'czscm-define-elisp-procedure
                          czscm-procedure-elisp-names))

;;;

(defun czscm-define-special-form (name &optional actual env)
  (or env (setq env czscm-top-level-environment))
  (czscm-variable-define
   name (czscm-procedure-create-special-form name actual) env))

(defun czscm-define-primitive-procedure (name &optional actual env)
  (or env (setq env czscm-top-level-environment))
  (czscm-variable-define
   name (czscm-procedure-create-primitive name actual) env))

(defun czscm-define-elisp-procedure (name &optional actual booleanp env)
  (or env (setq env czscm-top-level-environment))
  (czscm-variable-define
   name (czscm-procedure-create-elisp name actual booleanp) env))

;;;

(defun czscm-procedure-create-special-form (name &optional actual)
  (czscm-procedure-create-type 'special-form
    (format "#<special-form %s>" name) nil
    (czscm-procedure-actual-name nil "special-form" name)
    nil))

(defun czscm-procedure-create-primitive (name &optional actual)
  (czscm-procedure-create-type 'primitive-procedure
    (format "#<primitive-procedure %s>" name) nil
    (czscm-procedure-actual-name nil "primitive" name)
    nil))

(defun czscm-procedure-create-elisp (name &optional actual booleanp)
  (setq name
        (czscm-procedure-create-type 'elisp-procedure
          (format "#<primitive-elisp-procedure %s>" name) nil
          (czscm-procedure-actual-name actual "elisp" name)
          nil))
  (and booleanp
       (czscm-procedure-put-property name 'elisp-boolean t))
  name)

(defun czscm-procedure-create-compound (args bodyforms env)
  (czscm-procedure-create-type 'compound-procedure
    (format "#<compound-procedure %d>" (czscm-procedure-creation-count))
    args bodyforms env))

(defun czscm-procedure-create-type (type symbol-name args bodyforms env)
  (let ((new-sym (make-symbol symbol-name)))
    (set new-sym (list args bodyforms env nil))
    (czscm-procedure-put-property new-sym 'type 'procedure)
    (czscm-procedure-put-property new-sym 'procedure-type type)
    new-sym))

(put 'czscm-procedure-create-type 'lisp-indent-function
     (get 'while 'lisp-indent-function))

(defun czscm-procedure-actual-name (actual type name)
  (cond ((null actual)
         (intern (format "czscm-procedure-%s-%s" type name)))
        ((eq actual czscm-unspecified-symbol)
         name)
        (t actual)))
;;;

(defun czscm-procedure-get-arguments (procedure)
  (nth 0 (symbol-value procedure)))

(defun czscm-procedure-get-bodyforms (procedure)
  (nth 1 (symbol-value procedure)))

(defun czscm-procedure-get-environment (procedure)
  (nth 2 (symbol-value procedure)))

(defun czscm-procedure-get-property-list (procedure)
  (nth 3 (symbol-value procedure)))

(defun czscm-procedure-put-property-list (procedure proplist)
  (setcar (nthcdr 3 (symbol-value procedure)) proplist))

(defun czscm-procedure-get-property (procedure propname)
  (plist-get (czscm-procedure-get-property-list procedure) propname))

(defun czscm-procedure-put-property (procedure propname property)
  (czscm-procedure-put-property-list procedure
   (plist-put (czscm-procedure-get-property-list procedure)
              propname property)))

(defun czscm-procedure-type (procedure)
  (czscm-procedure-get-property procedure 'procedure-type))

(defun czscm-procedure-creation-count ()
  (setq czscm-procedure-creation-count (1+ czscm-procedure-creation-count)))


;;; Environment frame and variable access, creation, and mutation.
;;; In the current implementation, frames can be either obarrays or alists.

(defun czscm-variable-lookup (variable env)
  (let ((cell (czscm-variable-lookup-cell-in-environment variable env)))
    (if cell
        (czscm-variable-lookup-cell-value cell)
      (czscm-signal 'czscm-unbound-variable (list variable env)))))

(defun czscm-variable-define (variable value env)
  (czscm-variable-define-in-frame variable value (czscm-env-first-frame env)))

(defun czscm-variable-set! (variable value env)
  (let ((cell (czscm-variable-lookup-cell-in-environment variable env)))
    (cond ((null cell)
           (czscm-signal 'czscm-unbound-variable (list variable value env)))
          (t
           (czscm-variable-set-cell-value! value cell)))))

(defun czscm-variable-define-in-frame (variable value frame)
  (let ((cell (czscm-variable-lookup-cell-in-frame variable frame)))
    (and (null cell)
         (setq cell (czscm-env-make-new-cell-in-frame! variable frame)))
    (czscm-variable-set-cell-value! value cell)))

(defun czscm-variable-lookup-cell-in-environment (variable env)
  (let ((cell nil))
    (while env
      (setq cell (czscm-variable-lookup-cell-in-frame
                  variable (czscm-env-first-frame env)))
      (if cell
           (setq env nil)
        (setq env (czscm-env-rest-frames env))))
    cell))

(defun czscm-variable-lookup-cell-in-frame (variable frame)
  (cond ((vectorp frame)
         (intern-soft (symbol-name variable) frame))
        ((listp frame)
         (assq variable frame))))

(defun czscm-variable-lookup-cell-value (cell)
  (cond ((symbolp cell)
         (symbol-value cell))
        ((consp cell)
         (cdr cell))))

(defun czscm-variable-set-cell-value! (value cell)
  (cond ((consp cell)
         (setcdr cell value))
        ((symbolp cell)
         (set cell value))))

;;;

(defun czscm-env-first-frame (env)
  (car env))

(defun czscm-env-rest-frames (env)
  (cdr env))

(defun czscm-env-make-new-environment (vars vals parent-env &optional size)
  (czscm-env-link-frame-into-environment
   (czscm-env-make-new-frame vars vals size) parent-env))

(defun czscm-env-link-frame-into-environment (frame env)
  (cons frame env))

(defun czscm-env-make-new-frame (variables values &optional obarray-size)
  (or obarray-size (setq obarray-size czscm-env-default-obarray-size))
  (let ((frame nil))
    (cond (obarray-size
           (setq frame (make-vector obarray-size 0))
           (while variables
             (if (null values)
                 (setq values czscm-circular-unspecified-list))
             (set (intern (symbol-name (car variables)) frame) (car values))
             (setq variables (cdr variables))
             (setq values (cdr values))))
          (t
           (while variables
             (if (null values)
                 (setq values czscm-circular-unspecified-list))
             (setq frame (cons (cons (car variables) (car values)) frame))
             (setq variables (cdr variables))
             (setq values (cdr values)))))
    (or frame
        (cons czscm-env-null-frame-symbol nil))))

(defun czscm-env-make-new-cell-in-frame! (variable frame)
  (cond ((vectorp frame)
         (intern (symbol-name variable) frame))
        ((czscm-env-frame-null-p frame)
         (setcar frame (cons variable czscm-unspecified-symbol)))
        (t
         (let ((old-front (cons (car frame) (cdr frame))))
           (setcar frame (cons variable czscm-unspecified-symbol))
           (setcdr frame old-front))
         (car frame))))

(defun czscm-env-frame-null-p (frame)
  (eq (car frame) czscm-env-null-frame-symbol))


;;; Misc data structure accessors, mutators

(defun czscm-procedure-lookup-special-form (procedure)
  (czscm-procedure-get-bodyforms procedure))

(defun czscm-procedure-lookup-primitive (procedure)
  (czscm-procedure-get-bodyforms procedure))

(defun czscm-procedure-lookup-elisp (procedure)
  (czscm-procedure-get-bodyforms procedure))

(defun czscm-first-expression (sequence)
  (car sequence))

(defun czscm-rest-expressions (sequence)
  (cdr sequence))

(defun czscm-last-expression-p (sequence)
  (null (nthcdr 1 sequence)))

(defun czscm-make-tail-call (&optional expr env)
  (cons expr env))

(defun czscm-tail-call-expression (tail)
  (car tail))

(defun czscm-tail-call-environment (tail)
  (cdr tail))

(defun czscm-tail-call-set-expression! (tail expr)
  (setcar tail expr))

(defun czscm-tail-call-set-environment! (tail env)
  (setcdr tail env))

(defalias 'czscm-signal 'signal)


;; Error types and other misc routines

(defun czscm-define-error (error-name error-string &rest parent-error-types)
  (put error-name 'error-message error-string)
  (put error-name 'error-conditions (append parent-error-types '(error))))

(czscm-define-error 'czscm-wrong-type-argument
                  "Wrong type argument" 'wrong-type-argument)

(czscm-define-error 'czscm-unbound-variable "Unbound variable" 'void-variable)

(czscm-define-error 'czscm-eval-compound-type-error
                    "Unknown compound expression type" 'wrong-type-argument)

(czscm-define-error 'czscm-applying-non-procedure
                    "Attempt to call a non-procedure object as a procedure"
                    'czscm-wrong-type-argument)

(defun czscm-indent-like (existing new)
  (put new 'lisp-indent-function (get existing 'lisp-indent-function)))

(provide 'czscm)

;;; czscm.el ends here
