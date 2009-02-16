;;; lambdacalc.el --- Interpret lambda calculus expressions

;; Copyright (C) 2007 Michael Olson

;; Author: Michael Olson (mwolson AT gnu DOT org)
;; Date: Mon 19-Feb-2007
;; Version: 1.0
;; URL: http://mwolson.org/static/dist/elisp/lambdacalc.el

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides an interpreter for untyped lambda calculus
;; expressions.

;; * Use:
;;
;; To evaluate a lambda calculus expression, type in the following,
;; replacing <expression> with the desired expression.
;;
;;   M-x lc-eval-expression RET (<expression>) RET
;;
;; Note that the expression must be surrounded by a set of
;; parentheses.  Omit the period in `lambda' constructs.
;;
;; To define a metavariable, evaluate the following code, replacing
;; <varname> with the name of the metavariable, and <expression> with
;; the desired expression.
;;
;;   (lc-def <varname> (<expression>))
;;
;; If you wish to evaluate lambda calculus expressions
;; programmatically, you can use the `lc-eval' macro, which takes
;; something of the form (<expression>).  There is no need to quote
;; the expression.
;;
;; To see each individual step of the evaluation process, do the
;; following.  By default, the expressions are pretty-printed; to
;; change that, customize the `lc-enable-pp' option.
;;
;;   M-x lc-step-expression RET (<expression>) RET

;; * Implementation details:
;;
;; This interpreter works in several stages.
;;
;; ** Translation (Lambda Calculus -> Emacs Lisp):
;;
;; The first step is translation from a lambda calculus expression to
;; an Emacs Lisp expression.  This involves determining bound and free
;; variables, and replacing implicit function calls with explicit
;; calls to `lc-apply'.
;;
;; ** Evaluation:
;;
;; The next step is to evaluate the translated expression, namely:
;; substituting previously-defined expressions for free variables and
;; repeatedly applying function calls until a lambda expression is
;; reached.
;;
;; In order to prevent infinite evaluation, a limit has been placed on
;; the number of consecutive evaluations that are permitted.  The
;; default is 50.  To change this, customize the `lc-max-steps'
;; option.
;;
;; ** Untranslation (Emacs Lisp -> Lambda Calculus):
;;
;; The final step is to take the result from evaluation and make it
;; look like a lambda calculus expression.  This involves removing the
;; "bound" and "free" prefixes from variables, removing calls to
;; `lc-apply', and making the resulting expression as minimal as
;; possible.

;; * Credits:
;;
;; Most of the environment comes from the Types and Programming
;; Languages textbook by Benjamin C. Pierce.  As far as I can tell,
;; these sample definitions are considered common knowledge, so I
;; should be able to distribute them.
;;
;; A couple of the functions of the environment are from
;; <http://www.onebadseed.com/blog/?p=34>, but were modified to use a
;; single-argument lambda form.
;;
;; Thanks go to Riastradh on #emacs for suggesting that I use
;; functions rather than the macro mess I previously had.

;; * Endorsements:
;;
;; From an IRC chat on #hcoop:
;;
;;   <mwolson> i'm currently making a lambda calculus interpreter in
;;             Emacs Lisp
;;   * Smerdyakov gags. :D
;;
;; From an IRC chat on #emacs:
;;
;;   * mwolson is tantalizingly close to having the final piece of his
;;     lambda calculus interpreter (in Emacs Lisp) done
;;   <forcer> :-)
;;   <offby1> *shudder*

;;; Code:

(require 'pp)                           ; pretty-printer

;;; Options

(defgroup lc nil
  "Interpret lambda calculus expressions,"
  :group 'languages)

(defcustom lc-max-steps 50
  "Maximum number of steps permitted for evaluation."
  :type 'integer
  :group 'lc)

(defcustom lc-enable-pp t
  "Whether to enable pretty-printing."
  :type 'boolean
  :group 'lc)

;;; Helper functions

(defmacro lc-error (msg)
  "Display the given message as an error message."
  (list 'error (concat "Lambda calc: " msg)))

;;; Translation (Lambda Calculus -> Emacs Lisp)

(defun lc-bind (sym)
  "Add prefix to SYM to indicate that it is bound."
  (intern (concat "lc-bound-" (symbol-name sym))))

(defun lc-sym (bound-syms sym)
  "If SYM is in BOUND-SYMS, mark it as bound, otherwise free."
  (if (memq sym bound-syms)
      (lc-bind sym)
    (intern (concat "lc-free-" (symbol-name sym)))))

(defun lc-call (bound-syms fun &rest args)
  "Translate an implicit lambda calc function call to a call to
the `lc-apply' function."
  (let ((res nil))
    (if (not args)
        (setq res (lc-trans-1 bound-syms fun))
      (setq res (list 'lc-apply (lc-trans-1 bound-syms fun)
                      (lc-trans-1 bound-syms (car args))))
      (dolist (arg (cdr args))
        (setq res (list 'lc-apply res
                        (lc-trans-1 bound-syms arg)))))
    res))

(defun lc-trans-1 (bound-syms exp)
  "Helper function for `lc-trans'.  Tracks bound symbols."
  (cond ((atom exp)
         (if (symbolp exp)
             (lc-sym bound-syms exp)
           (lc-error "syntax error")))
        ((eq (cdr exp) nil)
         (lc-trans-1 bound-syms (car exp)))
        ((eq (car exp) 'lambda)
         (setq bound-syms (cons (cadr exp) bound-syms))
         (cons 'lambda (cons (list (lc-bind (cadr exp)))
                             (if (cddr exp)
                                 (list (lc-trans-1 bound-syms (cddr exp)))
                               nil))))
        ((consp (car exp))
         (apply #'lc-call bound-syms exp))
        ((symbolp (car exp))
         (apply #'lc-call bound-syms exp))
        (t (lc-error "syntax error"))))

(defun lc-trans (exp)
  "Translate expression EXP into an intermediary Emacs Lisp form."
  (lc-trans-1 nil exp))

(defmacro lc-def (fun exp)
  "Define a free variable FUN which evaluates to the expression EXP."
  (if (not (symbolp fun))
      (lc-error "invalid definition target")
    (let ((sym (lc-sym nil fun)))
      (list 'progn
            (list 'defvar sym nil)
            (list 'set `(quote ,sym)
                  (list 'lc-trans `(quote ,exp)))))))

;;; Untranslation (Emacs Lisp -> Lambda Calculus)

(defun lc-unsym (sym)
  "Return a new symbol with the bound/free prefix removed from SYM."
  (intern (replace-regexp-in-string "^lc-\\(free\\|bound\\)-" ""
                                    (symbol-name sym))))

(defun lc-uncall (exp)
  "Turn an instance of `lc-apply' back into the corresponding
lambda calculus expression."
  (cond ((not (consp exp))
         (lc-error "syntax error"))
        ((and (consp (cadr exp))
              (consp (car (cddr exp))))
         (nconc (if (eq (car (cadr exp)) 'lc-apply)
                    (lc-untrans (cadr exp))
                  (list (lc-untrans (cadr exp))))
                (list (lc-untrans (car (cddr exp))))))
        ((consp (cadr exp))
         (nconc (if (eq (car (cadr exp)) 'lc-apply)
                    (lc-untrans (cadr exp))
                  (list (lc-untrans (cadr exp))))
                (lc-untrans (car (cddr exp)))))
        ((consp (car (cddr exp)))
         (nconc (lc-untrans (cadr exp))
                (list (lc-untrans (car (cddr exp))))))
        (t (nconc (lc-untrans (cadr exp))
                  (lc-untrans (car (cddr exp)))))))

(defun lc-untrans (exp)
  "Turn an intermediary Emacs Lisp form back into a lambda calculus
expression, using the minimal amount of parentheses."
  (cond ((atom exp)
         (if (symbolp exp)
             (list (lc-unsym exp))
           (lc-error "syntax error")))
        ((eq (cdr exp) nil)
         (lc-untrans (car exp)))
        ((eq (car exp) 'lambda)
         (cons 'lambda (cons (lc-unsym (car (cadr exp)))
                             (if (cddr exp)
                                 (lc-untrans (car (cddr exp)))
                               nil))))
        (t (lc-uncall exp))))

;;; Evaluation

(defun lc-apply-1 (form arg val)
  "Helper function for `lc-apply'.
Traverses FORM, replacing ARG with VAL."
  (cond ((atom form)
         (if (eq form arg)
             val
           form))
        ((eq (car form) 'lambda)
         (if (eq (car (cadr form)) arg)
             form
           (list 'lambda (cadr form)
                 (lc-apply-1 (car (cddr form)) arg val))))
        ((eq (car form) 'lc-apply)
         (list 'lc-apply
               (lc-apply-1 (cadr form) arg val)
               (lc-apply-1 (car (cddr form)) arg val)))
        (t (lc-error "syntax error during application"))))

(defun lc-apply (form val)
  "Apply the replacement VAL to FORM."
  (let ((lc-eval-current-depth (1+ lc-eval-current-depth)))
    (funcall lc-eval-filter (list 'lc-apply form val))
    (setq form (lc-eval-1 form))
    (if (or (atom form) (not (eq (car form) 'lambda)))
        (lc-error "invalid application")
      (lc-apply-1 (car (cddr form)) (car (cadr form)) val))))

(defvar lc-eval-filter #'identity
  "The function to call for each step of evaluation.
It should take one argument and avoid modifying it.")

(defvar lc-eval-current-depth 0
  "The depth of the current evaluation step.")

(defun lc-eval-1 (exp)
  "Helper function for `lc-eval'.
Evaluates expression EXP until either
  1. The result is a lambda form.
  2. The number of evaluation steps exceeds `lc-max-steps'."
  (let ((steps 0)
        (lc-eval-current-depth (1+ lc-eval-current-depth)))
    (while (or (atom exp) (not (eq (car exp) 'lambda)))
      (when (> steps lc-max-steps)
        (lc-error "maximum number of eval steps reached"))
      (funcall lc-eval-filter exp)
      (setq exp (eval exp))
      (setq steps (1+ steps)))
    exp))

(defmacro lc-eval (exp)
  "Evaluate the lambda calculus expression EXP."
  (setq exp (lc-trans exp))
  (let ((lc-eval-filter #'identity))
    (setq exp (lc-eval-1 exp)))
  (list 'quote (lc-untrans exp)))

(defun lc-eval-expression (expstring)
  "Evaluate the lambda calculus expression in string EXPSTRING."
  (interactive (list (read-string "Lambda calc expression: ")))
  (let ((exp (macroexpand (list 'lc-eval (read expstring)))))
    (setq exp (cadr exp))               ; unquote exp
    (when (interactive-p)
      (message (format "%s" exp)))
    exp))

;;; Debugging

(defvar lc-step-buffer nil
  "Buffer used for the output of `lc-step-eval'.")

(defun lc-pp-expression (header exp)
  "Pretty-print the expression EXP, using HEADER.

HEADER should be a format string with a single argument.  EXP
will be formatted according to this string."
  (let ((blanks (make-string (length (format header ""))
                             ?\s))
        (beg (point)))
    (when lc-enable-pp
      (setq exp (pp-to-string exp)))
    (insert (format header exp))
    (when lc-enable-pp
      (while (< beg (progn (forward-line 0) (point)))
        (unless (eolp)
          (insert blanks))
        (forward-line -1))
      (goto-char (point-max)))))

(defun lc-step-eval-filter (exp)
  "Filter function used to display each step of the evaluation process."
  (with-current-buffer lc-step-buffer
    (let ((header (mapconcat #'identity
                             (make-list (1- lc-eval-current-depth) "    ")
                             "")))
      (lc-pp-expression (format "%s  [%s] %%s" header
                                lc-eval-current-depth)
                        (lc-untrans exp))
      (insert "\n")))
  exp)

(defun lc-step-expression-1 (exp)
  "Helper function for `lc-step-expression'.
Shows the steps made throughout the evaluation of the lambda
calculus expression EXP."
  (when (buffer-live-p lc-step-buffer)
    (kill-buffer lc-step-buffer))
  (setq lc-step-buffer (switch-to-buffer "*LC evaluation*"))
  (insert "\n")
  (let ((lc-eval-filter #'lc-step-eval-filter))
    (lc-pp-expression "  %s"
                      (lc-untrans (lc-eval-1 (lc-trans exp)))))
  (insert "\n\nEvaluation finished\n"))

(defun lc-step-expression (expstring)
  "Evaluate the lambda calculus expression in string EXPSTRING and show
the steps made while evaluating."
  (interactive (list (read-string "Lambda calc expression: ")))
  (lc-step-expression-1 (read expstring)))

;;; Environment

;; Church Booleans

(lc-def tru (lambda t lambda f t))
(lc-def fls (lambda t lambda f f))

(lc-def test (lambda l lambda m lambda n
               l m n))

(lc-def and (lambda b lambda c b c fls))
(lc-def or (lambda b lambda c b tru c))
(lc-def not (lambda b b fls tru))
(lc-def if (lambda p lambda a lambda b p a b))

;; Pairs

(lc-def pair (lambda f lambda s lambda b
               b f s))
(lc-def fst (lambda p p tru))
(lc-def snd (lambda p p fls))

;; Church Numerals

(lc-def c0 (lambda s lambda z z))
(lc-def c1 (lambda s lambda z s z))
(lc-def c2 (lambda s lambda z s (s z)))
(lc-def c3 (lambda s lambda z s (s (s z))))
(lc-def c4 (lambda s lambda z s (s (s (s z)))))
(lc-def c5 (lambda s lambda z s (s (s (s (s z))))))
(lc-def c6 (lambda s lambda z s (s (s (s (s (s z)))))))

(lc-def scc (lambda n lambda s lambda z
              s (n s z)))
(lc-def plus (lambda m lambda n lambda s lambda z
               m s (n s z)))
(lc-def times (lambda m lambda n lambda s
                m (n s)))
(lc-def power (lambda m lambda n
                m n))

(lc-def iszro (lambda m m (lambda x fls) tru))

(lc-def zz (pair c0 c0))
(lc-def ss (lambda p pair (snd p) (plus c1 (snd p))))
(lc-def prd (lambda m fst (m ss zz)))

(lc-def subtract (lambda m lambda n n prd m))

(lc-def equal (lambda m lambda n
                (and (iszro (m prd n))
                     (iszro (n prd m)))))

;; Utilities

(lc-def identity (lambda x x))
(lc-def compose (lambda f lambda g lambda x
                  f (g x)))

;; Lists

;; Pierce's list definitions.  Tail did not evaluate correctly for me.
;;
;; (lc-def nil (lambda c lambda n n))
;; (lc-def isnil (lambda l l (lambda h lambda t fls) tru))
;; (lc-def cons (lambda h lambda t lambda c lambda n
;;                c h (t c n)))
;; (lc-def head (lambda l l (lambda h lambda t h) fls))
;; (lc-def tail (lambda l
;;                fst (l (lambda x lambda p
;;                         pair (snd p) (cons x (snd p)))
;;                       (pair nil nil))))

;; The following definitions are from the onebadseed.com page.  The
;; head, tail, and cons forms work, but it does not seem to evaluate
;; (isnil nil) correctly.
;;
;; (lc-def nil (lambda l l identity identity tru))
;; (lc-def isnil (lambda c
;;                 (lambda tr lambda fa
;;                   c (lambda f lambda s lambda t
;;                       t tr fa))))
;; (lc-def cons (lambda f lambda s lambda l
;;                l f s fls))
;; (lc-def head (lambda c c (lambda f lambda s lambda t f)))
;; (lc-def tail (lambda c c (lambda f lambda s lambda t s)))

;; The following is the "rather different approach" from Pierce's
;; text.  It was able to handle all of the test cases I threw at it,
;; so I went with it.

(lc-def nil (pair tru tru))
(lc-def isnil fst)
(lc-def cons (lambda h lambda t
               pair fls (pair h t)))
(lc-def head (lambda z fst (snd z)))
(lc-def tail (lambda z snd (snd z)))

;; Recursion

(lc-def omega ((lambda x x x) (lambda x x x)))
(lc-def fix (lambda f
              (lambda x f (lambda y x x y))
              (lambda x f (lambda y x x y))))

;; Sample functions

(lc-def factorial
        (fix (lambda f lambda n
               (test (iszro n)
                     (lambda x c1)
                     (lambda x (times n (f (prd n)))))
               ;; dummy argument
               tru)))

;; Does not seem to work; need to test this at some point.
;;
;; (lc-def sumlist (lambda l l plus c0))

;;; Test cases

(defmacro lc-test (test result)
  "Make sure that the lambda calculus expression TEST yields the
lambda calculus expression RESULT, when both are evaluated.

If they do not yield the same value, throw 'test-field with a
cons of the test and the desired result."
  `(if (equal (lc-eval ,test)
              (lc-eval ,result))
       t
     (throw 'test-failed (cons ',test ',result))))

(defun lc-consistency-check ()
  "Perform various tests to make sure the evaluator and
environment work as expected."
  (interactive)
  (let ((err nil))
    (if (setq err
              (catch 'test-failed
                ;; lists
                (lc-test (equal (head (cons c1 c2)) c1)
                         tru)
                (lc-test (equal (head (cons c1 c2)) c2)
                         fls)
                (lc-test (equal (tail (cons c1 c2)) c1)
                         fls)
                (lc-test (equal (tail (cons c1 c2)) c2)
                         tru)
                (lc-test (equal (head (cons c3 (cons c1 c2))) c3)
                         tru)
                (lc-test (equal (head (cons c3 (cons c1 c2))) c1)
                         fls)
                (lc-test (equal (tail (cons c3 (cons c1 c2))) (cons c1 c2))
                         tru)
                (lc-test (isnil nil)
                         tru)
                (lc-test (isnil (cons nil nil))
                         fls)
                (lc-test (isnil (cons c0 c1))
                         fls)
                nil))
        (message "Consistency check for %s failed: expected %s"
                 (car err) (cdr err))
      (message "Consistency checks passed"))))

(provide 'lambdacalc)

;;; lambdacalc.el ends here
