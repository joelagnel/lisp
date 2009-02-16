;;; choose.el --- nondeterminism in elisp.
;; Time-stamp: <2001-10-14 17:03:53 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: choose.el
;; Package: choose
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.5


;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar choose-home-page  
  "http://www.glue.umd.edu/~deego/emacspub/choose/")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defvar choose-quick-start
  "Drop into your load-path and insert \(require 'choose\) in .emacs.

Type M-x choose-introduction for more."
)

(defun choose-quick-start ()
  "Provides electric help for function `choose-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar choose-introduction
  "
By the way, could anyone drop a hint as to why this file does not
compile on my emacs20.3.1, specifically, it does not seem to like the
function, (choose-invertible).. whereas that function works perfectly
fine.  The error i get is:

Compiling file /homes/deego/emacs/emacspub/choose/alpha/choose.el at Thu Oct  4 15:04:48 2001
  !! Symbol's value as variable is void ((n))


choose.el provides (some) non-determinism in elisp.   Does anyone know if
such a thing exists already?

The use of this facility is best illustrated by the example-function
choose-find-mul.  Where you can accomplish the task without a single
loop, whereas otherwise you would need 9 nested loops (of course,
emacs will internally loop for you, sombody's gotta do it..).  For
more example\(s\), type M-x choose-example.

Inspiration from Paul Graham's book 'On Lisp'.  choose/fail
essentially allows the programmer to simply ask lisp to choose a
choice rather than looping over the possibilities.  And the EMACS,
with its infinite foresight :), will always ensure that the choice it
made, was the right one, the one you wanted.  It never takes the false
path of \(choose-fail\). \(Okay, there are a lot of caveats to this, see
commentary for more details..\).

Conceptual and coding help with the TODO or any other help most
welcome. "
)

(defun choose-introduction ()
  "Provides electric help for function `choose-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-introduction) nil) "*doc*"))

;;; Commentary:
(defvar choose-commentary
  "Please first see M-x choose-quick-start and M-x choose-introduction.

It seems to me that any code that uses choose will run much faster if
compiled, since then the macro-expansions will already have been
done..  For an illustration, look at the time it takes to compile
choose-find-mul. 

Now: here are the caveats mentioned in the introduction..  Whereas if
you were using a language like scheme with an inbuilt support for
continutions, you can expect the wrong path to be \(effectively\) never
taken. In other languages, any decent choose/fail implementation would
atleast try to simulate continuations, as Graham does in this.  In
choose.el, we simply chicken out and not even simulate
continuations. All that happens is that the paths are iterated over
unless the right one is found. You had better keep track of any
important environment variables yourself..  Just think of choose as
asking emacs to iterate your code over all possibilities unless one
'goes thrrough' without running into failures.

In essence, choose just saves you loops, and provides you an
arbitrarily invokable fail ability --- you can fail anywhere in the
program.  In particular, the body of your code will be evaluated once
for each possible combination until the right one is found.  You had
better not make any \(careless\) side-effects, unless you really want
them executed once for each \(wrong\) possibility as well.

Also note that there is an alternative syntax available for choose,
available by typing M-x choose-toggle-syntax.

Also see bugs. which also contains a TODO.  Any coding or conceptual
help is most welcome for the TODO. Feature-requests and suggestions are
also welcome. "
)

(defun choose-commentary ()
  "Provides electric help for function `choose-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;; There being no support for continuations in elisp (or even common
;; lisp), this is obviously a very incomplete implementation of
;; nondeterminism, and perhaps doesn't even do justice to the term.

;; This code assumes a pretty rigorous syntax for choose.  Thus, we
;; can forget about (apply 'choose (lists.. )), the only syntax is
;; (choose <stuff>). 
;; 
;; A little bit of a remedy to this situation is the provision of
;; choose-n construct, which essentially allows you to do an
;; equivalent of (mapcar 'choose).. Moreover, the syntax for choose
;; and choose-n is chosen such that you will never need (hopefully) to
;; use apply with choose...   of course, this is still not as general
;; as you would like..  when using choose-n, the value of n should be
;; known before the wrapping choose-with.    This is a bit of a
;; drawback since you sometimes don't know the value of n deep into
;; the code.  But version 0.7 will seek to overcome this drawback as
;; follows:  whereas multiple nested choose-with's have always been
;; allowed, the inner ones really have no effect, for the outer ones
;; used to take care of all the choose's in between, but from now on,
;; the outer ones will leave any blobk of inner code beginning with
;; another choose-with alone.. thus, if you determine the value of n
;; deep inside a choose-with, all you gotta do is start another blovk
;; of choose-with.



;;; New features:
(defvar choose-new-features
  "
0.5: not only do we have a choose-n (see 0.1), but the program is way
way faster now..  the equivalent looping is more intuitive, and one of
the macros has now been converted to a function. 

0.1: has also implemented a choose-n.
     Earlier, you had to know how many chooses you were going to use
     in the program.  So, you if the number of variables to be 'chosen'
     were to be determined at run-time, there was no way.

     Now, you can type use a choose-n, which will create a list of n
     objects, each 'chosen'.  Moreover, the second element of the list
     can refer to the first, the third can refer to the first and
     second, and so on while choosing.

"
)

(defun choose-new-features ()
  "Provides electric help for function `choose-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert choose-new-features) nil) "*doc*"))

(defvar choose-version "0.5"

"
0.5: will try to redefine choose-with-choose so that the innermost
choose gets iterated over first..
0.4..")

;;==========================================
;;; Code:

(eval-when-compile (require 'cl))

(defvar choose-good-syntax-p t
  "You can choose good or bad syntax by making it t or nil.  You may
toggle this using `choose-toggle-syntax'.

t => This syntax is more cumbersome to use but is more general..
in this syntax:  you will have to write out stuff like
\(choose '\(\(+ x 1\) \(- x 3\)\)\)
as opposed to  bad-syntax:
\(choose \(+ x 1\) \(- x 3\)\).   The difference is an extra paren and a quote.

The advantage  of good-syntax is  that this is very  general.  Suppose
you have a list called ls storing  '\(\(+ x 1\) x\), now you want to apply
'choose to it..  since you can't  apply when using choose, there is no
way to do so unless you are using good-syntax...  The advantage of bad
syntax is less typing..

It would be obvious by now that though  we provide a choice \(only for
choose, not for choose-n\) , we recommend the
good-syntax..

Also note that the real powerful thing, choose-n \(as of now\) *assumes*
that we are using good syntax..

ACTUALLY, BAD SYNTAX IS NOW BEING FADED OUT..
")


;;;###autoload
(defun choose-toggle-syntax ()
  "Toggle `choose-good-syntax-p'."
  (interactive)
  (setq choose-good-syntax-p
	(not choose-good-syntax-p))
  (message "choose-good-syntax-p set to %s" choose-good-syntax-p))



(defvar choose-catch-tag nil
 "Internal.  Will be a symbol holding the current `choose-catch-tag'."
)


(defun choose-fail ()
  "Use this inside programs to fail, see examples."
  (throw choose-catch-tag choose-fail-symbol)
  ;;choose-fail-symbol
)

(defvar choose-fail-symbol 'choose-failed)

(defvar choose-choose-symbol 'choose)

;;;###autoload
(defmacro choose-with (&rest body)
  "All code expecting to use choose should begin with this, see examples.
BODY is your code.



Note in particular the syntax for choose-n:

\(choose-n 'n 'var-name  \(list of n lists\) &optional initial\)
'n is an expression that should evaluate to a number.  This expression
will be evaluate exactly once, in the beginning.

The var-name, itself can be accessed within the list of n lists.


Initial is the initial list that var should be bound to.  If INITIAL
is not a list or is not n element long, then we shall assume that
INITIAL refers to each element of the list, and will create a n-long
list by repeating initial n times.


Also note that the way we have implemented choose, the first
encountered choose is the one that runs through all possibilities
before a different possibility is considered for the second choose.."
  (choose-with-choose (choose-with-choose-n `(progn ,@body))))
  ;;`(choose-with-choose (progn ,@body)))

(defun choose-with-choose-n (expr)
  "Note that i am making this one a function.  Perhaps should have
done the same with choose-with-choose.."
  (choose-replace-if
   (lambda (arg) (and (listp arg) (equal (car arg) 'choose-n)))
   expr
   (lambda (arg) (eval arg))))
   

(defun choose-with-choose (expr)
  "Internal. Just transforms the expression into something else..
Is called from `choose-with', EXPR is the expression which gets
formatted.  


"
  (let* ((replace (gensym "choose-temp-replace"))
	 (replaced-return (choose-replace-choose
			   expr (list 'eval replace))))
    (if replaced-return
	`(choose-keep-trying
	  (lambda (,replace)
	    ,(choose-with-choose (first replaced-return)))
	  ,(cadr (second replaced-return))
	  )
      expr)))


(defun choose-keep-trying (fcn args)
  "Internal to choose.
FCN  accepts only  one arg..ARGS is a list of arguments.
keeps  trying until  one arg  doesn't
fail.. else  fails.. args be a list  of args.. the arg  is actually an
expression which will be evaled.
Thus (let ((x 1)) (coose (+ x 2) (+ x 3)))
will  work  because  the  expression  '(+  x  2)  will  be  passed  to
`choose-keep-trying' and will be evaled at run-time.."
  (if (null args)
      choose-fail-symbol
    (let* ((choose-catch-tag (gensym "choose-catch"))
	   (try (catch choose-catch-tag (funcall fcn (car args)))))
      (if (equal try choose-fail-symbol)
	  (choose-keep-trying fcn (cdr args))
	try))))


(defun choose-replace-choose (expr replace)
  "Internal.
replaces one (choose... ) in an EXPR.  REPLACE is what gets inserted there.."
  (choose-replace-one-if (lambda (arg) (and (listp arg)
					(equal (car arg) 'choose)))
		     expr (lambda (arg) replace)))



(defun choose-replace-one-if (pred tree repl-pred )
  "A general lisp function. 
Returns a new TREE with replaced elements..
The new  tree has  the first element  that satisfies PRED  replaced by
the funcall of repl-pred on that element.

If ALL is true, will replace all \(not just first\) elements of the tree
satisfying the predicate.  And will keep calling the checking even on
the replaced  elements..

This  function  returns  \(NEWLIST  REPLACED\).  Where  NEWLIST  is  the
replaced list.   REPLACED is  the first element  of the LIST  that got
replaced.

Note carefully,  that when  this function is  being applied,  it tests
each  element \(the tree  as well  as all  of its  sublists as  well as
atoms\) to see if they satisfy the pred..

If could not find any match, returns nil.

NB: THe ALL functionality is not yet implemented.. because not needed
in this program."
  (if (funcall pred (copy-tree tree))
      (list (funcall repl-pred (copy-tree tree))
	    (copy-tree tree))
    (choose-ifn
     (listp tree)
     nil
     (let*
	 ((subreplaceds
	   (mapcar (lambda (arg)
		     (choose-replace-one-if
		      pred arg repl-pred ))
		   tree))
	  (index
	   (position-if (lambda (arg)
			  arg)
			subreplaceds)))
       (choose-ifn
	index 
	;; if no match, please return nil
	nil
	(let ((sub (nth index subreplaceds)))
	  (list
	   (append (subseq tree  0 index)
		   (list (car sub))
		   (subseq tree (+ index 1) (length tree)))
	   (second sub))))))))


  
(defmacro choose-ifn (a b c)
  "A general utility. Like if but reverse.  A B C."
  `(if ,a ,c ,b))

(defun choose-failed-p (arg)
  "A good way to test if something resulted in choose-failure.
Preferably use this rather than testing yourself.  I don't know why.
Tells if ARG is same as 'choose-failed-symbol."

  (equal arg choose-fail-symbol))



(defun choose-n (varq ls &optional initlist nolet-p
			     index)
  " Behaves differently whether used inside or outside choose-with.
The intended use is inside choose-with.

This shall return an equivalent choose-expression..

varq is a
quoted variable.  ls is a list of sublists.  Choose will choose one
item from each of those sublists.  initlist is an optional list
\(ideally of the same length as ls.  If supplied, this is what varq
gets initially assigned to.  varq is normally initialized using a let.
But if nolet-p is true, then it is not.  \(used in recursive
self-calls\).  Similarly, if for some reason you want only the 5th
through 7th entries of varq to be 'chosen', then make the initlist 7
elements long, make the ls only 3 elements long.  And make the
index 4.  Thus, index is the starting index for which we will start
doing \(set \(nth index varq\) \(choose....\)\).  Again, this is
useful because called by the function for self. 

Thus, remember that each member of ls is itself a list of expressions..
Note that choose-n will go and set the variable you specify to the
chosen values.  If you intended to do that only locally, you will have
to use your own let wrapper around choose-n.




"
  (if (null index) 
      (setq index 0))
  (if (null initlist)
      (setq initlist 
	    (make-list (length ls) choose-fail-symbol)))
  (choose-ifn 
   nolet-p
   `(progn 
      (setq ,varq ',initlist)
      ,(choose-n varq ls initlist t index))
   (if (>= index (length ls))
       nil
     `(progn
	;; This one is to prevent unintended consequences for the sick
	;; few :-)  who use choose-n to loop rather than to choose.
	(setf ,varq (copy-tree ,varq))
	(setf (nth ,index ,varq) (choose ',(nth index ls)))
	,(choose-n varq ls (cdr initlist) t (+ index 1))))))


(defun choose-replace-if (pred tree repl-pred &optional not-root)
  "Replaces all matches in the tree that satisfy pred..returns the
replaced-tree.. see also the doc. of choose-replace-one-if..  returns
the original tree if no match could be found.   If not-root is true,
the immediate root is not tested against pred. 
 "
  (if not-root
      (if (or (null tree)
	      (not (listp tree)))
	  tree
	(mapcar 
	 (lambda (arg)
	   (funcall 'choose-replace-if pred arg repl-pred))
	 tree))
    (choose-replace-if 
     pred
     (if (funcall pred tree)
	 (funcall repl-pred tree) 
       tree)
     repl-pred
     'notroot)))

;;;====================================================
;; The rest of this file is some examples.

;;;###autoload
(defun choose-example ()
  "Run this."
  (interactive)
  (message "Try: M-x choose-find-mul, choose-define-invertible"))

;;;###autoload
(defun choose-find-mul ()
  "The problem is to choose 3 3-digit numbers.  Their sum should be
divisible by 7 and 13.  Each of the numbers should be progressive.
Let's define a progressive number as one in which every digit is
either 1+ or twice the digit to the right of it.  Thus, 632 is a
progressive number..  now imagine writing this program without using
choose.. the 9 nested loops you would need.. and now look at the
0-loop program below...

Thus, we have 3 progressive numbers, added ...
 a b c
 d e f
+g h i
=====
<multiple of 7 and 13>
=====

this example also illustrates that expressions can
be used inside choose's..  

On my machine, the numbers do not get too big for elisp.."
  (interactive)
  (message "hang on for 2 minutes..")
  (choose-with
   (let* (
	  (c (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (f (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (i (choose '(0 1 2 3 4 5 6 7 8 9)))
	  (b (choose '((* c 2) (+ c 1))))
	  (a (choose '((* b 2) (+ b 1))))
	  (e (choose '((* f 2) (+ f 1))))
	  (d (choose '((* e 2) (+ e 1))))
	  (h (choose '((* i 2) (+ i 1))))
	  (g (choose '((* h 2) (+ h 1)))))
     (if (or (> a 9) (> d 9) (> g 9) (= a 0) (= d 0) (= g 0))
	 (choose-fail))
     (message "Trying a:%S, b:%S, c:%S, d:%S, e:%S, f:%S, g:%S, h:%s i:%s"
	      a b c d e f g h i)
     (let ((sum
	    (+ (* 100 (+ a d g))
	       (* 10 (+ b e h))
	       (+ c f i))))
       (if (and (zerop (% sum 7)) (zerop (% sum 13)))
	   (message
	    (concat 
	     "Progressives %s, %s and %s sum to %s "
	     "which is divisible by 7 and 13")
	    (+ (* 100 a) (* 10 b) c)
	    (+ (* 100 d) (* 10 e) f)
	    (+ (* 100 g) (* 10 h) i)
	    sum)
	 (choose-fail)))))
  (sit-for 5)
  (message "Done.."))
  
  
;;;###autoload
(defun choose-invertible-4 ()
  "Get a 4-digit number which should divide its reverse..
Defining this function using loops might be more cumbersome.. (unless
you simply start with 1001 and use incf.. which is really a trick in
the current context..)"
  (interactive)
  (message "Hang on for a minute..")
  (choose-with
   (let (
	 (a (choose '(1 2 3 4 5 6 7 8 9)))
	 (b (choose '(0 1 2 3 4 5 6 7 8 9)))
	 (c (choose '(0 1 2 3 4 5 6 7 8 9)))
	 (d (choose '(0 1 2 3 4 5 6 7 8 9))))
     (message "Trying %S %S %S %S " a b c d)
     (let ((number
	    (+ d (* 10 c) (* 100 b) (* 1000 a)))
	   (revnum
	    (+ a (* 10 b) (* 100 c) (* 1000 d))))
       (if (= number revnum) (choose-fail))
       (if (= (% revnum number) 0)
	   (message "%S divides its reverse" number)
	 (choose-fail)))))
  (sit-for 5)
  (message "Done"))



(defun choose-maze-solve ()
  "Still in progress.."
  (interactive)
  (setq choose-good-syntax-p t)
  (require 'maze)
  (call-interactively 'maze)
  (message "waiting for 3 seconds...")
  (sit-for 3)
  (while
      (not (maze-check-win
	    (let ((choices (choose-maze-get-choices)))
	      (funcall (choose choices)))))))








(defun choose-define-invertible ()
  "  This is a 2-stage defition for a reason.. we want choose.el to
compile, but:
Functions that call macros sometimes do not compile:
the function choose-invertible will not compile for because it
calls a macro which, even though not recursive, can lead to possibly
infinite expansion (even though it never will at run-time..).. or
basically, i dunno why it does not compile... "
  (interactive)
  (eval 
   '(defun choose-invertible (&optional n)
    "Works just fine now.."
    (interactive)
    (if (or (null n) (< n 2))
	(setq n 2))
    (message "Trying n= %s" n)
    (if (choose-failed-p
	 (choose-with
	  (let (num number reverse)
	    (choose-n 'num (cons '(1)
				 (make-list (- n 1)
					    '(0 1 2 3 4 5 6 7 8 9))))
	    (setq number (choose-make-number num)
		  reverse (choose-make-number (reverse num)))
	    (message "Considering %s" number)
	    (when (= number reverse)
	      (message "number %S equals its reverse, failing.." number)
	      (choose-fail))
	    (if (zerop
		 (% reverse number))
		(progn
		  (message
		   "Number %s divides its reverse %S" number reverse)
		  (sit-for 5))
	      (choose-fail)
	      ))))
	(choose-invertible (+ n 1)))))
  (message "Please type M-x choose-invertible now"))

(defun choose-make-number (ls)
  (if (null ls)
      (error "null list")
    (if (= (length ls) 1)
	(car ls)
      (choose-make-number
       (cons (+ (* 10 (car ls))
		(cadr ls))
	     (cddr ls))))))
(provide 'choose)

;;; choose.el ends here
