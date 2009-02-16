;;; cl-guide.el --- an interactive guide to the CL package

;; Copyright (C) 2007  Dave O'Toole

;; Author: Dave O'Toole <dto@gnu.org>
;; Keywords: lisp, convenience, languages, tools
;; Time-stamp: <2007-10-23 12:40:53 dto>
;; $Id: cl-guide.el,v 1.4 2007/09/20 07:29:08 dto Exp dto $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file's home is http://dto.freeshell.org/e/cl-guide.el

;;; *1 Preface

;; The present work is a tutorial and usage guide for the CL package
;; included with Emacs. I wrote it in order to share what I have
;; learned about programming in Emacs Lisp.

;;; *1.01

;; This document is not intended to replace the CL Info manual, nor is
;; it a tutorial for Common Lisp. In particular, I do not discuss the
;; many differences between Common Lisp and the CL package, because
;; the CL manual already does that.

;;; *1.1 Conventions

;; In order to avoid ambiguity in what follows, we will use "Common
;; Lisp" to refer to the language Common Lisp, and write "CL" or "the
;; CL package" when we mean the Emacs Lisp package.

;;; *1.11

;; This text uses the Peano numbering system. It is explained in
;; http://dto.freeshell.org/e/peano.el

;;; *1.12

;; Some of the remarks are of a more personal nature, and express my
;; opinions on the issues being discussed. Others are tangential to
;; the discussion. The numbers of such remarks are prefixed with two
;; asterisks, to indicate that they are optional reading.

;;; *1.2 

;; Evaluating this file is probably not what you want, because it
;; would just run all the examples one after the other. Instead, visit
;; the file in GNU Emacs and execute the top-level forms by hand as
;; they appear in the course of the text.

(error "Loading cl-guide.el is probably not what you want; visit \
the file instead.")

;;; *1.3 Overview

;; GNU Emacs includes a package called "CL" that implements a large
;; subset of Common Lisp. Although it can be used to port Common Lisp
;; code, its main purpose is to provide Emacs Lisp developers with a set
;; of powerful programming tools. Here is an excerpt from the CL manual:

;; >     Common Lisp is a huge language, and Common Lisp systems tend
;; >     to be massive and extremely complex.  Emacs Lisp, by
;; >     contrast, is rather minimalist in the choice of Lisp features
;; >     it offers the programmer. As Emacs Lisp programmers have
;; >     grown in number, and the applications they write have grown
;; >     more ambitious, it has become clear that Emacs Lisp could
;; >     benefit from many of the conveniences of Common Lisp.

;; With CL, you can write your Emacs extensions in a hybrid dialect of
;; Emacs Lisp and Common Lisp. Everything in Emacs Lisp is still
;; available, and you can use as few or as many of the CL constructs
;; as you like.

;;; *1.4

;; There are a few caveats. The manual's comment about Common Lisp
;; systems being "massive and extremely complex" is quite true. The CL
;; manual lists more than 200 functions and macros, and it is not
;; always clear at first what you would use many of these constructs
;; for. (I still do not understand all of them.)

;;; *1.401

;; Learning to use CL effectively takes a significant investment in
;; time and effort. But it pays double, because if you become
;; proficient in the use of the CL package, you will have understood
;; how to use a very large chunk of Common Lisp. With some further
;; effort you can begin working more or less fluently in both Common
;; Lisp and Emacs Lisp, and share code between them.

;;; **1.402

;; Because it then becomes necessary to juggle the two dialects and
;; their many differences, the reader who wishes to take this approach
;; should read the entire CL Info manual at some point, wherein all of
;; these differences are documented. Reading the entire Emacs Lisp
;; Manual is also recommended.

;;; **1.403 

;; Because it makes heavy use of macros to transform Emacs Lisp, CL
;; showed me that I could use macros to bend and shape the language to
;; my needs. Before long I could implement a new object-oriented
;; language within Emacs Lisp.

;; For more on macros, see http://en.wikipedia.org/wiki/On_Lisp

;; My OOP project is at http://dto.freeshell.org/notebook/Eon.html

;;; **1.41

;; Using CL's constructs may make your program harder to understand
;; for people who are not familiar with them, and may even make it
;; difficult for you to contribute code to GNU Emacs. If these are
;; concerns for you, you may wish to see the Appendices for a
;; discussion of the GNU policy and ways of coping with it.

;;; **1.411

;; My only concern is to write the best possible programs that I can.
;; I program the way I want to, choosing tools I deem suited to the
;; task before me, and doing my best to avoid dogma and extremism.

;;; **1.42

;; Some programmers criticize Common Lisp for its size and complexity,
;; which do indeed create their own problems. Common Lisp is also a
;; multi-paradigm language; this can give the impression that it has
;; no "design center". But I see richness instead of size; it is more
;; like the size of a room than the size of something you must carry.

;; Interesting article: http://www.lambdassociates.org/lC21.htm

;;; **1.4211

;; Whatever lack of conceptual purity Common Lisp may suffer from
;; might also be seen as a refusal to enforce any "one true way" of
;; programming.

;;; **1.421

;; The Common Lisp standard was designed to codify existing practices
;; and to establish a single, portable, powerful common dialect of
;; Lisp. It is not an official proclamation telling you how you should
;; program. Its designers do not deny you useful constructs just to
;; keep the language specification short. Instead, the language helps
;; you write shorter programs.

;;; **1.422

;; Common Lisp may have been designed by a committee, but it was one
;; hell of a committee. Brilliant people from Digital Equipment
;; Corporation, Carnegie-Mellon University, MIT, Stanford, Symbolics,
;; UC Berkeley, Xerox PARC, and more put their brains together for
;; several years and came up with Common Lisp. 

;;; **1.43

;; I hope the examples in this document will encourage people to
;; experiment with the CL package and decide for themselves. In my
;; opinion, learning CL is time well spent. It has freed me to tackle
;; more interesting problems:

;; http://dto.freeshell.org/notebook/Eon.html
;; http://dto.freeshell.org/notebook/RogueLike.html

;;; *2 Program structure

;; We now explore in detail what I consider the most important
;; constructs from each part of the CL package. This document follows
;; (albeit roughly) the structure of the CL Info manual, so
;; you can read the two side-by-side if you like, and use Info to look
;; up functions that are mentioned here but not described in detail.

;; Use the command `eval-last-sexp' to run the example programs. Just
;; place the cursor after the sexp, and hit C-x C-e.

;; First we will load the CL package into Emacs:

(require 'cl)

;;; *2.1 Enhanced function argument lists

;; Sometimes I write functions that take several different optional
;; arguments. More often, I use an existing library with such
;; functions (standard Emacs Lisp has several.) When you want to
;; supply `x' as just one of the optional arguments to a function that
;; takes several, and the one you want to supply happens not to be the
;; first, you end up writing things like

;; (foo bar nil nil x)

;; I sort of cringe when I have to do this. Furthermore, if the
;; defaults are not `nil' then you also have to look up what are
;; acceptable values to pass for the arguments that you did not want
;; to specify in the first place. For example, `make-hash-table' would
;; arguably be harder to use if did not employ keyword arguments, of
;; which it takes five---and several of the defaults are non-nil.

;; Speaking of default argument values, in plain Emacs Lisp all
;; optional arguments default to nil. This means you cannot tell
;; whether the programmer provided nil on purpose, or just didn't
;; supply any value at all. This seems wrong to me, sort of like using
;; the integer 0 to represent "FALSE" in the C language. Anyway, if you
;; want the default to be something else, you can take nil as an alias
;; for the value you actually want, and write something like (or value
;; default-value) when you need the value. If you want to refer to the
;; value more than once, you should probably introduce a `let' binding
;; instead of writing "(or ..."  over and over again. If you have
;; several optional arguments, you need more bindings and more
;; expressions involving `or'.

;;; *2.11 defun*

;; You can change this by using `defun*' instead of ordinary
;; `defun'. It gives you everything `defun' does, plus named keyword
;; arguments and default argument values. (There are a few more
;; things, but these will be discussed later.) Here is an example:

(defun* foo (a &optional b (c 5) 
	       &key d (e (user-login-name)))
  (list 'a a 'b b 'c c 'd d 'e e))  

(foo 5)  
(foo 7 6) 
(foo 7 6 "smith" :d 9 :e "jones") 

;;; *2.111

;; The functions defined by `defun*' are ordinary Emacs Lisp functions
;; with a bit of argument parsing code tucked in front. No special
;; handling is required to `funcall', `apply', or `mapcar' them.

;;; *2.12

;; The CL package also provides `defmacro*', `function*', and
;; `defsubst*' which have the same additional features as
;; `defun*'. (Supposedly the implementation of `defsubst*' can be
;; faster than plain `defsubst', but I am not sure how much this
;; matters.)

;;; *2.13 &allow-other-keys

;; If you want to allow the user to pass keywords not explicitly
;; mentioned in the argument list, you may also write
;; `&allow-other-keys' in the argument list after the names of your
;; keyword arguments. This is useful when you need to read some of the
;; arguments and then pass the rest on to another function. In
;; addition, CL argument lists may be recursive; this is called
;; `destructuring' and will be covered later in this guide.

;;; *2.131

;; Here is another example of `defun*', and its macroexpansion:

(defun* fnorb (name address city state zip-code 
		    &rest other-keys &key +4-code suite (country "USA")
		    &allow-other-keys)
  (list name address city state zip-code +4-code suite
	country other-keys)) 

(defun fnorb (name address city state zip-code &rest other-keys)
  (let* ((+4-code (car (cdr (memq ':+4-code other-keys))))
	 (suite (car (cdr (memq ':suite other-keys))))
	 (country (car (cdr (or (memq ':country other-keys) '(nil "USA"))))))
    (list name
	  address
	  city
	  state
	  zip-code
	  +4-code
	  suite
	  country
	  other-keys)))

;; (The expansion's generated docstring has been omitted for clarity.)

(fnorb "David O'Toole" "XXXXXX" "Northboro" "MA" "01532"
       :+4-code "1429" :country "Emacsia" :suite "B" 
       :notes "Leave package for customer in front of garage.")

;; Cultural note: `zip-code' refers to a United States postal code,
;; and `+4-code' refers to the optional four-digit extension that can
;; identify different parts of a town or city.

;;; **2.14

;; To learn more about CL's argument list extensions, visit the
;; relevant manual page:

(info "(cl)Argument Lists") 

;;; **2.15

;; Many of the CL operators we will discuss in this guide accept
;; optional keyword arguments whose values modify the function's
;; behavior. If it is often the case that you will in one place
;; provide one keyword, and in another place provide two other
;; keywords because you use the function in several different ways, it
;; makes sense to use named keyword arguments that can appear in any
;; order. Then you can avoid ugly constructs like (foo bar nil nil t)
;; at call sites.

;;; **2.151

;; Keyword arguments can also make code clearer---in part by making it
;; more verbose, in part by letting you leave out arguments that are
;; not relevant to the caller's request. Here is a code snippet that
;; uses the standard `make-network-process' function:

'(make-network-process :name "my-server"
		       :local local
		       :server t
		       :service my-base-port
		       :filter 'my-process-filter
		       :sentinel 'my-process-sentinel
		       :buffer my-console)

;; TODO find a better example than this

;; In my opinion this is much easier to read. As mentioned before, you
;; do not have to supply the arguments in any particular order, and
;; you can supply just the arguments you need to.

;;; **2.152

;; Overuse of keyword arguments can make programs unnecessarily
;; verbose. I try to use them sparingly.

;;; **2.153

;; Another possible drawback to keyword arguments is that you may want
;; to change the name of the argument later. If the change is
;; superficial, and would otherwise be backward-compatible with
;; existing calls, you can make your function continue to accept the
;; old argument, and just use that value when the caller supplies
;; it. If the function changes so much that this extra line of
;; compatibility code still isn't enough (such as when you change the
;; expected type(s) of arguments) then you are probably redesigning
;; the function anyway, in which case it may be a better idea to stop
;; accepting the old argument name, and just break the old code whose
;; semantics might subtly change if you forget to revise some of the
;; call sites. That way, you will be sure to get an error when you hit
;; one of these no-longer-compatible calls.

;;; **2.154

;; For what it's worth, I rarely change the name of a keyword argument
;; unless I am redesigning the function itself (as when refactoring).
;; Often I will change its name at the same time.

;;; **2.16

;; With `defun*' and friends, you also get automatic binding of values
;; in a property list to corresponding local variables; you can
;; `apply' a function taking keyword arguments to a property list,
;; when you might otherwise have passed a plist to your function and
;; individually extracted each of its values with `plist-get'. (You
;; can also do this with `destructuring-bind'; see the section on
;; destructuring.)

;;; *3 Predicates

;;; *3.1 Type predicates

;; Many functions accept more than one type of value for some
;; arguments. For example, `get-buffer-create' and `display-buffer'
;; can accept either string buffer names, or buffer objects
;; themselves. For each type there is a corresponding predicate;
;; `bufferp', `stringp' and `integerp' are just a few.

;; With the CL Type Predicate facility, you can test objects against
;; more general "type specifiers" with the function `typep'. Instead
;; of defining a predicate for your type, you describe the type, and
;; CL works out the predicate itself.

(integerp 7)
(integerp 9.5)
(typep 7 'integer)
(type-of 7)
(type-of (current-buffer))
(type-of (make-hash-table))

;;; *3.11 More complex type specifiers

;; You can specify a type whose members are the integers (or floats,
;; etc) in a given range:

(typep 8 '(integer 0 7))
(typep 1138 '(integer 5 *)) 
(typep pi '(real 1 9))

;; Enumerated types:

(typep 0 '(member -2 -1 0 1 2))

;; Combinations of types:

(typep :foo '(or keyword integer))
(typep 8675309 '(or keyword integer))
(typep 1 '(and integer (not (member 0))))

;;; *3.12 Named types

;; If you need to test objects against one of your types frequently,
;; you can name the type:

(deftype natural-number () 
  '(integer 1 *))

(typep 7 'natural-number)
(typep -1 'natural-number)

;;; *3.13 typecase

;; When you need to handle an object differently depending on what
;; type it is, you can use `typecase':

(mapcar (lambda (x)
	  (typecase x
	    (float (floor x))
	    ((and natural-number (integer 8 *)) 'hello)
	    (natural-number (+ 1 x))
	    (otherwise nil)))
	(list 8.5 2 pi -1 0 20))

;; This is arguably a bit clearer than the `cond' that it expands
;; into:

(mapcar (function
	 (lambda (x)
	   (cond ((floatp-safe x)
		  (floor x))
		 ((and (and (integerp x) (>= x 1))
		       (and (integerp x) (>= x 8)))
		  'hello)
		 ((and (integerp x) (>= x 1))
		  (+ 1 x))
		 (t nil))))
	(list 8.5 2 pi -1 0 20))

;; And there is no chance that you will flip a sign by mistake when
;; testing whether a numeric value is within some range.

;;; **3.14

;; You can use `cl-prettyexpand' to print out macro expansions like
;; the one above. Understanding the generated code is important for
;; understanding how CL works, and helps a great deal during
;; debugging.

;;; *3.15

;; When the built-in type specifiers are not powerful enough, you can
;; write your own type predicate and use the `satisfies' specifier:

(deftype my-type () '(satisfies my-type-p))

;; Now you can use `my-type' with `typep' and `typecase'.

;;; **3.16

;; For more (including a discussion of the extended equality
;; predicates `eql' and `equalp') you can read the relevant section of
;; the CL manual:

(info "(cl)Predicates")

;;; *4 Control structure

;; This is where it gets interesting.

;;; *4.01 Parallel assignment: psetq

;;; *4.1 Generalized variables

;; Normal Lisp variables give names to Lisp values. Generalized
;; variables are an abstraction of this concept. A "place form" is an
;; arbitrary Lisp expression denoting some spot where you would like
;; to read data from or write data to. When you evaluate a place
;; form, the result is the value of the place:

(defvar places)
(setq places '(a b c))
(car places)
(cdr places)
(defvar array-places)
(setq array-places [1 2 3 4 5])
(aref array-places 2)

;; All this is very familiar. But if you use the CL macro `setf'
;; instead of `setq', you can also assign to such forms:

(setf (car places) 'x)
places
(setf (cdr places) '(y z))
places
(setf (aref array-places 2) pi)
array-places
(setf array-places [a b c])
array-places

;;; *4.11

;; `setf' performs a compile-time "inversion" to turn an expression
;; describing a value's location (i.e. a place form) into an
;; expression that actually sets the value. The first three `setf'
;; forms above expand at compile-time into the following:

(setcar places 'x)
(setcdr places '(y z))
(aset array-places 2 pi)

;;; *4.12

;; 'setf' works with many functions besides `car' and `cdr', including
;; `nth', `nthcdr', `gethash', `symbol-plist', and `getf' (which
;; allows you to treat values in property lists as place-forms.)

(setf places '(:foo bar :baz quux))
(setf (getf places :baz) 'fnorb)
places

;; The second expression expands into:

(let ((--cl-setf-- (cl-set-getf places :baz 'fnorb)))
  (setq places --cl-setf--)
  'fnorb)

;;; **4.13

;; The CL manual includes a long list of other Emacs Lisp functions
;; that can also be used as place-forms for `setf'. Some examples are
;; `buffer-name', `buffer-substring', `point', `selected-window',
;; `x-get-selection', and so on.

(defvar mystring)
(setf mystring "abcde")
(setf (substring mystring 2 4) "X")
mystring

;;; *4.14

;; Using `setf' means that you no longer have to remember the various
;; pairs of getters and setters, i.e. whether the setter for
;; `buffer-name' is called `set-buffer-name' or `rename-buffer', and
;; so on. Instead you can just use the "getter" forms with `setf', and
;; forget about the setter forms.

;;; **4.15

;; Places are one of the most useful concepts in CL. Other macros that
;; operate on place-forms include `incf', `decf', `push', `pop',
;; `pushnew', `rotatef', and `letf'.

;;; *4.16 defsetf

;; You can easily extend `setf' to accept new place forms (for
;; example, for making generalized variables out of some data type you
;; have defined.) The easiest way is to define a setter function and a
;; getter function, and pass both to `defsetf':

(defun real-part (complex-number)
  (first complex-number))

(defun set-real-part (complex-number value)
  (setf (first complex-number) value))

(defun imaginary-part (complex-number)
  (second complex-number))

(defun set-imaginary-part (complex-number value)
  (setf (second complex-number) value))

(defsetf real-part set-real-part)
(defsetf imaginary-part set-imaginary-part)

(defvar complex)
(setf complex '(2 2))
(setf (real-part complex) 0.1)
(setf (imaginary-part complex) pi)
complex

;; TODO better example

;;; **4.17

;; If you need something more powerful than `defsetf', you can use the
;; additional facilities documented in the manual:

(info "(cl)Customizing Setf")

;;; **4.18 callf

;; You can use `callf' to destructively apply a function to a place:

(defvar i)
(setf i 1)
(callf 1+ i)
i

(defvar keys)
(setf keys '(:foo :bar :baz))
(callf union keys '(:quux :bar :fnorb))
keys
(callf reverse keys)

;; As with some other constructs discussed in this guide, `callf' is
;; probably most useful for writing macros. If you like writing macros
;; like I do, you may get some mileage out of `callf'. 

;;; *4.29 letf and symbol-macrolet

;; TODO Move this stuff to the Variable Bindings section

;; With `letf' you can temporarily bind any number of places to
;; different values. Within its body, `symbol-macrolet' substitutes a
;; place-form (or any other form) everywhere a given symbol
;; appears. The form is then disguised as an ordinary local variable.

(defvar stuff)
(setf stuff '(a (:foo 1 :bar 2) c))
(symbol-macrolet ((x (first stuff))
		  (y (second stuff))
		  (z (third stuff))
		  (q (getf (second stuff) :bar)))
  (setf z 'q)
  (setf (getf y :baz) 3)
  (setf q 'hello)
  stuff)

;; So you can bind local variables to place-forms, not just values.

;;; **4.20

;; `symbol-macrolet' is more useful than the preceding example makes
;; it look. We will explore this later on.

;;; *4.21 destructuring-bind

;; If you just want to extract values from some structured list and
;; not store their values back, you can use `destructuring-bind':

(setf stuff '(1 2 3 :foo bletch :bar plic :baz quux))
(destructuring-bind 
    (a &optional b c &key foo bar baz) stuff
  (list 'a a 'b b 'c c 'foo foo 'bar bar 'baz baz))

;; TODO demonstrate more of what destructuring-bind does

;; This is very convenient when you use one of those Emacs Lisp
;; functions that returns a big data structure, or when you need to
;; repeatedly extract data from your own lists.

;;; **4.22

;; Notice how the first argument to `destructuring-bind' is an
;; argument list of the kind you would use with `defun*'. Both use
;; the same internal machinery.

;;; **4.23

;; At 18 letters, `destructuring-bind' is one of the longer
;; identifiers out there. It probably should have been called `dbind',
;; but you can impress your friends with the 18-letter version.
;; (Scheme's `call-with-current-continuation' still wins the contest.)

;;; *5 Variable bindings

;; This chapter covers CL's various generalizations of `let'. These
;; constructs can bind things other than variable names.

;;; *5.1 Dynamic bindings

;;; *5.2 Lexical bindings

;; In normal Emacs Lisp, all variables are dynamically scoped. You can
;; achieve lexical scoping with the CL package's `lexical-let'. The
;; bindings you create with `lexical-let' (or `lexical-let*') are only
;; visible to code that appears textually within the body forms. You
;; can use this to create closures.

;; TODO come up with a good example of closures

;;; *5.3 Function bindings

;;; *5.4 Macro bindings

;;; *5.5 Conditionals

;; In this chapter, we will discuss some generalizations of the
;; traditional Lisp control structures.

;;; *5.6 Blocks and exits

;; Next we will discuss several tools that make it easier to (among
;; other things) extend Emacs Lisp with macros that define something
;; like a domain-specific language.[4] Some of these are a bit too
;; powerful and could probably cause confusion (particularly `flet'.)
;; Use these constructs with care.

;; `macrolet' defines local macro definitions
;; `gensym'

;; TODO example

;; `symbol-macrolet' creates macros without arguments, for
;; substituting an arbitrary expression everywhere some symbol appears
;; within the body.

;; TODO example

;; The `labels' macro lets you create temporary, mutually-recursive,
;; lexically-scoped function definitions. I usually use this to
;; organize larger macros and make them more readable. 

;; TODO `labels' example

;; TODO case, typecase
;; TODO block/return-from
;; TODO dotimes, dolist
;; TODO `do' and `loop'. I need to figure out how to use these.

;;;; Destructuring

;; TODO `destructuring-bind'

;;;; Compiler macros
;;;; Declarations
;;;; Symbols

;;;; Numbers
;;;; Sequences
;;;; Lists
;;;; Structures
;;;; Assertions


;;; *A Plasma fractals
;;; *B Bresenham's line-drawing algorithm
;;; *C Object-oriented programming


;;; *D The GNU policy on CL

;; The following essay developed from a blog post of mine, which
;; prompted a discussion of the CL package on the emacs-devel mailing
;; list. 

;;; *B.1
;; TODO Revise all this stuff

;; By this point it should be clear how many interesting things you
;; can do with the CL package. There's so much here that Emacs Lisp
;; becomes a completely different language when you use it. You might
;; find that CL gives you so many shortcuts for so many common
;; programming tasks that you never want to go back to "plain" Emacs
;; Lisp.

;; Fortunately, the CL library is included with GNU Emacs, so you can
;; always depend on it being there. With this in mind, the following
;; excerpt from the CL manual is dismaying to say the least:

;; > Please note: the "CL" functions are not standard parts of the
;; > Emacs Lisp name space, so it is legitimate for users to define them
;; > with other, conflicting meanings.  To avoid conflicting with those user
;; > activities, we have a policy that packages installed in Emacs must not
;; > load "CL" at run time.  (It is ok for them to load "CL" at compile time
;; > only, with `eval-when-compile', and use the macros it provides.)  If
;; > you are writing packages that you plan to distribute and invite
;; > widespread use for, you might want to observe the same rule.

;; This sucks, because about half of the useful things in CL are
;; functions and not macros.

;; If I read the first two sentences correctly, it is clear that
;; the "activities" (i.e. people defining functions whose names
;; collide with CL's) are what the policy protects.

;; Presumably we protect these "activities" because they are good
;; ideas. After all, if they were bad ideas, it would be strange to
;; protect them with a policy. (Notice how the manual uses the
;; term "legitimate" and leaves the question open.)

;; But how on Earth could it be a good idea for anyone to
;; define (say) `remove-if' with a meaning that conflicts with that of
;; the `remove-if' from CL? First of all, the use of (require 'cl) is
;; widespread enough among third-party packages that you would be
;; sure to run into problems if you did this.

;; But let's say you are really dying to call your new function
;; `remove-if' (even though it does something different from what
;; knowledgeable people will think when they see the name
;; `remove-if'). So you studiously avoid loading or using any packages
;; that call `remove-if' at run-time (thereby losing the benefit of
;; those packages, CL being the most obvious example.)

;; Now imagine that another programmer also redefines `remove-if' with
;; yet another meaning; this would seem to be possible, as the
;; manual says that "users" (plural) may do this legitimately, and
;; if you think you have a good reason for doing it, then probably
;; so will he. Even if you don't distribute your package to others,
;; you might use his package, and then your definitions of `remove-if'
;; will obviously conflict. (The same result obtains if any user
;; wants to load both your packages.) And then we will be in the odd
;; position of having caused the collision that our adherence to the
;; policy supposedly prevents. Far from "`remove-if'" not being a
;; standard part of the Emacs Lisp name-space, it is actually a sort
;; of "reserved keyword" whose only impermissible definition is the
;; one from the CL package.

;; Next, we see that all Emacs Lisp developers are encouraged to
;; follow the policy, even if their code is not meant to be
;; installed in GNU Emacs:

;; > If you are writing packages that you plan to distribute and invite
;; > widespread use for, you might want to observe the same rule.

;; So if everybody follows the policy, we have no name
;; collisions. But it simply does not follow that we would have name
;; collisions if we did not follow the policy. Developers who define
;; `remove-if' in different ways would quickly figure out that doing
;; so is unworkable, and would choose different names (perhaps
;; mypackage-`remove-if', which is what everyone else does.)

;; The absurd result of this is to distribute a large library with
;; Emacs with dozens of functions that are never supposed to be
;; called. (The byte-compiler even warns you whenever you compile a
;; call to one of these functions.) And therefore any code destined
;; for inclusion in Emacs is somewhat uglified and bloated if, like
;; every program I've written so far, it would have benefited from
;; using the functions in CL. Either you implement your
;; own (possibly incorrect) versions of the functions you need, at
;; the expense of time and energy, or just use a less elegant idiom
;; to get the work done. It's a lose-lose situation.

;; So we have seen that the anti-(require 'cl) policy does not prevent
;; the name collisions that it purports to; it actually makes them
;; more likely by telling us that it's "legitimate" for people to
;; define conflicting functions with these names. As we saw above, if
;; anyone actually did this we would have a complete mess. Therefore
;; it is undesirable for people to redefine CL's functions in this
;; way. Therefore it is unreasonable to have a policy whose purpose is
;; to make such redefinitions "legitimate". Therefore it would
;; actually be better to allow (require 'cl) at run-time. Therefore:

((require 'cl) considered harmful) considered harmful))

;; I've known about this for a while, but I didn't see it as a big
;; problem until I was asked to contribute some of my code for
;; distribution in GNU Emacs. I use the CL package so heavily that I
;; would have to essentially re-write and re-debug a significant chunk
;; of it in order to comply with the policy. My programs would,
;; without question, become longer and uglier---as happened on a
;; smaller scale to the org-publish add-on code I contributed to Emacs
;; last year. I hated doing that to a few hundred lines of code; what
;; will I do when it's time to contribute as many as 10,000 lines? I
;; can go against my instincts as a programmer, and just deal with it;
;; more likely, I will simply decline to make the assignment to the
;; FSF. I submit that people deciding not to contribute stuff to Emacs
;; is unquestionably worse than unnamed "users" not being able to
;; define a function named `remove-if' that actually just displays a
;; message in the mini buffer.

;;; *Z scratch area

;; I was writing about two separate issues: the rationale for the CL
;; policy (which I find less than compelling) and what I see as the
;; deleterious effects of the policy on Emacs.

;;  (*) Despite what people say about still being able to use the macros
;; while complying with the policy, in my opinion the policy is still a
;; discouragement. You have to memorize which of its features you must
;; abstain from using (and therefore lose the benefit of those features)
;; if you are to have any hope of someday contributing Lisp code to GNU
;; Emacs.

;; This last point is what I am really getting at in my criticism of the
;; policy. I was asked to contribute two of my programs, both of which
;; make heavy use of the CL package and total upwards of 7,000 lines. I
;; am more than willing to assign them, and I already have Emacs papers
;; on file with the FSF; but it's clear now that these programs cannot be
;; accepted, because they use many CL functions. Furthermore I am not
;; willing to make such changes as would make them acceptable---it would
;; not improve the programs (probably the opposite) and as I argued
;; before, I would likely have to reimplement my own (possibly incorrect)
;; versions of the functions I want, at some expense in time and effort.

;; This has relatively little impact on me, but Emacs will lack two
;; packages that a maintainer had considered worth including. Maybe this
;; outcome doesn't bother you because you don't personally find my work
;; exciting, but it is ironic because the undesirability of this scenario
;; (i.e. the FSF not being able to include packages with Emacs that it
;; considers useful and appropriate) was adduced as an argument against
;; ELPA, and is clearly an important concern for the maintainers. But
;; that harm, only conjectural in the ELPA discussion, has already
;; resulted from the CL policy. It tells me years ahead of time that I
;; cannot contribute code to Emacs unless I eschew tools that are
;; powerful and readily understood by anyone familiar with Common Lisp,
;; and that are included in the standard Emacs distribution (yet are
;; somehow non-standard). Instead I must deliberately write programs in a
;; style I consider less expressive and less convenient and less
;; enjoyable, which I am not going to do. And I think some other cl.el
;; users will feel the same, despite how much they would like to improve
;; Emacs. If the use of the CL package were to become more widespread
;; (for example if someone writes a widely-read and translated tutorial
;; like the one I wrote for org-mode) there would be a larger body of
;; code that Emacs could not include, and the harm the policy causes
;; would then become more obvious.

;; To sum up: hypothetical bad things (name conflicts that can only be
;; created by people who are not using a package-specific prefix, and
;; thus not following the guidelines anyway) are said to justify a policy
;; which creates *real* bad things (Emacs maintainers having to reject
;; perfectly good packages because they invoke other Emacs functions
;; whose names are effectively reserved) and I do not see the sense in
;; it.

;; By pointing this out I hope it will encourage us to overcome such
;; issues as which manual a function is supposed to be documented in if
;; it lacks a package-specific prefix and just find a solution.

;; If the real issue is the Emacs maintainers not wanting to maintain or
;; debug programs that use the CL functions, that would be quite
;; understandable. But then put that in the manual, and not the argument
;; about name conflicts.

;; ----

;; I do not find the policy to be consistent with its stated rationale.

;; If you define remove-if, your code cannot be included in Emacs either.
;; Also, you are violating the package-specific prefix policy.

;; <dto> i think the most important thing about the thread was to dispel the
;;       various rationales given for the policy and instead prove that it is
;;       basically rms's dislike of keyword arguments that is at issue, and when
;;       pressed on this point of WHY keyword arguments are bad he just said
;;       "i've done all I can to explain my position."   [13:34]
;; <vincenz> s/os/is
;; <tromey> yeah
;; <tromey> it wasn't very illuminating for me
;; <technomancy> it's quite a shame
;; <technomancy> i've yet to hear a compelling case against it
;; <tromey> I found it especially weird because there are things already in elisp
;; 	 that use keyword arguments
;; *** bushwakko (n=bushwakk@124.80-202-210.nextgentel.com) has quit: 
;; <tromey> like defcustom
;; <tromey> or make-network-process
;; <RetroJ> I think it's cool that they're going to put some stuff from cl.el in
;; 	 emacs proper  [13:35]
;; <dto> i have never known rms to be reticent at all about exactly why he thinks
;;       something is bad. i guess keyword arguments truly tax his denunciatory
;;       abilities
;; <technomancy> heh
;; <enberg> dto: what part of "it adds unneccesary complexity" didn't you
;; 	 understand?
;; *** z` (n=nine@209-78-110-141.ded.pacbell.net) has joined channel #emacs
;; 								        [13:36]
;; *** cluck (i=voxvirus@a81-84-74-42.cpe.netcabo.pt) has quit: Remote closed the
;;     connection
;; <dto> enberg: what is complex about keyword arguments exactly?
;; <RetroJ> I can understand both sides of the debate.  I do think common lisp is
;; 	 an unnecessarily complicated language, and not necessarily the best
;; 	 ideal for elisp to be emulating
;; <tromey> I don't know CL so I suppose I can't comment on that :).  but there
;; 	 are thing in cl.el that are useful and that random packages end up
;; 	 reimplementing  [13:37]
;; *** Ohm{}nis (n=laumingi@ctv-217-147-36-17.vinita.lt) has quit: Read error:
;;     110 (Connection timed out)  [13:38]
;; <dto> nobody will explain why keyword arguments are unnecessarily complex. the
;;       most specific term he used was "ugly".
;; *** saorge_ (n=saorge@88.197.224.96) has joined channel #emacs
;; *** Ohm{}nis (n=laumingi@ctv-217-147-36-17.vinita.lt) has joined channel
;;     #emacs
;; <forcer> The problem of keyword arguments is that it's difficult to implement
;; 	 them efficiently. The same way why hash tables are "more complex"
;; 	 than integer-indexed arrays.  [13:39]
;; <RetroJ> dunno.. they can be used for good or ill, I guess, and it just
;; 	 depends on the situation  [13:40]
;; <emss> ,orgmode
;; <fsbot> From memory, OrgMode is at
;; 	http://www.emacswiki.org/cgi-bin/wiki.pl?OrgMode
;; <forcer> And _mixing_ keyword arguments with positional arguments is complex
;; 	 for the reader.
;; *** pocket12 (i=pocket12@gateway/tor/x-9bd06ea770a28af7) has joined channel
;;     #emacs  [13:41]
;; <dto> it's not like we're implementing the CLOS meta-object protocol. i think
;;       much of the argument about the CL package, which leaves out many of the
;;       oft-criticized Common Lisp features precisely because they are so
;;       complex to implement, is skewed by people who are criticizing Common
;;       Lisp in vague ways, and not criticizing cl.el in specific ways
;; <emss> WOYD should be WOTD?  [13:42]
;; <forcer> There is one specific criticism of cl.el by RMS which I can agree
;; 	 with.
;; <emss> ohnm
;; <emss> xD
;; * RetroJ is finding it hard to understand emss
;; <dto> forcer: the code generated by defun* to extract one keyword argument
;;       (with a default value) is: (let ((country (car (cdr (or (memq ':country
;;       other-keys) '(nil "USA")))))  [13:43]
;; <emss> RetroJ: the topic
;; <forcer> RetroJ: "Hey, the topic mentioned the word 'WOYD', shouldn't that be
;; 	 'WOTD'? Oh, I got it, never mind"
;; <dto> not complex. not inefficient (memq is fast, and unless you grossly
;;       overuse keyword arguments, we will be memq'ing a list of very few
;;       elements (on the order of 5 or 6)
;; <dto> Word of Ye Day
;; <RetroJ> ah
;; <forcer> dto: O(n) is very complex compared to O(1)
;; <forcer> (I do like and use keyword arguments, btw)  [13:44]
;; <Bonkers> not if 1 is sufficiently large enough
;; <dto> forcer: the n are so small that the difference is negligible. elisp hash
;;       tables are actually slower than the O(n) alists when n<20 or so
;; <forcer> Bonkers: In this case, the constant factors are pretty much the same,
;; 	 actually  [13:45]
;; <forcer> dto: Yes, I am aware of that. Passing arguments by position doesn't
;; 	 use hash tables, though, and is a lot faster than even _calling_
;; 	 `memq'.
;; <forcer> Let alone creating the list over which `memq' iterates when calling
;; 	 the keyword function.  [13:46]
;; <dto> forcer: i can grant that, i just don't think a few keyword arguments
;;       will make much of a difference (as with so many things that aren't in
;;       inner loops)
;; *** emss (i=emss@d233-29-196.nap.wideopenwest.com) has quit: Remote closed the
;;     connection
;; <forcer> dto: Oh, I agree completely with that :-)
;; <dto> forcer: ok :-)  [13:47]
;; <forcer> As I said, I do like and use keyword arguments to functions.
;; *** ruoso (n=ruoso@static-b5-252-25.telepac.pt) has quit: "Ex-Chat"
;; <dto> besides. rms's argument was from something called "complexity of the
;;       programming interface" and he took pains to point out that
;;       "implementation complexity" was not his concern
;; <dalias> quit bitching about this cl crap.. :)  [13:48]
;; <dto> don't worry, my essay (and subsequent mindshare victory) shall be
;;       unassailable and definitive.
;; <forcer> Actually, rms' argument regarding cl.el is mainly that it pollutes
;; 	 the top-level namespace, which is why using (eval-when-compile
;; 	 (require 'cl-macs)) is ok with him.
;; *** saorge (i=mentat@88.197.224.11) has quit: Read error: 110 (Connection
;;     timed out)  [13:49]
;; <dalias> indeed
;; <dalias> cl.el conflicts with some elisp semantics iirc  [13:50]
;; <dalias> making it unsuitable to be loaded by default
;; <dto> my problem with that is that all its function names become "reserved
;;       keywords". then you and your friends can implement remove-if in
;;       incompatible ways and then can'
;; <dto> can't load each other's packages.
;; <forcer> That's why you use package prefixes.
;; *** scfrazer (i=scfrazer@nat/cisco/x-e248f72929f23423) has joined channel
;;     #emacs
;; <johnsu01> yes, but when it's in the emacs distribution, functions shouldn't
;; 	   be overwriting each other
;; <dto> johnsu01: obviously but where do you see this happening?  [13:51]
;; *** Khabarach (n=Anonymou@83.147.136.130) has joined channel #emacs
;; <dto> johnsu01: do any elisp packages define #'remove-if for example?   [13:52]
;; <dto> (within the main distro)
;; <johnsu01> isn't dolist differently?
;; <dto> johnsu01: the built in packages can't define #'remove-if on their own
;;       because that symbol is explicitly reserved (cf. elisp coding
;;       conventions) for people to define with conflicting meanings  [13:53]
;; <dto> what are you saying about dolist?  [13:54]
;; <johnsu01> doesn't cl define a dolist that is different from the core?  [13:55]
;; <johnsu01> I don't have strong feelings about this, btw, I'm just repeating
;; 	   what my impression is
;; <dto> i think so. i don't think it is so different that it changes the
;;       semantics of code that would have used the other version
;; <dto> (the dolist)
;; <johnsu01> well... that's a pretty slippery slope  [13:56]
;; <johnsu01> one could have bugs that the other doesn't, for example
;; <dto> i would be happy to fix such actually existing name conflicts. 
;; <RetroJ> same with `push' but cl's push doesn't break code written for the
;; 	 `push' in core.  it just has more features.
;; <dto> in the guidelines, everyone is urged to use a package specific prefix,
;;       with the special exception that you are free (and almost encouraged) to
;;       define any of a special set of symbols (remove-if, delete-if, some,
;;       every, notany) however you want basically without regard to whatever
;;       people are doing.  [13:57]
;; <kelvie_> how do I make it so that emacs recognizes header files as C++ files
;; 	  by default?  [13:58]
;; <RetroJ> dto: that sounds screwy
;; *** aarkerio (i=manuel@189.146.125.162) has joined channel #emacs
;; <aarkerio> hi !  what means "1" in :
;; <aarkerio> --1-:**-F1  ecourses_controller.php      (PHP
;; 	   Abbrev)--L42--C0--Top-  [13:59]
;; <aarkerio> C-x  C-s does not work anymore
;; <tromey> kelvie_: add a new entry to the front of auto-mode-alist
;; >   if such chaos is not what is intended for those symbols, 

;; TODO package.el and emacs-help (drew adams + tromey) discussion

;;; cl-guide.el ends here
