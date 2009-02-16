;;; *0 eon.el --- prototype-oriented programming for emacs lisp
                  
;; Copyright (C) 2007  Dave O'Toole

;; Author: Dave O'Toole <dto@gnu.org>
;; Keywords: lisp, oop, extensions
;; Time-stamp: <2007-11-10 19:50:28 dto>
;; Package-Version: 0.99
;; Version: $Id: eon.el,v 1.2 2007/10/31 02:48:36 dto Exp dto $
;; Homepage: http://dto.freeshell.org/notebook/Eon.html
;;   ___  ___  _ __  
;;  / _ \/ _ \| '_ \ 
;; |  __/ (_) | | | |
;;  \___|\___/|_| |_|
;;
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

;; This file is not part of GNU Emacs.

;; TODO All objects have unique identifiers.
;; TODO show example flow via eon-logging
;; TODO stress that sending/delivering/responding are all separate steps
;; TODO remove symbol conflicts with struct+.el
;; TODO change to lisp-like arg order: (@ :slot-name foo)
;; TODO evaluate keyword argument of @
;; TODO >> applies value of slot to rest of arguments
;;      i.e. (@ :slot-name object arg1 arg2 ...)
;; TODO full CL arglists
;; TODO (defmacro with-object (symbol1 symbol2 ...or *...) + self 
;; TODO create function bindings so that you can do (method slot) instead of (>> self :method slot)
;; TODO macro highlighting and indentation

;;; *1 Conventions

;; This file uses the Peano numbering system. It is explained in
;; http://dto.freeshell.org/e/peano.el

;;; *1.11

;; To avoid ambiguity in what follows, we use the term "object" when
;; referring to objects in the sense of object-oriented programming,
;; and write "Lisp value" otherwise.

;;; *1.2 Overview

;; This library adds prototype-oriented programming to Emacs Lisp. In
;; this alternative view of object-orientation, there are no classes;
;; instead, objects are "cloned" from other objects called prototypes,
;; from which the new objects may inherit data and behavior.  Methods
;; (among other things) are chosen by symbols called selectors, and
;; employ named arguments as in Smalltalk.

;;; *1.21

;; Our strategy is to provide a few primitive mechanisms---objects,
;; selectors, and messages---and make it easy to build programs with
;; them. These primitives' default behaviors are simple, and probably
;; efficient enough for a broad range of applications. (The programmer
;; can override the default behaviors if more flexibility or speed are
;; required.)

;;; *1.22

;; We use a message-passing model instead of CLOS-like generic
;; functions. Some people find this un-Lispy. But with message-passing
;; we are no longer limited to using synchronous procedure calls to
;; make objects interact. You can (for example) forward messages
;; across the network or queue them up to be sent to their recipient
;; at a later time.

;;; *1.23

;; Multiple inheritance is not supported, but one may build objects
;; that respond to multiple interfaces; the latter are called
;; "protocols".

;;; *1.24

;; Hooks are provided for on-demand loading and creation of objects
;; from various "stores", which may also accept objects for
;; serialization, storage, and transmission.  These hooks may be
;; connected to user-provided Lisp code in order to create new sources
;; and sinks for objects. 

;;; *1.241 

;; Every object is issued a permanent RFC-4122 UUID (Universally
;; Unique Identifier.)

;;; *1.25

;; There are also several convenience features for the programmer:

;; - Macros are used to add several "operators" to the language. Two
;;   of them are the slot reference operator `@' and the method call
;;   operator `>>'.  The short symbol names make it easier to type
;;   and read programs that make use of this library.

;; - Slots are valid place-forms for `setf' and related constructs.

;; - In method bodies, we use symbol-macros to make slots look like
;;   ordinary local variables, reducing verbosity even further.

;; - As an option, the operators (and several of the macros) may be
;;   font-locked as language keywords.  See `eon-do-font-lock'.

;;; **1.26

;; When writing Eon I have tried to follow the Emacs model of software
;; design, where you have a flexible core designed especially for
;; extension. Eon is a small and relatively simple program, but it
;; defines a new object-oriented language within Emacs Lisp suitable
;; for writing applications and various types of wrappers. 

;; Eon lies in a sort of middle ground: significantly more complex
;; than Common Lisp's `defstruct' facility, but far simpler than CLOS.

;;; **1.27

;; Wherever possible, I adhered to these guiding principles:

;; - Do the simplest thing that could possibly work.
;; - Combine a few primitives in many combinations.
;; - Keep the core concepts under 1,000 lines of code.
;; - Write good documentation.

;;; **1.3

;; This project's home page is:

;; http://dto.freeshell.org/notebook/Eon.html

;; A few articles about the language ideas we are borrowing:

;; http://en.wikipedia.org/wiki/Prototype-based_programming
;; http://en.wikipedia.org/wiki/Message_passing

;;; *1.4 Status:

;; This is version 0.99; almost everything is implemented and
;; working. Comments and bug reports are welcome; please send them to
;; me at dto@gnu.org

;;; *1.41

;; An example "pixel art" application is available. It is incomplete
;; but should serve to demonstrate how things work. See the
;; "Installation" section for details on trying out the example.

;;; *1.5 Installation

;;  1. Place this file in your load-path
;;  2. Byte-compile it, otherwise it will be very slow.
;;     The following lisp will compile and load eon.el:

;;     (byte-compile-file (locate-library "eon") :load)

;;  3. Put (require 'eon) in your emacs initialization file.
;;  4. Download, byte-compile, and load the example programs:

;;           http://dto.freeshell.org/e/pixel.el
;;           http://dto.freeshell.org/e/cell.el

;;     If you have placed the example programs in your load-path,
;;     then the following Lisp will compile and load them:

;;     (byte-compile-file (locate-library "cell") :load)
;;     (byte-compile-file (locate-library "pixel") :load)

;;   5. M-x pixel-demo RET
;;      There are some more detailed demos at the bottom of pixel.el.

;;; *1.7 History

;; 0.99: This is the current release.
;;       Added gensyms where appropriate.
;;       You can use CL-style type specifiers to describe
;;         the types of slot values and method arguments.
;;       You can now document individual method arguments.
;;       Restructured the message-passing code.
;; 0.98: Improved documentation throughout.
;; 0.95: First release, with examples.

;;; *2

;; This program makes extensive use of the macros and functions
;; defined in Emacs' Common Lisp compatibility layer. We must load it
;; at run-time.

(require 'cl) 

(defmacro eon-type-error (goal type object)
  "Signal a type error."
  `(error "In computing %S expected %S but received %S, %S"
	 ',goal ',type (type-of ,object) ,object))

;;; *3 Object structure

;; We model our objects with Common Lisp structures. These structures
;; are linked to one another in order to establish various
;; relationships between them. Some links are implicit, meaning that
;; they are created and maintained automatically by the system.  Other
;; links are explicit, where one object refers to another for any
;; purpose it chooses (but usually in order to send messages to the
;; other object.)

;;; *3.1

;; The most important features of an object are its slots. Each slot
;; pairs an identifying symbol called a `selector' with an arbitrary
;; Lisp value. Methods are just function-valued slots. We also keep
;; some bookkeeping data in each object, primarily in order to
;; implement implicit links.

;;; *3.2

;; Unless you are going to edit this program, don't use the accessors
;; or the `make-object' routine defined here by `defstruct'. Instead
;; use the provided macros, like `>>', `@', and `define-prototype'.

;;; *3.3

(eval-and-compile
  (defstruct object
    slots ;; property list with selectors as keys. see `eon-read-slot'
          ;; and `eon-write-slot'.
    selector ;; optional selector uniquely identifying this object.
             ;; see also `eon-find-object'
    prototype ;; optional prototype object (or its selector)
              ;; see `eon-find-prototype' and `define-prototype'.
    prototype-p ;; when non-nil, this object is itself a prototype.
                ;; this affects the cloning process; see
                ;; `eon-default-clone-method'
    referent ;; when non-nil, this object is merely a reference to
	     ;; another. the value of this slot should be either nil,
	     ;; the selector of the referent object, or the referent
             ;; object itself. see also `eon-find-referent'.
    ))

;;; *4 Modules and Selectors

;;; *4.1 Modules

;; A `module' is a set of related objects that share a common
;; namespace. Some objects within a module are named by selectors (see
;; below); others have no names, and are included in the module
;; because some other object in the module refers to them.

;; For example, my pixmap editor has a main "pixel-sheet" object
;; containing all the pixel data, the coordinates of the cursor,
;; currently selected paint tool and color, and undo history. These
;; are collected into a module. When you save the module, all the
;; objects are serialized. You can send the file to a friend, who will
;; find the cursor in the same spot, with your undo history intact,
;; and so on.

(defstruct module 
  selector ;; module name
  resolver ;; function mapping selectors to objects
  hash ;; hash table mapping selectors to objects
 )
;; TODO :~local selectors?
  
;;; *4.2 Selectors

;; Selectors are Lisp values that identify things. The interpretation
;; of a selector (i.e. which thing it selects) varies according to the
;; context in which it appears. Slots, methods, and arguments are all
;; identified by selectors. Other selectors choose objects; for
;; example, the selector `:system' identifies the runtime system.
;; Sending a message to `:system' can invoke basic services like error
;; reporting or reflection. 

;; A selector can even describe an object that has not yet been
;; created. See `eon-find-object'.

;;; *4.21

;; The user may extend and even redefine the process of selection by
;; hooking additional Lisp code into the system. In this way we can
;; use selectors to identify any data or activity accessible from
;; within Emacs. See `eon-selection-functions'.

;;; *4.22

;; We use Lisp keyword symbols as selectors; each is unique and can
;; be compared with `eq'. Other types may be added in the future.

(deftype selector () 'keyword)

(defsubst selector-p (s)
  (typep s 'selector))

;;; *4.23 Universally Unique Identifiers

;;

;;; *4.3 Finding Objects

(defvar eon-selectors->objects (make-hash-table)
  "Hash table mapping selectors to objects.
This is the first place we will look for an object corresponding
to a given selector; see `eon-find-object'.")

(defsubst* eon-put-selector (selector object)
  (puthash selector object eon-selectors->objects))

(defsubst* eon-get-selector (selector)
  (gethash selector eon-selectors->objects))

(defvar eon-selection-functions ()
  "List of functions to use in converting selectors to objects.
Each function should accept a selector, and return an object
corresponding to that selector (or nil, of none can be found.)")

(defun eon-find-object (selector)
  "Obtain the object identified by SELECTOR.
The table `eon-selectors->objects' is searched first, in which
case the retrieved object is returned.  Otherwise, the functions
in `eon-selection-functions' are called until one returns an
object."
  (or (eon-get-selector selector)
      (eon-put-selector selector
			(some (lambda (f)
				(funcall f (symbol-name selector)))
			      eon-selection-functions))))

(defun eon-find-referent (object)
  "Return OBJECT's referent, if any.
If OBJECT's referent slot is a selector, replace the selector
with the referent object returned by `eon-find-object'.

Unless you are hacking eon itself, don't use this function in
Lisp programs.  Instead, use `eon-read-slot' and `eon-write-slot'
and their aliases, which handle references automatically."
  (let ((referent (object-referent object)))    
    (typecase referent
      (object referent)
      (selector (setf (object-referent object)
		      (eon-find-object referent)))
      (otherwise (eon-type-error eon-find-referent 
				 (or object selector) referent)))))

(defsubst* eon-find (whatever)
  "Return an object corresponding to WHATEVER, if any.

This is a catch-all function that tries to get an object any way
it can.  If WHATEVER is an object, return it (or its referent, if
any.)  If WHATEVER is a selector, look up the selector.  Other
types may be added in the future."
  (typecase whatever
    (object (if (object-referent whatever)
		(eon-find-referent whatever)
	      whatever))
    (selector (eon-find-object whatever))
    (otherwise (eon-type-error eon-find (or object selector) whatever))))

;;; *4.4 Descriptors

;; To describe a message, we need a selector to choose which message
;; to send, and a series of key/value pairs that hold the message's
;; arguments. A list of the form

;;  (:SELECTOR PLIST)

;; is called a descriptor. Descriptors are used in calls to
;; `define-prototype' and `define-method' (which see.)

;;; *4.41

;; Any selector may be used, not just those that identify messages.
;; When it's a slot selector, the values in PLIST instead describe the
;; slot. Both the system and the user can exploit such descriptions;
;; for example, describing the allowable types and values of a slot
;; makes it possible to construct a user interface for modifying the
;; slot's value without introducing type errors.

;; When used to describe slots or message arguments, the following
;; keys are relevant:

;;  :default-value   Any default value for this slot or argument.  
;;  :documentation   Documentation for the user.  
;;  :type            A type specifier suitable for use with `typep'

;; This list is expected to grow in the future. Any unknown keys are
;; ignored.

;;; *4.42

(eval-and-compile
  (defun --descriptor-p (m) 
    (typecase (car-safe m)
      (object (listp (cdr-safe m)))
      (selector (listp (cdr-safe m)))
      (otherwise nil))))

(deftype descriptor () '(satisfies --descriptor-p))

;;; *5 Prototypes

;; Any object can serve as a template for creating new objects; just
;; invoke its :clone method. But certain objects called "prototypes"
;; are specialized for this purpose, and these are handled very
;; differently during cloning. See `eon-default-clone-method' for an
;; explanation.

;;; *5.1 define-prototype

(defmacro define-prototype (prototype-spec documentation slot-descriptors)
  "Create a new prototype (possibly based on another prototype).

PROTOTYPE-SPEC should be either the selector of the prototype you
are defining, or a list of the form

 (NEW-PROTOTYPE-SELECTOR SUPER-PROTOTYPE-SELECTOR)

where NEW-PROTOTYPE-SELECTOR is the selector of the prototype you
are defining, and SUPER-PROTOTYPE-SELECTOR is the selector of the
previously-defined prototype that the new prototype will inherit
from.

SLOT-DESCRIPTORS is a list of descriptors giving selectors, type
information, default values, and/or documentation for the slots.
   
DOCUMENTATION is a documentation string for the new prototype.

You should use only this macro and `eon-clone' to create objects;
creation `ex nihilo' with `make-object' is discouraged, and leads
to unspecified behavior."
  (let (new-selector super-selector slots)
    ;;
    ;; figure out what kind of prototype-spec it is
    (typecase prototype-spec
      (selector (setf new-selector prototype-spec))
      (list (setf new-selector (first prototype-spec)
		  super-selector (second prototype-spec))))
    (setf slots
	  ;;
	  ;; install slots specific to this prototype
	  (append (mapcan (lambda (slot-descriptor)
			    ;;
			    ;; here we extract the default values for the args
			    (destructuring-bind
				(descriptor (&key default-value &allow-other-keys))
				slot-descriptor
			      (list descriptor default-value)))
			  slot-descriptors)
		  ;;
		  ;; install slots that are common to all prototypes 
		  (list :slot-descriptors slot-descriptors
			:documentation documentation
			:initialize #'eon-default-initialize-method
			:clone #'eon-default-clone-method)))
    ;;
    ;; register the new prototype
    `(eon-put-selector ,new-selector
		       (make-object :slots ',slots
				    :selector ',new-selector
				    :prototype-p t
				    :prototype ',super-selector))))

(defun eon-find-prototype (object)
  "Return the prototype of the OBJECT, if any."
  (let ((p (object-prototype object)))
    (typecase p
      (selector (setf (object-prototype object)
		      (eon-find-object p)))
      (object p)
      (otherwise nil))))

;;;  *5.2

(defun eon-default-clone-method (object &rest initargs)
  "Clone the OBJECT.  The INITARGS are passed to the initializer.

This function implements the default cloning policy.  Because
most new objects are created by cloning others, the details of
the cloning process influence the meaning and performance
characteristics of our programs.  So although this function's
body is quite short, its subtle consequences make further
explanation necessary.

When an ordinary object (i.e. not a prototype) is cloned, all its
slot values are copied into a new object (with `copy-tree'; if
you need different behavior, you can override the :clone method).
The prototype of the clone is the same as the prototype of the
cloned object, so if you clone a clone, you get a new object with
the same prototype yet again.  This prevents the slot lookup
chain from growing arbitrarily long as you clone ordinary
objects.

;; TODO artist-mode diagram of ordinary object clone

The effect of the preceding is that no connection will remain
between an ordinary object and its clone.  But when a prototype
is cloned, the prototype and the clone maintain an ongoing
relationship.  The slot values of the prototype are not copied
into the clone; instead, the clone begins life with no slot
values of its own, and instead delegates slot value requests and
messages to its prototype.  So each time you specialize a
prototype further by creating a new prototype from it, the slot
lookup chain grows one link longer.

Because the slot lookup procedure searches an object's prototype
when it can't find a slot value in the object itself, any changes
to the prototype object made after cloning will propagate to the
cloned objects (except when a clone has overridden the slot in
question.)

;; TODO artist-mode diagram of prototype clone

After cloning, the new object is initialized with INITARGS being
passed to the :initialize method.

Don't call this function yourself.  Instead, use `eon-clone'.

See also `eon-read-slot'."
  (let ((clone
	 (if (object-prototype-p object)
	     (make-object :prototype object)
	   (make-object :prototype (object-prototype object)
			:slots (copy-tree (object-slots object) :vectors)))))
    (when (@ clone :initialize)
      (eon-deliver-message clone :initialize initargs))
    clone))

(defun eon-default-initialize-method (object &rest ignore)
  "Initialize the OBJECT.
The default behavior is to do nothing, and IGNORE the arguments."
  nil)

(defun eon-clone (object-or-selector &rest initargs)
  "Clone OBJECT-OR-SELECTOR, passing INITARGS to the initializer."
  (eon-deliver-message (eon-find object-or-selector)		   
		       :clone
		       initargs))
		    
;;; *6 Slots

;; An object's slot collection is just a property list whose keys are
;; selectors. Thus we may use `getf' and related functions to store
;; and retrieve slot values. If a slot value is not present, the
;; object's prototype is also checked for a value; this is how objects
;; can inherit data and behavior from their prototypes. See
;; `eon-read-slot'.

;; When you set the value of any slot, the prototype's value is
;; effectively overridden. See `eon-write-slot'.

;;; **6.1

;; According to the Emacs Lisp Manual, plists and alists are actually
;; faster than hash-tables when you only have a few tens of items in
;; the collection---this should suffice for many objects. (I could
;; rationalize that an object with more than 20 or so slots should
;; probably be refactored. If I turn out to be wrong about that, it
;; will be easy to add objects whose slot collections are hash-tables
;; instead of property lists.)

;;; *6.2

(defun* eon-read-slot (object slot)
  "Retrieve from OBJECT the value of SLOT.
If the slot has no value in OBJECT, then the object's prototypes
are also checked.

When slot lookup fails to find a value, it sends
a :does-not-understand message to the original object to see if
it can recover. If that fails too, let the :system object handle
it."
  (block searching
    (let (pointer result)
      (setf pointer (eon-find object))
      ;;
      ;; search the chain of prototypes for a slot value.
      (while pointer
	(setf result (getf (object-slots pointer) 
			   slot :does-not-understand))
	(if (eq :does-not-understand result)
	    ;;
	    ;; it's not here. search the prototype, if any.
	    (setf pointer (eon-find-prototype pointer))
	  ;;
	  ;; we found a value in this object. we're done.
	  (return-from searching result)))
    (if (eq :does-not-understand slot)
	;;
	;; the object could not respond to :does-not-understand
	;; after having a slot lookup fail.
	(>> :system :does-not-understand :object object :message slot)
      ;;
      ;; a slot lookup failed. see if the object itself can recover.
      (>> object :does-not-understand :object object :message slot)))))

(defun eon-write-slot (object slot value)
  "Set OBJECT's SLOT to VALUE.
The new value overrides any inherited value."
  (let ((o (eon-find object)))
    (setf (getf (object-slots o) slot) value)))

(defalias '@ 'eon-read-slot)
(defsetf eon-read-slot eon-write-slot)
(defsetf @ eon-write-slot) ;; Now you can write (setf (@ object :slot) value)

;;; *7 Messages

;; A message is a method call waiting to happen. When you send a
;; message to an object, the system searches for an appropriate method
;; to invoke (with `eon-deliver-message', which see). If it can find a
;; method, the message is "delivered" and the method is invoked; if
;; not, the slot lookup will fail, and the system will attempt to
;; recover (see `eon-read-slot').

;;; *7.1

(defun eon-deliver-message (object method args &optional other-object)
  "Invoke upon OBJECT the METHOD with ARGS.

When OTHER-OBJECT is non-nil, use the method handler from
OTHER-OBJECT instead of the one from OBJECT; this is used to
implement `super>>'.

Don't use this function yourself.  Instead, use the message send
operator `>>'."
  (let* ((handler (@ (eon-find (or other-object object))
		     method)))
    (if handler
	(apply handler object args)
      (error "No method available for %S" method)))) 

(defun eon-send-messages (object messages)
  "Send the MESSAGES to OBJECT immediately, and return the result
of the last message in the list."
  (let (result)
    (dolist (m messages result)
    (destructuring-bind (method args) m
      (setf result (eon-deliver-message object method args))))))

;;; *7.2 The >> operator

(defmacro >> (object selector &rest args)
  "Send OBJECT the message SELECTOR with ARGS as arguments."
  `(eon-deliver-message ,object ,selector (list ,@args)))

;;; *8 Methods

;; A method is a lisp function that implements an object's response to
;; a message. See also `Descriptors'.

;;; *8.1 define-method

;; The `define-method' macro builds the new method's defun and
;; registers the method with its prototype. Additional global and
;; local macro definitions transform the method body code.

(defmacro define-method
 (method-selector prototype-selector arg-descriptors &rest method-body)
  "Define a new method.

METHOD-SELECTOR is a selector naming the method.
PROTOTYPE-SELECTOR is the selector of the existing prototype that
you are defining a method for.  ARG-DESCRIPTORS is a list of
descriptors, one for each argument of the method. 

If METHOD-BODY begins with a string, this string becomes the
documentation string for the method.

The forms in METHOD-BODY are executed when the method is invoked.
The hidden argument `self' may be referred to as needed within
the method body; it is bound to the object upon which the method
was invoked.  See also `with-slots'."
  (declare (debug (&define name name sexp sexp def-body)))
  (labels ((selector-to-symbol (s)
			       (intern (substring (symbol-name s) 1)))
	   (make-arglist-element 
	    (arg)
	    (typecase arg
	      (selector (selector-to-symbol arg))
	      (descriptor 
	       (destructuring-bind 
		   (selector (&key default-value &allow-other-keys))
		   arg
		 (list (selector-to-symbol selector)
		       default-value)))
	      (symbol arg)
	      (otherwise (eon-type-error define-method:args
					 (or selector descriptor)
					 arg)))))
    ;;
    ;; build the components of the defun
    (let ((method-defun-symbol (intern (concat "eon:method"
					       (symbol-name prototype-selector)
					       (symbol-name method-selector))))
	  (method-defun-arglist (mapcar #'make-arglist-element arg-descriptors))
	  (documentation (when (stringp (first method-body))
			   (first method-body))))
      ;;
      ;; now expand
      (let ((prototype (gensym))
	    (super (gensym))
	    (super-selector (gensym)))
	`(lexical-let (,super-selector
		       (,prototype (eon-get-selector ,prototype-selector)))
	   ;;
	   ;; make sure the prototype exists
	   (when (null ,prototype)
	     (error "Cannot define method %S for nonexistent prototype %S"
		    ,method-selector ,prototype-selector))
	   (setf ,super (eon-find-prototype ,prototype))
	   (when ,super
	     (setf ,super-selector
		   (typecase ,super
		     (object (object-selector ,super))
		     (selector ,super)
		     (otherwise (eon-type-error define-method:super
						(or object selector)
						,super)))))
	   ;;
	   ;; create local macro definitions
	   (macrolet ((self>> (method &rest args)
			      `(eon-deliver-message self
						    ,method (list ,@args)))
		      (super>> (method &rest args)
			       `(eon-deliver-message self 
						     ,method (list ,@args)
						     --super-selector)))
	     ;;
	     ;; define the method's Lisp function
	     (defun* ,method-defun-symbol (self &key ,@method-defun-arglist)
	       ,documentation
	       (let ((--super-selector (symbol-value ,super-selector)))
		 ,@(delete-if #'stringp method-body)))
	     ;;
	     ;; store the method's function in the prototype's slot
	     (setf (@ ,prototype ,method-selector) ',method-defun-symbol)
	     ;;
	     ;; add new slot-descriptor for this method to the prototype
	     (setf (@ ,prototype :slot-descriptors)
		   (adjoin '(,method-selector 
			     (:documentation ,documentation
					     :arguments ,arg-descriptors))
			   (@ ,prototype :slot-descriptors)
			   :key 'car))))))))

;;; *8.2

(defmacro with-slots (slots &rest body)
  "Within BODY, bind the symbols in SLOTS to the corresponding slots.

We use `symbol-macrolet' to create fake local variables.
References to the symbols in SLOTS are transformed into slot
reads/writes as appropriate.

This will not work outside of a `define-method' body."
  (declare (indent 1))
  `(symbol-macrolet
       ,(mapcar (lambda (slot)		  
		  `(,slot
		    (@ self ,(intern (concat ":" (symbol-name slot))))))
		slots)
     ,@body))

;;; *9 The :system object

;; Right now this doesn't do much but signal errors. At some point
;; there will be more services available.

(define-prototype :system "The eon system object."
  ((:log-buffer (:documentation "Emacs buffer where logs are displayed."
		 :type buffer))))

(define-method :does-not-understand :system
  ((:message (:documentation "The message that could not be delivered."
			     :type descriptor))
   (:object (:documentation "The object that could not respond to the message."
			    :type object)))
  "Respond to failure in delivery of MESSAGE."
  (error "System could not deliver message %S" message))

;;; *10 Workspaces

;;; *11 Persistence

;; TODO port this from common lisp
;; TODO review cmu codewalker stuff

;; (defun fold-model (model)
;;   "Turn a model with many duplicate (but equal) sublists into a
;; model with a table of objects and references between
;; them. Returns a hash table mapping sexps to integers, and the
;; transformed model."
;;   (let ((sexps->integers (make-hash-table :test 'equal))
;; 	(id 0)
;; 	(m (copy-tree model)))
;;     (labels ((fold-sexp (L)
;; 	       (let* ((sexp (car L))
;; 		      (sexp-id nil))
;; 		 ;;
;; 		 ;; don't match keywords or already-substituted references
;; 		 (when (and (listp sexp)
;; 			    (not (null sexp))
;; 			    (not (equal 'folded-reference (car sexp))))
;; 		   (fold-sexp sexp)
;; 		   (if (setf sexp-id (gethash sexp sexps->integers))
;; 		       (nsubst `(folded-reference ,sexp-id) sexp L :test 'equal)
;; 		       ;;
;; 		       ;; it's not in the hashtable. put it in
;; 		       (progn 
;; 			 (incf id)
;; 			 (setf (gethash sexp sexps->integers) id))))			
;; 		 ;;
;; 		 (when (not (null L))
;; 		   (fold-sexp (cdr L))))))
;;       (fold-sexp m)
;;       ;;
;;       ;; give the object table a "root object"
;;       (setf (gethash m sexps->integers) 0)
;;       ;;
;;       ;; now fold sexps that are in the hash table already
;;       (let ((new-sexps (make-hash-table :test 'equal)))
;; 	;;
;; 	;; first make a copy; we can't modify a hash table while iterating over it
;; 	(maphash (lambda (k v)
;; 		   (setf (gethash k new-sexps) v))
;; 		 sexps->integers)
;; 	;;
;; 	;; now fold the keys of the copy while modifying the original 
;; 	(maphash (lambda (k v)
;; 		   (fold-sexp k))
;; 		 new-sexps))
;;       ;;
;;       ;; return the mapping and the folded model
;;       (values sexps->integers m))))
      
;; (defun serialize-model (model)
;;   "Serialize a model into a set of sexps suitable for writing to a text file."
;;   (multiple-value-bind (sexp-hash folded-model) (fold-model model)
;;     (let ((sexps nil))
;;       (maphash (lambda (k v)
;; 		 (push (cons v k) sexps))
;; 	       sexp-hash)
;;       (sort sexps (lambda (x y)
;; 			(> (car x) (car y))))
;;       (nreverse sexps))))
	      
;; (defun write-model (model filename)
;;   "Write a model to disk."
;;   (with-open-file (file filename :direction :output 
;; 			:if-exists :overwrite
;; 			:if-does-not-exist :create)
;;     (format t "~S" model)
;;     (format file "~S" model)))

;; ;; TODO convert recursion to iteration below

;; (defun load-worksheet (filename)
;;   "Construct a worksheet from a file."
;;   (let* ((model (read-model filename))
;; 	 (integers->sexps (make-hash-table :test 'eql))
;; 	 (integers->objects (make-hash-table :test 'eql))
;; 	 (worksheet-model nil))
;;     ;;
;;     ;; grab the worksheet object, which is always first
;;     (setf worksheet-model (car model))
;;     ;;
;;     ;; read in all the sexps
;;     (dolist (m model)
;;       (setf (gethash (car m) integers->sexps) (cdr m)))
;;     ;;
;;     ;; now expand the sexps into objects by recursively unfolding all references
;;     ;;
;;     (labels ((remove-class-keywords (plist)
;;  	       ;; TODO ditch this
;;  	       ;; we do this because it makes it easier to
;;  	       ;; pass the plist to make-instance during
;;  	       ;; the unmodeling process.
;;  	       (let ((plist1 plist)
;;  		     (plist2 nil))
;;  		 (do ((p (pop plist1) (pop plist1)))
;;  		     ((null plist1))
;;  		   (if (equal :class p)
;;  		       (pop plist1) ; skip value after keyword
;;  		       (push p plist2)))
;;  		 ;;
;;  		 ;; handle the last element
;;  		 (push (car (last plist)) plist2)
;;  		 (prog1 
;;  		     (reverse plist2))))
;; 	     ;;
;; 	     (unmodel-object (plist)
;; 	       (let* ((plist2 (remove-class-keywords plist))
;; 		      (object-class (getf plist :class))
;; 		      (object (apply #'make-instance object-class plist2)))
;; 		 (unmodel object)
;; 		 object))
;; 	     ;;
;; 	     (expand (sexp)
;; 	       (if (not (listp sexp))
;; 		   sexp
;; 		   ;;
;; 		   ;; what type of list? 
;; 		   (cond
;; 		     ;;
;; 		     ;; a folded reference? 
;; 		     ((equal 'folded-reference (car sexp))
;; 		      (let* ((reference-number (car (cdr sexp)))
;; 			     (object (gethash reference-number integers->objects)))
;; 			;;
;; 			;; if already in object cache, return it.
;; 			;; otherwise, put it in
;; 			(if object
;; 			    object
;; 			    ;;
;; 			    ;; time to make the donuts!
;; 			    (let ((reference-sexp 
;; 				   (gethash reference-number integers->sexps)))
;; 			      (setf (gethash reference-number integers->objects)
;; 				    (expand reference-sexp))))))
;; 		     ;;
;; 		     ;; a modeled object?
;; 		     ((listp sexp)
;; 		      ;;
;; 		      ;; expand all subforms
;; 		      (setf sexp (mapcar #'expand sexp))
;; 		      ;;
;; 		      ;; create object when ready
;; 		      (if (equal :class (car sexp))
;; 			  (unmodel-object sexp)
;; 			  ;; otherwise just return sexp
;; 			  sexp))))))
;;       ;;
;;       ;;
;;       (values 
;;        (expand (gethash 0 integers->sexps))
;;        integers->objects integers->sexps))))

;;; *A1 Font-locking

;; Put this in your emacs initialization file to get the highlighting:
;; (add-hook 'emacs-lisp-mode-hook #'eon-do-font-lock)

(defvar eon-font-lock-keywords 
  '(("(\\(@\\>\\)" (1 font-lock-keyword-face))
    ("(\\(>>\\>\\)" (1 font-lock-keyword-face))
    ("(\\(self>>\\>\\)" (1 font-lock-keyword-face))
    ("(\\(super>>\\>\\)" (1 font-lock-keyword-face))
    ("(\\(suspend>>\\>\\)" (1 font-lock-keyword-face))				    
    ("(\\(define-prototype\\>\\)" (1 font-lock-keyword-face))
    ("(\\(define-method\\>\\)" (1 font-lock-keyword-face))))

(defun eon-do-font-lock ()
  "Highlight the keywords used in prototype-oriented programming."
  (font-lock-add-keywords nil eon-font-lock-keywords))

;;; **A2 A simple object dumper

(defun eon-dump-object (o)
  "Display a buffer showing information about the object O."
  (with-current-buffer (get-buffer-create "*eon-dump*")
    (delete-region (point-min) (point-max))
    (let ((p (eon-find o))
	  slots)
      (while p
	(insert "--------------\n")
	(insert (format "@selector %S @prototype %S\n@prototype-p %S @referent %S \n"
			(object-selector p)
			(object-prototype p)
			(object-prototype-p p)
			(object-referent p)))
	(insert "--------------\n")
	(setf slots (object-slots p))
	(while slots
	  (insert (format "  %S = %S\n" (first slots) (second slots)))
	  (pop slots)
	  (pop slots))
	(setf p (eon-find-prototype p)))))
  (display-buffer "*eon-dump*"))

;;; *Z

(provide 'eon)
;;; eon.el ends here

