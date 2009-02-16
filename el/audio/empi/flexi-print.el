;;; FLEXI-PRINT.EL --- Customizable formatted string output

;; Copyright (C) 2005 R.Ramkumar

;; Author: 	R.Ramkumar andyetitmoves@gmail.com
;; Created: 	06 Feb 2005
;; Version: 	1.0
;; Keywords:	format output

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this program's
;; author (send electronic mail to andyetitmoves@gmail.com) or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; flexi-print|R.Ramkumar|andyetitmoves@gmail.com
;; |Customizable formatted string output
;; |$Date$|$Revision$|~/packages/flexi-print.el

;;; Commentary:

;; Lisp packages often need to output data visibly, and it's always a good idea,
;; in true emacs spirit, to be able to customize such output, especially when
;; it's more than just a message in the log. Facilities for producing such
;; customized output are limited in the default setup - `format' is far too
;; generic and limited. Some ambitious projects like gnus do take it upon them
;; to define their own '%' escapes - no trivial exercise indeed. Hence this
;; package to make the entire routine a few lines of lisp. Now you have no
;; excuse for not customizing every output your package produces -
;; go the emacs way :)

;; In view of a modular design, this package has a few printing backends, and
;; the user can even choose the backend. Each backend operates upon two objects
;; - the scheme, a value particular to a package and usually fixed; and the
;; format, the user customizable value. For a logical example, scheme could be
;; 'f => foo, b => bar and format could be '%f: %b', where foo and bar are
;; variables. At the time of invocation, if foo = hello, bar = world; the output
;; would be 'hello: world'. Ok, that was just a *logical* example. The format is
;; specific to the backend, here it could be for the printf/format-like backend.

;; A quick guide. Use `flexi-print-compile' to create a scheme. If the scheme is
;; static (most likely), you can do this at compile time and store way the
;; result. Use `flexi-print' using the generated scheme and the format. Use
;; the `flexi-print-format' widget for customization entries (using the keys
;; :type 'flexi-print-format in say, `defcustom') . This is not the complete
;; documentation. Being a package for developers, I can take the liberty of
;; asking you to browse the source :)

;;; Code:

(defun binary-search (array elt &optional lessp)
  "Search a sorted ARRAY for ELT using less-than predicate LESSP."
  (or lessp (setq lessp '<))
  (let (res (beg 0) mid (end (1- (length array))))
    (while (not res)
      (if (> beg end)
	  (setq res -1)
	(setq mid (/ (+ end beg) 2))
	(cond
	 ((funcall lessp elt (aref array mid)) (setq end (1- mid)))
	 ((funcall lessp (aref array mid) elt) (setq beg (1+ mid)))
	 (t (setq res mid)))))
    (and (wholenump res) res)))

(defsubst car-string-lessp (a b)
  (string-lessp (car a) (car b)))

(defmacro assert-type (obj func)
  "Ensure that OBJ tests succeeds for type checking predicate FUNC.
The function emits a \"Wrong type argument\" signal on failure.
Note that the arguments are evalled twice in this process."
  `(or (,func ,obj) (signal 'wrong-type-argument (list (quote ,func) ,obj))))

(defun flexi-print-compile (&rest config)
  "Compile flexi-print configuration CONFIG.
The compiled scheme is suitable for use with `flexi-print'. CONFIG is a property
list, the main key being `:keys'. This is a association list, with keys being
strings or list of strings to match for, and the value being a symbol,
containing the value corresponding to the strings, or a function, to call with
no arguments to return the value corresponding to these strings.
`flexi-print-query' is bound to the query string at the time of the function
call. The key `:default' is also a similar symbol/function used for all strings
other than those mentioned."
  (let ((destlen 0) dests assocs assocval
	(keys (plist-get config :keys)) thiskey idest)
    (while keys
      (setq thiskey (cdar keys))
      (assert-type thiskey symbolp)
      (setq idest (1- (if (setq idest (memq thiskey dests))
			  (length idest)
			(setq dests (cons thiskey dests))
			(setq destlen (1+ destlen)))))
      (or (listp (setq thiskey (caar keys))) (setq thiskey (list thiskey)))
      (while thiskey
	(setq assocval (assoc (car thiskey) assocs))
	(if assocval
	    (setcdr assocval idest)
	  (setq assocs (cons (cons (car thiskey) idest) assocs)))
	(setq thiskey (cdr thiskey)))
      (setq keys (cdr keys)))
    (vector (apply 'vector (sort assocs 'car-string-lessp))
	    (apply 'vector (nreverse dests))
	    (plist-get config :default))))

(defun flexi-print-scheme-p (scheme)
  "Return t if SCHEME is a flexi-print scheme."
  (and (vectorp scheme) (= (length scheme) 3)))

(defmacro assert-flexi-print-scheme (scheme)
  `(assert-type ,scheme flexi-print-scheme-p))

(defvar flexi-print-query nil
  "Query string dynamically passed to query function in a flexi-print scheme.
This is a dummy definition to silence the compiler in client compilations.")

(defvar flexi-print-cookie nil
  "Data cookie passed to query function in a flexi-print scheme.
This package doesn't use this variable at all. This definition just suggests a
naming convention for a dynamic variable possibly passed from the client calling
`flexi-print' to the query function in the corresponding scheme. In such a case,
it also silences the compiler in client compilations.")

(defun flexi-print-query (scheme flexi-print-query)
  "Retrieves the value corresponding to FLEXI-PRINT-QUERY in SCHEME.
The value corresponding to FLEXI-PRINT-QUERY is got from SCHEME.
SCHEME's are created by using `flexi-print-compile' on a configuration."
  (assert-flexi-print-scheme scheme)
  (let ((result (binary-search (aref scheme 0)
			       (cons flexi-print-query -1) 'car-string-lessp)))
    (and (setq result
	       (if result
		   (aref (aref scheme 1) (cdr (aref (aref scheme 0) result)))
		 (aref scheme 2)))
	 (if (functionp result) (funcall result) (symbol-value result)))))

(defsubst print-any (obj)
  (if obj (if (stringp obj) obj (prin1-to-string obj)) ""))

(defun flexi-print-backend-format (scheme fmt)
  "Extended `format' like backend to `flexi-print'.
FMT argument accepts `format' like strings, using escapes like %. or %{.*},
where . is any one character string in the SCHEME, and .* is any multi-character
string. Testing for values in the scheme can be done by the syntax %(.<fmt>%) or
%({.*}<fmt>%). In such a case, <fmt>, any arbitrary format string, is output
only when the value got by . or {.*} is non-nil. An optional '!' immediately
after the opening brace negates this result."
  (assert-flexi-print-scheme scheme)
  (let ((newstr "") (end 0) key (skip 0) (state 0) (case-fold-search nil))
    (while (string-match "%\\((\\(!\\)?\\)?\\(\\(?:{[^}]*}\\)\\|.\\)" fmt end)
      (and (= skip 0)
	   (setq newstr
		 (concat newstr (substring fmt end (match-beginning 0)))))
      (setq end (match-end 0))
      (setq key (match-string 3 fmt))
      (and (= ?{ (aref key 0)) (setq key (substring key 1 -1)))
      (cond
       ((match-string 1 fmt)
	(or (if (match-string 2 fmt)
		(not (flexi-print-query scheme key))
	      (flexi-print-query scheme key))
	    (setq skip (1+ skip))))
       ((string-equal key ")") (or (= skip 0) (setq skip (1- skip))))
       ((= skip 0)
	(setq newstr
	      (concat newstr
		      (if (string-equal key "%") "%"
			(print-any (flexi-print-query scheme key))))))))
    (concat newstr (and (= skip 0) (substring fmt end)))))

(put 'flexi-print-backend-format 'flexi-print-backend-name "Format")
(put 'flexi-print-backend-format
     'flexi-print-backend-widget-type '(string :tag "Format"))

(defun flexi-print-backend-read-eval (scheme fmt)
  "Lisp string backend to `flexi-print'.
FMT is a string, in which any occurence of %. or %{.*}, where . is any single
character and .* any multi-character string in SCHEME, is replaced with its
corresponding value, and the result read and eval'ed to get the return value."
  (let ((newstr "") (end 0) key (case-fold-search nil))
    (while (string-match "%\\(\\(?:{[^}]*}\\)\\|.\\)" fmt end)
      (setq key (match-string 1 fmt))
      (and (= ?{ (aref key 0)) (setq key (substring key 1 -1)))
      (setq newstr (concat newstr (substring fmt end (match-beginning 0))
			   (prin1-to-string (flexi-print-query scheme key))))
      (setq end (match-end 0)))
    (eval (read (concat newstr (substring fmt end))))))

(put 'flexi-print-backend-read-eval 'flexi-print-backend-name "Read Eval")
(put 'flexi-print-backend-read-eval
     'flexi-print-backend-widget-type '(string :tag "Expression"))

(defun flexi-print-backend-function (scheme func)
  "Wrapper function backend to `flexi-print'.
Just calls any other flexi-print backend, without any format argument though."
  (funcall func scheme))

(put 'flexi-print-backend-function 'flexi-print-backend-name "Function")
(put 'flexi-print-backend-function 'flexi-print-backend-widget-type 'function)

(defun flexi-print (scheme format)
  "Flexible output formatting for packages.
SCHEME is the scheme used for printing, and got by `flexi-print-compile'.
The SCHEME contains a set of strings, and describes how these are converted to
corresponding values. FORMAT is (BACKEND . COOKIE), where BACKEND is the backend
used for printing, and COOKIE is passed to the backend. When the backend
recognizes one of the strings in the SCHEME in ARG, it is replaced with the
corresponding value described by the scheme. How the strings are recognized and
replaced are backend dependent. See `flexi-print-known-backends' for a list of
known backends. However, any function could potentially be a backend.

A flexi-print backend takes the scheme and an arbitrary cookie as arguments.
It can use `flexi-print-query' to get the value for the scheme corresponding
to some string. When the backend is added to `flexi-print-known-backends', it
becomes available for choice in the `flexi-print-format' widget which can say,
be used in the customization buffer. If a symbol holding the backend function
is added to the known list, some properties in the symbol are of importance.
`flexi-print-backend-name' is a descriptive name for the backend.
`flexi-print-backend-widget-type' is the widget type specification for the
argument accepted by the backend."
  (funcall (car format) scheme (cadr format)))

(defvar flexi-print-known-backends
  '(flexi-print-backend-format
    flexi-print-backend-read-eval
    flexi-print-backend-function)
  "List of known `flexi-print' backends.")

(require 'widget)

(defun widget-dyngroup-convert-widget (widget)
  (let* ((args (widget-get widget :args)) (sym (car args))
	 (default (or (widget-get widget :default-type) 'sexp))
	 (expand (or (widget-get widget :dyngroup-expand) 0)))
    (when (= expand 0)
      (setq args (cdr args))
      (or (symbolp sym) (signal 'wrong-type-argument (list 'symbolp sym)))
      (widget-put
       widget :args
       (mapcar '(lambda (item)
		  (or (if (listp item)
			  (or (get sym (car item))
			      (plist-get (cdr item) :default))
			(get sym item)) default)) args)))
    (widget-put widget :dyngroup-expand (1+ expand))
    (funcall (widget-get (widget-convert 'group) :convert-widget) widget)))

(define-widget 'dyngroup 'group
  "A widget which groups widget descriptions given in proplist of a symbol."
  :tag "Dynamic Group"
  :convert-widget 'widget-dyngroup-convert-widget)

(define-widget 'dynlist 'dyngroup
  "A widget for a list using `dyngroup'."
  :tag "Dynamic List"
  :format "%{%t%}:\n%v")

(defun widget-flexi-make-backend-item (item)
  `(dynlist :format "%v" ,item
	    (nil :default (function-item
			   :format "%t\n%h"
			   :tag ,(concat
				   (or (get item 'flexi-print-backend-name)
				       (symbol-name item))
				   " Backend") ,item))
	    (flexi-print-backend-widget-type :default (sexp :tag "Argument"))))

(defun widget-flexi-convert-widget (widget)
  (widget-put widget
	      :args (mapcar 'widget-flexi-make-backend-item
			    flexi-print-known-backends))
  (funcall (widget-get (widget-convert 'radio) :convert-widget) widget))

(defun widget-flexi-format-handler (widget esc)
  (cond
   ((eq esc ?l)
    (widget-create 'documentation-string "\
This variable serves as a formatting argument to `flexi-print'.")
    (insert "To know more about Flexi Print, read ")
    (widget-create 'emacs-commentary-link :tag "this" "flexi-print"))
   (t (funcall (widget-get (widget-convert 'radio) :format-handler)
	       widget esc))))

(define-widget 'flexi-print-format 'radio
  "A widget for `flexi-print' format."
  :tag "Flexi Print Format"
  :format "%{%t%}:\n\n%l\n\n%v"
  :convert-widget 'widget-flexi-convert-widget
  :format-handler 'widget-flexi-format-handler)

(provide 'flexi-print)

;;; FLEXI-PRINT.EL ends here
