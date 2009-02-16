;;; list-fns.el --- list-searching and manipulation functions

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 00, 01, 04, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: list-fns.el,v 1.22 2005/11/11 00:28:57 friedman Exp $

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
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some of these functions are fairly general-purpose.  Others manipulate
;; special-purpose lists, e.g. auto-mode-alist.

;;; Code:


;;; Functions for operating on generic lists

;;;###autoload
(defun add-list-members (list &rest new)
  "Insert items in list if they are not already members of that list.
Comparison is done with EQUAL, not EQ.

New items are inserted onto the front of the list.
LIST may be a list or a symbol whose value is a list.
  If LIST is a symbol, it is modified by side effect.
  If it is void, it will be initialized.

The new value of the list is returned."
  (let ((l (cond ((not (symbolp list))
                  list)
                 ((boundp list)
                  (symbol-value list)))))
    (while new
      (or (member (car new) l)
          (setq l (cons (car new) l)))
      (setq new (cdr new)))
    (if (symbolp list)
        (set list l))
    l))

(put 'add-list-members 'lisp-indent-function 1)

;;;###autoload
(defun delete-list-members (list &rest removals)
  "Remove items from list if they are members.
Comparison is done with EQUAL, not EQ.

LIST may be a list or a symbol whose value is a list.
  If LIST is a symbol, it is modified by side effect.

The new value of the list is returned."
  (let ((l (cond ((not (symbolp list))
                  list)
                 ((boundp list)
                  (symbol-value list)))))
    (while removals
      (setq l (delete (car removals) l))
      (setq removals (cdr removals)))
    (if (symbolp list)
        (set list l))
    l))

(put 'delete-list-members 'lisp-indent-function 1)

;; This used to be called `copy-tree', but that is now a standard function
;; in subr.el
;;;###autoload
(defun deep-copy-tree (obj &optional allp)
  "Make a deep copy of an object.
Traverse object recursively, copying elements to create a new copy of the
overall object.

Only conses are copied unless the optional argument ALLP is non-nil.
If ALLP is non-nil then the following objects are also copied:

   * vectors \(but obarrays will not be copied properly\)
   * strings
   * markers
   * hashtables

That is, these objects and their inner elements \(if any\) will be distinct
copies from the elements present in the original object."
  (cond
   ((consp obj)
    (cons (deep-copy-tree (car obj) allp)
          (deep-copy-tree (cdr obj) allp)))

   ((not allp) obj)

   ((vectorp obj)
    (let ((nv (make-vector (length obj) nil))
          (i 0))
      (while (< i (length nv))
        (aset nv i (deep-copy-tree (aref obj i) allp))
        (setq i (1+ i)))
      nv))

   ((stringp obj)
    (substring obj 0))

   ((markerp obj)
    (let ((m (make-marker)))
      (set-marker m (marker-position obj) (marker-buffer obj))
      (and (fboundp 'marker-insertion-type)
           (set-marker-insertion-type m (marker-insertion-type obj)))
      m))

   ((and (fboundp 'hash-table-p)
         (hash-table-p obj))
    (let ((newtbl (make-hash-table
                   :test             (hash-table-test             obj)
                   :size             (hash-table-size             obj)
                   :rehash-size      (hash-table-rehash-size      obj)
                   :rehash-threshold (hash-table-rehash-threshold obj)
                   :weakness         (hash-table-weakness         obj))))
      (maphash (lambda (key val)
                 (puthash (deep-copy-tree key allp)
                          (deep-copy-tree val allp) newtbl))
               obj)
      newtbl))

   (t obj)))

;;;###autoload
(defun delete-by (elt list equality-predicate)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.

Comparison of ELT against each member of LIST is performed with
EQUALITY-PREDICATE, which must accept two arguments and return true or `nil'.
E.g. If `eq' were specified, this function behaves exactly like `delq'.

If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `\(setq foo \(delete-by elt foo equality-predicate\)\)'
to be sure of changing the value of `foo'."
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (funcall equality-predicate elt (car l))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (funcall equality-predicate elt (car list))
      (cdr list)
    list))

;;;###autoload
(defun first-matching (predicate &rest sequence)
  "Return the first element of SEQUENCE matching PREDICATE.
PREDICATE is a function of one mandatory argument.
SEQUENCE may be a list, a vector, a bool-vector, or a string."
  (catch 'found
    (mapc (lambda (elt)
            (if (funcall predicate elt)
                (throw 'found elt)))
      ;; This handles the case where first-matching is called with a single
      ;; list as the sequence, or is called via apply or explicit
      ;; enumeration of the sequence.  i.e. (first-matching p '(a b c)) is
      ;; equivalent to (first-matching p 'a 'b 'c)
      (if (null (cdr sequence))
          (car sequence)
        sequence))
    nil))

(put 'first-matching 'lisp-indent-function 1)

;;;###autoload
(defun flatten-lists (&rest list)
  "Copy LISTS and make all nested sublists top-level in a single return list.
Descends recursively through LISTS to find any sublists, and returns a
new list with copies of all non-list elements (including vectors and
strings) at the same level.  Dotted pairs are handled as lists.

  Examples:  (flatten-lists '(foo (bar baz) (quux (bletch (moby . bignum)))))
             => (foo bar baz quux bletch moby bignum)

             (flatten-lists '(a b (c d (e)) . f) '(1 2 ((3) . 4) 5))
             => (a b c d e f 1 2 3 4 5)"
  (cond ((consp (cdr list))
         (apply 'nconc (mapcar 'flatten-lists list)))
        (t
         (setq list (car list))
         (let* ((flatlist (cons nil nil))
                (flatlist-tail flatlist))
           (while list
             (if (listp list)
                 (progn
                   (if (listp (car list))
                       (setcdr flatlist-tail (flatten-lists (car list)))
                     (setcdr flatlist-tail (list (car list))))
                   (setq list (cdr list)))
               (setcdr flatlist-tail (list list))
               (setq list nil))
             (while (cdr flatlist-tail)
               (setq flatlist-tail (cdr flatlist-tail))))
           (cdr flatlist)))))

(defalias 'flatten 'flatten-lists)

;;;###autoload
(defun make-general-car-cdr (symbol-name)
  "Return a list-traversal function based on SYMBOL-NAME.
The name must be of the form `c[ad]+r', e.g. `caddddr', `caadadar', etc."
  (and (symbolp symbol-name)
       (setq symbol-name (symbol-name symbol-name)))
  (let ((i (- (length symbol-name) 2))
        (form 'obj)
        c)
    (while (> i 0)
      (setq c (aref symbol-name i)
            i (1- i))
      (setq form (list (cond ((char-equal ?a c) 'car)
                             ((char-equal ?d c) 'cdr)
                             (t (make-symbol "<undefined>")))
                       form)))
    (setq form (list 'lambda '(obj) form))
    (if (fboundp 'byte-compile)
        (byte-compile form)
      (list 'function form))))

;;;###autoload
(defun member-by (elt list equality-predicate)
  "Return non-nil if ELT is an element of LIST.
The value is actually the tail of LIST whose car is ELT.

Comparison of ELT against LIST is performed with EQUALITY-PREDICATE,
which must accept two arguments and return t or nil.  E.g. If `eq' were
specified, this function behaves exactly like `memq'."
  (while (and list (not (funcall equality-predicate elt (car list))))
      (setq list (cdr list)))
    list)

;; This is an O(N) algorithm in time; don't use on excessively long lists.
;;;###autoload
(defun uniquify-list-members (list &optional equality-predicate)
  "Delete by side effect repeating occurences of any elt in LIST.
For each elt in LIST, any subsequent occurence of the same elt is deleted
from the list.  The end result is that every remaining member in the list
is unique.

If optional argument EQUALITY-PREDICATE is non-`nil', comparison against
 each member is performed with the function specified by that argument.
 It must accept two arguments and return true or `nil'.
By default, comparison is done with `equal'.

The modified LIST is returned.  Note that the first element of LIST can
never be deleted by this function, so it is not necessary to re-bind
any variables bound to the list."
  (or equality-predicate (setq equality-predicate 'equal))
  (let ((l list))
    (while l
      (setcdr l (delete-by (car l) (cdr l) equality-predicate))
      (setq l (cdr l))))
  list)


;;; Functions for operating on circular lists

;;;###autoload
(defun circular-list-p (l)
  "Determine whether list L contains a cycle."
  (let ((k l)
        (e t)
        (c nil))
    (while l
      (setq l (cdr l)
            e (not e))
      (and e
           (setq k (cdr k)))
      (and (eq l k)
           (setq c t
                 l nil)))
    c))

;; Thanks to Martin Buchholz <martin@xemacs.org> for pointing out that the
;; nexus can be found by giving hare a head start of 1 loop period.
;; Thanks to Sean Suchter <ssuchter@inktomi.com> for helping me find an
;; earlier solution which had involved destructively modifying the list.
;; Both that solution and this one require O(N) time and O(1) space.
;;;###autoload
(defun circular-list-size (list)
  "Return the number of nodes in circular list LIST.
That is, return the distance between the start of the list and the node
whose cdr is another node of the same list.

If LIST is not actually circular, just return the length of the list."
  (let ((tortoise list)
        (hare list)
        (tortoise-advance t)
        (len 0))
    ;; Find a member of the list guaranteed to be within the cycle.
    (while hare
      (setq hare (cdr hare)
            len  (1+ len)
            tortoise-advance (not tortoise-advance))
      (and tortoise-advance
           (setq tortoise (cdr tortoise)))
      (and (eq hare tortoise)
           (setq hare nil
                 len  0)))

    (if (not (zerop len)) len           ; list was non-circular
      ;; Determine the length of the cycle
      (setq hare (cdr tortoise)
            len 1)
      (while (not (eq hare tortoise))
        (setq hare (cdr hare)
              len (1+ len)))
      ;; Give hare a head start from the start of the list equal to the
      ;; loop size.  If both march at the same speed they must meet at the
      ;; nexus because they are in phase, i.e. when tortoise enters the
      ;; loop, hare must still be exactly one loop period ahead--but that
      ;; means it will be pointing at the same list element.
      (setq tortoise list
            hare (nthcdr len list))
      (while (not (eq tortoise hare))
        (setq hare (cdr hare)
              tortoise (cdr tortoise)
              len (1+ len)))
      len)))


;;; Functions for operating on property lists

;;;###autoload
(defun merge-into-property-list (primary &rest plists)
  "Alter property list PRIMARY by merging in remaining property lists.
PRIMARY property list is modified; remaining property lists are not changed.

If PRIMARY is nil, a new property list is returned.
Otherwise the altered property list is returned.

If any property list argument is a symbol, the property list for that
symbol is used.

If two property lists specify the same property, the value from the
later property list is merged into the primary property list."
  (and (symbolp primary)
       (setq primary (symbol-plist primary)))
  (let (plist)
    (while plists
      (setq plist (car plists)
            plists (cdr plists))
      (and (symbolp plist)
           (setq plist (symbol-plist plist)))
      (while plist
        (setq primary (plist-put primary (nth 0 plist) (nth 1 plist)))
        (setq plist (cdr (cdr plist))))))
  primary)

(put 'merge-into-property-list 'lisp-indent-function 1)


;;; Functions for operating on association lists

;;;###autoload
(defun delassoc-by (elt alist equality-predicate)
  "Delete by side effect any occurrences of ELT as a member of ALIST.
The modified ALIST is returned.

Comparison of ELT against ALIST is performed with EQUALITY-PREDICATE,
which must accept two arguments and return t or nil.  E.g. If `assq' were
specified, this function behaves like `(delq (assq elt alist) alist)'."
  (let (x)
    (while (setq x (funcall equality-predicate elt alist))
      (setq alist (delq x alist))))
  alist)

;;;###autoload
(defun set-alist-slot (alist-or-sym key value
                                    &optional ignore-if-new assq-or-assoc
                                              append)
  "In ALIST, set KEY's value to VALUE, and return new value of ALIST.
This function is like `set-nested-alist-slot', but KEY is a single key, not
a list of keys, and only the top-level alist structure can be modified.
All other options are the same.

Note the difference in semantics:

  (set-alist-slot 'foo '(\"mail\" \"home\") \"friedman@splode.com\")
  => (((\"mail\" \"home\") . \"friedman@splode.com\"))

  (set-nested-alist-slot 'bar '(\"mail\" \"home\") \"friedman@splode.com\")
  => ((\"mail\" (\"home\" . \"friedman@splode.com\")))"
  (set-nested-alist-slot alist-or-sym (cons key nil) value
                         ignore-if-new assq-or-assoc append))

;;;###autoload
(defun set-nested-alist-slot (alist-or-sym key-list value
                              &optional ignore-if-new assq-or-assoc
                                        append)
  "In ALIST, set KEY-LIST's value to VALUE, and return new value of ALIST.
ALIST may be an alist \(associative list\) or a symbol whose value is an alist.
If ALIST is an unbound symbol, it will be bound if necessary.

KEY-LIST should be a list of nested keys, if ALIST is an alist of alists.
If any key is not present in an alist, the key and value pair will be
inserted into the parent alist, unless the optional 3rd argument
IGNORE-IF-NEW is non-`nil'.

The optional 4th argument ASSQ-OR-ASSOC should be the symbol `assq' or
`assoc', depending on which kind of search should be done on members of
KEY-LIST.  If not specified, `assq' is used when a key is a symbol,
`assoc' otherwise.

Optional 5th argument APPEND non-`nil' means new values should be appended
to the end of the alist.  The default is to insert new elements at the
front of the alist since that is faster.

Examples:

  \(set-nested-alist-slot 'data '\(\"mail\" \"gnu\"\) \"friedman@gnu.org\"\)
  => \(\(\"mail\" \(\"gnu\" . \"friedman@gnu.org\"\)\)\)

  \(set-nested-alist-slot 'data '\(\"mail\" \"home\"\) \"friedman@splode.com\"\)
  => \(\(\"mail\" \(\"home\" . \"friedman@splode.com\"\)
              \(\"gnu\" . \"friedman@gnu.org\"\)\)\)

  \(set-nested-alist-slot 'data '\(\"name\"\) \"Noah Friedman\"\)
  => \(\(\"name\" . \"Noah Friedman\"\)
      \(\"mail\" \(\"home\" . \"friedman@splode.com\"\)
              \(\"gnu\" . \"friedman@gnu.org\"\)\)\)"
  (let* ((alist (cond ((symbolp alist-or-sym)
                       (and (boundp alist-or-sym)
                            (symbol-value alist-or-sym)))
                      (t alist-or-sym)))
         (key (car key-list))
         (elt (cond (assq-or-assoc (funcall assq-or-assoc key alist))
                    ((symbolp key) (assq key alist))
                    (t             (assoc key alist)))))
    (setq key-list (cdr key-list))

    (cond
     ((and (cdr elt) key-list)
      (set-nested-alist-slot (cdr elt) key-list value
                             ignore-if-new assq-or-assoc))
     ((and elt key-list)
      (setcdr elt (set-nested-alist-slot nil key-list value
                                         ignore-if-new assq-or-assoc)))
     (elt (setcdr elt value))
     (ignore-if-new)
     (t
      (let ((new))
        (setq key-list (nreverse (cons key key-list)))
        (while key-list
          (if new
              (setq new (cons (car key-list) (cons new nil)))
            (setq new (cons (car key-list) value)))
          (setq key-list (cdr key-list)))

        (cond ((and (symbolp alist-or-sym)
                    (not (eq nil alist-or-sym)))
               (if append
                   (nconc alist (cons new nil))
                 (set alist-or-sym (cons new alist)))
               (setq alist (symbol-value alist-or-sym)))
              ((null alist)
               (setq alist (cons new nil)))
              (t
               (if append
                   (nconc alist (cons new nil))
                 (setcdr alist (cons (car alist) (cdr alist)))
                 (setcar alist new)))))))
    alist))


;;; Control constructs

;;;###autoload
(defmacro nf:do (variable-init-step test-exprs &rest commands)
  "Usage: (do ((variable init step) ...) (test expressions ...) command ...)

`do' expressions are evaluated as follows: The `init' expressions are
evaluated \(in order from left to right as specified\), the `variables' are
bound to fresh locations, the results of the `init' expressions are
stored in the bindings of the `variables', and then the iteration phase
beings.

Each iteration begins by evaluating `test'; if the result is false, then
the `command' expressions are evaluated in order, then the `step'
expressions are evaluated in the order, the associated `variables' are
bound to their results, and the next iteration begins.

If `test' evaluates to a true value, then the `expressions' are
evaluated from left to right and the value of the last expression is
returned as the value of the do expression.  If no expressions are present,
then the value of `test' is returned.

If both a step and init are omitted, then the result is the same as if
\(variable nil nil\) had been written instead of \(variable\)."
  `(let ,(mapcar (lambda (arg)
                   (list (car arg) (car (cdr arg))))
                 variable-init-step)
     (while (not ,(car test-exprs))
       ,@commands
       ,@(mapcar (lambda (arg)
                   (let ((step (nthcdr 2 arg)))
                     (and step
                          (list 'setq (car arg) (car step)))))
                 variable-init-step))
     ,@(cdr test-exprs)))

(put 'do 'lisp-indent-function 2)

;;;###autoload
(defun for-each (fn &rest lists)
  "Like mapcar, but don't cons a list of return values.
This function also handles multiple list arguments.
The first arg, a function, is expected to take as many arguments as there
are subsequent list arguments to for-each, and each argument list is
assumed to be the same length."
  (cond ((consp (cdr lists))
         (let ((listrun (make-list (length lists) nil))
               listsl listrunl)
           (while (car lists)
             (setq listrunl listrun)
             (setq listsl lists)
             (while listsl
               (setcar listrunl (car (car listsl)))
               (setcar listsl (cdr (car listsl)))
               (setq listrunl (cdr listrunl))
               (setq listsl (cdr listsl)))
             (apply fn listrun))))
        (t
         ;; Speed/minimal-consing hack for when there is only one arglist.
         (setq lists (car lists))
         (while lists
           (funcall fn (car lists))
           (setq lists (cdr lists))))))

;; indent like `while'
(put 'for-each 'lisp-indent-function 1)

;;;###autoload
(defun run-hook-with-arguments (hooksym &rest args)
  "Take hook name HOOKSYM and run it, passing optional args ARGS.
HOOKSYM should be a symbol, a hook variable.
If the hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with args ARGS.
If it is a list, the elements are called, in order, with ARGS."
  (and (boundp hooksym)
       (symbol-value hooksym)
       (let ((value (symbol-value hooksym)))
         (if (and (listp value)
                  (not (eq (car value) 'lambda)))
             (while value
               (apply (car value) args)
               (setq value (cdr value)))
           (apply value args)))))


;;; Special-purpose list operators

;; Inspired by (but rewritten from) a version written 1997-07-02
;; by Roland McGrath <roland@frob.com>:
;;
;;         I just whipped this up for uses like
;;               gnudoit "(push-command '(vm))"
;;         which I have a xbiff type thingy do when I click on it.  Running
;;         (vm) directly from the process filter loses in various
;;         excitingly obscure ways.
;;
;; It turns out that this is also a good way of delaying evaluation of
;; initialization commands until after the command loop is entered, which
;; is sometimes necessary for tweaking frame parameters.
;;;###autoload
(defun push-command (form)
  "Execute FORM as an interactive command next time the command loop runs.
This works by setting `unread-command-events' (which see).
A process filter can use this to run a command outside the filter context,
and initialization code evaluation can be delayed until after the
interactive command loop is entered."
  (let* ((key-name nil)
         (key (progn
                (while (or (null key-name)
                           (intern-soft key-name))
                  (setq key-name (format "push-command-%d" (random))))
                (intern key-name)))
         (event (vector key)))
    (fset key `(lambda ()
                 (interactive)
                 (unwind-protect
                     ,form
                   (global-unset-key ,event)
                   ,(and (fboundp 'unintern)
                         `(unintern ',key)))))
    (unwind-protect
	(setq unread-command-events (nconc unread-command-events (list key)))
      (global-set-key event key))))

;;;###autoload
(defun replace-auto-mode (regexp function)
  "Change the default major mode associated with a kind of file.
Modify first occurence of (REGEXP . old-function) pair in `auto-mode-alist'
to specify FUNCTION instead of old-function.  If pair does not exist,
prepend a new pair of the form (REGEXP . FUNCTION) to the
front of it.  Return value is meaningless.

Warning: `auto-mode-alist' might initially be read-only because it was
dumped into the text segment of the emacs image.  If this function detects
such a condition, the alist is automatically copied so that it may be
modified."
  (condition-case err
      ;; Lucid/XEmacs through 19.12 invoke the debugger when
      ;; debug-on-error is t, even in a condition-case.  Since this
      ;; function is called at startup, this is precisely what is
      ;; happening.  Temporarily binding the variable to nil seems
      ;; to avoid this bug.
      (let ((debug-on-error nil))
        (set-alist-slot 'auto-mode-alist regexp function nil 'assoc))
    (error
     (cond ((string= (car (cdr err)) "Attempt to modify read-only object")
            (setq auto-mode-alist (copy-alist auto-mode-alist))
            (set-alist-slot 'auto-mode-alist regexp function nil 'assoc))
           (t
            (signal 'error (cdr err)))))))

;;;###autoload
(defun set-buffer-list-order (olist)
  "Modify buffer list order to match OLIST.

The buffer list is modified so that its order is the same as the list of
buffers in OLIST.  Each element in OLIST must be a buffer object or the
name of a buffer.

All buffers present in OLIST will be first in the new buffer list and will
match the order in OLIST.  Buffers which are not in OLIST will come after
all the others but their relative present order will be preserved.

This buffer list is returned by the function `buffer-list' and affects the
behavior of `other-buffer', etc.  In Emacs 20 and later, each frame has its
own ordered buffer list.  This function modifies the selected frame's
buffer list only."
  (let (firstbuf buf)
    (while olist
      (setq buf (car olist))
      (and (stringp buf)
           (setq buf (get-buffer buf)))
      (cond ((buffer-live-p buf)
             (bury-buffer buf)
             (or firstbuf
                 (setq firstbuf buf))))
      (setq olist (cdr olist)))
    (setq olist (buffer-list))
    (while (not (eq (car olist) firstbuf))
      (bury-buffer (car olist))
      (setq olist (cdr olist)))))

;;;###autoload
(defun set-load-path (&rest path-lists)
  "Construct load path from any number of string-lists or strings.

Each argument should be a string or list of directories to be added to the
load-path.  Directories are only added to load-path if they exist and are
unique \(i.e. are not already in the new load-path\).  The previous value of
load-path is lost."
  (let ((load-path-new nil)
        (paths (flatten-lists path-lists))
        dir)
    (while paths
      (setq dir (expand-file-name (car paths)))
      (setq paths (cdr paths))

      (and (file-directory-p dir)
           (not (member dir load-path-new))
           (setq load-path-new (cons dir load-path-new))))
    (setq load-path (nreverse load-path-new))))

;;;###autoload
(defun set-minor-mode-string (minor-mode string &optional globalp)
  "Set MINOR-MODE display string according to STRING.
STRING need not actually be a string; see `mode-line-format'.
If optional arg GLOBALP is non-nil, then always set the global value of
`minor-mode-alist'.  Otherwise, set the buffer-local value if there is one."
  (let* ((alist (if globalp
                    (default-value 'minor-mode-alist)
                  minor-mode-alist))
         (cell (assq minor-mode alist)))
    (cond (cell
           (setcar (cdr cell) string))
          (t
           (setq alist (cons (list minor-mode string) alist))
           (if globalp
               (setq-default minor-mode-alist alist)
             (setq minor-mode-alist alist))))))


;;; Misc type predicates

;;;###autoload
(defun autoloadp (fn)
  "Return t if FN is autoloaded."
  (and (symbolp fn)
       (fboundp fn)
       (setq fn (symbol-function fn)))
  (and (listp fn)
       (eq (car fn) 'autoload)))

;;;###autoload
(defun functionp (x)
  "Return `t' if X is a function, `nil' otherwise.
X may be a subr, a byte-compiled function, a lambda expression, or a symbol
with a function definition.
In the last case, no attempt is made to determine if the lambda expression
is actually well-formed (i.e. syntactically valid as a function)."
  (cond
   ((subrp x))
   ((and (fboundp 'byte-code-function-p) (byte-code-function-p x)))
   ((and (consp x) (eq (car x) 'lambda)))
   ((and (symbolp x) (fboundp x)))
   (t nil)))

;;;###autoload
(defun macrop (x)
  "Return `t' if X is a macro, `nil' otherwise.
X may be a raw or byte-compiled macro.  No attempt is made to determine if
the macro is actually well-formed (i.e. syntactically valid)."
  (cond ((not (consp x))
         nil)
        ((eq (car x) 'macro)
         (functionp (cdr x)))))

(provide 'list-fns)

;;; list-fns.el ends here.
