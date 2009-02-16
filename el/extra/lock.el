
;; -*- auto-recompile: t -*-
;;; lock.el--- General Framework to provide locks.. eg. capslock, etc.
;; Time-stamp: <2002-05-21 15:38:40 deego>
;; Copyright (C) 2002 D. Goel
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: lock.el
;; Package: lock
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 0.1.5
;; Author's homepage: http://www.glue.umd.edu/~deego
;; NAMESPACES: lock-, lnum-, lletter- and lboth-

;; 


;; For latest version: 

(defvar lock-home-page  
"http://www.glue.umd.edu/~deego/emacspub/lisp-mine/lock")


 
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
(defvar lock-quick-start
  "Drop this file in your load-path.  Add to .emacs:
\(require 'lock-autoloads\)
\(lock-install\). 

Thereafter, use any/several of the 9 provided locks..   thus, use
commands like M-x lnum, M-x lletter, M-x lletter-upcase, M-x lboth
etc.  Each of these commands foo has a corresponding M-x foo-disable
and M-x foo-enable. You can have any number of locks active at any
given time,  and the effect of these locks is cumulative.  lnum stands
for numbers and lletters stands for letters \(a--z\). 

\(You might find some of these extra commands like M-x lletter-all-off
etc. useful..\).   Defining your own locks is easy.. see the codes of
the 9 locks defined or see commentary. 

Should you ever need to uninstall lock.el's advising (you
shouldn't!..), type M-x lock-uninstall... this will disable the
advising done by lock..

"
)

;;;###autoload
(defun lock-quick-start ()
  "Provides electric help regarding `lock-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lock-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar lock-introduction
  "Locks.el is locks unlimited.. it is locks-run-amok!
It is locks-galore.. 

Locks.el provides a general framework for defining new \(virtual\)
locks, and i n particular, provides 9 prepackaged locks... any number
of those locks can be active at any given time.  To just use any of
the pre-bundled locks, see M-x lock-quick-start.  To program new
locks, see M-x lock-commentary.

Defining virtual locks like caps-lock etc. in emacs is not just a
matter of defining a minor-mode with appropriate self-inserts.  Only
in some cases is a key bound to self-insert.  So, to put it mildly,
the locks need to defined carefully.


moreover, the effects of all the locks is cumulative.  Thus, if there
are 2 locks active: a virtual-caps-lock, a virtual-general-shift-lock,
and your keyboard's caps-lock.  Then the effect of pressing q will
lead to emacs seeing Q.  And this Q does not try to self-insert but
does whatever it is bound to...  

Working with virtual locks is like having an extra
caps-lock. shift-lock key within emacs... except, of course, you can
define the shift-keys you always wanted in Emacs.  For instance, i use
elisp and common-lisp so i noticed i used \( and \) much more than the
number-keys.  I now have my 'lnum' turned on almost all the time, and
got used to the new bindings within one day.. with an improvement in
typing speed.

NAMING SCHEME:


 Namespace lock-      
 Code directly relevant to lock, and very direct utilities, like
 lock-cycle-region, is prefixed with lock-.   Code which is mostly for
 internal purposes is prefixed with lock--.  
 Code which is utilities that are only tangentially related to the
 main functionality is prefixed with lock--. 
 Code which comprises utilities for elipers or debuggers \(who may
 write, say, more locks\), is prefixed lock--. 

 Namespace For Individual Locks:
 The individual locks get their own namespace.   Thus, we have lnum-,
 lletter- and lboth- namespaces too..

ACKNOWLEMENTS:

The author first learnt how to do this lock-stuff was by looking
through the code of John Paul Wallington's caps-lock.el.. a very nifty
utility.  I hope to convince John to become co-author of lock.el one
day.

Patches, comments etc. welcome.  Non-trivial patches will require FSF
copyright-assignment. " )

;;;###autoload
(defun lock-introduction ()
  "Provides electric help regarding `lock-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lock-introduction) nil) "*doc*"))

;;; Commentary:
(defvar lock-commentary
  "One reason i philosophically prefer taking care of keyboard-remapping
needs through emacs is that this is very platform-independent.. none
of that xmodmap vs. regedit stuff... the other reason is that one can
make keymapping through emacs ased on arbitrarily complex code...  the
third reaosn is that this is a lot of fun.  some drawbacks are that
you cannot do stuff like: map caps-lock to control, distinguish ctrl-l
from ctrl-r, distinguish 1-on-numpad from 1-on-left etc.


If you think that you use the shifted key of a char more often than
the char itself, by all means, make the shift the default.. implement
your own lock, and if you like, send it to me for inclusion in
a locks-repository :\) --- i was surprised that it took my fingers less than
a day to use the parens without the shift now! ---- i use elisp and
lisp heavily...  or i could just get a Symbolics keyboard, as jpw
pointed out :\).

there may be several methods of going about defining new locks using
lock.el, but here's the one i recommend..

This file defines exactly one minor mode.  Still, Each lock kinda
behaves as if it were a minor mode of its own.  Yet, Defining a new
lock can be as simple as two little sexps.. viz.  Simply do what i do
in the examples below.. viz.  simply use a
(lock-define foo &optional doc initial-value lighter) to
define your new lock.  Make sure to define the foo-map.  foo-map is
what actually maps a given key to more keys.. Note that keylist is
just an addition to lock-declared keys.  All lock-declared keys are
passed through the 'locking-process' first.  Note that the foo in the
above macro-definition should not be quoted.  Once you have done this,
your lock is called foo.  The user has commands foo, foo-enable and
foo-disable available to her.  M-x foo simply toggles the lock foo.
The variable foo stores the current status of the lock.  The actual
locking is done via foo-map which you need to define.  foo-map takes a
key and maps it to another key. If it returns nil, that is taken to
mean: identity.  If no lighter is provided, lock.el determines one for
you.. thus, if you really want an empty lighter, please provide "" for
lighter..



Think that the above doesn't do all you want?  You can use the above
method to first define the stuff and then made any changes/additions
if you like..  Or you can improve the lock-define command and let me
know..

Or, you can forgo using the macro altogether, and do stuff on your
own..  the best way is perhaps to read the code of the macro, but
here's a brief, and perhaps-outdated english summary--->

Let's say you want to call your lock foo. \(for instance, foo could be
lnum\).  viz. you would like that: just like minor-modes, Let M-x foo
be the toggling function, and the variable foo store the (resulting)
active-status of foo.

*  Then, define the function foo-map which maps characters.  This foo
is basically like a minor-mode in design, even though it is not really
a minor-mode.

* Defvar the variables foo (and unless you prefer to simply pass the
lighter as a string), the variale, foo-lighter.  The variable foo
tells whether the lock is active or not.  Our convention: This
foo-lighter should star with a space, and the first letter should be a
small l (L, not ONE).  foo-lighter is what shows up in the modeline.

* Finally, take a list of all characters that you think get
influenced by this lock, and run lock-install-keys on them.  Also,
declare your new lock to lock.el by using the lock-alist-add-lock command.
You are now basically done.  <-- this information is now outdated.. we
use advice now.. 


Now, you need to define some interface functions for the user. For
example, M-x foo, M-x foo-enable, and M-x foo-disable.  When foo gets
enabled, you should make the variable foo and possible, foo-lighter
buffer-local, unless of course, your design goals dictate
otherwise. See the similarity with minor modes?  Ideally, you should
make the function foo treat its arguments the way minor modes do.
Moreover, a good design for foo-enable should ensure that the user
doesn't have to do anything more..foo-enable sould ensure that
lock-minor-mode itself has been enabled.  It should make sure that
foo-keys have been installed, that foo is buffer-local and that the
lock foo has been declared to lock.el

Browse through the exampes of lnum- and lletters- ..

foo-map maps a given char to another char.  It can return nil too,
which simply means that it want to leave the char alone. You might
find the command M-x lock--show-char useful when defining
foo-map.  also, you might find going through the code of lnum-map
useful, for example.


This file, in well-demarcated sections, defines 9 different locks as
examples.  These locks are:

lletter, lletter-upcase, lletter-downcase, lnum, lnum-upcase, lnum-downcase,
lboth, lboth-upcase, lboth-downcase.  

lletter toggle simply toggle letters' case.  lletter-upcase forces
upcase.  lletter-downcase forces downcase. lnum toggles between numbers
and corresponding symbols on the keyboard.  lnum-upcase forces the
symbols.  lnum-downcase forces numbers. 


Note that multiple locks can be active at any one time.  This simply
means that each active lock, in turn maps the character you input.
You should be able to see the sequence in your modeline.  Thus, if
lletters and lletters-downcase are active, you see ll lld in
modeline, and here's how ?a gets processed:  a-->A-->a.  cool?
"
)

;;;###autoload
(defun lock-commentary ()
  "Provides electric help regarding `lock-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lock-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar lock-new-features
  "Help..."
)

;;;###autoload
(defun lock-new-features ()
  "Provides electric help regarding `lock-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lock-new-features) nil) "*doc*"))

;;; TO DO:
(defvar lock-todo
  "The next task:
 since the cookied won't work here, create a file called
 lock-autoloads for each of the \( locks defined here...
 
 Among the quick-start instructions or commentary, urge 'lock-authors'
 to create autoloads for their locks..

 Make the function lock-rotate-region accept C-u arguments as
well---this arguemnt should be a number.. how many times to do that
thing... 

"
)

;;;###autoload
(defun lock-todo ()
  "Provides electric help regarding `lock-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert lock-todo) nil) "*doc*"))

(defvar lock-version "0.1.5")

;;==========================================
;;; Code:

(eval-when-compile (require 'cl))


(defcustom lock-before-load-hooks nil "")
(defcustom lock-after-load-hooks nil "")
(run-hooks 'lock-before-load-hooks)


;;;====================================================

;; ,----SECTION 0 (lock--)
;; | ,----
;;     INTERNAL STUFF.. NOT RECOMMENDED FOR USE, SPECIALLY IF IT FROBS
;;     INTERNAL LOCK- VARIABLES...
;; | `----
;; `----
;;;====================================================



(defvar lock--default-key-translation-map nil
  "Just Once per session, this variable makes note of the initial
key-translation-map before lock.el is acvtivated... 
This helps either update the key-translation-map or restore it to the
initial value..
")



(defvar lock--default-keyboard-translate-table nil
  "Just Once per session, this variable makes note of the initial
key-translation-map before lock.el is acvtivated... 
This helps either update the key-translation-map or restore it to the
initial value..
")


(defvar lock--default-stored-p nil
  "internal...")

(defun lock--save-defaults ()
  "use lock--update instead.."
  (unless lock--default-stored-p
    (setq lock--default-key-translation-map 
	  (copy-tree key-translation-map))
    (setq lock--default-keyboard-translate-table  
	  (copy-tree keyboard-translate-table))
    (setq lock--default-stored-p t)))

(defun lock--update ()
  "This is basically the main function, that should be called after
each frobbing of lock-alist.... "
  (lock--save-defaults)
  (lock--get-defaults)
  (lock--update-from-lock-alist))

(defun lock--get-defaults ()
  (setq key-translation-map 
	(copy-tree lock--default-key-translation-map))
  (setq keyboard-translate-table
	(copy-tree lock--default-keyboard-translate-table)))

(defun lock--update-from-lock-alist  ()
  "Don't use.. use lock--update instead.."
  (let ((la (reverse lock-alist)))
    (while la 
      (let* ((thislocks (car la))
	     (thislock (car thislocks)))
	(when (eval thislock)
	  (funcall (or (third thislocks) 
		       lock--default-update-function
		       )
		   (eval (lock--concat-symbols thislock "-map")))))

      (setq la (cdr la)))))






(defvar lock--default-update-function		       
  'lock--update-via-key-translation-map
)



(defun lock--update-via-key-translation-map (map)
  "Updates key-translation-map from the given MAP...
Note: The format of the map is NOT same as that of, say, lnum-map below...
"
  (let ((thismap map)
	(ktt-old (copy-tree key-translation-map)))
    (while thismap
      (lock--define-key (car thismap) ktt-old key-translation-map)
      (setq thismap (cdr thismap))))) 

(defun lock--update-via-keyboard-translate-table (map)
  "Updates keyboard-translate-table from the given map..
The format of map is similar to lnum-map in locks.el "
  (let ((thismap map)
	(ktt-old (copy-tree keyboard-translate-table)))
    (while thismap 
      (lock--keyboard-translate (car thismap) ktt-old)
      (setq thismap (cdr thismap)))))

(defun lock--keyboard-translate (key12 sourcetable)
  "key12 is a list of 2 keys..  unless of course, the user wants to
pass more keys for some reason...  the result is applied to
keyboard-translate-table ..   If the destination already exists in
sourcetable, then it is chased (once) before getting applied... "
  (let* ((from (first key12))
	 (to (second key12))
	 (finalto to))
    (when sourcetable
      (setq finalto (or (aref sourcetable from) to)))
    (keyboard-translate from to)))


(defun lock--define-key (key12 sourcemap sinkmap)
  "key12 is a list of 2 keys...  unless of course, the user wants to
pass  more keys for some reason..."
  (let* ((key1 (copy-tree (first key12)))
	 (key2 (copy-tree (second key12)))
	 (keya (if (arrayp key1) key1
		 (vector key1)))
	 ;;(keyamirror (lock--lookup-key sourcemap keya))
	 (keyb (if (arrayp key2)
		   key2
		 (vector key2)))
	 (keybmirror (lock--lookup-key sourcemap keyb)))
    
    ;; if keyb maps to somewhere, this means that the ultimate
    ;; destination of b is c, so we should map a to c instead of b..
    ;; unless, of course we have always done that....if keyb doesn'[t
    ;; map to somewhere, then lock--lockup-key has returned us the
    ;; back keyb in any case.... the complicated case is
    ;; lock--lookup-key has returned us the map of keyb as a list of
    ;; stuff... in this case, the user perhaps intends to map keya to
    ;; that list of stuff.. but we don't know how to do that.. and we
    ;; will map it to keyb instead...
    (unless (listp keybmirror)
      (define-key sinkmap keya keybmirror))))
      


(defun lock--lookup-key (map keya)
  "please note that keya is a vector..  this function is just like
lookup-key, except that if keya is too long, it tries to break up the
key into appropriate shorter pieces.. and construct a vector out of
the resulting key-translations.. of course, sometimes the results may
be functions instead of keys..  The aim of this function is to return
what would have happened if you had pressed keya in presence of map.
Thus if a part of key does not get mapped, it remains itself..  any
part that gets mapped should get mapped.. sometimes part of the
results in a function and another part results in keystrokes, in SUCH
a case, this function returns you the appropriate list of such
functions.. if the whole key-sequence itself results in a simple
function-name, then that function is returned....
"
  (let ((keyb (lookup-key 
	       map keya)))
    (if 
	(numberp keyb)
	(let* 
	    ((keyc (lock--lookup-key 
		    map (subseq keya 0 keyb)))
	     (keyd 
	      (let ((keyrem (subseq keya keyb (length keya))))
		(or 
		 (lock--lookup-key
		  map
		  keyrem)
		 keya))))
	  (cond 
	   ((and (vectorp keyc)
		 (vectorp keyd))
	    (vconcat keyc keyd))
	   (t
	    (append
	     (if (listp keyc) keyc (list keyc))
	     (if (listp keyd) keyd (list keyd))))))
      (if (null keyb) 
	  keya keyb))))



;; from concat-symbols-my
(defun lock--concat-symbols (&rest args)
  "each arg be a symbol.."
  (intern (apply 'concat
		 (mapcar '(lambda (a) (format "%s" a)) args))))


  



(defun lock--map (char &optional times)
  "This will NOT test for validity of new-char.. to allow the
innovative user to do innovative things.. Only if newchar is null does
it get reset to char..

perhaps use this to pass any amount of information they may
like... across various locks... ? 
"
  (unless (numberp times) (setq times 1))
  (if (<= times 0) char
    (let* ((next lock-alist)
	   lock
	   (newchar char)
	   )
      (while next
	(setq lock (caar next))
	(setq newchar 
	      (or
	       (if (eval lock) 
		   (funcall 
		    (lock--concat-symbols lock "-map")
		    newchar)
		 nil)
	       newchar))
	(setq next (cdr next)))
      (lock--map newchar (- times 1)))))


;(defun lock--add-event (char)
;  (if (char-valid-p char)
;      (add-to-list 'unread-command-events char)
;    (error "Lock-add-event was Not supplied a character")))



;;;====================================================

;; ,----SECTION 10 (lock-- )
;; | ,----
;; | | MISC utils.. both for lock.el and the user...
;; | | stuff with lock-- is really rather internal.. not meant to be
;; used outside of this program...
;; | `----
;; `----
;;;====================================================

(defun lock--double-map (map)
  (let ((mm (copy-tree map)))
    (append map
	    (lock--reverse-map map))))

(defun lock--reverse-map (map)
  (let ((mm (copy-tree map)))
    (mapcar
     '(lambda (arg)
	(list (cadr arg) (car arg)))
     map)))



(defun lock--update-modeline ()
  "Ensures proper ordering of locks in modeline.

The new ordering will be the same as the one in lock-alist.  IMHO,
this command will perhaps seldom be needed...
"
  (interactive)
  (setq minor-mode-alist
	(set-difference
	 (copy-tree minor-mode-alist)
	 lock-alist
	 :test 
	 '(lambda (a b) (eql (car a) (car b)))))
  (setq minor-mode-alist 
	(append minor-mode-alist (copy-tree lock-alist))))



(defun lock--rotate-string (string map)
  "Is per-map cycling..."
  (apply 'concat
	 (mapcar
	  (lambda (char)
	    (format "%c" (or (lock--rotate-map char map) char)))
	  string)))

(defun lock--rotate-map (char map)
  "Rotates through a map..."
  (let* 
      ((match-list
	(first (member* char map
			:test '(lambda (a b) (membe!r a b)))))
       (char-list (member char match-list)))
    (or (second char-list) (first match-list))))

(defmacro lock--withit (expr &rest rest)
  "Caution: var-capture by its very nature.."
  `(let ((it ,expr))
     ,@rest))

;;;====================================================
;; ,----SECTION  20 .. THE MAIN LOCK-ALIST- STUFF.. that deals with
;;   lock-alist. 
;; | ,----
;; | | MANAGING THE LIST OF (ACTIVE/NONACTIVE) LOCKS....
;; | | lock-alist- management stuff...
;;     YOU PROBABLY WILL NEVER NEED TO USE THIS EVEN IF YOU ARE A
;;     LOCK-PROGRAMMER.. YOU CAN SIMPLY USE LOCK-DEFINE FOR ALL YOUR NEEDS..

;; | `----
;; `----
;;;====================================================





(defvar lock-alist nil
  "List of all declared locks.
each member, in its simplest form looks like: 
 \(name lighter \)

These locks, when active, are applied in turn... see examples in
locks.el.. 

More fields can be specified.. the detailed form is:

 \(name lighter method \)

Method is a function that will be called to update.. If none is
supplied the default 'lock--update-via-keyboard-translate-table, which
IMHO, is the right way to go, is assumed...

Another that is available, and works fine right now, is:
 lock--update-via-key-translation-map.  

A third one, lock--update-via-self-insert is easy to implement, \(has
been implemented elsewhere\), but not yet included here.. unless
there's demand for it..  in principle, you can define more methods and
call them here....  If you do, and if the rest of lock.el creates
hindrances, let me know...


When the self-insert method is active, the lock works as follows: The
self-insert-command is advised so as to check for the lock...  

this third method is currently nonfunctional here..
<-- i have implemented the third method elsewhere, but am not
bothering transferring it over here, unless there is demand for it..

If you choose use key-translation-map, here's what you might run into:
  
 This happened to me on a terminal: While the translation from a to A
 and from A to a works fine, some other keys may stop working.
 Consider the right-arrow key.  working On a windoze QVT/term, the
 function-key-map had translated the <right> into an ESC O A, which
 was interpreted by Emacs to forward-char.. however, the
 key-translation-map then converted it into ESC o a, and the right key
 thus stopped working correctly..

If you use 'keyboard-translate-table \(the one we recommend\), things
 are fine.. you literally translate your keyboard.. but some 'special'
 locks may face minor problems as follows: viz.  If you define funky
 locks that map special stuff like ESC to other stuff, then note that
 ESC is NOT same everywhere.. ESC is 24 on terminals... but it is not
 even a valid key on sunspark keyboards.. It is function-key-map, i
 believe that translates that ESC into a valid key-sequence for
 SPARCS...

 Moreover, with this method, you cannot do the 'fancy' stuff like
 getting one key to substiture multiple keys or vice verse...


 ")


(defun lock-alist-add-lock (lock lighter &optional in-front-p)
  "Note that lock is a symbol here.  If a lock by this name already
exists, this function does nothing..  The lock is preferably added at
the end of the lock-alists (and therefore, in front of
minor-mode-alist), but this can be overrided using IN-FRONT-P.  The
aim of doing this is to ensure that the order of locks in the modeline
is the same as that in locks themselves... so you know which order
locks should be applied.  note however that, if the order of your locks does
matter for you, then your locks are pretty funky!---in other words,
locks should be associative.. in other words, i can't imagine anyone
turning on both lletter and lletter-upcase at the same time!

you may not need to use this if you use lock-define...
"
  (unless (assoc lock lock-alist)
    (add-to-list 'lock-alist (list lock lighter) (not in-front-p))
    (add-to-list 'minor-mode-alist 
		 (list lock lighter) (not in-front-p))))



   


;;;====================================================
;; ,----SECTION 30 .. (lock-).. 
;; | ,----
;; | | main lock-stuff.. and when applicable: LOCK minor mode...  
;; | `----
;; `----
;;;====================================================
;; once you activate lock-minor-mode, you probably never need to
;; 'deactivate' it...  




(defun lock-rotate-string (lrstring &optional times)
  (interactive "s")
  (apply 'concat
	 (mapcar '(lambda (char)
		    (format "%c" (or (lock--map char times) char)))
		 lrstring)))
	 
  
(defun lock-rotate-region (beg end )
  "This function kills (note: kills, not deletes) the region and
replaces it by the new one... "
  (interactive "*r")
  (let* ((aa (min beg end))
	 (bb (max beg end))
	 (str (buffer-substring aa bb))
	 (newstr (lock-rotate-string str)))
    (save-excursion
      (goto-char bb)
      (insert newstr)
      (delete-region aa bb))))
	      


;;;====================================================
;; ,----SECTION 40 
;; | ,----
;; | | The LOCK defining stuff...
;; | | 
;; | `----
;; `----
;;;====================================================



(defmacro lock-define  (foo &optional doc initval lighter )
  "The main utility..  

note that just defining a lock foo does not turn either the lock on,
or the lock-minor-mode on.  Use M-x foo-enable to turn the lock on,
which, of course will activate lock-minor-mode too... 


to ward off multiple evals, i have introduced an error message
below...  basically the point is that i could not construct the
gensyms etc properly to take care of multiple evals, so i haev simply
disallowed nonprimitived..

keylist may not necc be a primitive because used only once here...

" 
  (unless (every 'identity
		 (mapcar '(lambda (arg) (or (symbolp arg)
					    (stringp arg)))
			 (list foo doc initval lighter
			       )))
    (error "This macro only accepts primitives..."))
  ;;(unless (or (null keylist) (eql (first keylist) 'quote))
   ;; (error "List should be a quoted list..."))
  `(progn 
     ;; notice that ,gfoo gets the quoted form of foo.. viz. 
     ;; ,gfoo = 'letter...
     ;; doing this to get rid of any previous buffe-rlocal properties...
     ;;;(makunbound ',foo)
     (defvar ,foo ,initval
       ,(format "Value of the lock %s" foo))
     ;;(unless lock-global-p (make-variable-buffer-local ',foo))
     (lock-alist-add-lock ',foo ,lighter)
     ;; note that using arg here does NOT imply var-capture.. i think..
     
     (defun ,foo (arg)
       ,doc
       ;; this interactive will generate a warning upon
       ;; compilation.. that warning should be ignored..
       (interactive "P")
       (when (null arg)
	 (setq arg 1)
	 (when ,foo 
	   (setq arg -1)))
       (if (plusp arg)
	   (,(lock--concat-symbols foo "-enable"))
	 (,(lock--concat-symbols foo "-disable"))
	 ))
     
     
     (defun ,(lock--concat-symbols foo "-enable") ()
       (interactive)
       (setq ,foo t)
       (lock--update)
       )
     
     (defun ,(lock--concat-symbols foo "-disable") ()
       (interactive)
       ;;(make-variable-buffer-local ',foo)
       (setq ,foo nil)
       (lock--update))
     ))




(defun lock-set-defaults ()
  "This will set the current stuff as the default for the session.. 
USe it only if you are sure you wanna use it... :) "
  (let ((lock--default-stored-p nil))
    (lock--save-defaults)))

;;;###autoload
(defun lock-reset ()
  "If things have gone funky... , this goes a little bit towards 
making them saner for you.."
  (interactive)
  (setq key-translation-map lock--default-key-translation-map)
  (setq keyboard-translate-table 
	(copy-tree lock--default-keyboard-translate-table))
  (setq lock--default-stored-p nil)
  (setq lock-alist nil))

   
;;;###autoload
(defun lock-disable ()
  "Turn off all locking without affecting locks' values..
The current status of locks can be restored by typing M-x lock-enable.. 

this function does not affect the values (on or off) of the various
locks.., thus enabling you  to revert to the current lock-state
through lock-enable. "
  (interactive)
  (let ((lock-alist nil))
    (lock--update)))

;;;###autoload
(defun lock-enable ()
  "Re-Enable normal locking based on the values of the various locks... "
  (interactive)
  (lock--update))


;;;====================================================
;; ,----SECTION 300
;; | ,----
;; | | tangential interactive utilities.. 
;; | | useful for the elisper who programs new locks...
;; | `----
;; `----
;;;====================================================



;;; 2002-05-09 T15:08:37-0400 (Thursday)    D. Goel
;;;###autoload
(defun lock--show-char  ()
  "show the char you type...
is a copy of show-char-my in functions-my.el
This function may be retained here, but will soon be moved to
charhelp.el.. for now, the primary function  is show-char-my"
  (interactive)
  (lock--withit 
   (read-char "Enter any character: ")
   (message "You entered the character:    %S" it)))

(defun lock--locks-list ()
  (interactive)
  (message "%s..%s" (length lock-alist) lock-alist))



;;;====================================================

(provide 'lock)
(run-hooks 'lock-after-load-hooks)



;;; lock.el ends here
