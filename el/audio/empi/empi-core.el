;;; EMPI-CORE.EL --- Generic portions of EMPI

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	12 May 2004
;; Version: 	1.0
;; Keywords:	empi, music

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <andyetitmoves@gmail.com>) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; empi-core|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Generic portions of EMPI
;; |$Date: 2004/05/12 10:56:02 $|$Revision: 1.1 $|~/packages/empi-core.el

;;; Commentary:

;; This is EMPI, or The Emacs Media Player Interface, a generic package to
;; interface any media player with Emacs and make it offer its services through
;; an unified interface written in Emacs Lisp. An obvious question which comes
;; to one's mind is "why this package when there are plenty of packages around
;; enabling one to play music in emacs?". True, there are, but most of them have
;; their limitations. The most common limitation is that the interfacing is done
;; to a specific player, usually mpg123 or mpg321. This means that the user is
;; tied to that player, and believe me, people have strong preferences regarding
;; this. This indicates a clear lack of extensibility, as someone has to write a
;; few thousand lines of code everytime a new player comes up. Further,
;; obviously, there is no unified user interface for all these packages. Instead
;; of nitpicking at other packages, it would be better to see the features of
;; EMPI, they provide a good overview of the motivation behind this package.

;; Central to EMPI is a framework which brokers the communication between user
;; interface code and backends communicating with a particular resource. The UI
;; code asks for information or requests action from the framework which
;; forwards the same to the appropriate backend. As a result, EMPI is totally
;; transparent towards how the backend operates, it could be through an external
;; process or even some other lisp package. This means that one could as well
;; drive the other media player packages through EMPI.

;; EMPI operates on a set of "players", each of which have a few "handlers". A
;; command or a query is passed to each of the handlers of the "active player"
;; chosen by the user, till a favourable response is obtained. For example, for
;; the "xmms" player, you could have a handler for the small command-line
;; interface provided by xmms itself, a handler using "xmms-shell" - a more
;; sophisticated command line interface to xmms, as well as a handler employing
;; lisp code to communicate with the control socket of xmms.

;; If you are now thinking that the entire headache is now transferred to the
;; backends for which one has to write lots of lisp code, there are a few
;; generic backends to chose from, like "empi-proc" for communicating through
;; the command-line, which reduces creating a backend for a specific player to
;; writing some tens of lines of rc file like (and perhaps cryptic) code.

;; EMPI is also indifferent towards what the command or query requested. This
;; means that the end user code could even ask for the
;; :kiss-the-butterfly-fluttering-through-the-next-street action provided the
;; backend supports it. This is significant for two aspects - One, EMPI doesn't
;; restrict its functionality to the common subset of all the features of its
;; backends. The end code could ask for some specialized action offered by one
;; odd program, and then gracefully degrade if that action is not
;; supported. Second, as I have been pretending all this while, this isn't just
;; an interface for *media players*, it could be so for anything else as
;; well. Only that instead of an `empi-play' or an `empi-pause', you would have
;; some other function using the framework. Of course, the framework isn't just
;; a dumb forwarder, it does a lot of other things as well...

;; Ok, if you are tired or lost reading this rant, here comes the concrete
;; part. As you would have known by now, just loading this file won't get you
;; anywhere. You would have all these nice looking interactive functions at your
;; disposal, and all would return an "Unsupported or failed action ..."
;; error. Get the backends for the players you have - there are a few shipped
;; for the most common ones, but you might have to get it from somewhere or
;; write one yourselves, or fall back on what has been the bane of other
;; packages - get a program for which the backend exists. You don't need to be a
;; lisp guru to write backends, if you know lisp just a bit beyond blindly
;; pasting that brackety stuff from the commentary of packages to your .emacs,
;; you should be through. And if you do happen to write a backend, do mail it to
;; me, so that I could include it in the distribution.

;; First get the EMPI and the backend files somewhere on the load path. From
;; there on, customizing the `empi' group should take you all the way. A vital
;; customization is the `empi-player-alist'. This is where you declare your
;; players and string together the handlers required. There should be a symbol
;; typically named as empi-playername-backendname provided by each backend
;; package. This is the handler symbol. You will typically need to know the
;; handler symbols and the files where they are declared.

;; Its a good idea, though not necessary, to keep empi-forward as the last
;; handler for each player. This is a generic handler which tries to convert an
;; unhandled command to something else, hoping that the new command would be
;; handled. For example, it would change an unsupported :voladd action,
;; requesting for a addition in volume, to two commands :qvolume, querying for
;; the current volume, and :volume asking for the volume to be set to some
;; value. But do ensure that this is the *last* handler in the list.

;; The order of handlers is important for the fact that this is the order in
;; which support for a specific command is queried. Typically, one should keep
;; the one that does the job well and does it fast, ahead in the list. It isn't
;; necessary for the one handling the most commands to come first, as if there
;; are commands in it which no other handler handles, control would anyway come
;; to it. If there is a handler which executes the only command it handles
;; extremely fast, you could position it higher up.

;; Use `empi-initial-backends' to load the backends which define the
;; backends. Also ensure that you select the default player. The default is to
;; select the first player in the list after telling you about that. Note that
;; there might be some initialisation required by the backends as well, like for
;; example, specifying the hostname and port number, if a backend operates
;; through a network connection. You can also get some of the modes, like the
;; `empi-caption-mode', to display a constantly updated media caption in the
;; title bar, started using the customization interface.

;;; Code:

(defconst empi-version "1.0")

;;;; User serviceable area

(defgroup empi nil
  "The Emacs Media Player Interface"
  :prefix "empi-"
  :link '(emacs-commentary-link :tag "Introduction to EMPI" "empi-core")
  :group 'multimedia :group 'external)

(defcustom empi-initial-backends nil
  "The list of backends required at startup by EMPI.
This list is usually decided by the handlers in `empi-player-alist'.
Each element of the list needs to be feature to be provided by a file with a
similar filename on the load path. See `require' for more details.
Do not set this variable directly, use the customization interface instead."
  :type '(repeat (symbol :tag "Feature"))
  :group 'empi :set '(lambda (sym val)
		       (eval-after-load "empi" `(mapc 'require (quote ,val)))
		       (set sym val)))

(defun empi-player-alist-custom-friendly (val conv)
  (let ((cur val) item)
    (while cur
      (setq item (cdr-safe (car-safe cur)))
      (while item
	(cond
	 ((eq conv 'get)
	  (or (listp (car-safe item)) (setcar item (list (car item)))))
	 ((eq conv 'set)
	  (and (listp (car-safe item)) (not (cdr-safe (car item)))
	       (setcar item (caar item)))))
	(setq item (cdr-safe item)))
      (setq cur (cdr-safe cur))) val))

(defcustom empi-player-alist nil
  "Alist of players and their corresponding handlers for EMPI.
Each of the elements in the alist has the player name as the car and a list of
handlers as the cdr. Each handler is either the symbol containing the function
to call as its value, or a list. In the first case, the handler is given a
chance to handle any action. In the second case, the first element of the list
is the name of the symbol, and the elements following are action symbols for
which this handler is used. It is advised to keep `empi-forwarder' as the last
handler for all players."
  :type
  '(repeat
    (list :format "   %v"
	  (string :tag "Name")
	  (repeat :inline t :tag "Handler List"
		  :value (empi-forwarder)
		  (list :format "%v"
			:value (empi-)
			(symbol :tag "Symbol")
			(set :inline t :format "%v"
			     (list :inline t :format "%v"
				   (const :format "" :restrict)
				   (repeat :tag "Selective Actions"
					   (symbol :format "%v" :value :))))))))
  :get '(lambda (sym)
	  (empi-player-alist-custom-friendly (symbol-value sym) 'get))
  :set '(lambda (sym val)
	  (set sym (empi-player-alist-custom-friendly val 'set)))
  :group 'empi)

(put 'empi-player-alist 'custom-tag
     (substitute-command-keys
      "Empi Player Alist: This variable has no sane default.
This always needs to be specified. To know more about this variable, read the
package commentary using `\\[finder-commentary] empi-core'.
See {srcdir}/cust-eg.html for an example customization."))

(defcustom empi-default-player nil
  "*The default player used for all EMPI actions.
Should be one of the values defined by `empi-player-alist'. The current player
used can be modified by a call to `empi-select-player'."
  :type '(choice (const :tag "First in list" nil)
		 (string :tag "Player name")) :group 'empi)

;; Utility functions

(defmacro oconcat (&rest args)
  (let (actual (str "") item)
    (while args
      (setq item (car args))
      (if (stringp item)
	  (setq str (concat str item))
	(unless (string= str "")
	  (setq actual (cons str actual))
	  (setq str ""))
	(setq actual (cons item actual)))
      (setq args (cdr args)))
    (or (string= str "") (setq actual (cons str actual)))
    (if (cdr actual)
	(cons 'concat (nreverse actual))
      (apply 'concat (nreverse actual)))))

(defalias 'oconcat 'concat)

(defun byte-compile-oconcat (form)
  (let (actual (str "") item (args (cdr form)))
    (while args
      (setq item (car args))
      (if (stringp item)
	  (setq str (concat str item))
	(unless (string= str "")
	  (setq actual (cons str actual))
	  (setq str ""))
	(setq actual (cons item actual)))
      (setq args (cdr args)))
    (or (string= str "") (setq actual (cons str actual)))
    (byte-compile-form
     (if (cdr actual)
	 (cons 'concat (nreverse actual))
       (apply 'concat (nreverse actual))))))

(put 'oconcat 'byte-compile 'byte-compile-oconcat)

(or (fboundp 'warn)
    (defmacro warn (&rest args)
      `(message (oconcat "Warning: " ,@args))))

(defmacro serror (str)
  `(signal 'error (list ,str)))

(defmacro emperet (str)
  `(serror (oconcat "Unexpected result on " ,str " query")))

(defmacro empuns (str)
  `(serror (oconcat "Unsupported or failed action " ,str)))

(defmacro empuns-nf (str)
  `(progn
     (message (oconcat "Warning: Unsupported or failed action " ,str)) nil))

(defun list-alternate (l)
  (if (listp l)
      (if l
	  (append (list (list (symbol-name (car l))))
		  (list-alternate (cdr (cdr l)))))
    (error "Argument is not a list")))

(defsubst plist-keys (sym)
  (list-alternate (symbol-plist sym)))

(defun walk-white (str begin &optional rev)
  (if (or (= (elt str begin) ? )
	  (= (elt str begin) ?\n)
	  (= (elt str begin) ?\t)
	  (= (elt str begin) ?\v))
      (if (if rev (> begin 0) (< begin (1- (length str))))
	  (walk-white str (+ begin (if rev -1 1)) rev))
    begin))

(defun trim-string (str)
  (if (= (length str) 0) ""
    (let ((nwbeg (walk-white str 0)))
      (if nwbeg
	  (substring str nwbeg (1+ (walk-white str (1- (length str)) t))) ""))))

(eval-and-compile
  (defmacro with-unlogged-message (&rest args)
    `(let ,(list
	    (if (boundp 'log-message-filter-function)
		'(log-message-filter-function #'ignore)
	      '(message-log-max nil))) ,@args))
  (defmacro unlogged-message (&rest args)
    `(with-unlogged-message
      (message ,@args))))

(defun define-keys (map &rest args)
  (while (cadr args)
    (define-key map (car args) (cadr args))
    (setq args (cddr args))))

(defun gcd-2 (a b)
  (let (temp)
    (while (not (= b 0))
      (setq temp a a b b (mod temp a)))) a)

(defmacro with-critical-lock (lock &rest body)
  "Lock the enclosing code with LOCK against re-entrancy.
LOCK is evaluated thrice in this process."
  `(unless (intern-soft ,lock)
     (intern ,lock)
     (unwind-protect
	 (progn ,@body)
       (unintern ,lock))))
(put 'with-critical-lock 'lisp-indent-function 1)
(def-edebug-spec with-critical-lock t)

;;;; Core commands

(defvar empi-current-player nil)

;;;###autoload
(defun empi-select-player (pname)
  (interactive
   (list
    (if (and (listp empi-player-alist) (> (length empi-player-alist) 0))
	(completing-read
	 (concat "Enter player name"
		 (if (and empi-current-player (listp empi-current-player))
		     (concat " (" (car empi-current-player) ")")) ": ")
	 empi-player-alist nil t nil nil (car empi-current-player))
      (error "No players available"))))
  (let ((newp (assoc pname empi-player-alist)))
    (if newp
	(if (listp (cdr newp))
	    (setq empi-current-player newp)
	  (error "The handler list for the player specified is invalid, \
please check your settings"))
      (error "The player specified does not exist"))))

(defun empi-ensure-current-player ()
  (when (not empi-current-player)
    (if (stringp empi-default-player)
	(empi-select-player empi-default-player)
      (if (listp empi-player-alist)
	  (if (= (length empi-player-alist) 0)
	      (error "No players available")
	    (message "No default player, defaulting to first player in list")
	    (setq empi-current-player (car empi-player-alist)))
	(error "Invalid player alist")))))

(defvar empi-forwarder-chain nil)

(defun empi-command (cmd &rest args)
  (empi-ensure-current-player)
  (if (not (listp (cdr empi-current-player)))
      (error "Invalid player entry %s" (car empi-current-player))
    (let ((htail (cdr empi-current-player)) ret func cur)
      (setq empi-forwarder-chain (list cmd))
      (while (and htail (not ret))
	(setq cur (car htail))
	(and (listp cur)
	     (setq cur (and (let ((restrict (plist-get (cdr cur) :restrict)))
			      (or (not restrict) (memq cmd restrict)))
			    (car cur))))
	(when cur
	  (if (and (symbolp cur) (functionp (setq func (symbol-value cur))))
	      (setq ret (apply func cur cmd args))
	    (warn "Invalid handler found for player "
		  (car empi-current-player) ", ignoring")))
	(setq htail (cdr htail)))
      (setq empi-forwarder-chain nil) ret)))

(defun empi-forward (cmd command-func &rest args)
  (or empi-forwarder-chain (error "No forwarder chain present"))
  (unless (memq cmd empi-forwarder-chain)
    (setq empi-forwarder-chain (cons cmd empi-forwarder-chain))
    (let (retval)
      (unwind-protect
	  (setq retval (apply command-func cmd args))
	(setq empi-forwarder-chain (cdr-safe empi-forwarder-chain)))
      retval)))

;;;; Cache support

(defvar empi-global-cache nil)
(defvar empi-cache-lock 0)
(defvar empi-query-dependencies nil)

(defun empi-add-dependency (query &rest actions)
  (mapc '(lambda (item)
	   (let ((val (plist-get empi-query-dependencies item)))
	     (if val (or (memq query val) (setcdr val (cons query (cdr val))))
	       (setq empi-query-dependencies
		     (plist-put empi-query-dependencies item (list query))))))
	actions))

(defalias 'empi-static-dep 'empi-add-dependency)

(defun empi-uncache (&rest keys)
  (mapc
   '(lambda (item)
      (setq empi-global-cache (plist-put empi-global-cache item nil))) keys))

(defun empi-merge-command (cmd &rest args)
  (setq args (apply 'empi-command cmd args))
  (if (listp args)
      (let ((ftail args) necdr
	    (necessary (plist-get empi-query-dependencies cmd)))
	(if necessary
	    (setq necessary (append necessary (list cmd)))
	  (setq necessary (list cmd)))
	(while ftail
	  (setq necessary (delq (car ftail) necessary))
	  (and (symbolp (car ftail)) (cadr ftail)
	       (setq empi-global-cache (plist-put empi-global-cache (car ftail)
						  (cadr ftail))))
	  (setq ftail (cddr ftail)))
	(apply 'empi-uncache necessary)
	(plist-get empi-global-cache cmd))
    (apply 'empi-uncache (plist-get empi-query-dependencies cmd))
    (setq empi-global-cache (plist-put empi-global-cache cmd args)) args))

(defun empi-cached-command (cmd &rest args)
  (or (plist-get empi-global-cache cmd)
      (apply 'empi-merge-command cmd args)))

(defsubst empi-clear-cache () (setq empi-global-cache nil))

(defmacro with-empi-cache-group-by (group &rest body)
  `(unwind-protect
       ,`(,@group
	  (if (= empi-cache-lock 0) (empi-clear-cache))
	  (setq empi-cache-lock (1+ empi-cache-lock))
	  ,@body)
     (setq empi-cache-lock (1- empi-cache-lock))))

;;; Convenience macros/aliases

(eval-when-compile (require 'edebug))

(defmacro with-empi-cache (&rest body)
  `(with-empi-cache-group-by (progn) ,@body))
(put 'with-empi-cache 'lisp-indent-function 0)
(def-edebug-spec with-empi-cache (body))

(defmacro with-empi-cache-let (vars &rest body)
  `(with-empi-cache-group-by ,`(let ,vars) ,@body))
(put 'with-empi-cache-let 'lisp-indent-function 1)
(def-edebug-spec with-empi-cache-let let)

(defalias 'empi-query 'empi-cached-command)
(defalias 'empi-action 'empi-merge-command)

(defmacro empi-forward-command (cmd &rest args)
  `(empi-forward ,cmd 'empi-command ,@args))

(defmacro empi-forward-query (cmd &rest args)
  `(empi-forward ,cmd 'empi-query ,@args))

(defmacro empi-forward-action (cmd &rest args)
  `(empi-forward ,cmd 'empi-action ,@args))

(defmacro empi-symbol-name (sym)
  (if (symbolp sym)
      (symbol-name sym)
    (list 'symbol-name sym)))

(defmacro empi-simple-command (cmd &rest args)
  `(or (empi-command ,cmd ,@args) (empuns (empi-symbol-name ,cmd))))

(defmacro empi-simple-query (cmd &rest args)
  `(or (empi-query ,cmd ,@args) (empuns (empi-symbol-name ,cmd))))

(defmacro empi-simple-action (cmd &rest args)
  `(or (empi-action ,cmd ,@args) (empuns (empi-symbol-name ,cmd))))

(defmacro empi-recov-command (cmd &rest args)
  `(or (empi-command ,cmd ,@args) (empuns-nf (empi-symbol-name ,cmd))))

(defmacro empi-recov-query (cmd &rest args)
  `(or (empi-query ,cmd ,@args) (empuns-nf (empi-symbol-name ,cmd))))

(defmacro empi-recov-action (cmd &rest args)
  `(or (empi-action ,cmd ,@args) (empuns-nf (empi-symbol-name ,cmd))))

;;; Display update support

(defvar empi-update-list nil)
(defvar empi-update-timer nil)
(defvar empi-update-disable nil)
(defvar empi-update-schedule nil)
(defvar empi-update-schedule-pos nil)

;;; Redeclared with defcustom, this is to silence the compiler.
(defvar empi-update-reference-interval)

(defun empi-update-build-schedule (list)
  (let* ((length (/ (aref empi-update-timer 2) (aref empi-update-timer 1)))
	 (ring (make-list length nil)) (ringpos 0) ringptr interval)
    (while list
      (setq ringptr ring interval (/ length (aref (car list) 1)))
      (while ringptr
	(and (zerop ringpos)
	     (setcar ringptr (cons (aref (car list) 0) (car ringptr))))
	(if (= (setq ringpos (1+ ringpos)) interval) (setq ringpos 0))
	(setq ringptr (cdr ringptr)))
      (setq list (cdr list))) ring))

(defun empi-update-timer-handler ()
  (with-critical-lock "empi-update-timer-handler-lock"
    ;; If there's a command already running, don't poke your nose in...
    ;; This is a bit paranoid, but nobody guarantees a timer event anyway :)
    (unless empi-forwarder-chain
      (with-empi-cache
	(mapc
	 '(lambda (item)
	    (condition-case err
		(funcall item)
	      (error
	       (message "empi-update: error in timer function%s: %s"
			(if (symbolp item) (concat " " (symbol-name item)) "")
			(error-message-string err))
;;; Disabling removal of handler on error...Undecided design issue.
;;;	       (setcar empi-update-schedule-pos
;;;		       (delete item (car empi-update-schedule-pos)))
	       )))
	 (car empi-update-schedule-pos)))
      (setq empi-update-schedule-pos
	    (or (cdr empi-update-schedule-pos) empi-update-schedule)))))

(defun empi-update-stop ()
  (when (aref empi-update-timer 0)
    (cancel-timer (aref empi-update-timer 0))
    (aset empi-update-timer 0 nil)))

(defun empi-update-start (&optional rebuild)
  (when empi-update-timer
    (empi-update-stop)
    (and rebuild (setq empi-update-schedule
		       (empi-update-build-schedule empi-update-list)))
    (setq empi-update-schedule-pos empi-update-schedule)
    (aset empi-update-timer 0
	  (run-at-time nil
		       (/ empi-update-reference-interval
			  (float (aref empi-update-timer 2)))
		       'empi-update-timer-handler))))

(defvar empi-update-register-notrigger nil)

(defun empi-update-register (func scale deps)
  (or (and (wholenump scale) (not (zerop scale)) (functionp func))
      (error "Invalid scale value"))
  (if empi-update-timer
      (let ((olcm (aref empi-update-timer 2)))
	(aset empi-update-timer 1 (gcd-2 (aref empi-update-timer 1) scale))
	(aset empi-update-timer 2 (* (/ olcm (gcd-2 olcm scale)) scale)))
    (setq empi-update-timer (vector nil scale scale)))
  (setq empi-update-list (cons (vector func scale deps) empi-update-list))
  (or empi-update-register-notrigger (empi-update-start t))
  (car empi-update-list))

(defun empi-update-ref-p (obj)
  (and (vectorp obj) (= (length obj) 3)))

(defmacro combine-empi-update-registers (&rest body)
  `(let ((empi-update-register-notrigger t) res)
     (unwind-protect
	 (setq res (progn ,@body))
       (empi-update-start t) res)))

(defun gcd-lcm-n (a &rest args)
  (setq a (aref a 1))
  (let ((lcm a) item)
    (while args
      (setq item (aref (car args) 1)
	    a (gcd-2 a item)
	    lcm (* (/ lcm (gcd-2 lcm item)) item))
      (setq args (cdr args)))
    (cons a lcm)))

(defun empi-update-unregister (obj)
  (setq empi-update-list (delq obj empi-update-list))
  (if empi-update-list
      ;; Maybe merge gcd-lcm-n to use specifically here
      (let ((gcd-lcm-n (apply 'gcd-lcm-n empi-update-list)))
	(aset empi-update-timer 1 (car gcd-lcm-n))
	(aset empi-update-timer 2 (cdr gcd-lcm-n))
	(empi-update-start t))
    (empi-update-stop)
    (setq empi-update-timer nil)))

(define-widget 'wholenum 'restricted-sexp
  "Whole number type"
  :tag "Whole number" :value 0 :size 10
  :type-error "This field should contain a whole number"
  :match-alternatives '(wholenump))

(define-widget 'natnum 'restricted-sexp
  "Natural number type"
  :tag "Natural number" :match-alternatives '(natnump) :value 1 :size 10
  :type-error "This field must contain a natural number")

(defun positive-nump (val) (> val 0))

(define-widget 'posnum 'restricted-sexp
  "Positive number type"
  :tag "Positive number" :match-alternatives '(positive-nump) :value 1 :size 10
  :type-error "This field must contain a positive number")

(defcustom empi-update-reference-interval 1
  "Reference interval for timed action in the EMPI system.
Various timed actions in EMPI use a factor of this value as the interval between
successive executions."
  :type 'posnum :set '(lambda (var val)
			(set var val)
			(empi-update-start)) :group 'empi)

;;; Generic interactive functions

;;;###autoload
(defun empi-send-command (cmd)
  (interactive
   (list (intern-soft (completing-read "Enter Command: "
				(apply 'nconc
				       (mapcar 'plist-keys
					       (progn
						 (empi-ensure-current-player)
						 (cdr empi-current-player))))
				nil t nil nil))))
  (display-message-or-buffer
   (trim-string (format "%s" (empi-simple-command cmd)))))

(provide 'empi-core)

;;; EMPI-CORE.EL ends here
