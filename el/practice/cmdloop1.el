;;; cmdloop.el
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.
 
;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;; Written by Richard Mlynarik 8-Jul-92

;; Putting this in lisp slows things down.

(defun recursive-edit ()
  "Invoke the editor command loop recursively.
To get out of the recursive edit, a command can do `(throw 'exit nil)';
that tells this function to return.
Alternately, `(throw 'exit t)' makes this function signal an error."
  (interactive)
  (let ((command-loop-level (1+ command-loop-level)))
    (redraw-modeline)
    (let ((_buf (and (not (eq (current-buffer)
			      (window-buffer (selected-window))))
		     (current-buffer))))
      (unwind-protect
           ;; command_loop
           (if (catch 'exit
                 (let ((standard-output t)
                       (standard-input t))
                   ;; command_loop_2
                   (while t (funcall command-loop t))))
               ;; turn abort-recursive-edit into a quit
               (signal 'quit '()))
        (if _buf (set-buffer _buf))
        (redraw-modeline)))
    nil))

;; We demand lexical scope!
(defun command-loop (_catch_errors)
  "This function is the default value of the variable command-loop."
  (setq prefix-arg nil)
  (setq last-command 't)
  (cond ((not _catch_errors)
         (command-loop-1))
        ((> (recursion-depth) 0)
         (while (condition-case e
                    (command-loop-1)
                  (t (command-error e) t))))
        (t
         (if (not (null top-level))
             ;; On entry to the outer level, run the startup file
             (condition-case e
                 (catch 'top-level
                   (eval top-level))
               (t (command-error e))))

	 ;; If an error occurred during startup and the initial device
	 ;; wasn't created, then die now (the error was already printed out
	 ;; on the terminal device).
	 (if (and (not (noninteractive))
		  (or (not (devicep (selected-device)))
		      (eq 'terminal (device-type (selected-device)))))
	     (kill-emacs -1))

	 ;; End of -batch run causes exit here.
         (if (noninteractive)
             (kill-emacs t))

         (catch 'top-level
           (while (condition-case e
                      (command-loop-1)
                    (t (command-error e) t)))))))

;; Putting this in lisp slows things down a lot; see also comment above.
;(defun command-loop-1 ()
;  (let ((_event (allocate-event))
;      (_old-command-loop command-loop)
;      ;; We deal with quits ourself
;      (_old-inhibit-quit inhibit-quit)
;      (inhibit-quit t))
;
;  ;; ## cancel_echoing();
;
;  ;; This magically makes single character keyboard macros work just
;  ;; like the real thing.  This is slightly bogus, but it's in here for
;  ;; compatibility with Emacs 18.
;  ;; It's not even clear what the "right thing" is.
;  (and executing-macro
;       (eq (length executing-macro) 1)
;       (setq last-command 't))
;
;  ;; Keep looping until somebody wants a different command-loop
;  (while (eq command-loop _old-command-loop)
;
;    ;; Make sure current window's buffer is selected.
;    (set-buffer (window-buffer (selected-window)))
;
;    ;; C code had a `QUIT' here so that if ^G was typed before we got here
;    ;; (that is, before emacs was idle and waiting for input) then we treat
;    ;; that as an interrupt.  The easiest way to do that here is to make a
;    ;; function call (but pick one the compiler won't optimize away...)
;    (let ((inhibit-quit _old-inhibit-quit)) (eval nil))
;
;    ;; This condition-case was originally just wrapped around the
;    ;;  call to dispatch-event, but in fact we can have errors signalled
;    ;;  by process-filters in either sit-for and next-event.  Those errors
;    ;;  shouldn't be fatal to the command-loop, so we put the condition-case
;    ;;  here and hope we're not hiding other bugs in the process.
;    (condition-case e
;        (progn
;          (if (and (> (minibuffer-depth) 0)
;                   (message-displayed-p))
;              (progn
;                (sit-for 2)
;                (message nil)))
;
;          (next-event _event)
;          ;; If ^G was typed while emacs was reading input from the user, 
;          ;; then it is treated as just another key.  This is what v18
;          ;; did.  This is bogus because it gives the illusion that one
;          ;; can bind commands to sequences involving ^G, when really one
;          ;; can only execute those sequences in non-typeahead contexts.
;          (setq quit-flag nil)
;
;          (let ((inhibit-quit _old-inhibit-quit))
;            (dispatch-event _event))
;
;          ;; check for bogus code trying to use the old method of unreading.
;          (if (globally-boundp 'unread-command-char)
;              (progn
;                (makunbound 'unread-command-char)
;                (error
;                 "%S set unread-command-char instead of unread-command-event."
;		   this-command)))
;	    )
;        (t
;         (command-error e))))))

(setq-default command-loop 'command-loop)
