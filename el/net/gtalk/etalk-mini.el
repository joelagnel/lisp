;;; etalk-mini --- minibuffer support with completion on user names
;;
;; Author: Eric Ludlam (zappo@gnu.ai.mit.edu)
;; Version: 0.8
;; Keywords: extensions
;;
;; Copyright (C) 1995 Free Software Foundation
;;
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
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;;   With support from sformat, these routines supply parsing, and
;;   minibuffer commands for selecting users based on a finger
;;   listing.  This code could prove useful in other types of
;;   programs, and should be considered for such tasks.  Please let
;;   me know of any such intentions so that I might help.
;;

;;; $Id: etalk-mini.el,v 1.9 1997/10/08 18:23:20 zappo Exp $
;;
;; History:
;;
;; eml 7/14/94
;; changed etalk-untabulate-buffer and etalk-get-hosts to use
;; re-search-forward for speed.
;;
;; eml 7/17/94
;; etalk-format now correctly handles the short/long tty case
;; reguardless of type of input, and also the console case.
;;
;; eml 9/14/94
;; Added "Trying" regular expression to finger parser for the IRIX
;; version which prints that instead of "[name]"
;;
;; eml 10/23/94
;; When parsing GNU finger output with linux, tty1 showed up as
;; tty1*.  Completion engine now strips the * from the end.
(require 'sformat)

;;; Code:

(defvar etalk-complete-map nil
  "Map used in my completion thing.")

(if etalk-complete-map
    ()
  (setq etalk-complete-map (make-sparse-keymap))
  (define-key etalk-complete-map " " 'etalk-complete)
  (define-key etalk-complete-map "\C-g" 'abort-recursive-edit)
  (define-key etalk-complete-map "\C-h" 'etalk-minibuffer-help)
  (define-key etalk-complete-map "\C-i" 'etalk-complete-maximize)
  (define-key etalk-complete-map "\e\C-i" 'etalk-who-is-that)
  (define-key etalk-complete-map "\C-j" 'etalk-exit-minibuffer)
  (define-key etalk-complete-map "\C-m" 'etalk-exit-minibuffer)
  ;; force rescan and redo completion
  (define-key etalk-complete-map "\C-r" 'etalk-force-read-complete)
)

(defvar etalk-last-scanned-machine nil
  "Last machine scanned.  Used to speed up completion.")

(defvar etalk-completion-list nil
  "List of all users currently online generated here.")

(defcustom etalk-finger-backup-command "finger %s"
  "*Command used to do a finger if the regular finger command doesn't work."
  :group 'etalk
  :type 'string)

(defcustom etalk-finger-command "finger -i %s"
  "*Command used to do a finger.  Some machines are different."
  :group 'etalk
  :type 'string)

(defconst etalk-finger-regxp-list
  '(;; normal dec header.  This matches Linux and some others as well,
    ;; so labelling it as DEC is a misnomer, but I don't feel like
    ;; dealing with a global search and replace right now.
    ("\\(Login\\)[^~]+\\(TTY\\|Tty\\|tty\\)[^~]+\\( Idle \\)[^~]+" etalk-finger-parse-dec)

    ;; output from finger -i has no stuff at end of line
    ("\\(Login\\)[^~]+\\(TTY\\)[^~]+\\(Idle\\)" etalk-finger-parse-dec2)

    ;; output from gnu's finger deamon. Space for Idle because col starts early
    ;; soulborn (world.std.com) finger format is different, but same order
    ;;   as GNU finger. The difference is the - before and after User.
    ("\\(-?User-?\\)[^~]+\\( Idle\\)[^~]+\\(TTY\\)" etalk-finger-parse-GNU)

    ;; Linux says horrible things from net connections!  Ew!
    ;; The things in the finger info confilict slightly with VMS id, so
    ;; we need to put this before the VMS entry.
    ("^[ \t\r]+$" etalk-finger-zap-line-retry)
    ("^\C-j" etalk-finger-zap-line-retry)
    ("load average" etalk-finger-zap-line-retry)
    ("Welcome\\|UNIX\\|Unix\\|unix" etalk-finger-zap-line-retry)

    ;; vms thing.  First line usually has "Up:" or "Uptime" in it.
    ;; This is the closest I could come to a VMS standard circa 1992
    ("[Uu]p" etalk-finger-parse-VMS)

    ;; finger usually has : "[place.over.there]" first.  Kill it.
    ("\\[[^~]+\\]" etalk-finger-zap-line-retry)
    ;; some fingers say "trying 123..." before a connect. Hmph!
    ("Trying" etalk-finger-zap-line-retry)

    ;; if the .cshrc file has an stty request doing nasty things, ignore it.
    ("[Ss]tty:" etalk-finger-zap-line-retry)
    
    ;; avoid recursive retrys for this error message...
    ("error in phone number" etalk-finger-zap-line-retry)

    ;; in case a file name is 1st.  sun w/ quota problem
    ;; ("\\" etalk-finger-zap-line-retry)

    ;; host was not available
    ;; these cases must come first or the "finger:" part will put us in an
    ;; infinite loop.
    ("unknown host:" "Host %m is unknown.")
    ("[Rr]efused" "Host %m refused connection.")
    ;; No one logged on message
    ("\\(No one logged on\\)" "No one is logged onto %m.")

    ;; Oops.  Try the backup finger command. This must come after
    ;; unknown host for systems which reply "finger: unknown host:"
    ("^[Uu]sage:" etalk-finger-bad-fingercommand)
    ("^[Ff]inger:" etalk-finger-bad-fingercommand)

    ;; They didn't say anything at all.
    ("Connection refused" "Can't open connection to %m.")
    ("connect:" "Connection error.")

    ;; if all else fails, no response
    ("\n" "Host %m did not bless us with a response")

    ;; Last ditch effort!
    ("" "No available responses to finger output.")
    )
  "List of regxp strings followed by function to run to get that info.")

(defcustom etalk-idle-pester-limit 10
  "*Upper idle timelimit before etalk asks if you really want to talk to them.
Time is in minutes."
  :group 'etalk
  :type 'integer)

(defvar etalk-host-completion-list '()
  "List of all hosts read from /etc/hosts.")

(defvar etalk-host-file-size-limit 10000
  "*Maximum size of file /etc/hosts.talk before asking to read from it.
Setting this to NIL never read it.")

(defcustom etalk-morehosts-file "~/.hosts.talk"
  "*File containing more remote nodes for talk completion."
  :group 'etalk
  :type 'file)

(defun etalk-read-username (&optional initial)
  "Read a username from the minibuffer with completions gotten from finger.
Optional argument is the INITIAL value given."

  (setq etalk-last-scanned-machine nil)

  (save-window-excursion
    (unwind-protect
	(let ((minibuffer-scroll-window nil))
	  (read-from-minibuffer "Talk to whom? "
				initial
				etalk-complete-map nil))
      (etalk-kill-minibuffer-buffers))))

(defun etalk-exit-minibuffer ()
  "Check a couple things before exiting the minibuffer."
  (interactive)
  (let* ((contents (save-excursion
		     (set-buffer (window-buffer (minibuffer-window)))
		     (buffer-string)))
	 (namelst (etalk-parse-address contents))
	 (name (nth 0 namelst))
	 (remote (nth 1 namelst))
	 (tty (nth 2 namelst))
	 (nametype (nth 3 namelst))
	 (buffname (concat name " " tty)))
  (if (assoc buffname etalk-completion-list)
      (if (< etalk-idle-pester-limit (etalk-userstring-idle buffname))
	  (if (y-or-n-p (format
			 "%s has been idle for %d minutes. Really call? "
			 name (etalk-userstring-idle buffname)))
	      (exit-minibuffer)
	    (etalk-temporary-minibuffer-message " [Abandoned]"))
	(exit-minibuffer))
    (exit-minibuffer))))

(defun etalk-minibuffer-help ()
  "Display some help into a buffer somewhere..."

  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*Talk to whom help*"))
    (delete-region (point-min) (point-max))
    (goto-char 1)
    (save-excursion
      (insert "Talk to whom help:

    SPC - Complete address portion (either remote host or username and tty)
    TAB - Complete as best. Intelligent tty choice based on idle time.
    ESC TAB - Get even more information about a single user, reguardless of
          wether he is online or not.
    RET - Accept input.
    C-r - Rescan and print differences.
    C-g - Abort edit.
    C-h - This help."))
    (display-buffer "*Talk to whom help*")))

(defun etalk-force-read-complete ()
  "Do a forced finger scan and do completion."

  (interactive)
  (let* ((msgstring "")
	 (old-list etalk-completion-list)
	 (new-list (progn
		     (setq etalk-last-scanned-machine nil)
		     (etalk-complete)
		     etalk-completion-list)))
    (while (and old-list new-list)
      (if (not (string-equal (car (car old-list)) (car (car new-list))))
	  (if (string< (car (car old-list)) (car (car new-list)))
	      (progn
		(setq msgstring (concat msgstring
					(format " -%s" (car (car old-list)))))
		(setq old-list (cdr old-list)))
	    (setq msgstring (concat msgstring
				    (format " +%s" (car (car new-list)))))
	    (setq new-list (cdr new-list)))
	(setq old-list (cdr old-list))
	(setq new-list (cdr new-list))))
    (if new-list
	(while new-list
	  (setq msgstring (concat msgstring
				  (format "+%s" (car (car new-list)))))
	  (setq new-list (cdr new-list)))
      (while old-list
	(setq msgstring (concat msgstring (format "+%s" (car (car old-list)))))
	(setq old-list (cdr old-list))))
    (if (equal (length msgstring) 0)
	(etalk-temporary-minibuffer-message " [No changes]")
      (message msgstring))))

;;; This function was moved to allow this file to be stand-alone when
;; attempting to use the completion commands
(defun etalk-parse-address (in-string)
  "Take the encoded address IN-STRING and return a list of it's parts.
The string is of the form \"name@place tty\" or \"place!name tty\"
and the list is of the form (USERNAME MACHINE TTY ADDRESSTYPE).
ADDRESSTYPE is 1 for name@place, and 2 for place!name, and 3 if there
is not machine name present."

  (let ((addrtype 0)
	(somebody-else "")
	(somewhere-else "")
	(sometty-else "")
	(data (match-data)))
    (if (string-match "\\(@\\)[^ ]+\\([ ]+tty\\)" in-string)
	(progn
	  (setq addrtype 1)
	  (setq somebody-else (substring in-string 0 (match-beginning 1)))
	  (setq somewhere-else (substring in-string (match-end 1)
					  (match-beginning 2)))
	  (setq sometty-else (substring in-string (- (match-end 2) 3)
					(length in-string)))
	  (if (string-match "\\([ ]+\\)" sometty-else)
	      (setq sometty-else (substring sometty-else 0
					    (match-beginning 1)))))
      (if (string-match "\\(@\\)" in-string)
	  (progn
	    (setq addrtype 1)
	    (setq somebody-else (substring in-string 0 (match-beginning 1)))
	    (setq somewhere-else (substring in-string (match-end 1)
					    (length in-string)))
	    (if (string-match "\\( \\)" somewhere-else)
		(setq somewhere-else (substring somewhere-else 0
						(match-beginning 1))))
	    (setq sometty-else nil))
	(if (string-match "\\(!\\)[^ ]+\\([ ]+\\)" in-string)
	    (progn
	      (setq addrtype 2)
	      (setq somewhere-else (substring in-string 0 (match-beginning 1)))
	      (setq somebody-else (substring in-string (match-end 1)
					     (match-beginning 2)))
	      (setq sometty-else (substring in-string (match-end 2)
					    (length in-string))))
	  (if (string-match "\\(!\\)" in-string)
	      (progn
		(setq addrtype 2)
		(setq somewhere-else (substring in-string 0
						(match-beginning 1)))
		(setq somebody-else (substring in-string (match-end 1)
					       (match-beginning 2)))
		(setq sometty-else nil))
	    (if (string-match "\\([ ]+\\)" in-string)
		(progn
		  (setq addrtype 0)
		  (setq somebody-else (substring in-string 0
						 (match-beginning 1)))
		  (setq sometty-else (substring in-string (match-end 1)
						(length in-string)))
		  (setq somewhere-else (system-name)))
	      (setq addrtype 0)
	      (setq somebody-else in-string)
	      (setq somewhere-else (system-name)) ; no computer
					; specified, assume this one.
					  (setq sometty-else nil))))))
    (store-match-data data)
    (list somebody-else somewhere-else sometty-else addrtype)))

(defun etalk-format (formatstring &optional name machine tty)
  "Take FORMATSTRING and replace % codes with address parts.
Replaces % codes with the optional parameters, or locally
defined variables etalk-remote-who, etalk-remote-where, and
etalk-remote-tty.  The optional paramters are  NAME, MACHINE, and TTY.

Valid  %'s are:

%u = username *
%m = machine name *
%M = machine name up to first `.`
%t = tty with preceeding `tty` |
%T = tty without the `tty` string |
%p = remote's preferred name*
%P = My preferred name*
* -> uses the ':' numeric argument
| -> word argument adds conditional space before

Integer arguments:
%#u = # letters in username
%#m = # letters in machine name
%:#m = # words in machine name
%#:$m = # letters , but only $ words.  Pad otherwise.

Note: A '.' can be used in place of ':'"
  (let* ((name (if name name
		 (if (boundp 'etalk-remote-who) etalk-remote-who nil)))
	 (machine (if machine machine
		    (if (boundp 'etalk-remote-where) etalk-remote-where nil)))
	 (tty (if (and tty (> (length tty) 0))tty
		(if (boundp 'etalk-remote-tty) etalk-remote-tty nil)))
	 (stty (if tty
		   (if (or (string= "co" tty) (string-match "console" tty))
		       "co"
		     (if (string-match "\\(tty\\)" tty)
			 (substring tty (match-end 1))
		       tty))
		 ""))
	 (ltty (if tty
		   (if (string= "co" stty)
		       "console"
		     (concat "tty" stty))
		 "")))
    (Sformat '( (?u name)
		(?m machine)
		(?M machine nil 1)
		(?t ltty nil 'preceeding-space)
		(?T stty nil 'preceeding-space)
		(?p etalk-remote-preferred-name)
		(?P etalk-preferred-name)
		)
	     formatstring))
  )

(defun etalk-kill-minibuffer-buffers ()
  "Kill all extraneous buffers make by talk prompt."
  (if (get-buffer "SOME PEOPLE")
      (kill-buffer "SOME PEOPLE"))
  (if (get-buffer "*Talk to whom help*")
      (kill-buffer "*Talk to whom help*"))
  (if (get-buffer "SOME NODES")
      (kill-buffer "SOME NODES"))
  (if (get-buffer "*etc-hosts*")
      (kill-buffer "*etc-hosts*"))
  (if (get-buffer "Your Rights with ETALK")
      (kill-buffer "Your Rights with ETALK")))

(defun etalk-complete-maximize ()
  "Run talk complete with the maximize flag set."
  (interactive)
  (etalk-complete t))

(defun etalk-complete (&optional maximize)
  "Do a completion on current contents of the minibuffer.
Check to see if you need to rescan remote machine.  Optional argument
MAXIMIZE is non-nil if the address is to be expanded to the maximum
size possible.  Supports the following network name addresses where
tty is optional:
  name tty
  name@host tty
  host!name tty"
  
  (interactive)
  (let* ((contents (save-excursion
		     (set-buffer (window-buffer (minibuffer-window)))
		     (buffer-string)))
	 (namelst (etalk-parse-address contents))
	 (name (nth 0 namelst))
	 (remote (nth 1 namelst))
	 (tty (nth 2 namelst))
	 (cname (if tty
		    (concat name " " tty)
		  (if (string-match " " contents)
		      (concat name " ")
		    name)))
	 (nametype (nth 3 namelst))
	 (onamelen (length name))
	 (oldname name)
	 (contcomp t)
	 (possible nil)
	 (havespace maximize)
	 (addlist nil)
	 )
    ;; Here, check to see it the tty exists, if not, if there is a
    ;; space.  This if for completion stopping on the space.
    (if tty
	(progn
	  (setq havespace t)
	  (setq name (format "%s %s" name tty)))
      (if (string-match " " contents)
	  (setq havespace t)))

    ;; redo _only_ if change since it's slow
    ;; also check for completion on the host
    (if (not (equal etalk-last-scanned-machine remote))
	(progn
	  (if (not (equal nametype 0))
	      (progn
		(etalk-get-hosts)
		(setq possible (try-completion
				remote etalk-host-completion-list))
		(if (equal remote possible)
		    (progn
		      (setq possible (all-completions
				      remote etalk-host-completion-list))
		      (etalk-display-host-completions possible)
		      (etalk-temporary-minibuffer-message
		       (format " [%d possible hosts]" (length possible)))
		      (setq contcomp nil))
		  (if (and (stringp possible)
			   (> (length possible) (length remote)))
		      (progn
			(set-buffer (window-buffer (minibuffer-window)))
			(delete-region (point-min) (point-max))
			(setq addlist (etalk-build-new-address
				       nametype oldname
				       (if (stringp possible)
					   possible
					 remote)
				       tty))
			(insert (car addlist))
			(goto-char (point-max))
			(if maximize
			    (etalk-complete t)
			  (etalk-temporary-minibuffer-message " [Check]"))
			(setq contcomp nil))))))
	  (if contcomp
	      (progn
		(message "Making completion list...")
		(etalk-get-users remote)
		(setq etalk-last-scanned-machine remote)))))
    ;; WOO WOO!  We made it this far past machine names.. now for people!!!
    (if contcomp
	(progn
	  (setq possible (try-completion cname etalk-completion-list))
	  (if (string= possible cname)
	      (progn
		(if (and maximize (string-match " " possible))
		    ;; in this instance, we look for least idletime.
		    (progn
		      (setq possible (etalk-userstring-leastidle cname))
		      (if (string= cname possible)
			  ()
			(set-buffer (window-buffer (minibuffer-window)))
			(delete-region (point-min) (point-max))
			(if (string-match "\\([ ]+\\)" possible)
			    (progn
			      (setq name (substring possible 0
						    (match-beginning 1)))
			      (setq tty (substring possible (match-end 1)
						   (length possible)))))
			(let ((newaddr (etalk-build-new-address
					nametype name remote tty)))
			  (insert (car newaddr))
			  (goto-char (point-max)))))
		  ;; ok, now do the window thingy
		  (setq possible (all-completions cname etalk-completion-list))
		  (if (and (equal onamelen (length name))
			   (not (equal nametype 2)))
		      (progn
			(goto-char 1)
			(forward-char onamelen)))
		  (etalk-display-completions
		   possible "List of users currently logged on")
		  (etalk-temporary-minibuffer-message
		   (format " [%d possible]" (length possible)))))
	    (progn
	      (if (not possible)
		  (etalk-temporary-minibuffer-message " [No Match]" t)
		(if (equal possible t)
		    (etalk-temporary-minibuffer-message
		     (format " [Matched : %d minutes idle]"
			     (etalk-userstring-idle name)))
		  (progn
		    (set-buffer (window-buffer (minibuffer-window)))
		    (delete-region (point-min) (point-max))
		    (if (string-match "\\([ ]+\\)" possible)
			(progn
			  (setq name (substring possible 0
						(match-beginning 1)))
			  (setq tty (substring possible (match-end 1)
					       (length possible))))
		      (progn (setq name possible)
			     (setq tty nil)))
		    (setq addlist
			  (if havespace
			      (etalk-build-new-address nametype name remote
						      tty)
			    (etalk-build-new-address
			     nametype name remote
			     (if (eq onamelen (length name))
				 "" nil))))
		    (insert (car addlist))
		    (goto-char 1)
		    (forward-char (car (cdr addlist))))))))))))

(defun etalk-userstring-leastidle (user)
  "Return a completion for USER with the least amount of idle time."
  (if (string-match " " user)
      (let ((list (assoc user etalk-completion-list)))
	(if list
	    ;; only one occurence.. might as well
	    user
	  (setq list (all-completions user etalk-completion-list))
	  (if (not list)
	      ;; no occurence.. hope they know what they are doing.
	      user
	    (let ((smallidlestr (car list))
		  (smallidle 999999)
		  (tmpidle nil))
	      (while list
		(setq tmpidle (etalk-userstring-idle (car list)))
		(if (< tmpidle smallidle)
		    (progn (setq smallidle tmpidle)
			   (setq smallidlestr (car list))))
		(setq list (cdr list)))
	      smallidlestr))))
    ;; if there is no space in name, then we may be faced with more than one
    ;; username, so return it.
    user))
		      
(defun etalk-userstring-idle (user)
  "Look up USER in user list, and snarf idle time from it.  Return nil if name not found, or whatever is in the idle position of the alist."
  (let ((list (assoc user etalk-completion-list)))
    (if list
	(if (cdr list) (cdr list) 0)
      0)))
	
(defun etalk-temporary-minibuffer-message (m &optional error1)
  ;; The guts of this procedure are given thanks to Joe Wells!!!!
  ;; He even changed this a little from the 1989 version for me.
  ;; Thanks...
  "Prints string MESSAGE in the current buffer to the right of all text
in the buffer.  It is used mainly for putting messages in the minibuffer
while also showing the minibuffer text."
  (let ((osize 0)
	(opnt 0)
	(inhibit-quit t)
	(enable-recursive-minibuffers t))
    (save-excursion
      (set-buffer (window-buffer (minibuffer-window)))
      (setq osize (point-max))
      (setq opnt (point))
      (goto-char osize)
      (insert m)
      (goto-char opnt)
      ;; The next statement is a gross hack.
      ;; The purpose is to set minibuf_message = 0, so that the contents
      ;; of the minibuffer will show.
      (let ((unread-command-char ?\C-m))
	(read-from-minibuffer "" nil nil nil))
      ;; This sets prev_echo_area_contents = echo_area_contents (which is 0)
      ;; *** fix this so it doesn't assume RET exits minibuffer
      (let ((unread-command-char help-char)
	    (help-form '(setq unread-command-char ?\C-m)))
	(read-key-sequence nil))
      (if error1 (ding))
      (sit-for 2)
      (delete-region osize (point-max))
      (if quit-flag
          (setq quit-flag nil
                unread-command-char ?\C-g)))))

;; Check if temp-minibuf-message has been fixed in the C code.
;;(or (and (fboundp 'temp-minibuf-message)
;;         (subrp (symbol-function 'temp-minibuf-message)))
;;    (fset 'temp-minibuf-message
;;          (symbol-function 'etalk-temporary-minibuffer-message)))

;; End of kool procedural stuff from Joe wells.

(defun etalk-build-new-address (nametype name remote tty)
  "Build an address based on NAMETYPE, NAME, REMOTE, and TTY.
The return argument is a list of type (\"name\" character-pointer)
where character pointer is the most logical place to put the cursor next."
  
  (let ((cp 0))
    (cons
     (cond
      ((equal nametype 0) (if (not tty) (progn (setq cp (length name))
					       (format "%s" name))
			    (progn
			      (setq cp (+ (length name) (length tty) 1))
			      (format "%s %s" name tty))))
      ((equal nametype 1) (if (not tty) (progn (setq cp (length name))
					       (format "%s@%s" name remote))
			    (progn
			      (setq cp (+ (length name) (length remote)
					  (length tty) 2))
			      (format "%s@%s %s" name remote tty))))
      ((equal nametype 2) (if (not tty) (progn (setq cp (+ (length remote)
							   (length name) 1))
					       (format "%s!%s" remote name))
			    (progn
			      (setq cp (+ (length remote) (length name)
					  (length tty) 2))
			      (format "%s!%s %s" remote name tty)))))
     (list cp))))

(defun etalk-who-is-that ()
  "Generate a buffer full of lots of information about a remote user."
  
  (interactive)
  (let* ((contents
	  (save-excursion
	    (set-buffer (window-buffer (minibuffer-window)))
	    (buffer-string)))
	 (addrlst (etalk-parse-address contents))
	 (name (nth 0 addrlst))
	 (remote (nth 1 addrlst))
	 (tmp nil))

    (if (not (string= remote (system-name)))
	(message "Checking %s for more info." remote))
    
    (save-excursion
      (get-buffer-create "SOME PEOPLE")
      
      (set-buffer (get-buffer "SOME PEOPLE"))
      (delete-region (point-min) (point-max))
      
      (goto-char 1)
      (move-marker (setq tmp (make-marker)) (point))
      (if (string= remote (system-name))
	  (insert "w " name "; echo \"---------\" ;"))
      
      (insert (format "finger -m %s@%s" name remote))
      (call-process-region tmp (point) "sh" t "SOME PEOPLE" nil)
      
      (display-buffer "SOME PEOPLE")
      (goto-char 1))
    (setq minibuffer-scroll-window (get-buffer-window "SOME PEOPLE"))))

(defun etalk-display-completions (list title)
  "Display all possible user completions in a temporary buffer.
The completions are in LIST, and TITLE is the completion message."

  (get-buffer-create "SOME PEOPLE")
  (set-buffer (get-buffer "SOME PEOPLE"))
  (display-buffer "SOME PEOPLE")
  (delete-region (point-min) (point-max))
  (goto-char 1)
  (insert title ".\n\n")
  (while list
    (insert (format "   %-20s      %-20s      %-20s\n"
		    (car list)
		    (if (car (cdr list)) (car (cdr list)) "")
		    (if (car (cdr (cdr list))) (car (cdr (cdr list))) "")))
    (setq list (cdr (cdr (cdr list)))))
  (setq minibuffer-scroll-window (get-buffer-window "SOME PEOPLE"))
)

(defun etalk-get-users (host)
  "Get a list of all users on HOST.
Only thier usernames and for completion in minibuffer. Use empty
string for host 0."

  (interactive "sRemote System:")
  (setq etalk-completion-list '())

  (unwind-protect
      (save-window-excursion
	(if (not (equal (length host) 0))
	    (message "Checking %s for more info..." host))
	
	(get-buffer-create "talk-complete-buffer")
	(get-buffer-create "talk-complete-output")
	
	(set-buffer (get-buffer "talk-complete-output"))
	(delete-region (point-min) (point-max))
	
	(set-buffer (get-buffer "talk-complete-buffer"))
	(delete-region (point-min) (point-max))
	
	(goto-char 1)
	(let ((tmp nil))
	  (move-marker (setq tmp (make-marker)) (point))
	  
	  (insert (format etalk-finger-command (if (equal (length host) 0)
						   "" (concat "@" host) )))
	  
	  (call-process-region tmp (point) "sh" t "talk-complete-output" nil)
	  (kill-buffer "talk-complete-buffer")
	  
	  (set-buffer "talk-complete-output")
	  
	  ;; some systems put tabs in to line stuff up.  BAD BAD!
	  (etalk-untabulate-buffer)
	  
	  (let* ((numlist (etalk-parse-finger-header host)))
	    (if numlist
		;; if USAGE bad, a second call to
		;; etalk-get-users will result in a NIL
		;; returned here!
		(let ( (namestart (car numlist))
		       (ttystart (nth 1 numlist))
		       (ttynone (nth 2 numlist))
		       (idlestart (nth 3 numlist))
		       (idlefunc (nth 4 numlist))
		       (count 1)
		       (ts 0)
		       (tname "")
		       (ttty ""))
		  (while (not (eobp))
		    (move-marker tmp (point))
		    (forward-line)
		    (setq ts (buffer-substring tmp (point)))
		    (setq tname (substring ts namestart (+ namestart 9)))
		    (if (string-match "\\( \\)" tname)
			(setq tname (substring tname 0 (match-beginning 1))))
		    (if ttystart			;check for VMS no ttyness
			(if (not ttynone)
			    (progn
			      (setq ttty
				    (substring ts ttystart
					       (+ ttystart 7)))
			      (if (string-match "\\(\\*\\| \\)" ttty)
				  (setq ttty
					(concat
					 " " (substring
					      ttty 0 (match-beginning 1))))
				(setq ttty (concat " " ttty))))
			  (setq ttty
				(substring ts ttystart (+ ttystart 2)))
			  (if (string= ttty "co")
			      (setq ttty " console")
			    (if (string-match "\\( \\)" ttty)
				(if (= (match-beginning 1) 0)
				    (setq ttty (concat "0" (substring ttty 1 2)))
				  (setq ttty (concat "0" (substring ttty 0 1))))
			      (if (string-match "\\(\\*\\)" ttty)
				  (setq ttty (substring ttty 0 (match-beginning 1)))))
			    (setq ttty (concat " tty" ttty))))
		      (setq ttty ""))
		    (if (not (string= tname ""))
			(setq etalk-completion-list
			      (cons (cons (downcase (concat tname ttty))
					  (if idlefunc
					      (funcall idlefunc ts idlestart)
					    0))
				    etalk-completion-list)))))))
	  
	  ;; now sort the list with sort.  Need special lambda function to
	  ;; compare the cars of the elements due to association list.
	  (setq etalk-completion-list
		(sort etalk-completion-list
		      '(lambda (A1 A2) (string< (car A1) (car A2)))))
	  nil))
    (if (get-buffer "talk-complete-output")
	(kill-buffer "talk-complete-output"))
    (if (get-buffer "talk-complete-buffer")
	(kill-buffer "talk-complete-buffer"))))
  
(defun etalk-untabulate-buffer ()
  "Detabulate the current buffer."
  (if (fboundp 'untabify)
      (untabify (point-min) (point-max))
    (goto-char 0)
    (message "Detabulating output for parsing...")
    (while (re-search-forward "\\(\t\\)" nil t)
      ;; (goto-char (match-end 1)) ; automatically there already
      (let* ((numspc (- (point) (save-excursion (beginning-of-line) (point)) 1))
	     (num (if (= (% numspc 8) 0) 8 (- 8 (% numspc 8))))
	     (spcs (substring "         " 0 num)))
	(delete-char -1)
	(insert spcs)))
    (message "Detabulating output for parsing...done")))

(defun etalk-parse-finger-header (host)
  "Return a parse list for the finger output from HOST.
The return value is a list of ints.  In order they are:
namestartcol ttystartcol ttyincluded &optional idlestart idleparse"

  (goto-char 1)
  (let ((tmp-str (buffer-substring (point) (save-excursion (forward-line 1)
							   (point))))
	(reglist etalk-finger-regxp-list)
	(object nil))
    (while (and (not (string-match (if (car (car reglist))
				       (car (car reglist))"")
				   tmp-str))
		reglist)
      (setq reglist (cdr reglist)))

    (setq object (car (cdr (car reglist))))
    (if reglist
	(if (stringp object)
	    (error (etalk-format object "" host ""))
	  (if (fboundp object)
	      (funcall object host)
	    ;; this also has a return value for this function
	    (error "Found REGEXP, but invalid association.")))
      (error "Header has no match \"%s\"" tmp-str))
    ))

(defun etalk-finger-bad-fingercommand (host)
  "Change the version of finger we use while accessing HOST.
If the string \"Usage:\" is seen, then we have bad parameters in
the finger command variable.  Change it back and try again."

  (message "The command \"%s\" is invalid.  Using \"%s\" instead."
	   etalk-finger-command etalk-finger-backup-command)
  (setq etalk-finger-command etalk-finger-backup-command)
  (etalk-get-users host)
)

(defun etalk-finger-zap-line-retry (host)
  "Skip one line in finger output from HOST.
Used in the main list to skip over various kinds of notices."

  (goto-char 1)
  (delete-region (point) (save-excursion (forward-line 1) (point)))
  (etalk-parse-finger-header host))  ; this host is from prev call to
                       ;; etalk-parse-finger-header which called this.

(defun etalk-finger-parse-dec (host)
  "Parse the header of HOST if it is Ultrix or SunOS finger list."

  (let ((namestart (match-beginning 1))
	(ttystart (match-beginning 2))
	(idlestart (match-beginning 3)))

    (etalk-finger-parse-line2 namestart ttystart idlestart 'etalk-idleparse1)))


(defun etalk-finger-parse-dec2 (host)
  "Parse the header of HOST, running Ultrix when idle is last."

  (let ((namestart (match-beginning 1))
	(ttystart (match-beginning 2))
	(idlestart (- (match-beginning 3) 4)))
    (etalk-finger-parse-line2 namestart ttystart idlestart 'etalk-idleparse2)))

(defun etalk-finger-parse-GNU (host)
  "Parse the header of a finger list if HOST is running GNU finger."

  (let ((ttystart (match-beginning 3))
	(idlestart (match-beginning 2))
	(namestart (if (< (match-beginning 1) 5)
		       0
		     (match-beginning 1))))
    (etalk-finger-parse-line2 namestart ttystart idlestart
			      'etalk-idleparseGNU)))

(defun etalk-finger-parse-line2 (name-start tty-start idlestart func)
  "Read the second line of finger header to make sure that the TTY col match.
Uses pre-identified positions NAME-START TTY-START IDLESTART.
FUNC is returned as part of the list."
  (goto-char 1)
  (forward-line 1)
  (let* ((ts (buffer-substring (point) (save-excursion (forward-line 1)
						       (point))))
	 (ttty (if tty-start
		   (substring ts tty-start (+ tty-start 7))
		 nil))
	 (ttynone nil))
    (if tty-start
	(if (string-match "\\(tty\\)" ttty)
	    (progn
	      (setq ttynone nil)
	      (setq tty-start (+ tty-start (match-beginning 1)))
	      (setq ttty (substring ts tty-start (+ tty-start 5))))
	  ;; ok, now we check to see if the operator is on the console.
	  ;; Since finger _always_ sorts by tty, console is
	  ;; alphabetically before tty. (in /dev directory, look and
	  ;; see) so if it shows up, it is here and we just calculate
	  ;; the offsets from it and type of tty listing.  We don't worry
	  ;; about it in the future list build.
	  (if (string-match "\\(console\\)" ttty)
	      (progn
		(setq ttynone nil)	;long form with tty
		(setq tty-start (+ tty-start (match-beginning 1)))
		(setq ttty (substring ts tty-start (+ tty-start 7))))
	    (if (string-match "\\(co\\)" ttty)
		(progn
		  (setq ttynone t)
		  (setq tty-start (+ tty-start (match-beginning 1)))
		  (setq ttty "console"))
	      (setq ttynone t)
	      (if (string-match "\\([a-zA-Z0-9]\\)" ttty)
		  (progn
		    (setq tty-start (+ tty-start (match-beginning 1)))
		    (if (equal (- (match-end 1) (match-beginning 1)) 1)
			(setq ttty (concat "tty"
					   (substring ts (- tty-start 1)
						      (+ tty-start 1))))
		      (setq ttty (concat "tty"
					 (substring ts tty-start
						    (+ tty-start 2)))))
		    ;; gnu finger abandons 0's  if first is 0 then make sure
		    ;; we don't forget everyone else has that needed.
		    (if (equal (length ttty) 4)
			(setq tty-start (- tty-start 1))))
		(if tty-start
		    (error "Found REGEXP, TTY column, but lost the TTY.")))))))
    (list name-start tty-start ttynone idlestart func)))

(defun etalk-finger-parse-VMS (host)
  "Parse output from HOST since it *might* be a VMS finger header."
  
  (let ((namestart nil)
	(tmp-str nil))
    (while (and (not (eobp)) (not namestart))
      ;; Cook lines until we find something we want!
      (end-of-line)
      (forward-char)
      (setq tmp-str (buffer-substring (point-min) (point)))
      (delete-region (point-min) (point))
      (if (string-match "\\(User\\)" tmp-str)
	  (setq namestart 0)))
    (if (not namestart)
	(error "Funky header.  Thought %s might be VMS but got confused."
	       host))
    (forward-line 1)
    (if (string-match (buffer-substring (point) (save-excursion
						  (end-of-line) (point)))
		      "---")
	(forward-line 1))
    (list namestart nil nil nil nil)))

(defun etalk-idleparse1 (line start)
  "Parse an Ultrix idle time and return it as a number.
LINE is the text to parse, and START is where to start searching."
  (let ((idlestr (substring line start (+ start 5)))
	(idlefor 0))
    (cond
     ((string-match "\\(d\\)" idlestr)
      (setq idlefor (* (string-to-int (substring idlestr 0
						 (match-beginning 1)))
		       1440)))
     ((string-match "\\([0-9]+\\):\\([0-9]+\\)" idlestr)
      (setq idlefor (+ (* (string-to-int (substring idlestr
						    (match-beginning 1)
						    (match-end 1)))
			  60)
		       (string-to-int (substring idlestr
						 (match-beginning 2)
						 (match-end 2))))))
     ((string-match "\\([0-9]+\\):" idlestr)
      (setq idlefor (* (string-to-int (substring idlestr
						 (match-beginning 1)
						 (match-end 1)))
		       60)))
     (t
      (setq idlefor (string-to-int idlestr))))
    idlefor))

(defun etalk-idleparse2 (line start)
  "Parse the idle LINE for finger -i output starting at START."

  (if (< (length line) start)
      nil
    (let ((idlefor 0))
      (if (string-match "\\([0-9]+\\) \\(days\\)" line)
	  (setq idlefor (* (string-to-int
			    (substring line (match-beginning 1) (match-end 1)))
			   1440)))
      (if (string-match "\\([0-9]+\\) \\(hours\\)" line)
	  (setq idlefor (+ idlefor (* 24  (string-to-int
					   (substring line (match-beginning 1)
						      (length line)))))))
      (if (string-match "\\([0-9]+\\) \\(minutes\\)" line)
	  (setq idlefor (+ idlefor (string-to-int
				    (substring line (match-beginning 1)
					       (length line))))))
      idlefor)))

(defun etalk-idleparseGNU (line start)
  "Parse a GNU finger idle time from LINE at START and return it as a number."
  (if (< (length line) start)
      nil
    (let ((idlefor 0)
	  (idlestr (substring line start (+ start 6))))
      (cond
       ((string-match "\\(mon\\)" idlestr)
	(setq idlefor 99999))
       ((string-match "\\(da\\)" idlestr)
	(setq idlefor (* (string-to-int idlestr) 1440)))
       ((string-match "\\([0-9]+\\):\\([0-9]+\\)" idlestr)
	(setq idlefor (+ (* (string-to-int
			     (substring idlestr (match-beginning 1)
					(match-end 1)))
			    60)
			 (string-to-int (substring idlestr (match-beginning 2)
						   (match-end 2)))))))
      idlefor)))

(defun etalk-read-host (prompt)
  "Read the name of a host computer with PROMPT using completion."

  (etalk-get-hosts)
  (save-window-excursion
    (completing-read prompt etalk-host-completion-list)))

(defun etalk-get-hosts ()
  "Generate completion list from /etc/talk.hosts and ~/.talk.hosts."

  (interactive)
  (or etalk-host-completion-list
      (unwind-protect
	  (save-excursion
	    (setq etalk-host-completion-list '())
	    (message "Building completion list from /etc/talk.hosts ...")
	    (get-buffer-create "*etc-hosts*")
	    (set-buffer (get-buffer "*etc-hosts*"))
	    (delete-region (point-min) (point-max))
	    (if (not etalk-host-file-size-limit)
		()
	      ;; more-hosts file causes duplicates when setting the
	      ;; type.  We may just go with a system-wide /etc/hosts.talk
	      ;; or tough noogies for the user, do it yourself.
	      (if (file-exists-p "/etc/hosts.talk")
		  (insert-file-contents "/etc/hosts.talk"))
	      (if (> (point-max) etalk-host-file-size-limit)
		  (if (y-or-n-p
		       "Hosts file will take a long time to parse. Parse?")
		      ()
		    (message "Building completion list from %s only ..."
			     etalk-morehosts-file)
		    (delete-region (point-min) (point-max)))))
	    ;; this is so I can insert more into it.
	    (setq buffer-read-only nil)
	    (end-of-buffer)
	    (if (file-exists-p etalk-morehosts-file)
		(insert-file-contents etalk-morehosts-file nil))
	    (goto-char 1)
	    (while (re-search-forward
		    "^[\t ]*\\([.0-9]+\\)[ \t]+\\([.a-zA-Z0-9]+\\)" nil t)
	      (let ((tmpname (buffer-substring (match-beginning 2)
					       (match-end 2))))
		(setq etalk-host-completion-list
		      (cons (cons tmpname '(1))
			    etalk-host-completion-list)))))
	(setq etalk-host-completion-list
	      (sort etalk-host-completion-list
		    '(lambda (A1 A2)
		       (string< (car A1) (car A2)))))
	(if (get-buffer "*etc-hosts*")
	    (kill-buffer "*etc-hosts*"))))
  etalk-host-completion-list)
  
(defun etalk-display-host-completions (list)
  "Display all possible user completions in a temporary buffer.
The completions are generated from LIST."
    (with-output-to-temp-buffer "SOME NODES"
      (display-completion-list list))
    (setq minibuffer-scroll-window (get-buffer-window "SOME NODES"))
    )

(provide 'etalk-mini)
;;; etalk-mini ends here
