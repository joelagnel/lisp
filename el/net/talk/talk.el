;; This file contains code of questionable quality.
;;
;; I accept no responsibility for damaged machines
;; accounts or egos.
;;
;; To the best of my knowlege, what is contianed here seems
;; to work for myself, and for my friends.
;;
;; All testing of this software so far was done with:
;;   berkely unix V  4.2A
;;   emacs        v  18.58.2
;;   gcc          v  2.1
;;
;; boring blurb dated: July 6th

(provide 'talk)

(defvar talk-mode-syntax-table nil 
  "Syntax table used while in Talk mode")

(if talk-mode-syntax-table
    ()         ; don't change if already there
  (setq talk-mode-syntax-table (make-syntax-table))
  (set-syntax-table talk-mode-syntax-table)
  ;; make syntax insertions here.
  (modify-syntax-entry ?\ " " talk-mode-syntax-table)
  )

(defvar talk-mode-abbrev-table nil
  "Abbreviation table")

(define-abbrev-table 'talk-mode-abbrev-table ())

(defvar talk-mode-c-map nil 
  "Keymap used on control c in talk mode")
(if talk-mode-c-map 
    ()
  (setq talk-mode-c-map (make-keymap))
  (fillarray talk-mode-c-map nil)
  (define-key talk-mode-c-map "\C-c" 'talk-nuke-TALK)
  (define-key talk-mode-c-map "\C-h" 'talk-hug-remote)
  (define-key talk-mode-c-map "m"    'talk-send-minibuffer-message)
  )

(defvar talk-mode-map nil 
  "Keymap used in talk mode")
(if talk-mode-map 
    ()
  (setq talk-mode-map (make-keymap))
  (suppress-keymap talk-mode-map)
  (fillarray talk-mode-map 'talk-insert-char)
  (define-key talk-mode-map "\C-@" 'set-mark-command)
  (define-key talk-mode-map "\C-c"  talk-mode-c-map)
  (define-key talk-mode-map "\C-f" 'talk-auto-fill-now)
  (define-key talk-mode-map "\C-k" 'talk-delete-line)
  (define-key talk-mode-map "\C-l" 'talk-clear-window)
  (define-key talk-mode-map "\C-m" 'talk-RET)
  (define-key talk-mode-map "\C-r" 'talk-setup-windows)
  (define-key talk-mode-map "\C-w" 'talk-delete-word-backwards)
  (define-key talk-mode-map "\C-x" 'Control-X-prefix)
  (define-key talk-mode-map "\C-y" 'talk-yank-text)
  (define-key talk-mode-map "\C-z" 'suspend-emacs)
  (define-key talk-mode-map "\e" nil)
  (define-key talk-mode-map "\177" 'talk-delete-backwards)
  )

(put 'talk-mode 'mode-class 'special)

(defvar talk-process-file "/afs/sipb.mit.edu/contrib/emacs/elisp/talk/TALK.@sys"
  "This string declares where the talk program executable
lives on the system.")

(defvar talk-local-buffer-name "*TALK*  {local} "
  "The name given to a local talk buffer.")

(defvar talk-remote-mode-string "TALK-remote"
  "The mode name of a buffer attached to a talk process.")

(defvar talk-local-mode-string "TALK"
  "Mode name of the local talk buffer.")

(defvar talk-remote-process-list '()
  "This list contains the group of active talk buffers
for local talk.  Input to talk window is sent to all remote windows.")

(defvar talk-announce-as (getenv "USER")
  "This string contains a name passed to the remote talk deamon
as your \"username\".  Changing this means you announce yourself with a 
different name.")

(defvar talk-edit-characters-mine ""
  "This string represents the characters used when sending
edit characters to remote system.")

(defvar talk-function-trace nil
  "Setting this to t will print the function names as they
are called.")

(defvar talk-message-to-minibuffer t
  "Dictates wether messages from TALK are displayed in minibuffer,
or in the talk remote buffer.")

(defvar talk-zappo-string ""
  "String used for debugging stuff")

(require 'talk-process)
(require 'talk-edit)
(require 'talk-special)

;; ---------------------------------------------------------------- ;;
;; Ok, now that the preliminaries are over, define the major modes. ;;
;; This is broaken down into 4 functions.                           ;;
;;                                                                  ;;
;; talk              : find/start local talk buffer and remote.     ;;
;; talk-mode-local:  : setup the local talk buffer with marker.     ;;
;; talk-mode-remote: : setup names, marker, process.                ;;
;; talk-mode:        : setup keymaps etc common to both buffers.    ;;
;; ---------------------------------------------------------------- ;;

(defun talk (somebody-else)
  "Function:  This function initialized the talk buffers in your emacs
session.  Parameter is a talk style address like:
   joe@some.big.computer.edu"

  (interactive "sTalk to whom? ")
  ;; check for duplicate calls.
  (message "BETA talk release.  Bugs go to s5451066@titan.ucc.umass.edu")
  (sleep-for 1)
  (if (not (get-buffer talk-local-buffer-name))
      (progn
	(set-buffer (get-buffer-create talk-local-buffer-name))
	(talk-mode-local)))
  (set-buffer (get-buffer-create "talk-temp"))
  (talk-mode-remote somebody-else)
  (talk-setup-windows)
  (run-hooks 'talk-hooks)
)

(defun talk-mode-local ()
  "Function: creates and sets up a local talk buffer. See talk-mode
for details."
  (interactive)
  (setq mode-name talk-local-buffer-name)
  (talk-mode)
  (setq mode-name talk-local-mode-string)
  (run-hooks 'talk-mode-local-hooks))
  
(defun talk-mode-remote (in-string)
  "Function: This takes a buffer and turns it into a remote talk window,
complete with TALK process."
  (interactive "sTalk to whom? ")
  ;; parse the command line parameter.
  (if (string-match "\\(@\\)\\( \\)" in-string)
      (progn
	(setq somebody-else (substring in-string 0 (match-beginning 1)))
	(setq somewhere-else (substring in-string (match-end 1) 
					(match-beginning 2)))
	(setq sometty-else (substring in-string (match-end 2)
				      (length in-string))))
    (if (string-match "\\(@\\)" in-string)
	(progn
	  (setq somebody-else (substring in-string 0 (match-beginning 1)))
	  (setq somewhere-else (substring in-string (match-end 1) 
					  (length in-string)))
	  (setq sometty-else nil))
      (if (string-match "\\( \\)" in-string)
	  (progn
	    (setq somebody-else (substring in-string 0 (match-beginning 1)))
	    (setq sometty-else (substring in-string (match-end 1) 
					  (length in-string)))
	    (setq somewhere-else (system-name))) ; no computer 
	                                   ; specified, assume this one.
	(progn
	  (setq somebody-else in-string)
	  (setq somewhere-else (system-name)) ; no computer 
					; specified, assume this one.
	  (setq sometty-else nil)))))
  (setq tname (format "*TALK* {%s}" somebody-else))
  (if (get-buffer tname)
      (progn
	(kill-buffer (current-buffer))
	(set-buffer (get-buffer tname)))
    (rename-buffer tname))
  (talk-mode)
  (setq mode-name talk-remote-mode-string)
  (make-local-variable 'talk-remote-who) 
  (setq talk-remote-who somebody-else)
  (make-local-variable 'talk-remote-where) 
  (setq talk-remote-where somewhere-else)
  (make-local-variable 'talk-remote-tty) 
  (setq talk-remote-tty sometty-else)
  (make-local-variable 'talk-edit-chars) 
  (setq talk-edit-chars "")
  (make-local-variable 'talk-filter-message)
  (setq talk-filter-message "")
  (make-local-variable 'talk-remote-is-emacs) 
  (setq talk-remote-is-emacs nil)
  (talk-startup-talkprocess)
  (run-hooks 'talk-mode-remote-hooks)
)
 
(defun talk-mode ()
  "Major mode for using the standard TALK interface via Emacs buffers.
All keystrokes are taken over except ctrl-x and Meta sequnces.
Editing commands are as follows:

When connected to a regular talk server, the following keys work.

  C-@     Set mark.
  C-c     Extended commands
     C-c  Hangup
  C-f     Force line wrap on current word.
  C-k     Kill line. (backwards)
  C-l     Refresh screen.
  C-r     Redo the windows.
  C-w     Delete word backwards.
  C-y     Yank kill ring into your talk window.

When connected to someone else using Emacs Talk.

  C-c     Extended commands
      m   Send message to other emacs minibuffer.
"
  (interactive)
  (kill-all-local-variables)
  ;; set the talk-point marker if it exists, or make one.
  (if (not (markerp 'talk-point)) 
      (progn 
	(make-local-variable 'talk-point)
	(setq talk-point (make-marker))))
  (set-marker talk-point (point-max))

  (use-local-map talk-mode-map)
  (setq mode-name "Talk-Remote")
  (setq major-mode 'talk-mode)
  (setq local-abbrev-table talk-mode-abbrev-table)
  (set-syntax-table talk-mode-syntax-table)
  (run-hooks 'talk-mode-hooks))

(defun talk-setup-windows ()
  "Function: Talkes the list of active talk buffers and
sets up windows for them on the screen."
  (interactive)

  ;; take over the screen.
  (switch-to-buffer talk-local-buffer-name)
  (delete-other-windows (selected-window))
  
  ;; get window data type stuff.
  (setq h (window-height))		;height available
  (setq nw (+ 1 (length talk-remote-process-list))) ;number of windows
  (setq lpw (/ h nw))			;lines per window

  ;; ok, now start dividing.
  (setq h (- h lpw))
  (setq l talk-remote-process-list)
  
  (while (> nw 1)
    (split-window (selected-window) h)
    (switch-to-buffer (process-buffer (car l)))
    (sleep-for 1)
    (setq h (- h lpw))
    (setq nw (- nw 1))
    (setq l (cdr l)))
  (select-window (get-buffer-window (get-buffer talk-local-buffer-name))))

(defun talk-pop-kill-ring ()
  "Remove the first element from the kill ring."
  (interactive)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))

(defun talk-error-thing (fmt &optional ARG ARG2 ARG3)

  (if talk-function-trace
      (progn
	(message fmt ARG ARG2 ARG3)
	(sleep-for 1))))

(message "Send praise and bug reports to s5451066@titan.ucc.umass.edu")
(sleep-for 1)
