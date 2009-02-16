;;; eev.el -- add support for e-scripts in Emacs.

;; Copyright (C) 1999,2000,2001,2002,2003,2004,2005,2006 Free Software
;; Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2006nov12
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>
;;            and: <http://angg.twu.net/eev-current/eev-langs.el.html>

;;; Commentary:

;; A good introduction to eev is the article about it (called "Emacs
;; and eev, or: How to Automate Almost Everything"). Here's a link to
;; the article: <http://angg.twu.net/eev-article.html>.
;;
;; When I started using GNU Emacs in 1994 I immediately realized that
;; it could be used as my main interface to the system; essentially,
;; due to the way that Lisp was integrated in Emacs, by programming
;; just a few functions I could keep "executable logs" of everything
;; that I did... so I wrote `find-fline', `find-node', `eev' and
;; `code-c-d', and I knew that by using them I would soon learn enough
;; to able to write the other functions that I needed.
;;
;; My central idea with eev - and I was so isolated that it took me
;; about five years to realize that it should really be called "my
;; idea" instead of "the way that Emacs was intended to be used" - was
;; that _everything that we do once should be easy to repeat later_.
;; This "everything" included not only executing commands at shell
;; prompts but also opening files, manpages, manuals in Info format,
;; etc, and locating information in them... At that time graphical
;; interfaces were not so prevalent, and it was quite natural then to
;; just dismiss them as a flawed idea; GUIs suggest that everything
;; should be done with "magic buttons", while Emacs (with eev) offers
;; us an alternative: instead of magic buttons whose innards are
;; hidden we can use small programs - often one-liners - that when
;; executed (and executing them shouldn't be much harder than pressing
;; a button) worked as buttons whose lids are open, with their inner
;; structure visible...
;;
;; "Buttons" to open files, info nodes, manpages, etc, were very easy
;; to implement as one-liners in Lisp; and it was trivial to put those
;; "sexp buttons" in almost all kinds of text files. In programs and
;; scripts they would appear after a comment sign; there would be at
;; most one of those per line, and they would always be the rightmost
;; part of a line. In plain text files we just had to warn possible
;; readers that the sexps sprinkled throught the text were "buttons" -
;; or, in modern terminology, "hyperlinks"... And in order to make
;; those buttons "work" in a file, i.e., be executable, we just had to
;; open the file in Emacs.
;;
;; The interface for executing blocks of shell commands was a bit more
;; unusual, as those blocks looked even less like buttons that the
;; hyperlink sexps did... I realized that I could suppose that:
;;   1) the user who would execute a block of shell command would know
;;      what (s)he was doing;
;;   2) the user executing a block of shell commands would generally be
;;      the same person who wrote the block;
;;   3) both would usually be me; 8-|
;;   4) the user executing a block knows on inspection which lines
;;      (s)he wants to execute, and (s)he can select those lines
;;      manually and issue a command - `M-x eev', typically - to save
;;      those lines in a temporary script; also, (s)he can switch to a
;;      shell and issue a command ("ee") that means "execute the lines
;;      stored in the temporary script";
;;   5) it is possible to send commands to other interpreters besides
;;      shells; the user knows this, and knows how to identify the
;;      language of each block, and knows the commands for sending the
;;      block for each of the supported interpreters.
;; One design decision forced itself on eev from the beginning: Emacs
;; should NOT, NEVER, EVER try to detect sexp hyperlinks or blocks of
;; commands, or the interpreter associated with a block of commands,
;; by itself; hyperlinks and blocks of commands were just plain text
;; with no special mark-up, and the user would be responsible by
;; selecting parts of a text himself and executing them in the right
;; way.
;;
;; That being so, it's no wonder that most people when confronted with
;; eev find it super-weird - especially nowadays, as everyone has got
;; used to Emacs being very good at syntactical analysis and context
;; detection. The idea of a user going over his notes, sometimes
;; adding text or making changes, and sometimes selecting manually a
;; block to execute - by running a function that would "do something
;; on the current block", where both this "something to do" and the
;; notion of the "current block" would depend on the function - now
;; might look very alien - but it was quite natural in the mid-90's...
;;
;; (By the way: the "current block" can be the sexp before point, or
;; the sexp that ends at the end of the current line; or the "region"
;; in Emacs, i.e., the text between "point" and "mark", of everything
;; around point until the first occurrence backwards and forward of
;; certain delimiters - other notions of "current block" are possible
;; but not common. And the "do something on the current block" can be
;; "execute it as lisp", "save it into a temporary script", "save it
;; in a temp script then run a program", etc.)
;;
;; The compensation for this apparent weirdness of eev is that
;; implementing support for sending commands to a new interpreter - or
;; for a new kind of hyperlink - is usually something that is
;; accomplished in a handful of lines of Lisp at most - see the
;; definitions for most "find-" functions in this file, and the file
;; (find-eevfile "eev-langs.el").
;;
;; So, one of the most important parts of eev is its extensibility. It
;; is - or it should be - trivial to extend it to support new external
;; interpreters and new kinds of "buttons" or "hyperlinks". Also,
;; sometimes people complain that they can't understand what's the
;; essence of eev; well, here are two articles about that:
;;
;;   <http://www.gnu.org/software/emacs/emacs-paper.html>
;;   <http://www.multicians.org/mepap.html>
;;
;; The "kernel" of eev, morally, is these functions,
;;   (find-efunctiondescr 'find-fline)
;;   (find-efunctiondescr 'find-node)
;;   (find-efunctiondescr 'eev)
;;   (find-efunctiondescr 'code-c-d)
;; and, to a much lesser degree, the ones in "eev-insert.el" and
;; "eev-steps.el":
;;   (find-eevfile "eev-insert.el")
;;   (find-eevfile "eev-steps.el")

;; <unfinished, as lots of docs in this file and elsewhere>
;; <the central point is that people should be using sexps much more,
;; everywhere - why - free software - programming for everyone>
;; <add a pointer to Forth>

;; How to install eev:
;; ===================
;; (find-file "INSTALL")
;; (find-file "eev-rctool")
;;
;; How to try eev without installing it:
;; =====================================
;; (add-to-list 'load-path "~/eev-current/")
;; (add-to-list 'load-path default-directory)
;; (require 'eev)
;; (eev-mode 1)
;; (ee-invade-global-namespace)
;; (require 'eev-insert)
;; (require 'eev-steps)
;; (require 'eev-glyphs)
;; (require 'eev-compose)
;; (eev-set-default-glyphs)
;;
;; Starting points (mainly docstrings):
;; ====================================
;; (find-efunctiondescr 'find-fline)
;; (find-efunctiondescr 'find-node)
;; (find-efunctiondescr 'eev)
;; (find-efunctiondescr 'eev-mode)
;; (find-efunctiondescr 'eev-mode "`pop-up-windows' is off")
;; (find-efunctiondescr 'code-c-d)
;; (find-eevfile "eev-insert.el" "create and display a buffer")
;; (find-efunctiondescr 'eemklinks-yank-pos-spec)
;; (find-efunctiondescr 'eesteps)

;; About the naming of function in this file
;; =========================================
;; Many functions in this file have very short names. This is
;; because they are intended to be used in one-liners in comments,
;; like this:
;;   (find-node "(emacs)Lisp Eval" "C-x C-e")
;;
;; Note: the "C-x C-e" part of the hyperlink above is a "pos-spec". Most
;; hyperlink functions defined by eev support "pos-spec-lists"; see:
;;   (find-efunctiondescr 'ee-goto-position)
;;
;; Prefixes:
;;   "find-"    functions are hyperlinks.
;;   "find-e"   functions are hyperlinks to "Emacs things".
;;   "ee"       execute block or send it to an external program
;;   "ee-"      internal functions (and variables).
;; Suffixes:
;;   "0"        means "more low-level", and
;;   "00"       means "even more low-level". For example:
;;   "sh"       means "run on a shell, display the output in a buffer",
;;   "sh0"      means "run on a shell, display the output in the echo area",
;;   "sh00"     means "same as `sh0', but don't strip the last newline";
;;   "pp"       means "pretty-print a sexp, display the result in a buffer",
;;   "pp0"      means "pretty-print a sexp, display in the echo area"
;; Prefixes and suffixes:

;;   "eeb-"     bounded wrapper (see eev-bounded)
;;   "-bounded" bounded wrapper (see eev-bounded)

;;   "find-"    functions are generated by `(code-c-d "xxx" ...)'.


;; Structure of this file:
;; autoloads for external functions
;; environment variables
;; variables
;; basic hyperlinks: find-fline and find-node
;; support for pos-specs in hyperlinks
;; hyperlinks to anchors
;; hyperlinks to the output of Emacs's help-like functions
;; hyperlinks to the source code of Emacs functions and variables
;; hyperlinks to buffers
;; pretty-printing sexps
;; hyperlinks to other things internal to Emacs
;; hyperlinks to the output of shell commands
;; hyperlinks to manpages
;; hyperlinks to files in html
;; hyperlinks to pages in dvi/ps/pdf documents
;; around point / ask
;; hyperlinks to information about Debian packages
;; code-ps/dvi: mass-producing hyperlink functions
;; code-c-d: mass-producing hyperlink functions
;; examples of calls to code-c-d (debian-centric)
;; temporary highlighting (flashing)
;; evaluating sexps (alternatives to eval-last-sexp)
;; eev and friends (or: saving regions as temporary scripts)
;; setting `pop-up-windows' to nil inside eev-mode
;; eev mode keymap
;; eev mode
;; invading the global namespace
;; aliases for compatibility with previous versions

;; For printing:
;; (find-angg ".emacs" "eea2ps")
;; (progn (find-eev "eev.el") (eea2ps3 (point-min) (point-max)))

;; Big letters courtesy of Figlet.




;;;              _        _                 _     
;;;   __ _ _   _| |_ ___ | | ___   __ _  __| |___ 
;;;  / _` | | | | __/ _ \| |/ _ \ / _` |/ _` / __|
;;; | (_| | |_| | || (_) | | (_) | (_| | (_| \__ \
;;;  \__,_|\__,_|\__\___/|_|\___/ \__,_|\__,_|___/
;;;                                               
;;; autoloads for external functions
;; (find-elnode "Autoload")

(autoload 'Info-goto-node "info")
(autoload 'Info-find-node "info")
(autoload 'find-function-read "find-func")
(autoload 'pp-to-string "pp")
(autoload 'Man-fontify-manpage "man" nil t)
(autoload 'word-at-point "thingatpt")
(autoload 'list-iso-charset-chars     "mule-diag")
(autoload 'list-non-iso-charset-chars "mule-diag")



;;;   ___ _ ____   __  __   ____ _ _ __ ___ 
;;;  / _ \ '_ \ \ / /  \ \ / / _` | '__/ __|
;;; |  __/ | | \ V /    \ V / (_| | |  \__ \
;;;  \___|_| |_|\_/      \_/ \__,_|_|  |___/
;;;
;;; Set some environment variables (for ee-expand, getenv,
;;; shell buffers, xterms started from Emacs, etc).

;; (find-eevrcfile ".bashrc")
;; (find-eevrcfile ".zshrc")

(defun ee-expand (fname)
"Expand \"~\"s and \"$ENVVAR\"s in file names, but only at the beginning of the string."
  (cond ((string-match "^\\$\\([A-Za-z_][0-9A-Za-z_]*\\)\\(.*\\)" fname)
	 (concat (getenv (match-string 1 fname))
		 (match-string 2 fname)))
	((string-match "^\\(~\\([a-z][0-9a-z_]*\\)?\\)\\(/.*\\)?$" fname)
	 (concat (expand-file-name (match-string 1 fname))
		 (match-string 3 fname)))
	(t fname)))

(defun ee-setenv (envvar value)
  "In case the environment variable ENVVAR was not set set it to VALUE."
  (if (null (getenv envvar))
      (setenv envvar (ee-expand value))))

(ee-setenv "EEVDIR"
	   (let ((fname (locate-library "eev")))
	     (if fname (directory-file-name (file-name-directory fname))
	       "$HOME/eev-current")))	; eev.el, etc
(ee-setenv "EEVTMPDIR" "$HOME/.eev")	; ee.sh and other temp scripts

(ee-setenv "EEVRCDIR"  "$EEVDIR/rcfiles")
(ee-setenv "EE"        "$EEVTMPDIR/ee.sh")
(ee-setenv "EEG"       "$EEVTMPDIR/ee.eeg")
(ee-setenv "EEGDB"     "$EEVTMPDIR/ee.gdb")
(ee-setenv "EETEX"     "$EEVTMPDIR/ee.tex")
(ee-setenv "EEC"       "$EEVTMPDIR/ee.c")
(ee-setenv "EETMPC"    "$EEVTMPDIR/tmp.c")
(ee-setenv "EEAOUT"    "$EEVTMPDIR/ee.aout")





;;;                  _       _     _
;;;                 (_)     | |   | |          
;;; __   ____ _ _ __ _  __ _| |__ | | ___  ___ 
;;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;;
;;; variables
;;;

(defvar ee-eevdir       (ee-expand "$EEVDIR/")
  "The directory where the elisp files for eev live.")
(defvar ee-eevtmpdir    (ee-expand "$EEVTMPDIR/")
  "The directory where the temporary script files are put.")
(defvar ee-eevrcdir     (ee-expand "$EEVRCDIR/")
  "The directory where some auxiliary rcfiles for eev are to be found.")

(defvar ee-file         (ee-expand "$EE")
  "The temporary script file used by `eev'.")
(defvar ee-file-tex     (ee-expand "$EETEX")
  "The temporary script file used by `eelatex'.")
(defvar ee-file-gdb     (ee-expand "$EEGDB")
  "The temporary script file used by `eegdb'.")
(defvar ee-file-generic (ee-expand "$EEG"))

(defvar eelatex-eevscript
  "cd $EEVTMPDIR/; latex tmp.tex && xdvi tmp.dvi &" "See `eelatex'.")
(defvar ee-anchor-format       nil       "See `ee-goto-anchor'.")

;; To do: check where these variables are used and unify them.
;; (find-eev "eev-insert.el")
(defvar ee-hyperlink-prefix "# ")
(defvar ee-comment-prefix "# ")

;; Emacs 22 needs these `put's. See:
;; (find-elnode "File Local Variables" "`safe-local-variable' property")
(put 'ee-anchor-format     'safe-local-variable 'stringp)
(put 'ee-comment-prefix    'safe-local-variable 'stringp)
(put 'ee-hyperlink-prefix  'safe-local-variable 'stringp)

(defvar ee-find-man-flag nil)		; for asynchronous `ee-goto-position's
(defvar ee-find-man-pos-spec-list nil)	; for asynchronous `ee-goto-position's
(defvar ee-buffer-name nil)		; overridden by `let's
(defvar ee-arg nil)			; overridden by `let's
(defvar ee-info-file nil)		; for eev-insert.el
(defvar ee-info-code nil)		; for eev-insert.el
(defvar ee-pop-up-windows nil)		; for an eegud hack, not working

;; (setq eeb-highlight-spec '(highlight 0.2))
(defvar ee-highlight-spec  '(highlight 0.75)) ; to do: rename highlight->flash
(defvar eeb-highlight-spec '(highlight 0.5))
(defvar eek-highlight-spec '(region 0.75))
(defvar eeflash-default '(highlight 0.5))

;; (defvar eev-mode-map nil)		; moved down to make the html nicer
;; (defvar eev-mode-global-settings-restorer nil)   ; below
;; (defvar eev-mode-global-settings-saver ...)      ; below

;; (defvar code-c-d-keywords nil		    ; below
;;   "An alist of (KEYWORD . SEXP) pairs. See `code-c-d'.")

(defvar code-c-d-list nil
  "Each (code-c-d C D) call generates an entry (C (ee-expand D)) in this list.
A new entry with the same C as a previous one removes the old
one. This list is maintained by `code-c-d-register' and is used
by some functions in \"eev-insert.el\".")





;;;  _               _        _ _       _        
;;; | |__   __ _ ___(_) ___  | (_)_ __ | | _____ 
;;; | '_ \ / _` / __| |/ __| | | | '_ \| |/ / __|
;;; | |_) | (_| \__ \ | (__  | | | | | |   <\__ \
;;; |_.__/ \__,_|___/_|\___| |_|_|_| |_|_|\_\___/
;;;                                              
;;; Basic links: find-fline and find-noe

(defun find-fline (fname &rest pos-spec-list)
  "Hyperlink to a file (or a directory).
This function is similar to `find-file' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (find-file  \"~/.emacs\")
  (find-fline \"~/.emacs\")
  (find-fline \"~/.emacs\" \"Beginning of the eev block\")"
  (find-file (ee-expand fname))
  (apply 'ee-goto-position pos-spec-list))

(defun find-node (nodestr &rest pos-spec-list)
  "Hyperlink to an info page.
This function is similar to `info' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (info \"(emacs)Lisp Eval\")
  (find-node \"(emacs)Lisp Eval\" \"C-x C-e\")"
  (Info-goto-node nodestr)
  (apply 'ee-goto-position pos-spec-list))



;;;                                                _ _     _       
;;;  _ __   ___  ___       ___ _ __   ___  ___    | (_)___| |_ ___ 
;;; | '_ \ / _ \/ __| ___ / __| '_ \ / _ \/ __|___| | / __| __/ __|
;;; | |_) | (_) \__ \|___|\__ \ |_) |  __/ (__|___| | \__ \ |_\__ \
;;; | .__/ \___/|___/     |___/ .__/ \___|\___|   |_|_|___/\__|___/
;;; |_|                       |_|                                   
;;;
;;; support for pos-spec-lists in hyperlinks
;;;

(defun ee-goto-position (&optional pos-spec &rest rest)
  "Process the \"absolute pos-spec-lists\" arguments in hyperlink functions.
POS-SPEC, the first element of a pos-spec-list, is treated
specially; if it is a string then jump to the first occurrence of
that string in the buffer, and if it a number jump to the line
with that number in the buffer; if it is nil do nothing.

The rest of the pos-spec-list, REST, is treated by
`ee-goto-rest'.

Many kinds of hyperlinks - for example,

  (find-efunction 'ee-goto-position)

already jump to specific positions of a buffer; those hyperlink
functions support \"relative pos-spec-lists\", and they invoke
`ee-goto-rest' straight away to handle their pos-spec-lists -
they skip the first \"absolute\" pos-spec."
  (when pos-spec
    (cond ((numberp pos-spec)
	   (goto-char (point-min))
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)
	   (goto-char (save-excursion	          ; This used to be just:
			(goto-char (point-min))	  ; (goto-char (point-min))
			(search-forward pos-spec) ; (search-forward pos-spec)
			(point))))		  ;
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

(defun ee-goto-rest (list)
  "Process \"relative pos-spec-lists\".
For each element in LIST, if it is:

  a string -> jump to the next occurrence of that string in the
              current buffer
  a number -> go down that many lines
  a list   -> evaluate the list (take care!)

anything else generates an error - but users are encouraged to
create their own extended versions of this function and override
the standard definition."
  (cond ((null list))
	((stringp (car list))
	 (search-forward (car list))
	 (ee-goto-rest (cdr list)))
	((numberp (car list))
	 (forward-line (car list))
	 (ee-goto-rest (cdr list)))
	((consp (car list))
	 (eval (car list))
	 (ee-goto-rest (cdr list)))
	(t (error "Not a valid pos-spec item: %S" (car list)))))




;;;                   _                    
;;;   __ _ _ __   ___| |__   ___  _ __ ___ 
;;;  / _` | '_ \ / __| '_ \ / _ \| '__/ __|
;;; | (_| | | | | (__| | | | (_) | |  \__ \
;;;  \__,_|_| |_|\___|_| |_|\___/|_|  |___/
;;;
;;; hyperlinks to anchors
;;;

(defun ee-format-as-anchor (tag)
  "Convert TAG into an anchor using `ee-anchor-format'."
  (if ee-anchor-format
      (format ee-anchor-format tag)
    (error "`ee-anchor-format' is nil - can't convert string to anchor")))

(defun ee-goto-anchor (&optional tag &rest rest)
  "Like `ee-goto-position', but TAG is converted to an anchor.
If the anchor obtained from TAG is not found then issue an error
but do not move point.
For example, if `ee-anchor-format' is \"<<%s>>\" then

  (ee-goto-anchor \"foo\" \"bar\")

searches for the first occurrence of \"<<foo>>\" in the current
buffer, then for the first occurrence of \"bar\" after that. If
\"<<foo>>\" is not found then do not move point.

It is good style to set `ee-goto-anchor' globally to nil and only
use anchors in files where `ee-anchor-format' is declared in the
local variables section of the file; see:

  (find-node \"(emacs)File Variables\")
  (find-node \"(emacs)Specifying File Variables\")

a hint: one way of forcing reloading the local variables by hand
is by running `\\[normal-mode]'. 

The glyphs defined in (find-eev \"eev-glyphs.el\") can be used to
make anchors using characters that stand out."
  (if tag (goto-char
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward (ee-format-as-anchor tag))
	     (point))))
  (ee-goto-rest rest))



(defun find-anchor (fname &optional tag &rest pos-spec-list)
  "Like `find-fline', but TAG is converted to an anchor if not nil.
See `ee-goto-anchor'."
  (find-fline fname)
  (apply 'ee-goto-anchor tag pos-spec-list))

(defun ee-to (tag &rest pos-spec-list)
  "Like `find-anchor', but does not switch to another buffer or file."
  ;; To do: include an example of an index.
  ;; Maybe a link to a files in "examples/"? (find-eevexfile "lua.e")
  (interactive "sAnchor: ")
  (apply 'ee-goto-anchor tag pos-spec-list))




;;;   __ _           _                    _   _   _     
;;;  / _(_)_ __   __| |    __      _____ | |_| |_| |__  
;;; | |_| | '_ \ / _` |____\ \ /\ / / _ \| __| __| '_ \ 
;;; |  _| | | | | (_| |_____\ V  V / (_) | |_| |_| |_) |
;;; |_| |_|_| |_|\__,_|      \_/\_/ \___/ \__|\__|_.__/ 
;;;
;;; hyperlinks to the output of Emacs's help-like functions
;;;

(defun find-wottb-call (sexp bufname &rest pos-spec-list)
  "Hyperlink to functions that call `with-output-to-temp-buffer'.
First evaluate SEXP with a trick to not let it split the current window,
then switch to the buffer that it created (it must be called BUFNAME),
then go to the position specified by POS-SPEC-LIST.\n
\(This is a horrible hack.)"
  (let ((same-window-buffer-names
	 (cons bufname same-window-buffer-names)))
    (eval sexp))
  (set-buffer bufname)			; why is this needed?
  (apply 'ee-goto-position pos-spec-list))

(defun find-eapropos (regexp &rest pos-spec-list)
  "Hyperlink to the result of running `apropos' on REGEXP."
  (interactive "sApropos symbol (regexp): ")
  (apply 'find-wottb-call '(apropos regexp) "*Apropos*" pos-spec-list))

(defun find-efunctiondescr (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `describe-function' on SYMBOL."
  (interactive (find-function-read))
  (apply 'find-wottb-call '(describe-function symbol) "*Help*" pos-spec-list))

(defun find-evariabledescr (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `describe-variable' on SYMBOL."
  (interactive (find-function-read 'variable))
  (apply 'find-wottb-call '(describe-variable symbol) "*Help*" pos-spec-list))

(defalias 'find-evardescr 'find-evariabledescr)

(defun find-ekeydescr (key &rest pos-spec-list)
  "Hyperlink to the result of running `describe-key' on KEY."
  (interactive "kFind function on key: ")
  (apply 'find-wottb-call '(describe-key key) "*Help*" pos-spec-list))

(defun find-efacedescr (face &rest pos-spec-list)
  "Hyperlink to the result of running `describe-face' on FACE."
  (interactive (list (read-face-name "Describe face")))
  (apply 'find-wottb-call '(describe-face face) "*Help*" pos-spec-list))

(defun find-efaces (&rest pos-spec-list)
  "Hyperlink to the result of running `list-faces-display'."
  (interactive)
  (apply 'find-wottb-call '(list-faces-display) "*Faces*" pos-spec-list))

(defun find-ecolors (&rest pos-spec-list)
  "Hyperlink to the result of running `list-colors-display'."
  (interactive)
  (apply 'find-wottb-call '(list-colors-display) "*Colors*" pos-spec-list))

(defun find-efunctiond (function &rest pos-spec-list)
  "Hyperlink to the result of running `disassemble' on FUNCTION."
  (interactive (find-function-read))
  (apply 'find-wottb-call '(disassemble function) "*Disassemble*"
	 pos-spec-list))




;;;   __ _           _            __                  _   _             
;;;  / _(_)_ __   __| |      ___ / _|_   _ _ __   ___| |_(_) ___  _ __  
;;; | |_| | '_ \ / _` |____ / _ \ |_| | | | '_ \ / __| __| |/ _ \| '_ \ 
;;; |  _| | | | | (_| |____|  __/  _| |_| | | | | (__| |_| | (_) | | | |
;;; |_| |_|_| |_|\__,_|     \___|_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|
;;;                                                                      
;;; hyperlinks to the source code of Emacs functions and variables
;;;

(defun find-ebufferandpos (buffer-and-pos &rest pos-spec-list)
  "Internal use; hyperlink to a \"buffer and pos\" structure.
Emacs has some standard (i.e., non-eev) functions that can be
used as hyperlinks, like `find-function' and `find-variable';
they call internal functions like `find-function-noselect' and
`find-variable-noselect', that return structures of the form
BUFFER-AND-POS, that are conses like (#<buffer foo> . 42). This
function jumps to the position described by a cons like that, and
then processes an optional relative POS-SPEC-LIST using
`ee-goto-rest'.

Functions like `find-efunction' and `find-evariable' (defined in
eev.el) are wrappers around `find-function' and `find-variable'
that add support for a relative pos-spec-list after the symbol."
  (if (not (bufferp (car buffer-and-pos)))
      (error "Bad (BUFFER . POS): %S" buffer-and-pos))
  (switch-to-buffer (car buffer-and-pos))
  (goto-char (cdr buffer-and-pos))
  (ee-goto-rest pos-spec-list))

(defun find-efunction (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `find-function' on SYMBOL.
The `find-function' function of Emacs can be used as a hyperlink
- it finds the Elisp source code of SYMBOL -, but it doesn't
support a POS-SPEC-LIST like this function does."
  (interactive (find-function-read))
  (apply 'find-ebufferandpos (find-function-noselect symbol) pos-spec-list))

(defun find-evariable (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `find-variable' on SYMBOL."
  (interactive (find-function-read 'variable))
  (apply 'find-ebufferandpos (find-variable-noselect symbol) pos-spec-list))

(defun find-eCfunction (fun &rest pos-spec-list)
  "Hyperlink to the source in C for an Emacs primitive."
  (interactive (find-function-read))
  (apply 'find-ebufferandpos
	 (find-function-search-for-symbol
	  fun nil (help-C-file-name (indirect-function fun) 'fun))
	 pos-spec-list))

(defun find-eCvariable (symbol &rest pos-spec-list)
  "Hyperlink to the definition in the C source of an Emacs variable."
  (interactive (find-function-read 'variable))
  (apply 'find-ebufferandpos
	 (find-variable-noselect
	  symbol (help-C-file-name symbol 'var))
	 pos-spec-list))




;;;   __ _           _            _            __  __           
;;;  / _(_)_ __   __| |       ___| |__  _   _ / _|/ _| ___ _ __ 
;;; | |_| | '_ \ / _` |_____ / _ \ '_ \| | | | |_| |_ / _ \ '__|
;;; |  _| | | | | (_| |_____|  __/ |_) | |_| |  _|  _|  __/ |   
;;; |_| |_|_| |_|\__,_|      \___|_.__/ \__,_|_| |_|  \___|_|   
;;;                                                             
;;; Hyperlinks to buffers

(defun find-ebuffer (buffer &rest pos-spec-list)
  "Hyperlink to an Emacs buffer (existing or not)."
  (interactive "bBuffer: ")
  (switch-to-buffer buffer)
  (apply 'ee-goto-position pos-spec-list))

(defun find-escratchbuffer (buffer-name &rest pos-spec-list)
  "Hyperlink to an empty scratch buffer named BUFFER-NAME.
If a buffer named BUFFER-NAME exists then try to kill it with
`kill-buffer' to create a scratch buffer in its place. If the
buffer contains something precious then `kill-buffer' will ask
the user for confirmation; if the user decides not to kill the
buffer then this function aborts with an error."
  (if (get-buffer buffer-name)
      (if (kill-buffer buffer-name)
	  (switch-to-buffer buffer-name)
	(error "Not killing the buffer %s" buffer-name))
    (switch-to-buffer buffer-name)))

(defun find-estring (str &rest pos-spec-list)
  "Visit a temporary buffer whose contents are given by STR.
The default name for the buffer is \"*string*\", but this can be
overriden by setting `ee-buffer-name' to another name with a `let'.
If the buffer already exists its contents are destroyed.
The buffer is not made read-only."
  (find-escratchbuffer (or ee-buffer-name "*string*"))
  (delete-region (point-min) (point-max))
  (insert str)
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

(defun find-eoutput (code &rest pos-spec-list)
  "Hyperlink to the output of running CODE in an empty scratch buffer.
CODE is run again every time this hyperlink if followed; compare
with `find-eeffect' and `find-sh'."
  (find-escratchbuffer (or ee-buffer-name "*output*"))
  ;; (delete-region (point-min) (point-max))
  (eval code)
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

;; hyperlinks to the output of sexps
;; Note: this is recent (2006jan22), and maybe several functions
;; should be reimplemented using this
;;
(defun find-eeffect (ee-buffer-name code &rest pos-spec-list)
  "Hyperlink to the effect of running CODE in Emacs.
If the buffer EE-BUFFER-NAME does not exist then create it and
run CODE there; if the buffer already exists then reuse it -
suppose that its contents are already the result of running CODE
and do not run CODE again."
  (if (get-buffer ee-buffer-name)
      (apply 'find-ebuffer ee-buffer-name pos-spec-list)
    (find-ebuffer ee-buffer-name)
    (eval code)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (apply 'ee-goto-position pos-spec-list)))



;;;   __ _           _                        
;;;  / _(_)_ __   __| |      ___ _ __  _ __  
;;; | |_| | '_ \ / _` |____ / _ \ '_ \| '_ \ 
;;; |  _| | | | | (_| |____|  __/ |_) | |_) |
;;; |_| |_|_| |_|\__,_|     \___| .__/| .__/ 
;;;                             |_|   |_|    
;;; pretty-priting sexps

;; "pp0" -> "pretty-print a Lisp object in a very compact way".
;;
(defun ee-pp0 (object &optional tick)
  "Convert OBJECT (usually a sexp) into a string, for use in hyperlinks.
Quote newlines to make it fit in a single line.
If TICK is non-nil and OBJECT is a list then precede it with a \"'\".
The result of this function is always a string that can be `read' as Lisp."
  (let ((str (let ((print-escape-newlines t)
		   (print-escape-nonascii t) ; isn't escaping esc, \r, etc
		   (print-quoted t))
	       (prin1-to-string object))))
    (setq str (replace-regexp-in-string "\r" "\\\\r" str))
    (if (and tick (consp object))
	(setq str (concat "'" str)))
    str))

(defun find-epp0 (object)
  "Display a pretty-printed version of OBJECT in the echo area.
This function uses `message' and so it only makes sense to call
it from commands bound to keys, not by sexps that are evaluated
explicitly. Try this: (progn (message \"foo\") \"bar\")"
  (message (ee-pp0 object)))

(defun find-epp (object &rest pos-spec-list)
  "Visit a temporary buffer containing a pretty-printed version of OBJECT."
  (let ((ee-buffer-name (or ee-buffer-name "*pp*")))
    (apply 'find-estring (pp-to-string object) pos-spec-list)))

(defun find-efunctionpp (symbol &rest pos-spec-list)
"Visit a temporary buffer containing the pretty-printed Lisp code for SYMBOL."
  (interactive (find-function-read))
  (let ((ee-buffer-name
	 (or ee-buffer-name (format "*function %S*" symbol))))
    (apply 'find-epp
	   (symbol-function symbol)
	   ;; Note: if instead of the above we use
	   ;;  `(fset ',symbol ',(symbol-function symbol))
	   ;; the we get a buffer in which we can edit the code for SYMBOL.
	   pos-spec-list)))

(defun find-etpat (&rest pos-spec-list)
"Hyperlink to a pretty-version of the result of (text-properties-at (point))."
  (interactive)
  (let* ((ee-buffer-name
	  (or ee-buffer-name "*(text-properties-at (point))*")))
    (apply 'find-epp (text-properties-at (point)) pos-spec-list)))

(defun find-etpat0 ()
  "Show the result of (text-properties-at (point)) in the echo area."
  (interactive)
  (find-epp0 (text-properties-at (point))))




;;;   __ _           _                             
;;;  / _(_)_ __   __| |       _____  ____  ____  __
;;; | |_| | '_ \ / _` |_____ / _ \ \/ /\ \/ /\ \/ /
;;; |  _| | | | | (_| |_____|  __/>  <  >  <  >  < 
;;; |_| |_|_| |_|\__,_|      \___/_/\_\/_/\_\/_/\_\
;;;                                                
;;; hyperlinks to other things internal to Emacs

(defun find-echarsetchars (charset &rest pos-spec-list)
  "See: (find-efunction 'list-charset-chars)
Examples: (find-echarsetchars 'mule-unicode-0100-24ff \"733x\")
          (find-echarsetchars 'mule-unicode-2500-33ff)"
  (interactive (list (read-charset "Character set: ")))
  (apply 'find-eoutput 
	 '(cond ((charsetp charset)
		 (list-iso-charset-chars charset))
		((assq charset non-iso-charset-alist)
		 (list-non-iso-charset-chars charset))
		(t (error "Invalid character set %s" charset)))
	 pos-spec-list))

(defun find-eccldump (ccl-code &rest pos-spec-list)
  "Hyperlink to the result of running `ccl-dump' on CCL-CODE.
Example: (find-eccldump ccl-decode-mule-utf-8)"
  (apply 'find-eoutput `(ccl-dump ,ccl-code) pos-spec-list))

(defun find-ekeymapdescr (keymap &rest pos-spec-list)
  "Hyperlink to the list of bindings in KEYMAP.
Example: (find-ekeymapdescr isearch-mode-map \"toggle-regexp\")"
  ;; To do: add the buttons/link thing
  (apply 'find-estring (substitute-command-keys "\\<keymap>\\{keymap}")
	 pos-spec-list))

(defun ee-minor-mode-keymap (mode-symbol)
  "An auxiliary function for `find-ekeymapdescr'.
Example: (find-ekeymapdescr (ee-minor-mode-keymap 'eev-mode))"
  (cdr (assq mode-symbol minor-mode-map-alist)))


;; 2




;;;   __ _           _           _     
;;;  / _(_)_ __   __| |      ___| |__  
;;; | |_| | '_ \ / _` |_____/ __| '_ \ 
;;; |  _| | | | | (_| |_____\__ \ | | |
;;; |_| |_|_| |_|\__,_|     |___/_| |_|
;;;                                    
;;; hyperlinks to the output of shell commands
;;;

(defun find-sh (command &rest pos-spec-list)
  "Hyperlink to the result of running the shell command COMMAND.
If a buffer named COMMAND does not exist then create it and put
there the output or running COMMAND; if a buffer named COMMAND
already exists then reuse it and do not run COMMAND again."
  (interactive "sShell command: ")
  (if (get-buffer command)		; if the buffer already exists
      (switch-to-buffer command)	; then just switch to it
    (switch-to-buffer command)		; otherwise create it
    (insert (shell-command-to-string command)) ; prepare its contents
    (goto-char (point-min)))		; and place point at its beginning
  (apply 'ee-goto-position pos-spec-list))

;; (defalias 'find-sh0 'shell-command-to-string)
(defun find-sh0 (command)
  "Hyperlink to the result of running the shell command COMMAND.
This function does not create a buffer like `find-sh' does;
instead, it just returns the output of COMMAND as string,
removing a trailing newline from the output if one is found.
Follow a `find-sh0' hyperlink just displays the output of the
COMMAND in the echo area."
  (replace-regexp-in-string "\n$" "" (shell-command-to-string command)))

;; The two functions below bypass calling a shell and instead run
;; external programs directly. Note that the suffix "0" means "do not
;; create a new buffer, just display the result in the echo area or
;; return it", and the suffix "00" means "be even more low-level than
;; with "0": do not strip out the final newline".

;; Note: these functions are much faster than find-sh on some
;; badly-designed OSs.

(defun find-callprocess00 (prog &rest args)
  "Example: (find-callprocess00 \"lua\" \"-e\" \"print(1+2)\")"
  (with-output-to-string
    (with-current-buffer
      standard-output
      (apply 'call-process prog nil t nil args))))

(defun find-callprocess0 (&rest args)
  "Example: (find-callprocess0 \"lua\" \"-e\" \"print(1+2)\")"
  (replace-regexp-in-string "\n$" "" (apply 'find-callprocess00 args)))





;;;   __ _           _                             
;;;  / _(_)_ __   __| |      _ __ ___   __ _ _ __  
;;; | |_| | '_ \ / _` |_____| '_ ` _ \ / _` | '_ \ 
;;; |  _| | | | | (_| |_____| | | | | | (_| | | | |
;;; |_| |_|_| |_|\__,_|     |_| |_| |_|\__,_|_| |_|
;;;                                                
;;; hyperlinks to manpages
;;;

(defadvice Man-notify-when-ready (around find-man (man-buffer) activate)
  "After rendering a manpage jump to `ee-find-man-pos-spec-list'."
  (if (not ee-find-man-flag)
      ad-do-it
    (switch-to-buffer man-buffer)
    (apply 'ee-goto-position ee-find-man-pos-spec-list)
    (setq ee-find-man-flag nil)))

(defun find-man (manpage &rest pos-spec-list)
  "Hyperlink to a manpage."
  (interactive (list (ee-manpagename-ask)))
  (setq ee-find-man-flag t
	ee-find-man-pos-spec-list pos-spec-list)
    (man manpage))

;; Missing: find-woman. (find-node "(woman)Top")




;;;   __ _           _               _____           
;;;  / _(_)_ __   __| |    __      _|___ / _ __ ___  
;;; | |_| | '_ \ / _` |____\ \ /\ / / |_ \| '_ ` _ \ 
;;; |  _| | | | | (_| |_____\ V  V / ___) | | | | | |
;;; |_| |_|_| |_|\__,_|      \_/\_/ |____/|_| |_| |_|
;;;
;;; hyperlinks to files in html
;;;

;; To do: factor this and create a function `ee-expand-url'.

(defun find-w3m (url &rest pos-spec-list)
  "Hyperlink to a page in HTML.
Use w3m to render the page as text in an Emacs buffer.
Apply `ee-expand' to URL; this changes URL when it starts with
\"~\" or \"$\". After that if URL starts with \"/\" prepend
\"file://\" to it.

These operations on URL keep \"real urls\" unchanged and convert
several kinds of filenames into urls that w3m can process - but
it doesn't convert relative filenames into urls. See
`expand-file-name'."
  (interactive "Murl: ")
  (let ((enable-local-variables nil)	; workaround for a w3m-el bug
	(w3m-async-exec nil))
    (w3m (replace-regexp-in-string "^/" "file:///" (ee-expand url))))
  (ee-goto-rest pos-spec-list))




;;;   __ _           _          _       _    __                                
;;;  / _(_)_ __   __| |      __| |_   _(_)  / / __  ___ _ __   __ _  __ _  ___ 
;;; | |_| | '_ \ / _` |____ / _` \ \ / / | / / '_ \/ __| '_ \ / _` |/ _` |/ _ \
;;; |  _| | | | | (_| |____| (_| |\ V /| |/ /| |_) \__ \ |_) | (_| | (_| |  __/
;;; |_| |_|_| |_|\__,_|     \__,_| \_/ |_/_/ | .__/|___/ .__/ \__,_|\__, |\___|
;;;                                          |_|       |_|          |___/      
;;; hyperlinks to pages in dvi/ps/pdf documents
;;;

(defun find-dvipage-old (fname &optional n &rest ignore)
  "Write into $EE a command that opens the dvi file FNAME at page N.
The command is \"xdvi +N FNAME\".
See `eev'."
  (interactive "fDVI file: ")
  (let ((command (format "xdvi +%d %s &" (or n 1) fname)))
    (eev command nil)
    command))

(defun find-pspage-old (fname &optional n &rest ignore)
  "Write into $EE a command that opens the ps/pdf file FNAME at page N.
The command is \"gv -page N FNAME\".
See `eev'."
  (interactive "fPS or PDF file: ")
  (let ((command (format "gv -page %d %s &" (or n 1) fname)))
    (eev command nil)
    command))




;; (eebg-xdvi "/usr/share/doc/gdb/refcard.dvi.gz")
;; (eebg-gv "/usr/share/doc/gv/gv.ps.gz" 3 '("-scale" "-2"))
;; (eebg-channel-xterm "A" "bash" '("-gravity" "nw"))

(defun find-dvipagenow (fname &optional page xdviargs)
  "Launch an xdvi process browsing the dvi file FNAME, starting at PAGE.
The process is invoked like this: \"xdvi +PAGE $XDVIARGS FNAME\".
XDVIARGS is a list of strings - by default ().
It runs in background and its output (including error messages)
goes to the buffer \"*Messages*\"."
  (interactive "fdvi file: ")
  (apply 'start-process "xdvi" "*Messages*"
	 `("xdvi"
	   ,@(if page (list (format "+%d" page)))
	   ,@xdviargs
	   ,(ee-expand fname))))

(defun find-pspagenow (fname &optional page gvargs)
  "Launch a gv process browsing the ps or pdf file FNAME, starting at PAGE.
The process is invoked like this: \"gv --page=N $GVARGS FNAME\".
GVARGS is a list of strings - by default ().
It runs in background and its output (including error messages)
goes to the buffer \"*Messages*\"."
  (interactive "fPS or PDF file: ")
  (apply 'start-process "gv" "*Messages*"
	 `("gv"
	  ,@(if page (list (format "--page=%d" page)))
	  ,@gvargs
	  ,(ee-expand fname))))

(defalias 'find-dvipage 'find-dvipagenow)
(defalias 'find-pspage  'find-pspagenow)

(defalias 'eebg-xdvi 'find-dvipagenow)	; backward compatibility
(defalias 'eebg-gv   'find-pspagenow)	; backward compatibility




;;;                                                  _
;;;                                  _              (_)      _
;;;   __ _ _ __ ___  _   _ _ __   __| |  _ __   ___  _ _ __ | |_ 
;;;  / _` | '__/ _ \| | | | '_ \ / _` | | '_ \ / _ \| | '_ \| __|
;;; | (_| | | | (_) | |_| | | | | (_| | | |_) | (_) | | | | | |_ 
;;;  \__,_|_|  \___/ \__,_|_| |_|\__,_| | .__/ \___/|_|_| |_|\__|
;;;                                     |_|                      
;;; around point / ask
;;; inspired by: (find-efile "thingatpt.el")
;;;

(defun ee-message (object)
  "An obsolete hack to debug functions bound to keys. See the code."
  (message "%S" object)
  object)

(defun ee-message-maybe (object)
  "An obsolete hack to debug functions bound to keys. See the code."
  (if show-it (message "%S" object))
  object)

(defalias 'ee-maybe-showing-it 'ee-message-maybe) ; backward compatibilty

;; A way to examine the result of the functions below - by executing
;; them with `M-x functionname' with point on some interesting place -
;; without using ee-message-maybe:
;; (progn (find-estring "Mmm foobar") (eek "5*<right>") (ee-manpagename-around-point))

(defun ee-stuff-around-point (chars &optional show-it)
  (interactive "MChars: \np")		; for tests
  (ee-message-maybe
   (save-excursion
     (let* ((e (progn (skip-chars-forward  chars) (point)))
	    (s (progn (skip-chars-backward chars) (point))))
       (buffer-substring s e)))))

(defun ee-debpkgname-around-point (&optional show-it)
"Return the name of the Debian package around point.
This function is not very smart."
  (interactive "p")
  (ee-stuff-around-point "a-z0-9-+." show-it))

(defun ee-debpkgname-ask (&optional prompt show-it)
"Ask for the name of a Debian package; the default is the debpkgname at point."
  (interactive (list nil t))
  (ee-message-maybe
   (read-string (or prompt "Debian package name: ")
		(ee-debpkgname-around-point))))

(defun ee-manpagename-around-point (&optional show-it)  
"Return the manpagename around point.
This function is not very smart - it doesn't understand section names."
  (interactive "p")
  (ee-stuff-around-point "A-Za-z0-9-+_:." show-it))

(defun ee-manpagename-ask (&optional prompt show-it)
"Ask for the name of a manpage; the default is the manpage name at point."
  (interactive (list nil t))
  (ee-message-maybe
   (read-string (or prompt "Manpage: ")
		(ee-manpagename-around-point))))



;;;      _      _     _             
;;;   __| | ___| |__ (_) __ _ _ __  
;;;  / _` |/ _ \ '_ \| |/ _` | '_ \ 
;;; | (_| |  __/ |_) | | (_| | | | |
;;;  \__,_|\___|_.__/|_|\__,_|_| |_|
;;;                                 
;;; hyperlinks to information about Debian packages
;;;

(defun find-Package (fname &optional packagename &rest pos-spec-list)
  "Hyperlink to \"Package: \" achors in Debian package control files.
See: `find-status', `find-available', (find-man \"grep-dctrl\")"
  (find-fline fname)
  (apply 'ee-goto-position
	 (if packagename (format "\nPackage: %s\n" packagename))
	 pos-spec-list))

(defun find-status (packagename &rest pos-spec-list)
  "Hyperlink to the info about the package PACKAGENAME in /var/lib/dpkg/status.
This is Debian-specific. See `find-Package'."
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/status" packagename pos-spec-list))

(defun find-available (packagename &rest pos-spec-list)
"Hyperlink to the info about the package PACKAGENAME in /var/lib/dpkg/available.
This is Debian-specific. See `find-Package'."
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/available" packagename pos-spec-list))

(defun find-grep-status (grepargs &rest pos-spec-list)
  (interactive "sgrep-status ")
  (apply 'find-sh (concat "grep-status " grepargs) pos-spec-list))

(defun find-grep-available (grepargs &rest pos-spec-list)
  (interactive "sgrep-available ")
  (apply 'find-sh (concat "grep-available " grepargs) pos-spec-list))



;;;                _                         __  _       _ 
;;;   ___ ___   __| | ___       _ __  ___   / /_| |_   _(_)
;;;  / __/ _ \ / _` |/ _ \_____| '_ \/ __| / / _` \ \ / / |
;;; | (_| (_) | (_| |  __/_____| |_) \__ \/ / (_| |\ V /| |
;;;  \___\___/ \__,_|\___|     | .__/|___/_/ \__,_| \_/ |_|
;;;                            |_|                         
;;;
;;; code-ps/dvi: mass-producing hyperlink functions

(defun ee-eval-read-format (formatstr &rest rest)
  "Generate a string from FORMATSTR and REST, then read and eval it.
The resulting string can contain several sexps - this function
wraps it in a \"(progn ...)\" to handle that.

If `ee-arg' is not nil then insert the resulting string in the
current buffer at point instead of evaluating it; see
`find-code-c-d' for an use of that."
  (let ((s (apply 'format formatstr     ; or (concat "(progn " formatstr ")")
		  rest)))
    (if ee-arg (insert "\n" s)		; for debugging
      (eval (read (concat "(progn\n" s "\n)"))))))

(defun code-ps (code psfile)
  "Define a function `find-CODEpage' as a hyperlink to a page in PSFILE.
See `find-pspage', `code-c-d' and `code-dvi'.
An example:\n
  (code-ps \"foo\" \"/tmp/bar.ps\")\n
runs:\n
  (defun find-foopage (&optional n &rest comments)
     (interactive)
     (find-pspage \"/tmp/bar.ps\" n))"
  (ee-eval-read-format
   "(defun find-%spage (&optional n &rest comments) (interactive)
       (find-pspage %S n))"
   code psfile))

(defun code-dvi (code dvifile)
  "Define a function `find-CODEpage' as a hyperlink to a page in DVIFILE.
See `find-dvipage', `code-c-d' and `code-ps'.
An example:\n
  (code-dvi \"foo\" \"/tmp/bar.dvi\")\n
runs:\n
  (defun find-foopage (&optional n &rest comments)
    (interactive)
    (find-dvipage \"/tmp/bar.dvi\" n))"
  (ee-eval-read-format
   "(defun find-%spage (&optional n &rest comments) (interactive)
       (find-dvipage %S n))"
   code dvifile))


;;;                _                          _ 
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                             
;;; code-c-d: mass-producing hyperlink functions

(defun ee-find-tag (tag &rest pos-spec-list)
  (let ((tags-add-tables nil))
    (find-tag tag))
  (ee-goto-rest pos-spec-list))

;; This `ee-read-file-name' is semi-bogus and needs fixing.
;; A test: (ee-read-file-name nil data-directory)
;; Remember, its result will be concatenated to ee-xxxdir.
;; When it returns absolute filenames things break.
;;
(defun ee-read-file-name (prompt defaultdir)
  (let ((default-directory (or defaultdir default-directory)))
    (read-file-name (or prompt "Find file: " nil) nil ".")))


(defalias 'ee-find-codenode 'ee-find-xxxnode) ; backward compatibility
(defalias 'ee-find-cd-sh    'ee-find-xxxsh)   ; backward compatibility
(defalias 'ee-find-cd-sh0   'ee-find-xxxsh0)  ; backward compatibility

(defun ee-find-xxxnode (code infofile nodename &rest pos-spec-list)
  (if code (setq ee-info-code code
		 ee-info-file infofile))
  (apply 'find-node (format "(%s)%s" infofile nodename) pos-spec-list))

(defun ee-find-xxxsh (dir command &rest pos-spec-list)
  "Run COMMAND at DIR and display the result. See `code-c-d'."
  (apply 'find-sh (format "cd %s\n%s" dir command) pos-spec-list))
(defun ee-find-xxxsh0 (dir command)
  "Run COMMAND at DIR and return the result. See `code-c-d'."
  (find-sh0 (format "cd %s\n%s" dir command)))

(defun ee-aref (alist idx)
  "Like `aref', but for alists.
Example: (ee-aref '((1 . one) (2 . two) (3 . three)) 2)
                                -> two"
  (cdr (assoc idx alist)))

(defun ee-adel (alist idx)
  "Like `remq', but for alists. This is non-destructive, so wrap it in a setq.
Example: (ee-adel '((1 . one) (2 . two) (3 . three)) 2)
                -> ((1 . one)           (3 . three))"
  (remq (assoc idx alist) alist))

(defun ee-aset (alist idx newelt)
  "Like `aset', but for alists. This is non-destructive, so wrap it in a setq.
Example: (ee-aset '((1 . one) (2 . two) (3 . three)) 2 'foo)
      -> ((2 . foo) (1 . one)           (3 . three))"
  (cons (cons idx newelt) (ee-adel alist idx)))

(defun code-c-d-register (c d)
  "Add the pair (C D) to `code-c-d-list'.
If `code-c-d-list' already has an entry with the same C delete
the previous one. The new entry is always added to the beginning
of the list."
  (let ((c-d (assoc c code-c-d-list)))
    (if c-d (setq code-c-d-list (delq c-d code-c-d-list))))
  (setq code-c-d-list (cons (list c d) code-c-d-list)))

(defun code-c-d-base (c d)
  "See `code-c-d'."
  (ee-eval-read-format "(setq ee-%sdir \"%s\")" c d)
  (ee-eval-read-format "(setq ee-%stagsfile \"%sTAGS\")" c d)
  (ee-eval-read-format "
    (defun ee-%sfile (str)
      (concat (ee-expand ee-%sdir) str))" c c)
  (ee-eval-read-format "
    (defun ee-use-%s-tags ()
      (setq tags-file-name ee-%stagsfile))" c c)
  (ee-eval-read-format "
    (defun find-%sfile (str &rest pos-spec-list)
      (interactive (list \"\"))
      (ee-use-%s-tags)
      (apply 'find-fline (ee-%sfile str) pos-spec-list))" c c c)
  (ee-eval-read-format "
    (defun find-%stag (str &rest pos-spec-list)
      (ee-use-%s-tags) (apply 'ee-find-tag str pos-spec-list))" c c)
  (ee-eval-read-format "
    (defun find-%ssh (command &rest pos-spec-list)
      (apply 'ee-find-xxxsh ee-%sdir command pos-spec-list))" c c)
  (ee-eval-read-format "
    (defun find-%ssh0 (command)
      (funcall 'ee-find-xxxsh0 ee-%sdir command))" c c)
  (ee-eval-read-format "
    (defun find-%sw3m (furl &rest pos-spec-list)
      (apply 'find-w3m (ee-%sfile furl) pos-spec-list))" c c))

(defvar code-c-d-keywords
  '((:info
     (if b
	 (ee-eval-read-format "
           (setq ee-info-code %S)
           (setq ee-info-file %S)
           (defun find-%snode (nodename &rest pos-spec-list)
             (apply 'ee-find-xxxnode %S %S nodename pos-spec-list))"
          c   b   c c b))
     (code-c-d-rest (cddr rest)))
    (:anchor
     (ee-eval-read-format "
       (defun find-%s (file &rest rest)
         (apply 'find-anchor (ee-%sfile file) rest))" c c)
     (code-c-d-rest (cdr rest)))
    (:gdb
     (ee-eval-read-format "
       (defun eeb-%sgdb-start (&optional fname)
         (ee-use-%s-tags)
         (eeb-gdb-start ee-%sdir fname))" c c c)
     (code-c-d-rest (cdr rest)))
    (:linux
     (ee-eval-read-format "
       (defun find-%sconfvar (var &rest rest)
         (apply 'find-%sfile \"Documentation/Configure.help\"
           (concat \"\\n\" var \"\\n\") rest))" c c)
     (code-c-d-rest (cdr rest)))
    (:xdvi
     (ee-eval-read-format "
       (defun find-%sxdvi (fname &rest rest)
       (apply 'eebg-xdvi (ee-%sfile fname) rest))" c c))
    (:gv
     (ee-eval-read-format "
       (defun find-%sgv (fname &rest rest)
       (apply 'eebg-gv (ee-%sfile fname) rest))" c c))
    )
  "An alist of (KEYWORD . SEXP) pairs. See `code-c-d'.")

(defun code-c-d-rest (rest)
  "Process the \"REST\" of the arguments for a `code-c-d', besides the C and D.
This function works recursively by evaluating sexps from
`code-c-d-keywords'. See the documentation for `code-c-d'."
  (if rest
      (let* ((a (car rest))
	     (b (cadr rest))
	     (pair (assoc a code-c-d-keywords)))
	(if pair (eval (cons 'progn (cdr pair)))
	  (error "Not a code-c-d keyword: %S" a)))))

(defun code-c-d (c d &rest rest)
  "Define a lot of `find-xxxblah' and `ee-xxxblah' functions at once.
This function works by preparing strings and `read'ing and `eval'ing them.
Use `find-code-c-d' or `ee-code-c-d' to examine the generated
code without `eval'ing it.
For example:\n
  (find-code-c-d \"CODE\" \"/PATH/TO/\" \"INFONAME\" :anchor)\n
  (find-estring (ee-code-c-d \"CODE\" \"/PATH/TO/\" \"INFONAME\") \"sh0\")

If REST starts with a string then REST is replaced by (cons :info
REST) for convenience; the resulting value of REST is passed to
`code-c-d-rest', that processes it recursively.

When REST is empty the processing stops. When it is non-empty it
should start with a keyword that has an entry in the
`code-c-d-keywords' alist; `code-c-d-rest' executes the code
associated to that keyword, and that code - that might access the
values of C and D - takes as many arguments as it needs from the
beginning or REST, does something with that, and then invokes
`code-c-d-rest' again with what's left."
  (code-c-d-base c d)
  (unless (equal d "")
    (code-c-d-register c (ee-expand d)))
  (if rest
      (if (stringp (car rest))
	  (code-c-d-rest (cons :info rest))
	(code-c-d-rest rest)))
  c)

;; Compatibility:
(defun code-c-d-anchor (c d &optional i) (code-c-d c d :info i :anchor))
(defun code-c-d-gdb    (c d &optional i) (code-c-d c d :info i :gdb))
(defun code-c-d-linux  (c d) (code-c-d c d :linux))

;; For debugging:
(defun ee-code-c-d (&rest rest)
"Like `code-c-d', but returning the code as a string instead of evaluating it.
See `ee-eval-read-format' and `find-code-c-d'."
  (with-temp-buffer
    (let ((ee-arg 1))
      (apply 'code-c-d rest)
      (buffer-substring (point-min) (point-max)))))

(defun find-code-c-d (&rest rest)
"Show the code that a `code-c-d' call would evaluate, without evaluating it.
This function processes its arguments in exactly the same way as
`code-c-d' does; it doesn't accept a pos-spec-list."
  (find-estring (apply 'ee-code-c-d rest)))



;;;                _                          _     
;;;   ___ ___   __| | ___        ___       __| |___ 
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` / __|
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| \__ \
;;;  \___\___/ \__,_|\___|      \___|     \__,_|___/
;;;                                                 
;;; examples of calls to code-c-d (debian-centric)
;;;

(setq ee-emacs-lisp-directory
  (or (file-name-directory (locate-library "loadup.el"))
      (format "/usr/share/emacs/%d.%d/lisp/"
	      emacs-major-version emacs-minor-version)))

(code-c-d "e"   ee-emacs-lisp-directory "emacs")
(code-c-d "el"  ee-emacs-lisp-directory "elisp")
(code-c-d "eli" ee-emacs-lisp-directory "emacs-lisp-intro")
(code-c-d "eetc" data-directory)

(code-c-d-anchor "eev"    "$EEVDIR/" "eev")     ; (find-eevfile "eev-dev.el")
(code-c-d-anchor "eevtmp" "$EEVTMPDIR/")        ; (find-eevtmpfile "")
(code-c-d-anchor "eevrc"  "$EEVRCDIR/")         ; (find-eevrcfile "")
(code-c-d-anchor "eevex"  "$EEVDIR/examples/")  ; (find-eevexfile "")

(code-c-d "ud" "/usr/share/doc/")	 ; (find-udfile "bash/")
(code-c-d "vldi" "/var/lib/dpkg/info/")	 ; (find-vldifile "bash.list")

(code-c-d "bash" (ee-udfile "bash/") "bashref")
(code-c-d "zsh"  (ee-udfile "zsh/")  "zsh")
(code-c-d "apthowto" "/usr/share/doc/Debian/apt-howto/")




;;;             __ _           _     
;;;   ___  ___ / _| | __ _ ___| |__  
;;;  / _ \/ _ \ |_| |/ _` / __| '_ \ 
;;; |  __/  __/  _| | (_| \__ \ | | |
;;;  \___|\___|_| |_|\__,_|___/_| |_|
;;;                                  
;;; temporary highlighting (flashing)
;;; Note: compare with `eeflash-new' in eev-bounded.el... (2006sep19)
;;; (find-eevfile "eev-bounded.el" "defun eeflash-new")
;;;

(defun eeflash (start end &optional face duration)
  "Highlight the region between START and END using FACE, for time DURATION."
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face (or face 'region))
    (run-at-time (or duration 1) nil 'delete-overlay ovl)))

(defun eeflash+ (s &optional e spec add-to-e)
  "Highlight the region between S and E; face and duration are taken from SPEC.
This function only tries to do any work when S is a number and SPEC is non-nil.
When SPEC is non-nil it should be a pair of the form (FACE DURATION).
The argument ADD-TO-E is a hack for when we know that the region between S and
E+1 ends with a newline and it looks nicer to highlight the newline too; then
we set ADD-TO-E to 1."
  (if (and (numberp s) spec)
      (eeflash s (+ e (or add-to-e 0))
	       (car spec) (cadr spec)))
  (list s e spec add-to-e))



;;;                  _                                            _ 
;;;   _____   ____ _| |      ___  _____  ___ __         ___  ___ | |
;;;  / _ \ \ / / _` | |_____/ __|/ _ \ \/ / '_ \ _____ / _ \/ _ \| |
;;; |  __/\ V / (_| | |_____\__ \  __/>  <| |_) |_____|  __/ (_) | |
;;;  \___| \_/ \__,_|_|     |___/\___/_/\_\ .__/       \___|\___/|_|
;;;                                       |_|                       
;;;
;;; evaluating sexps (alternatives to eval-last-sexp)
;;;

;; ee-eval-sexp-eol may be obsolete
;; ee-arg is still used in eev-insert.el (ack!)

;; TO DO: rename the prefixes from "eek" to just "ee".
;; TO DO: rename ee-eval-sexp-eol to ...-old.

;; We used to have
;; M-e -> `ee-eval-sexp-eol', but now it's
;; M-e -> `eek-eval-sexp-eol' and `ee-eval-sexp-eol' is not bound to
;; any key.
;;
(defun ee-eval-sexp-eol (ee-arg)	; this ee-arg shadows the global one
  (interactive "P")
  (end-of-line)
  (eval-last-sexp nil))

;; See (find-efunction 'eval-last-sexp-1)
(defun eek-backward-sexp ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp -1)
    (when (eq (preceding-char) ?\\)
      (forward-char -1)
      (when (eq (preceding-char) ??)
	(forward-char -1))))
  (point))

(defun eek-forward-sexp ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp 1))
  (point))

(defun eek-last-sexp ()
  (save-excursion
    (buffer-substring-no-properties
     (eek-backward-sexp) (eek-forward-sexp))))

(defmacro eek-no-debug (&rest body)
  `(let ((debug-on-error nil)) . ,body))

(defun eek-eval (sexp)
  (eek-no-debug (eval sexp)))

(defun eek-eval-last-sexp (&optional arg)
  (interactive "P")
  (cond ((eq arg 0)
	 (save-excursion
	   (eeflash+ (eek-backward-sexp) (eek-forward-sexp)
		     eek-highlight-spec)))
	((eq arg 1) (prin1 (eek-last-sexp)))
	((eq arg 2) (prin1 (read (eek-last-sexp))))
	((eq arg 3) (eek-eval (read (eek-last-sexp))))
	((eq arg 4) (let ((sexp (read (eek-last-sexp)))) (debug) (eval sexp)))
	((eq arg 5) (let ((sexp (read (eek-last-sexp)))
			  (debug-on-error t))
		      (eval sexp)))
	((eq arg 8) (find-epp (eek-eval (read (eek-last-sexp)))))
	((eq arg 9) (let ((interactive-clause (read (eek-last-sexp))))
		      (eek-no-debug
		       (call-interactively
			`(lambda (&rest args) ,interactive-clause
			   (message "%S" args))))))
	(t (prin1 (let ((ee-arg arg))
		    (eek-eval (read (eek-last-sexp))))))))

(defun eek-eval-sexp-eol (&optional arg)
  (interactive "P")
  (end-of-line)
  (eek-eval-last-sexp arg))




;;;  __  __                             
;;; |  \/  |    __  __    ___  _____   __
;;; | |\/| | __ \ \/ /   / _ \/ _ \ \ / /
;;; | |  | ||__| >  <   |  __/  __/\ V / 
;;; |_|  |_|    /_/\_\   \___|\___| \_/  
;;;
;;; eev and friends (or: saving regions as temporary scripts)
;;;

(defun ee-se-to-string (s e)
  "Convert the pair (S E) to a string.
If S is a number then return the contents of the current buffer
between the positions S and E; if S is a string then return S and
ignore E. See `write-region' - it uses the same convention for
interpreting \"(S E)\"-pairs as this function."
  (cond ((numberp s) (buffer-substring-no-properties s e))
        ((stringp s) s)))

(defun octal-to-num (str)
  "Convert STR - a sequence of octal digits - to a number."
  (let ((lastv (- (string-to-char (substring str -1)) ?0))
	(rest (substring str 0 -1)))
    (if (string= "" rest) lastv (+ lastv (* 8 (octal-to-num rest))))))

(defun ee-write-string (str &optional altfile fmode)
  "Write STR to ALTFILE, or to ee-file if ALTFILE is nil.
FMODE should be either nil or a string containing a sequence of
octal digits; if it is not nil then do the equivalent of a
\"chmod FMODE file\"."
  (let ((fname (substitute-in-file-name (or altfile ee-file))))
    (write-region str nil fname)	; a standard kludge
    (if fmode (set-file-modes fname (octal-to-num fmode)))))

(defun ee-write (s e pre post &optional altfile fmode)
  "Write PRE+(ee-se-to-string S E)+POST to ALTFILE, or to `ee-file'.
PRE and POST must be strings. See `ee-se-to-string' and
`ee-write-string'."
  (ee-write-string (concat pre (ee-se-to-string s e) post)
		   altfile fmode))

(defun ee-se-to-string-with-nl (s e)
  "Same as `ee-se-to-string', but force the result to end with a newline."
  (let ((str (ee-se-to-string s e)))
    (if (string-match "[^\n]\\'" str) (concat str "\n") str)))

(defun ee-write-with-nl (s e pre post &optional altfile fmode)
  "Same as `ee-write', but using `ee-se-to-string-with-nl'."
  (ee-write-string (concat pre (ee-se-to-string-with-nl s e) post)
		   altfile fmode))



(defun eev (s &optional e altfile)
  "Save the region in `ee-file', or in ALTFILE if it is non-nil.
If S is a string write then write the string instead. See `ee-write'.
This function is mostly used to send blocks of commands to shells via
a temporary script file. The shells do not receive the commands
immediately - we need to tell them to execute the commands stored in
the temporary script.\n
For example, if we mark the block below and type `M-x eev',\n
  # A hyperlink: (find-efunction 'eev)
  echo $[1+2]
  # Temporary scripts can change the
  # directory and the environment.
  cd /tmp/\n
and then go to a prepared shell and run `ee', we see something like
this:\n
  /home/edrx$ ee
  # A hyperlink: (find-efunction 'eev)
  echo $[1+2]
  3
  # Temporary scripts can change the
  # directory and the environment.
  cd /tmp/
  /tmp$ \n
Note that this only works in \"prepared shells\", where `ee' has been
defined as a shell function in the correct way; the relevant code for
.bashrc or .zshrc is this:\n
  export EEVTMPDIR ;: ${EEVTMPDIR:=~/.eev}
  export EE        ;: ${EE:=$EEVTMPDIR/ee.sh}
  function ee () { set -v; . $EE$*; set +v; }\n
See: (find-eevfile \"INSTALL\")
and: (find-eevfile \"eev-rctool\")"
  (interactive "r")
  (ee-write-with-nl s e "" "" altfile)
  (format "eev: wrote %s" (or altfile ee-file)))

(defun eevs (s &optional e suffix)
  "Like `eev', but with a suffix; write the region to `ee-file'+SUFFIX.
For example, if $EE is \"~/.eev/ee.sh\" and SUFFIX is \"0\" then
write the region to the file \"~/.eev/ee.sh0\". The shell
function \"ee\" concatenates its first argument to the value of
$EE, so running \"ee 0\" on a prepared shell executes the
temporary script \"~/.eev/ee.sh0\" instead of \"~/.eev/ee.sh\".
If S is a string write then write the string instead. See `ee-write'."
  (interactive "r\nsSuffix: ")
  (eev s e (concat ee-file suffix)))

(defun eelatex (s &optional e)
  "Save the region to `ee-file-tex', then save `eelatex-eevscript' to `ee-file'.
An example: run `M-x eelatex' on the line below,

  Hello! $\\frac{42}{\\sqrt{5}}$

then go to a prepared shell and run \"ee\". A temporary LaTeX
file will be processed by \"latex\" and the resulting dvi file
will be shown on the screen.
If S is a string write then write the string instead. See `eev'."
  (interactive "r")
  (ee-write s e "" "" ee-file-tex)
  (eev eelatex-eevscript nil)
  (format "eelatex: wrote %s and %s" ee-file-tex ee-file))

(defun eegdb (s &optional e)
  "Save the region to the temporary GDB script file given by `ee-file-gdb'.
After that if your GDB init file was prepared adequately then
running \"ee\" on a GDB prompt will make GDB execute the commands
in the temporary GDB script.
If S is a string write then write the string instead. See `eev'."
  (interactive "r")
  (ee-write s e "" "" ee-file-gdb)
  (format "eegdb: wrote %s" ee-file-gdb))

;; Obsolete, or almost? Used by: (find-eevfile "eeg4")
(defun eeg (s &optional e)
  (interactive "r")
  (ee-write s e "" "" ee-file-generic)
  (format "eeg: wrote %s" ee-file-gdb))

(defun eeeval (s &optional e)
"Like `eev', but instead of saving the region execute it immediately as Lisp.
This function is very similar to `eval-region'."
  (interactive "r")
  (eval (read (concat "(progn " (ee-se-to-string s e) "\n)"))))



(defun ee-default-directory ()
  "Return `default-directory' usually, but behave specially in some modes.
If the current buffer is a w3m buffer that is visiting a local
file (i.e., if the url is like \"file://...\") then extract the
directory from the url instead of returning the value of
`default-directory'.\n
This function is used by `eecd'."
  (if (eq major-mode 'w3-mode)
      (let ((url (url-view-url 0)))
	(if (string-match "^file:\\(.*/\\)[^/]*$" url)
	    (match-string 1 url)
	  (error "Current url is %S, which is not a local file" url)))
    default-directory))

;; 2005jan10, incompatible change: added "dir"
(defun eecd (&optional dir command)
  "Save to $EE a \"cd\" command to `cd' to the current directory.
If DIR is not nil then use DIR; otherwise run `ee-default-directory'.
If COMMAND is not nil then save \"cd DIR; COMMAND\" instead of just
\"cd DIR\".\n
See `eev' for more about $EE and the temporary script file."
  (interactive)
  (eev (concat "cd " (file-name-directory
		      (or dir (ee-default-directory)))
	       "\n" (or command ""))))




;;;           _           _                 _          _                      
;;; __      _(_)_ __   __| | _____      __ | |__   ___| |__   __ ___   ___ __ 
;;; \ \ /\ / / | '_ \ / _` |/ _ \ \ /\ / / | '_ \ / _ \ '_ \ / _` \ \ / / '__|
;;;  \ V  V /| | | | | (_| | (_) \ V  V /  | |_) |  __/ | | | (_| |\ V /| |   
;;;   \_/\_/ |_|_| |_|\__,_|\___/ \_/\_/   |_.__/ \___|_| |_|\__,_| \_/ |_|   
;;;                                                                           
;;; setting `pop-up-windows' to nil inside eev-mode
;;; (what about setting debug-on-error and friends to nil too?)
;;;

(defun ee-setq-to-current-1 (var)
  "If the current value of VAR is VALUE then return (setq VAR 'VALUE).
Example: (let ((a 22)) (ee-setq-to-current-1 'a))"
  `(setq ,var ',(symbol-value var)))

(defun ee-setq-to-current (&rest vars)
  "Example: (find-epp (let ((a 22) (b 33)) (ee-setq-to-current 'a 'b)))
This function is used by `eev-mode' (actually from the code in
the variable `eev-mode-global-settings-saver') to create a sexp
that when eval'ed will restore the current value of some global
variables."
  (cons 'progn (mapcar 'ee-setq-to-current-1 vars)))

;; Ideas for names:
;; eev-vars-setup
;; eev-vars-setup-code
;; eev-vars-reset-code

(defvar eev-mode-global-settings-restorer nil)
(defvar eev-mode-global-settings-saver
  '(progn (setq eev-mode-global-settings-restorer
		(ee-setq-to-current 'pop-up-windows))
	  (setq pop-up-windows nil))
  "If you don't want eev-mode to set any global flags on entry then set this to nil.
The default value for this variable is some Lisp code that sets
the global value of `pop-up-windows' to nil when `eev-mode' is
entered and restores the old value when the mode is left.

Several functions in Emacs can be used as elisp hyperlinks, but
some of them try to follow one convention on splitting windows,
while other follow another one. Let's call those conventions the
\"display help buffer\" convention and the \"follow hyperlink\"
convention; the \"display help buffer\" convention is to keep the
current buffer visible and at the same time display the target of
the hyperlink in another window, splitting the current window if
needed; the \"follow hyperlink\" convention is to never split
windows or switch a different window, and always replace the
buffer in the current window by the target of the hyperlink.

The \"follow hyperlink\" convention makes it easier to return
from hyperlinks: after following a hyperlink just run
`kill-this-buffer' (bound to
`\\<eev-mode-map>\\[kill-this-buffer]' in eev-mode) or
`bury-buffer' (`\\<eev-mode-map>\\[bury-buffer]' in eev-mode).

All hyperlink functions defined by eev follow the \"follow
hyperlink\" convention, and if we set `pop-up-windows' to nil
then most standard Emacs functions usable as hyperlinks also
behave in the \"follow hyperlink\" way.

If you don't want eev-mode to change global variables then do
this (with eev-mode off):

    (setq eev-mode-global-settings-saver nil)")

(put 'eev-mode-global-settings-saver    'risky-local-variable t)
(put 'eev-mode-global-settings-restorer 'risky-local-variable t)

(defun eev-mode-global-settings-set ()
  "This is run by `eev-mode' when eev-mode is turned on.
See the documentation of `eev-mode-global-settings-saver'."
  (if eev-mode-global-settings-restorer
      (message "Not overwriting `eev-mode-global-settings-restorer'.")
    (eval eev-mode-global-settings-saver)))

(defun eev-mode-global-settings-restore ()
  "This is run by `eev-mode' when eev-mode is turned off.
See the documentation of `eev-mode-global-settings-saver'."
  (eval eev-mode-global-settings-restorer)
  (setq eev-mode-global-settings-restorer nil))




;;;  _                                    
;;; | | _____ _   _ _ __ ___   __ _ _ __  
;;; | |/ / _ \ | | | '_ ` _ \ / _` | '_ \ 
;;; |   <  __/ |_| | | | | | | (_| | |_) |
;;; |_|\_\___|\__, |_| |_| |_|\__,_| .__/ 
;;;           |___/                |_|    
;;;
;;; eev mode keymap
;;;

;; Some people have told me that their window managers bind `M-h'...
;; How do I create a secondary key that acts like the `M-h' prefix?

(defvar eev-mode-map nil)
(if eev-mode-map
    ()
(setq eev-mode-map (make-sparse-keymap))
(define-key eev-mode-map "\M-E" 'eek-eval-last-sexp)    ; extends     C-x C-e
(define-key eev-mode-map "\M-e" 'eek-eval-sexp-eol)     ; extends C-e C-x C-e
(define-key eev-mode-map "\M-k" 'kill-this-buffer)      ; convenience
(define-key eev-mode-map "\M-K" 'bury-buffer)           ; convenience
(define-key eev-mode-map [f3]   'eeb-default)
;; (define-key eev-mode-map [f8] 'eewalk-do-walk-or-set) ; in eev-walk.el
(define-key eev-mode-map [f8]   'eepitch-this-line)
(define-key eev-mode-map [f9]   'eechannel-do-this-line)
(define-key eev-mode-map [f12]  'eesteps-do-step)
(define-key eev-mode-map "\M-P" 'ee-yank-one-line)
(define-key eev-mode-map "\M-?" 'eev-help-page)
(define-key eev-mode-map "\M-G" 'eegud-show-gud-buffer)	; hack, in eev-insert.el

(define-key eev-mode-map "\M-h\M-d" 'find-debpkg-links)    ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-f" 'find-efunction-links) ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-i" 'find-einfo-links)     ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-k" 'find-ekey-links)      ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-m" 'find-manpage-links)   ; in eev-insert.el
(define-key eev-mode-map "\M-h\M-v" 'find-evariable-links) ; in eev-insert.el
(define-key eev-mode-map "\M-hf"    'find-file-links)      ; in eev-insert.el
(define-key eev-mode-map "\M-hm"    'find-last-manpage-links) ;in eev-insert.el
(define-key eev-mode-map "\M-h\M-y" 'eemklinks-yank-pos-spec) ;in eev-insert.el
(define-key eev-mode-map "\M-h2"    'eemklinks-duplicate-this-line) ; idem
(define-key eev-mode-map "\M-h\M-2" 'eemklinks-duplicate-this-line) ; idem
(define-key eev-mode-map "\M-I"     'ee-ill)               ; in eev-insert.el

(define-key eev-mode-map "\M-h\M-c" 'describe-char)
(define-key eev-mode-map "\M-h\M-t" 'find-etpat)
(define-key eev-mode-map "\M-ht"    'find-etpat0)
(define-key eev-mode-map "\M-h\M-s" 'find-efacedescr)

(define-key eev-mode-map [?\C-,] 'eev-compose-two-keys)     ; only works on X
(define-key eev-mode-map [?\M-,] 'eev-compose-two-keys)
)



;;;                                           _      
;;;   ___  _____   __     _ __ ___   ___   __| | ___ 
;;;  / _ \/ _ \ \ / /____| '_ ` _ \ / _ \ / _` |/ _ \
;;; |  __/  __/\ V /_____| | | | | | (_) | (_| |  __/
;;;  \___|\___| \_/      |_| |_| |_|\___/ \__,_|\___|
;;;                                                  
;;; eev mode
;;;

(define-minor-mode eev-mode
  "Toggle eev mode, i.e, activate or deactivate the `eev-mode-map' keymap.
With a prefix argument ARG, turn eev-mode on if positive, else off.
\\<eev-mode-map>
Commands to follow hyperlinks:
  \\[eek-eval-last-sexp] -- eval the sexp at the left of point
  \\[eek-eval-sexp-eol] -- go to the end of line, then do \\[eek-eval-last-sexp]
Commands to return from hyperlinks:
  \\[kill-this-buffer] -- kill this buffer
  \\[bury-buffer] -- put this buffer at the end of the list of all buffers
Commands to execute regions or steps:
  \\[eeb-default]  -- execute the default action on bounded regions
  \\[eepitch-this-line]  -- pitch this line to another Emacs buffer,
           or execute this line as lisp if it starts with `'
  \\[eechannel-do-this-line]  -- send this line through the default channel,
           or execute this line as lisp if it starts with `'
  \\[ee-yank-one-line]   -- \"send\" the first line of the last kill, as if the
           user had typed it
  \\[eesteps-do-step] -- execute the next step from an `eesteps' list
  \\[eewalk-do-walk-or-set]  -- read a step from the eewalk window or buffer and
           execute it (from anywhere). With an argument, set the
           eewalk window or buffer. (EXPERIMENTAL FEATURE!)
Commands to generate pages with lists of hyperlinks:
  \\[find-file-links]   -- hyperlinks to the current file
  \\[find-einfo-links] -- hyperlinks to the current Info node
  \\[find-efunction-links] -- hyperlinks to an Emacs function
  \\[find-ekey-links] -- hyperlinks to a key sequence and to the function
             associated to it
  \\[find-evariable-links] -- hyperlinks to an Emacs variable
  \\[find-manpage-links] -- hyperlinks to a manpage (ask for name)
  \\[find-last-manpage-links]   -- hyperlinks to a manpage (being viewed)
  \\[find-debpkg-links] -- hyperlinks about a Debian package
Commands to edit hyperlinks:
  \\[eemklinks-duplicate-this-line]   -- duplicate this line
  \\[eemklinks-yank-pos-spec] -- yank into pos-spec-list
  \\[ee-ill]     -- transform filename into hyperlink
Other commands:
  \\[describe-char] -- lots of info about the character at point
  \\[find-etpat] -- text properties at point
  \\[find-etpat0]   -- text properties at point (output in the echo area)
  \\[find-efacedescr] -- describe a face (default: face at point)
  \\[eev-help-page]     -- switch to a help page, or hide it and return

Note: eev's hyperlinks behave so much more sensibly when
`pop-up-windows' is off that eev-mode sets that
variable (globally!) to nil when eev-mode is entered, and
restores the previous global value when you leave the mode. If
you don't like that behavior then do this (with eev-mode off):

    (setq eev-mode-global-settings-saver nil)

See the docs for `eev-mode-global-settings-saver' for more info."
  :init-value nil :global t
  (if eev-mode
      (eev-mode-global-settings-set)
    (eev-mode-global-settings-restore)))

;; (find-efunctiondescr 'eev-mode)


;;;
;;; invading the global namespace
;;;

(defun eev-set-aliases ()
  "Define some aliases for functions in eev.
The new function names - namely, `to', `inn', `inns', `dff',
`dfa' and `ill' - violate the convention on prefixes. Except for
them, all the names of functions, variables and faces in eev
start with either \"ee\", or \"find-\", or \"code-c-d\"."
  (interactive)
  (defalias 'to   'ee-to)
  ;; (defalias 'back 'ee-back)		; oops - I removed `back'
  (defalias 'inn  'ee-inn)
  (defalias 'inns 'ee-inns)
  (defalias 'dff  'ee-dff)
  (defalias 'dfa  'ee-dfa)
  (defalias 'ill  'ee-ill))

;; (defun ee-invade-global-keymap ())
;; (defun ee-invade-global-menu-bar ())	; I removed the menu code


;;;
;;; aliases for compatibility with previous versions
;;;

(defalias 'ee-invade-global-namespace 'eev-set-aliases) 

(defalias 'ee-substitute-in-file-name 'ee-expand)
(defalias 'find-availablegrep 'find-grep-available)
(defalias 'find-progoutput 'find-sh)

(defalias 'highlight-temporarily         'eeflash)  ; bad prefix!
(defalias 'highlight-temporarily-by-spec 'eeflash+) ; bad prefix!


(provide 'eev)



;; Is it ok to put `require's at the end of elisp files?
;; I've just split `eev-steps' from this file...
;; same for `eev-bounded-old'.

;; (require 'eev-bounded-old)		; (find-eev "eev-bounded-old.el")
(require 'eev-bounded)			; (find-eev "eev-bounded.el")
(require 'eev-steps)			; (find-eev "eev-steps.el")



;; `ee-ill' needs another name.
;; (find-efunction 'ee-ill)
;; (global-set-key "\M-I" 'ee-ill)

;; (find-elnode "Active Keymaps")
;; overriding-local-map
;; (current-local-map)




;; Local Variables:
;; mode:              outline-minor
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";;"
;; no-byte-compile:   t
;; End:
