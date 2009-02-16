;;; cpcomment - comment maintenance for c

;;;
;;; Copyright (C) 1994, 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: $Id: cpcomment.el,v 1.3 1996/05/23 00:38:57 zappo Exp $
;;; Keywords: c, comment
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;

;;; Commentary:
;;;
;;; Entry points:
;;;   cpc-insert-function-comment - Create comments for functions.
;;;   cpc-insert-new-file-header - Create comments for tops o files
;;;
;;;   The GNU coding standards claims that comments are to be written
;;; in certain ways in association with functions, variables,
;;; functins, etc.  These emacs functions are designed to aid in
;;; making all that documentation gobbldygook easier to manage.  The
;;; functionality takes the form of one callable function called
;;; cpc-insert-function-comment, which inserts or modifies a comment
;;; appearing just before the function the cursor is on at any given
;;; moment.  Through completely configurable format statements, any
;;; comment type can be created, and through alists of regular
;;; expressions, a BEST GUESS at how to comment a function is handled.
;;; The comments in question provide format tokens for the function
;;; name, description location, return values, parameter lists, and
;;; history elements.  Parameters in turn have tokens for the name,
;;; type, and auto-comment.  History elements have tokens for the
;;; user, time, change-id, and auto-comment.  The time has tokens to
;;; create a date string of any format I could think of.
;;;
;;;   The auto-comment lists provides alists matching some regular
;;; expression to a comment to be used when there is match, such that
;;; if the function is : struct goober alloc_goober(stuff), the
;;; auto-comment will return: Allocates and initializes a new goober
;;;
;;;                          IMPORTANT
;;;
;;;   The purpose of the autocomment code is NOT to write perfect
;;; english comments!  It is supposed to provided all easilly
;;; descernable words (verbs, nouns, adjectives, etc) in some
;;; sembalance of order so the programmer who is managing the comments
;;; need write them less often.  "M-D" is easier to type than "object"
;;; should extra words be guesstamated.  If a syntax god would like to
;;; play with this to create better sentances, or a C god whould like
;;; to create source parsers to turn source into english, be my guest.
;;;
;;;                         IMPORTANT # 2
;;;
;;;   Even though the comments created by this function are designed to
;;; aid in the upkeep of the coding standard, they are not perfect!
;;; For example, the coding standard does not talk about specifically
;;; listing your variables in comments, claims you need not put the
;;; functions name into the comment, and a couple other things I
;;; happily do for you, simply because it takes no effort now.  In
;;; addition, the comment patterns were written in accordance with the
;;; way I must write comments where I work.
;;;
;;; History
;;; eml 9/23/94
;;; Added some extra parts to some autocomment lists.
;;;

(require 'cparse)			;parsing routines
(require 'sformat)			;formatter

(defvar cpc-my-initials (user-login-name)
  "*The initials to use when commenting in bits and peices.")

(defvar cpc-copyright-holder (user-full-name)
  "*The string to use as the copyright holder in the file comment.")

(defvar cpc-copyright-notice-file nil
  "*A file name describing where a plain formatted text copyright
notice lives which will be then reformatted in the header to have the
correct prefix character.  See the %N token in cpc-file-comment")

(defvar cpc-current-spr nil
  "*If you work for a company that requires change report numbers,
this is set for the SPR number.")

(defvar cpc-extra-line-after-short-parameters t
  "*Set to t if an extra line is inserted when the list of parameters
is 1 or less.")

(defvar cpc-spr-number nil
  "*Set this when working on some top-level change and modify
cpc-history-element to include %S, and this string will be inserted.")

(defvar cpc-runflags nil
  "List of flags set during modification of a comment to aid in the
creation of the History comment.")

(defvar cpc-comment-left-edge " * "
  "The string appearing down the side of a comment.")

(defvar cpc-file-comment "/* %B
 *
 * Copyright (C) %Y %O
 *
 * %N
 * 
 * Description:
 * 
 *   %D
 * 
 * History:
 * %H
 *
 * Tokens: %T
 */
"
  "This is the comment block to put in front of a new C file which
should describe briefly what the file contains.  The format tokens
available are:
 %B - Breif description of the file (Auto-comment by file name)
 %D - Made up documentation
 %N - Copyright notice for your organization
 %O - Owner (full name of copyright holder held in cpc-copyright-holder
 %H - History elements
 %T - cproto header id token.  Always is form 'token file.h' where
      token is defined in cpr-header-token, and file.h is the
      relational file name of the header.  If you wish to use cproto's
      features, you must have this somewhere in the header.
 %Y - Year
")

(defvar cpc-header-comment "/*
 * Copyright (c) %Y %O
 *
 * %N
 * 
 * History:
 * %H
 */
"
  "This is the comment block to put in front of a new C file which
should describe briefly what the file contains.  The format tokens
available are the same as for cpc-file-comment")

(defvar cpc-file-brief-comment "%F - %C"
  "Format of the brief comment with tokens.
 %F - file name short
 %C - Comment field")

(defvar cpc-function-comment "
/*
 * Function: %F
 *
 * %f  %D%p
 *
 * Returns:     %R
 * Parameters:  %P
 * History:
 * %H
 */
"
  "Comment block describing what you want in the beginning of each
function.  There are several format tokens represent the following:
  %F - function name
  %D - Made up documentation
  %f - Place, where everything before the point is the distance to set
       in the fill prefix.  Allows a first line in paragraph indent
  %p - Where to place point after insertion of a new header
  %R - Returns
  %P - Parameter list
  %H - History insertion point

  The parts %f and %p can be confusing, so here is an example:

 * Moose: %f%D%p

 Will set fill prefix to ` *         `, and put the point AFTER the
description.  The `Moose:` will not be in the prefix.  The default
value shows the equivalent of a hanging indent.
")

(defvar cpc-param-element "%P - %D"
  "The format of a parameter element in the list of parameters. The
parts are:
 %P - Parameter name spaced to length of max param
 %p - Parameter name with no padding
 %R - Type of element
 %D - Description of parameter as known by cpc.")

(defvar cpc-history-element "%-7U %-10D %C"
  "Sformatable string which represents a history element.  Valid %
codes are:
  %U - Username, initials, what have you.
  %D - The current date formatted as in cpc-date-element
  %S - System Change id, SCCS vers, major change comment, etc
  %C - Auto comment area, cursor goes here for new elts.")

(defvar cpc-date-element "%M/%D/%y"
  "Sformatable string which represents how the date element in the
history element is displayed.  Valid format chars are:
  %H - Hours
  %h - Hours 24 hr format
  %a - AM/PM flag
  %I - mInutes
  %S - Seconds
  %D - Day
  %w - Weekday string
  %M - Month as number
  %m - Month as string
  %Y - Year
  %y - Year as 2 numbers 1994 -> 94")

(defvar cpc-autocomment-function-alist
  '(
    ("abort" . "Aborts the")
    ;; trick to get re-alloc and alloc to pair into one sentence.
    ("realloc" . "moves or ")
    ("alloc\\(ate\\)?" . "Allocates and initializes a new ")
    ("clean" . "Cleans up the")
    ("clobber" . "Removes")
    ("close" . "Cleanly closes")
    ("check" . "Checks the")
    ("comp\\(are\\)?" . "Compares the")
    ("create" . "Creates a new ")
    ("find" . "Finds ")
    ("free" . "Frees up space")
    ("gen\\(erate\\)?" . "Generates a new ")
    ("get\\|find" . "Looks for the given ")
    ("gobble" . "Removes")
    ("he?lp" . "Provides help for")
    ("li?ste?n" . "Listens for ")
    ("connect" . "Connects to ")
    ("acc?e?pt" . "Accepts a ")
    ("load" . "Loads in ")
    ("match" . "Check that parameters match")
    ("name" . "Provides a name which ")
    ("parse" . "Parses the parameters and returns ")
    ("print\\|display" . "Prints out")
    ("read" . "Reads from")
    ("reset" . "Resets the parameters and returns")
    ("scan" . "Scans the ")
    ("setup\\|init\\(iallize\\)?" . "Initializes the ")
    ("select" . "Chooses the ")
    ("send" . "Sends a")
    ("re?c\\(v\\|ieves?\\)" . "Receives a ")
    ("wait" . "Waits for ")
    ("write" . "Writes to")
    )
  "List of names to string match against the function name.  Certain
prefixes may always mean the same thing, and the same comment can be used
as a beginning for the description.  RXPs should be lower case since
the string they are compared to is downcased.  A string may end in a
space, in which case, last-alist is searched to see how best to
describe what can be returned.  Doesn't always work correctly, but
that is just because English doesn't always work correctly.")

(defvar cpc-autocomment-common-nouns-abbrevs
  '(
    ("sock\\(et\\)?" . "socket")
    ("addr\\(ess\\)?" . "address")
    ("buf\\(f\\(er\\)?\\)?" . "buffer")
    ("cur\\(r\\(ent\\)?\\)?" . "current")
    ("dev\\(ice\\)?" . "device")
    ("file" . "file")
    ("line" . "line")
    ("msg\\|message" . "message")
    ("name" . "name")
    ("next\\|nxt" . "next")
    ("port" . "port")
    ("host" . "host")
    ("obj\\|object" . "object")
    ("previous\\|prev" . "previous")
    ("str\\(ing\\)?" . "string")
    ("use?r" . "user")
    ("num\\(ber\\)?" . "number")
    ("\\(^\\|\\s-\\)id\\($\\|\\s-\\)" . "Identifier") ;complex cause ;commen sylable
    )
  "List of common abbreviations or full words of things (as opposed to
verbs) for use in creating Englified versions of variable and function
names.")

(defvar cpc-autocomment-return-first-alist
  '(
    ;; Static must be first in the list to provide the intro to the sentence
    ("static" . "Locally defined function which ") 
    ("Bool\\|BOOL" . "Status of ")
    )
  "List of type regexps associated with comment fields which will supply a
small amount of prepended text to the comments of the functin based on
return value.")

(defvar cpc-autocomment-return-last-alist
  '(
    ("static[ \t\n]+struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("struct \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("union \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("enum \\([a-zA-Z0-9_]+\\)" . "%s")
    ("static[ \t\n]+\\([a-zA-Z0-9_]+\\)" . "%s")
    ("\\([a-zA-Z0-9_]+\\)" . "of type %s")
    )
  "List of regexps which provide the type of the return value.  The
strings struct and union are not needed when appened into a comment,
so they are removed.  The %s provides a simple way to include the
parsed part into the string.  (This one only).  If you know what
something is, just say '(\"userobj\" . \"user object\")")

(defconst cpc-autocomment-param-alist 
  '( ("[Cc]txt" . "Context")
     ("[Ii]d" . "Identifier of")
     ("[Tt]ype" . "Type of")
     ("[Nn]ame" . "Name of")
     ("argc" . "Number of arguments")
     ("argv" . "Argument vector")
     ("envp" . "Environment variable vector")
     )
  "Alist of potential parameters which are always the same in the current
project.  When one is encountered, cpc-insert-parameters will automatically
place this comment after the parameter name.")

(defconst cpc-autocomment-param-type-alist
  '(("const" . "Constant")
    ("void" . "Empty")
    ("char[ ]*\\*" . "String ") 
    ("\\*" . "Pointer ") 
    ("char[ ]*\\([^ \t*]\\|$\\)" . "Character")
    ("int\\|long" . "Number of")
    ("FILE" . "File of")
    ("float\\|double" . "Value of")
    ;; How about some X things?
    ("Bool\\|BOOL" . "Flag")
    ("Window" . "Window")
    ("GC" . "Graphic Context")
    ("Widget" . "Widget")
    )
  "Alist of potential input parameter types and the corresponding
strings to place after the description of said type.  If there are
modifiers to the type, place then first in the list so the strings
come out correctly. (ie. Constant Number of <things I need>) If there is
a result from cpc-autocomment-param-alist then this isn't referenced.")

(defvar cpc-new-hist-comment "Created"
  "String used as a comment in a history element which is used in a
function comment being created.")

(defvar cpc-autocomment-modify-alist
  '((cpc-newparam . "%s")
    )
  "List of symbols set in cpc-runflags and strings associated with
them.  When the comment is being checked over for new information,
these symbols are set, and the associated comment with them.  The %s
will be filled with the associated string created at that time.")

(defun cpc-insert-function-comment ()
  "Starting at pnt, look for a function definition.  If the definition
exists, parse for the name, else, fill everything in as null.  Then
insert the variable cpc-function-comment, and fill in the %s with the
parts determined.
If the comment already exists, this function will try to update only
the HISTORY part."
  (interactive)
  (let ((tv (cparse-on-major-sexp))
	(cpc-runflags nil))
    (cond
     ((equal (cparse-type tv) 'fndef)
      (if (and (vectorp tv) (cpc-have-comment tv))
	  (progn
	    (save-excursion
	      (cpc-update-paramlist (cpc-have-comment tv) (cparse-readparams tv)))
	    (cparse-toplevel (buffer-name))
	    (setq tv (cparse-on-major-sexp))
	    (cpc-update-history (cpc-have-comment tv) (cpc-get-history-elt "")))
	(cpc-insert-function-comment-new tv))
      (message "Done..."))
     ((or (equal (cparse-type tv) 'defun)
	  (equal (cparse-type tv) 'defvar))
      (error "DEFUNS and DEFVARS comment themselves!"))
     ((equal tv nil)
      (error "You are not on a function definition."))
     (t
      (error "Type %s is not managed by cpc autocomment."
	     (cparse-type tv))))))

(defun cpc-insert-function-comment-new (tv)
  "Insert a new comment which explains the function found in TV"
  (let ((hist (cpc-get-history-elt ""))
	(pnt 0)
	(upnt 0)
	(st 0)
	(zpnt 0)
	(fname nil)
	(returns nil)
	(params nil)
	)
    ;; tv should always be correct.
    (goto-char (cparse-start tv))
    (setq st (point))
    (save-restriction
      (narrow-to-region (cparse-start tv) (cparse-end tv))
      (goto-char (point-min))
      (if (re-search-forward (concat "\\(" cparse-typedef-regexp "\\)"))
	  (setq returns (cparse-trim-word
			 (buffer-substring (match-beginning 1) (match-end 1)))))
      (setq fname (cparse-name tv))
      (setq params (cparse-readparams tv)))
    (goto-char st)
    (insert (Sformat (list (list ?F fname)
			   (list ?f '(lambda () (setq zpnt (Sformat-point)) ""))
			   (list ?p '(lambda () (setq pnt (Sformat-point)) ""))
			   (list ?D (cpc-function-name-comment fname returns))
			   (list ?R (cpc-insert-return returns))
			   (list ?P '(lambda () (cpc-insert-parameters params)))
			   (list ?H (concat hist cpc-new-hist-comment)))
		     cpc-function-comment))
    (goto-char (+ zpnt st))
    (message "Setting fill prefix to: \"%s\""
	     (setq fill-prefix (substring " *                                   "
					  0 (current-column))))
    (goto-char (+ pnt st))
    (auto-fill-mode 1)
    )
  )

(defun cpc-function-name-comment (fname retval)
  "Take the function name, and estamate a good comment to use based on
a regular expression list.  If we can identify a verb in the list,
followed, potentially by some name part then check the return value
to see if we can use that to finish off the sentence. ie.  Any
function with 'alloc' in it will be allocating something based on
it's type."
  (let ((al cpc-autocomment-return-first-alist)
	(dropit nil)
	(tailit nil)
	(news ""))
    ;; check for modifiers like static
    (while al
      (if (string-match (car (car al)) (downcase retval))
	  (progn
	    (setq news (concat news (cdr (car al))))
	    (setq dropit t)
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for verb parts!
    (setq al cpc-autocomment-function-alist)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news 
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    ;; if we end in a space, then we are expecting a potential
	    ;; return value.
	    (if (= ?  (aref news (1- (length news))))
		(setq tailit t))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; check for noun parts!
    (setq al cpc-autocomment-common-nouns-abbrevs)
    (while al
      (if (string-match (car (car al)) (downcase fname))
	  (progn
	    (setq news 
		  (concat news (if dropit (downcase (cdr (car al)))
				 (cdr (car al)))))
	    (setq al nil)))
      (setq al (cdr al)))
    ;; add tailers to names which are obviously returning something.
    (if tailit
	(progn
	  (setq al cpc-autocomment-return-last-alist)
	  (while al
	    (if (string-match (car (car al)) (downcase retval))
		(progn
		  (setq news 
			(concat news " "
				;; this one may use parts of the return value.
				(format (cdr (car al))
					(cpc-programmer->english
					 (substring retval (match-beginning 1)
						    (match-end 1))))))
		  (setq al nil)))
	    (setq al (cdr al)))))
    news))

(defun cpc-insert-parameters (params &optional commentlist)
  "Convert a parameter list of PARAMS into a vertical list seperated
by -es.  Optional COMMENTLIST is a list of previously know parts."

  (let* ((col (if Sformat-formatting (Sformat-column) (current-column)))
	 (newl params)
	 ;; returns is local to the caller
	 (longest (cpc-longest-name newl))
	 (numdfs 0)
	 (newp ""))
    (while newl
      (if (equal (cparse-type (car newl)) 'vardef)
	  (let* ((n (car newl))
		 (nl (cparse-name n))
		 (al (cparse-array n))
		 ttv)
	    (if (not (listp nl))
		(setq nl (list nl)))
	    (while nl
	        (setq ttv (cparse-makev 'vardef
					     nil
					     (car nl)
					     (cparse-start n)
					     (cparse-end n)
					     (list (car al))
					     ))
		(aset ttv 5 (aref n 5))
		(setq numdfs (1+ numdfs))
		(let ((nextp (Sformat 
			      (list (list ?P 
					  (substring (concat 
						      (cparse-name ttv)
						      "                   ")
						     0 longest))
				    (list ?p (cparse-name ttv))
				    (list ?R (cparse-trim-word (aref ttv 5)))
				    (list ?D (cpc-parameter-comment 
					      ttv (cdr newl)
					      commentlist)))
			      cpc-param-element)))
		  (setq newp 
			(concat 
			 newp nextp
			 ;; the following always assumes that there is
			 ;; always a comment starting with SPC * on
			 ;; every line.  Mabee fix, but this is what I
			 ;; use, so tough noogies as of right now.
			 (if (or (cdr nl)
				 (cpc-more-defs newl 'vardef))
			     (concat "\n" cpc-comment-left-edge
				     (substring
				      "                                 "
				      0 (- col 3)))))))
		(setq nl (cdr nl)
		      al (if al (cdr al) nil)))))
      (setq newl (cdr newl)))
    (if (= (length newp) 0) (setq newp "None"))
    (if (and cpc-extra-line-after-short-parameters (<= numdfs 1))
	(setq newp (concat newp "\n *")))
    newp)
  )

(defun cpc-parameter-comment (param list &optional commentlist)
  "Take the PARAM, the parameter description vector, and create a
suitable comment about it, or at least, the prefix of a comment.  Also
looks at LIST, and if the next elt is a comment, then use that comment
text. Optional COMMENTLIST is list of previously existing comments to
use instead in alist form.  If the name doesn't appear in the list of
standard names, then englishafy it. instead."
  (let ((cmt "")
	(aso nil)
	(fnd nil))
    (if (setq aso (assoc (cparse-name param) commentlist))
	(setq cmt (cdr aso))
      (if (and (/= (length cmt) 0)
	       (equal (cparse-type (car list)) 'comment))
	  (save-excursion
	    (let ((comt (car list)) b e)
	      (goto-char (cparse-start comt))
	      (re-search-forward "/\\*\\s-*" (cparse-end comt) t)
	      (setq b (point))
	      (re-search-forward "\\(\\s-*\\)\\(\\*/\\|$\\)" (cparse-end comt) t)
	      (setq e (match-beginning 1))
	      (setq cmt (buffer-substring b e))))
	(setq aso cpc-autocomment-param-alist)
	(while aso
	  (if (string-match (car (car aso)) (cparse-name param))
	      (progn
		(setq fnd t)
		(setq cmt (concat cmt (cdr (car aso))))))
	  (setq aso (cdr aso)))
	(if (/= (length cmt) 0)
	    nil
	  ;; finally check for array parts
	  (if (car (cparse-array param))
	      (setq cmt (concat cmt "array of ")))
	  (setq aso cpc-autocomment-param-type-alist)
	  (while aso
	    (if (string-match (car (car aso)) (aref param 5))
		(setq cmt (concat cmt (cdr (car aso)))))
	    (setq aso (cdr aso)))
	  )
	(if (not fnd) (setq cmt (concat cmt " " (cpc-programmer->english
						 (cparse-name param)))))))
    cmt))

(defun cpc-more-defs (list type)
  "Scan LIST for more parse elements of type TYPE, and return t if there
are some."
  (let ((res nil))
    ;; advance one since we must skip this guy.
    (setq list (cdr list))
    (while list
      (if (equal (cparse-type (car list)) type)
	  (progn
	    (setq list nil)
	    (setq res t)))
      (setq list (cdr list)))
    res))

(defun cpc-longest-name (list)
  "Go through LIST, and return the length of the longest name."
  (let ((longest 1)
	(nl nil))
    (while list
      (setq nl (cparse-name (car list)))
      (if (not (listp nl)) (setq nl (list nl)))
      (while nl
	(if (< longest (length (car nl)))
	    (setq longest (length (car nl))))
	(setq nl (cdr nl)))
      (setq list (cdr list)))
    longest))

(defun cpc-insert-return (returnval)
  "Take the return value, and return a string which is ready to be
commented."
  (if (string-match "^void\\|^VOID" returnval)
      "Nothing"
    (if (= (length returnval) 0)
	"int - "
      (concat returnval " - "))))

(defun cpc-insert-new-file-header (&optional header)
  "Insert a new comment describing this function based on the format
in the variable cpc-file-comment.  It is a string with sformat tokens
for major parts.  Optional HEADER is the header to use for the cpr
token"
  (interactive)
  (goto-char 0)
  (let ((pnt nil))
    (insert (cpc-new-file-header header))
    (if pnt
	(goto-char pnt))))

(defun cpc-new-file-header (&optional header)
  "Return a string which is the newly created file header comment
string for the current buffer.  Optional HEADER is the header file to
use under Token."
  (Sformat (list (list ?B '(lambda () (cpc-file-brief-comment)))
		 (list ?D 
		       (if (boundp 'pnt)
			   '(lambda () (setq pnt (Sformat-point)) "")
			 ""))
		 (list ?N '(lambda () 
			     (cpc-copyright-notice cpc-comment-left-edge)))
		 (list ?O cpc-copyright-holder)
		 (list ?Y (cpc-get-date-time-string "%Y"))
		 (list ?T '(lambda ()
			     (concat cpr-header-token
				     " "
				     (if header header
				       (cpr-find-header)))))
		 (list ?H (cpc-get-history-elt "Created")))
	   (if (string-match "\\.c$" (buffer-file-name))
	       cpc-file-comment
	     cpc-header-comment)))

(defun cpc-set-copyright-file (f)
  "Interactivly find the file name which contains the copyright blurb."
  (interactive "FCopyright Notice File (RET for none): ")
  (if (string= f (buffer-file-name))
      (setq cpc-copyright-notice-file "")
    (setq cpc-copyright-notice-file f)))

(defun cpc-copyright-notice (prefix)
  "Get the copyright notice from sources commented in various
variables and return a string with that stuff in it.  Because notices
are usually more than one line, put PREFIX before each line."
  (if (not cpc-copyright-notice-file)
      (call-interactively 'cpc-set-copyright-file))
  (if (= (length cpc-copyright-notice-file) 0)
      "??Public Domain Software??"
    (let* ((b (get-buffer-create "CPC TEMP"))
	   (s nil)
	   (plen (Sformat-column))
	   (pstr (substring (concat cpc-comment-left-edge "         ")
			    0 plen)))
      (setq s 
	    (save-excursion 
	      (set-buffer b) 
	      (insert-file-contents cpc-copyright-notice-file)
	      ;; Now put comment marks all over.
	      (goto-char 0)
	      (forward-line 1)
	      (end-of-line)
	      (while (not (eobp))
		(beginning-of-line)
		(insert pstr)
		(end-of-line)
		(forward-char 1)
		(end-of-line))
	      (forward-char -1)
	      (if (equal (following-char) ?\n)
		  (delete-char 1))
	      (set-buffer-modified-p nil)
	      (buffer-string)))
      (kill-buffer b)
      s)))

(defun cpc-file-brief-comment ()
  "Make a brief comment about the file we are currently editing."
  (Sformat (list (list ?F (file-name-nondirectory (buffer-file-name)))
		 (list ?C '(lambda () 
			    (read-string "Breif Description of file: "))))
	   cpc-file-brief-comment))

(defun cpc-get-history-elt (changes)
  "Create a string representing a history element with the changes elt
set to CHANGES."
  (Sformat (list '(?U cpc-my-initials)
		 (list ?D (cpc-get-date))
		 '(?S cpc-spr-number)
		 '(?C changes))
	   cpc-history-element))

(defun cpc-get-date-time-string (form)
  "Return a string matching the format of cpc-date-element"
  (let* ((date (current-time-string))
         (garbage
          (string-match
           (concat "^\\([A-Z][a-z]*\\) *\\([A-Z][a-z]*\\) *\\([0-9]*\\)"
	   " \\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\)"
	   " \\([0-9]*\\)$")
           date))
	 (wkdy (substring date (match-beginning 1) (match-end 1)))
	 (hour (string-to-int 
		(substring date (match-beginning 4) (match-end 4))))
	 (min (substring date (match-beginning 5) (match-end 5)))
	 (sec (substring date (match-beginning 6) (match-end 6)))
         (month 
	  (cdr (assoc (substring date (match-beginning 2) (match-end 2))
		      '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
			("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
			("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))
	 (ms (substring date (match-beginning 2) (match-end 2)))
	 (day (substring date (match-beginning 3) (match-end 3)))
         (year (substring date (match-beginning 7) (match-end 7))))
    (Sformat (list (list ?H (% hour 12))
		   (list ?h hour)
		   (list ?a (if (> hour 12) "pm" "am"))
		   (list ?I min)
		   (list ?S sec)
		   (list ?D day)
		   (list ?M month)
		   (list ?m ms)
		   (list ?Y year)
		   (list ?y (substring year 2))
		   (list ?w wkdy))
	     form)))

(defun cpc-get-date ()
  "Return a string which is the current date."
  (cpc-get-date-time-string cpc-date-element))

(defun cpc-update-history (v h)
  "Look in the comment described in V, locate %H descriptor, and add
new line with history element H on it."
  (let ((endpos 0))
    (save-excursion
      (goto-char (cparse-end v))
      (if (not (re-search-backward "/\\*" (cparse-start v) t))
	  (error "Comment confuses me."))
      (let ((s (cpc-just-after-token-regexp ?H cpc-function-comment)))
	(if (not s) (error "Can't find where to enter new history elt."))
	(re-search-forward (concat "\\(" s "\\)")
			   (1+ (cparse-end v)) t)
	(goto-char (match-beginning 1))
	(insert (concat "\n" cpc-comment-left-edge))
	(insert h)
	(setq endpos (point))))
    (goto-char endpos)
    (while cpc-runflags
      (let ((p (assoc (car (car cpc-runflags)) cpc-autocomment-modify-alist)))
	(if p (insert (format (cdr p) (cdr (car cpc-runflags))))))
      (setq cpc-runflags (cdr cpc-runflags)))))

(defun cpc-update-paramlist (v l)
  "Look in the comment described in V, locate %P descriptor, and check
against list in L to verify that all elts are in the correct order,
and accounted for."
  (let ((endpos 0) st en (il nil)
	(case-fold-search nil))
    (save-excursion
      (goto-char (cparse-start v))
      (let ((s (cpc-just-after-token-regexp ?P cpc-function-comment))
	    (s2 (cpc-just-before-token-regexp ?P cpc-function-comment)))
	(if (or (not s) (not s2))
	    (error "Cannot break format string into findable begin and end tokens."))
	(if (not (re-search-forward (concat "\\(" s "\\)")
				    (1+ (cparse-end v)) t))
	    (error "Comment is not formatted correctly for param check."))
	(goto-char (match-beginning 1))
	(setq en (point))
	(goto-char (cparse-start v))
	(if (not (re-search-forward s2 (cparse-end v) t))
	    (error "Comment is not formatted correctly for param check."))
	(setq st (point))
	;; At this point we have the beginning and end of the
	;; parameter list in the comment.  Now lets search through
	;; it and generate a list (name . commentpart) so we can
	;; re-build it if it doesn't match L
	(while (re-search-forward 
		(concat "\\(" cparse-symbol-regexp "\\)\\s-*-[ \t]*")
		en t)
	  (let ((n (buffer-substring (match-beginning 1) (match-end 1)))
		(c nil))
	    (setq c (point))
	    (re-search-forward "$" (cparse-end v) t)
	    (setq c (buffer-substring c (point)))
	    (setq il (cons (cons n c) il))))
	;; run verify on two lists of parameters to make sure they
	;; are the same.
	(let ((tl (cparse-expand-list l)) (stop nil))
	  ;; zap non vardefs
	  (while (and tl (not (equal (cparse-type (car tl)) 'vardef)))
	    (setq tl (cdr tl)))
	  (while (and tl (not stop))
	    (if (not (assoc (cparse-name (car tl)) il))
		(setq stop t))
	    ;; inc and zap non vardefs.
	    (setq tl (cdr tl))
	    (while (and tl (not (equal (cparse-type (car tl)) 'vardef)))
	      (setq tl (cdr tl))))
	  (if (not stop)
	      (setq il nil)))
	;; check if we want to modify the parameter list.
	(if (not (and il
		      (y-or-n-p "Paramter list changed.  Fix?")))
	    (message "Not fixing.")
	  ;; delete what was there, and insert the new stuff.
	  (let ((ntl (cparse-expand-list l))
		(cs1 nil)
		(num 0))
	    (while ntl
	      (if (not (assoc (cparse-name (car ntl)) il))
		  (progn 
		    (setq num (1+ num))
		    (setq cs1 (concat cs1 (if cs1 ", ") (cparse-name (car ntl))))))
	      (setq ntl (cdr ntl)))
	    (if cs1 
		(if (= num 1)
		    (setq cs1 (concat "Added parameter " cs1))
		  (setq cs1 (concat "Added parameters " cs1)))
	      (setq cs1 "Removed parameters."))
	    (setq cpc-runflags (cons (cons 'cpc-newparam cs1) cpc-runflags)))
	  (let ((dif (- en st))
		(newc nil))
	    (delete-region st en)
	    (setq newc (cpc-insert-parameters l il))
	    (setq dif (- (length newc) dif))
	    (insert newc)
	    ;; update the vector describing the comment
	    (aset v 4 (+ (cparse-end v) dif))))))
    (goto-char endpos)))

(defun cpc-have-comment (v)
  "Look for the comment relating to the function found in V, and
return the comment."
  (let ((c (cpc-find-function-comment (cparse-start v))))
    (if (and c (cpc-comment-refers-to c (cparse-name v)))
	c
      nil)))

(defun cpc-comment-refers-to (v fn)
  "Check comment described in part V to see if it refers to function
FN."
  (save-excursion
    (goto-char (cparse-start v))
    (re-search-forward fn (cparse-end v) t)))

(defun cpc-find-function-comment (pnt)
  "Starting at point, goto beginning of comment belonging to this
function."
  (cparse-toplevel (buffer-name))
  (let ((l cparse-mainlist)
	(last nil))
    (while (and l (> pnt (cparse-end (car l))))
      (setq last  (car l))
      (setq l (cdr l)))
    (if (and l (equal (cparse-type last) 'comment))
	last)))

(defun cpc-programmer->english (varname)
  "Takes VARNAME and converts it into English.  Works with the
following rules. 
  1) convert all _ into spaces.
  2) inserts spaces in front of all lowerUpper case combos
  3) expands noun names based on common programmer nouns.
  
  This function is designed for Variables, not functions.  This does
not account for verb parts.
"
  (let ((ind 0)				;index in string
	(llow nil)			;lower/upper case flag
	(wlist nil)			;list of words after breaking
	(newstr nil)			;new string being generated
	(al nil))			;autocomment list
    ;;
    ;; 1) Convert underscores
    ;;
    (while (< ind (length varname))
      (setq newstr (concat newstr
			   (if (= (aref varname ind) ?_)
			       " " (char-to-string (aref varname ind)))))
      (setq ind (1+ ind)))
    (setq varname newstr
	  newstr nil
	  ind 0)
    ;;
    ;; 2) Find word brakes between case changes
    ;;
    (while (< ind (length varname))
      (setq newstr 
	    (concat newstr
		    (let ((tc (aref varname ind)))
		      (if (and (>= tc ?a) (<= tc ?z))
			  (progn
			    (setq llow t)
			    (char-to-string tc))
			(if llow 
			    (progn
			      (setq llow nil)
			      (concat " " (char-to-string tc)))
			  (char-to-string tc))))))
      (setq ind (1+ ind)))
    ;;
    ;; 3) Expand the words if possible
    ;;
    (setq llow nil
	  ind 0
	  varname newstr
	  newstr nil)
    (while (string-match (concat "^\\s-*\\([^ \t\n]+\\)") varname)
      (let ((ts (substring varname (match-beginning 1) (match-end 1)))
	    (end (match-end 1)))
	(setq al cpc-autocomment-common-nouns-abbrevs)
	(setq llow nil)
	(while al
	  (if (string-match (car (car al)) (downcase ts))
	      (progn
		(setq newstr (concat newstr (cdr (car al))))
		;; don't terminate because we may actuall have 2 words
		;; next to eachother we didn't identify before
		(setq llow t)))
	  (setq al (cdr al)))
	(if (not llow) (setq newstr (concat newstr ts)))
	(setq newstr (concat newstr " "))
	(setq varname (substring varname end))))
    (cparse-trim-word newstr)))

;;; These two routines find the string between different % tokens, and
;;; returns them as regular expressions vie regexp-quote.  The result
;;; will allow a program to find text surrounding major parts within a
;;; comment, thus, the parts in a comment that need to be changed.

(defun cpc-just-before-token-regexp (token format)
  "Return a string which represents a search string which will
identify something before the character TOKEN in FORMAT"
  (let ((rs nil) (case-fold-search nil))
    (if (string-match (concat "\\(%" (char-to-string token) "\\)") format)
	(progn
	  (setq rs (substring format 0 (match-beginning 1)))
	  ;; scan for previous tokens and shorten
	  (while (string-match "\\(%\\)" rs)
	    (setq rs (substring rs (+ (match-end 1) 1))))
	  (regexp-quote rs))
      nil)))

(defun cpc-just-after-token-regexp (token format)
  "Return a string which represents a search string which will
identify something after the TOKEN in FORMAT"
  (let ((rs nil) (case-fold-search nil))
    (if (string-match (concat "\\(%" (char-to-string token) "\\)") format)
	(progn
	  (setq rs (substring format (match-end 1)))
	  (if (string-match "\\(%\\)" rs)
	      (setq rs (substring rs 0 (match-beginning 1))))
	  (regexp-quote rs))
      nil)))

;;; end of lisp
(provide 'cpcomment)
;;; require here to avoid recursive requires
(require 'cproto)
