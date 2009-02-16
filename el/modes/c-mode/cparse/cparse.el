;;; cparse - c programmer's aid to finding lost or hidden definitions.

;;;
;;; Copyright (C) 1994, 1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: $Id: cparse.el,v 1.10 1996/05/23 00:40:22 zappo Exp $
;;; Keywords: c
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
;;; Up to date copies of cparse can be found at:
;;; ftp://ftp.ultranet.com/pub/zappo/cparse*.tar.gz

;;;
;;; Major entry points:
;;;   cparse-toplevel - Just parse and save data locally.
;;;   cparse-listparts - initialize a PARTS buffer about the current file
;;;   cparse-on-major-sexp - prints name of major sexp you are in.
;;;   cparse-enclosing-expression - pull up PARTS buffer of inner SEXP
;;;                                 within a c function.
;;;   cparse-find-name - like cparse-open-on-line with query
;;;   cparse-open-on-line - takes string at pt, and checks each scope
;;;                          moving outwards.  Then checks all include
;;;                          files untill it is found.
;;;   cparse-load-header - header finding routine based on list of
;;;                        places a header may be found.
;;;   cparse-filedb-save - optionally save the db of saved parts.
;;;                        Currently, this is bad news, and can create
;;;                        really humungous files.
;;;
;;; This program has several tasks
;;;   1) parse out major expressions from c files
;;;   2) etags like functionality to find and blink names found in text.
;;;   3) browser list buffer
;;;   4) storing C parts (quesitionable functionality)
;;;   5) header hunting
;;;
;;; 1) Parsing
;;;    Take a file, and form a list of things and pointers associated
;;;    with it.  The routines will search for certain instances of
;;;    things, and return lists of tags into the file.
;;;
;;;    A typical form will be:
;;;
;;;    [ type string name start end datum1 array-part ]
;;;
;;;    where type is what it is (variable, comment, function, etc)
;;;    and start and end are integer pointers into the buffer.
;;;
;;;    The types that will be encountered are:
;;;
;;;    'comment 'inc-l 'inc-s 'define 'type 'typedef 'vardef
;;;    'function 'defun 'defvar
;;;
;;;    Where inc-* are includes with l->local and s->system.  This
;;;    differentiation aids in a search optimization.  'type will be
;;;    an enum, struct, or union, and a typedef can be a named struct.
;;;    A 'defun or 'defvar is a part of emacs source which is really a
;;;    function or variable definition but really baffles the hell out
;;;    of cparse.  Included because this of for use by GNU types.
;;;
;;; 2) Searching
;;;    Grab a name from a PARSED buffer, or c file text, and find that
;;;    name somewhere.  Searching uses the following algorithm:
;;;    0) Check against lists of key words and standard types.
;;;    1) Check each successive embeded block from the inside out,
;;;       such as: (where -!- is the cursor)
;;;       if(thing) {
;;;          int i;
;;;          for(i = 0; i<10; i++) {
;;;             int j;
;;;             j = i-!- + j;
;;;          }
;;;        }
;;;        It will try to find i, first by parsing in the for loop,
;;;        then coming up negative, searching the if loop. (Which
;;;        skips definitions after the first key word, for)
;;;    2) Check parameter list of function we are in (if we are in a
;;;       function)
;;;    3) If these fail, it searches the top-level parsing of the file
;;;       you are in.
;;;    4) Performs a breadth first search of all header files,
;;;       searching local headers (those in "double quotes") first
;;;       across multiple branches (list based searching). Breadth
;;;       first is more likely to find local definitions faster
;;;       because most c files order the includes with system level
;;;       includes first, and local's second.  The locals will be
;;;       included first, and breadth first makes sure the highest
;;;       level of structures are found first.
;;;    5) Gives up and says things
;;;
;;;    Names which are matches are include file names, defined
;;;    variables, variable names (in lists) structure names, typedef
;;;    names (listed with struct names when applicable) enum names,
;;;    enum parts, function names from prototypes and definitions.
;;;
;;; 3) Browser
;;;    The browswer will first list the toplevel definitions found in
;;;    a c file.  They are the parts listed above.  Comments default
;;;    to being turned off in this list, but can be re-activated with
;;;    the "c" key.  When the browser is first turned on, the cursor
;;;    is placed on the line that best matches where the cursor was in
;;;    the buffer being disected.  SPC will flash the definition the
;;;    cursor is on, and "f" will find the definition.  The "o" key
;;;    will do it's best to "open" the definition.  For types (structs
;;;    etc) the parts are parsed as "vardefs" and placed in a new
;;;    PARSED buffer.  For variables, and function prototypes, the
;;;    type is looked up (see search) and for include files, the file
;;;    in question is opened, and parsed, and the cursor placed in a
;;;    new PARSED buffer.
;;;
;;; 4) Storing
;;;    Storing C parts will occur in the file saved in the variable
;;;    cparse-filedb-rc when emacs is killed, or when the interactive
;;;    function cparse-filedb-save is called.  The hook
;;;    cparse-filedb-save-hook is called just before a save.
;;;    Setting the variable cparse-save-filedb to t must be done because
;;;    saving  defaults to nil.  Use of saving when using X is terribly
;;;    unpleasant to the size of the .cparse file.  
;;;
;;;    Since the files only need to be parsed once (when you first
;;;    need it) I feel this option should be left off, but is left in
;;;    for posterity.
;;;
;;; 5) Headers
;;;    Has a couple routines to find headers in the standard locations
;;;    where they can be found.  Special routine to find header in all
;;;    possible subdirectories of /usr/include called interactivly
;;;    only.
;;;
;;; It is entirly possible that this could form a framework around
;;; solving similar tasks for other languages.  Assembler, ada, and
;;; elisp all have "include" type functions, and the same sorta
;;; functional stuff.
;;;
;;; In terms of C++, whenever possible, the \s<char> expression has
;;; been used so the :: things will probably work since they are part
;;; of a symbol, but who can tell without trying.
;;;
;;;                         IMPORTANT
;;;
;;;   This code does not represent a real grammar based parser!  It is
;;; a complete fake!  It's job is to use regexps to identify all named
;;; objects within 1 scope level!  It always restricts itself as best
;;; it can to a certain scope for speed!  The chances of this code
;;; failing in a peice of code due to an unusual C context is
;;; completely possible!  If you feel this does not handle a case
;;; which it should, please let me know.
;;;
;;; History:
;;; 4/9/96 eml
;;; Attempted to handle array variables.
;;; changed buffer-substring to no longer grab colors

(defvar cparse-version 0
  "Version of cparse")

(defvar cparse-search-system t
  "* t if you wish cparse to go rummaging around in the system header
files, nil will only search local header files.")

(defvar cparse-noisy-p t
  "* t if you don't want cparse to print the PARSE: %% message in the
echo area while it's working.")

(defvar cparse-save-filedb nil
  "* t if you want to have the parsed data saved in a file.")

(defvar cparse-brief-find-file-hooks nil
  "*Special list of hooks used during a find file of include files
when done en-mass.  This solves slowness caused by hilit19, and really
weird stuff that can happen due to vc.")

(defvar cparse-query-depth 2
  "*Depth at which cparse queries if the users wishes to continue searching.")

(defvar cparse-include-list
  '( "/usr/include" "." ".." )
  "*List of directories where the pre-processor looks for include files.")

(defvar cparse-filedb-rc "~/.cparserc"
  "*File in which C parts are stored between sessions.")

(defvar cparse-filedb-save-hook nil
  "*Hook called just before c parts are being written to disk.")

(defvar cparse-depthmarker nil
  "Contains the depth of an active search.  Important for breadth
first search and the query depth.")

(defvar cparse-tag nil
  "Tag used when searching header files to prevent repetative searches.")

(defvar cparse-mainlist nil
  "(local) Main list of cparse elements in the current buffer.")

(defvar cparse-parsesize nil
  "(local) Cheezy checksum holding size of buffer since last parse.  If this
changes, we need to reparse.")

(defvar cparse-reference-buffer nil
  "(local) In PARSED mode, a pointer back to the buffer which we are
referencing.")

(defvar cparse-show-comments nil
  "When t, show the comment parts.")

(defvar cparse-filedb nil
  "Database of parsed file things.")

(defvar cparse-filedb-modified nil
  "Flag saying that saved DB is out of date or not.")

(defvar cparse-filedb-loaded nil
  "Flag saying that the DB has not been loaded yet.")

(defvar cparse-PARSE-keymap nil
  "Keymap used in PARSE buffer")

(defvar cparse-added-hook-p nil
  "t if hook for emacs-kill has been added.")

(if cparse-PARSE-keymap
    ()
  (setq cparse-PARSE-keymap (make-sparse-keymap))
  (define-key cparse-PARSE-keymap "c" 'cparse-toggle-comments)
  (define-key cparse-PARSE-keymap "g" 'cparse-update-p)
  (define-key cparse-PARSE-keymap "f" 'cparse-find)
  (define-key cparse-PARSE-keymap "o" 'cparse-open)
  (define-key cparse-PARSE-keymap "q" 'cparse-nuke-buffer)
  (define-key cparse-PARSE-keymap " " 'cparse-goto-line))

(defconst cparse-num-elts 7
  "Number of elements in a cparse reference vector.")

(defconst cparse-comment-regexp "\\(/\\*\\([^*\n]\\|*\\)*\\*/\\)"
  "Regexp to define a comment")
(defconst cparse-file-regexp "\\([--_A-Za-z.0-9/~$#^]+\\)"
  "This finds a file name part.  Not really accurate since files can
even contain whitespace, but hey, we have to put our foot down somewhere.")
(defconst cparse-symbol-regexp "\\(\\w\\|\\s_\\)+"
  "This indicates at least one symbol.")
(defconst cparse-optsymbol-regexp "\\(\\w\\|\\s_\\)*"
  "This indicates an optional symbol.")
(defconst cparse-struct-regexp "\\(struct\\|union\\|enum\\)"
  "This indicates a struct, union, or enum type (no name)")
(defconst cparse-modifiers-regexp 
  "\\(_export\\|register\\|const\\|extern\\|static\\|volitile\\|unsigned\\)"
  "Various modifiers to a given type, there can be more than one.
Technically, long is a modifier, but that fails long as the type which
happens more often.")
(defconst cparse-basic-type-regexp 
  "\\(void\\|char\\|short\\|int\\|long\\|float\\|double\\)"
  "Various builtin types.")
(defconst cparse-keywords-regexp
  "\\(if\\|else\\|return\\|do\\|while\\|for\\|switch\\|case\\|sizeof\\)"
  "Regular expression for keywords.")

(defconst cparse-arrayparts-regexp "[^]]"
  "Regexp to gobble up space in the declaration of arrays.  Here, just
warp to the end bracket.  We aren't doing code, just declarations,
and you can define the size of an array with the contents of a
variable, therefore, we needn't worry about embeded array references.")

(defconst cparse-dirty-whitespace
  (concat "\\(" cparse-comment-regexp "\\|\\s-\\|\n\\)" )
  "Regexp for white space which may have comments in it.")

(defconst cparse-typedef-regexp 
  (concat
   "\\(" cparse-modifiers-regexp "\\s-+\\)*" ;modifiers
   "\\(" cparse-struct-regexp "\\s-+\\)?" ;possible structure
   cparse-symbol-regexp ;symbol name of type
   "\\(\\s-*\\*\\s-*\\|\\s-+\\)")
  "Regexp for finding some TYPE.")
				 
(defconst cparse-optarray-regexp
  (concat "\\(\\s-*\\[\\s-*" cparse-arrayparts-regexp "*\\s-*\\]\\)*")
  "The parts of an array after a variable name, when there needn't be
anything between the brackets.")

(defconst cparse-isarray-regexp "\\s-*\\["
  "If the cursor is after a symbol, this regexp will only match if the
next part is an array part.")

(defconst cparse-anyvardef-regexp 
  (concat
   "\\(" cparse-symbol-regexp		;the name of the variable
   cparse-optarray-regexp		;array parts
   "\\s-*\\(=\\|,\\|;\\)\\)")           ;stuff after variable
  "Regexp for finding everything up to a symbol name when declaring a
variable.")
   
(defconst cparse-anyfndef-regexp
  (concat
   "\\(" cparse-symbol-regexp "\\)" ;the name of the function
   "\\s-*\\(_+P\\)?\\s-*("		;what is this optional P thing?
   )
  "Regexp for finding the declarations of functions.")

(defconst cparse-toplevel-regexp
  (concat "\\("
	  "/\\*\\|"			;comment beginning
	  "#\\s-*include\\|"		;include
	  "#\\s-*define\\|"		;define
	  "#\\s-*\\(if\\|else\\|endif\\|pragma\\)\\|" ;ifdef things -> skip!
	  "^DEFUN\\|"			;emacs source code id token
	  "DEFVAR_\\|"			;emacs source code id token
	  "\\(\\<typedef\\s-+\\)?" cparse-struct-regexp "\\s-+" 
	  cparse-optsymbol-regexp cparse-dirty-whitespace "*{\\|" ;struct dfn
	  "typedef\\s-+" cparse-typedef-regexp cparse-symbol-regexp "\\s-*;\\|"
	  "\\<" cparse-typedef-regexp "\\)")
  "Regular expression which matches the beginning of major expressions.")

;;;
;;; PARSING routines
;;;

;;; Make sure we know how to correctly grab text out of a buffer
(fset 'cparse-bss (if (fboundp 'buffer-substring-no-properties)
		      'buffer-substring-no-properties
		    'buffer-substring))

(defun cparse-toplevel (&optional name fast)
  "This function will attempt to parse the whole buffer into a list of
functions and other top level ids.  The types returned will only be at
the topmost level. (functions, inc-s and the like)  Optional FAST
means that as soon as a keyword is found, exit, because the searcher
is only looking for type definitions."
  (interactive)
  (if (not cparse-mainlist)
      (progn
	(make-local-variable 'cparse-mainlist)
	(make-local-variable 'cparse-parsesize)))
  (if (and cparse-parsesize (= cparse-parsesize (point-max))
	   cparse-mainlist)
      ()
    (setq cparse-mainlist nil)
    (setq cparse-parsesize (point-max))
    (let ((l nil)
	  (type nil)
	  (bs nil)
	  (n nil)
	  (v nil)
	  (s nil)
	  (e nil)
	  (c nil)
	  (array-here nil)
	  ;; Now define all regexps to be used...
	  (ifs "#\\s-*\\(if\\|else\\|endif\\|pragma\\)")
	  (incs "#\\s-*include")
	  (incs2 (concat "\\(#\\s-*include\\)\\s-+[<\"]" cparse-file-regexp 
			 "\\([>\"]\\)"))
	  (defines "#\\s-*define")
	  (defines2 (concat "\\(#\\s-*define\\)\\s-+\\(" cparse-symbol-regexp
			    "\\)"))
	  (keys1 (concat cparse-keywords-regexp "\\s-+"))
	  (mwhite (concat cparse-dirty-whitespace "*"))
	  (mword (concat "\\(" cparse-symbol-regexp "\\)"))
	  (pfun (concat "\\(typedef\\s-+\\)?" cparse-typedef-regexp 
			"(\\s-*\\*\\s-*" cparse-symbol-regexp "\\s-*[()]"))
	  (pfun2 (concat "(\\s-*\\*\\s-*\\(" cparse-symbol-regexp 
			 "\\)\\s-*\\((\\|)\\)"))
	  (pfun_param (concat cparse-dirty-whitespace "*\\(,\\|\\;\\)"))
	  (funs (concat "\\(" cparse-typedef-regexp "\\)?" 
			cparse-anyfndef-regexp))
	  (funs2 (concat "\\(" cparse-symbol-regexp "\\)\\s-*\\(_+P\\)?\\s-*("))
	  (funs3 (concat cparse-dirty-whitespace "*\\(,\\|\\;\\)"))
	  (structs (concat "\\(\\(typedef\\s-+\\)?\\)"
			   cparse-struct-regexp "\\s-+\\("
			   cparse-optsymbol-regexp "\\)"
			   cparse-dirty-whitespace "*{"))
	  (structs2 (concat cparse-struct-regexp "\\s-+\\("
			    cparse-optsymbol-regexp "\\)"
			    cparse-dirty-whitespace "*{"))
	  (structs3 (concat "\\(" cparse-symbol-regexp "\\)"))
	  (structs4  (concat "," cparse-dirty-whitespace "*"))
	  (typedef1 (concat "\\}\\s-*\\(" cparse-symbol-regexp "\\)"))
	  (typedef2 (concat "\\(typedef\\s-+" cparse-typedef-regexp "\\)"))
	  (typedef3 (concat "\\(" cparse-typedef-regexp "\\)"))
	  (vareq (concat cparse-dirty-whitespace "*{"))
	  (vareq2 (concat cparse-dirty-whitespace "*,"))
	  )
      (save-excursion
	(goto-char 0)
	(if (and name (listp name)) (setq name (car name)))
	(while (re-search-forward cparse-toplevel-regexp nil t)
	  (goto-char (match-beginning 1))
	  (if cparse-noisy-p
	      (message "PARSE%s: %d%%" 
		       (if name (concat " [" name "]") "")
		       (* (/ (float (point)) (float (point-max))) 100)))
	  (setq s (point))
	  (goto-char (match-end 1))
	  (setq e (point))
	  (goto-char s)
	  (setq c t)
	  (cond
	   ;; ifdefs and the like.  Must continue like defines to
	   ;; non-\ ending lines.
	   ((looking-at ifs)
	    (re-search-forward "\\([^\\]$\\)" nil t)
	    (setq e (point))
	    (setq c 'safe))
	   ;; #include statements
	   ((looking-at incs)
	    (save-excursion
	      (re-search-forward incs2 nil t)
	      (if (/= (match-beginning 1) s)
		  (setq c nil)
		;; we don't really need the string anymore.
		;; (setq bs (cparse-bss (match-beginning 1) (match-end 3)))
		(setq n (cparse-bss (match-beginning 2) (match-end 2)))
		(setq e (point)))
	      (goto-char s)
	      (if (re-search-forward "<" e t)
		  (setq type 'inc-s)	;system include
		(setq type 'inc-l)))) ;local include
	   ;; #define statements
	   ((looking-at defines)
	    (setq type 'define)
	    (save-excursion
	      (re-search-forward "\\([^\\]$\\)" nil t)
	      (setq e (point))
	      ;; we don't really need the string.
	      ;; (setq bs (cparse-bss s (match-end 1)))
	      (goto-char s)
	      (if (re-search-forward defines2 nil t)
		  (progn
		    (setq n (cparse-bss (match-beginning 2) (match-end 2)))))))
	   ;; skip all keywords (when parsing functions only!)
	   ((looking-at keys1)
	    ;; skip over the key word
	    (re-search-forward cparse-symbol-regexp nil t)
	    ;; check and skip parameter lists
	    (if (re-search-forward "(" nil t)
		(progn
		  (forward-char -1)	;back over it
		  (forward-sexp 1)))	;skip over the parameters
	    (re-search-forward mwhite nil t)
	    ;; now skip over large groups of parameters and forms
	    (if (looking-at "{")
		(forward-sexp 1))
	    (setq e (point))
	    (setq c nil))
	   ;; Lets do somethings special just for emacs source
	   ((looking-at "^DEFUN\\s-*(")
	    (setq type 'defun)
	    (re-search-forward "(" nil t)
	    (if (and (re-search-forward "\"" nil t)
		     (re-search-forward "[^/]\"\\s-*,\\s-*" nil t)
		     (re-search-forward mword nil t))
		;; pull name out of the defun
		(setq n (cparse-bss (match-beginning 1) (match-end 1)))
	      (message "DEFUN was formatted funny.")
	      (setq c nil))
	    ;; Now find the start of the function itself
	    (if (re-search-forward "{" nil t)
		(progn
		  (forward-char -1)
		  (forward-sexp 1)
		  (forward-line 1)
		  (beginning-of-line))
	      (setq c nil))
	    (setq e (point)))
	   ((looking-at "DEFVAR_[_A-Z]*\\s-*(")
	    (setq type 'defvar)
	    (re-search-forward "(" nil t)
	    (if (and (re-search-forward "\"" nil t)
		     (re-search-forward "[^/]\"\\s-*,\\s-*" nil t)
		     (re-search-forward mword nil t))
		;; pull name out of the defun
		(setq n (cparse-bss (match-beginning 1) (match-end 1)))
	      (message "DEFVAR was formatted funny.")
	      (setq c nil))
	    ;; Now find the end of the variable prototype
	    (re-search-forward ")\\s-*;$" nil t)
	    (forward-line 1)
	    (beginning-of-line)
	    (setq e (point)))
	   ;; functions with function return value double Eww!
	   ;; (X typedefs these puppies as fn pointers!  Ick.)
	   ((looking-at pfun)
	    (re-search-forward pfun2 nil t)
	    (setq n (cparse-bss (match-beginning 1) (match-end 1)))
	    (goto-char s)		;go to the beginning
	    (re-search-forward "(")	;open paren before fn name
	    (forward-char -1)		;skip back onto the open/close paren.
	    (forward-sexp 1)		;skip over matching parens
	    ;; now skip over the next containing expression of parameters
	    (if (looking-at (concat cparse-dirty-whitespace "*("))
		(if (re-search-forward "\\s-*(")
		    (progn
		      (forward-char -1)
		      (forward-sexp 1))))
	    ;; comma separated list, or end of prototype is a prototype
	    (if (looking-at pfun_param)
		(progn
		  (setq type 'fnprot)
		  (re-search-forward ";" nil t)
		  (setq e (point)))
	      (setq type 'fndef)
	      (re-search-forward "{" nil t)
	      (save-excursion
		(forward-char -1)	;just before the {
		(forward-sexp 1)	;skip over function body
		(forward-line 1)	;plus ending comment (if there is one)
		(beginning-of-line)))	;etc
	    (setq e (point)))
	   ;; functions must appear before variables.  Make sure there
	   ;; isn't a blank type here.
	   ((looking-at funs)
	    (re-search-forward funs2 nil t)
	    (setq n (cparse-bss (match-beginning 1) (match-end 1)))
	    ;; here flag for funcal, not funprot
	    (if (= (match-beginning 1) s)
		(setq type 'fndef))
	    (forward-char -1)		;skip back onto the open paren.
	    (forward-sexp 1)		;skip over matching parens
	    ;; comma separated list, or end of prototype is a prototype
	    (if (looking-at funs3)
		(if (not (equal type 'fndef))
		    (progn
		      (setq type 'fnprot)
		      (re-search-forward ";" nil t)
		      (setq e (point)))
		  ;; here, we have a fun call, not a prototype with
		  ;; not return type.
		  (re-search-forward ";" nil t)
		  (setq c nil))
	      (setq type 'fndef)
	      (re-search-forward "{" nil t)
	      (save-excursion
		(forward-char -1)	;just before the {
		(forward-sexp 1)	;skip over function body
		(forward-line 1)	;plus ending comment (if there is one)
		(beginning-of-line)	;etc
		(setq e (point)))))
	   ;; structures
	   ((looking-at structs)
	    (setq type 'type)
	    (re-search-forward "\\(typedef\\s-+\\)?" nil t) ;skip type
	    (if (looking-at "enum")
		(setq type 'enum))
	    (re-search-forward structs2 nil t)
	    (setq n (cparse-bss (match-beginning 2) (match-end 2)))
	    ;; structs and unions
	    (if (not (equal type 'enum))
		(save-excursion
		  (goto-char (1- e))
		  (forward-sexp 1)		;skip body of struct
		  (re-search-forward ";" nil t) ; find simicolon
		  (setq e (point)))
	      ;; enumerations needs to have all the names together for searching
	      (if (> (length n) 0)
		  (setq n (list n))
		(setq n nil))
	      (re-search-forward structs3 nil t)
	      (setq n (cons (cparse-bss (match-beginning 1) (match-end 1)) n))
	      ;; skoot to end and loop over insides.
	      (save-excursion
		(re-search-forward ";" nil t)
		(setq e (point)))
	      (save-restriction
		(narrow-to-region (point) e)
		(while (and (re-search-forward structs4 nil t)
			    (re-search-forward mword nil t))
		  (setq n (cons (cparse-bss (match-beginning 1)
					    (match-end 1)) n)))
		(setq n (reverse n))))
	    (goto-char s)
	    (if (and (looking-at "typedef")
		     (progn
		       (goto-char e)
		       (re-search-backward typedef1 nil t)))
		(progn 
		  (setq type 'typedef)
		  (if (and n (listp n))
		      (setq n (cons (cparse-bss (match-beginning 1) 
						(match-end 1)) n))
		    (if (and (stringp n) (> (length n) 0))
			(setq n (list n (cparse-bss (match-beginning 1) 
						    (match-end 1))))
		      (setq n (cparse-bss (match-beginning 1) 
					  (match-end 1))))))))
	   ;; this must appear after struct for normal typedefed vars
	   ((looking-at "typedef")
	    (setq type 'typedef)
	    (re-search-forward typedef2 nil t)
	    (re-search-forward mword nil t)
	    (setq n (cparse-bss (match-beginning 1) (match-end 1)))
	    (re-search-forward ";" nil t)
	    (setq e (point)))
	   ;; variables
	   ((looking-at (concat cparse-typedef-regexp cparse-anyvardef-regexp))
	    (setq type 'vardef)
	    ;; go past typedef part
	    (re-search-forward typedef3 nil t)
	    (let ((cont t))
	      (while cont
		(if (re-search-forward mword nil t)
		    (progn
		      (if n
			  (setq n (cons (cparse-bss (match-beginning 1)
						    (match-end 1)) n))
			(setq n (list (cparse-bss (match-beginning 1)
						  (match-end 1)))))
		      ;; skip array parts after name
		      (if (looking-at cparse-isarray-regexp)
			  (progn
			    (if array-here
				(setq array-here (cons t array-here))
			      (setq array-here (list t)))
			    (re-search-forward cparse-optarray-regexp nil t))
			(if array-here
			    (setq array-here (cons nil array-here))
			  (setq array-here (list nil))))
		      (if (looking-at "[ \t\n]*=") ;equals qualify as whitespace! ick!
			  (progn 
			    (re-search-forward "[ \t\n]*=" nil t)
			    (if (looking-at vareq)
				(progn
				  (re-search-forward vareq nil t)
				  (forward-char -1)
				  (forward-sexp 1))
			      (re-search-forward "\\(,\\|;\\)" nil t)
			      (forward-char -1))))
		      (if (looking-at "\\s-*;")
			  (progn
			    (re-search-forward "\\s-*;" nil t)
			    (setq e (point))
			    (setq cont nil))
			(if (looking-at vareq2)
			    (re-search-forward 
			     (concat cparse-dirty-whitespace "*,") nil t)
			  ;; we have no condition for this.  Exit!
			  (setq cont nil)
			  (setq e (point))))))))
	    (if (not n) (setq c nil))
	    (setq n (reverse n)))
	   ;; must list comments last because they sometime appear in the other types
	   ((looking-at "/\\*")
	    (setq type 'comment)
	    (save-excursion
	      (re-search-forward "\\(\\*/\\)" nil t)
	      (setq bs nil)
	      (setq e (point))))
	   (t
	    (setq c nil)))
	  (if (equal c t)
	      (progn
		(if (and l (equal type 'comment) 
			 (equal (aref (car l) 0) 'comment))
		    (aset (car l) 4 e)	;simply make the last comment longer.
		  (setq v (cparse-makev type bs n s e))
		  (if (and (eq type 'vardef) array-here)
		      (aset v 6 array-here))
		  (setq l (cons v l))))
	    ;; if fast, and we hit bad text, just skip it.
	    (if (and (equal c nil) fast) (setq e (point-max))))
	  (goto-char e)
	  ;;(sit-for 1)
	  (setq n nil
		array-here nil
		bs nil))
	)				;save excursion paren
      (setq cparse-mainlist (reverse l))
      (reverse l))))

(defun cparse-makev (type str name start end &optional array-part)
  "Create a part-vector which describes a part of a c construct."
  (let ((v (make-vector cparse-num-elts nil)))
    (aset v 0 type)
    (aset v 1 str)
    (aset v 2 name)
    (aset v 3 start)
    (aset v 4 end)
    (aset v 6 array-part)
    v))

(defun cparse-type (v)
  "Return the type of V."
  (if (vectorp v) (aref v 0) nil))

(defun cparse-str (v)
  "Return the string of V."
  (if (vectorp v) (aref v 1) nil))

(defun cparse-name (v)
  "Return the name of V."
  (if (vectorp v) (aref v 2) nil))

(defun cparse-start (v)
  "Return point where part V starts."
  (if (vectorp v) (aref v 3) 0))

(defun cparse-end (v)
  "Return point where part V ends."
  (if (vectorp v) (aref v 4) 0))

(defun cparse-datum (v)
  "The datum member of part struct of V."
  (if (vectorp v) (aref v 5) 0))

(defun cparse-array (v)
  "The array member of vardef of V"
  (if (vectorp v) (aref v 6) nil))

(defun cparse-returns (v)
  "Return the Type, or Return value of V (vars &  fns only)"
  (if (and (vectorp v) (or (equal (cparse-type v) 'vardef)
			   (equal (cparse-type v) 'fnprot)
			   (equal (cparse-type v) 'fndef)))
      (save-excursion
	(if (bufferp (cparse-datum v))
	    (set-buffer (cparse-datum v)))
	;; make sure things are what they seem.
	(save-restriction
	  (widen)
	  (cparse-toplevel (buffer-name))
	  (goto-char (cparse-start v))
	  (if (looking-at (concat "\\(" cparse-typedef-regexp "\\)"))
	      (let ((ts (cparse-bss (match-beginning 1) (match-end 1))))
		(if (= (aref ts (1- (length ts))) ?\n)
		    (setq ts (concat (substring ts 0 (1- (length ts))) " ")))
		(cparse-trim-word ts))
	  nil)))
    nil))

(defun cparse-expand-list (l)
  "Take list L and expand all contiguous parts.  IE, any vardef which
looks like this: int c, d, e; turns into ['vardef nil (c d e) x y nil]
and this routine will turn it into three vectors of one name each."
  (let (nl)
    (while l
      ;; suggestion from Valeriy
      ;;
      ;; use consp instead of listp??  Saves comments
      ;;
      (if (not (consp (cparse-name (car l))))
	  (setq nl (cons (car l) nl))
	(let ((tl (cparse-name (car l)))
	      (ap (cparse-array (car l))))
	  (while tl
	    (setq nl (cons (cparse-makev (cparse-type (car l))
					 (cparse-str (car l))
					 (car tl)
					 (cparse-start (car l))
					 (cparse-end (car l))
					 (list (if ap (car ap) nil))
					 )
			   nl))
	    (aset (car nl) 5 (cparse-datum (car l)))
	    (setq tl (cdr tl)
		  ap (if ap (cdr ap) nil)))))
      (setq l (cdr l)))
    (reverse nl)))

;;;
;;; PARTS buffer routines
;;;

(defun PARTS-mode (b list)
  "Major mode which keeps track of parts of a selected C file.  In
this buffer you may select view, jump to, and open parts of a c file.
\\<cparse-PARSE-keymap>
\\[cparse-toggle-comments] - toggle display of comment parts.
\\[cparse-update-p] - reparse c file.
\\[cparse-goto-line] - blink that position in c file in other window.
\\[cparse-find] - zap to that part in c file.
\\[cparse-open] - find out more good stuff about a c part.
\\[cparse-nuke-buffer] - make the buffer go away.

  Takes two arguments to start, B, the buffer this one references, and
LIST, the list of parts.
"
  (make-local-variable 'cparse-show-comments)
  (make-local-variable 'cparse-reference-buffer)
  (setq cparse-reference-buffer b)
  (make-local-variable 'cparse-mainlist)
  (setq cparse-mainlist list)
  (setq major-mode 'PARTS-mode)
  (setq mode-name "PARSED")
  (use-local-map cparse-PARSE-keymap)
  (run-hooks 'PARTS-hooks))

(defun cparse-listparts (&optional subpart list)
  "List all the parts in the current buffer in another buffer."

  (interactive)
  (if (not subpart) (cparse-toplevel (buffer-name)))
  (let ((tmp (if list list cparse-mainlist))
	(pt (point))
	(b (current-buffer)))
    (switch-to-buffer-other-window 
     (if subpart
	 (get-buffer-create (format "Parts of %s" subpart))
       (get-buffer-create (format "Parts of %s" 
				  (file-name-nondirectory
				   (buffer-file-name (current-buffer)))))))
    (PARTS-mode b tmp)
    (toggle-read-only -1)
    (cparse-write-parts tmp)
    (toggle-read-only 1)
    (let ((l cparse-mainlist))
      (while (and l (> pt (aref (car l) 4)))
	(setq l (cdr l)))
      (goto-char 0)
      (if l
	  (re-search-forward (int-to-string (aref (car l) 4)) nil t)
	(forward-line 1)))
    (beginning-of-line)
    (balance-windows)
    (shrink-window-if-larger-than-buffer)))

(defun cparse-write-parts (tmp-o)
  "Write the parts into the current buffer.  Relys on local variables
for the list being set, and having it's buffer being correctly set.
TMP is the list o parts"
  (delete-region (point-min) (point-max))
  (insert "Type\tName\t\tBegin\tEnd\tString\n")
  (let ((atleast-one nil) tmp)
    (while (not atleast-one)
      (setq tmp tmp-o)
      (while tmp
	(if (or cparse-show-comments 
		(not (equal (aref (car tmp) 0) 'comment)))
	    (progn
	      (setq atleast-one t)
	      (let* ((s1 nil) (s nil)
		     (n (progn (setq s1 (aref (car tmp) 2))
			       (if (listp s1) (setq s1 (car s1)))
			       (setq s (if s1 s1 ""))
			       (setq s (substring 
					(if (< (length s) 15)
					    (concat s "                 ")
					  s) 0 15)))))
		(insert (format "%S\t%s\t%d\t%d\t%s\n"
				(aref (car tmp) 0)
				n
				(aref (car tmp) 3)
				(aref (car tmp) 4)
				(let* ((s1 (aref (car tmp) 1))
				       (s (if s1 s1 "")))
				  (setq s (if (< (length s) 55)
					      s
					    (substring s 0 55)))
				  (if (string-match "\\(\n\\)" s)
				      (setq s (substring s 0 (match-beginning 1))))
				  s))))))
	(setq tmp (cdr tmp)))
      (if (not atleast-one) (setq cparse-show-comments t)))))

(defun cparse-update-p ()
  "From PARSED buffer, update list if we need to."
  (interactive)
  (save-window-excursion
    (set-buffer cparse-reference-buffer)
    (cparse-toplevel (buffer-name))))

(defun cparse-nuke-buffer ()
  "Delete this buffer and the window it is in."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window (selected-window)))

(defun cparse-toggle-comments (&optional set)
  "Toggle display of comments in PARSE buffer."
  (interactive)
  (if set (setq cparse-show-comments set))
  (setq cparse-show-comments (not cparse-show-comments))
  (let (n)
    (beginning-of-line)
    (re-search-forward "\t\\([0-9]+\\)" nil t)
    (setq n (cparse-bss (match-beginning 1) (match-end 1)))
    (cparse-update-p)
    (toggle-read-only -1)
    (cparse-write-parts cparse-mainlist)
    (toggle-read-only 1)
    (goto-char 0)
    (if (not (re-search-forward (regexp-quote n) nil t))
	(forward-line 1))
    (beginning-of-line)))

(defun cparse-find ()
  "When in PARSED buffer, goto the line in associated buffer where
this line is."
  (interactive)
  (cparse-goto-line t))

(defun cparse-goto-line (&optional go)
  "When in PARSED buffer, go the the other buffer where this line
points.  Optionally, when GO is nil, flash the point, otherwise, stay there."
  (interactive)
  (cparse-goto-vect-def (cparse-line2vect) go))
  
(defun cparse-goto-vect-def (vect &optional go buff)
  "Goto the line specified in VECT.  If GO then go there, otherwise,
blink the point."
  (let* ((p (point)) (b (current-buffer))
	 (split (and (equal buff b)
		     (cparse-one-window-ok (point) (aref vect 3)))))
    (if buff
	(if (not split)
	    (switch-to-buffer-other-window buff))
      (switch-to-buffer-other-window cparse-reference-buffer))
    (goto-char (aref vect 3))
    (if (not split) 
	(recenter (/ (window-height) 4)))
    (if (not go)
	(progn
	  (sit-for 1)
	  (if (not split) (switch-to-buffer-other-window b))
	  (goto-char p))
      (delete-other-windows))))

(defun cparse-open ()
  "In a PARSE buffer, take whatever is on the current line, and try to
do something with it.  The following conditions will be followed.
  'include - find-file the include, parse, and list.
  t        - same as cparse-goto-line
"
  (interactive)
  (let ((v (cparse-line2vect)))
    (save-excursion
      (set-buffer cparse-reference-buffer)
      (cparse-open-driver v))))

(defun cparse-open-driver (v)
  "From any c buffer, take V, a vector defining something, and open
it, meaning, the following for the defined types:
 'inc-l, inc-s            - open the include (local or system) file.
 'vardef, cvardef, fnprot - find the definition of its type.
 'type, typdef            - open the structure part as a file would be opened
 t                        - go there in the buffer."
  (cond
   ((or (equal (aref v 0) 'inc-l)
	(equal (aref v 0) 'inc-s))
    (message "Opening %s ..." (aref v 2))
    (cparse-find-header (aref v 2) t t t)
    (cparse-listparts))
   ((or (equal (aref v 0) 'type)
	(equal (aref v 0) 'typedef))
    (switch-to-buffer-other-window (current-buffer))
    (goto-char (aref v 3))
    (if (not (re-search-forward "{" (aref v 4) t))
	(message "%s is not a structure." (aref v 2))
      (forward-char -1)
      (save-restriction
	(narrow-to-region (point) (save-excursion (re-search-forward "}" nil t) (point)))
	(let ((l nil))
	  (let ((cparse-mainlist nil)	;fake it out.
		(cparse-parsesize 0))
	    (cparse-toplevel (aref v 2))
	    (setq l cparse-mainlist))
	  (cparse-listparts (aref v 2) l)))))
   ((or (equal (aref v 0) 'vardef)
	(equal (aref v 0) 'cvardef)
	(equal (aref v 0) 'fnprot))
      ;;; We must trim off the optional parts.
    (let ((end (aref v 4))
	  (m nil)
	  (s nil)
	  (n nil))
      (save-excursion
	(goto-char (aref v 3))
	(if (re-search-forward (concat "\\(" cparse-modifiers-regexp "\\)")
			       end t)
	    (setq m (cparse-trim-word
		     (cparse-bss (match-beginning 1) (match-end 1)))))
	(if (re-search-forward (concat "\\(\\(" cparse-struct-regexp
				       "\\s-+\\)?\\)") end t)
	    (setq s (cparse-trim-word
		     (cparse-bss (match-beginning 1) (match-end 1)))))
	(if (re-search-forward (concat "\\(" cparse-symbol-regexp "\\)")
			       end t)
	    (setq n (cparse-trim-word 
		     (cparse-bss (match-beginning 1) (match-end 1))))))
      (if n (cparse-findname n (concat s)))))
   (t
    (cparse-goto-line t))))

(defun cparse-line2vect ()
  "In a PARSE buffer, return the type vector of where the cursor is."
  (let ((l cparse-mainlist) (n 0))
    (save-excursion
      (cparse-update-p)
      (beginning-of-line)
      (re-search-forward "\t\\([0-9]+\\)" nil t)
      (setq n (string-to-int (cparse-bss (match-beginning 1)
					 (match-end 1))))
      (while (and l (not (= n (aref (car l) 3))))
	(setq l (cdr l)))
      (if l
	  (car l)))))

;;;
;;; In a C buffer stuff
;;;

(defun cparse-one-window-ok (p1 p2)
  "Find out if the window we are in is big enough to display points p1
and p2 simutaneously.  If it is, recenter the window between two
points if one is off the screen, otherwise do nothing.  Then, return t
if one window is ok."
  (let ((ans nil))
    (if (and (pos-visible-in-window-p p1 (selected-window))
	     (pos-visible-in-window-p p2 (selected-window)))
	(setq ans t)
      (save-window-excursion
	(goto-char (/ (+ p1 p2) 2))
	(recenter)
	(if (and (pos-visible-in-window-p p1 (selected-window))
		 (pos-visible-in-window-p p2 (selected-window)))
	    (setq ans 'recenter)))
      (if (equal ans 'recenter)
	  (progn
	    (goto-char (/ (+ p1 p2) 2))
	    (recenter))))
    ans))

(defun cparse-on-major-sexp ()
  "Print a message, or return the vector describing the major SEXP the
cursor is in.  It will describe a function, variable definition, macro
etc."
  (interactive)
  (cparse-toplevel (buffer-name))
  (let ((l cparse-mainlist))
    (while (and l (> (point) (aref (car l) 4)))
      (setq l (cdr l)))
    (if l
	(if (>= (point) (aref (car l) 3))
	    (if (interactive-p)
		(save-excursion
		  (message "=> %S" (car l))
		  (cparse-goto-vect-def (car l) nil (current-buffer)))
	      (car l))
	  (if (interactive-p)
	      (message "You are not in any major sexps")
	    nil)))))

(defun cparse-enclosing-expression (&optional num fast)
  "Returns list of cparts NUM number of levels to move outside of the
enclosing expression.  If interactive, then just print messages.
Returns 'cparse-tomany if NUM is too large.  Optional FAST means go
fast when searching because we are looking for something."
  (interactive "P")
  (if (not num) (setq num 1))
  (let ((ce (cparse-on-major-sexp))
	(p (point)) (n num)
	(b nil) (e nil))
    (if (or (not (vectorp ce)) (not (equal (aref ce 0) 'fndef)))
	(if (interactive-p)
	    (message "Not on an expression containing parsable stuff!")
	  'cparse-tomany)
      (goto-char (aref ce 3))		;goto the beginning
      (re-search-forward "{" nil t)	;now find the beginning
      (if (< p (point))
	  (if (interactive-p)
	      (message "No containing expression before function body!")
	    'cparse-tomany)
	(setq b (point))		;beginning position
	(goto-char p)
	;; scan backwards over curlies
	(while (and (> (point) b) (> n 0))
	  (while (and (re-search-backward "{" nil t) ;there is a ;previous
		      (> (point) b)	;point check
		      (> p (save-excursion (forward-char -1)
					   (forward-sexp 1)
					   (point))))) ;while it contains point
	  (setq n (1- n)))
	;; scan forward one sexp to define the total size of inside sexp
	(if (> n 0)
	    (if (interactive-p)
		(progn
		  (message "Request for too many levels of nesting!")
		  (goto-char p))
	      'cparse-tomany)
	  (setq b (point))		;beginning of this sexp
	  (forward-sexp 1)
	  (forward-char -1)
	  (setq e (point))
	  (save-restriction
	    (narrow-to-region b e)
	    (let ((l nil))
	      (let ((cparse-mainlist nil)	;fake it out.
		    (cparse-parsesize 0))
		(cparse-toplevel (format "Sexp in %s (%d)" (aref ce 2)
					 num) fast)
		(setq l cparse-mainlist))
	      (goto-char p)
	      (if (interactive-p)
		  (if l
		      (cparse-listparts (format "Sexp in %s" (aref ce 2)) l)
		    (message "No displayable containing expressions!"))
		;; if it is not interactive, then return the list o parts!
		l))))))))

(defun cparse-find-name (name)
  "Searches the c buffer starting at cursor, and working outwards."
  (interactive "sName to find: ")
  (cparse-findname name))

(defun cparse-open-on-line ()
  "Grab the object under the cursor and find it's definition."
  (interactive)
  (require 'thingatpt)
  (let ((wrd (thing-at-point 'sexp)))
    (cparse-findname wrd "")))

(defun cparse-findname (name &optional mods)
  "Find NAME from any c buffer by hunting down it's variable,
function, and define declarations.  Optional MODS for messages printed
the type modifiers"
  (if (string-match (concat "^" cparse-basic-type-regexp "$") name)
      (message "It is the standard type [%s%s]" 
	       (if (> (length mods) 0) (concat mods " ") "")
	       name)
    (if (string-match (concat "^" cparse-keywords-regexp "$") name)
	(message "It is a keyword [%s]" name)
      (if (string-match (concat "^" cparse-modifiers-regexp "$") name)
	  (message "It is a variable type modifier [%s]" name)
	(if (string-match "^[.0-9]+$" name)
	    (message "It is a number [%s]" name)
	  (message "Searching for [%s %s] in source..." mods name)
	  (let ((n 1)
		(l nil)
		(tv nil))
	    (save-excursion
	      ;; check out the parameters to enclosing function
	      (save-excursion
		(setq tv (cparse-on-major-sexp))
		(if (vectorp tv)
		    (if (equal (aref tv 0) 'fndef)
			(let ((l (cparse-readparams tv)))
			  (setq tv (cparse-findtype name l)))
		      (setq tv nil))))
	      ;; incrementally go up the nesting levels in a function
	      (if (not tv)
		  (setq l (cparse-enclosing-expression n t)))
	      (while (and (not tv) (not (equal l 'cparse-tomany)))
		(if l (setq tv (cparse-findtype name l)))
		(if tv
		    ()
		  (setq n (1+ n))
		  (setq l (cparse-enclosing-expression n t)))))
	    (if (not tv)
		(progn
		  ;; Look in all the header files.
		  (message "Searching for [%s %s] in headers..." mods name)
		  (setq tv (cparse-find-type-search name))))
	    (if (vectorp tv)
		(progn
		  (if (and (equal (aref tv 5) (current-buffer))
			   (> (point) (aref tv 3)) (< (point) (aref tv 4)))
		      (message "You are on => %S" tv)
		    (message "=> %S" tv)
		    (cparse-goto-vect-def tv 
					  (equal last-command 'cparse-open)
					  (aref tv 5)))
		  ;; if we don't clean the vect, then saves will mess up.
		  (aset tv 5 nil))
	      (if (numberp tv)
		  (message "Error finding [%s] in source and headers at max depth %d"
			   name tv)
		(message "Error finding [%s] in source and headers." name)))))))))

(defun cparse-readparams (v)
  "Find the definition in V, and if it is a function definition, read
it's parameter list, and turn it into a list of vardefs, and return."
  (if (and (vectorp v) (equal (aref v 0) 'fndef))
      (save-excursion
	(save-restriction
	  (let ((b nil) (e nil))
	    (widen)
	    (goto-char (aref v 3))
	    ;; find the paren, and narrow
	    (re-search-forward (concat cparse-symbol-regexp "\\s-*(" ) nil t)
	    (forward-char -1)
	    (setq b (point))
	    (forward-sexp 1)
	    (setq e (point))
	    (narrow-to-region b e)
	    (goto-char (point-min))
	    ;; match the param list names
	    (if (re-search-forward (concat "\\(" cparse-typedef-regexp "\\)"
					   "\\(" cparse-symbol-regexp "\\)"
					   cparse-optarray-regexp "\\(,\\|)\\)")
				   nil t)
		(cparse-read-ansic-params v)
	      (widen) (cparse-read-standardc-params v)))))
    nil))

(defun cparse-read-ansic-params (v)
  "Read parameter list from an ANSI c defined function. Must be called
after a restriction has been place eliminating all but the parameter
list parts.  Unlike other parser parts, set the datum part to the type
for functions that need that sort of thing."
  (let ((l nil) b e c ty s ss se (a nil))
    (goto-char (point-min))
    (setq c (point))
    (while (re-search-forward 
	    (concat "\\(" cparse-typedef-regexp "\\)"
		    "\\(" cparse-symbol-regexp "\\)"
		    cparse-optarray-regexp "\\(,\\|)\\)")
	    nil t)
      (save-excursion
	(goto-char c)
	(re-search-forward 
	 (concat "\\(" cparse-typedef-regexp "\\)") nil t)
	(setq ty (cparse-bss (match-beginning 1) (match-end 1)))
	(re-search-forward (concat "\\(" cparse-symbol-regexp "\\)") nil t)
	(setq s (cparse-bss (match-beginning 1) (match-end 1))
	      ss (match-beginning 1)
	      se (match-end 1))
	(if (looking-at cparse-isarray-regexp) (setq a (list t)))
	(re-search-forward (concat cparse-optarray-regexp
				   "\\(,\\|)\\)") nil t)
	(setq l (cons (cparse-makev 'vardef nil s ss se a) l))
	(aset (car l) 5 ty))
      (setq c (point)))
    (reverse l)))

(defun cparse-read-standardc-params (v)
  "Read parameter list from a standard C parameter list. Must be
called after a restriction has been place eliminating all but the
parameter list parts.  Unlike other parser parts, set the datum part
to the type for functions that need that sort of thing."
  ;;; We can cheat here because it is really a bunch of vardefs which
  ;;; we can parse with the main parser.
  (goto-char (cparse-start v))
  (if (not (re-search-forward "(" nil t))
      nil
    (let (b e l s)
    (forward-char -1)
    (forward-sexp 1)
    (setq b (point))
    (if (not (re-search-forward "{" nil t))
	nil
      (forward-char -1)
      (setq e (point))
      (save-restriction
	(narrow-to-region b e)
	(let ((cparse-mainlist nil)	;fake it out.
	      (cparse-parsesize 0))
	  (cparse-toplevel (format "Standard C parameters to %s"
				   (cparse-name v)) t)
	  (setq l cparse-mainlist)))
      (setq s l)
      (while s
	(if (equal (aref (car s) 0) 'vardef)
	    (progn
	      (goto-char (cparse-start (car s)))
	      (re-search-forward (concat "\\(" cparse-typedef-regexp "\\)") nil t)
	      (aset (car s) 5 (cparse-bss (match-beginning 1)
					  (match-end 1)))))
	(setq s (cdr s)))
      l))))


(defun cparse-find-type-search (name)
  "Search for the type named NAME in as many headers as can be found
until it is located."
  (let ((tv nil)
	(cparse-depthmarker 1))
    (setq tv (cparse-findtype name))
    (if (not tv)
	(progn
	  (setq cparse-tag nil)
	  (setq tv (cparse-find-type-list-breadth name cparse-mainlist))
	  ;; keep searching until found, or run out of depth.
	  (while (and (not (vectorp tv)) tv
		      (or (/= cparse-depthmarker cparse-query-depth)
			  (y-or-n-p 
			   (format "%s not found at depth %d. Continue?"
				   name cparse-depthmarker))))
	    (setq cparse-depthmarker (1+ cparse-depthmarker))
	    (setq tv (cparse-find-type-list-breadth name tv)))))
    (if tv tv cparse-depthmarker)))

(defun cparse-find-type-list-breadth (name list)
  "The engine of the list based breadth first search.  Look for NAME
among the includes in LIST, create a new list, and return.  Will take
lists of more than just includes, but returns lists only of returns."
  (let ((sl list)			;search list
	(nl nil)			;new list
	(f nil)				;file name looked at
	(l nil)				;list found in header
	(tv nil))			;vector found
    (while (and (not tv) sl)
      (if (equal (aref (car sl) 0) 'inc-l)
	  (progn
	    (setq f (aref (car sl) 2))
	    (if (member f cparse-tag)
		nil
	      (message "Depth %d Looking at: %s" 
		       cparse-depthmarker f)
	      (setq cparse-tag (cons f cparse-tag))
	      (setq l (cparse-find-header f nil t nil))
	      (setq tv (cparse-findtype name l f))
	      (if (not tv)
		  (let ((typ nil))
		    (while l
		      (setq typ (aref (car l) 0))
		      (if (and (or (equal typ 'inc-s) (equal typ 'inc-l))
			       (not (member (aref (car l) 2) cparse-tag)))
			  (setq nl (cons (car l) nl)))
		      (setq l (cdr l))))))))
      (setq sl (cdr sl)))
    (setq sl list)
    (while (and cparse-search-system (and (not tv) sl))
      (if (equal (aref (car sl) 0) 'inc-s)
	  (progn
	    (setq f (aref (car sl) 2))
	    (if (member f cparse-tag)
		nil
	      (message "Depth %d Looking at: %s" 
		       cparse-depthmarker f)
	      (setq cparse-tag (cons f cparse-tag))
	      (setq l (cparse-find-header f nil t nil))
	      (setq tv (cparse-findtype name l f))
	      (if (not tv)
		  (let ((typ nil))
		    (while l
		      (setq typ (aref (car l) 0))
		      (if (and (or (equal typ 'inc-s) (equal typ 'inc-l))
			       (not (member (aref (car l) 2) cparse-tag)))
			  (setq nl (cons (car l) nl)))
		      (setq l (cdr l))))))))
      (setq sl (cdr sl)))
    (if tv tv (reverse nl))))

(defun cparse-findtype (name &optional buf fname)
  "Look up a type (type/typedef) named NAME in optional c file BUF (or
current buffer) or if BUF is a list of parts, search BUF list.
Returns a type vector."
  (interactive "sName: ")
  (if (not buf) (setq buf (current-buffer)))
  (if (bufferp buf)
      (save-excursion
	(set-buffer buf)
	(cparse-toplevel (buffer-name))))
  (let ((l (if (and buf (listp buf)) buf cparse-mainlist)) tv)
    (while (and l (not (or (and (listp (aref (car l) 2))
				(member name (aref (car l) 2)))
			   (and (stringp (aref (car l) 2))
				(string= (aref (car l) 2) name)))))
      (setq l (cdr l)))
    (if (not l)
	(if (string-match (concat "\\(^\\|\\s-\\)\\(" cparse-basic-type-regexp
				  "\\)\\(\\s-+\\*\\)?")
			  name)
	    (setq tv (cparse-makev 'std-type name 
				   (substring name (match-beginning 2)
					      (match-end 2))
				   0 0)))
      (setq tv (car l)))
    (if (interactive-p)
	(message "Found %S" tv)
      (if (vectorp tv)			;set datum to buffer it was found in
	  (if (stringp fname)
	      (aset tv 5 (cparse-find-header fname t t nil))
	    (aset tv 5 (current-buffer))))
      tv)))

(defun cparse-trim-word (word)
  "Take WORD and trim all spaces from before and after.  Leave spaces 
in the middle, though."
  ;; replace extra width spaces, or single non-space whitespace with one space
  (while (string-match "[\n\t]\\|  " word)
    (setq word
	  (concat
	   (substring word 0 (match-beginning 0)) " " 
	   (substring word (match-end 0)))))
  (let ((omd (match-data)))
    (if (string-match "^\\(\\s-*\\)" word)
	(setq word (substring word (match-end 1))))
    (if (string-match "\\(\\s-*$\\)" word)
	(setq word (substring word 0 (match-beginning 1))))
    (store-match-data omd))
  word)

;;;
;;; Header hunting
;;;

(defun cparse-load-header (header)
  "Looks up header file using the cparse routines.  It generically
searches all files in /usr/include, plus all immediate subdirectories.
Will append .h if it is not included."
  (interactive "sHeader: ")
  (let ((fl (directory-files "/usr/include" t))
	(dl nil))
    (while fl
      (if (file-directory-p (car fl))
	  (setq dl (cons (car fl) dl)))
      (setq fl (cdr fl)))
    (if (not (string-match ".h$" header))
	(setq header (concat header ".h")))
    (let ((cparse-include-list (append cparse-include-list dl)))
      (cparse-find-header header t nil t))))

(defun cparse-find-header (header &optional find parse popup)
  "Locate where HEADER is in the file system.  Optionally, FIND will
load the file in, and PARSE will run the parser on it.  Optional
POPUP will put that buffer into a window when done.  If the file is
opted to be loaded, return the buffer it is in."
  (let ((fl cparse-include-list)
	(f nil)
	(b nil)
	(l nil)
	(ak nil)
	(dbl nil))
    (while (and (not f) fl)
      (let ((d (expand-file-name (car fl))))
	(if (/= (aref d (1- (length d))) ?/)
	    (setq d (concat d "/")))
	(if (file-exists-p (concat d header))
	    (setq f (concat d header))))
      (setq fl (cdr fl)))
    (if (not f)
	(progn
	  (message "Could not find %s in %S" header cparse-include-list)
	  nil)
      ;; Now do loading thingies based on flags.
      (setq dbl (cparse-filedb-find f))
      ;; check size, and make out of date if it has changed.
      ;; attrib returned is off by one to what emacs thinks it is.
      (if (and dbl (/= (1- (aref (car dbl) 1))
		       (nth 7 (file-attributes (aref (car dbl) 0)))))
	(setq dbl nil))
      ;; check if buffer is already loaded, and set auto-kill flag.
      (if (get-file-buffer f) (setq ak nil) (setq ak t))
      ;; find it if we need to
      (if (or find popup (and parse (not dbl)))
	  (if popup
	      (setq b (find-file-other-window f))
	    (setq b (cparse-fast-find f))))
      ;; check if we need parsing information
      (if parse
	  (if (and dbl b)
		(save-excursion
		  (set-buffer b)
		  (setq cparse-mainlist (aref (car dbl) 2))
		  (setq cparse-parsesize (point-max))
		  (setq l (aref (car dbl) 3)))
	    (if b
		(save-excursion
		  (set-buffer b)
		  (cparse-toplevel (buffer-name b))
		  (cparse-filedb-add (current-buffer))
		  (setq l cparse-mainlist))
	      (if dbl (setq l (aref (car dbl) 2))))))
      ;; if we wanted parsed, but no popup, then return list.
      (if (and parse (not find))
	  (progn
	    ;; if qualifies for auto-kill, kill it.
	    (if (and b ak) (kill-buffer b))
	    l)
	b))))

(defun cparse-fast-find (file)
  "Do a fast non-selected find file, and return the buffer."
  (let ((b nil)
	(c-mode-hook nil)
	(find-file-hooks cparse-brief-find-file-hooks))
    (setq b (find-file-noselect file t))
    b))

;;;
;;; File Database of Parts
;;;

(defun cparse-filedb-find (filename)
  "Search the cparse file list for a reference to FILENAME.  If found, return
the list, where the record we want is the CAR of the list."
  (if (not cparse-filedb-loaded)
      (progn
	(setq cparse-filedb-loaded t)
	(load cparse-filedb-rc t nil t)))
  (let ((ml cparse-filedb))
    ;; search the whole list for our file.
    (while (and ml (not (string= (aref (car ml) 0) filename)))
      (setq ml (cdr ml)))
    ml))

(defun cparse-filedb-add (buffer)
  "Add BUFFER to the database of parsed files.  Create a DBvect and
stores it.  A vect is 4 elements [filename sizeoffile parselist spare]"
  (save-excursion
    (setq cparse-filedb-modified t)
    (set-buffer buffer)
    (let ((dbv (make-vector 4 nil))
	  (ml (cparse-filedb-find (buffer-file-name))))
      (aset dbv 0 (buffer-file-name))
      (aset dbv 1 (point-max))
      (aset dbv 2 cparse-mainlist)
      (if ml 
	  ;; if we find that buffer in here, replace.
	  (setcar ml dbv)
	;; otherwise, tak it onto our list.
	(setq cparse-filedb (cons dbv cparse-filedb))))))

(defun cparse-filedb-save ()
  "Save the current C parts DB."
  (interactive)
  (if (and cparse-filedb-modified cparse-save-filedb)
      (save-excursion
	(message "Saving C parts database in %s" cparse-filedb-rc)
	(run-hooks 'cparse-filedb-save-hook)
	(set-buffer (cparse-fast-find cparse-filedb-rc))
	(erase-buffer)
	(insert ";;; CPARSE database of C parts.\n;;;\n;;; Parts is parts!\n;;;")
	(insert "(setq cparse-filedb\n      '")
	(insert (format "%S" cparse-filedb))
	(insert "\n)\n")
	(save-buffer))))
    
(if (not cparse-added-hook-p)
    (progn
      (setq cparse-added-hook-p t)
      (add-hook 'kill-emacs-hook 
		'(lambda () (cparse-filedb-save)))))

;;; end of lisp
(provide 'cparse)
