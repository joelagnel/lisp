;;; cproto - c prototype management

;;;
;;; Copyright (C) 1994,1996 Eric M. Ludlam
;;;
;;; Author: <zappo@gnu.ai.mit.edu>
;;; Version: $Id: cproto.el,v 1.4 1996/05/23 00:39:55 zappo Exp $
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

;;; Commentary:
;;;
;;;  Entry point:
;;;    `cpr-store-in-header' - Take definition under cursor and put it in
;;;                            the header file.
;;;
;;;  The ANSI c standard provides us with the ability to place the
;;; name and types of parameters in the definition of a c function.
;;; It also provides that these definitions can be placed in a
;;; function prototype.  These prototypes are typically held in a
;;; header file, allowing good compilers (like gcc) to do compile time
;;; error checking of parameters when calling these functions.  (I
;;; personally thought this was obvious until I worked in industry
;;; where people didn't know that this was a good thing. ACK!)
;;;    cproto is designed to help keep track of these definitions.
;;; One of my most common errors was forgetting to update the header
;;; file, and compiling ok, but running poorly.  cproto solved this
;;; for me.
;;;    cproto's job is simple.  When the function cpr-store-in-header
;;; is called, it identifies the the major c object the cursor is on
;;; (see cparse documentation), and stows it away into the header file
;;; of choice.  It will manage both function and variable
;;; declarations, and will initialize header files with the normal
;;; stuff one might find in there.  (Completely configurable, see
;;; cpcomment.) Many of the services require that comments in headers
;;; and c files be a little more verbose than usual, but I feel it
;;; makes for cleaner, and easier to read code.
;;;
;;;                       IMPORTANT
;;;
;;;   The purpose of this package is not to create perfectly formatted
;;; code!  Its purpose is to get the information into the right place
;;; with the appropriate tokens floating about to make it's job
;;; easier.  Spacing (newlines, spaces etc) will sometimes be off.
;;; Deleting extra lines, however, is easier than doing all this
;;; typing yourself.
;;;
;;;                      VERY IMPORTANT
;;;
;;;   When first using this package on old source code, be sure to
;;; check every transaction!  Header files that don't have source
;;; tokens in them can place your definitions in very strange places!
;;; Please be careful!
;;;
;;; History:
;;; eml 9/24/94
;;; Fixed but when the /* file */ token is not in the header file.
;;; It now is correctly inserted.
;;; eml 11/20/94
;;; Added space gobbling function for types with lots of spaces
;;; between the type and the * found in std. c definition with the
;;; names lined up.
;;; eml 4/9.96
;;; Fixed so that variable definitions with array parts are correctly
;;; transferred.

(require 'cparse)
(require 'sformat)

(defvar cpr-compiler-header-dir nil
  "*Directory where you might place some files and use '-I' to make the
compiler look.")

(defvar cpr-header-dir nil
  "*Where are headers for this stuff kept.  For systems where headers
are kept centralized, and possibly a subdirectory based on
compiler-header-dir.")

(defvar cpr-header-token "::Header::"
  "*A string placed before the header file name.  nil means don't use
this feature at all.  The result is that you are asked for the file
each time.")

(defvar cpr-prototype-cpp-variable "PROTOTYPES"
  "*String used as a #define variable which marks boundries between
prototypes with and without parameter lists.")

(defvar cpr-new-header-format
  "%C

#ifndef %F
#define %F

/* Structures:: */
%S

/* Prototypes:: */
%E

#endif /* %F */
"
  "String used to initialize a new header file which will be used to
store prototypes.  Available tokens are:
 %C - Big comment part (see cpcomment)
 %F - File name converted to DEFINEable string blah.h -> BLAH_H
 %f - The file name, unchanged.
 %S - Structures and parts not defined in C source goes here
 %E - Prototype elements go here.

The %S and %E don't actually print anything, they just provide markers
to identify text expressions (the comments before them) which allow
cproto to identify sections of the header file which will be used for
prototype elements and such. 

You don't need to use cpr to initialize your files, but this still
needs to be set up because it is used to create expressions to find
where things belong.")

(defvar cpr-prototype-element-group
  "/* %F */
%P

"
  "String used to define the way a group of prototypes (from an
individual file) is defined in the header file under the %E token.
Viable tokens are:
 %F - The short file from which the function lives.
 %P - Function and variable prototypes go here.
 %p - Function and variable prototypes with no parameters go here.
 %I - String declared in cpr-prototype-cpp-variable

 Note: It is important to have /* %F */ somewhere as a 'start token'
of this file.  If it does not exist, the cpr may have trouble
identifying where things are supposed to go.  In addition, file parts
are found by uniquely matching parts around the %P, so if extra stuff
is added in here, make sure that the strings and comments form
something relatively unique.  Note that the section beloning to %F is
terminated with just a blank line.  DON'T remove this.  CPR needs it.
If you modify this, remember we need more than just one cr to identify
the region of insertion.")

(defvar cpr-prototype-string "extern %T%S%N(%P);"
  "This defines a 'simple prototype' used in the %P and %p sections of
the element-group string.  Viable tokens are:
 %T - return value of function (type)
 %N - name of the function
 %P - parameter list.  If non-prototype version, take this out.
 %I - String declared in cpr-prototype-cpp-variable
 %S - One space if the type is not a pointer
 For functions which receive functions and have all those parenthises
all over, this may cause problems, but in most cases, this should be
enough.")

(defvar cpr-static-prototype-string "%T%S%N(%P);"
  "This defines a 'simple prototype' used in the %P and %p sections of
the element-group string.  This is how to insert prototypes for STATIC
functions which appear in the same source file.  Viable tokens are:
 %T - return value of function (type)
 %N - name of the function
 %P - parameter list.  If non-prototype version, take this out.
 %I - String declared in cpr-prototype-cpp-variable
 %S - One space if the type is not a pointer
 For functions which receive functions and have all those parenthises
all over, this may cause problems, but in most cases, this should be
enough.")

(defvar cpr-variable-prototype-string "extern %T%S%N%A;"
  "This defines a 'simple variable prototype' used in %P in the
element group string.  It has the same tokens as the prototype string.
If a variable is declared 'int a, b=4, c[3];' it will turn into:
extern int a;
extern int b;
extern int c[];
  You may realize that %P is used for functions as well.  Variables
appear before function prototypes when being inserted.")

(defun cpr-store-in-header ()
  "Grab the header from current position, load in the header file, and
make any needed substitutions to update the header file.  If the
function is static, then create needed stuff in this c file for the
prototype."
  (interactive)
  (let* ((tv (cparse-on-major-sexp))
	 (cf (file-name-nondirectory (buffer-file-name)))
	 (junk (if (not (or (equal (cparse-type tv) 'vardef)
			    (equal (cparse-type tv) 'fndef)))
		   (error "You are not on a function or variable definition")))
	 (b nil))
    (aset tv 5 (current-buffer))

;;; From valerie
    
    ;; now fix the mainlist, and get new position of tv
;       (cparse-toplevel (format "Fixing %s" (buffer-name)))
;       (setq tv (cparse-findtype (cparse-name tv)))
   ;                    this is ("name") instead of "name"
   ;                    cparse-expand-list first???
;       (message "Loading and modifying header...")
;           ...

;      I made a temp fix, so that cparse-toplevel always applies
;      cparse-expand-list to mainlist, and cparse-exapnd-list uses
;      (consp (cparse-name (car l))) instead of listp, to prevent
;      comments of being whipped out.


    (if (string-match "static" (cparse-returns tv))
	;; We must insert the prototype in this c file
	(let ((tl cparse-mainlist)
	      (lasti nil) (lastp nil))
	  ;; scan for position to insert prototype
	  (while (and tl (not (member (cparse-type (car tl)) 
				      '(vardef fndef fnprot))))
	    (setq lasti (car tl)
		  tl (cdr tl)))
	  (while (and tl (equal (cparse-type (car tl)) 'fnprot))
	    (setq lastp (car tl)
		  tl (cdr tl)))
	  (save-excursion
	    (let ((sm (if lasti (1+ (cparse-end lasti)) 0))
		  (em (if lastp (1+ (cparse-end lastp) )
			(if (car tl) (cparse-start (car tl))
			  (point-max)))))
	      (cpr-add-prototype sm em tv nil t)
	      ;; tack in an extra space before if needed
	      (if (and lasti (not lastp)) 
		  (save-excursion
		    (goto-char sm)
		    (insert "\n")))
	      ;; tack in an extra space after if needed
	      (if (not lastp)
		  (insert "\n")))
	    ))

      ;; We must insert the prototype into a header file instead
      (let ((hf (cpr-find-header)))
	(if (not hf) (error "You must define a header file."))
	(cpr-verify-header-token hf)
	(if (and (not (cpr-include-header-p hf))
		 (y-or-n-p (format "Add include for %s?" hf)))
	    (cpr-add-include-header hf))
	;; now fix the mainlist, and get new position of tv
	;; 
	;; SELF NOTE:
	;;  This should be a part of previous if statement for efficiency? 
	;;
	(cparse-toplevel (format "Fixing %s" (buffer-name)))
	(setq tv (cparse-findtype (if (listp (cparse-name tv))
				      (car (cparse-name tv))
				    (cparse-name tv))))

	(message "Loading and modifying header...")
	(setq b (cpr-prepare-headerfile hf (file-name-nondirectory 
					    (buffer-file-name))))
	(save-window-excursion
	  (switch-to-buffer b)
	  ;; Find the parts around these guys.  We must build a temp string
	  ;; from cpr-prototype-element-group so we can ident the file name
	  ;; parts.
	  (let* ((pts (Sformat (list (list ?F cf)
				     (list ?I cpr-prototype-cpp-variable)
				     ;; leave these tokens as is.
				     (list ?p "%%p")
				     (list ?P "%%P"))
			       cpr-prototype-element-group))
		 (bp (cpc-just-before-token-regexp ?p pts))
		 (ep (cpc-just-after-token-regexp ?p pts))
		 (bP (cpc-just-before-token-regexp ?P pts))
		 (eP (cpc-just-after-token-regexp ?P pts)))
	    ;; SELF NOTE
	    ;;
	    ;; To be accurate, I should narrow the buffer to everything
	    ;; that appears around the %E token of cpr-new-header-format,
	    ;; but for simplicity sake, I first check for the region
	    ;; around %P in cpr-prototype-element-group.
	    ;;
	    ;; This lets cproto support files in which users don't want to
	    ;; put complex text tokens into thier header files, yet still
	    ;; wish to update thier headers based on file tokens, or on
	    ;; headers with different format strings during thier creation.
	    (goto-char (point-min))
	    (if (and bp ep)
		(let ((sm (re-search-forward bp nil t))
		      (em (re-search-forward ep nil t)))
		  (cpr-add-prototype sm em tv nil)))
	    (goto-char (point-min))
	    (if (and bP eP)
		(let ((sm (re-search-forward bP nil t))
		      (em (re-search-forward eP nil t)))
		  (if (not (and sm em))
		      ;; Oh oh, the /* file */ string is not here yet. ;(
		      (let ((ibp (cpc-just-before-token-regexp 
				  ?E cpr-new-header-format)))
			(if (not (re-search-forward ibp nil t))
			    (progn
			      (message "could not find a place to put the prototype.")
			      (setq sm (point-max))
			      (setq em (point-max)))
			  ;; re-search will dump us at the right spot.
			  (insert (Sformat 
				   (list (list ?F cf)
					 (list ?I cpr-prototype-cpp-variable)
					 ;; Don't put in prototype strings
					 (list ?p "")
					 (list ?P ""))
				   cpr-prototype-element-group))
			  (goto-char (point-min))
			  ;; Ok, go back and re-find the tokens.
			  (setq sm (progn (re-search-forward bP nil t) (point)))
			  (setq em (re-search-forward eP nil t)))))
		  (cpr-add-prototype sm em tv t)))
	    ;; At this point we should update the history in the header
	    ;; file for completeness.
	    )
	  (sit-for 1))
	)
      )
    )
  (message "Done."))

(defun cpr-add-prototype (start end part proto &optional static)
  "Add new definition between START and END for PART.  If PROTO is
non-nil, then use prototypes of function parameters too."
  (save-restriction
    (narrow-to-region start end)
    (let* ((tl (cparse-expand-list (list part))))
      (while tl
	(let* ((tp nil) (ll nil) (l nil) (r (cparse-returns (car tl)))
	       (exp (if (equal (cparse-type (car tl)) 'fndef)
			(Sformat (let ((cprp (cpr-parameters (car tl))))
				   (list (list ?T r)
					 (list ?N (cparse-name (car tl)))
					 (list ?P (if cprp cprp ""))
					 (list ?S (if (string-match "\\*$" r)
						      "" " "))
					 (list ?I cpr-prototype-cpp-variable)))
				 (if static
				     cpr-static-prototype-string
				   cpr-prototype-string))
		      (Sformat (list (list ?T r)
				     (list ?N (cparse-name (car tl)))
				     (list ?S (if (string-match "\\*$" r)
						  "" " "))
				     (list ?A (if (car (cparse-array (car tl)))
						  "[]" ""))
				     )
			       cpr-variable-prototype-string))))

	  (let ((cparse-mainlist nil)	;fake it out.
		(cparse-parsesize 0))
	    (cparse-toplevel (format "Parts in header from %s"
				     (cparse-datum proto)))
	    (setq l cparse-mainlist)
	    ;; Now, check PART, and see if it exists.  If so, replace it
	    ;; reguardless of type.
	    (setq tp (cparse-findtype (cparse-name (car tl)) l))
	    (if tp
		(progn
		  (goto-char (cparse-start tp))
		  (delete-region (cparse-start tp) (cparse-end tp))
		  (insert exp))
	      ;; otherwise, find where to place it.
	      (if (equal (cparse-type (car tl)) 'vardef)
		  ;; variables come before functions
		  (while (and l (not (equal (cparse-type (car l)) 'fnprot)))
		    (setq ll (car l)) (setq l (cdr l)))
		;; functions get stuffed onto the end.
		(while l (setq ll (car l)) (setq l (cdr l))))
	      (if ll (goto-char (cparse-end ll))
		(goto-char (point-min)))
	      (insert (concat (if ll "\n") exp))
	      (if (/= (following-char) ?\n) (insert "\n")))))
	(setq tl (cdr tl))))))

(defun cpr-parameters (part)
  "Read the parameters from the function definition in PART, and
return a string with those parts all tidilly strung together.  If PART
is not a fundef, then return empty string."
  (save-excursion
    (if (bufferp (cparse-datum part))
	(set-buffer (cparse-datum part)))
    (let ((pl (cparse-expand-list (cparse-readparams part)))
	  (rs nil))
      (while pl
	(let ((r (cparse-trim-word (cparse-datum (car pl)))))
	  (setq rs (concat rs 
			   r
			   (if (string-match "\\*$" r) "" " ")
			   (cparse-name (car pl))
			   (if (car (cparse-array (car pl))) "[]" "")
			   (if (cpc-more-defs pl 'vardef) ", " ""))))
	(setq pl (cdr pl)))
      rs)))

(defun cpr-prepare-headerfile (file cf)
  "Load in FILE, then add any header info needed (like ifdefs and
comments etc) and search for correct location to insert based on CF
which is the c file."
  (set-buffer (find-file-noselect file))
  (if (not (file-exists-p file))
      (let ((ifdef-string (cpr-header-defined file))
	    (protos (Sformat (list (list ?F cf)
				   (list ?I cpr-prototype-cpp-variable)
				   (list ?p "")
				   (list ?P ""))
			     cpr-prototype-element-group)))
	(insert (Sformat (list (list ?C (cpc-new-file-header))
			       (list ?F ifdef-string)
			       (list ?f file)
			       (list ?S "")
			       (list ?E protos))
			 cpr-new-header-format))
	(save-buffer)))
  (current-buffer))

(defun cpr-add-include-header (header)
  "Place HEADER as the last local #include in a list of #include files."
  (cparse-toplevel (buffer-name))
  (let ((blist cparse-mainlist)
	(ilist nil)
	(clist nil)
	(startpt 0))
    ;; first, scan over first comment
    (if (equal (cparse-type (car blist)) 'comment)
	(progn
	  (setq startpt (cparse-end (car blist)))
	  (setq blist (cdr blist))))
    ;; go over all other #includes and comments
    (while (or (equal (cparse-type (car blist)) 'comment)
	       (equal (cparse-type (car blist)) 'inc-l)
	       (equal (cparse-type (car blist)) 'inc-s))
      (cond ((equal (cparse-type (car blist)) 'comment)
	     (setq clist blist))
	    (t (setq ilist blist)))
      (setq blist (cdr blist)))
    ;; here, clist,ilist is the last comment/include, insert near here.
    (if ilist
	(setq startpt (cparse-end (car ilist)))
      (if clist (setq startpt (cparse-start (car clist)))))
    (save-excursion
      (goto-char startpt)
      (insert (format "\n#include \"%s\"" header)))))

(defun cpr-include-header-p (header)
  "Check the current c file and return the include vector if it
includes HEADER somewhere."
  (cparse-findtype header))

(defun cpr-verify-header-token (header)
  "Verify that HEADER is in the top level header, and has the correct
name.  If the Token isn't in the file, add it in based on cpcomment
rule cpc-file-comment"
  (cparse-toplevel (buffer-name))
  (let ((tc (car cparse-mainlist)))
    (if (not (equal (cparse-type tc) 'comment))
	(if (y-or-n-p "Insert comment at beginning of file?")
	    (progn
	      (cpc-insert-new-file-header header)
	      (cparse-toplevel (buffer-name)))
	  (message "This may cause problems later...")
	  (sit-for 1)))
    (save-excursion
      (goto-char (point-min))
      (if (and (not (re-search-forward cpr-header-token nil t))
	       (y-or-n-p "Add header token into existing header?"))
	  (let ((etr (cpc-just-after-token-regexp 
		      ?T cpc-file-comment)))
	    (if (not etr)
		(message "Could not find %%T in cpc-file-comment")
	      (goto-char (point-min))
	      (if (not (re-search-forward (concat "\\(" etr "\\)") 
					  (cparse-end tc) t))
		  (if (y-or-n-p "Comment missmatch: Insert compatible header?")
		      (progn
			(cpc-insert-new-file-header header)
			(cparse-toplevel (buffer-name)))
		    (message "This may cause problems later..."))
		(goto-char (match-beginning 1))
		(insert (concat cpr-header-token " " header "\n"))))))
      (sit-for 1))))


(defun cpr-find-header ()
  "Find the name of the header file for this file, and return it."
  (let ((name nil))
    (save-excursion
      (goto-char (point-min))
      (setq name (cpr-read-header-id))
      (if name
	  nil
	(setq name (cpr-get-header-name))
	(setq name (read-file-name "Header for prototypes: "
				   (file-name-directory 
				    (buffer-file-name (current-buffer)))
				   name
				   nil
				   name))))
    (cpr-relational-header name)))
	  
(defun cpr-read-header-id (&optional buff)
  "Look for the header id string in a c file and return that file.  If
it doesn't exist, return nil"
  (if (and (re-search-forward (concat "\\(" cpr-header-token "\\)\\(\\s-*\\)" )
			      nil t)
	   (looking-at (concat "\\(" cparse-file-regexp "\\)")))
      (buffer-substring (match-beginning 1) (match-end 1))
    nil))

(defun cpr-relational-header (header)
  "Take header id, and convert it to a relational file name.  This
name will be partially based on compiler dir (if it is within it) or
simply relational to the current directory of the C file."

  (let* ((cf (buffer-file-name))
	(remote (cpr-relate-fnames header cf)))
    (if (string= (file-name-directory (expand-file-name header))
		 (file-name-directory cf))
	(if (or cpr-compiler-header-dir cpr-header-dir)
	    (concat "./" (file-name-nondirectory header))
	  (file-name-nondirectory header))
      remote)))

(defun cpr-relate-fnames (header cf)
  "Relate two file names and return a new file name thereof."

  (let ((pos 0)
	(relf nil))
    ;; first, expand just in case.
    (setq header (expand-file-name header))
    (if (and cpr-compiler-header-dir
	     (string-match (concat "\\(" cpr-compiler-header-dir "\\)")
			   header))
	(substring header (1+ (match-end 1)) (length header))
      ;; locate first difference
      (while (and (< pos (1- (length cf))) (< pos (1- (length header)))
		  (= (aref cf pos) (aref header pos)))
	(setq pos (1+ pos)))
      ;; backtrack to last "/" character
      (while (and (> pos 0) (/= (aref cf pos) ?/))
	(setq pos (1- pos)))
      ;; Save, and read "up" directories from c file
      (let ((upc pos) (upd 0))		;-1 because we start with a "/"
	(while (< upc (1- (length cf)))
	  (setq upc (1+ upc))
	  (if (= (aref cf upc) ?/)
	      (setq upd (1+ upd))))
	;; now work out relational ".."s
	(while (> upd 0)
	  (setq relf (concat relf "../"))
	  (setq upd (1- upd)))
	;; and append the file name we got from before.
	(setq relf (concat relf (substring header (1+ pos) (length header)))))
      relf
      )))

(defun cpr-get-header-name (&optional buff)
  "Create the filename header associated with the current .c file or
with file in BUFF.  Use the cpr-header-dir to make sure it is in the
correct place."
  (if (not buff) (setq buff (current-buffer)))
  (save-excursion
    (set-buffer buff)
    (let ((fname (buffer-file-name))
	  (newname ""))
      (if (not (equal major-mode 'c-mode))
	  (error "This buffer is not a c sourse file.")
	(setq newname (concat (if cpr-compiler-header-dir 
				  (concat cpr-compiler-header-dir "/"))
			      (if cpr-header-dir (concat cpr-header-dir "/"))
			      (file-name-nondirectory fname)))
	(setq newname (substring newname 0 (1- (length newname))))
	(setq newname (concat newname "h"))))))

(defun cpr-header-defined (file)
  "Generate an ifndef define variable string to use based on FILE.
Rule:  Drop directory, and replace . with _ and capitalize."
  (setq file (file-name-nondirectory file))
  (if (string-match "\\(.h$\\)" file)
      (setq file (concat (substring file 0 (match-beginning 0))
			 "_H"))
    (error "Invalid file name for header define generation %s." file))
  (upcase file))

	
;;; end of lisp
(provide 'cproto)
;; require here to avoid recursive requirements.
(require 'cpcomment)
