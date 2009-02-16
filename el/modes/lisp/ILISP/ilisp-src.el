;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-src.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-src.el,v 1.13 2003/12/21 03:16:39 rgrjr Exp $

(require 'cl)

;;; See ilisp.el for more information.

;;;%Source file operations
(unless (boundp 'tags-file-name)
  (defvar tags-file-name nil))

(defvar lisp-last-definition nil "Last definition (name type) looked for.")

(defvar lisp-last-file nil "Last used source file.")

(defvar lisp-last-point nil "Last point in last source file.")

(defvar lisp-last-locator nil "Last source locator used.")

(defvar lisp-search nil "Set to T when searching for definitions.")

(defvar lisp-using-tags nil "Set to T when using tags.")

;;;%%lisp-directory
(defvar lisp-edit-files t
  "Controls editing of of source files through Emacs' buffers.
If T, then buffers in one of 'lisp-source-modes' will be searched by
'edit-definitions-lisp' if the source cannot be found through the
inferior LISP.  It can also be a list of files to edit definitions
from set up by \(\\[lisp-directory]).  If it is set to nil, then no
additional files will be searched.")

;;;
(defun lisp-extensions ()
  "Return a regexp for matching file extensions.
The extensions are those of files that enter one of
'lisp-source-modes' according to 'auto-mode-alist'."
  (let ((entries auto-mode-alist)
	(extensions nil))
    (dolist (entry entries)
      (when (memq (cdr entry) lisp-source-modes)
	(setq extensions 
	      (concat "\\|" (car entry) extensions))))
    (substring extensions 2)))

;;;
(defun lisp-directory (directory add)
  "Edit the files in DIRECTORY.
The files must have an 'auto-mode' alist entry in 'lisp-source-modes'.
With a positive prefix, add the files on to the already existing
files.  With a negative prefix, clear the list.  In either case set
tags-file-name to nil so that tags are not used."
  (interactive 
   (list (if (not (eq current-prefix-arg '-))
	     (read-file-name "Lisp Directory: "
			     nil
			     default-directory
			     nil))
	     current-prefix-arg))
  (setq tags-file-name nil)
  (if (eq add '-)
      (progn (setq lisp-edit-files t)
	     (message "No current lisp directory"))
      (if add
	  (message "Added %s as a lisp directory" directory)
	  (message "%s is the lisp directory" directory))
      (setq directory (expand-file-name directory))
      (if (file-directory-p directory)
	  (setq lisp-edit-files
		(append
		 (directory-files directory t (lisp-extensions))
		 (if add (if (eq lisp-edit-files t) nil lisp-edit-files))))
	  (error "%s is not a directory" directory))))

;;;; New M-. implementation.  -- rgr, 25-Mar-03.

(defvar lisp-fall-back-on-find-tags ':ask
  "*Can be one of nil for never, :ask to ask the user each time, or
anything else to always use find-tag as a last resort (when ilisp can't
find anything).")
(defvar lisp-find-definition-verbose-p nil
  "Whether to generate informational messages about internal progress.")

;; these regexp fragments are shared between a number of regexp-constructing
;; definitions.
(defvar ilisp-whitespace* "\\([ \t\n]*.\\)?[ \t\n]*"
  "This looks hairy because it also matches an optional Symbolics
font-change escape sequence (the \"^F.\" in the middle).")
(defvar ilisp-whitespace+ "\\([ \t\n]+\\(.[ \t\n]*\\)?\\|.[ \t\n]+\\)"
  "More font-change hairiness.")
(defvar ilisp-terminal-whitespace "[ \t\n();]"
  "Regexp that matches 'terminal' whitespace (whatever might end a symbol).")
(defvar lisp-match-name "[^ \t\n();#]+"
  "Regexp that matches a Lisp symbol (including any package name).  This
is greatly oversimplified.")
(defvar lisp-optional-package-regexp
	(concat "\\(" lisp-match-simple-name ":+\\)?")
  "Regexp that matches an optional package prefix.")
(defvar lisp-ws-and-opt-pkg-regexp
	(concat ilisp-whitespace+ lisp-optional-package-regexp)
  "Regexp that matches one or more whitespace characters, followed by an
optional package prefix.")
(defvar lisp-match-package-prefix (concat "^" lisp-match-simple-name ":+")
  "Note that this will not match the prefix in (e.g.) '(pcl::method foo ...)'.")

(defstruct ilisp-defn-spec
  ;; Structure that describes an entry in the *Edit-Definitions* buffer, which
  ;; is parsed and returned by the lisp-find-next-possibility fn.  This is what
  ;; gets stored into the lisp-locate-definition variable (formerly, it was just
  ;; a cons of (name . type).
  (name nil)	;; an ilisp-symbol
  (name-string "")	;; string version of above
  (type "function")
  (file nil)
  (parent nil)
  (hints nil))

;; [find a better place for this?  -- rgr, 25-Mar-03.]
(defun ilisp-getf (disembodied-plist symbol &optional default)
  ;; symbol is an emacs lisp symbol.  This is case insensitive for
  ;; disembodied-plist indicators, to paper over emacs/CL case differences.
  (let ((result default) (name (downcase (symbol-name symbol))))
    (while disembodied-plist
      (if (string-equal (downcase (symbol-name (car disembodied-plist))) name)
	  (setq result (car (cdr disembodied-plist))
		disembodied-plist nil)
	  (setq disembodied-plist (cdr (cdr disembodied-plist)))))
    result))

(defun lisp-read-cl-syntax (string)
  ;; Given a string containing zero or more forms in Common Lisp syntax, read
  ;; them into emacs lisp.  [Unfortunately, the quoting we have to do here will
  ;; be duplicated by %S on re-output, so this doesn't win by itself.  -- rgr,
  ;; 14-Sep-02.]
  (save-excursion
    (set-buffer (get-buffer-create " *ilisp-cl-result*"))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    ;; Scan the buffer for characters that need quoting.  Everything within
    ;; quoted strings is fine, hence the in-quoted-string-p state maintenance.
    (let ((in-quoted-string-p nil)
	  (uninteresting-chars "^\"\\#.?"))
      (skip-chars-forward uninteresting-chars)
      (while (not (eobp))
	(let ((char (char-after (point))))
	  (cond ((= char ?\")
		  (setq in-quoted-string-p (not in-quoted-string-p)))
		((= char ?\\)
		  ;; We don't ever want to escape a backslash, and we don't even
		  ;; want to think about the char after it.
		  (forward-char 1))
		(in-quoted-string-p)
		((= char ?\.)
		  ;; Dots act as the "consing dot" in dotted-pair notation in
		  ;; both CL and elisp, but Common Lisp allows them to be
		  ;; constituent characters (no "\" needed).  Embedded and
		  ;; trailing dots in CL atoms used to be treated as consing
		  ;; dots by the elisp reader, but I notice that FSF emacs 20.7
		  ;; reads them compatibly.  But leading dots are not
		  ;; compatible; even 20.7 reads "(foo .bar)" as a dotted pair,
		  ;; whereas CL does not, so we always make this correction
		  ;; regardless of emacs version.  Note that it relies on the
		  ;; fact that CL always prints whitespace before and after a
		  ;; consing dot; other possibilities handled by the CL reader
		  ;; will break.  -- rgr, 14-Sep-02.
		  (if (not (save-excursion
			     (skip-chars-backward " \t\n")
			     (looking-at "[ \t\n]+\\.[ \t\n]+")))
		      (insert "\\")))
		(t
		  (insert "\\")))
	  (forward-char 1))
	(skip-chars-forward uninteresting-chars)))
    ;; Now read and return what we quoted.
    (let ((result nil))
      (goto-char (point-min))
      (condition-case error
	  (while t
	    (setq result (cons (read (current-buffer)) result)))
	;; EOF could also be incomplete syntax, or some CL syntax we can't read.
	(end-of-file nil))
      ;; Deal with NIL vs nil.  Other symbol case issues need to be addressed by
      ;; the caller, but the empty list has to be the empty list.
      (nreverse (nsubst nil 'NIL result)))))

;;; Source code definition line matching hacks.

;; ilisp-cl-source-locator-patterns --
;;
;; Note:
;;
;; 19990804 Marco Antoniotti
;; The new ones (method and generic-fucntion) should be carefully checked.
;;
;; [updated, made more concise.  -- rgr, 7-Sep-02.]

(defvar ilisp-cl-source-locator-patterns
	(let (;; <ws+> used to be
	      ;; "\\([ \t\n]+\\(.\\)?[ \t\n]*\\|[ \t\n]*.[ \t\n]+\\)".  --
	      ;; rgr, 7-Sep-02.
	      (<ws+> ilisp-whitespace+)
	      ;; this used to be "\\(.\\)?[ \t\n]*"; changed to look like
	      ;; ilisp-whitespace*, but doesn't match newlines.
	      (<ws*> "\\([ \t]*.\\)?[ \t]*")
	      ;; terminal whitespace (or whatever might end a symbol).
	      (<tws> ilisp-terminal-whitespace))
	  (list
	    (list 'variable	(concat "^" <ws*>
					"(def\\(var\\|parameter\\|constant\\)"
					<ws+> "%s" <tws>))
	    (list 'structure	(concat "^" <ws*> "(defstruct" <ws+>
					"(?" <ws*> "%s" <tws>))
	    ;; [this doesn't work by itself any more, because of the two %s
	    ;; occurrences (see lisp-make-cl-setf-fspec-finder), but is kept
	    ;; here for readability.  -- rgr, 9-Sep-02.]
	    (list 'setf		(concat "^" <ws*> "(def\\(setf" <ws+> "%s" <tws>
					"\\|un" <ws+> "(setf" <ws+> "%s" <ws*>
					")\\)"))
	    ;; this describes the structure of the rest; it has an extra "%s" to
	    ;; insert the name of the defining form.
	    (list '<standard>	(concat "^" <ws*> "(%s" <ws+> "%s" <tws>))
	    '(function		<standard> "\\(defun\\|defmacro\\)")
	    '(macro		<standard> defmacro)
	    '(type		<standard> deftype)
	    '(class		<standard> defclass)
	    '(method		<standard> defmethod)
	    '(generic-function	<standard> defgeneric)))
  "*Alist of (definition-name regexp-template), where definition-name is
an elisp symbol, and regexp-template is a format string that, given a
string that matches a symbol, returns a regexp that match that kind of
definitions of that symbol.  If regexp-template is a symbol, then that
is an indirect reference to another named pattern (to save having to
look at too many nearly identical regexps).  Anything after the
regexp-template is prefixed to the definition name in the format call.")

(defvar lisp-match-deffoo-template
	;; originally "^[ \t\n]*(def[^ \t\n]*[ \t\n]+(?%s[ \t\n(]+"
	(format (car (cdr (assoc '<standard> ilisp-cl-source-locator-patterns)))
		(concat "def" lisp-match-name)
		"(?%s")
  "Generates a regexp that matches a line that starts with
'(def<something> (?name', with liberal whitespace allowance, when given
to format with a suitable name pattern.")

(defun lisp-make-standard-cl-defn-regexp (name-regexp type)
  "Look for a named definition of the given type, assuming that it is
defined using the obvious Lisp syntax.  Always searches for something,
falling back on '(<type> <name-regexp> ...)' if nothing specific is known about
the definition type.  Use ilisp-cl-source-locator-patterns to customize
this."
  (let* ((type (intern type))
	 (entry (assoc type ilisp-cl-source-locator-patterns))
	 (pattern (car (cdr entry)))
	 (extras (cdr (cdr entry))))
    (cond ((null entry)
	    ;; no specific pattern; try to match "(<type> <name-regexp> ...".
	    (setq pattern '<standard>)
	    (setq extras (list (lisp-make-gf-name-matcher type)))))
    (cond ((symbolp pattern)
	    ;; internal reference to another pattern template.
	    (setq entry (or (assoc pattern ilisp-cl-source-locator-patterns)
			    (error "Internal error: %S is missing %S."
				   'ilisp-cl-source-locator-patterns
				   pattern)))
	    (setq pattern (car (cdr entry)))))
    (if extras
	(apply (function format) pattern
	       (append extras (list name-regexp)))
	(format pattern name-regexp))))

(defun lisp-make-whitespace-and-symbol-matcher (name)
  ;; notice that we carefully preserve the package prefix as specified if a
  ;; keyword, and allow any or no package prefix otherwise.
  (let ((string (cond ((stringp name) name)
		      ((symbolp name) (symbol-name name))
		      ;; don't die horribly.
		      (t (format "%s" name)))))
    (cond ((string-match "^:" string)
	    (concat "[ \t\n]*" (regexp-quote string)))
	  ((string-match ":+" string)
	    (concat lisp-ws-and-opt-pkg-regexp
		    (substring string (match-end 0))))
	  (t
	    (concat lisp-ws-and-opt-pkg-regexp string)))))

(defvar lisp-conc-name-regexp
	(concat "(:conc-name" ilisp-whitespace+ "\\(" lisp-match-name "\\)")
  "Matches '(:conc-name <foo>' form, where <foo> is returned by the
third subexpressions.")

(defun lisp-find-accessor-in-random-defstruct (accessor-symbol)
  ;; look for a defstruct that has (a) a structure name or :conc-name attribute
  ;; compatible that matches some prefix of accessor-name, and (b) that also
  ;; contains a slot with the remainder of accessor-name.  This has to be done
  ;; algorithmically, because returning a single regexp that could cover all the
  ;; possibilities would be horrendous.  The basic premise is probably pretty
  ;; sound, but the regexps may be a bit sloppy in places.
  (let* ((accessor-name (downcase (if (stringp accessor-symbol)
				      accessor-symbol
				      (lisp-symbol-name accessor-symbol))))
	 (template (car (cdr (assoc 'structure
				    ilisp-cl-source-locator-patterns))))
	 (pattern (if (string-match "%" template)
		      (substring template 0 (match-beginning 0))
		      (error "internal error")))
	 (found-p nil))
    ;; search for all defstructs, parsing out name & :conc-name.
    (while (and (not found-p)
		(not (eobp))
		(re-search-forward pattern nil 'move))
      (let* ((structure-start (match-beginning 0))
	     (structure-name (let ((start (point)))
			       (forward-sexp)
			       (buffer-substring-no-properties start (point))))
	     (structure-end (save-excursion
			      (end-of-defun-lisp)
			      (point)))
	     (conc-name-hit
	       (save-excursion
		 (and (re-search-forward lisp-conc-name-regexp structure-end t)
		      (lisp-strip-package-prefix-from-symbol
		        (downcase (match-string 3))))))
	     ;; the prefix should be exactly what a compliant Common Lisp would
	     ;; decide (ignoring case).
	     (prefix (cond ((null conc-name-hit)
			     ;; no :conc-name (or :conc-name without an arg)
			     ;; means use the structure name.
			     (concat (lisp-strip-package-prefix-from-symbol
				       structure-name)
				     "-"))
			   ((string-equal conc-name-hit "nil")
			     ;; "(:conc-name nil)" means do not prepend
			     ;; anything, so the slot name is the accessor name.
			     nil)
			   (t conc-name-hit)))
	     (slot-name (cond ((null prefix)
				accessor-name)
			      ((and (> (length accessor-name) (length prefix))
				    (string-equal (substring accessor-name
							     0 (length prefix))
						  prefix))
				(substring accessor-name (length prefix))))))
	(setq found-p (and slot-name
			   (re-search-forward (regexp-quote slot-name)
					      structure-end t)))
	'(message "Found %S (end %S, prefix %S) slot-name %S, found %S."
		 structure-name structure-end prefix slot-name found-p)))
    (not (null found-p))))

;;; Making regexps that match specific defmethod forms.

(defun lisp-match-defmethod-specializer (name)
  ;; oops; name might actually be the list "(eql :foobar)" . . .
  (let* ((whitespace "[ \t\n]")
	 (name (lisp-strip-package-prefix-from-symbol name))
	 (matcher (concat "(" whitespace "*" lisp-match-name
			  lisp-ws-and-opt-pkg-regexp (regexp-quote name)
			  whitespace "*)")))
    (concat whitespace "*"
	    (if (member name '("T" "t"))
		(concat "\\(" matcher "\\|" lisp-match-name "\\)")
		matcher))))

(defun lisp-pop-trivial-method-specializers (specializer-list)
  ;; this is so we don't have to match "T T T)" at the end of the arglist for
  ;; unspecialized args.  The list is understood to be reversed.
  (cond ((null specializer-list) nil)
	((member (car specializer-list) '(t T))
	  (lisp-pop-trivial-method-specializers (cdr specializer-list)))
	(t
	  specializer-list)))

(defun lisp-make-gf-name-matcher (gf-name)
  ;; Make a regular expression that matches the generic function name ignoring
  ;; packages, also handling (setf foo) names.  Does not match leading or
  ;; trailing whitespace.
  (if (consp gf-name)
      ;; assumes a proper list.
      (concat "(" ilisp-whitespace*
	      (lisp-make-gf-name-matcher (car gf-name))
	      (apply (function concat)
		     (mapcar (function (lambda (x)
			       (concat ilisp-whitespace+
				       (lisp-make-gf-name-matcher x))))
			     (cdr gf-name)))
	      ilisp-whitespace* ")")
      (concat lisp-optional-package-regexp
	      (regexp-quote (lisp-strip-package-prefix-from-symbol gf-name)))))

(defun lisp-make-cl-method-definition-regexp (spec-string)
  (let* ((tail (cdr (car (read-from-string spec-string))))
	 (gf-name (car tail))
	 (setf-function-p (and (consp gf-name)
			       (eq (car gf-name) 'setf)))
	 (gf-name-matcher (lisp-make-gf-name-matcher gf-name))
	 (rev-tail (reverse (cdr tail)))
	 (specializers (car rev-tail))
	 (nontrivial-specializers (reverse
				    (lisp-pop-trivial-method-specializers
				      (reverse specializers))))
	 (method-qualifiers (reverse (cdr rev-tail)))
	 (whitespace "[ \t\n]")
	 (defmethod-matcher
	   (apply (function concat)
		  "^(defmethod"
		  (if setf-function-p ilisp-whitespace* ilisp-whitespace+)
		  gf-name-matcher
		  (append (mapcar (function
				    lisp-make-whitespace-and-symbol-matcher)
				  method-qualifiers)
			  (list whitespace "*(")
			  (mapcar (function lisp-match-defmethod-specializer)
				  nontrivial-specializers)))))
    '(message "[Got %S]" (list gf-name method-qualifiers
			      nontrivial-specializers))
    (if (and (null method-qualifiers)
	     (= (length specializers)
		(if setf-function-p 2 1)))
	;; could also be an reader/writer/accessor generated by a defclass form.
	;; [***bug***: this doesn't insist on a hit in the right defclass form.
	;; more rigorous would be to do a lisp-find-accessor-in-random-defstruct
	;; style search only when hints require it.  -- rgr, 10-Sep-02.]
	(concat defmethod-matcher "\\|"
		"\\(:accessor\\|:writer\\|:reader\\)"
		ilisp-whitespace+
		(if setf-function-p
		    (lisp-make-gf-name-matcher (car (cdr gf-name)))
		    gf-name-matcher)
		ilisp-terminal-whitespace)
	;; just look for defmethod forms.
	defmethod-matcher)))

;;; Visiting source file possibilities.

(defun lisp-make-cl-method-fspec-finder (definition)
  (lisp-make-cl-method-definition-regexp
    (ilisp-defn-spec-name-string definition)))

(defun lisp-make-cl-pcl::fast-method-fspec-finder (name)
  ;; hack for pcl::fast-method function specs.
  (lisp-make-cl-method-fspec-finder name))

(defun lisp-make-cl-setf-fspec-finder (definition)
  ;; This also looks for defstruct accessors if given the right hint.
  (let* ((setf-fn-spec (ilisp-defn-spec-name-string definition))
	 (function-spec (car (read-from-string setf-fn-spec)))
	 (symbol (car (cdr function-spec)))
	 (name-string (if (symbolp symbol)
			  (lisp-strip-package-prefix-from-symbol symbol)
			  (error "'%S' is a bad SETF function spec."
				 setf-fn-spec)))
	 (type (ilisp-defn-spec-type definition))
	 (parent (ilisp-defn-spec-parent definition)))
    (cond ((and parent
		(string-equal (car parent) "structure"))
	    (lisp-find-accessor-in-random-defstruct name-string))
	  ((string-equal type "generic-function")
	    (lisp-make-standard-cl-defn-regexp
	      (lisp-make-gf-name-matcher function-spec)
	      type))
	  (t
	    ;; defsetf or defun with (setf foo) spec.
	    (let ((name-regexp (concat lisp-optional-package-regexp
				       (regexp-quote name-string)))
		  (setf-entry (assoc 'setf ilisp-cl-source-locator-patterns)))
	      (lisp-re nil (car (cdr setf-entry))
		       name-regexp name-regexp))))))

;;; Common Lisp definition matching interface.

(defun lisp-cl-symbolic-name-regexp (definition)
  ;; helper for lisp-locate-clisp below; takes care of cases where "symbol" is
  ;; actually known to be Lisp symbol.  we ignore its package, though.
  (let* ((symbol (ilisp-defn-spec-name definition))
	 (type (ilisp-defn-spec-type definition))
	 (name (lisp-symbol-name symbol))
	 (name-regexp (concat lisp-optional-package-regexp
			      ;; symbol-name of the read-from-string result
			      ;; undoes lisp-read-cl-syntax quoting.
			      (regexp-quote
			        (symbol-name (car (read-from-string name))))))
	 (parent (ilisp-defn-spec-parent lisp-last-definition))
	 (hints (ilisp-defn-spec-hints lisp-last-definition)))
    (cond ((or (not (equal type "function"))
	       (and (null parent) (null hints)))
	    ;; all nonfunction cases (for which no hints are supported yet), or
	    ;; a simple function case, named with a symbol either way.  These
	    ;; are totally trivial (we hope).
	    (lisp-make-standard-cl-defn-regexp name-regexp type))
	  ;; Problem function (with an ordinary (symbolic) name).
	  ((and parent
		(string-equal (car parent) "structure"))
	    ;; desperate heuristic #1: look inside all defstruct forms.
	    (lisp-find-accessor-in-random-defstruct symbol))
	  ;; Problem case without any applicable hacks.
	  (t
	    (lisp-make-standard-cl-defn-regexp name-regexp type))
	  ;; [move this to lisp-locate-clisp somewhere?  -- rgr, 4-Oct-02.]
	  (t
	    (message "[Pulling out all the stops.]")
	    ;; Search for a "(defsomething (?name" form.  This is way too broad
	    ;; to use in general, but things are pretty desperate at this point.
	    (format lisp-match-deffoo-template name-regexp)))))

(defun lisp-make-cl-definition-regexp (definition)
  (let ((name (ilisp-defn-spec-name-string definition)))
    (cond ((string-match "^(\\([a-zA-Z0-9-_:]+\\)[ \t\n]+" name)
	    ;; 'interesting' function naming cases, including setf & method
	    ;; function specs.  we try to dispatch on the car of the list, which
	    ;; may include a package prefix.
	    (let* ((spec-type (downcase (match-string 1 name)))
		   (finder (intern (concat "lisp-make-cl-" spec-type
					   "-fspec-finder"))))
	      (if (and (not (fboundp finder))
		       (string-match ":+" spec-type))
		  ;; try without the package prefix.
		  (setq finder
			(intern (concat "lisp-make-cl-"
					(substring spec-type (match-end 0))
					"-fspec-finder"))))
	      (cond ((fboundp finder)
		      (funcall finder lisp-last-definition))
		    (t
		      (message "Oops; don't know how to find %S %s definitions."
			       spec-type (ilisp-defn-spec-type definition))
		      (sit-for 1)
		      nil))))
	  (t
	    (lisp-cl-symbolic-name-regexp lisp-last-definition)))))

(defun lisp-locate-clisp (symbol type first-p back-p)
  "Try to find SYMBOL's TYPE definition in the current Common Lisp
buffer.  Return true if sucessful, and move point to the match; else
return nil and move point to the end of the buffer.  lisp-locate-clisp
acts as the top-level classifier of Common Lisp definition types,
dispatching to other functions to implement the appropriate search
tactics for the definition type, and using any hints or parentage data
we may find in the lisp-locate-definition global.

   [This was changed to interpret non-symbol definition names, use hint
and parent heuristics, and centralize the regexp search.  Somewhat
kludgily, it requires that lisp-last-definition be set to the
ilisp-defn-spec structure for the passed symbol and type.  -- rgr,
25-Mar-03.]

   [The FIRST-P and BACK-P arguments are ignored, since they are now
always t and nil, respectively.  lisp-locate-clisp is one of several
possible values returned by \(ilisp-value 'ilisp-locator), so we need to
preserve the protocol; eventually, this should be fixed.  -- rgr,
4-Oct-02.]"
  (if lisp-find-definition-verbose-p
      (message "[doing %S.]"
	       (list 'lisp-locate-clisp symbol type first-p back-p)))
  ;; [should check for lisp-last-definition validity here.  -- rgr, 4-Oct-02.]
  (if (equal type "any")
      ;; [may have to support this, for non-Lisp search.  -- rgr, 7-Sep-02.]
      (error "shouldn't have been given type %S, for %S." type symbol))
  (let* ((case-fold-search t)
	 (regexp (lisp-make-cl-definition-regexp lisp-last-definition)))
    (cond ((symbolp regexp)
	    ;; lisp-make-cl-definition-regexp already did the searching for us.
	    regexp)
	  ((not (stringp regexp))
	    (error "bug: %S should have returned a regexp string."
		   'lisp-make-cl-definition-regexp))
	  ((re-search-forward regexp nil t)
	    (goto-char (match-beginning 0))
	    t)
	  (t
	    ;; [use fallback strategy?  -- rgr, 4-Oct-02.]
	    nil))))

(defun lisp-set-up-for-search-in-new-file (file)
  ;; Go to the file's buffer, reading it if necessary (and possible), and move
  ;; to the start after setting lisp-last-file and lisp-last-point.  Returns t
  ;; if successful, and returns nil and prints a message if not.
  (let ((target-buffer (get-file-buffer file)))
    (cond ((or target-buffer
	       (file-readable-p file))
	    (set-buffer (or target-buffer
			    (find-file-noselect file)))
	    (setq lisp-last-file (buffer-file-name))
	    (setq lisp-last-point (point))
	    (goto-char (point-min))
	    t)
	  (t
	    (message "File %S doesn't exist!" file)
	    (sit-for 1)
	    nil))))

(defun lisp-locate-definition-in-file (locator defn-spec &optional pop)
  ;; [may no longer do POP correctly.  -- rgr, 4-Sep-02.]
  "Use LOCATOR to find the next DEFN-SPEC (an ilisp-defn-spec instance).
Search starts at POINT, optionally BACKWARDS and POP to buffer.  Return T
if successful."
  (if lisp-find-definition-verbose-p
      (message "[doing %S.]"
	       (list 'lisp-locate-definition-in-file locator defn-spec pop)))
  ;; set this whether we succeed or not, because it will be used to generate a
  ;; message in certain failure cases.  [and, kludgily, we refer to it freely
  ;; in locator functions, which allows us to avoid patching all locators to
  ;; pass the defn-spec instead of symbol & type.  yech.  -- rgr, 7-Sep-02.]
  (setq lisp-last-definition defn-spec)
  (let* ((symbol (ilisp-defn-spec-name defn-spec))
	 (name (ilisp-defn-spec-name-string defn-spec))
	 (type (ilisp-defn-spec-type defn-spec))
	 ;; [we need this because lisp-set-up-for-search-in-new-file does
	 ;; set-buffer for us.  -- rgr, 4-Oct-02.]
	 (old-buffer (current-buffer))
	 ;; go to the right place in the file buffer (if we can).
	 (result (lisp-set-up-for-search-in-new-file
		   (ilisp-defn-spec-file defn-spec))))
    (cond ((null result)
	    nil)
	  ((progn
	     (message "Searching %s for %s %s" (buffer-file-name) type name)
	     (setq result (funcall locator symbol type t nil))
	     ;; (message "[locator returned %S.]" result)
	     result)
	    ;; found it.
	    ;; [this message would be clobbered by the "Mark set" message
	    ;; anyway.  -- rgr, 4-Oct-02.]
	    ;; (message "Found %s %s definition" type name)
	    (push-mark lisp-last-point)
	    (switch-to-buffer (current-buffer))
	    (save-excursion
	      (or (bolp)
		  (beginning-of-defun))
	      ;; reposition the start of the definition at the top of the
	      ;; screen, regardless of where point ended up.
	      ;; [if there are comments above this definition, and the window is
	      ;; already positioned to show them, then this hides the comments
	      ;; again.  we need a way of telling reposition-window-lisp never
	      ;; to hide comments.  -- rgr, 25-Mar-03.]
	      (reposition-window-lisp))
	    t)
	  (t
	    ;; not found in this buffer.  restore point in searched buffer,
	    ;; before returning to the old buffer.  [shouldn't this be in an
	    ;; unwind-protect cleanup?  -- rgr, 4-Oct-02.]
	    (goto-char lisp-last-point)
	    (set-buffer old-buffer)
	    nil))))

;;; Getting definition specifications out of the *Edit-Definitions* buffer.

(defun lisp-parse-parent-hint (line-end)
  ;; The line-end is the part after "is inside +", and may contain the name
  ;; and/or type of the parent definition.  This parses what
  ;; lisp-insert-defn-inside-defn-comment generates.
  (cond ((string-match "the definition of :?\\([^ ]+\\) +\\(.+\\)" line-end)
	  (let ((type (downcase (match-string 1 line-end)))
		(fn-spec (match-string 2 line-end)))
	    (list type (lisp-string-to-symbol fn-spec))))
	((string-match "a \\(.+\\) definition" line-end)
	  (list nil (lisp-string-to-symbol (match-string 1 line-end))))
	((string-match "some ?:\\([^ ]+\\) definition" line-end)
	  (list (downcase (match-string 1 line-end)) nil))
	((string-match "another definition" line-end)
	  (list nil nil))
	(t
	  (message "Can't parse %S as a 'parent hint.'" line-end)
	  (sit-for 1)
	  ;; return the result for "another definition", so we still turn on all
	  ;; the extra heuristics.
	  (list nil nil))))

(defun lisp-find-next-possibility (back-p)
  "Return the next source file location in *Edit-Definitions* as an
ilisp-defn-spec structure, or nil if none.  Returns the previous if
back-p is non-nil."
  ;; [bug: back-p doesn't fully work.  -- rgr, 15-Aug-02.]
  (let ((file t)
	(parent-hint nil)
	(result nil)
	(original-buffer (current-buffer)))
    ;; [can't use save-excursion because we have to update point in the
    ;; definitions buffer.  -- rgr, 6-Aug-02.]
    (if lisp-find-definition-verbose-p
	(message "[lfnp %swards in %s:]"
		 (if back-p "back" "for") original-buffer))
    (unwind-protect
	 (progn
	   (set-buffer (or (get-buffer "*Edit-Definitions*")
			   (error "Bug:  No *Edit-Definitions* buffer.")))
	   (if back-p 
	       (forward-line -1))
	   (while (not (or result
			   (if back-p (bobp) (eobp))))
	     (if back-p 
		 (forward-line -1))
	     (cond ((looking-at "\n"))
		   ((looking-at "^;+ *\\(.*\\)")
		     (cond ((not back-p)
			     (message "%s" (match-string 1))
			     (sit-for 1))))
		   ((looking-at "^!+ *\\(.*\\)")
		     (cond (back-p
			     ;; [***bug***: we don't find these right when
			     ;; working backwards.  -- rgr, 5-Sep-02.]
			     )
			   (t
			     (let ((match (match-string 1)))
			       (message "%s" match)
			       (sit-for 1)
			       ;; if there are several nested parent hints, take
			       ;; only the first, as that will be the outermost.
			       (if (null parent-hint)
				   (setq parent-hint
					 (lisp-parse-parent-hint match)))))))
		   ((looking-at "^:?\\([^ ]+\\) +\\(.+\\) in file \\(.+\\)")
		     ;; normal definition match.
		     (let ((type (downcase (match-string 1)))
			   (fn-spec (match-string 2))
			   (file (match-string 3)))
		       (setq result
			     (make-ilisp-defn-spec
			       :name (lisp-string-to-symbol fn-spec)
			       :name-string fn-spec
			       :type type :file file :parent parent-hint)))))
	     (if (not back-p)
		 (forward-line)))
	   (if back-p
	       (forward-line)))
      (set-buffer original-buffer))
    result))

(defun lisp-next-definition (&optional back pop quiet-p)
  "Go to the next definition from *Edit-Definitions*.  Movement is BACK
with prefix and POPping.  Return 'first if found first time, 'none if no
definition ever, and nil if no more definitions are found.  So the
normal result is a series of first's, followed by a nil, or a 'none if a
definition couldn't be found in source.  [The return value is
semi-historical (and may be broken).  lisp-next-definition used to
return t if another definition was found in the same file, but this is
no longer possible, as lisp-next-definition iterates over definitions
instead of files.  -- rgr, 25-Mar-03.]"
  ;; find the next possibility, which specifies a file.
  (let ((possibility (lisp-find-next-possibility back)))
    (if lisp-find-definition-verbose-p
	(message "%S returned %S." 'lisp-find-next-possibility possibility))
    (let ((result (cond ((null possibility) nil)
			((lisp-locate-definition-in-file
			   lisp-last-locator possibility
			   (prog1 pop (setq pop nil)))
			  'first)
			(t 'none))))
      ;; Generate a message in the "no more" cases; if something had been found,
      ;; then lisp-locate-definition-in-file will have issued a message to that
      ;; effect for us.
      (if (and (not quiet-p)
	       (member result '(nil none)))
	  ;; the outer save-excursion restores the current buffer; the inner one
	  ;; preserves the point in the "*Edit-Definitions*" buffer.
	  (save-excursion
	    (set-buffer "*Edit-Definitions*")
	    (save-excursion
	      (goto-char (point-min))
	      (let ((message (if (looking-at "^;+ +\\(any +\\)?\\(.+\\):")
				 (match-string 2)
				 "definitions")))
		(if (null result)
		    (message "No more %s." message)
		    (message "Can't find %s." message))))))
      (if lisp-find-definition-verbose-p
	  (message "%S returns %S." 'lisp-next-definition result))
      result)))

(defun next-definition-lisp (back &optional pop)
  "Edit the next definition from *Edit-Definitions*.
Movement is BACK with prefix and optionally POPping or call
'tags-loop-continue' if using tags."
  (interactive "P")
  (cond (lisp-using-tags
	  ;; [i don't think this tells us whether we've succeeded or not.  --
	  ;; rgr, 31-Jul-02.]
	  (tags-loop-continue)
	  'maybe)
	(lisp-search
	  (error "lisp-search is %S, but this feature is broken." lisp-search))
	(t
	  (lisp-next-definition back pop))))

;;; Putting definition specifications in the *Edit-Definitions* buffer.

(defvar lisp-edit-definitions-mode-map
	(let ((map (make-sparse-keymap)))
	  ;; [these are ok in compilation-mode-map, but don't help here.]
	  ;; (define-key map " " 'scroll-up)
	  ;; (define-key map "\^?" 'scroll-down)
	  (define-key map "\r" 'lisp-this-definition)
	  ;; This is for compatibility with ACL eli.
	  (define-key map "." 'lisp-this-definition)
	  map)
  "Mode map for *Edit-Definitions* buffers.")

(defun lisp-edit-definitions-mode ()
  "Major mode for Lisp definition buffers.
To visit the source for a given definition,
move point to the appropriate line and type \\[compile-goto-error].

Runs `lisp-edit-definitions-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lisp-edit-definitions-mode-map)
  (setq major-mode 'lisp-edit-definitions-mode
	mode-name "Edit Definitions")
  ;; [this would be really nice . . .  -- rgr, 12-Feb-03.]
  '(set (make-local-variable 'revert-buffer-function)
       'lisp-edit-definitions-revert-buffer)
  (run-hooks 'lisp-edit-definitions-mode-hook))

(defun lisp-this-definition ()
  (interactive)
  "Visit the source of the definition named on this line."
  (beginning-of-line)
  (lisp-next-definition))

(defun fix-source-filenames ()
  "Apply the 'ilisp-source-directory-fixup-alist' to the current buffer,
normally *Edit-Definitions*.  The aim is to change any distributed
source-file locations to point to local source file locations.

See 'ilisp-source-directory-fixup-alist'."
  ;; [now supports more than one hit per regexp per buffer.  -- rgr, 31-Jul-02.]
  (let ((alist (ilisp-value 'ilisp-source-directory-fixup-alist t)))
    (save-excursion
      (while alist
	(let* ((cons (car alist))
	       (pattern (car cons)) (replacement (cdr cons)))
	  (goto-char (point-min))
	  (while (re-search-forward pattern nil t)
	    (if (stringp replacement)
		;; [this is an enhancement with respect to the
		;; ilisp-source-directory-fixup-alist documentation string.  --
		;; rgr, 27-Aug-02.]
		(replace-match replacement t nil)
		(funcall replacement))))
	(setq alist (cdr alist))))))

(defun lisp-handle-comment-message (comment-strings)
  ;; note that we lose if any of these strings have newlines.
  (while comment-strings
    (let ((thing (car comment-strings)))
      (insert ";; "
	      (if (stringp thing) thing (format "%S" thing))
	      "\n"))
    (setq comment-strings (cdr comment-strings))))

(defun lisp-definition-spec-type-name-string (defn-spec)
  (let ((type-name (ilisp-getf (cdr defn-spec) ':type)))
    (and type-name
	 (capitalize (symbol-name type-name)))))

(defun lisp-insert-defn-inside-defn-comment (child parent &optional is-inside)
  ;; helper for lisp-handle-definition-message, below.
  (let* ((child-name (ilisp-getf (cdr child) ':name))
	 (child-type (lisp-definition-spec-type-name-string child))
	 (parent-name (ilisp-getf (cdr parent) ':name))
	 (parent-type (lisp-definition-spec-type-name-string parent))
	 (both-defined-p (and parent-name parent-type)))
    (insert (if both-defined-p ";; " "!! ")
	    (or child-type "???") (format " %S " child-name)
	    (or is-inside "is inside") " "
	    (cond (both-defined-p
		    (format "the definition of %s %S"
			    parent-type parent-name))
		  (parent-name
		    (format "a %S definition" parent-name))
		  (parent-type
		    (format "some %s definition" parent-type))
		  (t
		    "another definition."))
	    "\n")
    both-defined-p))

(defun lisp-insert-defn-files (defn-spec file-tail)
  ;; helper for lisp-handle-definition-message, below.
  (let* ((name (ilisp-getf (cdr defn-spec) ':name))
	 (type-string (lisp-definition-spec-type-name-string defn-spec))
	 (preamble (format "%s %S" type-string name)))
    (or (and name type-string)
	;; [debugging hack.  -- rgr, 4-Sep-02.]
	(error "Malformed definition message %S." defn-spec))
    (if file-tail
	(while file-tail
	  (insert preamble " in file " (car file-tail) "\n")
	  (setq file-tail (cdr file-tail)))
	(insert ";; " preamble
		" has no known source files.\n"))))

(defun lisp-handle-definition-message (defn-spec)
  ;; Helper for lisp-set-up-definitions-buffer, below.  If we find a parent
  ;; definition, then an "X inside of Y" comment is generated.  [Bug: this
  ;; currently fails if definitions are nested more than two deep.  -- rgr,
  ;; 5-Sep-02.]  Note that if the parent definition has both type and name, then
  ;; we put *that* in the buffer, since the parent will be easier to find.
  (let ((comment (ilisp-getf (cdr defn-spec) ':comment))
	(parent (ilisp-getf (cdr defn-spec) ':parent)))
    (if comment
	(lisp-handle-comment-message (cdr comment)))
    (let ((use-parent-instead-p
	    (and parent
		 (lisp-insert-defn-inside-defn-comment defn-spec parent))))
      (lisp-insert-defn-files (if use-parent-instead-p
				  parent
				  defn-spec)
			      (or (if parent
				      ;; presumably, these are more
				      ;; authoritative.
				      (ilisp-getf (cdr parent) ':files))
				  (ilisp-getf (cdr defn-spec) ':files))))))

(defun lisp-set-up-definitions-buffer (initial-message definition-specs
						       &optional buffer-name)
  "Set up the *Edit-Definitions* buffer with INITIAL-MESSAGE to edit
DEFINITION-SPECS, which must be a new-style list of :DEFINITION spec
items."
  ;; [for some odd reason, initializing lisp-last-locator is the caller's
  ;; responsibility.  -- rgr, 4-Sep-02.]
  (setq lisp-using-tags nil)
  (setq lisp-last-file nil)
  (save-excursion
    (set-buffer (get-buffer-create (or buffer-name "*Edit-Definitions*")))
    (erase-buffer)
    (lisp-edit-definitions-mode)
    (and initial-message
	 ;; [putting an extra semi on this may make it easier to identify.  --
	 ;; rgr, 4-Sep-02.]
	 (insert ";;; " initial-message "\n\n"))
    (let ((tail definition-specs))
      (while tail
	(let* ((entry (car tail))
	       (message-type (car entry)) (message-body (cdr entry)))
	  ;; canonicalize case.
	  (if (and message-type
		   (symbolp message-type))
	      (setq message-type
		    (intern (downcase (symbol-name message-type)))))
	  ;; now try to decode it.
	  (cond ((eq message-type ':comment)
		  (lisp-handle-comment-message message-body))
		((eq message-type ':definition)
		  (lisp-handle-definition-message entry))
		(t
		  (insert ";; [oops; unknown message type "
			  (format "%S" message-type)
			  "]\n"))))
	(setq tail (cdr tail))))
    (fix-source-filenames)
    (goto-char (point-min))
    (and initial-message
	 (forward-line 2))
    (set-buffer-modified-p nil)))

;;; Getting definition specifications back from the inferior Lisp.

(defvar lisp-inferior-source-definitions-cache nil
  "internal cache used by lisp-find-inferior-source-definitions, below.")

(defun lisp-parse-and-cache-source-definitions (type symbol files
						     &optional no-cache-p)
  ;; handles conversion from the old list-of-pathnames format.  helper for
  ;; lisp-find-inferior-source-definitions, below.
  (let* ((read-result (lisp-read-cl-syntax files))
	 (file-specs (car read-result)))
    (if (or (cdr read-result)
	    (stringp file-specs))
	(let ((name (car (lisp-read-cl-syntax (lisp-buffer-symbol symbol)))))
	  ;; old style file-name string(s); there may be more than one of these.
	  ;; convert to new format.
	  (setq file-specs (list (list ':definition ':name name
				       ':type (intern type)
				       ':files read-result)))))
    (or no-cache-p
	(setq lisp-inferior-source-definitions-cache
	      (cons (cons symbol type) file-specs)))
    file-specs))

(defun lisp-find-inferior-source-definitions (symbol type &optional no-cache-p)
  ;; Utility function for querying the Lisp about source information.
  ;; Returns nil for most error cases.
  (let ((symbol-name (lisp-symbol-name symbol))
	(command (ilisp-value 'ilisp-find-source-command t))
	(source nil))
    (cond ((and (not no-cache-p)
		lisp-inferior-source-definitions-cache
		(equal (car lisp-inferior-source-definitions-cache)
		       (cons symbol type)))
	    (cdr lisp-inferior-source-definitions-cache))
	  ((null command)
	    nil)
	  ((not (comint-check-proc ilisp-buffer))
	    (error "The inferior lisp in %s is no longer running."
		   ilisp-buffer))
	  ((let ((command (format command symbol-name
				  (or (lisp-symbol-package symbol)
				      (lisp-buffer-package))
				  type)))
	     ;; (message "[sending %S.]" command)
	     (setq source
		   (ilisp-send
		     command
		     (concat "Finding " type " "
			     (lisp-buffer-symbol symbol) " definitions")
		     'source)))
	    (let* ((result (lisp-last-line source))
		   (return-value (and (stringp (car result))
				      (downcase (car result))))
		   (files (cdr result))
		   (source-ok (and (not (ilisp-value 'comint-errorp t))
				   (string-equal "t" return-value))))
	      (cond ((not source-ok)
		      ;; normally, this will be an error message.
		      (message "%s" return-value)
		      (sit-for 2)
		      nil)
		    (t
		      (lisp-parse-and-cache-source-definitions type symbol
							       files))))))))

;;; The edit-definitions-lisp command and friends.

(defun lisp-make-edit-files-definitions (type name edit-files)
  ;; Compatibility.
  (let ((files-origin "lisp-directory files"))
    (if (eq edit-files t)
	(let ((buffers (buffer-list)))
	  (setq edit-files nil)
	  (setq files-origin "all Lisp buffers")
	  (save-excursion 
	    (while buffers
	      (set-buffer (car buffers)) 
	      (if (and (memq major-mode lisp-source-modes)
		       (buffer-file-name))
		  (setq edit-files (cons (buffer-file-name) edit-files)))
	      (setq buffers (cdr buffers))))
	  (setq edit-files (nreverse edit-files))))
    (if (null edit-files)
	(error "No files to edit; see the lisp-directory command."))
    (list (list ':definition ':type (intern type) ':name (intern name)
		':comment (list (format "Searching through %s to find %s %S"
					files-origin type name))
		':files edit-files))))

(defun lisp-edit-definitions-normal (symbol type &optional stay locator)
  (let* ((file-specs (lisp-find-inferior-source-definitions symbol type))
	 ;; case-fold-search is always buffer-local when set, so binding this
	 ;; here may not mean much.  -- rgr, 14-Sep-02.
	 (case-fold-search t)
	 (try-tags-p lisp-fall-back-on-find-tags)
	 (found-p nil)
	 (name (lisp-buffer-symbol symbol)))
    ;; (message "[got files %S.]" file-specs)
    (cond (file-specs
	    ;; We've gotten something useful from the inferior lisp.
	    (setq lisp-last-locator (or locator (ilisp-value 'ilisp-locator)))
	    (lisp-set-up-definitions-buffer
	      (format "%s %s definitions in %s:" type name ilisp-buffer)
	      file-specs)
	    (setq found-p (next-definition-lisp nil t))))
    (cond ((and (not found-p)
		nil	;; [disabled until fixed.  -- rgr, 17-Dec-03.]
		lisp-edit-files)
	    ;; No luck from the inferior lisp; search the available buffers.
	    ;; [note that this does not work now, since it tries to pass type
	    ;; "any" to lisp-locate-clisp, which no longer accepts this.  --
	    ;; rgr, 14-Sep-02.]
	    (setq lisp-last-locator (or locator (ilisp-value 'ilisp-locator)))
	    (lisp-set-up-definitions-buffer
	      (format "%s %s definitions in %s:" type name 'lisp-edit-files)
	      (lisp-make-edit-files-definitions type name lisp-edit-files))
	    (setq found-p (next-definition-lisp nil t))))
    ;; Now check to see if tags searching is needed/wanted.
    (cond (found-p)
	  ((and (or tags-file-name tags-table-list)
		(if (eq try-tags-p ':ask)
		    (yes-or-no-p (format "Can't find %s %s; use tags? "
					 (or type "any definitions of")
					 name))
		    try-tags-p))
	    ;; Use tags.  This will generally lose on packages.
	    (let ((symbol-name (lisp-symbol-name symbol)))
	      (setq lisp-using-tags t)
	      (if (string-match "Lucid" emacs-version)
		  (find-tag symbol-name stay)
		  (find-tag symbol-name nil stay))))
	  (t
	    (message "Can't find %s %s."
		     (or type "any definitions of")
		     name)
	    nil))))

(defun lisp-find-inferior-definition-types (symbol)
  ;; Returns a list of unique definition types (as lowercase strings) known to
  ;; the inferior Lisp for symbol.
  (let ((tail (lisp-find-inferior-source-definitions symbol "any"))
	(type nil)
	(result nil))
    (while tail
      (let ((entry (car tail)))
	(if (and (consp entry)
		 (memq (car entry) '(:DEFINITION :definition))
		 (symbolp (setq type (ilisp-getf (cdr entry) ':type))))
	    (let ((type-string (downcase (symbol-name type))))
	      (if (string-match "^:" type-string)
		  (setq type-string (substring type-string 1)))
	      (or (member type-string result)
		  (setq result (cons type-string result))))))
      (setq tail (cdr tail)))
    (sort result (function string-lessp))))

(defun lisp-ask-for-definition-to-edit (current-prefix-arg)
  ;; Used in the edit-definitions-lisp 'interactive' form.  May also ask for a
  ;; definition type as well, if not one of the continuation cases, and if the
  ;; inferior lisp knows how to find more than one such definition (e.g. a
  ;; function with the same name as a global variable).
  (let* ((numeric-arg (and current-prefix-arg
			   (prefix-numeric-value current-prefix-arg)))
	 (function (lisp-function-name)))
    (cond ((or (null numeric-arg)
	       (not (get-buffer "*Edit-Definitions*")))
	    (let* ((defn-spec
		     (let ((result (ilisp-read
				     (format "Edit Definition [%s]: "
					     (lisp-buffer-symbol function)))))
		       (if (equal result "")
			   function
			   (lisp-string-to-symbol result))))
		   (name (lisp-symbol-name defn-spec))
		   (possible-types
		     (cond ((string-match "[()]" name)
			     ;; compound definition names happen only for
			     ;; functions, so there is no point in asking.
			     ;; [***bug***: this is not true in general for
			     ;; user-defined types.  -- rgr, 15-Aug-02.]
			     ;; besides, it is not helpful to offer a choice
			     ;; between "generic-function" and "function" when
			     ;; the user gives a GF name.
			     '("function"))
			   ((not (comint-check-proc ilisp-buffer))
			     ;; oops; this probably won't do us much good . . .
			     (mapcar (function car)
				     (ilisp-value 'ilisp-source-types t)))
			   (t
			     (lisp-find-inferior-definition-types defn-spec)))))
	      (list defn-spec
		    (if (cdr possible-types)
			(ilisp-completing-read
			  "Type [any]: "
			  (cons '("any")
				(mapcar (function list) possible-types))
			  "any")
			;; [specifying "any" uses the cache.  -- rgr, 4-Sep-02.]
			;; (car possible-types)
                      (if (ilisp-value 'ilisp-find-source-command t)
                          ;; inferior lisp will tell us
                          "any"
                        (let ((types (ilisp-value 'ilisp-source-types t)))
                          (ilisp-completing-read
                           (format "Type [%s]: " (caar types))
                           types (caar types))))))))
	  ;; Non-interactive cases (reusing the last definition name).
	  ((zerop numeric-arg)
	    (list nil 'visit))
	  ((minusp numeric-arg)
	    ;; continue looking backward
	    (list nil 'back)) 
	  (t
	    ;; continue looking forward
	    (list nil nil)))))

(defun edit-definitions-lisp (definition-name
			      &optional type stay search locator)
  "Visit all definitions of DEFINITION-NAME of type TYPE.  The
DEFINITION-NAME is normally a symbol, but in Common Lisp can also be a
list like \"(SETF FOO)\" or \"(method frob (random-thing))\".  The type
defaults to \"any\", which really means to look for *all* definitions of
DEFINITION-NAME.  If specified interactively, emacs prompts for a type
only if the Lisp knows about more than one definition type \(but the
default is still \"any\"\).

   When invoked interactively with a numeric arg, \\[edit-definitions-lisp]
continues looking for the last definition specified: a positive arg
means forward, while a negative arg means backward.  Possible locations
for the last requested definition are kept in the *Edit-Definitions*
buffer; forward and backward means relative to point in this buffer, and
a zero arg means to switch to this buffer.  If you want to jump to an
arbitrary possibility, move the point just before it and do
\"\\[universal-argument] \\[edit-definitions-lisp]\".  [bug: the
keybinding is not available unless you switch back to an ilisp buffer
first.  -- rgr, 4-Sep-02.]

   If the optional STAY argument is true, put the source file in the
same window.  If SEARCH, do not look for DEFINITION-NAME in the inferior
Lisp.  The optional LOCATOR can be a function of args \(definition-name
type first-p back-p\); normally this is specified by the inferior Lisp,
which knows the syntax of its source files.

   In order to find the definition, we look for it in the following way:

   0.  If SEARCH is true, we do the equivalent of \\[tags-search] and
skip the other steps.  [This is really a distinct protocol, and perhaps
ought to be made separate.  -- rgr, 31-Jul-02.]

   1.  First we ask the inferior Lisp if it knows about the definition's
source file.  If it does, then we look for the definition in that
file(s), using ilisp's built-in definition-finding heuristics.

   2.  If the Lisp doesn't know (or we were told not to ask), and
lisp-edit-files is not nil, we search the files in lisp-edit-files,
possibly set up by the \\[lisp-directory] command, or the buffers in one
of lisp-source-modes if lisp-edit-files is T.
[Bug:  This doesn't work.  -- rgr, 17-Dec-03.]

   3.  If still not found, and at least one tag table is defined, we may
use \\[find-tag] to keep searching.  This is controlled by the
lisp-fall-back-on-find-tags variable \(q.v.), which defaults to :ask."
  (interactive (lisp-ask-for-definition-to-edit current-prefix-arg))
  (or type
      (setq type "any"))
  (and (stringp definition-name)
       (setq definition-name (lisp-string-to-symbol definition-name)))
  (cond (search 
	  ;; Search through all files listed in tags table.  [really, this is a
	  ;; different protocol; should be handled by another command.  -- rgr,
	  ;; 31-Jul-02.]
	  (let ((case-fold-search t))
	    (setq tags-loop-scan (list locator
				       (list 'quote definition-name) 
				       type t nil))
	    (setq tags-loop-operate nil)
	    (setq lisp-using-tags t)
	    (tags-loop-continue t)))
	(definition-name
	  (lisp-edit-definitions-normal definition-name type stay locator))
	((eq type 'visit)
	  (switch-to-buffer "*Edit-Definitions*"))
	(t
	  ;; continuing to edit the same thing.
	  (next-definition-lisp (eq type 'back)))))

;;;; End of new M-. code.

;; [some of the rest of this could profit from better integration with the new
;; M-. infrastructure.  in particular, search-lisp and replace-lisp use the
;; *Edit-Definitions* buffer incompatibly.  -- rgr, 25-Mar-03.]

;;;%%Utilities

(defun lisp-setup-edit-definitions (message edit-files)
  "Set up *Edit-Definitions* with MESSAGE.
If EDIT-FILES is T, insert all buffer filenames that are in one of
lisp-source-modes into the current buffer.  If it is a list of files
set up by lisp-directory, insert those in the buffer.  If it is a
string put that in the buffer."

  ;; Note
  ;; 19990804 Marco Antoniotti
  ;; Are we sure we want to set 'lisp-using-tags' to nil?
  (setq lisp-using-tags nil
	lisp-search (not (stringp edit-files)))
  (set-buffer (get-buffer-create "*Edit-Definitions*"))
  (erase-buffer)
  (insert message)
  (insert "\n\n")
  (if edit-files
      (progn
	(if (eq edit-files t)
	    (let ((buffers (buffer-list)))
	      (while buffers
		(let ((buffer (car buffers)))
		  (if (save-excursion 
			(set-buffer buffer) 
			(and (memq major-mode lisp-source-modes)
			     (buffer-file-name buffer)))
		      (progn (insert ?\") (insert (buffer-file-name buffer))
			     (insert "\"\n"))))
		(setq buffers (cdr buffers))))
	    (if (stringp edit-files)
		(progn (insert edit-files)
		       	;; Remove garbage collection messages
		       (replace-regexp "^;[^\n]*\n" "")
		       (fix-source-filenames))
		(let ((files edit-files))
		  (while files
		    (insert ?\")
		    (insert (car files))
		    (insert "\"\n")
		    (setq files (cdr files))))))
	(goto-char (point-min))
	(forward-line 2)
	(set-buffer-modified-p nil))
      (error 
       (substitute-command-keys
	"Use \\[lisp-directory] to define source files."))))
	  
;;;
(defun lisp-next-file (back)
  "Return the next filename in *Edit-Definitions*, or nil if none."
  (let ((file t) 
	result)
    (set-buffer (get-buffer-create "*Edit-Definitions*"))
    (if back 
	(progn (forward-line -1)
	       (if (looking-at "\n")
		   (progn 
		     (forward-line 1)
		     (end-of-line)
		     (setq file nil)))))
  (if file
      (progn
	(skip-chars-forward "^\"")
	(if (eobp)
	    (progn (bury-buffer (current-buffer))
		   (setq result nil))
	    (let* ((start (progn (forward-char 1) (point))))
	      (skip-chars-forward "^\"") 
	      (setq file
		    (prog1 (buffer-substring-no-properties start (point))
		      (end-of-line)))
	      (bury-buffer (current-buffer))))))
  (if (not (eq file 't)) file)))

;;;%%Searching
(defun lisp-locate-search (pattern type first back)
  "Find PATTERN in the current buffer."
  (if back
      (search-backward pattern nil t)
      (search-forward pattern nil t)))

;;;
(defun lisp-locate-regexp (regexp type first back)
  "Find REGEXP in the current buffer."
  (if back
      (re-search-backward regexp nil t)
      (re-search-forward regexp nil t)))

;;;

(defvar lisp-last-pattern nil "Last search regexp.")

(defun search-lisp (pattern regexp)
  "Search for PATTERN through the files or buffers.
Search for file in 'lisp-edit-files' if it is a list or the
current buffers in one of 'lisp-source-modes' otherwise.  If
lisp-edit-files is nil, no search will be done.  If called with a
prefix, search for regexp.  If there is a tags file, call 'tags-search'
instead."
  (interactive
   (list (read-string (if current-prefix-arg 
			  "Search for regexp: "
			  "Search for: ") lisp-last-pattern)
	 current-prefix-arg))
  (if tags-file-name
      (progn (setq lisp-using-tags t)
	     (tags-search (if regexp pattern (regexp-quote pattern))))
      (setq lisp-last-pattern pattern
	    lisp-last-definition (cons pattern t)
	    lisp-last-file nil
	    lisp-last-locator (if regexp
				  'lisp-locate-regexp
				  'lisp-locate-search))
      (lisp-setup-edit-definitions (format "Searching for %s:" pattern) 
				   lisp-edit-files)
      (next-definition-lisp nil nil)))

;;;%%Replacing
(defvar lisp-last-replace nil "Last replace regexp.")

(defun replace-lisp (old new regexp)
  "Query replace OLD by NEW through the files or the current buffers.
The query is done in 'lisp-edit-files' if it is a list and the current
buffers in one of 'lisp-source-modes' otherwise.  If 'lisp-edit-files'
is NIL, no search will be done.  If called with a prefix, replace
regexps.  If there is a tags file, then call tags-query-replace
instead."
  (interactive
   (let ((old (read-string (if current-prefix-arg
			       "Replace regexp: "
			       "Replace: ") lisp-last-pattern)))
     (list old
	   (read-string (if current-prefix-arg
			    (format "Replace regexp %s by: " old)
			    (format "Replace %s by: " old))
			lisp-last-replace)
	   current-prefix-arg)))
  (cond (tags-file-name
	 (setq lisp-using-tags t)
	 (tags-query-replace (if regexp old (regexp-quote old))
			     new))
	(t
	 (setq lisp-last-pattern old
	       lisp-last-replace new)
	 (lisp-setup-edit-definitions 
	  (format "Replacing %s by %s:\n\n" old new)
	  lisp-edit-files)
	 (let ((file nil))
	   (while (setq file (lisp-next-file nil))
	     (lisp-find-file file)
	     (let ((point (point)))
	       (goto-char (point-min))
	       (if (if regexp 
		       (re-search-forward old nil t)
		       (search-forward old nil t))
		   (progn (beginning-of-line)
			  (if regexp
			      (query-replace-regexp old new)
			      (query-replace old new)))
		   (goto-char point))))))))

;;;%%Edit-callers
(defvar lisp-callers nil 
  "T if we found callers through inferior LISP.")

;;;
(defun who-calls-lisp (function &optional no-show)
  "Put the functions that call FUNCTION into the buffer *All-Callers*.
Show the buffer *All-Callers* unless NO-SHOW is T.  Return T if successful."
  (interactive 
   (let* ((function (lisp-defun-name))
	  (symbol (lisp-buffer-symbol function)))
     (if (lisp-minus-prefix)
	 (list function)
	 (list (ilisp-read-symbol 
		(format "Who Calls [%s]: " symbol)
		function
		t t)))))
  (let* ((name (lisp-buffer-symbol function))
	 (command (ilisp-value 'ilisp-callers-command t))
	 (callers
	  (if command
	      (ilisp-send
	       (format command
		       (lisp-symbol-name function)
		       (lisp-symbol-package function))
	       (concat "Finding callers of " name)
	       'callers)))
	 (last-line (if callers (lisp-last-line callers)))
	 (case-fold-search t))
    (set-buffer (get-buffer-create "*All-Callers*"))
    (erase-buffer)
    (insert (format "All callers of function %s:\n\n" name))
    (if (and command (not (ilisp-value 'comint-errorp t)))
	(if (string-match "nil" (car last-line))
	    (error "%s has no callers" name)
	    (message "")
	    (insert (cdr last-line))
	    (goto-char (point-min))
	    ;; Remove garbage collection messages
	    (replace-regexp "^;[^\n]*\n" "")
	    (goto-char (point-min))
	    (forward-line 2)
	    (if (not no-show) 
		(if (ilisp-temp-buffer-show-function)
		    (funcall (ilisp-temp-buffer-show-function)
			     (get-buffer "*All-Callers*"))
		    (view-buffer "*All-Callers*")))
	    t)
	(insert "Using the current source files to find callers.")
	nil)))

;;;
(defun next-caller-lisp (back &optional pop)
  "Edit the next caller from *All-Callers*.
With prefix, edit the previous caller.  If it can't get caller
information from the inferior LISP, this will search using the current
source files.  See lisp-directory."

  (interactive "P")
  (if (not lisp-callers)
      (next-definition-lisp back pop)
      (set-buffer (get-buffer-create "*All-Callers*"))
      (if back (forward-line -1))
      (skip-chars-forward " \t\n")
      (if (eobp)
	  (progn
	    (bury-buffer (current-buffer))
	    (error "No more callers"))
	  (let* ((start (point))
		 (caller-function
		  (progn
		    (skip-chars-forward "^ \t\n")
		    (buffer-substring-no-properties start (point)))))
	    (bury-buffer (current-buffer))
	    (edit-definitions-lisp (lisp-string-to-symbol caller-function) 
				  (car (car (ilisp-value 'ilisp-source-types)))
				  (not pop))))))

;;;
(defun edit-callers-lisp (function)
  "Edit the callers of FUNCTION.
With a minus prefix use the symbol at the start of the current defun."
  (interactive
   (let* ((function (lisp-defun-name)))
     (if (lisp-minus-prefix)
	 (list function)
	 (list (ilisp-read-symbol 
		(format "Edit callers of [%s]: "
			(lisp-buffer-symbol function))
		function
		t)))))
  (if (save-excursion (setq lisp-callers (who-calls-lisp function t)))
      (progn 
	(setq lisp-last-locator (ilisp-value 'ilisp-calls-locator))
	(next-caller-lisp nil t))
      (edit-definitions-lisp function "calls" nil t 
			    (ilisp-value 'ilisp-calls-locator))))

;;;%Locators
(defun lisp-re (back format &rest args)
  "Search BACK if T using FORMAT applied to ARGS."
  (let ((regexp (apply 'format format args)))
    (if back
	(re-search-backward regexp nil t)
	(re-search-forward regexp nil t))))

;;;
(defun lisp-locate-ilisp (symbol type first back)
  "Find SYMBOL's TYPE definition in the current file.  Return T if successful.
A definition is of the form

    (def<something><whitespace>(?name<whitespace>

This is used as the default value of ilisp-locator; the specific dialect
will usually override this with something more clever."
  (lisp-re back
	   "^[ \t\n]*(def[^ \t\n]*[ \t\n]+(?%s[ \t\n(]+" 
	   (regexp-quote (lisp-symbol-name symbol))))

;;;
(defun lisp-locate-calls (symbol type first back)
  "Locate calls to SYMBOL."
  (lisp-re back "\\(#'\\|(\\|'\\)%s\\([ \t\n]+\\|)\\)"
	   (regexp-quote (lisp-buffer-symbol symbol))))


;;;%% Locators for Scheme

;;; Matthias Koeppe <mail.math.uni-magdeburg.de>
;;;
;;; The standard locators would fail on "(define (thunk) ....)"  and
;;; report "(define (procedure ...) ....)" as a call to procedure.

(defun ilisp-locate-scheme-definition (symbol type first back)
  "Find SYMBOL's TYPE definition in the current file. Return T if successful.
This is the Scheme counterpart of `lisp-locate-clisp'."
  (lisp-re back
	   "[ \t\n]*(def[^ \t\n]*[ \t\n]+(*%s\[ \t\n()]"
	   (regexp-quote (lisp-symbol-name symbol))))

(defun ilisp-locate-scheme-calls (symbol type first back)
  "Locate calls to SYMBOL.
This is the Scheme counterpart of `lisp-locate-calls'."
  (let ((call-regexp 
	 (format "[( \t\n]+%s[ \t\n()]+"
		 (regexp-quote 
		  ;; Scheme has no package prefixes, so we use
		  ;; lisp-symbol-name instead of lisp-buffer-symbol.
		  (lisp-symbol-name symbol))))
	(def-regexp "[ \t\n]*(def[^ \t\n]*[ \t\n]+(*")
	(result 'unknown))
    (while (eq result 'unknown)
      (cond 
       ((if back
	    (re-search-backward call-regexp nil t)
	  (re-search-forward call-regexp nil t))
	(if (not (save-excursion	; check whether definition
		   (goto-char (match-beginning 0))
		   (backward-sexp) (backward-char)
		   (looking-at def-regexp)))
	    (setq result t)))
       (t (setq result nil))))
    result))	    


;;; end of file -- ilisp-src.el --
