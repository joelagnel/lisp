;;; Debugging info for self: Saved through ges-version 1.5dev
;;; ;;; From: Thomas Link <t.link02a.ZERO.SPAM@gmx.net>
;;; ;;; Subject: excerpt.el 0.1.3
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Tue, 23 Jul 2002 09:42:46 +0200
;;; ;;; Organization: Vienna University, Austria

;;; Hi,

;;; This is a package for extracting portions of text from a source file
;;; and to insert them into a target file. Apart from its main goal,
;;; writing commentaries or interpretations, one could use it for
;;; maintaining snippet libraries or, maybe, for implementing something
;;; known as dynamic fields in some word processors.

;;; This is the first public release and I don't claim that it works reliable.

;;; Requires tellib.el -- which I posted to this newsgroup too.

;;; Cheers,
;;; Thomas.
;; excerpt.el --- extraxt text & report to main file

;; Copyright (C) 2002 Thomas Link

;; Author: Thomas Link AKA samul AT web DOT de
;; Time-stamp: <2002-07-22>
;; Keywords: editing, summaries, writing reports, text snippets

(defvar excerpt-version "0.1.3")
(defvar excerpt-homepage "http://members.a1.net/t.link/CompEmacsExcerpt.html")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; ;---(:excerpt-beg excerpt :name doc)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsExcerpt")---
;; ** Description
;; 
;; Extraxt text & report to main file. This is done using the following
;; commands:
;; 
;; =excerpt-insert= :: Insert an excerpt from a source file. Use
;; TAB-completion to see which excerpts are available in a certain file. 
;; (See also the variable =excerpt-sources=.)
;; 
;; =excerpt-insert-with-prefix= :: Insert a prefixed excerpt.
;; 
;; =excerpt-source= :: Insert a source mark in the source file. The user
;; will be asked to enter a category name.
;; 
;; =excerpt-named-source= :: Insert a source mark in the source file. The
;; user will be asked to enter a category name and an ID.
;; 
;; 
;; ** In the Main File
;; 
;; In the main file text to be reported is marked as 
;; 
;; <example>
;; 	\<prefix\>\<comment-string\>(:excerpt NAME [:id ID])
;; 	\<prefix\><comment-string>(:excerpt-source FILE-NAME)
;; 	\<prefix\>[previously reported text]
;; 	\<prefix\>\<comment-string\>(:excerpt-end [ID])
;; </example>
;; 
;; A local keymap covers the excerpt. Pressing Enter jumps to the source
;; file, pressing Mouse-2 opens a menu.
;; 
;; If no ID is given, all occurances of category NAME are being excerpted.
;; (Hm. To be honest, I'm not sure if this is true, if I planned to
;; implement such a feature, and if I've already done so.)
;; 
;; The whole excerpt will be prefixed with the first line's PREFIX. This
;; is a convenient way for inserting excerpts as comments.
;; 
;; The file/buffer local variable =excerpt-sources= defines where sources
;; are normally located. The default file for =excerpt-insert= is either
;; built on the value of this variable or the current buffer's file name.
;; 
;; 
;; ** In the Source File
;; 
;; In the source file these text portions are marked as
;; 
;; <example>
;; 	\<format-string NAME>\<comment-string\>(\<start-string\> ID)
;; 	text to be reported
;; 	\<comment-string\>(\<end-string\> ID)
;; </example>
;; 
;; NAME ... a string -- may not contain ")"
;; 
;; FORMAT-STRING is mode sensitive but can be changed via buffer local
;; variables. It is possible to use tags like =LaTeX=' =\\index=.
;; 
;; START-STRING and END-STRING are global but can be changed via buffer
;; local variables.
;; 
;; 
;; ** Caveats
;; 
;; Using regular expressions for finding excerpt sources and excerpt
;; targets is of course a fragile solution. There are many ways to confuse
;; =excerpt.el=. Here are some tips.
;; 
;; 
;; *** Excerpt sources have to end with a newline
;; 
;; Excerpt sources have to end with a newline. This means something like
;; the following
;; 
;; <example>
;; \t	%% ---(:excerpt test)@(:begin wrong)---
;; \t	source text%% ---(:end test)---
;; </example>
;; 
;; will not work. One possible solution would be:
;; 
;; <example>
;; \t	%% ---(:excerpt test)@(:begin right)---
;; \t	source text%
;; \t	%% ---(:end test)---
;; </example>
;; 
;; 
;; *** COMMENT-START, COMMENT-END
;; 
;; For hiding excerpt marks to the compiler, text formatter, preprocessor,
;; or whatsoever, these marks are enclosed with =COMMENT-START= and
;; =COMMENT-END=. This means of course that these variables have to be
;; defined somewhere. Normally this is done by the major mode. If this is
;; not the case, you could use file local variables or you could use some
;; project management package that sets these variables for files in a
;; certain directory.
;; 
;; 
;; ** Customization Via Buffer Local Variables
;; 
;; The following variables overwrite global settings if defined as local
;; variables:
;; 
;; =excerpt-start-format-string=, =excerpt-end-format-string= :: Define
;; source start and end marks.
;; 
;; =excerpt-start-string=, =excerpt-end-string= :: These variables are
;; created by excerpt.el and define the excerpt start and end marks.
;; 
;; =excerpt-sources= :: Either a file name or a directory name where
;; excerpt sources are located.
;; 
;; =excerpt-update-flag= :: If non-nil, update all excerpts after opening.
;; 
;; For setting these variables for all files in a specific directory, use
;; some project management package. (You could also use CompEmacsFilesets,
;; defining some custom :open function.)
;; 
;; 
;; ** Types of excerpts
;; 
;; At the moment there are two "types" of excerpts:
;; 
;; excerpt :: Insert marked excerpts.
;; 
;; file :: Insert the whole file. When running the command
;; =excerpt-insert=, a pseudo excerpt with the name =*FILE*= is generated.
;; 
;; ;---(:excerpt-end excerpt :name doc)---


;;; Known problems:

;; ;---(:excerpt-beg excerpt :name problems)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsExcerpt")---
;; 
;; ;---(:excerpt-end excerpt :name problems)---


;;; Change log:

;; ;---(:excerpt-beg excerpt :name log)---
;; ;---(:excerpt-source "~/Etc/TL-Wiki/CompEmacsExcerpt")---
;; 
;; v0.1 :: initial release (experimental)
;; 
;; ;---(:excerpt-end excerpt :name log)---


;;; To do:

;- mode for browsing excerpts
;- automatically convert excerpts between file formats
;- excerpt listings
;- non-intrusive excerpt source definition
;- check names (uniqueness -> numbered names)???
;- excerpt-format-strings by major-mode, minor-mode, file name
;- more than one possible format for marking excerpts in the target file
;- short forms for excerpts ~ dynamic fields. E.g. in LaTeX this could
;be \excerpt{CAT@NAME}{EXCERPT} The source file would be defined via a
;file local variable. The command \excerpt has to defined, of course:
;\newcommand{\excerpt}[2]{#2}. But what if the excerpt contains "}"?
;- virtual excerpts??? (not marked & automatically extracted; but don't
;we already have this feature using file-local search string?)


;;; Code:

(require 'tellib)
(tellib-version-check "tellib.el" tellib-version "0.1.3")

(when tellib-running-xemacs
  (require 'overlay))


;; Customizations

(defgroup excerpt nil
  "Extraxt text & report to main file."
  :prefix "excerpt-"
  :group 'data)

(defcustom excerpt-global-categories-file
  (if tellib-running-xemacs
      "~/.xemacs/excerpt-categories.txt"
    "~/.excerpt-categories.txt")
  "*Global categories file."
  :type 'file
  :group 'excerpt)

(defcustom excerpt-global-start-format-string
  "(:begin #{NAME})"
  "*Excerpt source start string."
  :type 'string
  :group 'excerpt)

(defcustom excerpt-global-end-format-string
  "(:end #{NAME})"
  "*Excerpt source end string."
  :type 'string
  :group 'excerpt)

(defcustom excerpt-set-read-only-flag
  nil
  "*Set excerpts read-only."
  :type 'boolean
  :group 'excerpt)

(defcustom excerpt-invisible-header-flag
  nil
  "*Make excerpt definition headers invisible.
Invisible header lines are also marked read-only."
  :type 'boolean
  :group 'excerpt)

(defcustom excerpt-bind-space-to-update-flag nil
  "*Bind space-key to `excerpt-update-current'
in the excerpt's local keypam."
  :type 'boolean
  :group 'excerpt)

(defcustom excerpt-use-relative-or-absolute-file-name
  '((same-dir t relative)
    (t t relative))
  "*Use relative file names when inserting excerpts.
Abbreviated? If non-nil, the abbreviated file name (see
`abbreviate-file-name') is matched against regexp."
  :type '(repeat :tag "List"
		 (choice :tag "Select"
			 (list :tag "Pattern"
			       :value ("^~/.*$" t relative)
			       (regexp :tag "Regexp" :value "^~/.*$")
			       (boolean :tag "Abbreviated?" :value t)
			       (choice :tag "Mode"
				       (const :tag "absolute"
					      :value absolute)
				       (const :tag "relative"
					      :value relative)))
			 (list :tag "Pair"
			       :value (("^~/.*$" "^~/.*$") t relative)
			       (list :tag "Regexps"
				     (regexp :tag "Source" :value "^~/.*$")
				     (regexp :tag "Main File" :value "^~/.*$"))
			       (boolean :tag "Abbreviated?" :value t)
			       (choice :tag "Mode"
				       (const :tag "absolute"
					      :value absolute)
				       (const :tag "relative"
					      :value relative)))
			 (list  :tag "Same directory"
				:value (same-dir t relative)
				(const :format "" :value same-dir)
				(const :format "" :value t)
				(choice :tag "Mode"
					(const :tag "absolute"
					       :value absolute)
					(const :tag "relative"
					       :value relative)))
			 (list  :tag "Else"
				:value (t t relative)
				(const :format "" :value t)
				(const :format "" :value t)
				(choice :tag "Mode"
					(const :tag "absolute"
					       :value absolute)
					(const :tag "relative"
					       :value relative)))))
  :group 'excerpt)

(defcustom excerpt-mouse-button
  (if tellib-running-xemacs [(button2)] [mouse-2])
  "*Mouse button to press for popping up the main file's excerpt menu."
  :type 'sexp
  :group 'excerpt)

(defcustom excerpt-format-strings
  '(("\\.tex$"
     "\\index{#{CATEGORY}}%%%#{START}"
     "%%% #{END}")
    (emacs-wiki-mode
     "<!-- ---(:excerpt #{CATEGORY})@#{START}--- -->"
     "<!-- ---#{END}--- -->"
     ((:no-buffer-local-vars t)))
    (""
     "#{COMMENT-START}---(:excerpt #{CATEGORY})@#{START}---#{COMMENT-END}"
     "#{COMMENT-START}---#{END}---#{COMMENT-END}"))
  "*Format strings for excerpt source marks.

An alist of the form
 '((FILE-PATTERN START-FORMAT-STRING END-FORMAT-STRING)
   ...)

FILE-PATTERN ... a regexp matching the source file name

START-FORMAT-STRING ... #{START} will be replaced by
`excerpt-global-start-format-string'

END-FORMAT-STRING ... #{END} will be replaced by
`excerpt-global-end-format-string'"
  :type '(repeat
	  (list :tag "Definiton"
		(choice :tag "Mode"
			(regexp :tag "Source file pattern")
			(symbol :tag "Symbol"))
		(string :tag "Source start string")
		(string :tag "Source end string")
		(repeat
		 (choice :tag "Props"
			 (list :tag "Don't add buffer local variables"
			       :value (:no-buffer-local-vars t)
			       (const :format "" :value :no-buffer-local-vars)
			       (boolean :tag "Boolean" :value t))))))
  :group 'excerpt)


;; Vars etc.

(defvar excerpt-start-format-string nil)
(defvar excerpt-end-format-string nil)
(defvar excerpt-start-string nil)
(defvar excerpt-end-string nil)
(defvar excerpt-last-id "")
(defvar excerpt-last-id-3 nil)

(defvar excerpt-names nil)
(defvar excerpt-categories nil)
(defvar excerpt-categories-file nil)

(defvar excerpt-buffer-categories-file nil)
(make-variable-buffer-local 'excerpt-buffer-categories-file)

(defvar excerpt-sources nil
  "Either a file name or a directory name where excerpt sources are located.")
(make-variable-buffer-local 'excerpt-sources)

(defvar excerpt-update-flag nil
  "If non-nil, update all excerpt after opening.
Of course, this means that the buffer will always modified.")
(make-variable-buffer-local 'excerpt-update-flag)

(defface excerpt-hyper-face
  '((t
     ;;(:foreground "blue" :background "yellow")
     (:background "grey85")
     ))
  "Face for marking excerpts."
  :group 'excerpt)

(defface excerpt-hyper-header-face
  '((t
     ;;(:foreground "darkblue" :background "orange")
     (:background "grey70")
     ))
  "Face for marking excerpt headers."
  :group 'excerpt)

(defface excerpt-source-header-face
  '((t
     ;;(:background "yellow")
     ;;(:background "grey85")
     (:background "AntiqueWhite3")
     ))
  "Face for marking source headers."
  :group 'excerpt)

(defvar excerpt-get-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'excerpt-find-source-at-point)
    (when excerpt-bind-space-to-update-flag
      (define-key map [space] 'excerpt-update-excerpt-at-point))
    (define-key map excerpt-mouse-button 'excerpt-find-source-at-mouse)
    map)
  "Local map referring to SOURCE-FILE.")


;; Utils

(defun excerpt-get-id-at-cat (cat id)
  "Return the compound name for ID and CAT."
  (concat cat "@" id))

(defun excerpt-find-file (filename &optional no-select-flag cat name)
  "Open an excerpt's source file."
  (when (file-readable-p filename)
    (let ((rv (if no-select-flag
		  (find-file-noselect filename t)
		(find-file filename))))
      (when (and cat name)
	(beginning-of-buffer)
	(let* ((fname buffer-file-name)
	       (def (excerpt-get-fsdef-entry fname)))
	  (if def
	      (let ((sfs (excerpt-format-format-string
			  (excerpt-get-start-format-string fname def)
			  "#{START}"
			  (excerpt-use-this-start-string)
			  cat
			  name)))
		(tellib-re-search sfs t))
	    (message "excerpt: Can't goto to %s"
		     (excerpt-get-id-at-cat cat name)))))
      rv)))


;; Access to `excerpt-format-strings'

(defun excerpt-get-fsdef-entry (filename &optional def)
  "Get the `excerpt-format-strings' DEF for FILENAME."
  (or def
      (some (lambda (this)
	      (let ((mode-def (nth 0 this)))
		(when (or (and (stringp mode-def)
			       (string-match mode-def filename))
			  (and (symbolp mode-def)
			       (equal major-mode mode-def)))
		  this)))
	    excerpt-format-strings)))

(defun excerpt-get-fsdef (filename &optional def position default)
  "Get the POSITION element of `excerpt-format-strings' DEF for FILENAME."
  (let ((this (excerpt-get-fsdef-entry filename def)))
    (if this
	(or (nth position this)
	    default)
      (tellib-error 'error
		    "excerpt: " 
		    filename
		    " doesn't match any entry in `excerpt-format-strings'"))))

(defun excerpt-get-start-format-string (filename &optional def)
  "Get the start-format-string (see `excerpt-format-strings') for FILENAME."
  (excerpt-get-fsdef filename def 1))
;;test: (excerpt-get-start-format-string buffer-file-name)

(defun excerpt-get-end-format-string (filename &optional def)
  "Get the end-format-string (see `excerpt-format-strings') for FILENAME."
  (excerpt-get-fsdef filename def 2))
;;test: (excerpt-get-end-format-string buffer-file-name)

(defun excerpt-get-fsdef-props (prop filename &optional def default carp)
  "Get properties for FILENAME -- use DEF if provided."
  (tellib-alist-get (excerpt-get-fsdef filename def 3)
		    prop
		    default
		    carp))

(defun excerpt-use-this-start-string ()
  "Select global or local start-format-string."
  (or excerpt-start-format-string
      excerpt-global-start-format-string))

(defun excerpt-use-this-end-string ()
  "Select global or local start-format-string."
  (or excerpt-end-format-string
      excerpt-global-end-format-string))

(defun excerpt-use-this-format-start-string ()
  "Select global or local start-format-string."
  (or excerpt-start-string
      excerpt-global-start-format-string))

(defun excerpt-use-this-format-end-string ()
  "Select global or local start-format-string."
  (or excerpt-end-string
      excerpt-global-end-format-string))


;; Get marks
(defun excerpt-format-format-string (format what string
					    &optional name id non-literal-flag)
  "Helper function: format marks"
  (let ((txt format))
    (dolist (this `(("#{COMMENT-START}" ,(or comment-start ""))
		    ("#{COMMENT-END}"   ,(or comment-end ""))
		    (,what              ,string)
		    ("#{NAME}"          ,id)
		    ("#{CATEGORY}"      ,name))
		  txt)
      (let ((fs (car this))
	    (rs (cadr this)))
	(when rs
	  (setq txt (replace-in-string txt (regexp-quote fs) rs t)))))))
;;test: (excerpt-format-format-string "#{END}" "#{END}" "\\(.+\\)")
;;test: (excerpt-format-format-string
;;       "#{COMMENT-START}---(:excerpt #{CATEGORY})@#{START}---#{COMMENT-END}"
;;       "#{START}" "\\(.+\\)" "cat" "id")
;;test: (excerpt-format-format-string
;;       "<!-- ---(:excerpt #{CATEGORY})@(:begin #{NAME})--- -->"
;;       nil nil "\\(.+\\)" "\\(.+\\)")
;;test: (excerpt-format-format-string
;;       "#{COMMENT-START}---(:excerpt #{CATEGORY})@#{START}---#{COMMENT-END}"
;;       nil nil "\\(.+\\)" "\\(.+\\)")

(defun excerpt-auto-id ()
  "Get an automatic id based on current time."
  (let* ((id (apply 'format (cons "%x%x" (current-time)))))
    (if (string-equal id excerpt-last-id)
	(progn
	  (setq excerpt-last-id-3 (+ excerpt-last-id-3 1))
	  (apply 'format (list "%s:%x" id excerpt-last-id-3)))
      (progn
	(setq excerpt-last-id id)
	(setq excerpt-last-id-3 0)
	id))))
;;test: (excerpt-auto-id)

(defun excerpt-source-start-string (format-start id name)
  "Format excerpt source start mark"
  (if excerpt-start-string
      (excerpt-format-format-string excerpt-start-string
				    nil
				    nil
				    name id)
    (excerpt-format-format-string format-start
				  "#{START}" 
				  (excerpt-use-this-start-string)
				  name id)))

(defun excerpt-source-end-string (format-end id name)
  "Format excerpt source end mark"
  (if excerpt-end-string
      (excerpt-format-format-string excerpt-end-string
				    nil
				    nil
				    name id)
    (excerpt-format-format-string format-end
				  "#{END}"
				  (excerpt-use-this-end-string)
				  name id)))


;; Categories

(defun excerpt-read-categories-file (filename)
  (unless (tellib-lax-plist-get excerpt-categories filename)
    (setq excerpt-categories
	  (tellib-lax-plist-put 
	   excerpt-categories
	   (expand-file-name filename)
	   (mapcar (lambda (x) (list x))
		   (with-temp-buffer
		     (insert-file-contents filename)
		     (tellib-filter-list (split-string (buffer-string) "\n")
					 (lambda (x)
					   (string-match "[^ \t]" x)))))))))

(defun excerpt-set-categories (&optional file-name)
  "Set categories for current buffer."
  (when (or file-name (not excerpt-buffer-categories-file ))
    (setq excerpt-buffer-categories-file
	  (expand-file-name (or file-name
				excerpt-categories-file
				excerpt-global-categories-file)))
    (excerpt-read-categories-file excerpt-buffer-categories-file)))

(defun excerpt-get-categories ()
  "Get current buffer's categories."
  (tellib-lax-plist-get excerpt-categories 
			excerpt-buffer-categories-file))

(defun excerpt-put-new-category (name)
  "Add a new category."
  (setq excerpt-categories
	(tellib-lax-plist-put 
	 excerpt-categories
	 excerpt-buffer-categories-file
	 (cons `(,name t) (excerpt-get-categories)))))
;;test: (excerpt-put-new-category "test")

(defun excerpt-change-categories-file ()
  "Change the buffer's categories file."
  (interactive)
  (let ((nf (read-file-name "Use categories from: ")))
    (when nf
      (tellib-update-local-variable-def "excerpt-categories-file" nf)
      (excerpt-set-categories nf))))


;; Names

(defun excerpt-get-names (category)
  "Get a category's names."
  (tellib-lax-plist-get excerpt-names category))
;;test: (excerpt-get-names "test")

(defun excerpt-name-is-defined-p (category name)
  "Check whether NAME is already defined for CATEGORY."
  (member name (excerpt-get-names category)))
;;test: (excerpt-name-is-defined-p "test" "a")
;;test: (excerpt-name-is-defined-p "test" "c")

(defun excerpt-add-name (category name)
  "Add a new NAME to CATEGORY."
  (let ((lst (excerpt-get-names category)))
    (unless (member name lst)
      (setq excerpt-names
	    (tellib-lax-plist-put 
	     excerpt-names
	     category
	     (cons name lst))))))
;;test: (excerpt-add-name "test" "a")
;;test: (excerpt-add-name "test" "b")


;; Source file

(defun excerpt-make-rx-from-format-string (string &optional name id)
  "Turn a format STRING into a valid regexp."
  (excerpt-format-format-string string nil nil
				(or name
				    "\\(.+\\)")
				(or id
				    "\\(.+\\)")))
;;test: (excerpt-make-rx-from-format-string
;;       "<!-- ---(:excerpt #{CATEGORY})@(:begin #{NAME})--- -->")
;;test: (excerpt-make-rx-from-format-string
;;       "#{COMMENT-START}---(:excerpt #{CATEGORY})@#{START}---#{COMMENT-END}")

(defun excerpt-filename->name (filename)
  "Turn FILENAME into a proper category name."
  (replace-in-string (file-name-sans-extension
		      (file-name-nondirectory filename))
		     "\\W"
		     "_"))

(defun excerpt-source-collect (&optional filename name0 id0)
  "Return a list of regions containing source marks.
If NAME0 and ID0 are provided, only this one excerpt is being collected."
  (save-excursion
    (save-restriction
      (let* ((fname (or filename buffer-file-name))
	     (def   (excerpt-get-fsdef-entry fname)))
	(if def
	    (let ((buff (excerpt-find-file fname t)))
	      (when buff
		(set-buffer buff)
		(let* ((sfs (excerpt-format-format-string
			     (excerpt-get-start-format-string fname def)
			     "#{START}"
			     (excerpt-use-this-start-string)))
		       (efs (excerpt-format-format-string
			     (excerpt-get-end-format-string fname def)
			     "#{END}"
			     (excerpt-use-this-end-string)))
		       (rsfs (excerpt-make-rx-from-format-string sfs))
		       ;; (refs (excerpt-make-rx-from-format-string efs))
		       (coll nil))
		  (widen)
		  (beginning-of-buffer)
		  ;;(message "DEBUG: %S %S" sfs rsfs)(sleep-for 3)
		  ;;(message "DEBUG: %S %S" (point) rsfs)(sleep-for 3)
		  (while (tellib-re-search rsfs t)
		    (let* ((beg  (match-beginning 0))
			   (begx (+ (match-end 0) 1))
			   (name (match-string 1))
			   (id   (match-string 2))
			   (refs (excerpt-make-rx-from-format-string
				  efs name id)))
		      (save-excursion
			(unless (string-equal id "#{NAME}")
			  (let* ((endx (progn
					 (tellib-re-search refs t)
					 (match-beginning 0)))
				 (end  (progn
					 (goto-char endx)
					 (point-at-bol 2))))
			    (when (or (not (and name0 id0))
				      (and (equal name0 name)
					   (equal id0 id)))
			      (setq coll
				    (append coll
					    `(,(list ':name name
						     ':id   id
						     ':beg  beg
						     ':end  end
						     ':begx begx
						     ':endx endx
						     ':text (buffer-substring 
							     begx endx)))
					    ))))))))
		  (append coll
			  (when (or (not (and name0 id0))
				    (equal name0 "*FILE*"))
			    `(,(list ':name "*FILE*"
				     ':id   (excerpt-filename->name fname)
				     ':beg  (point-min)
				     ':end  (point-max)
				     ':begx (point-min)
				     ':endx (point-max)
				     ':text (buffer-substring 
					     (point-min) (point-max)))))
			    )))))))))


;; Main file

(defun excerpt-get-beg-mark (mode category name file-name 
				  &optional prefix type process)
  "Return main file's mark header."
;	<comment-string>(:excerpt CATEGORY :name NAME \
;				[:type TYPE] [:process FN])
;	<comment-string>(:excerpt-source FILE-NAME)
  (let ((inserter #'(lambda (name val)
		      (cond
		       ((and val (equal mode 'insert))
			(format " %s %s" name val))
		       ((and val (equal mode 'match))
			(format " *?\\(%s %s\\)?" name val))
		       (t
			""))))
	(prefix (or prefix "")))
    (concat (if (equal mode 'insert) prefix "")
	    comment-start
	    "---("
	    (format ":excerpt-beg %s :name %s" category name)
	    (funcall inserter ":type" type)
	    (funcall inserter ":process" process)
	    ")---" comment-end "\n"
	    prefix
	    comment-start
	    (format "---(:excerpt-source %s)---" (tellib-quote file-name))
	    comment-end "\n")))

(defun excerpt-get-end-mark (category name &optional prefix)
  "Return main file's mark footer."
;	<comment-string>(:excerpt-end CATEGORY :name NAME)
  (let ((prefix (or prefix "")))
    (concat prefix
	    comment-start
	    (format "---(:excerpt-end %s :name %s)---" category name)
	    comment-end "\n")))

(defun excerpt-find-next-excerpt (&optional reverse-direction-flag cat name)
  "Find the *next* excerpt."
  (interactive "P")
  (let ((bs (excerpt-get-beg-mark 'match
				  (format "\\(%s\\)" (if cat
							 (regexp-quote cat)
						       ".+?"))
				  (format "\\(%s\\)" (if name
							 (regexp-quote name)
						       ".+?"))
				  "\\(.+\\)"  ;; file-name
				  ".*"        ;; prefix
				  ;; "\\(.+?\\)" ;; type
				  ;; "\\(.+?\\)" ;; process
				  )))
    (when (tellib-re-search bs t reverse-direction-flag)
      (let* ((bp    (match-beginning 0))
	     (be    (match-end 0))
	     (cat   (or cat (match-string 1)))
	     (name  (or name (match-string 2)))
	     ;; (type  (match-string 4))
	     ;; (proc  (match-string 6))
	     ;; (fname (match-string 7))
	     (type  nil)
	     (proc  nil)
	     (fname (match-string 3))
	     (prfx  (save-excursion
		      (goto-char bp)
		      (buffer-substring (point-at-bol) (point))))
	     (prfxl (length prfx))
	     (es    (excerpt-get-end-mark (regexp-quote cat)
					  (regexp-quote name)))
	     (ep    (save-excursion
		      (goto-char be)
		      (tellib-re-search es t)
		      (match-beginning 0)))
	     (ee    (match-end 0)))
	(when ee
	  (goto-char ee)
	  (list ':beg      (- bp prfxl)
		':end      ee
		':begtxt   be
		':endtxt   (- ep prfxl)
		':category cat
		':name     name
		':prefix   prfx
		':type     (or type 'excerpt)
		':process  (or proc 'identity)
		':file     fname))))))

(defun excerpt-remove-excerpt (ol beg end)
  "Remove excerpt covered by overlay OL."
  (overlay-put ol 'read-only nil)
  (delete-region beg end))

(defun excerpt-highlight-source-mark (beg end)
  "Highlight the region BEG to END as source mark."
  (let ((ol (make-overlay beg end)))
    (overlay-put ol 'face 'excerpt-source-header-face)))

(defun excerpt-make-hyper (source-file beg end begtxt endtxt cat name)
  "Mark buffer BEG to END being as a source mark referring to SOURCE-FILE."
  (let ((ol (make-overlay beg end)))
    (overlay-put ol 'mouse-face 'highlight)
    (overlay-put ol 'evaporate t)
    (when excerpt-set-read-only-flag
      (overlay-put ol 'read-only t))
    (overlay-put ol 'local-map excerpt-get-local-map)
    (overlay-put ol 'excerpt-ref-cat cat)
    (overlay-put ol 'excerpt-ref-name name)
    (overlay-put ol 'excerpt-reference source-file))
  (let ((ol (make-overlay begtxt endtxt)))
    (overlay-put ol 'face 'excerpt-hyper-face))
  (let ((marker (lambda (beg end invisible-flag read-only-flag)
		  (let ((ol (make-overlay beg end)))
		    (cond
		     (excerpt-invisible-header-flag
		      (when invisible-flag
			(overlay-put ol 'invisible t))
		      (when read-only-flag
			(overlay-put ol 'read-only t)))
		     (t
		      (overlay-put ol 'face 'excerpt-hyper-header-face)))))))
    (funcall marker beg begtxt t t)
    (funcall marker endtxt end t t)))

(defun excerpt-get-my-overlay-at-pos (&optional pos)
  "Get the right overlay at POS or (point) for further processing."
  (tellib-some (lambda (x)
		 (when (overlay-get x 'excerpt-ref-cat)
		   x))
	       (overlays-at (or pos (point)))))

(defun excerpt-find-source-at-point (&optional pos)
  "Find source at point POS."
  (interactive)
  (let* ((ol   (excerpt-get-my-overlay-at-pos pos))
	 (src  (overlay-get ol 'excerpt-reference))
	 (cat  (overlay-get ol 'excerpt-ref-cat))
	 (name (overlay-get ol 'excerpt-ref-name)))
    (if src
	(excerpt-find-file src nil cat name)
      (message "excerpt: Can't open file '%s'" src))))

(defun excerpt-update-excerpt-at-point (&optional pos)
  "Find source at point POS."
  (interactive)
  (let* ((ol   (excerpt-get-my-overlay-at-pos pos))
	 (cat  (overlay-get ol 'excerpt-ref-cat))
	 (name (overlay-get ol 'excerpt-ref-name))
	 (beg  (overlay-start ol))
	 (end  (overlay-end ol)))
    (excerpt-update-current beg end nil cat name)))

(defun excerpt-setup-source-marks ()
  "Highlight all source marks in the current buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((marks (excerpt-source-collect (buffer-file-name))))
      (dolist (this marks)
	(let ((name (plist-get this :name))
	      (beg  (plist-get this :beg))
	      (begx (plist-get this :begx))
	      (end  (plist-get this :end))
	      (endx (plist-get this :endx)))
	  (unless (equal name "*FILE*")
	    (excerpt-highlight-source-mark beg (- begx 1))
	    (excerpt-highlight-source-mark endx end)))))))

(defun excerpt-setup-all-excerpt ()
  "Set faces and text properties for all excerpts."
  (interactive)
  (save-excursion
    (excerpt-setup-source-marks)
    (if excerpt-update-flag
	(excerpt-update)
      (beginning-of-buffer)
      (catch 'end
	(while t
	  (let ((this (excerpt-find-next-excerpt)))
	    (if this
		(let ((beg    (plist-get this ':beg))
		      (end    (plist-get this ':end)))
		  (excerpt-make-hyper (plist-get this ':file)
				      beg
				      end
				      (plist-get this ':begtxt)
				      (plist-get this ':endtxt)
				      (plist-get this ':category)
				      (plist-get this ':name))
		  (goto-char (+ end 1)))
	      (throw 'end t))))))))


;; Core Commands

(defun excerpt-info ()
  (interactive)
  (tellib-info "excerpt"))

(defun excerpt-update-source-marks (&optional dont-replace-flag)
  "Update local variable definitions of excerpt start and end marks
If DONT-REPLACE-FLAG is non-nil, don't replace already existing definitions."
  (interactive "P")
  (tellib-update-local-variable-def 
   "excerpt-start-string"
   (excerpt-format-format-string
    (excerpt-get-start-format-string buffer-file-name)
    "#{START}"
    (excerpt-use-this-start-string))
   dont-replace-flag)
  (tellib-update-local-variable-def 
   "excerpt-end-string"
   (excerpt-format-format-string
    (excerpt-get-end-format-string buffer-file-name)
    "#{END}"
    (excerpt-use-this-end-string))
   dont-replace-flag))

(defun excerpt-source (beg end &optional id)
  "Mark current region as excerpt source"
  (interactive "r")
  (when (or buffer-file-name
	    (and (when (yes-or-no-p "File has not been saved. Save now? ")
		   (save-buffer))
		 buffer-file-name))
    (let* ((def (excerpt-get-fsdef-entry buffer-file-name))
	   (upd (not (excerpt-get-fsdef-props ':no-buffer-local-vars
					      buffer-file-name
					      def
					      nil
					      t)))
	   (sfs (when def
		     (excerpt-get-start-format-string buffer-file-name def)))
	   (efs (when def
		     (excerpt-get-end-format-string buffer-file-name def))))
      (excerpt-set-categories)
      (if (and sfs efs)
	  (let* ((name (let ((this (completing-read "excerpt: Category: "
						    (excerpt-get-categories))))
			 (if (string-equal this "")
			     "default"
			   this)))
		 (id   (or id
			   (excerpt-auto-id))))
	    (unless (some (lambda (a) (string-equal (car a) name))
			  (excerpt-get-categories))
	      (excerpt-put-new-category name))
	    (goto-char end)
	    (insert-face (excerpt-source-end-string efs id name)
			 'excerpt-source-header-face)
	    (newline)
	    (goto-char beg)
	    (insert-face (excerpt-source-start-string sfs id name)
			 'excerpt-source-header-face)
	    (newline)
	    (when upd
	      (excerpt-update-source-marks t)))
	(tellib-error 'error "excerpt-source: sfs=" sfs "efs=" efs)))))

(defun excerpt-named-source (beg end)
  "Mark current region as named excerpt source."
  (interactive "r")
  (let ((txt (read-from-minibuffer "Name: ")))
    (excerpt-source beg end
		    (if (string-equal txt "")
			nil
		      txt))))

(defun excerpt-get-source-filename (filename directory)
  "Return a filename based on `excerpt-use-relative-or-absolute-file-name'.
Any regexps will be matched against the expanded FILENAME."
  (if (and filename
	   directory)
      (catch 'exit
	(let ((lfname (expand-file-name filename))
	      (sfname (abbreviate-file-name filename t))
	      (exit (lambda (mode filename)
		      (if (equal mode 'relative)
			  (throw 'exit (file-relative-name filename directory))
			(throw 'exit filename)))))
	  (dolist (this excerpt-use-relative-or-absolute-file-name filename)
	    (let* ((rx   (nth 0 this))
		   (abbr (nth 1 this))		  
		   (mode (nth 2 this))
		   (fname (if abbr sfname lfname)))
	      (cond
	       ((stringp rx)
		(when (string-match rx fname)
		  (funcall exit mode filename)))
	       ((listp rx)
		(let ((rxa (nth 0 rx))
		      (rxb (nth 1 rx)))
		  (when (and (string-match rxa fname)
			     (string-match rxb directory))
		    (funcall exit mode filename))))
	       ((and (equal rx 'same-dir)
		     (string= (file-name-directory lfname)
			      (expand-file-name directory)))
		(throw 'exit (file-name-nondirectory filename)))
	       ((equal rx t)
		(funcall exit mode filename)))))))
    filename))

(defun excerpt-insert (&optional def beg-old end-old force-prefix)
  "Insert a excerpt mark into the main file.

Use DEF (a plist describing an excerpt) if provided. BEG-OLD and END-OLD
define the region covering the old excerpt, which should be deleted
before inserting the new one."
  (interactive)
  (save-excursion
    (let* ((source-file (plist-get def ':file))
	   (category    (plist-get def ':category))
	   (name        (plist-get def ':name))
	   (prefix      (or force-prefix
			    (plist-get def ':prefix)))
	   ;; (type        (plist-get def ':type))
	   ;; (process     (plist-get def ':process))
	   (bfn         buffer-file-name)
	   (currdir     (when bfn
			  (file-name-directory bfn)))
	   (dir         (cond
			 ((and excerpt-sources
			       (file-directory-p excerpt-sources))
			  (file-name-as-directory excerpt-sources))
			 (excerpt-sources
			  (file-name-directory excerpt-sources))
			 (t
			  currdir)))
	   (dftfl       (cond
			 ((and excerpt-sources
			       (file-directory-p excerpt-sources))
			  nil)
			 (excerpt-sources
			  (file-name-nondirectory excerpt-sources))
			 (bfn
			  (file-name-nondirectory bfn))))
	   (file        (excerpt-get-source-filename
			 (or source-file
			     (abbreviate-file-name
			      (expand-file-name
			       (read-file-name "Source: " dir nil t dftfl)
			       dir)
			      t))
			 currdir)))
      (when file
	(let* ((lst (mapcar (lambda (x)
			      (let ((id (plist-get x ':id))
				    (ct (plist-get x ':name)))
				(list (excerpt-get-id-at-cat ct id)
				      ct id
				      (plist-get x ':text))))
			    (excerpt-source-collect file category name)))
	       (this (if (and category name)
			 (excerpt-get-id-at-cat category name)
		       (completing-read "Select: " lst nil t))))
	  (when this
	    (when (and beg-old end-old)
	      (delete-region beg-old end-old))
	    (let* ((entry (assoc this lst))
		   (ct    (nth 1 entry))
		   (id    (nth 2 entry))
		   (tx    (nth 3 entry)))
	      ;;(message "DEBUG: %S %S %S" this (assoc this lst) lst)
	      ;;(message "DEBUG: %S %S %S %S %S %S" def this entry ct id tx)
	      ;;(sleep-for 3)
	      (when (and entry ct id tx)
		(let* ((prefix (or prefix ""))
		       (beg    (point))
		       (begtxt nil)
		       (endtxt nil)
		       (bmark  (excerpt-get-beg-mark 'insert ct id file prefix))
		       (emark  (excerpt-get-end-mark ct id prefix)))
		  (insert-face bmark 'excerpt-hyper-header-face)
		  (let ((rest (tellib-split-string-by-char tx ?\n)))
		    (setq begtxt (point))
		    (while (not (null rest))
		      (let ((this (car rest)))
			(setq rest (cdr rest))
			(if (null rest)
			    (unless (equal this "")
			      (insert-face prefix 'excerpt-hyper-face)
			      (insert-face this 'excerpt-hyper-face))
			  (insert-face prefix 'excerpt-hyper-face)
			  (insert-face this 'excerpt-hyper-face)
			  (newline)))))
		  (setq endtxt (point))
		  (insert-face emark 'excerpt-hyper-header-face)
		  (excerpt-make-hyper file beg (point) begtxt endtxt ct id))
		t))))))))

(defun excerpt-insert-with-prefix ()
  "Insert a prefixed excerpt."
  (interactive)
  (excerpt-insert nil nil nil (read-from-minibuffer "Prefix: ")))

(defun excerpt-update-current (beg end &optional find-def cat name)
  "Update currently marked excerpt (take care).
This command modifies the buffer contents. Take care."
  (interactive "r")
  (save-restriction
    (when (and beg end)
      (narrow-to-region beg end))
    (let ((def (or find-def
		   (progn
		     (goto-char (point-min))
		     (excerpt-find-next-excerpt nil cat name)))))
      (when def
	(let ((beg (plist-get def ':beg))
	      (end (plist-get def ':end)))
	  (narrow-to-region beg end)
	  (end-of-buffer)
	  (excerpt-insert def beg end)
	  (end-of-buffer)
	  t)))))

(defun excerpt-update-next (&optional reverse-flag)
  "Update the *next* excerpt.
With prefix argument, update the previous excerpt.
This command modifies the buffer contents. Take care."
  (interactive "P")
  (let ((def (excerpt-find-next-excerpt reverse-flag)))
    (when def
      (excerpt-update-current (plist-get def ':beg) (plist-get def ':end) def))))

(defun excerpt-find-source (beg end)
  "Goto currently selected excerpt's source file."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((def (excerpt-find-next-excerpt)))
      (when def
	(excerpt-find-file (plist-get def ':file)
			   nil
			   (plist-get def ':category)
			   (plist-get def ':name))))))

(defun excerpt-update ()
  "Update all excerpts.
This command modifies the buffer contents. Take care."
  (interactive)
  (beginning-of-buffer)
  (while (excerpt-update-next) nil))

(defun excerpt-update-region (beg end)
  "Update all excerpts in region.
This command modifies the buffer contents. Take care."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (excerpt-update)))


;; Setup

(defun excerpt-kill-emacs-hook ()
  (when excerpt-categories
    (dolist (this (plist-to-alist excerpt-categories))
      (let* ((file (car this))
	     (lst  (tellib-filter-list (cdr this)
				       (lambda (a) (nth 1 a)))))
	(when lst
	  (with-temp-buffer
	    (insert-file-contents file)
	    (end-of-buffer)
	    (dolist (this lst)
	      (insert (car this)) (newline))
	    (write-file file)))))))
;;test: (excerpt-kill-emacs-hook)

(defun excerpt-popup-menu (pos)
  (let* ((ol   (excerpt-get-my-overlay-at-pos pos))
	 (cat  (overlay-get ol 'excerpt-ref-cat))
	 (name (overlay-get ol 'excerpt-ref-name))
	 (src  (overlay-get ol 'excerpt-reference))
	 (beg  (overlay-start ol))
	 (end  (overlay-end ol)))
    (popup-menu
     `("This is an excerpt!"
       ["Goto point"        (goto-char ,pos)]
       ["Find source file"  (excerpt-find-file ,src nil ,cat ,name)]
       ["Remove excerpt!"   (excerpt-remove-excerpt ,ol ,beg ,end)]
       ["Update excerpt!"   (excerpt-update-current ,beg ,end nil ,cat ,name)]
       ))))

(defun excerpt-find-source-at-mouse (event)
  "Find source at mouse position."
  (interactive "e")
  (tellib-call-with-event-pos #'excerpt-popup-menu event))

(add-hook 'kill-emacs-hook (function excerpt-kill-emacs-hook))
(add-hook 'find-file-hooks (function excerpt-setup-all-excerpt))


(provide 'excerpt)


;;; excerpt.el ends here

;;; ;;; Local Variables:
;;; ;;; excerpt-end-string:";---(:end #{NAME})---"
;;; ;;; excerpt-start-string:";---(:excerpt #{CATEGORY})@(:begin #{NAME})---"
;;; ;;; auto-recompile:1
;;; ;;; time-stamp-format:"%y-%02m-%02d"
;;; ;;; excerpt-sources: "~/Etc/TL-Wiki/CompEmacsExcerpt"
;;; ;;; End:

