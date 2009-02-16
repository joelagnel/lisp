;;; From: Thomas Link <esendepe@yahoo.de>
;;; Subject: tellib.el 0.1.7
;;; Newsgroups: gnu.emacs.sources
;;; Date: Tue, 08 Oct 2002 14:04:51 +0200
;;; Organization: Vienna University, Austria

;;; Hi,

;;; This is a small library of hopefully useful elisp functions I use in
;;; several packages. I post it here because it's needed by wordnet.el,
;;; which I will post next.

;;; Cheers,
;;; Thomas.
;;; TELLIB.EL --- Thomas' ELisp LIBrary

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Thomas Link aka samul at web dot de
;; Time-stamp: <2002-10-04>
;; Keywords:

(defvar tellib-version "0.1.7")
(defvar tellib-homepage "http://members.a1.net/t.link/CompEmacsTellib.html")

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

; Some possibly useful functions.


;;; Change log:


;;; To do:


;;; Thanks to:

;; Christian Ohler (Christian DOT Ohler AT Informatik DOT Uni-Oldenburg DOT DE)
;;	help with `tellib-re-search'

;;; Code:

(eval-and-compile
  (unless (boundp 'tellib-running-xemacs)
    (defvar tellib-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
      "Non-nil means we are running XEmacs.")))



; ------------------------------------------- * Error & Message

(eval-and-compile
  (if tellib-running-xemacs
      (fset 'tellib-error 'error)
    ;; a version by RMS
    (defmacro tellib-error (class &rest args)
      "`error' wrapper."
      `(error (mapconcat 'identity ,args " "))))

  (defvar tellib-test-flag nil
    "Non-nil means, perform compile-time testing.")

  (defmacro tellib-test (name result &rest body)
    "Evaluate BODY at compile time and check if it returns RESULT.
\(Throw an error if this is not the case.)

Sample usage:
	\(tellib-test \"test-add-1\" 2 \(+ 1 1))
"
    (let ((val (make-symbol "val")))
      `(eval-when-compile
	 (when ,tellib-test-flag
	   (let ((,val (progn ,@body)))
	     (if (equal ,result ,val)
		 (message "TEST: %s succeeded" ,name)
	       (tellib-error 'error (format "TEST: %s failed: expected %S, got %S"
					    ,name ,result ,val))))))))
  
  (defmacro tellib-test-error (name error-list &rest body)
    "Evaluate BODY at compile time and check if it throws an error as expected.
ERROR-LIST ... a list of acceptable errors.
\(Throw an error if this is not the case.)

Sample usage:
	\(tellib-test-error \"test-add\" '\(some-error error) \(+ 1 \"a\"))
"
    `(eval-when-compile
       (when ,tellib-running-xemacs
       ;; As far as I remember Emacs doesn't have this kind of error
       ;; handling
	 (if (catch 'exit
	       (condition-case nil
		   (progn ,@body)
		 ,@(mapcar (lambda (err)
			     `(,err (throw 'exit t)))
			   error-list)
		 (t nil)))
	     (message "tellib: Test %s succeeded" ,name)
	   (tellib-error
	    'error
	    (format "tellib: Test %s didn't throw an acceptable error %S"
		    ,name ,error-list))))))
  )
;;test: (tellib-test "test-add-1" 2 (+ 1 1))
;;test: (tellib-test "test-add-2" 3 (+ 1 1))
;;test: (tellib-test-error "test-add-3" '(error) (+ 1 "a"))
;;test: (tellib-test-error "test-add-4" nil (+ 1 "a"))
;;test: (tellib-test-error "test-add-5" '(some-error) (+ 1 "a"))
;;test: (tellib-test-error "test-add-6" '(some-error error) (+ 1 "a"))
;;test: (tellib-test-error "tellib-test-error" '(error)
;;		   (tellib-test-error "test-add-5" '(some-error) (+ 1 "a")))

(defun tellib-not-yet-implemented (feature)
  (tellib-error 'error (format "`%s' is not yet implemented." feature)))

(defvar tellib-verbosity 1
  "An integer defining the level of verbosity. 0 means no messages
at all.")

(defun tellib-message (level &rest args)
  "Show a message only if LEVEL is sufficiently high.
This function can be called in two ways:

\(tellib-message LEVEL &rest ARGS): LEVEL will be tested against
`tellib-verbosity'. ARGS will be passed to `message'.

\(tellib-message LEVEL CLASS FORMAT-STRING &rest ARGS): LEVEL will be
tested against CLASS-verbosity \(or `tellib-verbosity' if
CLASS-verbosity isn't defined). FORMAT-STRING will be prepended with
\"CLASS: \".
"
  (let* ((class     (when (symbolp (car args))
		      (car args)))
	 (class$    (symbol-name (or class 'tellib)))
	 (args      (if class
			(append (list (concat (upcase-initials class$)
					      ": " (cadr args)))
				(cddr args))
		      args))
	 (name      (intern-soft (concat class$ "-verbosity")))
	 (verbosity (if name
			(eval name)
		      tellib-verbosity)))
    (when (<= level (abs verbosity))
      (apply 'message args)))
  nil)
;;test: (tellib-message 1 "test")
;;test: (tellib-message 2 "test")
;;test: (tellib-message 1 'tellib "test %s" 1)
;;test: (tellib-message 2 'tellib "test")

(defmacro tellib-testing (mode-string feature-name messagep &rest body)
  (cond
   ((equal mode-string "testing")
    `(progn ,@body))
   (messagep
    (message "Tellib: feature `%s' is disabled." feature-name)
    nil)
   (t
    nil)))

(defun tellib-info (name)
  "Display NAME's version information."
  (interactive)
  (let ((v  (eval (intern-soft (concat name "-version"))))
	(hp (eval (intern-soft (concat name "-homepage")))))
    (when (and v hp
	       (y-or-n-p (format "%s v%s: visit homepage? " name v)))
      (browse-url hp))))



; ------------------------------------------- * Events

(eval-and-compile
  (if tellib-running-xemacs
      (defun tellib-call-with-event-pos (tellib-fn tellib-event
						   &rest tellib-args)
	"Set current buffer and call FN with the position where EVENT occured.
This will result in \(FN POS . ARGS)."
	(set-buffer (window-buffer (event-window tellib-event)))
	(apply tellib-fn (cons (event-point tellib-event) 
			       tellib-args)))
    (defun tellib-call-with-event-pos (tellib-fn tellib-event
						 &rest tellib-args)
      "Set current buffer and call FN with the position where EVENT occured.
This will result in \(FN POS . ARGS)."
      (set-buffer (posn-window (event-start tellib-event)))
      (apply tellib-fn (cons (posn-point (event-start tellib-event)) 
			     tellib-args)))))



; ------------------------------------------- * Files

(defun tellib-directory-files (dir &optional
				   what file-pattern dir-pattern full-name-flag)
  "List files or dirs in DIR.
WHAT ... :dirs or :files.
"
  (cond
   ((file-exists-p dir)
    (let ((files           nil)
	  (dirs            nil)
	  (coll-files-flag (not (equal what ':dirs)))
	  (coll-dirs-flag  (not (equal what ':files)))
	  (get-name (lambda (filename)
		      (if full-name-flag
			  (concat (file-name-as-directory dir) filename)
			filename)))
	  (sort-fn (lambda (a b)
		     (string< (upcase a) (upcase b)))))
      (dolist (this (file-name-all-completions "" dir))
	(cond 
	 ((string-match "^\\.+/$" this)
	  nil)
	 ((and coll-dirs-flag (string-match "[:/\\]$" this))
	  (when (or (not dir-pattern)
		    (string-match dir-pattern this))
	    (setq dirs (append dirs (list (funcall get-name this))))))
	 (coll-files-flag
	  (when (or (not file-pattern)
		    (string-match file-pattern this))
	    (setq files (append files (list (funcall get-name this))))))))
      (append (when dirs  (sort (copy-sequence dirs)  sort-fn))
	      (when files (sort (copy-sequence files) sort-fn)))))
   (t
    (tellib-error 'error "Tellib: " dir " does not exist"))))
;;test: (tellib-directory-files "~/" :all "^[^.]" "^[^.]")
;;test: (tellib-directory-files "~/" :all)

(defun tellib-simplify-string (string &optional regexp)
  "Replace suspicious characters in STRING with an underscore."
  (replace-in-string string (or regexp "\\W") "_"))
(fset 'tellib-make-proper-filename 'tellib-simplify-string)
;;test: (tellib-make-proper-filename "a+b*c/")
;;test: (tellib-make-proper-filename "a+.b* c/" "[. ]")

(defun tellib-file-name-last-bit (filename)
  "Returns the last bit of FILENAME -- either directory or file."
  (if (file-directory-p filename)
      (file-name-as-directory
       (file-name-nondirectory
	(directory-file-name filename)))
    (file-name-nondirectory filename)))

(defun tellib-file-name-break-up (filename)
  "Return a list of FILENAME parts.
\"\" at position 0 means: absolute path.
\"\" at the last position means: it's a directory."
  (tellib-split-string-by-char filename directory-sep-char))
;;test: (tellib-file-name-break-up "/d1/d2/filename")
;;test: (tellib-file-name-break-up "/d1/d2/d3/")



; ------------------------------------------- * Lists 

(defun tellib-filter-list (tellib-list tellib-cond-fn)
  "Remove all elements not conforming to TELLIB-COND-FN from list TELLIB-LIST.
COND-FN takes one argument: the current element."
  (let ((rv nil))
    (dolist (elt tellib-list rv)
      (when (funcall tellib-cond-fn elt)
	(setq rv (append rv (list elt)))))))

(defun tellib-sublist (tellib-list tellib-beg &optional tellib-end)
  "Get the sublist of TELLIB-LIST from TELLIB-BEG to TELLIB-END - 1."
  (let ((rv  nil)
	(i   tellib-beg)
	(top (or tellib-end
		 (length tellib-list))))
    (while (< i top)
      (setq rv (append rv (list (nth i tellib-list))))
      (setq i (+ i 1)))
    rv))

(defun tellib-zip (&rest tellib-lists)
  "Zip lists. Turns '\((A1 ...) (A2 ...) ...) into '\((A1 A2 ...) ...)."
  (let ((max nil))
    (dolist (this tellib-lists)
      (if (and max
	       (/= (length this) max))
	  (tellib-error 'error "tellib-zip: Lists have to be of same length.")
	(setq max (length this))))
    (let ((rv nil))
      (do ((pos 0 (setq pos (+ pos 1))))
	  ((>= pos max) rv)
	(setq rv (append rv (list (mapcar (lambda (x) (nth pos x))
					  tellib-lists))))))))
;;test: (tellib-zip '(1 2 3))
;;test: (tellib-zip '(1 2 3) '(a b c))
;;test: (tellib-zip '(1 2 3) '(a b c) '(A B C))
;;test: (tellib-zip '(1 2 3) '(a b c) '(A B C D))

(defun tellib-zip-1 (default &rest tellib-lists)
  "Zip lists. Like `tellib-zip-1' but accepts lists of unequal length --
i.e. missing elements will be replaces with DEFAULT."
  (let ((max 0))
    (dolist (this tellib-lists)
      (let ((lt (length this)))
	(when (> lt max)
	  (setq max lt))))
    (let ((rv nil))
      (do ((pos 0 (setq pos (+ pos 1))))
	  ((>= pos max) rv)
	(setq rv (append rv (list (mapcar (lambda (x)
					    (if (>= pos (length x))
						default
					      (nth pos x)))
					  tellib-lists))))))))
;;test: (tellib-zip-1 'nope '(1 2 3))
;;test: (tellib-zip-1 'nope '(1 2 3) '(a b))
;;test: (tellib-zip-1 'nope '(1 2 3) '(a b) '(A))
;;test: (tellib-zip-1 'nope '(1 2 3) nil '(A B C D))

(defun tellib-andmap (tellib-pred &rest tellib-lists)
  "Return non-nil if TELLIB-PRED is true for all elements of TELLIB-LISTS."
  (catch 'failure
    (dolist (tellib-this (apply 'tellib-zip tellib-lists) t)
      (unless (apply tellib-pred tellib-this)
	(throw 'failure nil)))))
;;test: (tellib-andmap 'equal '(1 2) '(1 2))
;;test: (tellib-andmap 'equal '(1 2) '(1 3))

;(defun tellib-ormap (tellib-pred tellib-list)
;  "Return SUBLIST of TELLIB-LIST, for which \(TELLIB-PRED (car SUBLIST)) is true."
;  (let ((tellib-list tellib-list)
;	(tellib-rv nil))
;    (while (and (not (null tellib-list))
;		(null tellib-rv))
;      (if (funcall tellib-pred (car tellib-list))
;	  (setq tellib-rv tellib-list)
;	(setq tellib-list (cdr tellib-list))))
;    tellib-rv))
(defmacro tellib-ormap (tellib-pred tellib-list)
  "Return SUBLIST of TELLIB-LIST, for which \(TELLIB-PRED (car SUBLIST)) is true."
  (let ((list (gensym "tellib-list"))
	(rv   (gensym "tellib-rv")))
    `(let ((,list ,tellib-list)
	   (,rv   nil))
       (while (and (not (null ,list))
		   (null ,rv))
	 (if (funcall ,tellib-pred (car ,list))
	     (setq ,rv ,list)
	   (setq ,list (cdr ,list))))
       ,rv)))
;;test: (tellib-ormap (lambda (x) (> x 2)) '(1 2 3))

(defun tellib-some (tellib-pred tellib-list)
  "Return the first item of list for which TELLIB-PRED is true."
  (catch 'exit
    (dolist (tellib-this tellib-list nil)
      (let ((tellib-rv (funcall tellib-pred tellib-this)))
	(when tellib-rv
	  (throw 'exit tellib-rv))))))
;(fset 'tellib-some 'some)

(defun tellib-member (tellib-item tellib-list &rest tellib-keys)
  "A clone of `member*'. At the moment only the :test key is supported."
  (let ((tellib-test (plist-get tellib-keys ':test (function equal))))
    (tellib-ormap (lambda (tellib-this)
		      (funcall tellib-test tellib-item tellib-this)) 
		    tellib-list)))
;(fset 'tellib-member 'member*)

(defun tellib-mapcart (tellib-fn tellib-list)
  "Similar to `mapcar' but drops nil values.
Apply TELLIB-FN to each element of TELLIB-LIST. If the result is
non-nil, append it to the list of return values."
  (let (tellib-rv)
    (dolist (tellib-this tellib-list tellib-rv)
      (let ((tellib-tmp (funcall tellib-fn tellib-this)))
	(when tellib-tmp
	  (setq tellib-rv (append tellib-rv (list tellib-tmp))))))))
;;test: (tellib-mapcart #'identity '(1 nil 2))



; ------------------------------------------- * Property Lists

(eval-and-compile
  (if (fboundp 'valid-plist-p)
      (fset 'tellib-valid-plist-p 'valid-plist-p)
    (defun tellib-valid-plist-p (tellib-plist)
      "XEmacs' valid-plist-p mimicry."
      (= (mod (length tellib-plist) 2) 0))))



; ------------------------------------------- * Lax Property Lists

(eval-and-compile
  (if (fboundp 'lax-plist-get)
      (fset 'tellib-lax-plist-get 'lax-plist-get)
    (defun tellib-lax-plist-get (lax-plist property &optional default)
      "Emacs support for XEmacs' lax-plists."
      (if (tellib-valid-plist-p lax-plist)
	  (let ((this (member property lax-plist)))
	    (if this
		(if (= (mod (length this) 2) 0)
		    (cadr this)
		  (tellib-lax-plist-get (cdr this) property default))
	      default))
	(tellib-error 'malformed-property-list
		      'tellib-lax-plist-get lax-plist))))
  
  (if (fboundp 'lax-plist-put)
      (fset 'tellib-lax-plist-put 'lax-plist-put)
    (defun tellib-lax-plist-put (lax-plist property value)
      "Emacs support for XEmacs' lax-plists."
      (if (tellib-valid-plist-p lax-plist)
	  (let ((this (member property lax-plist)))
	    (if this
		(progn
		  (if (= (mod (length this) 2) 0)
		      (setcdr this (cons value (cddr this)))
		    (setcdr this (tellib-lax-plist-put (cdr this)
						       property value)))
		  lax-plist)
	      (append (list property value) lax-plist)))
	(tellib-error 'malformed-property-list
		      'tellib-lax-plist-put lax-plist)))))



; ------------------------------------------- * Association Lists

(defun tellib-alist-get (alist key &optional default car-flag)
  "Get KEY's value in the association list ALIST.
Return DEFAULT if not found.  Return \(car value) if CAR-FLAG is non-nil."
  (let* ((elt (assoc key alist)))
    (cond
     (elt
      (if car-flag
	  (cadr elt)
	(cdr elt)))
     (default default)
     (t nil))))

(defun tellib-alist-set (alist key &rest values)
  "Replace KEY's value with VALUES in ALIST or append (KEY . VALUES) to ALIST."
  (let ((this (assoc key alist)))
    (if this
	(progn
	  (setcdr this values)
	  alist)
      (append alist (list (cons key values))))))
;;test: (tellib-alist-set '(("a" 1) ("b" 2)) "a" 3)
;;test: (tellib-alist-set '(("a" 1) ("b" 2)) "a" 3 4 5)
;;test: (tellib-alist-set '(("a" 1) ("b" 2)) "x" 3)
;;test: (tellib-alist-set nil "x" 3)

(defun tellib-alist-setcdr (alist key list)
  "Replace KEY's cdr with LIST in ALIST or append (KEY . LIST) to ALIST."
  (if (listp list)
      (let ((this (assoc key alist)))
	(if this
	    (progn
	      (setcdr this list)
	      alist)
	  (append alist `((,key . ,list)))))
    (tellib-error 'error "tellib-alist-setcdr: not a list" list)))
;;test: (tellib-alist-setcdr '(("a" 1) ("b" 2)) "a" 3) -> incorrect
;;test: (tellib-alist-setcdr '(("a" 1) ("b" 2)) "a" '(3 4 5))
;;test: (tellib-alist-setcdr '(("a" 1) ("b" 2)) "x" 3) -> incorrect
;;test: (tellib-alist-setcdr nil "x" 3) -> incorrect
;;test: (tellib-alist-setcdr nil "x" '(3))

(defun tellib-alist-p (alist)
  "Return non-nil if ALIST appears to be a proper association list."
  ;;  (and (listp alist)
  ;;       (catch 'exit
  ;;	 (dolist (this alist t)
  ;;	   (unless (listp this)
  ;;	     (throw 'exit nil))))))
  (tellib-andmap #'listp alist))

;;test: (tellib-alist-p nil)
;;test: (tellib-alist-p '(nil))
;;test: (tellib-alist-p '(1 nil))
;;test: (tellib-alist-p '((a 1) (b 2)))
;;test: (tellib-alist-p '((a 1) b 2))



; ------------------------------------------- * RegExp, search & find

(defun tellib-re-search (regexp &optional
				case-sensitive
				backward-flag)
  "Search for REGEXP while taking care of `case-fold-search'.

CASE-SENSITIVE can be t, nil, or 'default \(don't change the value of
`case-fold-search').
If BACKWARD-FLAG is non-nil, the search will proceed in reverse direction.
"
;  (let ((cfs case-fold-search)
;	(rv  (progn
;	       (when (not (equal case-sensitive 'default))
;		 (setq case-fold-search (not case-sensitive)))
;	       (if backward-flag
;		   (re-search-backward regexp nil t)
;		 (re-search-forward regexp nil t)))))
;    (setq case-fold-search cfs)
;    rv))
;;; the following version was kindly proposed by Christian Ohler
  (let ((case-fold-search (if (eq case-sensitive 'default)
			      case-fold-search
			    (not case-sensitive))))
    (if backward-flag
	(re-search-backward regexp nil t)
      (re-search-forward regexp nil t))))



; ------------------------------------------- * Work on current buffer

(defun tellib-update-local-variable-def (var val &optional dont-replace-flag)
  "Set local VAR's definition to VAL.
If DONT-REPLACE-FLAG is non-nil, only add new entries.

Problem: This function moves the cursor (XEmacs 21.4.8) but it shouldn't."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (let* ((begrex (concat "^" comment-start 
			   "[" comment-start " ]+Local Variables:"))
	   (begloc "Local Variables:")
	   (endloc "End:")
	   (lvs    (let ((here (tellib-re-search begrex t t)))
		     (when here
		       (goto-char here)
		       (tellib-re-search begloc t))))
	   (pre    (if lvs
		       (progn
			 (goto-char (match-beginning 0))
			 (buffer-substring (point-at-bol) (point)))
		     (concat comment-start comment-start comment-start " ")))
	   (post   (if lvs
		       (progn
			 (goto-char (match-end 0))
			 (buffer-substring (point) (point-at-eol)))
		     (concat " " comment-end comment-end comment-end))))
      (let* ((lvss (concat pre begloc post))
	     (lves (concat pre endloc post))
	     (lve  (when lvs
		     (goto-char lvs)
		     (tellib-re-search (regexp-quote lves) t))))
	(save-restriction
	  (if lve
	      (progn
		(goto-char lvs)
		(end-of-line)
		(narrow-to-region lvs lve))
	    (let ((beg (point-max))
		  here)
	      (end-of-buffer)
	      (newline)
	      (insert lvss)
	      (setq here (point))
	      (newline)
	      (insert lves)
	      (newline)
	      (narrow-to-region beg (point))
	      (goto-char here)))
	  (let* ((sfss (concat "^"
			       (regexp-quote pre)
			       (format "%s:\\W*\\(.*\\)"
				       (regexp-quote (if (stringp var)
							 var
						       (symbol-name var))))
			       (regexp-quote post)
			       "$"))
		 (sfsn (concat pre
			       (format "%s: %S" var val)
			       post))
		 (sfs  (when lve
			 (tellib-re-search sfss t))))
	    ;;(message "DEBUG: %s %s %s" lve sfs sfss)(sleep-for 3)
	    ;;(when lve (widen))
	    (if sfs
		(unless dont-replace-flag
		  ;;(goto-char (match-beginning 0))
		  ;;(delete-region (match-beginning 0) (match-end 0))
		  ;;(insert sfsn))
		  (replace-match sfsn t t))
	      (newline)
	      (insert sfsn))))))))
;;test: (tellib-update-local-variable-def 'var "val")
;;test: (tellib-update-local-variable-def 'auto-recompile 1)
;;test: (tellib-update-local-variable-def 'auto-recompile 2)
;;test: (tellib-update-local-variable-def 'auto-recompile 2 t)



; ------------------------------------------- * String, Text etc.

(defun tellib-quote (txt)
  "Return TXT in quotes."
  (concat "\"" txt "\""))

(defun tellib-string-count (string regexp)
  "Count occurances of REGEXP in STRING."
  (let ((pos   0)
	(count 0))
    (while (string-match regexp string pos)
      (setq count (+ count 1))
      (setq pos   (match-end 0)))
    count))
;;test: (tellib-string-count "abcabc" "x")
;;test: (tellib-string-count "abcabc" "a")
;;test: (tellib-string-count "abcabc" "[abc]+")

(eval-and-compile
  (if tellib-running-xemacs
      (fset 'tellib-split-string-by-char 'split-string-by-char)
    (defmacro tellib-split-string-by-char (string sepchar)
      "Split STRING into a list of substrings originally separated by SEPCHAR."
      `(split-string ,string (regexp-quote (char-to-string ,sepchar))))))

(defun tellib-replace-args (string args-alist &optional escape-char)
  "Replace args-alist in string. 

ARGS-ALIST is an association list with the form '\(\(SHORTCUT
REPLACEMENT) ...). A shortcut may be any string except \"§\".

The default for ESCAPE-CHAR is '%'."
  (let* ((esc (or escape-char ?\%))
	 (rv  (replace-in-string string (string esc esc) (string esc ?\§))))
    (replace-in-string
     (dolist (this args-alist rv)
       (let ((sc (nth 0 this))
	     (rs (nth 1 this)))
	 (setq rv (replace-in-string rv (format "%c%s" esc sc) rs t))))
     (string esc ?\§)
     (string esc)
     t)))
;;test: (tellib-replace-args "test %x" '(("x" "0^0")))
;;test: (tellib-replace-args "test %x" '(("x" "0\\10")))



; ------------------------------------------- * Variables

(defun tellib-scan-variables (trunc &optional beg end buffer)
  "Scan variables TRUNC-??? and combine there values to a list.
BEG (default=1) and END (default=infinit) are numbers."
  (let ((count  (or beg 1))
	(end    (or end nil))
	(rv     nil)
	(varlist (buffer-local-variables buffer))
	(trunc$ (if (stringp trunc)
		    trunc
		  (symbol-name trunc))))
    (catch 'exit
      (while (or (null end)
		 (<= count end))
	(let ((this (intern-soft (format "%s-%s" trunc$ count))))
	  (if this
	      (setq count (+ count 1)
		    rv    (append rv (list (cdr (assoc this varlist)))))
	    (throw 'exit nil)))))
    rv))
;;test: tellib-scan-test-1, tellib-scan-test-2
;;test: (tellib-scan-variables 'tellib-scan-test)     -> ("a" "b")
;;test: (tellib-scan-variables 'tellib-scan-test 1 1) -> ("a")
;;test: (tellib-scan-variables 'tellib-scan-test 3 3) -> nil



; ------------------------------------------- * Version check

(defun tellib-version-check-p (version-string required-string)
  "Return non-nil if VERSION-STRING satisfies REQUIRED-STRING.
A version string has the form \"MAJOR\", \"MAJOR.MINOR\", or
\"MAJOR.MINOR.MICRO\" etc.

VERSION-STRING and REQUIRED-STRING have to adhere to the same version
numbering system, i.e. they must have the same length.
"
  (let* ((mapfn (lambda (x)
		  (if (and (> (length x) 3)
			   (equal (substring x 0 3) "pre"))
		      (- (/ 1 (string-to-int (substring x 3))))
		    (string-to-int x))))
	 (vl    (mapcar mapfn
			(tellib-split-string-by-char version-string ?\.)))
	 (rl    (mapcar mapfn
			(tellib-split-string-by-char required-string ?\.))))
    (tellib-andmap (lambda (x y)
		     (>= x y))
		   vl rl)))
;;test: (tellib-version-check-p "1" "1") -> t
;;test: (tellib-version-check-p "1" "2") -> nil
;;test: (tellib-version-check-p "2" "1") -> t
;;test: (tellib-version-check-p "1.1" "2.0") -> nil
;;test: (tellib-version-check-p "1.3" "1.2") -> t
;;test: (tellib-version-check-p "1.2.-99" "1.2.0") -> nil
;;test: (tellib-version-check-p "1.2.pre1" "1.2.0") -> nil
;;test: (tellib-version-check-p "1.2.pre1" "1.2.pre2") -> nil
;;test: (tellib-version-check-p "1.2.pre2" "1.2.pre1") -> t

;(defun tellib-version-check (name version-string required-string)
;  "Throw an error if VERSION-STRING doesn't fit REQUIRED-STRING."
;  (unless (tellib-version-check-p version-string required-string)
;    (tellib-error
;     'error
;     (format "excerpt: %s is v%s, but at least v%s is required"
;	     name version-string required-string))))
(defmacro tellib-version-check (name version-string required-string)
  "Throw an error if VERSION-STRING doesn't fit REQUIRED-STRING."
  `(eval-when-compile
     (unless (tellib-version-check-p ,version-string ,required-string)
       (tellib-error
	'error
	(format "excerpt: %s is v%s, but at least v%s is required"
		,name ,version-string ,required-string)))))

(defmacro tellib-version-check-with-name (package-name required-string)
  "Throw an error if PACKAGE-NAME-version doesn't fit REQUIRED-STRING."
  `(tellib-version-check ,package-name
			 (eval (intern-soft (concat ,package-name "-version")))
			 ,required-string))


(provide 'tellib)

;;; TELLIB.EL ends here

;;; ;;; Local Variables: ***
;;; ;;; auto-recompile: 1 ***
;;; ;;; time-stamp-format:"%y-%02m-%02d" ***
;;; ;;; tellib-scan-test-1:"a" ***
;;; ;;; tellib-scan-test-2:"b" ***
;;; ;;; End: ***
