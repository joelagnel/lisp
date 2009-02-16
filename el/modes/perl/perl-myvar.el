;;; perl-myvar.el --- Declare lexicaly scoped vars as my().
;;
;; ~harley/share/emacs/pkg/perlvar/perl-myvar.el ---
;;
;; $Id: perl-myvar.el,v 1.23 2004/10/07 07:10:17 harley Exp $
;;

;; Author:    Harley Gorrell <harley@panix.com>
;; URL:       http://www.mahalito.net/~harley/elisp/perl-myvar.el
;; License:   GPL v2
;; Keywords:  perl, emacs, my, declare

;;; Commentary:

;; Declaring all those variables in a perl sub can be time
;; consuming and I seem to overlook one or two now and then.
;; 'pmv-declare' insures that all the variables are
;; declared.

;; Bind 'pmv-declare' to a function key and go wild.
;; Inserting these two lines in your .emacs should be sufficient.
;;  (require 'perl-myvar)
;;  (define-key perl-mode-map [f11] 'pmv-declare)

;; This package expects 'properly' indented code to find sub
;; starts. That is not a big restriction when using
;; perl-mode, so pmv expects it.

;; Global variables are variables which start in the first
;; column anywhere in the buffer.  Variables declared with
;; 'my' or 'local' should have only whitespace in front of
;; them.  You may use '#global(vars);' to document global
;; variables.  '#ignore(vars);' may be used to work around
;; over zealous declerations on the part of pmv-declare.

;; If font-lock-mode is on, pmv-declare will use the face of
;; text to avoid extracting variables from strings.  If not,
;; it will think "%s" is a hash.

;;; History:
;;  2003-03-16 : Updated URL and contact info.


;;(require 'perl-mode)

;;; Code:

(defvar pmv-always-ignored '( "%ENV" "@ARGV" )
  "*List of variables which are always ignored.")

(defvar pmv-always-local '( "$a" "$b" )
  "*List of variables which should be local.
These are typically the variables used in a sort() expression.")

(defvar pmv-declare-global t
  "*Should `pmv-declare' declare globals?")

(defvar pmv-declare-local t
  "*Should `pmv' declare locals?")

(defvar pmv-tag-string "#" ;"#pmv"
  "*String to tag inserted lines with.")

(defvar pmv-always-update-global-vars t
  "*Always scan the buffer for global vars before inserting declarations.
Set to nil if you have a big buffer.  The
global variable list may be manually updated with
'pmv-set-global-list'." )

(defvar pmv-buffer-global-vars nil
  "Global perl variables used in this buffer."  )

(defvar pmv-declarer 'pmv-declarer-default
  "The function to determine the decleraton type.")

(defvar pmv-variable-regexp 
  "[$@%][A-Za-z][A-Za-z0-9_]*\\(::[A-Za-z][A-Za-z0-9_]+\\)*[[{]?"
  "A regexp to match perl variables.
It should match the starting sigil '$%@' and the beginning of the deref
if any.  '::' is allowed as part of a package name.")

;; Make 'em all buffer local
(make-variable-buffer-local 'pmv-always-ignored)
(make-variable-buffer-local 'pmv-always-local)
(make-variable-buffer-local 'pmv-declare-global)
(make-variable-buffer-local 'pmv-declare-local)
(make-variable-buffer-local 'pmv-tag-string)
(make-variable-buffer-local 'pmv-always-update-global-vars)
(make-variable-buffer-local 'pmv-buffer-global-vars)
(make-variable-buffer-local 'pmv-declarer)

;;
(defun pmv-find-vars (r-start r-end)
  "Return a list of variables in the region with their declaration.

The list is formatted like: ((var1 .  decl) (var2 .  decl) ... )
Where var is a string and decl is one of
   'special
   'global-decl     'global-use
   'local-decl      'local-use
   'my-decl         'my-use
   'ignore-decl
Argument R-START region start.
Argument R-END region end."
  (interactive "r")
  (let ((v-list nil) v-name v-loc v-pos v-type v-face)
    (save-excursion
      ;; top
      (goto-char r-start)

      ;; find the next var
      ;; FIXME: what about vars of the form $main::foo="bar";
      ;;        or $v=defined($v)?$v:1;
      (while (search-forward-regexp pmv-variable-regexp r-end t)

	;; get its name
	(setq v-name (buffer-substring-no-properties
		      (match-beginning 0) (match-end 0)))
	(setq v-name (pmv-proper-var v-name))

	;; get its face
	(setq v-face (get-text-property (match-beginning 0) 'face))
	  
	;; How is this variable defined?
	(setq v-pos (match-end 0))
	(beginning-of-line)
	(setq v-type
	      (cond
	       ;; special, global
	       ((member v-name pmv-always-ignored) 'special)
	       ((assoc v-name pmv-buffer-global-vars) 'global-decl)
	       ;; declarations
	       ((looking-at "\\s-*#global(") 'global-decl)
	       ((looking-at "\\s-*local(") 'local-decl)
	       ((looking-at "\\s-*my(") 'my-decl)
	       ((looking-at "\\s-*#ignore(") 'ignore-decl)
	       ;; in a string or comment?
	       ((and 
                 font-lock-mode
                 (boundp 'font-lock-string-face)
		 (or (eq v-face font-lock-string-face)
		     (eq v-face font-lock-comment-face))) nil) ; forget
	       ;; Used in some way... How should we declare it?
	       (t (funcall pmv-declarer v-name))))
        ;; back to pos
	(goto-char v-pos)

	;; add to list
	(if (and v-type (not (assoc v-name v-list)))
	    (setq v-list (cons (cons v-name v-type) v-list))))
      ;; the vars
      v-list)))

(defun pmv-find-global-vars (r-start r-end)
  "Return a list of global varibles in the region.
Argument R-START region start.
Argument R-END region end."
  (interactive "r")
  (save-excursion
    (let (v-name v-list)
      ;; top
      (goto-char r-start)
      ;; find
      (while (search-forward-regexp "^[$@%][A-z][A-Za-z0-9_:]*" r-end t)
	(setq v-name (buffer-substring-no-properties
                      (match-beginning 0) (match-end 0)))
	(setq v-list (cons (cons v-name 'global) v-list)))
      ;;
      v-list)))


(defun pmv-declarer-my (v)
  "Always declare the variable V as 'my'."
  'my)

(defun pmv-declarer-default (v)
  "Deterimine what kind of declaration the variable V should receive.
Return value is one of special, global, local, or my."
  (let ((case-fold-search nil))
    (cond
     ((member v pmv-always-local) 'local-use)
     ((string-match "^%ENV$" v) 'special)
     ((string-match "^[$@%][A-Z]+$" v) 'global-use)
     ((string-match "::" v) 'global-use)
     ((string-match "^[$@%]opt_" v) 'global-use)
     ((string-match "^\\$[ab]$" v) 'local-use) ;; for sort({$a<=>$b})
     (t 'my-use))))

(defun pmv-filter-vars (v-type v-list)
  "Filter by the type V-TYPE the list of variables in V-LIST."
  (let ((v-out ()))
    (while v-list
      (if (eq (cdr (car v-list)) v-type)
	  (setq v-out (cons (car (car v-list)) v-out)))
      (setq v-list (cdr v-list)))
    v-out))

;; FIXME "$#ARGV" => @ARGV
(defun pmv-proper-var (v-name)
  "Convert the perl variable V-NAME to its 'proper' form.
The expression '$foo{...}' is a use of the hash '%foo',
while '$bar[...]' is a use of the list '@bar'."
  (let (v-type v-name-len)
    ;;
    (setq v-name-len (1- (length v-name)))
    (setq v-type (aref v-name v-name-len))
    ;;
    (cond
     ((= v-type   91 ) ; [
      (concat "@" (substring v-name 1 v-name-len)))
      ((= v-type 123 ) ; {
       (concat "%" (substring v-name 1  v-name-len)))
      (t v-name) )))

;; (pmv-proper-var "$foo{")
;; (pmv-proper-var "$foo[")
;; (pmv-proper-var "$foo")

(defun pmv-insert (v-decl v-list)
  "Insert the declaration V-DECL of the list of variables V-LIST."
  (beginning-of-line)
  (insert " " v-decl "(") ; " " to move it off col 0 for tabbing
  (while v-list
    (insert (car v-list))
    (setq v-list (cdr v-list))
    (if v-list
	(insert ","))
    )
  ;; The "#" is to mark the lines this function inserts.
  (insert "); " pmv-tag-string)
  (indent-according-to-mode)
  (insert "\n") )

;; (pmv-insert "my" '( "aaa" "bbb"))

(defun pmv-set-global-list ()
  "Set the `pmv' cache of global variables."
  (interactive)
  (setq pmv-buffer-global-vars
	(pmv-find-global-vars (point-min) (point-max)) ))

(defun pmv-clear-global-list ()
  "Clear the `pmv' cache of global variables."
  (interactive)
  (setq pmv-buffer-global-vars nil))


;;;###autoload
(defun pmv-declare ()
  "Examine the current perl subroutine and insert a declaration.
Variables are considered 'declared' if they appear as:

     my($myvar);
     local($localvar);
or
     #global($globalvar);
     #ignore(%d);

Used but undeclared variables are inserted as a 'my' declaration.

If font-lock mode is active, the faces are used to do a better job."
  (interactive)
  (let (r-s r-e v-list g-list l-list m-list)
    ;; Select the block
    (save-excursion
      ;; something broken here...
      (perl-mark-function) ;; I want to fiddle with the mark
      (setq r-s (point) 
	    r-e (mark t)))
    ;; Inside a sub?
    (if (and r-s r-e (< r-s (point)) (> r-e (point)))
	(progn
	  ;; Refresh globals?
	  (if (or pmv-always-update-global-vars
		  (not pmv-buffer-global-vars))
	      (pmv-set-global-list))
	  ;; Make a list of vars in the region
	  (setq v-list (pmv-find-vars r-s r-e))
	  ;; global?
	  (if pmv-declare-global
	      (progn
		(setq g-list (pmv-filter-vars 'global-use v-list))
		(if g-list (pmv-insert "#global" (sort g-list 'string<)))))
	  ;; local?
	  (if pmv-declare-local
	      (progn
		(setq l-list (pmv-filter-vars 'local-use v-list))
		(if l-list (pmv-insert "local" (sort l-list 'string<)))))
	  ;; my
	  (setq m-list (pmv-filter-vars 'my-use v-list))
	  (if m-list (pmv-insert "my" (sort m-list 'string<))) ))))

;;
(provide 'perl-myvar)

;;; perl-myvar.el ends here
