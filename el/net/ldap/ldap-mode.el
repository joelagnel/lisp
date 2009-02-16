;;; ldap-mode.el --- major modes for editing LDAP schema and LDIF files  -*-coding: iso-8859-1;-*-

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: data
;; $Revision: 1.2 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; LDAP schema is defined in RFC 2252.
;; LDIF is defined in RFC 2849.
;; Both sorts of file are defined to use UTF-8 strings and so should
;; presumably use UTF-8 as the file coding system.

;; The RFC ASN.1 description of schemas isn't actually the syntax you
;; need to put in OpenLDAP schema files -- each object or attribute
;; definition is prefixed by an option name.  See
;; `ldap-convert-asn1-schema' for conversion.

;; This file isn't called ldap.el, since that's part of EUDC.

;;; Code:

(eval-when-compile
  ;; Emacs <= 21.3 doesn't expand rx at compile time and has a single arg.
  (unless (equal "" (macroexpand '(rx "")))
    (defmacro rx (&rest regexps)
      (if (cdr regexps)
	  (rx-to-string `(and ,@regexps) t)
	(rx-to-string (car regexps) t))))
  (defvar imenu-use-markers))

;;;; LDAP

(defconst ldap-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?' "\"" table)
    table))

(defconst ldap-font-lock-keywords
  `(,(rx
      word-start
      (or
       ;; §S4.2 in RFC 2252 ("NAME" is done below.)
       "DESC" "OBSOLETE" "SUP" "EQUALITY" "ORDERING" "SUBSTR" "SYNTAX"
       "SINGLE-VALUE" "COLLECTIVE" "NO-USER-MODIFICATION" "USAGE"
       ;; §S4.4
       "ABSTRACT" "STRUCTURAL" "AUXILIARY" "MUST" "MAY"
       ;; §S4.5
       "APPLIES"
       ;; §6.11
       "AUX" "NOT"
       ;; These are OpenLDAP option names occurring in schema files.
       ;; (Schema elements are part of the syntax of slapd.conf, but
       ;; the schemas are normally kept in separate, included files.)
       ;; slapd.conf(5) doesn't say the option names are
       ;; case-insensitive, but the distributed schema files contain
       ;; both "objectclass" and "objectClass", for instance.
       "attributetype" "objectclass" "attributeType" "objectClass"
       "ditcontentrule" "ditContentRule"
       "objectIdentifier" "objectidentifier")
      word-end)
    (,(rx word-start "NAME" word-end)	; highlighting defined name
     (0 'font-lock-keyword-face)
     (ldap-match-string-and-skip
      (skip-chars-forward "^'(") nil
      ;; If we fontified strings this would need to specify overriding.
      (1 'font-lock-variable-name-face)))
    (,(rx line-start (or "objectIdentifier" "objectidentifier")
	  (1+ blank) (group (1+ word)))
     (1 'font-lock-type-face))		; highlight defined oid name
    (,(rx word-start (group "include") (1+ blank) (group (1+ not-newline)))
     (1 'font-lock-keyword-face) (2 'font-lock-string-face))
    ;; §4.2
    (,(rx word-start (or "userApplications" "directoryOperation"
			 "distributedOperation" "dSAOperation")
	  word-end)
     (0 'font-lock-constant-face))))

(defconst ldap-syntactic-keywords
  `((,(rx not-newline (group "#")) 1 "."))
  "Give `#' punctuation syntax when not at line beginning.")

(defconst ldap-string-regexp
  (rx "'" (group (1+ (or "\\'" (not (any ?'))))) "'"))

(defun ldap-match-string-and-skip (limit)
  "Fontification anchored match function.
Deals with fontifying contents of a string or list of strings."
  (condition-case ()
      (if (and
	   (if (fboundp 'syntax-ppss)
	       (not (syntax-ppss-context (syntax-ppss))) ; check if in comment
	     t)
	   (re-search-forward ldap-string-regexp limit t))
	  (skip-chars-forward " \t\n"))
    (error nil)))

(defvar ldap-is-openldap nil
  "Non-nil means the buffer is assumed to visit an OpenLDAP schema file.")
(make-variable-buffer-local 'ldap-is-openldap)

(defun ldap-create-index ()
  "`imenu-create-index-function' for LDAP schema."
  ;; Look for `NAME 'name'' or `NAME ( 'name' 'name' ...' not in comments.
  (let (case-fold-search attributes objects oids)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(while (re-search-backward "\\<NAME\\>" nil t)
	  (unless (save-match-data	; check if in comment
		    (if (fboundp 'syntax-ppss)
			(syntax-ppss-context (syntax-ppss))
		      (save-excursion
			(back-to-indentation)
			(eq ?# (char-after)))))
	    (save-excursion
	      (goto-char (match-end 0))
	      (skip-chars-forward " \t\n(")
	      (let ((pos (if imenu-use-markers (point-marker) (point)))
		    (attr (save-excursion
			    (save-match-data
			      (beginning-of-defun)
			      (if ldap-is-openldap (forward-sexp))
			      (not (ldap-object-def-p))))))
		(while (looking-at ldap-string-regexp)
		  (save-excursion
		    (let ((elt (cons (match-string-no-properties 1) pos)))
		      (if attr
			  (push elt attributes)
			(push elt objects))))
		  (goto-char (match-end 0))
		  (skip-chars-forward " \t"))))))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(let ((case-fold-search t))
	  (while (re-search-backward
		  (rx line-start "objectidentifier" (1+ blank)
		      (group (1+ word)))
		  nil t)
	    (push (cons (match-string-no-properties 1)
			(if imenu-use-markers
			    (copy-marker (match-beginning 1))
			  (match-beginning 1)))
		  oids)))))
    (list (if oids (cons "objectidentifiers" oids))
	  (if objects (cons "objectclasses" objects))
	  (if attributes (cons "attributetypes" attributes)))))

(defconst ldap-defun-regexp
  (rx line-start (or "attributetype" "objectclass") word-end))

(define-skeleton ldap-attr-skeleton
  "Insert OpenLDAP attribute definition skeleton."
  "Name? " "attributetype ( " _ "\n    NAME '" str "' " \n _  > ")\n")

(define-skeleton ldap-obj-skeleton
  "Insert OpenLDAP object definition skeleton."
  "Name? " "objectclass ( " _ "\n    NAME '" str "' " \n _  > ")\n")

(define-skeleton ldap-change-skeleton
  "Insert LDIF change skeleton."
  nil "dn: " _ "\nchangetype: "
  (completing-read "Change type? "
		   '(("add") ("delete") ("modify") ("modrdn") ("moddn")))
  ?\n _ ": " _ "\n-\n\n")

(defvar ldap-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-a" #'ldap-attr-skeleton)
    (define-key map "\C-c\C-o" #'ldap-obj-skeleton)
    map))

(defun ldap-openldap-p ()
  "Return non-nil if this appears to be an OpenLDAP schema file.
The criterion is the presence of `attributetype' or `objectclass' options."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (let ((case-fold-search t))
	(re-search-forward (rx line-start
			       (or "attributetype" "objectclass")
			       word-end)
			   10000 t)))))

;; Fixme: Do something better for indentation than `indent-relative'?

;;;###autoload
(define-derived-mode ldap-mode fundamental-mode "LDAP"
  "Major mode for editing LDAP schema.

Attributetype and objectclass definitions are treated as defuns.
The mode decides whether or not to treat the buffer as OpenLDAP
schema according to whether it uses the `attributetype' or
`objectclass' keywords to label the definitions.  (OpenLDAP
schema definitions are assumed to be collected into separate
files for inclusion into slapd.conf.)

Imenu, outline and skeleton support are provided.  See also
command `ldap-convert-asn1-schema' `ldap-convert-ldif-schema'.

\\{ldap-mode-map}"
  (setq ldap-is-openldap (ldap-openldap-p))
  (set (make-local-variable 'font-lock-defaults)
       '(ldap-font-lock-keywords
	 nil nil nil nil
	 (font-lock-syntactic-keywords . ldap-syntactic-keywords)
	 ;; Don't highlight strings.
	 (font-lock-syntactic-face-function
	  . (lambda (state)
	      (if (nth 4 state) font-lock-comment-face)))))
  (set (make-local-variable 'comment-start) "# ") ; Only valid at BOL.
  (when ldap-is-openldap
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
    (setq defun-prompt-regexp
	  (rx line-start
	      (or "attributetype" "objectclass" "attributeType" "objectClass")
	      (+ blank))))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'imenu-create-index-function) #'ldap-create-index)
  (set (make-local-variable 'outline-regexp) ldap-defun-regexp)
  (set (make-local-variable 'outline-level) (lambda () 1)))

;;;; Schema conversion

(defun ldap-convert-asn1-schema (&optional start end)
  "Convert schema from ASN.1, as used in RFCs, to one suitable for OpenLDAP.

Operates on the contents of the current buffer.
In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Basically replaces lines like

  ( <oid> ...

with

attributetype ( <oid> ...

or

objectclass ( <oid> ...

depending on whether they describe objects or attributes.  Regions of
the text outside object or attribute definitions are converted to
comments.

If <oid> is of the form <name>.<number>, it is converted to
<name>:<number>.  The former is apparently an old usage and
IPlanet-ism eliminated in RFC 2252.  The latter is an OpenLDAP OID
macro use which requires the macro to be defined using
`objectidentifier'."
  (interactive (list (if (and transient-mark-mode mark-active)
			 (region-beginning))
		     (if (and transient-mark-mode mark-active)
			 (region-end))))
  (let ((start (or start (point)))
	(end (or end (point-max)))
	(comment-start-pos start)
	(bol))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((case-fold-search nil))
	(goto-char start)
	(while (re-search-forward	; find start of data
		(rx line-start (* blank) "(" (1+ blank)
		    (? (and (group (and letter (* alphanumeric))) "."))
		    (group (1+ (any "0-9."))))
		nil t)
	  
	  (beginning-of-line)
	  (setq bol (point-marker))
	  ;; Comment-out commentary.
	  (if (> (point) comment-start-pos)
	      (save-excursion
		(save-match-data
		  (comment-region comment-start-pos (point)))))
	  ;; Operate on the data.
	  (let ((obj (ldap-object-def-p)))
	    (save-match-data
	      (replace-match (if obj "objectClass" "attributeType") t))
	    (insert " ( ")
	    (if (match-beginning 1)
		(insert (match-string 1) ":"))
	    (insert (match-string 2)))
	  (goto-char bol)
	  (forward-sexp 2)
	  (skip-chars-forward " \t\n")
	  (setq comment-start-pos (point)))
	(skip-chars-forward " \t\n")
	(unless (eobp)
	  (comment-region (point) (point-max))))
      (ldap-openldap-p)))))

(defun ldap-convert-ldif-schema (&optional start end)
  "Convert schema from LDIF, as used by Netscape, to one suitable for OpenLDAP.

Operates on the contents of the current buffer.
In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operate from point to the end of the buffer.

Basically replaces lines like

attributeTypes: ( ...
objectClasses: ( ...

with

attributetype ( <oid> ...

or

objectclass ( <oid> ...

If <oid> is of the form <name>.<number>, it is converted to
<name>:<number>.  The former is apparently an old usage and
IPlanet-ism eliminated in RFC 2252.  The latter is an OpenLDAP OID
macro use which requires the macro to be defined using
`objectidentifier'.

You likely want to use \\[ldap-mode] on the buffer after this command."
  (interactive (list (if (and transient-mark-mode mark-active)
			 (region-beginning))
		     (if (and transient-mark-mode mark-active)
			 (region-end))))
  (let ((start (or start (point)))
	(end (or end (point-max)))
	(comment-start-pos start)
	(bol))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((case-fold-search nil))
	(goto-char start)
	(flush-lines "^dn: *cn=schema")
	(while (re-search-forward (rx line-start (group "attributeTypes:")
				      (* blank) "(" (* blank)
				      (? (group (and letter (* alphanumeric))
						(group "."))))
				  nil t)
	  (save-match-data
	    (replace-match "attributetype " t t nil 1))
	  (if (match-beginning 2)
	      (replace-match ":" t t nil 3)))
	(goto-char (point-min))
	(while (re-search-forward (rx line-start (group "objectClasses:")
				      (* blank) "(" (* blank)
				      (? (group (and letter (* alphanumeric))
						(group "."))))
				  nil t)
	  (save-match-data
	    (replace-match "objectclass " t t nil 1))
	  (if (match-beginning 2)
	      (replace-match ":" t t nil 3)))


	(while (re-search-forward	; find start of data
		(rx line-start (* blank) "(" (1+ blank)
		    (? (and (group (and letter (* alphanumeric))) "."))
		    (group (1+ (any "0-9."))))
		nil t)))
      (ldap-openldap-p)))))

(defun ldap-object-def-p ()
  "Return non-nil if current definition is of an object, not an attribute.
Assumes point is before the opening paren (and possible intervening
whitespace)."
  (save-match-data
    ;; Look for keyword indicating an object definition.  Should
    ;; really move via sexps in case a value might match.
    (re-search-forward
     (rx word-start
	 (or "STRUCTURAL" "ABSTRACT" "AUXILIARY" "MUST" "MAY")
	 word-end)
     ;; forward-sexp could only fail with invalid data, I think.
     (save-excursion (forward-sexp) (point)) t)))

;;;; LDIF

(defconst ldif-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)	; actually only comment at BOL
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "." table)
    table))

(eval-and-compile
  (defconst ldif-continued-line-regexp
    '(group (1+ not-newline) (* (and "\n" (1+ blank) (1+ not-newline))))))

(if (default-boundp 'font-lock-extra-managed-props)
    ;; This only works in Emacs 22, where we have
    ;; `font-lock-extra-managed-props', though it could probably be
    ;; made to work in 21.3, at least after a fashion.
(progn
(defgroup ldap-mode ()
  "Editing LDAP schema and LDIF"
  ;; Fixme: add to ldap group too?
  :group 'data)

(defface ldif-decoded
  '((t :inherit underline))
  "Face indicating decoded base64 data.
This is merged with the normal font-lock face for the attribute data."
  :group 'faces
  :group 'ldap-mode)

(defcustom ldif-font-lock-decode t
  "Non-nil means that Font Lock displays base64 data decoded.
This applies to all DN attributes.  It may also apply to other
attributes -- see `ldif-font-lock-decode-all'.

This feature only works in Emacs 22 and newer."
  :type 'boolean
  :group 'ldap-mode)

(defcustom ldif-font-lock-decode-all nil
  "Non-nil means Font Lock displays base64 data decoded for all attributes.
This only applies if `ldif-font-lock-decode' is non-nil.  This
decoding depends on Emacs being able to decode the data as UTF-8.
It may be wrong if the data are actually binary, not text.
Turning this on may be quite expensive."
  :type 'boolean
  :group 'ldap-mode))

;; else (Emacs 21) just bind variables used below
(defconst ldif-font-lock-decode nil)
(defconst ldif-font-lock-decode-all nil))

(defcustom ldif-attribute-face nil
  "Face with which to highlight LDIF object attributes other than the DN.
The default is not to highlight."
  :group 'ldap-mode
  :type '(choice face (const :tag "No highlight" nil)))

(defconst ldif-font-lock-keywords
  `((,(rx line-start (group "dn") "::" (* blank)
	  (eval ldif-continued-line-regexp))
     (1 'font-lock-keyword-face)
     (2 (if ldif-font-lock-decode
	    ;; Decode the base64-encoded utf-8 as a display string.
	    (list 'face '(:inherit (font-lock-variable-name-face
				    ldif-decoded))
		  'help-echo "base64-encoded"
		  'display (decode-coding-string (base64-decode-string
						  (match-string 2))
						 'utf-8))
	  'font-lock-variable-name-face)))
    ;; Keep this below the `dn::' case.
    (,(rx line-start (group (or "dn" "add" "delete" "replace")) ":"
	  (* blank) (eval ldif-continued-line-regexp))
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    (,(rx line-start (group "changetype") ":" (* blank)
	  (eval ldif-continued-line-regexp))
     (1 'font-lock-keyword-face) (2 'font-lock-constant-face))
    (,(rx line-start (group "-") (* blank) line-end)
     (1 'font-lock-warning-face))  ; make hyphen as strong as possible
    (,(rx line-start (group "version") ":" (* blank) (group (1+ word)))
     (1 'font-lock-keyword-face))
    ;; I'm not sure it's useful to highlight all the attribute names.
    ;; Let's try to display decoded base64, though.  We don't know
    ;; whether an attribute is textual, i.e. whether it should be
    ;; decoded, though we could keep a list of attributes known to
    ;; have text syntax (e.g. `ldap-attribute-syntaxes-alist').
    ;; Instead, just test whether the data are valid UTF-8.
    (,(rx line-start (1+ (not (any ?:))) "::" (* blank)
	  (eval ldif-continued-line-regexp))
	  (1 (or (and ldif-font-lock-decode ldif-font-lock-decode-all
		       (let* ((data (base64-decode-string (match-string 1)))
			      (coding-category-utf-8 'utf-8)
			      (coding-category-list '(coding-category-utf-8))
			      (coding (detect-coding-string data))
			      utf-8)
			 (dolist (c coding)
			   (if (memq c '(utf-8 utf-8-unix utf-8-dos utf-8-mac
					 undecided undecided-unix
					 undecided-dos undecided-mac))
			       (setq utf-8 t)))
			 (if utf-8
			     (list 'face 'ldif-decoded
				   'display (decode-coding-string data 'utf-8)
				   'help-echo "base64-encoded"))))
		  nil)))
    (,(rx line-start (group (not (any ?\ )) (1+ (not (any ?:)))) ":")
     (1 ldif-attribute-face))))

;; Fixme: for `dn::', decode the name from base64/utf-8
(defconst ldif-generic-expression
  `((nil ,(rx line-start "dn:" (? ":") (* blank)
	      (eval ldif-continued-line-regexp))
	 1))
  "`imenu-generic-expression' for LDIF.")

;;;###autoload
(define-derived-mode ldif-mode fundamental-mode "LDIF"
  "Major mode for editing LDAP LDIF files.
Imenu and outline support is provided.  There are no special
keybindings.  Font-Lock support includes decoding some
base64-encoded attributes -- see `ldif-decoded'."
  (set (make-local-variable 'font-lock-defaults)
       `(ldif-font-lock-keywords
	 nil nil nil nil
 	 (font-lock-syntactic-keywords . ldap-syntactic-keywords)
	 (font-lock-extra-managed-props . (display help-echo))
	 (font-lock-multiline . t)))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'comment-start) "# ") ; Only valid at BOL.
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'imenu-generic-expression)
       ldif-generic-expression)
  (set (make-local-variable 'parse-sexp-lookup-properties) t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ldif\\'" . ldif-mode) t)
;;;###autoload
(modify-coding-system-alist 'file "\\.ldif\\'" 'utf-8)


(defun ldif-decode-base64 ()
  "Decode base64-encoded UTF-8 data for attribute at point.
Also change the double colon after the attribute name to a single one.
An error is signalled if the attribute data aren't base64-encoded UTF-8.
Note that this loses the significance of encoded newlines and leading
whitespace."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp)) (eq ?\  (char-after)))
      (forward-line -1))
    (if (looking-at (rx line-start (1+ word) (group "::" (* blank))
			(eval ldif-continued-line-regexp)))
	(let* ((data (base64-decode-string (match-string 2)))
	       (coding-category-utf-8 'utf-8)
	       (coding-category-list '(coding-category-utf-8))
	       (coding (detect-coding-string data))
	       utf-8)
	  (dolist (c coding)
	    (if (memq c '(utf-8 utf-8-unix utf-8-dos utf-8-mac
			  undecided undecided-unix undecided-dos
			  undecided-mac))
		(setq utf-8 t)))
	  (unless utf-8
	    (error "Not UTF-8 data"))
	  (save-match-data
	    (save-restriction
	      (narrow-to-region (match-beginning 2) (match-end 2))
	      (replace-match (decode-coding-string
			      (base64-decode-string (match-string 2))
			      'utf-8)
			     t t nil 2)
	      (goto-char (point-min))
	      (while (search-forward "\n" nil t)
		(replace-match ""))))
	  (replace-match ": " t t nil 1))
      (error "Not at base64-encoded attribute"))))

(defun ldif-base64-encode ()
  "Convert the attribute data to base64-encoded UTF-8 if possible.
Line continuations are removed.  See also `ldif-base64-encode-region'."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp)) (eq ?\  (char-after)))
      (forward-line -1))
    (unless (looking-at (rx line-start (1+ word) ":" (* blank)
			    (eval ldif-continued-line-regexp)))
      (error "Not at attribute"))
    (save-restriction
      (narrow-to-region (match-beginning 1) (match-end 1))
      (let ((coding (find-coding-systems-region (point-min) (point-max))))
	(unless (or (memq 'undecided coding)
		    (memq 'utf-8 coding)
		    (memq 'mule-utf-8 coding))
	  (error "Can't encode the data as UTF-8")))
      (goto-char (point-min))
      ;; Remove line continuations.
      (while (re-search-forward "\n[ \t]" nil t)
	(replace-match ""))
      (encode-coding-region (point-min) (point-max) 'utf-8)
      (base64-encode-region (point-min) (point-max))
      (goto-char (point-min))
      (while (zerop (forward-line))
	(insert ?\ ))
      (goto-char (point-min))
      (insert ?:))))

(defun ldif-base64-encode-region (beg end)
  "Encode the region as base64-encoded UTF-8.
This doesn't remove LDIF line continuations in the region,
c.f. `ldif-base64-encode'.  It does insert a leading space on
each line of the result so that it may be used as attribute data.
See also `ldif-base64-encode'."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((coding (find-coding-systems-region (point-min) (point-max))))
	(cond ((memq 'undecided coding))
	      ((or (memq 'utf-8 coding)
		   (memq 'mule-utf-8 coding))
	       (encode-coding-region (point-min) (point-max) 'utf-8))
	      (t (error "Can't encode the data as UTF-8"))))
      (base64-encode-region (point-min) (point-max))
      (goto-char (point-min))
      (insert ?\ )
      (while (= 0 (forward-line))
	(insert ?\ )))))

(provide 'ldap-mode)
;;; ldap-mode.el ends here
