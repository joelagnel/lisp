<!-- X-URL: http://mail.gnu.org/pipermail/gnu-emacs-sources/1999-December/000131.html -->
<!-- Date: Sun, 28 Jan 2001 17:19:03 GMT -->
<BASE HREF="http://mail.gnu.org/pipermail/gnu-emacs-sources/1999-December/000131.html">

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML>
 <HEAD>
   <TITLE> lisp-re - Regular expressions in Lisp notation
   </TITLE>
   <LINK REL="Index" HREF="index.html" >
   <LINK REL="made" HREF="mailto:detlev.zundel%40stud.uni-karlsruhe.de">
   <META NAME="robots" CONTENT="index,nofollow">
   
   <LINK REL="Previous"  HREF="000132.html">
   
 </HEAD>
 <BODY BGCOLOR="#ffffff">
   <H1>lisp-re - Regular expressions in Lisp notation
   </H1>
    <B>Detlev Zundel
    </B> 
    <A HREF="mailto:detlev.zundel%40stud.uni-karlsruhe.de"
       TITLE="lisp-re - Regular expressions in Lisp notation">detlev.zundel@stud.uni-karlsruhe.de
       </A><BR>
    <I>31 Dec 1999 20:04:36 +0100</I>
    <P><UL>
        <LI> Previous message: <A HREF="000132.html">re-builder - Build regular expressions interactively
</A></li>
        
         <LI> <B>Messages sorted by:</B> 
              <a href="date.html#131">[ date ]</a>
              <a href="thread.html#131">[ thread ]</a>
              <a href="subject.html#131">[ subject ]</a>
              <a href="author.html#131">[ author ]</a>
         </LI>
       </UL>
    <HR>  
<!--beginarticle-->
<PRE>Ok, as the previously posted `re-builder' can make use of this I'll
better also post it - have fun!

Cheers
  Detlev

-- 
LISP has survived for 21 years because it is an approximate local
optimum in the space of programming languages.
                                           -- John McCarthy (1980)


;;; lisp-re.el --- Transform REs written in a Lisp like notation to strings

;; Copyright (C) 1999 by Detlev Zundel

;; Author: Detlev Zundel &lt;<A HREF="mailto:Detlev.Zundel@stud.uni-karlsruhe.de">Detlev.Zundel@stud.uni-karlsruhe.de</A>&gt;
;; Keywords: matching, lisp

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; $Id: lisp-re.el,v 1.1 1999/12/31 18:54:54 dzu Exp $

;; The `flat string representation' for a regular expression can
;; sometimes be complicated when a notation allowing for clearer
;; grouping and more easily remembered operators could be
;; `structurally clearer'.

;; For a Lisp programmer clearly such a notation is the Lisp list
;; syntax.  The functions below allow to toy with such a notation for
;; regular expressions which was inspired by the REs in the `RScheme'
;; system.  They are basically a front end to the string
;; representation used in ELisp.  (Although one could conceive
;; different backends for other (slightly) different syntaxes, as
;; for example the Perl RE syntax)

;; REs can be specified with the following `function' symbols:
;;
;; (seq &lt;e1&gt; ...)     - Sequence of expressions
;; (or  &lt;e1&gt; ...)     - Alternatives
;; (+ &lt;e&gt;)            - One or more occurrences of &lt;e&gt;
;; (\? &lt;e&gt;)           - Zero or one occurrences of &lt;e&gt;
;; (* &lt;e&gt;)            - Zero or more occurrences of &lt;e&gt;
;; (save &lt;e&gt;)         - Save occurrence of &lt;e&gt; for reference and returning
;; (ref &lt;num&gt;)        - Occurrence of &lt;num&gt;th `save'd expression
;; (syntax &lt;s&gt;)       - Character of syntax &lt;s&gt;
;; (not-syntax &lt;s&gt;)   - Character of syntax other than &lt;s&gt;
;; (range &lt;f&gt; &lt;t&gt;)    - Range between &lt;f&gt; and &lt;t&gt;
;; (not &lt;r&gt;)          - Complement of range &lt;r&gt;
;; (cclass &lt;c1&gt; ..)   - Character class allowing &lt;c1&gt; ... (either chars
;;                     or ranges)
;;
;; If you like more verbose constructs one-or-more, zero-or-one and
;; zero-or-more can be used for the postfix operators.

;;; The following symbols can be used as expressions: (zw = zero-width)

;; any           - Any character
;; eol           - End of line (zw)
;; bol           - Beginning of line (zw)
;; bob           - Beginning of buffer (zw)
;; eob           - End of Buffer (zw)
;; wordlimit     - Beginning or end of a word (zw)
;; not-wordlimit - Not the beginning or end of a word (zw)
;; bow           - Beginning of word (zw)
;; eow           - End of word (zw)
;; wordchar      - Word character
;; non-wordchar  - Non-word character
;; point         - Position of point (zw)
;; cr            - Carriage return

;;; Also the following predefined character classes are available:
;; digit, alpha, uppercase, lowercase, hexdigit, printable and  space

;; As the RE is a normal list you can insert comments just like in any
;; other Lisp code - without the need to `extend' the syntax.

;; This notation introduces a problem with the sub-expressions in REs.
;; Because a `(one-or-more &lt;string&gt;)' construct needs to group the
;; &lt;string&gt; (if it is longer than one character) to be sensible, there
;; might be `unwanted' subexpressions, i.e. groups not resulting from
;; `save' constructs, in the string representation.  In other words
;; the mapping of the lispy REs to string REs is not injective.  (In
;; Perl syntax it could be because Perl has got the `(?' to group
;; without allowing references to the group)

;; To cope with this the `lre-compile-stringmatcher' and
;; `lre-compile-buffermatcher' functions return lambda expressions
;; that handle the actual matching and return a list of the whole
;; matched string and the `save'd parts respectively.  This insulating
;; layer makes the mapping lispy RE to lambdas injective.

;; If you do not want to use them you can still call
;; `lre-compile-string' and evaluate the value of `lre-match-list'
;; afterwards.  It is a list of the numbers of the subexpressions
;; corresponding to the `save's.

;; Examples:

;; The notorious (hopelessly inefficient but nevertheless elegant)
;; primality test via regexps can thus be written as follows:

; (defun is-prime (n)
;   (not (funcall
; 	(lre-compile-stringmatcher
; 	 '(seq bol
; 	       (save any (one-or-more any))
; 	       (one-or-more (ref 1))
; 	       eol))
; 	(concat (make-vector n ?1)))))

;;; Code:


(defvar lre-symbols
  '((any .&quot;.&quot;)
    (eol . &quot;$&quot;)
    (bol . &quot;^&quot;)
    (bob . &quot;\\`&quot;)
    (eob . &quot;\\'&quot;)
    (wordlimit . &quot;\\b&quot;)
    (not-wordlimit . &quot;\\B&quot;)
    (bow . &quot;\\&lt;&quot;)
    (eow . &quot;\\&gt;&quot;)
    (wordchar . &quot;\\w&quot;)
    (non-wordchar . &quot;\\W&quot;)
    (point . &quot;\\=&quot;)
    (cr . &quot;\\n&quot;))
  &quot;Symbols known by the regexp compiler.&quot;)

(defvar lre-classes
  '((digit . &quot;[0-9]&quot;)
    (alpha . &quot;[a-zA-Z]&quot;)
    (uppercase . &quot;[A-Z]&quot;)
    (lowercase . &quot;[a-z]&quot;)
    (hexdigit . &quot;[0-9a-fA-F]&quot;)
    (printable . &quot;[ -~]&quot;)
    (space . &quot;[ \t]&quot;))
  &quot;Character classes known by the regexp compiler.&quot;)

(defvar lre-match-list nil
  &quot;Internal list for `lre-compile'.&quot;)
(defvar lre-match-pos 0
  &quot;Internal counter for `lre-compile'.&quot;)

(defun lre-compile-stringmatcher (exp)
  &quot;Compile an regular expression in lisp-like format to a lambda form.
The resulting lambda form takes one string argument to match with the
regular expression.  Return nil if no match was found and a list with
n+1 entries where n is the number of `save' expressions in EXP.
The list contains the whole match and the `save'd parts of it.&quot;

  (setq lre-match-list nil
	lre-match-pos 0)
  (let ((re (cdr (lre-compile-internal exp))))
    `(lambda (string)
       (if (string-match ,re string)
	   (list (match-string 0 string)
		 ,@(mapcar (lambda (num) (list 'match-string num 'string))
			   lre-match-list))))))

(defun lre-compile-buffermatcher (exp)
  &quot;Compile an regular expression in lisp-like format to a lambda form.
The resulting lambda form takes no arguments and searches in the
current buffer from point onwards.  Return nil if no match was found
and a list with n+1 entries where n is the number of `save'
expressions in EXP.  The list contains the whole match and the `save'd
parts of it.&quot;

  (setq lre-match-list nil
	lre-match-pos 0)
  (let ((re (cdr (lre-compile-internal exp))))
    `(lambda ()
       (if (re-search-forward ,re (point-max) t)
	   (list (match-string 0)
		 ,@(mapcar (lambda (num) (list 'match-string num))
			   lre-match-list))))))

(defun lre-compile-string (exp)
  &quot;Compile the regular expression EXP in lisp-like format to a string.&quot;

  (setq lre-match-list nil
	lre-match-pos 0)
  (lre-compile-internal-string exp))

(defmacro lre-compile-macro (exp)
  &quot;Macro version of `lre-compile-string'.&quot;

  (lre-compile-string exp))

;; Below `primary' in contrast to `compound' parts of an RE need no
;; further grouping for the postfix operators.

(defun lre-prim (exp)
  &quot;Constructor for a primary expression EXP in lisp-re.&quot;
  (cons 'primitive exp))

(defun lre-prim-p (exp)
  &quot;Predicate testing whether EXP is a primary expression in lisp-re.&quot;
  (eq (car exp) 'compound))

(defun lre-comp (exp)
  &quot;Constructor for a compound expression EXP in lisp-re.&quot;
  (cons 'compound exp))

(defun lre-comp-p (exp)
  &quot;Predicate testing whether EXP is a compund expression in lisp-re.&quot;
  (eq (car exp) 'compound))

(defun lre-comp-group-if-nec (exp)
  &quot;Compile EXP and group if necessary.  Also adjust `saved' positions.&quot;

  (let* ((oldmatch-pos lre-match-pos)
	 (parsedarg (lre-compile-internal exp)))
    (if (lre-comp-p parsedarg)
	(progn
	  (lre-advance-matches oldmatch-pos)
	  (setq lre-match-pos (1+ lre-match-pos))
	  (concat &quot;\\(&quot; (cdr parsedarg) &quot;\\)&quot;))
      (cdr parsedarg))))

(defun lre-advance-matches (from)
  &quot;Shift current match positions starting at FROM.&quot;

  (let ((list (nthcdr from lre-match-list)))
    (while list
      (setcar list (1+ (car list)))
      (setq   list (cdr list)))))

(defun lre-char-or-class (exp)
  &quot;If EXP is a symbol return character class.  Return string otherwise.&quot;

  (cond
   ((symbolp exp)
    (let ((class (cdr (assoc exp lre-classes))))
      (if class
	  (substring class 1 (1- (length class)))
	(error &quot;Invalid character class: `%s'&quot; exp))))
   ((listp exp)
    (let ((parsedarg (lre-compile-string exp)))
      (if (eq (aref parsedarg 0) (aref &quot;[&quot; 0))
	  (substring parsedarg 1 (1- (length parsedarg)))
	(error &quot;Invalid character class: `%s'&quot; exp))))
   (t (char-to-string exp))))

(defun lre-compile-internal-string (exp)
  &quot;Compile a regular expression EXP in lisp-like format to a string.&quot;

  (cdr (lre-compile-internal exp)))

(defun lre-compile-internal (exp)
  &quot;Internal helper for `lre-compile' compiling EXP.&quot;

  (cond ((null exp)
	 (lre-prim &quot;&quot;))
	((symbolp exp)
	 (lre-prim (or (cdr (assoc exp lre-symbols))
		       (cdr (assoc exp lre-classes))
		       (error &quot;Invalid symbol: `%s'&quot; exp))))
	((integerp exp)
	 (lre-prim (char-to-string exp)))
	((stringp exp)
	 (if (&gt; (length exp) 1)
	     (lre-comp exp)
	   (lre-prim exp)))
	((listp exp)
	 (let ((op (car exp))
	       (arg (cadr exp)))
	   (cond
	    ((eq op 'seq)
	     (lre-comp
	      (mapconcat 'lre-compile-internal-string (cdr exp) &quot;&quot;)))
	    ((eq op 'or)
	     (lre-comp (mapconcat
			'lre-compile-internal-string (cdr exp) &quot;\\|&quot;)))
	    ((eq op 'save)
	     (setq lre-match-pos (1+ lre-match-pos)
		   lre-match-list (append lre-match-list
					  (list lre-match-pos)))
	     (lre-prim
	      (concat &quot;\\(&quot;
		      (lre-compile-internal-string (cons 'seq (cdr exp)))
		      &quot;\\)&quot;)))
	    ((eq op 'ref)
	     (lre-prim
	      (concat &quot;\\&quot; (or (nth (1- arg) lre-match-list)
			       (error &quot;Invalid backward reference to `%d'&quot;
				      arg)))))
	    ((memq op '(+ one-or-more))
	     (lre-comp (concat (lre-comp-group-if-nec arg) &quot;+&quot;)))
	    ((memq op '(\? zero-or-one))
	     (lre-comp (concat (lre-comp-group-if-nec arg) &quot;?&quot;)))
	    ((memq op '(* zero-or-more))
	     (lre-comp (concat (lre-comp-group-if-nec arg) &quot;*&quot;)))
	    ((eq op 'syntax)
	     (lre-prim (concat &quot;\\s&quot; arg)))
	    ((eq op 'not-syntax)
	     (lre-prim (concat &quot;\\S&quot; arg)))
	    ((eq op 'range)
	     (let ((from arg)
		   (to (caddr exp)))
	       (lre-prim
		(concat &quot;[&quot;
			(if (stringp from) from (char-to-string from))
			&quot;-&quot;
			(if (stringp to) to (char-to-string to))
			&quot;]&quot;))))
	    ((eq op 'cclass)
	     (lre-prim (concat &quot;[&quot;
			      (mapconcat 'lre-char-or-class (cdr exp) &quot;&quot;)
			      &quot;]&quot;)))
	    ((eq op 'not)
	     (let ((class (lre-compile-internal-string arg)))
	       (if (eq (aref class 0) (aref &quot;[&quot; 0))
		 (lre-prim (concat &quot;[^&quot; (substring class 1)))
		 (error &quot;Invalid character class: `%s'&quot; arg))))
	    (t (error &quot;Invalid regexp-function: `%s'&quot; op)))))))

;;; lisp-re.el ends here

</PRE>

<!--endarticle-->
    <HR>
    <P><UL>
        <!--threads-->
	<LI> Previous message: <A HREF="000132.html">re-builder - Build regular expressions interactively
</A></li>
	
         <LI> <B>Messages sorted by:</B> 
              <a href="date.html#131">[ date ]</a>
              <a href="thread.html#131">[ thread ]</a>
              <a href="subject.html#131">[ subject ]</a>
              <a href="author.html#131">[ author ]</a>
         </LI>
       </UL>
</body></html>
