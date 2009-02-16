;;;; keymap-hacks.el -- some keymap-hacking code
;;; Time-stamp: <2006-01-17 16:46:02 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'keymap-hacks)

(defvar map-key-defs-in-progress nil
 "Used internally by map-key-definitions.")

;; based on map-key-definitions in subr.el in the GNU Emacs 21.3.1 distribution

(defun map-key-definitions (function keymap &optional oldmap prefix)
  "Apply FUNCTION to all bindings in KEYMAP."

  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning
  
  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (vec1 (vector nil))
	 (prefix1 (vconcat prefix vec1))
	 (map-key-defs-in-progress
	  (cons scan map-key-defs-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (while (consp scan)
      (if (consp (car scan))
	  (let ((char (car (car scan)))
		(defn (cdr (car scan))))
	    ;; The inside of this let duplicates exactly
	    ;; the inside of the following let that handles array elements.
	    (aset vec1 0 char)
	    (aset prefix1 (length prefix) char)
	    (let (inner-def skipped)
	      ;; Skip past menu-prompt.
	      (while (stringp (car-safe defn))
		(setq skipped (cons (car defn) skipped))
		(setq defn (cdr defn)))
	      ;; Skip past cached key-equivalence data for menu items.
	      (and (consp defn) (consp (car defn))
		   (setq defn (cdr defn)))
	      (setq inner-def defn)
	      ;; Look past a symbol that names a keymap.
	      (while (and (symbolp inner-def)
			  (fboundp inner-def))
		(setq inner-def (symbol-function inner-def)))
	      (funcall function prefix1 defn)
	      (if (and (keymapp defn)
			 ;; Avoid recursively scanning
			 ;; where KEYMAP does not have a submap.
			 (let ((elt (lookup-key keymap prefix1)))
			   (or (null elt)
			       (keymapp elt)))
			 ;; Avoid recursively rescanning keymap being scanned.
			 (not (memq inner-def
				    map-key-defs-in-progress)))
		    ;; If this one isn't being scanned already,
		    ;; scan it now.
		    (map-key-definitions function keymap
					 inner-def
					 prefix1))))
	(if (vectorp (car scan))
	    (let* ((array (car scan))
		   (len (length array))
		   (i 0))
	      (while (< i len)
		(let ((char i) (defn (aref array i)))
		  ;; The inside of this let duplicates exactly
		  ;; the inside of the previous let.
		  (aset vec1 0 char)
		  (aset prefix1 (length prefix) char)
		  (let (inner-def skipped)
		    ;; Skip past menu-prompt.
		    (while (stringp (car-safe defn))
		      (setq skipped (cons (car defn) skipped))
		      (setq defn (cdr defn)))
		    (and (consp defn) (consp (car defn))
			 (setq defn (cdr defn)))
		    (setq inner-def defn)
		    (while (and (symbolp inner-def)
				(fboundp inner-def))
		      (setq inner-def (symbol-function inner-def)))
		    (funcall function prefix1 defn) 
		    (if (and (keymapp defn)
			       (let ((elt (lookup-key keymap prefix1)))
				 (or (null elt)
				     (keymapp elt)))
			       (not (memq inner-def
					  map-key-defs-in-progress)))
			  (map-key-definitions function keymap
					       inner-def
					       prefix1))))
		(setq i (1+ i))))
	  (if (char-table-p (car scan))
	      (map-char-table
	       (function (lambda (char defn)
			   (let ()
			     ;; The inside of this let duplicates exactly
			     ;; the inside of the previous let,
			     ;; except that it uses set-char-table-range
			     ;; instead of define-key.
			     (aset vec1 0 char)
			     (aset prefix1 (length prefix) char)
			     (let (inner-def skipped)
			       ;; Skip past menu-prompt.
			       (while (stringp (car-safe defn))
				 (setq skipped (cons (car defn) skipped))
				 (setq defn (cdr defn)))
			       (and (consp defn) (consp (car defn))
				    (setq defn (cdr defn)))
			       (setq inner-def defn)
			       (while (and (symbolp inner-def)
					   (fboundp inner-def))
				 (setq inner-def (symbol-function inner-def)))
			       (funcall function prefix1 defn) 
			       (if (and (keymapp defn)
					  (let ((elt (lookup-key keymap prefix1)))
					    (or (null elt)
						(keymapp elt)))
					  (not (memq inner-def
						     map-key-defs-in-progress)))
				     (map-key-definitions function keymap
							  inner-def
							  prefix1))))))
	       (car scan)))))
      (setq scan (cdr scan)))))

(defun test-map-key-definitions ()
  (interactive)
  (with-output-to-temp-buffer "*Key bindings*"
    (map-key-definitions
     (lambda (a b)
       (if (symbolp b)
	   (princ (format "%S: %S\n"
			  (if (vectorp a)
			      (key-description a)
			    a)
			  b))))
     global-map)))

(defun keymap-overwrite-entries (target-map source-map)
  "To TARGET-MAP, add all the definitions in SOURCE-MAP, which must be sparse.
Really meant to take the output of map-key-definitions for SOURCE-MAP rather than
a real keymap, but will probably work on simple real keymaps too."
  (while source-map
    (let ((binding (car source-map)))
      (when (consp binding)
	(define-key target-map (car binding) (cdr binding))))
    (setq source-map (cdr source-map))))

;;; end of keymap-hacks.el
