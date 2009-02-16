;;; compile+.el --- Extensions to `compile.el'.
;;
;; Filename: compile+.el
;; Description: Extensions to `compile.el'.
;; Author: Drew ADAMS
;; Maintainer: Drew ADAMS
;; Copyright (C) 2004-2006, Drew Adams, all rights reserved.
;; Created: Tue Nov 16 16:38:23 2004
;; Version: 21.0
;; Last-Updated: Thu Jul 27 11:29:41 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 752
;; URL: http://www.emacswiki.org/cgi-bin/wiki/compile+.el
;; Keywords: tools, processes
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `compile', `compile-', `fit-frame', `frame-cmds',
;;   `frame-fns', `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `compile.el'.
;;
;;  See also the companion file `compile-.el'.
;;        `compile-.el' should be loaded before `compile.el'.
;;        `compile+.el' should be loaded after `compile.el'.
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'compile+)
;;
;;  Additional keys are bound here.  Some bindings that would normally
;;  try to modify a compilation mode buffer are unbound, so they are
;;  available for local (Compilation Mode) definition.
;;
;;
;;  ***** NOTE: The following variable defined in `compile.el'
;;              has been REDEFINED HERE:
;;
;;  `compilation-error-regexp-alist-alist' -
;;     Regexp matches whole line, so mouse-over it.
;;
;;
;;  ***** NOTE: The following macro defined in `compile.el'
;;              has been REDEFINED HERE:
;;
;;  `compilation-assq'.
;;
;;
;;  ***** NOTE: The following functions defined in `compile.el'
;;              have been REDEFINED HERE:
;;
;;  `compilation-compat-error-properties',
;;  `compilation-directory-properties',
;;  `compilation-internal-error-properties'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005/12/16 dadams
;;     Updated to use compilation-mouseover (in compile-.el).
;;       Added: Redefinitions of compilation-error-regexp-alist-alist,
;;              compilation-assq, compilation-compat-error-properties,
;;              compilation-directory-properties
;;              compilation-internal-error-properties.
;;     Added compile-mode-summary and key bindings.
;;     Removed redefinitions of compilation-goto-locus and overlay.
;;       No longer require strings.el.
;; 2004/11/16 dadams
;;     New version for Emacs 21. Old version renamed to compile+20.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'compile-) ;; compilation-mouseover
(require 'compile) ;; compilation-error-regexp-alist-alist, compilation-minor-mode-map,

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key compilation-minor-mode-map "?" 'describe-mode) ; Defined in `help.el'.
(define-key compilation-minor-mode-map "a" 'first-error)
(define-key compilation-minor-mode-map "b" 'compile-mode-summary)
(define-key compilation-minor-mode-map "c" 'compile)
(define-key compilation-minor-mode-map "d" 'compile-mode-summary)
(define-key compilation-minor-mode-map "e" 'compile-mode-summary)
(define-key compilation-minor-mode-map "f" 'compile-goto-error)
(define-key compilation-minor-mode-map "g" 'recompile)
(define-key compilation-minor-mode-map "h" 'describe-mode) ; Defined in `help.el'.
(define-key compilation-minor-mode-map "i" 'compile-mode-summary)
(define-key compilation-minor-mode-map "j" 'compile-mode-summary)
(define-key compilation-minor-mode-map "k" 'kill-compilation)
(define-key compilation-minor-mode-map "l" 'compile-mode-summary)
(define-key compilation-minor-mode-map "m" 'compile)       ; Make.
(define-key compilation-minor-mode-map "n" 'compilation-next-error)
(define-key compilation-minor-mode-map "o" 'compile-mode-summary)
(define-key compilation-minor-mode-map "p" 'compilation-previous-error)
(define-key compilation-minor-mode-map "q" 'quit-window)
(define-key compilation-minor-mode-map "r" 'recompile)
(define-key compilation-minor-mode-map "s" 'compile-mode-summary)
(define-key compilation-minor-mode-map "t" 'compile-mode-summary)
(define-key compilation-minor-mode-map "u" 'compile-mode-summary)
(define-key compilation-minor-mode-map "v" 'compile-mode-summary)
(define-key compilation-minor-mode-map "w" 'compile-mode-summary)
(define-key compilation-minor-mode-map "x" 'compile-mode-summary)
(define-key compilation-minor-mode-map "y" 'compile-mode-summary)
(define-key compilation-minor-mode-map "z" 'compile-mode-summary)
(define-key compilation-minor-mode-map "A" 'first-error)
(define-key compilation-minor-mode-map "B" 'compile-mode-summary)
(define-key compilation-minor-mode-map "C" 'compile)
(define-key compilation-minor-mode-map "D" 'compile-mode-summary)
(define-key compilation-minor-mode-map "E" 'compile-mode-summary)
(define-key compilation-minor-mode-map "F" 'compile-goto-error)
(define-key compilation-minor-mode-map "G" 'recompile)
(define-key compilation-minor-mode-map "H" 'describe-mode) ; Defined in `help.el'.
(define-key compilation-minor-mode-map "I" 'compile-mode-summary)
(define-key compilation-minor-mode-map "J" 'compile-mode-summary)
(define-key compilation-minor-mode-map "K" 'kill-compilation)
(define-key compilation-minor-mode-map "L" 'compile-mode-summary)
(define-key compilation-minor-mode-map "M" 'compile)       ; Make
(define-key compilation-minor-mode-map "N" 'compilation-next-error)
(define-key compilation-minor-mode-map "O" 'compile-mode-summary)
(define-key compilation-minor-mode-map "P" 'compilation-previous-error)
(define-key compilation-minor-mode-map "Q" 'quit-window)
(define-key compilation-minor-mode-map "R" 'recompile)
(define-key compilation-minor-mode-map "S" 'compile-mode-summary)
(define-key compilation-minor-mode-map "T" 'compile-mode-summary)
(define-key compilation-minor-mode-map "U" 'compile-mode-summary)
(define-key compilation-minor-mode-map "V" 'compile-mode-summary)
(define-key compilation-minor-mode-map "W" 'compile-mode-summary)
(define-key compilation-minor-mode-map "X" 'compile-mode-summary)
(define-key compilation-minor-mode-map "Y" 'compile-mode-summary)
(define-key compilation-minor-mode-map "Z" 'compile-mode-summary)
(define-key compilation-minor-mode-map "{" 'compilation-previous-file)
(define-key compilation-minor-mode-map "}" 'compilation-next-file)


;; Use mouseover on whole line.  Same as original in `compile.el', except for this.
(unless (featurep 'compile+)
  (setq compilation-error-regexp-alist-alist
        (mapcar (lambda (elt)`(,(car elt) ,(concat (cadr elt) ".*") ,@(cddr elt)))
                compilation-error-regexp-alist-alist)))

;;; Undefine some bindings that would try to modify a Compilation mode buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Compilation Mode) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands compilation-mode-map (current-global-map)))

;;;###autoload
(defun compile-mode-summary ()
  "Display brief help message for Compile Mode."
  (interactive)
  (message
   (concat
    (substitute-command-keys
     "\\[describe-mode]= help,  \\[compile-goto-error] & \
\\[compile-mouse-goto-error]= this error,  \\[next-error]= next error,  \
\\[kill-compilation]= kill,  \\[grep]= grep,  \\[compile]= compile,  \
\\[recompile]= recompile"))))



;;; ----------------------------------------------------------------
;;; The rest of this file is redefinitions of standard functions in
;;; `compile.el.  The only changes made have been to replace face
;;; `highlight' by face `compilation-mouseover'.  There is no change
;;; at all in macro `compilation-assq'.
;;; ----------------------------------------------------------------


;; Internal function for calculating the text properties of a directory
;; change message.  The directory property is important, because it is
;; the stack of nested enter-messages.  Relative filenames on the following
;; lines are relative to the top of the stack.
(defun compilation-directory-properties (idx leave)
  (if leave (setq leave (match-end leave)))
  ;; find previous stack, and push onto it, or if `leave' pop it
  (let ((dir (previous-single-property-change (point) 'directory)))
    (setq dir (if dir (or (get-text-property (1- dir) 'directory)
			  (get-text-property dir 'directory))))
    `(face ,(if leave
		compilation-leave-directory-face
	      compilation-enter-directory-face)
      directory ,(if leave
		     (or (cdr dir)
			 '(nil))	; nil only isn't a property-change
		   (cons (match-string-no-properties idx) dir))
      mouse-face compilation-mouseover
      keymap compilation-button-map
      help-echo "mouse-2: visit current directory")))

;; Data type `reverse-ordered-alist' retriever.	 This function retrieves the
;; KEY element from the ALIST, creating it in the right position if not already
;; present. ALIST structure is
;; '(ANCHOR (KEY1 ...) (KEY2 ...)... (KEYn ALIST ...))
;; ANCHOR is ignored, but necessary so that elements can be inserted.  KEY1
;; may be nil.	The other KEYs are ordered backwards so that growing line
;; numbers can be inserted in front and searching can abort after half the
;; list on average.
(eval-when-compile            ;Don't keep it at runtime if not needed.
  (defmacro compilation-assq (key alist)
    `(let* ((l1 ,alist)
            (l2 (cdr l1)))
       (car (if (if (null ,key)
                    (if l2 (null (caar l2)))
                  (while (if l2 (if (caar l2) (< ,key (caar l2)) t))
                    (setq l1 l2
                          l2 (cdr l1)))
                  (if l2 (eq ,key (caar l2))))
                l2
              (setcdr l1 (cons (list ,key) l2)))))))

(defun compilation-internal-error-properties (file line end-line col end-col type fmts)
  "Get the meta-info that will be added as text-properties.
LINE, END-LINE, COL, END-COL are integers or nil.
TYPE can be 0, 1, or 2, meaning error, warning, or just info.
FILE should be (FILENAME) or (RELATIVE-FILENAME . DIRNAME) or nil.
FMTS is a list of format specs for transforming the file name.
 (See `compilation-error-regexp-alist'.)"
  (unless file (setq file '("*unknown*")))
  (let* ((file-struct (compilation-get-file-structure file fmts))
	 ;; Get first already existing marker (if any has one, all have one).
	 ;; Do this first, as the compilation-assq`s may create new nodes.
	 (marker-line (car (cddr file-struct)))	; a line structure
	 (marker (nth 3 (cadr marker-line)))	; its marker
	 (compilation-error-screen-columns compilation-error-screen-columns)
	 end-marker loc end-loc)
    (if (not (and marker (marker-buffer marker)))
	(setq marker nil)		; no valid marker for this file
      (setq loc (or line 1))		; normalize no linenumber to line 1
      (catch 'marker			; find nearest loc, at least one exists
	(dolist (x (nthcdr 3 file-struct))	; loop over remaining lines
	  (if (> (car x) loc)		; still bigger
	      (setq marker-line x)
	    (if (> (- (or (car marker-line) 1) loc)
		   (- loc (car x)))	; current line is nearer
		(setq marker-line x))
	    (throw 'marker t))))
      (setq marker (nth 3 (cadr marker-line))
	    marker-line (or (car marker-line) 1))
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (marker-position marker))
	    (when (or end-col end-line)
	      (beginning-of-line (- (or end-line line) marker-line -1))
	      (if (or (null end-col) (< end-col 0))
		  (end-of-line)
		(compilation-move-to-column
		 end-col compilation-error-screen-columns))
	      (setq end-marker (list (point-marker))))
	    (beginning-of-line (if end-line
				   (- line end-line -1)
				 (- loc marker-line -1)))
	    (if col
		(compilation-move-to-column
		 col compilation-error-screen-columns)
	      (forward-to-indentation 0))
	    (setq marker (list (point-marker)))))))

    (setq loc (compilation-assq line (cdr file-struct)))
    (if end-line
	(setq end-loc (compilation-assq end-line (cdr file-struct))
	      end-loc (compilation-assq end-col end-loc))
      (if end-col			; use same line element
	  (setq end-loc (compilation-assq end-col loc))))
    (setq loc (compilation-assq col loc))
    ;; If they are new, make the loc(s) reference the file they point to.
    (or (cdr loc) (setcdr loc `(,line ,file-struct ,@marker)))
    (if end-loc
	(or (cdr end-loc)
	    (setcdr end-loc `(,(or end-line line) ,file-struct ,@end-marker))))

    ;; Must start with face
    `(face ,compilation-message-face
	   message (,loc ,type ,end-loc)
	   ,@(if compilation-debug
		 `(debug (,(assoc (with-no-warnings matcher) font-lock-keywords)
			  ,@(match-data))))
	   help-echo ,(if col
			  "mouse-2: visit this file, line and column"
			(if line
			    "mouse-2: visit this file and line"
			  "mouse-2: visit this file"))
	   keymap compilation-button-map
	   mouse-face compilation-mouseover)))

(defun compilation-compat-error-properties (err)
  "Map old-style error ERR to new-style message."
  ;; Old-style structure is (MARKER (FILE DIR) LINE COL) or
  ;; (MARKER . MARKER).
  (let ((dst (cdr err)))
    (if (markerp dst)
	;; Must start with a face, for font-lock.
	`(face nil
	  message ,(list (list nil nil nil dst) 2)
	  help-echo "mouse-2: visit the source location"
	  keymap compilation-button-map
	  mouse-face compilation-mouseover)
      ;; Too difficult to do it by hand: dispatch to the normal code.
      (let* ((file (pop dst))
	     (line (pop dst))
	     (col (pop dst))
	     (filename (pop file))
	     (dirname (pop file))
	     (fmt (pop file)))
	(compilation-internal-error-properties
	 (cons filename dirname) line nil col nil 2 fmt)))))

;;;;;;;;;;;;;;;;;;

(provide 'compile+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile+.el ends here
