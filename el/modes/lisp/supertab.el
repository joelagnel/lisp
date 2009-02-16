;;; $Id: supertab.el,v 1.7 1997/03/16 13:25:01 queinnec Exp $
;;; Copyright (C) 1994-1997 by C.Queinnec (Polytechnique & INRIA)

;;; LCD Archive Entry:
;;; supertab|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; Enhanced TAB in Lisp-based modes.|
;;; $Date: 1997/03/16 13:25:01 $|$Revision: 1.7 $|
;;; ~/misc/supertab.el.Z|

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; The purpose of this package is to enhance the behavior of TAB in
;;; Lisp-based mode. Usually TAB only indents the current line. After
;;; loading this file, TAB will perform different actions depending on
;;; where it is invoked. Consider the following two lines:
;;;    /-----------------------------------------------
;;;    |        (foo   (bar hux)       
;;;    |            wek)
;;; a single TAB wherever on first line will indent it,
;;; a single TAB wherever on second line will indent it:
;;;    /-----------------------------------------------
;;;    |(foo   (bar hux)       
;;;    |       wek)
;;; When a line is correctly indented, TAB within the line becomes meaningful.
;;; a TAB between foo and (bar hux) will leave a single space:
;;;    /-----------------------------------------------
;;;    |(foo (bar hux)       
;;;    |       wek)
;;; a TAB after (bar hux) will swallow all the following (useless) spaces.
;;; a second TAB after (bar hux) will swallow the newline and the 
;;;                              following spaces before wek:
;;;    /-----------------------------------------------
;;;    |(foo (bar hux) wek)

;;; Installation:

;;; To use this file, just (auto)load (or require) it and binds TAB to the
;;; supertab-handle-tab function:
;;;     (autoload 'supertab-handle-tab "supertab")
;;;     (define-key {scheme|lisp|emacs-lisp}-mode-map "\t" 
;;;         'supertab-handle-tab )

;;; Repository:

;;; Newer versions will be sent to the LCD Archive but may appear earlier
;;; on ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/supertab.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     file://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html

;;; Code:

;;; This code should work with all lisp-based modes: lisp, scheme,
;;; emacs-lisp, lisp-interaction, shared-lisp etc. 
;;; The supertab-handle-tab has the same interface as lisp-indent-line.

(defun supertab-handle-tab (&optional whole-exp)
  "Indent current line as Lisp or Scheme code, remove trailing whitespaces 
at the end of a line and also in between Sexpressions. If doubled and at the
end of a line, consider the next NL as a whitespace."
  (interactive "P")
  (funcall indent-line-function whole-exp) 
  (let ((here     (point)) 
        (repeated (and (eq this-command last-command)
                       (eq this-command 'supertab-handle-tab) )) )
    (cond 
     ;; second time the command is called, swallow end of line if possible.
     (repeated 
      (if (and (eolp) (not (eobp))) 
          (progn (delete-char 1)
                 ;; stop repetition.
                 (setq this-command nil) ) )
      (supertab-swallow-surrounding-whitespaces here) )
     ;; first time the command is called
     (t (beginning-of-line)
        (skip-chars-forward " \t")
        (cond ((= (point) here)
               ;; point is at the first non blank char, leave it.
               nil )
              (t (goto-char here)
                 (supertab-swallow-surrounding-whitespaces here) ) ) ) ) ) )

;;; This function removes all the spaces surrounding the dot, it then
;;; calls supertab-maybe-insert-one-whitespace to reintroduce a single
;;; space if needed for readability or syntax.

(defun supertab-swallow-surrounding-whitespaces (here)
  "Remove in Lisp-based syntax the surrounding whitespaces to the minimal
possible. Leave in place the heading indentation and try to leave the dot 
in place."
  (let (there size)
    (supertab-skip-whitespaces 'backward)
    (setq there (point))
    (setq size (supertab-skip-whitespaces 'forward))
    (if (> size 0) (progn (delete-region there (point))
                          (supertab-maybe-insert-one-whitespace) ))
    ;; try to keep point where it was
    (if (< here (point)) (goto-char here)) ) )

;;; skip-syntax-*ward do not exist in Emacs 18 so provide a function to
;;; skip over whitespaces.

(defun supertab-skip-whitespaces (way)
  "Skip {for/back}ward over whitespaces. 
Return the amount of skipped characters as skip-syntax-*ward."
  (let ((result 0)
        (categ (char-syntax ? )) )
    (cond ((eq way 'backward)
           (while (= (char-syntax (preceding-char)) categ)
             (setq result (+ result 1))
             (backward-char 1) ) )
          ((eq way 'forward)
           (while (= (char-syntax (following-char)) categ)
             (setq result (+ result 1))
             (forward-char 1) ) )
          (t (error "No way")) )
    result ) )

;;; This function tries to insert a space only if it eases
;;; readability.  It may be improved but as it stands, it already
;;; covers a lot of situations.

(defun supertab-maybe-insert-one-whitespace ()
  "Try to see if a whitespace is needed in the current context ie after a
supertab-swallow-surrounding-whitespaces command."
  (let ((prev-type (char-syntax (preceding-char)))
        (fol-type  (char-syntax (following-char))) )
    (if (cond 
         ;; end of line, dont insert blank
         ((eolp) nil)
         ;; dont between two open parens
         ((and (= prev-type fol-type) 
               (or (= prev-type ?\() (= prev-type ?\))) )
          nil )
         ;; in doubt, insert one blank.
         (t t) )
        (insert " ") ) ) )

;;; so it can be require'd

(provide 'supertab)

;;; end of supertab.el
