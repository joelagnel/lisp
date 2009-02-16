;;;-*- auto-recompile: t -*-
;; Time-stamp: <2001-01-17 17:12:28 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: clel.el
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 1.1alpha
 
;;; VERY UNSOPHISTICATED HELP TO CONVERT COMMON-LISP TO ELISP..

(setq clel-version "1.1alpha")

;;; 12/17/00
;;; GPL'ed as under GNU's license.
;;; Copyright (C) Deepak Goel
;;; Author/maintainer-- Deepak Goel <deego@glue.umd.edu>
;;;====================================================
;;;Availability: This code and associated files can be gotten from
;;;http://www.glue.umd.edu/~deego/emacs.html
;;;====================================================
;;;CONTRIBUTIONS: are most welcome, and will lead to invitation for
;;;co-authorship, providing you agree to GNU-freeness..
;;;====================================================
;;;QUICK INSTRUCTIONS for novices: Drop this in yr load-path, and add
;;;(require 'clel) in your .emacs.  To elispify a common-lisp code,
;;; type M-x clel-elispify ..
;;;====================================================
;;;COMMENTARY: clel provides VERY TRIVIAL conversion-helps from (Franz)
;;;Common-Lisp code to Emacs-Lisp..  It is meant only as an aid to
;;;manual code-conversion..

;;;  It does not do anything fancy.  I may make it fancier with time,
;;;  but for now, it just helps me automate some very simple jobs,
;;;  like: convert (format t)'s to (format), #' to #, and insert
;;;  (require 'cl) at the beginning of file.., and convert *some*
;;;  string-arguments to lisp..

;;;  I don't know much more about doing more fancy stuff...  If you
;;;  feel like extending the code, the following words from Colin
;;;  Walters may be useful:
; "Such a utility would be pretty impressive if it could handle anything
; but extremely trivial cases.

; The difficulties involved in converting a program using lexical scope
; to a program which uses dynamic scope, while still maintaining
; readablity, would seem to be prohibitive.  Not to mention CLOS, the
; common lisp package system, etc."
;;;====================================================



;;;###autoload
(defun clel-elispify-region (beg end)
  "Elispifies region.  VERY UNSOPHISTICATED. 
Every change is user-queried!
Does NOT insert (require 'cl) anywhere.

Note that the current-string being query-replaced will usually be in
the center of the window."

 (interactive "r")
 (save-restriction
   (narrow-to-region beg end)

;;;====================================================
;;;STAGE 1: (format t..) --> (format ..)
;;;====================================================

   (if (y-or-n-p "STAGE 1: Query replace \"(format t \" with \"(format \" ?  ")
       (query-replace "(format t " "(format ")
     )
   (goto-char (point-min))

;;;====================================================
;;;STAGE 2: #'  --------------> '
;;;====================================================
   (if (y-or-n-p "STAGE 2: Query-replace #' with ' ?  ")
       (query-replace "#'" "'"))
   (goto-char (point-min))

;;;====================================================
;;; STAGE 3: format's string-formats pseudo-corrected.
;;;====================================================
   (if (y-or-n-p "STAGE 3: Elispify the first arguments of format's? ")
       (while
	   (search-forward-regexp "\\bformat\\b" nil t)	 
	 (recenter)
	 (let* ((aa (point))
		(bb (scan-sexps aa 1))
		(initstring
		 (buffer-substring aa bb))
		(finstring
		 (with-temp-buffer
		   (insert initstring)
		   (goto-char (point-min))
		   (while (search-forward "~A" nil t)
		     (replace-match "%S" nil t))
		   (goto-char (point-min))
		   (while (search-forward "~%" nil t)
		     (replace-match "%n" nil t))
		   (goto-char (point-min))
		   (while (search-forward "~" nil t)
		     (replace-match "%" nil t))
		   (buffer-substring (point-min) (point-max)))))
	   (if (not (string= initstring finstring))
	       (if 
		   (y-or-n-p
		    (concat "Replace " initstring " by " finstring " ? "))
		   (progn
		     (kill-region aa bb)
		     (insert finstring)))))))
   
   )
 (goto-char (point-min))

;;;====================================================
;;; STAGE 4: REPLACE / BY CLEL-DIVIDE, WHICH IS CLOSER TO COMMON-LISP..
;;;====================================================
 (if (y-or-n-p "STAGE 4: Replace / by clel-divide ? ")
     (progn
       (query-replace "/" "clel-divide")
       ))
 (message "Done elispifying region.")
)

;;;###autoload
(defalias 'clel-elispify 'clel-elispify-buffer)

;;;###autoload
(defun clel-elispify-buffer ()
  "Elispifies buffer.  VERY UNSOPHISTICATED. 
Before that, Optionally inserts (require 'cl) at the beginning of buffer.
Every change is user-queried!  This also tries to make sure that
file-extension is correct.

Caution: See any warnings about (clel-elispify-region)."
  (interactive)
  (if (not (string=
	    (first (clel-extension (buffer-file-name)))
	    ".el"))
      (progn
	(ding t)
	(ding t)
	(ding t)
	(ding t)
	(if (not
	     (y-or-n-p 
"CAUTION: File-name does NOT end in .el . Continue anyway ? "))
	    (error "Halted due to incorrect filename."))))

;;;====================================================
;;; STAGE A: INSERT (REQUIRE 'CL)
;;;====================================================
  (if (y-or-n-p "STAGE A: Insert \"(require 'cl)\" ?  ")
      (progn
	(goto-char (point-min))
	(insert "(require 'cl)\n")))

;;;====================================================
;;; STAGE B: Elispify all the code..
;;;====================================================

  (if (y-or-n-p "STAGE B: Perform elispification on code? ")
      (clel-elispify-region (point-min) (point-max)))

;;;====================================================
;;; STAGE C: DEFINE THE FUNCTION CLEL-DIVIDE..
;;;====================================================
  (if (y-or-n-p "STAGE C: Define function clel-divide ?  ")
      (progn
	(goto-char (point-max))
	(insert 
	 "\n\n(defun clel-divide (&rest args)
  \"A lisp-like definition of /.
Does not say 4 / 3 = 0. Note: this usues equal and not equalp, the
last time i checked , equalp seemed to work as well.. \"
  (let ((aa (apply '/ args)))
    (if (equal (car args) (apply '* aa (cdr args)))
	aa
      (apply '/ (cons (float (car args)) (cdr args))))))
")))
  (message "Done Elispifying buffer.")
)


(defun clel-extension (filename)
  "Takes a filename and returns (extension bare-name) Thus, \"aa.tex\"
returns '(\".tex\" \"aa\"), and \"aa\" returns '(\"\" \"aa\") "
  (let ((match nil))
    (with-temp-buffer
      (insert filename)
      (goto-char (point-max))	 
      (setq match (search-backward "." nil t))
      (if (null match) (setq match (point-max)))
      (list (buffer-substring
	     match (point-max))
	    (buffer-substring
	     (point-min) match))))
)


;;;clel.el ends here.