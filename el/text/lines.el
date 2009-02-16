;;; -*- auto-recompile: t -*-
;;; Lines.el,  GNU PUBLIC LICENSE. 
;; Time-stamp: <2001-02-28 23:20:29 dgoel>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: lines.el
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.2.3release

(defvar lines-version "0.2.3release")

;;;COMMENTARY: lines functions to help deal with data-files..

;;; Sometimes you want to use lines- functions instead of point-
;;; functions, even though it is slower.  Particularly if u r dealing with
;;; parsing/editing a data-file, with, say data arranged in columns.
;;; lines.el defines most lines- counterparts of (point-max) (point-min)
;;; (point) (kill-region) etc. [for instance, emacs' default lines-what
;;; does not return an integer, which is what u may want during
;;; programming]

;;; Lines.el also defines functions such as  lines-get-fields (which gets
;;; all fields on this line, assuming they are lisp-expressions). 


;;; BEFORE DOING ANYTHING WITH A BUFFER, please do not forget to call 
;;;; lines-narrow-initial..

(require 'cl)


(defun lines-empty-error ()
  ""
  (error "Empty buffer. NOTE: M-x lines-warning.")
)

(defun lines-warning ()
  ""
  (let ((lines-loudness 1))
    (lines-message 
   "THIS program assumes that the proper form of the (data-) file you
  examine ends in \\n.  Anything in your file after the last \\n
  will be ignored."))
)

;;;Mon Jan 15 04:09:30 2001
;;;###autoload
(defun lines-widen ()
  (widen)
)

;;;Mon Jan 15 03:32:05 2001
;;;###autoload
(defun lines-narrow-initial (&optional ERR)
  "Narrows such that the last char is a \\n
If the buffer survives, returns the size of the buffer, else nil.
Optional arg ERR results in ERR upon empty buffer. 
FOR FUTURE EDITS: NEVER CALL OTHER LINES FUNCTIONS WITHIN THIS
FUNCTION, THIS ONE IS CALLED BY ALL OTHERS!  

"
  (interactive)
  (save-excursion
    (let
	((lastn
	  (progn
	    (goto-char (point-max))
	    (if 
		(search-backward "\n" nil t)
		(+ (point) 1)
	      (point-min)))))
      (narrow-to-region (point-min) lastn))
    (if (> (point-max) (point-min))
	(point-max)
      nil))
)


(defvar lines-loudness 0.6 "Tells you how noisy lines will be..
Between  0 and 1 are meaningful values")

(defun lines-message (&optional args)
  (if (> lines-loudness 0.5) (apply 'message args))
)


;;;###autoload
(defalias 'lines-what-line 'lines-what)

;;;Wed Jan 17 00:11:38 2001
;;;###autoload
(defun lines-what-narrowed (&optional given-point )
  " Like lines-what-line, except assumes a narrowed buffer. 
Mostly like what-line, except: returns integer! 
Tells you the current line.. If narrowed, assumes that the first
visible line is number 1..   As if the buffer were the entire buffer..
Respects narrowing..

If DONTNARROW is t, assume that lines has already been narrowed..
"
  (interactive)
  (let ((opoint (if given-point given-point (point)))
	start)
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line)
      (setq start (point))
      (goto-char opoint)
      (beginning-of-line)
      (let
	  ((result
	    (if (/= start 1)
		(1+ (count-lines start (point)))
	      (1+ (count-lines start (point))))))
	(if (interactive-p)
	    (message (format "%S" result)))
	result)))
  )

;;;Wed Jan 17 00:11:38 2001
;;;###autoload
(defun lines-what(&optional given-point )
  " Mostly like what-line, except: returns integer! 
Tells you the current line.. Ignores any narrowing when counting
lines, but does not disrupt the narrowing..
If DONTNARROW is t, assume that lines has already been narrowed..

Hacked from the code of what-line, and i still don't understand some
stuff about the relevance of start here..

Thus, even if the buffer has been narrowed, lines-what will try to
return the true line-number.. Agreed this may slow things down for
large files, but makes sense to me.. if u don't like this, please
consider using  lines-what-narrowed..
"
  (interactive)
  (let ((opoint (if given-point given-point (point)))
	start)
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line)
      (setq start (point))
      (goto-char opoint)
      (beginning-of-line)
      (let
	  ((result
	    (if (/= start 1)
		(1+ (count-lines 1 (point)))
	      (1+ (count-lines 1 (point))))))
	(if (interactive-p)
	    (message (format "%S" result)))
	result)))
  )


;;;###autoload
(defalias 'lines-line-difference 'lines-difference)

;;;###autoload
(defun lines-difference (start end )
  "Nothing more than the difference between the line at start and the
one at end.  start and end are points..  See also the default
count-lines..
If DONTNARROW is t, assume that lines has already been narrowed..

"
  (save-excursion
    (- (lines-what-line end )
       (lines-what-line start )))
  )


;;;###autoload
(defalias 'lines-last-line-p 'lines-last-p)

;;;###autoload
(defun lines-last-p ()
" Tells if we are on the last line.
If DONTNARROW is t, assume that lines has already been narrowed.."
  (interactive)
  (save-excursion
    (end-of-line)
    (equal (point) (point-max)))
 )

;;;###autoload
(defalias 'lines-first-line-p 'lines-first-p)

;;;###autoload
(defun lines-first-p ()
"If DONTNARROW is t, assume that lines has already been narrowed.."
  (interactive)
    (save-excursion
      (beginning-of-line)
      (equal (point) (point-min)))
)
    
;;;###autoload
(defalias 'lines-line-min 'lines-min)

;;;###autoload
(defun lines-min ()
  "Like point-min..
If DONTNARROW is t, assume that lines has already been narrowed..

"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (lines-what-line (point) ))
)

;;;###autoload
(defalias 'lines-line-max 'lines-max)

(defun lines-max ()
  "Like point-max
If DONTNARROW is t, assume that lines has already been narrowed..
"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (lines-what-line (point) ))
)


;;;Tue Jan 16 11:26:30 2001
;;;###autoload
(defalias 'lines-kill-this 'lines-kill-one)

;;;Tue Jan 16 11:26:26 2001
;;;###autoload
(defun lines-kill-one (&optional pt1 )
  (interactive)
  (if (null pt1) 
      (setq pt1 (point)))
  (lines-kill pt1 pt1 ))


;;;Tue Jan 16 11:50:55 2001
;;;###autoload
(defun lines-kill-by-lines (&optional l1 l2 )
  "Kills from line1 to line2.  If l1 or l2 is not specified, passes nil to
lines-kill..

Use this function only if necessary..
This function calls line-kill.. which is the one to be preferred for
speed..

"
  (save-excursion
    (let
	((pt1 
	  (if (null l1) nil
	    (progn
	      (goto-line l1)
	      (point))))
	 (pt2
	  (if (null l2) nil
	    (progn
	      (goto-line l2)
	      (point)))))
      (lines-kill pt1 pt2 )))
  )

	  


;;;Tue Jan 16 11:26:22 2001
;;;###autoload
(defalias 'lines-kill-line 'lines-kill-one)


;;;###autoload
(defun lines-kill (&optional pt1 pt2 )
  "Kills this line completely.  

If PT1 and PT2 are specified, kills all lines through the line on PT1
to line on PT2, inclusive.  

If neither PT1 is not specified, kills between point and mark.

If only PT1 is specified, and PT2 is nil, takes PT2 to be PT1,
viz. kills the line on PT1.


If DONTNARROW is t, assume that buffer has already been narrowed
initially. 

If the second point to be killed is point-max, viz. is at a line we
don't consider to be on the buffer, this function appropriately
subtracts 1 from it so as to make it a part of the last legal line. 

"
  (interactive)
  (when (null pt1)
    (setq pt1 (mark))
    (setq pt2 (point)))
  (when (null pt1) ;;if mark is undefined..
    (setq pt1 pt2))
  (lines-swap-if-necc 'pt1 'pt2)  ;;;ensure pt1 <= pt2.
  (if (= pt2 (point-max)) (setq pt2 (- pt2 1)))
  (if (= pt1 (point-max)) (setq pt1 (- pt1 1)))
  (save-excursion
    (let ((a1 
	   (progn
	     (goto-char pt1)
	     (beginning-of-line)
	     (point)))
	  (a2
	   (progn
	     (goto-char pt2)
	     (end-of-line)
	     (+ (point) 1))))
      (kill-region a1 a2)))
  )

     



(defun lines-backward-char ()
  "Moves one point back.  Returns point if succeeds, else nil.
Never gives error!
Actually, i don't think we need this function..
"
  (interactive)
  (let ((pt (point)))
    (ignore-errors (backward-char 1))
    (if (/= (point) pt)
	pt
      nil))
  )

;;;Tue Jan 16 17:35:29 2001
;;;###autoload
(defun lines-get-fields-by-lines (&optional line)
  "Gets the field on the given line"
  (lines-get-fields (lines-point-for-line line))
)


;;;Thu Feb  8 14:48:47 2001
;;;###autoload
(defun lines-point-for-line (line)
  (save-excursion
    (goto-line line)
    (point)))


;;;Mon Jan 15 02:42:19 2001
;;;###autoload
(defun lines-get-fields (&optional pt )
  "Gets the fields if any on the current line, as a list. 
Uses scan-sexps==>
will be affected by the value of parse-sexp-ignore-comments..

I think this needs to be totally rewritten.. to give the same results,
but much more efficiently..
"
  (interactive)
  (if (null pt) (setq pt (point)))
  (if (= pt (point-max))
      (goto-char (- pt 1)))
  (save-excursion
    (goto-char pt)
    (let ((expr (lines-at-point ))
	  fields)
      (if (null expr)
	  (error "Attempt to get fields beyond the last RET "))
      (with-temp-buffer
	(insert "(setq fields (quote (")
	(insert expr)
	(insert " )))")
	(eval-buffer))
      (if (interactive-p) (message "%S" fields))
      fields))
  )

;;;       (let ((doing (point-min)))
;;; 	(while doing
;;; 	  (setq doing (scan-sexps doing 1))
;;; 	  (when doing
;;; 	    (goto-char doing)
;;; 	    (setq fields (cons (format "%S" (sexp-at-point)) fields))))))
;;;     (reverse fields))
;;;  )

       
;;;Mon Jan 15 16:29:12 2001
;;;###autoload
(defalias 'lines-line-at-point-verbatim 'lines-at-point-verbatim)

;;;Mon Jan 15 03:02:17 2001
;;;###autoload
(defun lines-at-point-verbatim ( )
  "Gives you just this one line at tthe current point. 
this returns you the line along with the trailing \\n.   Thus, if the
buffer ended up empty upon line-narrowing, this will return \"\".  
If DONTNARROW is t, assume that lines has already been narrowed..
"
  (interactive)
  (buffer-substring
   (save-excursion
     (beginning-of-line)
     (point))
   (save-excursion
     (forward-line 1)
     (point)))
  )

;;;Mon Jan 15 16:29:40 2001
;;;###autoload
(defalias 'lines-line-at-point 'lines-at-point)

;;;Mon Jan 15 03:55:05 2001
;;;###autoload
(defun lines-at-point ()
  "Returns the line at this point, without the trailing \\newline.
If the buffer is empty, returns nil.
If DONTNARROW is t, assume that lines has already been narrowed..
"
  (interactive)
  (let ((string (lines-at-point-verbatim )))
    (let  ((len (length string)))	   
      (if (> len 0)
	  (substring string 0 (- len 1))
	nil)))
)





;;;Tue Jan 16 11:35:20 2001
(defun lines-swap-if-necc (sym1 sym2)
  "INTERNAL..
Ensures that the value of symbol SYM1 if less than that of SYM2"
  (when (> (eval sym1) (eval sym2))
    (let ((v2 (eval sym2)))
      (set sym2 (eval sym1))
      (set sym1 v2)))
)



;;;Tue Jan 16 15:50:31 2001
;;;###autoload
(defun lines-narrow (&optional pt1 pt2 )
  "If called with no arguments, will assume point mark.  If pt2 is
undefined, will take it to be the same as pt1. 

Will narrow buffer from the line starting pt1 to the line ending
pt2, inclusive.  If pt1 is > pt2, will be swapped.. "
  (interactive)
  (if (null pt1)
      (progn
	(setq pt1 (mark))
	(setq pt2 (point))))
  (if (null pt2)
      (setq pt2 pt1))
  (lines-swap-if-necc 'pt1 'pt2)
  (save-excursion
    (narrow-to-region
     (progn
       (goto-char pt1)
       (beginning-of-line)
       (point))
     (progn
       (goto-char pt2)
       (end-of-line)
       (if (not (= (point-max) (point)))
	   (forward-char 1))
       (point))))
)

;;;Tue Jan 16 17:33:51 2001
;;;###autoload
(defun lines-for-point (&optional pt)
 "Line number on the point" 
 (interactive)
 (if (null pt) (setq pt (point)))
 (save-excursion
   (goto-char pt)
   (lines-what)))

  

(provide 'lines)

;;;lines.el ends here..  
