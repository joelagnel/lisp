;;; epsf.el - Support for the epsf style option.

;; Contributed by Marc Gemis <makke@wins.uia.ac.be>

;;; Code: 

(TeX-add-style-hook "epsf"
 (function
  (lambda ()
    (TeX-add-symbols
     '("epsfsize" TeX-arg-epsfsize)
     '("epsffile" TeX-arg-file)
     '("epsfbox" TeX-arg-file)
     "epsflly" "epsfury" "testit" "epsfgetlitbb"
     "epsfnormal" "epsfgetbb" "other" "epsfsetgraph"
     "PsFragSpecialArgs" "epsfaux" "testit" "epsfgrab"
     "epsfllx" "epsflly" "epsfury" "epsfverbosetrue"
     )
    (LaTeX-add-environments
     '("epsffig" LaTeX-env-epsffigure)
     )

    )))


(defun LaTeX-env-epsffigure (environment)
  "Create a `figure'-environment with \\label and \\caption and \\epsfbox
commands. Eventually a `psfrags'-environment is inserted round the \\epsfbox."

  (let ((float (read-input "Float to: " LaTeX-float))
	(caption (read-input "Caption: "))
	(label (read-input "Label: " LaTeX-figure-label))
        ; gf: ask if there is an psfrag environment needed
	(psfrag (y-or-n-p "PS fragments: "))
	(psfile (read-file-name "EPS-file: " "" "" nil))
	)

    (setq LaTeX-float (if (zerop (length float))
			  LaTeX-float
			float))

    (LaTeX-insert-environment "figure"
			      (concat LaTeX-optop LaTeX-float LaTeX-optcl))
    (LaTeX-insert-environment "center")
    (if psfrag
	(progn
	  (LaTeX-insert-environment "psfrags")
	  (newline-and-indent)
	  ))
    (if (or (zerop (length label))
	    (and (string= "figure" environment)
		 (equal LaTeX-figure-label label))
	    )
	()
      (newline-and-indent)
      (insert TeX-esc "label" TeX-grop label TeX-grcl)
      (end-of-line 0)
      (LaTeX-indent-line))


    (newline-and-indent)
    (insert TeX-esc "leavevmode")
    (newline-and-indent)
    (insert TeX-esc "epsfbox" TeX-grop psfile TeX-grcl)
    (if (zerop (length caption))
	()
      (newline-and-indent)
      (insert TeX-esc "caption" TeX-grop caption TeX-grcl))
    (newline)
    (forward-line 4)
    (newline)

))

(defun TeX-arg-epsfsize (optional &optional prompt definition)
  "Create a line that print epsf figures at a certain percentage"
  (interactive)
  (let ((scale (read-input "Scale (%): "))
	)
    (setq scalestr (if (zerop (length scale))
		       "75"
		     (format "%s" scale)
		     ))
    (save-excursion
      ; append #1#{scale#1}
      (insert "#1#2" TeX-grop "0." scale "#1" TeX-grcl)
      ; insert \def before \epsfsize
      (beginning-of-line 1)
      (newline)
      (insert TeX-esc "def")
      (forward-line -1)
      (insert "% From now on print figures at " scale "% of original size")
      )
    (end-of-line)))

;;; epsf.el ends here
