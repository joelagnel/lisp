;;; amsmath.el --- Style hook for the AMS-LaTeX amsmath package.
;;;
;;; This will also load the amstext, amsbsy and amsopn style files.
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(TeX-add-style-hook "amsmath"
 (function
  (lambda ()

    (LaTeX-add-environments
     '("align"      LaTeX-env-label)
     '("gather"     LaTeX-env-label)
     '("flalign"    LaTeX-env-label)
     '("multline"   LaTeX-env-label)
     '("alignat"    LaTeX-amsmath-env-alignat)
     '("alignat*"   LaTeX-amsmath-env-alignat)
     '("xalignat"   LaTeX-amsmath-env-alignat)
     '("xalignat*"  LaTeX-amsmath-env-alignat)
     '("xxalignat"  LaTeX-amsmath-env-alignat)
     '("aligned"    LaTeX-amsmath-env-aligned)
     '("gathered"   LaTeX-amsmath-env-aligned)
     "align*" "gather*" "flalign*" "multline*" "equation*"
     "alignat*" "xalignat*"
     "split"
     "cases"
     "matrix" "smallmatrix" "pmatrix" "bmatrix" "Bmatrix" "vmatrix" "Vmatrix"
     "subequations" "subarray"
     )

    (TeX-add-symbols
     '("eqref" TeX-arg-ref)
     '("numberwithin" TeX-arg-counter "Section level")
     '("raisetag" "Dimension")
     '("intertext" t)
     '("hdotsfor" ["Stretch"] "Number of columns to cover")
     '("xleftarrow" ["Below"] "Above")
     '("xrightarrow" ["Below"] "Above")
     '("overset" "Accent symbol" "Symbol")
     '("underset" "Accent symbol" "Symbol")
     '("dfrac" 2)
     '("tfrac" 2)
     '("binom" 2)
     '("dbinom" 2)
     '("tbinom" 2)
     '("genfrac" "Left delimiter" "Right delimiter" "Thickness"
       "Mathstyle" 2)
     '("cfrac" ["position (l or r)"] t)
     '("smash" ["where (t or b)"] t)
     '("sideset" "Left" "Right")
     '("tag" "(Tag)")
     '("tag*" "Tag")
     '("raisetag" "Dimension")
     '("displaybreak" ["Weight (0..4)"])
     '("allowdisplaybreaks" ["Weight (1..4)"])
     '("substack" t)
     '("leftroot" "Push root index left by")
     '("uproot" "Push root index left by")
     '("boxed" t)
     "overleftarrow"  "overrightarrow"  "overleftrightarrow"
     "underleftarrow" "underrightarrow" "underleftrightarrow"
     "dotssc" "dotssb" "dotssm" "nobreakdash" 
     "Hat" "Check" "Tilde" "Acute" "Grave" "Dot" "Ddot" "Breve" "Bar" "Vec"
     "dddot" "ddddot"
     "lvert" "rvert" "lVert" "rVert" 
     "iint" "iiint" "iiiint" "idotsint"
     )
    
    (setq  LaTeX-item-list 
	   (append '(("split"    . LaTeX-item-equation)
		     ("multline" . LaTeX-item-equation)
		     ("gather"   . LaTeX-item-equations)
		     ("gather*"  . LaTeX-item-equation)
		     ("gathered" . LaTeX-item-equation)
		     ("align"    . LaTeX-item-equations)
		     ("align*"   . LaTeX-item-equation)
		     ("aligned"  . LaTeX-item-equation)
		     ("alignat"  . LaTeX-item-equations)
		     ("alignat*" . LaTeX-item-equation)
		     ("flalign"  . LaTeX-item-equations)
		     ("flalign*" . LaTeX-item-equation)
		     ("cases"    . LaTeX-item-equation))
		   LaTeX-item-list))

    (setq LaTeX-label-alist
	  (append '(("align"      . LaTeX-amsmath-label)
		    ("alignat"    . LaTeX-amsmath-label)
		    ("xalignat"   . LaTeX-amsmath-label)
		    ("aligned"    . LaTeX-amsmath-label)
		    ("flalign"    . LaTeX-amsmath-label)
		    ("gather"     . LaTeX-amsmath-label))
		  LaTeX-label-alist))

    ;; amsmath includes amstext, amsbsy, & amsopn.
    ;; So we run their hooks, too.
    (TeX-run-style-hooks "amstext" "amsbsy" "amsopn")

    ;; If RefTeX is loaded, make it recognize the amsmath environments.
    (if (featurep 'reftex)
	(reftex-add-to-label-alist '(AMSTeX)))
    )))

(defun LaTeX-amsmath-env-alignat (env)
  (let ((ncols (read-string "Number of columns: ")))
    (LaTeX-insert-environment env (concat TeX-grop ncols TeX-grcl))
    (and (not (string= "xxalignat" env))
	 (LaTeX-label environment)
	 (newline-and-indent))))

(defun LaTeX-amsmath-env-aligned (env)
  (let ((where (read-string "(optional) Vertical position (t or b): ")))
    (if (string= where "")
	(setq where "")
      (setq where (concat "[" where "]")))
    (LaTeX-insert-environment env where)))

(defun LaTeX-item-equation ()
  (end-of-line 0)
  (if (not (eq (preceding-char) ? ))
      (insert " \\\\")
    (insert "\\\\"))
  (forward-line 1)
  (indent-according-to-mode))

(defun LaTeX-item-equations ()
  (LaTeX-item-equation)
  (let ((environment (LaTeX-current-environment 1)))
    (and (LaTeX-label environment)
	 (newline-and-indent))))

(defcustom LaTeX-amsmath-label LaTeX-equation-label
  "*Default prefix to amsmath equation labels.

Amsmath equations include \"align\", \"alignat\", \"xalignat\", \"aligned\",
\"flalign\" and \"gather\"."
  :group 'LaTeX-label
  :type 'string)


;;; amsmath.el ends here.
