;;; slides.el - Special code for slitex.
;;
;; $Id: slides.el,v 1.3 1999/07/16 13:48:18 abraham Exp $

;;; Code:

(TeX-add-style-hook "slides"
 (function
  (lambda ()
    (setq LaTeX-default-style "slides")
    (add-hook 'LaTeX-document-style-hook 'LaTeX-style-slides)
    (LaTeX-add-environments '("slide" LaTeX-env-slide)
			    '("overlay" LaTeX-env-slide))
    (TeX-run-style-hooks "SLITEX"))))

(defvar LaTeX-slide-color "" 
  "*Default slide color.")

 (make-variable-buffer-local 'LaTeX-slide-color)

(defun LaTeX-style-slides ()
  "Prompt for and insert SliTeX options."
  (let ((slide-file (read-input "Slide file: "))
	(slide-colors (read-input "Slide colors (comma separetade list): "
				  "black")))
    (save-excursion
      (goto-char (point-min))		; insert before \end{document}
      (if (re-search-forward ".end.document." (point-max) t)
	  (beginning-of-line 1))
      (open-line 2)
      (indent-relative-maybe)
      (if (equal slide-colors "black")
	  (insert TeX-esc "blackandwhite"
		  TeX-grop slide-file TeX-grcl)
	(progn
	  (insert TeX-esc "colors"
		  TeX-grop slide-colors TeX-grcl)
	  (newline-and-indent)
	  (insert TeX-esc "colorslides"
		  TeX-grop slide-file TeX-grcl))))))

(defun LaTeX-env-slide (environment)
  "Insert ENVIRONMENT and prompt for slide colors."
  (setq LaTeX-slide-color
	(read-input "Slide colors: " LaTeX-slide-color))
  (LaTeX-insert-environment environment
			    (concat TeX-grop LaTeX-slide-color TeX-grcl)))


;;; slides.el ends here
