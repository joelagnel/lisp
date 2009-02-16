;; SYNOPSIS: Functions for converting from txt to html.
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh

(global-set-key [S-down] 'format-this-para)

(defun mosh-format-this-para (&optional go)
  "Format a para, optionally goto end of para."

  (interactive)
  (let (eop)               ;; eop is end-of-para point.
    (save-excursion
      (forward-paragraph)
      (setq eop (point)))
    (save-excursion
      (if (re-search-forward "^ *[A-Z]" eop)
          (progn (goto-char (match-beginning 0)) (insert "\n"))))
  )
  (fill-paragraph 1)
  (if go (forward-paragraph))
)

(defun find-sections ()
  (interactive)
  (list-matching-lines "^\\([A-Z0-9][.]\\)")
)


(defun find-section-before-it ()
  (interactive)
  (list-matching-lines "[^\C-j][\C-j][A-Z0-9][.]")
)

(defun find-section-after-it ()
  (interactive)
  (list-matching-lines "^[A-Z0-9][.].*[\C-j][^\C-j]")
)

(defun space-before-it ()
  ;; First section-before-it
  (interactive)
  (while (re-search-forward "[^\C-j][\C-j][A-Z0-9][.]" )
    (beginning-of-line)
    (insert "\C-j")
    (forward-line 1)
))

(defun space-after-it ()
  ;; First section-after-it
  (interactive)
  (while (re-search-forward "^[A-Z0-9][.].*[\C-j][^\C-j]" )
    (beginning-of-line)
    (insert "\C-j")
))

(defun put-para-marks ()
  "Put a <p> whenever empty lines are seen."
  (interactive)
  (query-replace-regexp "\n\n\\([A-Za-z0-9]\\)" "\n\n<p>\n\\1" nil)
)

;; Not needed, doesn't do much.
;; Eg. (mhtml-make-mark "ma") gives:
;;     <a name="ma">ma</a>
;;     <a href="#ma"> Click here to goto ma</a>

(defun mhtml-make-mark (name)
  "Put a name here, so we can jump here from elsewhere.
     Example:    (mhtml-make-mark \"hello\")
     You get: 1  <a name=\"hello\"> hello </a>
     and      2  <a href=\"#hello\"> click here to goto \"hello\" </a>
    You should move line 2 to whereever you need it.
  "

  (interactive "sName for this point:")
  (insert "<a name=\""  name "\">" name "</a>\n")
  (insert "<a href=\"#" name "\"> Click here to goto " name "</a>\n")
)

;;; Doesn't do much.

(defun mhtml-make-link (link)

  "Make current line point to a link.
   Example:
      Old:       HELLO
      Command:   (mhtml-make-link \"heaven.html\")
      New:       <A HREF=\"heaven.html\">HELLO</A>
   "

  (interactive "Flink:")
  (beginning-of-line)
  (insert "<A HREF=\"" link "\">")
  (mosh-eat-whitespace-forward)
  (end-of-line)
  (mosh-eat-whitespace-backward)
  (insert "</A>")
)

;;; EOF
