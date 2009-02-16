;;; block-comment.el        -*- Emacs-Lisp -*-

;;; Draw block comment blocks around text

;;; Created:    "Sun Feb  6 02:57:51 EST 2000"
;;; Time-stamp: "Fri Feb 11 20:49:44 EST 2000"
;;; Author:     Alex Shinn <foof@eyeofdog.com>


 (defvar block-comment-start nil
   "Starting string for an entire block comment" )
 
(defvar block-comment-end nil
  "Ending string for an entire block comment" )

(defvar block-comment-left nil
  "Starting string for each line of a block comment" )

(defvar block-comment-right nil
  "Ending string for each line of a block comment" )

(defvar block-comment-top-right nil
  "Ending string for the firstline of a block comment" )

(defvar block-comment-bot-left nil
  "Starting string for last line of a block comment" )

(defvar block-comment-char nil
  "Char to use for line drawing with block comments" )


(defun max-line-width (start end)
  "Count the maximum line width in the current region"
  (interactive "r")
  (save-excursion
    (let ((mlw 0))
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward ".+" end t))
        (if (> (string-width (match-string 0)) mlw)
            (setq mlw (string-width (match-string 0))) ))
      mlw )))

(defun block-comment-region (start end &optional sides)
  "Formats the current region in a pretty comment block.

The optional prefix argument represents the number of sides of the box
to draw around the comment.  With no prefix argument, or with a prefix
argument of 1, creates a block comment with a single line along the
left.  With a prefix of two, inserts a comment initiator and line
seperator before and after the region, but does not change the
intervening lines (obviously this doesn't work with languages that
don't support block comments).  A prefix of three draws the top,
bottom and left hand side of a box around the comment, while a prefix
of four closes the box with a right hand column.

This can be customized for different languages by setting the
variables `block-comment-start', `block-comment-end',
`block-comment-left', `block-comment-right',
`block-comment-top-right', `block-comment-bot-left' and
`block-comment-char'.  There is no un-block-comment-region functions,
since this is intended for commenting descriptive text.  If you're
just selectively commenting and uncommenting lines of code you should
use comment-region."
  (interactive "r\np")
  (if (not sides) (setq sides 1))
  (save-excursion
    (save-restriction
      (let ((bcs (or block-comment-start "##"))
            (bce (or block-comment-end ""))
            (bcl (or block-comment-left "## "))
            (bcr (or block-comment-right " #"))
            (bct (or block-comment-top-right ""))
            (bcb (or block-comment-bot-left ""))
            (bcc (or block-comment-char ?#))
            (width (or fill-column 72)) )
        ;; Adjust the width if we're filling the RHS
        (if (eq sides 4)
            (setq width (+ (max-line-width start end)
                           (max (+ (string-width bcs)
                                   (string-width bct) )
                                (+ (string-width bcl)
                                   (string-width bcr) )
                                (+ (string-width bce)
                                   (string-width bcb) )))))
        ;; Just work between start and end
        (narrow-to-region start end)
        ;; Open the comment
        (goto-char start)
        (insert bcs)
        (if (> sides 1)
            (insert (make-string (- width (string-width bcs)
                                    (string-width bct) )
                                 bcc )
                    bct "\n" )
          (forward-line) )
        ;; Comment out the interior
        (if (= sides 2) nil
          (while (not (eobp))
            (insert bcl)
            (end-of-line)
            (if (< sides 4) nil
              (insert (make-string (- width (- (line-end-position)
                                               (line-beginning-position) )
                                      (string-width bcr) )
                                   ?  )
                      bcr ))
            (forward-line) ))
        ;; Close the comment
        (insert bcb)
        (if (> sides 1)
            (insert (make-string (- width (string-width bce)
                                    (string-width bcb) )
                                 bcc )))
        (insert bce) ))))

(provide 'block-comment)
