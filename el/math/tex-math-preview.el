;;; tex-math-preview.el --- preview TeX math expressions.

;; Copyright (C) 2006 Kevin Ryde
;;
;; tex-math-preview.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; tex-math-preview.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; http://www.gnu.org/licenses/gpl.txt, or you should have one in the file
;; COPYING which comes with GNU Emacs and other GNU programs.  Failing that,
;; write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Commentary:

;; This is a spot of code for previewing TeX maths.  `tex-mode' has buffer
;; and region previewing, but tex-math-preview is designed to pick out the
;; maths at point, and not have a shell the way tex-mode does, just show an
;; image or an error.
;;
;; This is good in texinfo-mode if you normally preview with makeinfo and
;; just want to check maths in @tex conditionals.  It's also good in
;; wikipedia-mode, where the markup not otherwise TeX at all.  It might even
;; work in other places which are not tex but have bits of tex style maths.
;;
;; An expression at point is recognised in
;;
;;     $...$ or $$...$$   tex (and texinfo inside @tex)
;;     @math{...}         texinfo (not usually anything complicated)
;;     <math>...</math>   wikipedia markup
;;
;; Previewing is done by running the expression through tex and looking at
;; the resulting DVI with `tex-dvi-view-command' (the same as tex-mode
;; uses).
;;
;; The default `tex-dvi-view-command' under X is xdvi, which is good.  On an
;; SVGA console you can use dvisvga (tmview), or alternately try a
;; combination of dvipng (or dvips+ghostscript) and an image viewer (like
;; zgv).
;;

;;; Install:

;; To make M-x tex-math-preview available, put tex-math-preview.el somewhere
;; in your load-path and the following in .emacs
;;
;;     (autoload 'tex-math-preview "tex-math-preview" nil t)
;;
;; Bind it to a key if you like, eg. f8,
;;
;;     (add-hook 'texinfo-mode-hook
;;                (lambda ()
;;                  (define-key texinfo-mode-map [f8] 'tex-math-preview)))
;;

;;; History:

;; Version 1 - the first version.


;;; Code:

;; See tex-math-preview docstring below for what's matched.
;; Note the non-greedy patterns, don't want to run though to last ending.
;;
;; thing-at-point-looking-at doesn't quite do what's wanted here, on account
;; of "$" being both a start and an end in the regexp.  skip-chars-backward
;; and plain looking-at cover the cases where point is just before or after
;; "$" or "$$" (or in the middle of "$$").  For those we take the "$"s to be
;; the start of the math form.
;;    
(put 'tex-math 'bounds-of-thing-at-point
     (lambda ()
       (let ((re "\\$+\\([^$]+?\\)\\$+\\|<math>\\(.*?\\)</math>\\|@math{\\([^}]+?\\)}"))
         (save-excursion
           (skip-chars-backward "$")
           (and (or (looking-at re)
                    (thing-at-point-looking-at re))
                (cond ((match-beginning 1)
                       (cons (match-beginning 1) (match-end 1)))
                      ((match-beginning 2)
                       (cons (match-beginning 2) (match-end 2)))
                      (t
                       (cons (match-beginning 3) (match-end 3)))))))))

(defun tex-math-preview ()
  "Preview the tex math expression at point.
`tex-dvi-view-command' is used for viewing.
Math expressions are recognised within

    $...$ or $$...$$   tex
    @math{...}         texinfo
    <math>...</math>   wikipedia"

  (interactive)
  (let ((str (thing-at-point 'tex-math)))
    (or str
        (error "Not in a TeX math expression"))
    (tex-math-preview-str str)))

(defun tex-math-preview-str (str)
  "Preview the given STR string as a TeX math expression.
`tex-dvi-view-command' is used for viewing.
STR should not have any $ or $$ delimiters (they're added here)."

  (require 'tex-mode)
  (let* ((dir     (make-temp-file "tex-math-preview-" t))
         (dot-tex (concat dir "/foo.tex"))
         (dot-dvi (concat dir "/foo.dvi"))
         (dot-log (concat dir "/foo.log"))

         ;; eval/expand like `tex-view' and `tex-send-command'
         (view-template (eval tex-dvi-view-command))
         (view-command  (replace-regexp-in-string "\\*" dot-dvi
                                                  view-template t t)))
    (if (string-equal view-command view-template)
        (setq view-command (concat view-command " " dot-dvi)))

    (with-temp-file dot-tex
      (insert "$$\n")
      (insert (replace-regexp-in-string "\n" " " str t t))
      (insert "\n$$\n\\par\\bye\n"))

    ;; don't show all the tex ramblings in the minibuffer, leave it to the
    ;; shell buffer, and show that only if there's an error (by putting back
    ;; the window config on success)
    ;;
    (let ((max-mini-window-height 1)
          (windows (current-window-configuration)))
      (when (eq 0 (shell-command
                   (concat
                    "tex -output-directory " dir " " dot-tex " </dev/null &&"
                    view-command)))
        (set-window-configuration windows)))

    (dolist (filename (list dot-tex dot-dvi dot-log))
      (condition-case err (delete-file filename) (error)))
    (delete-directory dir)))


(provide 'tex-math-preview)

;;; tex-math-preview.el ends here
