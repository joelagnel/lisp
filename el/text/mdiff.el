;; SYNOPSIS: Compare, skip diff, merge. What ediff doesn't do.
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh


(defun mosh-compare-windows ()
  "Compare windows, move to difference, which is colored red."
  (interactive)
  (if (eq (count-windows) 2) nil
    (error "mosh-compare-windows: need TWO WINDOWS to compare."))

  ; forward line, for easy continuation to next diff.
  (forward-line) (other-window 1)
  (forward-line) (other-window 1)

  ; Now compare them from cursor in each.
  (compare-windows 't)

  ; Now color the next char, ie. the diff
  (other-window 1) (mosh-color-x-region 'ignore 'forward-char 'red)
  (other-window 1) (mosh-color-x-region 'ignore 'forward-char 'red)
)

(defun mosh-compare-lines (dir)
  "Move two windows together, you could do line by line compare,
   sticks to goal-column, bound to C-M-up/down"
  (interactive)
  (or (= (count-windows) 2) (error "Need exactly two windows."))
  (forward-line dir) (beginning-of-line) (mosh-color-line 'bold)
  (other-window 1)
  (forward-line dir) (beginning-of-line) (mosh-color-line 'bold)
)

;; Can use ediff now? Ediff works great on linux.

(defun mosh-color-windiff ()
  "Colors an unified diff buffer file.dif, like windiff.
   Assumes you have done: diff -D WINDIFF file1 file2 > file.dif
  "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (color)
      (while (not (eobp))
        (beginning-of-line)
        (cond
         ((looking-at "^#.* WINDIFF") (mosh-color-line 'green))
         ((looking-at "\\\n") (insert " ") (backward-char))
        )
        (cond
         ((looking-at "#ifndef WINDIFF")     (setq color 'tomato))
         ((looking-at "#ifdef WINDIFF")      (setq color 'yellow))
         ((looking-at "#else /. WINDIFF ./") (setq color 'yellow))
         ((looking-at "#endif /. WINDIFF ./")     (setq color nil))
         ((looking-at "#endif /. not WINDIFF ./") (setq color nil))
         (color (mosh-color-line color))
        )
        (forward-line)
    ))
))

(defun mosh-windiff-files (file1 file2)
  "Windiff file1 and file2, generates a merged file with IFDEFs,
   and colors the diffs by calling mosh-color-windiff."
  (interactive (list
                (read-file-name "File1:")
                (read-file-name "File2:")))
  (let (shellcmd)
    (setq shellcmd (concat "diff -D WINDIFF " file1  " " file2))
    (shell-command shellcmd) (other-window 1)
    ;; (switch-to-buffer "*Shell Command Output*")
    (mosh-color-windiff)
))

(defun mosh-color-cvs-diff ()
 "Call after doing vc-diff, or cvs diff > x"
 (interactive)
;;  (delete-matching-lines "^Index")     ; dump cvs messages.
;;  (delete-matching-lines "^RCS")
;;  (delete-matching-lines "^retrieving")
;;  (delete-matching-lines "^[0-9]")
;;  (delete-matching-lines "^===")
;;  (delete-matching-lines "^---")
;;  (delete-matching-lines "^[<>] *$")  ; dump blank line diff.
;(mosh-color-regexp "^diff.*\n" 'query-replace) ; black on lightpurple incl eol?
 (mosh-color-regexp "^diff.*" 'query-replace) ; black on lightpurple
 (mosh-color-regexp "^>.*"  'bold)
 (mosh-color-regexp "^<.*"  'italic)
 (mosh-color-regexp "^!.*"  'region)
 (mosh-color-regexp "^+ .*" 'bold)
 (mosh-color-regexp "^- .*" 'italic)
)

(provide 'mdiff)
; EOF
