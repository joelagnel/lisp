;; chord-mode.el by Martin Schwenke, 1994
;;
;; Defines some stuff for editing guitar music.

(add-to-list 'auto-mode-alist '("\\.crd$\\|\\.chd$" . chord-mode))

(defun chord-mode ()
  "Chord mode currently defines a single key-stroke, C-', which is bound
to the function mms-insert-chords, and enters text mode."

  (interactive)
  (text-mode)
  (modify-syntax-entry ?# "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?( "w")
  (modify-syntax-entry ?) "w")
  (setq comment-start "#")
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c'" 'mms-insert-chords))

(defun mms-insert-chords ()
  "Insert chords written above lyrics into lyrics."

  (interactive)

  (beginning-of-line)
  (kill-line 2)
  (yank)
  (yank)
  (forward-line -2)

  (let ((this-point (point)))
    (end-of-line)
    (backward-word 1)

    (while (>= (point) this-point)
      (let ((this-column (current-column)))
	(kill-word 1)
	(forward-line)
	(move-to-column this-column)
	(if (< (current-column) this-column)
	    (insert-char ?  (- this-column (current-column))))
	(insert-string "[")
	(yank)
	(insert-string "]")
	(forward-line -1)
	(move-to-column this-column)
	(yank)
	(move-to-column this-column)
	(backward-word 1)))

    (if (< (point) this-point)
	(forward-line 1))
    (kill-line 1)
    (forward-line -2)))

(provide 'chord-mode)
