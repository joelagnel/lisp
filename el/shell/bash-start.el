
(defun bash-start ()
  "Open a bash window in the current directory."
  (interactive)
  (start-process-shell-command
   "*gse-bash*"
   nil
   "Eterm"
   "-t 23Oz_G --title 'ike Lisp Machine' --colors-suppress-bold --buttonbar off --scrollbar 0 -g 116x33"))

;;---------------------------------------------------------------------------

(provide 'bash-start)
