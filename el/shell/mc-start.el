
(defun mc-start()
  "Open a bash window in the current directory."
  (interactive)
  ;; I'd like to use mswindows-shell-execute but it doesn't seem to start
  ;; in the current directory, or provide a way to specify start directory.
  (start-process-shell-command
   "*gse-mc*"
   nil
   "Eterm"
   "-t 23Oz_G --title 'ike Lisp Machine' --buttonbar off --scrollbar 0 --exec mc"))

;;---------------------------------------------------------------------------

(provide 'mc-start)
