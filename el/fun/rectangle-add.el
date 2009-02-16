     (defun rectangle-add (start end)
       "Add all the lines in the region-rectangle and put the result in the 
        kill ring."
       (interactive "r")
       (let ((sum 0))
         (mapc (lambda (line)
                 (setq sum (+ sum (rectangle-add-make-number line))))
               (extract-rectangle start end))
         (kill-new (number-to-string sum))
         (message "%s" sum)))

    (defun rectangle-add-make-number (n)
      "Turn a string into a number, being tolerant of commas and even other 
       'junk'.
    When I started programming, my numeric input routines translated l 
    (lowercase ell) into 'one', as many users had learnt their
      keyboarding on manual typewriters which typically lacked 
      a separate key for the digit 1. Am I old, or what?"
    (while (string-match "[^0-9.]" n)
      (setq n (replace-match "" nil nil n)))
      (string-to-number n))