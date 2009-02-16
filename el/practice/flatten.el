

(defun flatten (lst)
  (if (null lst)
      nil
    (if (listp (car lst))
	(append (flatten (car lst))
		(flatten (cdr lst)))
      (cons (car lst)
	    (flatten (cdr lst))))))

(flatten '(a (a b ) (c d)))

(mapcar '(lambda (x)
	   (capitalize x))
	'("lisp" "is" "cool"))
		
	       