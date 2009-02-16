;; Version 1:
;; A simple evaluator, without tracing info

(defun *eval-expr (expr)
  (cond ((or (numberp expr)
	     (stringp expr)
	     (eq nil expr)
	     (eq t expr))
	 expr)
	((symbolp expr)
	 (*eval-varexpr expr))
	((listp expr)
	 (*eval-listexpr expr))
	(t (error "I don't know how to *eval-expr %S" expr))))

(defun *eval-varexpr (expr)
  (symbol-value expr))

(defun *eval-exprs (exprs)
  (if exprs (let* ((head (car exprs))
		   (rest (cdr exprs)))
	      (cons (*eval-expr head)
		    (*eval-exprs rest)))))

(defun *eval-listexpr (expr)
  (let* ((head (car expr))
	 (rest (cdr expr)))
    (apply head (*eval-exprs rest))))

;; Some tests

' (let ((a 2))
    (*eval-expr 'a))

' (let ((a 1) (b 2) (c 3) (d 4))
    (*eval-expr '(* (+ a b) (+ c d))))




;; Version 2:
;; The same, but with tracing info

;; Trace functions

(defvar *object nil)

(defun *show ()
  (insert (format "\n%S" *object)))

(defun *setcar (cell-up newcar)
  (cond ((null cell-up))
	((listp cell-up) (setcar cell-up newcar))
	((symbolp cell-up) (set cell-up newcar)))
  newcar)

(defun *setcdr (cell-left newcdr)
  (cond ((null cell-left))
	((listp cell-left) (setcdr cell-left newcar))
	((symbolp cell-left) (set cell-left newcar)))
  newcdr)

(defun *add-mark-horizontal    (cell-left cell-right)
  (let ((cell-temp (cons '_ cell-right)))
    (*setcdr cell-left cell-temp)
    cell-temp))
(defun *remove-mark-horizontal (cell-left cell-right)
  (*setcdr cell-left cell-right))

(defun *add-mark-vertical    (cell-up cell-down)
  (let ((cell-temp (cons '_ cell-down)))
    (*setcar cell-up cell-temp)
    cell-temp))
(defun *remove-mark-vertical (cell-up cell-down)
  (*setcar cell-up cell-down)))



;; 

(defun **eval-exprs (cell-left exprs)
  (*add-mark-horizontal cell-left exprs)
  (*show)
  (if exprs
      (progn
	(**eval-expr 
	(*remove-mark-horizontal cell-left exprs)


(null exprs)


      (let ((expr (car exprs))
	    (rest (cdr exprs)))
	(*setcdr cell-left (cons (*eval-expr expr
      (*setcdr cell-left (cons (*eval-expr



  (**setcdr cell-left (cons '_ exprs))
  (**show)
  (**setcdr cell-left
	    (if exprs (let* ((head (car exprs))
			     (rest (cdr exprs)))
			(cons (**eval-expr head)
			      (**eval-exprs rest)))))






(defun *eval-exprs (expr



(*setcar parent (cons '_ expr))


;; em *eval-listexpr
(defun *eval-listexpr (expr &optional cell-up)
  (let ((head (car expr))
	(rest (cdr expr)))
    (*setcar cell-up (cons '_ expr))
    (*show)







(setq *object '(* (+ 1 2) (+ 3 4)))
(*eval-listexpr *object '*object)



  (if cell-up (setcar cell-up newcar)))
(defun *setcdr (cell-left newcdr)
  (if cell-up (setcar cell-up newcar)))
    

(


(*setcdr cell-left 

(*setcdr cell-left (cons result (cons




_ (+ 1 2)
_ (_ + 1 2)
_ (+ _ 1 2)
_ (+ 1 _ 2)
_ (+ 1 _ 2)


(* (+ 1 2) (+ 3 4))
(_ * (+ 1 2) (+ 3 4))
(* _ (+ 1 2) (+ 3 4))
(* _ (_ + 1 2) (+ 3 4))
(* _ (+ _ 1 2) (+ 3 4))
(* _ (+ 1 _ 2) (+ 3 4))
(* _ (+ 1 2 _) (+ 3 4))
(* 3 _ (+ 3 4))
(* 3 _ (_ + 3 4))
(* 3 _ (+ _ 3 4))
(* 3 _ (+ 3 _ 4))
(* 3 _ (+ 3 4 _))
(* 3 7 _)
21


(_ square (+ 1 2))
(_ (lambda (x) (* x x)) (+ 1 2))
((lambda (x) (* x x)) _ (+ 1 2))
((lambda (x) (* x x)) _ (_ + 1 2))
((lambda (x) (* x x)) _ (+ _ 1 2))
((lambda (x) (* x x)) _ (+ 1 _ 2))
((lambda (x) (* x x)) _ (+ 1 2 _))
((lambda (x) (* x x)) 3 _)
(let ((x 3)) _ (* x x))
(let ((x 3)) _ (_ * x x))
(let ((x 3)) _ (* _ x x))
(let ((x 3)) _ (* 3 _ x))
(let ((x 3)) _ (* 3 3 _))
(let ((x 3)) 9 _)
9



contexto como bloquinhos

outros modos: substituição de variáveis livres
comparação com lambda-cálculo



# (find-node "(/usr/share/info/elisp)Sequence Functions")



;; Local Variables:
;; coding:               raw-text-unix
;; ee-delimiter-hash:    "\n#\n"
;; ee-delimiter-percent: "\n%\n"
;; ee-anchor-format:     "«%s»"
;; ee-comment-prefix:    ";;"
;; modes:                (emacs-lisp-mode fundamental-mode)
;; End:
