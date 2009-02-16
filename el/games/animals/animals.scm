;#!/usr/local/bin/guile -s
;!#
;;; File: <animals.scm - 1998-05-21 Thu 14:37:32 EDT sds@mute.eaglets.com>
;;; $Id: animals.scm,v 1.1 2000/12/31 04:33:15 sds Exp $
;;; $Source: /usr/local/cvs/sds/lisp/animals.scm,v $
;;;
;;; Copyright (C) 1997 by Sam Steingold
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.

;; for stalin: uncomment `define getenv'; comment out `force-output'
;; for guile: comment out `define getenv'; uncomment `force-output'

;(debug-enable 'backtrace)
;; for MIT-Scheme
;; (restart 1) (load "/home/sds/lisp/animals.scm")
;; (declare (usual-integrations))
;; (if (symbol-bound? #f 'force-output) ()
;;     (define force-output flush-output))
;; (define get-string prompt-for-expression)
;; (define y-or-n? prompt-for-confirmation)

(define animals-debug-output #t)
(define animals-debug-use-built-in-data #f)
(define animals-default-data '("Is it an insect" ("Can it sting" "an ant" .
			       "a fly") "Can it fly" "a duck" . "a penguin"))
(define animals-data #f)
(define animals-port #f)
(define animals-file-name #f)

;(define getenv (foreign-procedure (char*) char* "getenv"))

;; input the initial data
(cond (animals-debug-use-built-in-data
       (set! animals-data animals-default-data))
      (else
       (set! animals-file-name
	     (or (getenv "ANIMALS")
		 (string-append (getenv "HOME") "/lisp/animals")))
       (display (string-append "Using animals file: `"
			       animals-file-name "'\n"))

       (set! animals-port (open-input-file animals-file-name))
       (set! animals-data (read animals-port))
       (cond ((not (pair? animals-data))
	      (set! animals-data animals-default-data)
	      (display "empty or incorrect file. using the default data.\n")))
       (close-input-port animals-port)))
(define animals-data-modified #f)

;;(with-input-from-file animals-file-name (define animals-data (read)))

;; the actual code
;; UI functions, to be replaced with something better
(cond (animals-debug-output
       (display "animals-data now: ")
       (write animals-data)
       (newline)))

(if animals-debug-output (display "defining `y-or-n?'..."))
(define (y-or-n? prompt)	; Ask an y/n question.
  (do ((res '()) (pf "") (nn 1 (1+ nn))) ((boolean? res) res)
    (display (string-append pf prompt " (y or n) ")) (force-output)
    (do ((ch (read-char)))
        ((not (char-ready? (current-input-port)))
         (cond ((char-ci=? ch #\y) (set! res #t))
               ((char-ci=? ch #\n) (set! res #f))
               (else (set! pf (string-append
                               "[" (number->string nn)
                               "] The answer must be `y' or `n'. ")))))
      (read-char))))
(if animals-debug-output (display "done\n"))

;;; from mit-scm reference manual:
;;(define (read-string char-set input-port)
;;  (let ((char (peek-char input-port)))
;;    (if (eof-object? char) char
;;	(list->string
;;	 (let loop ((char char))
;;	   (if (or (eof-object? char) (char-set-member? char-set char)) '()
;;	       (begin
;;		 (read-char input-port)
;;		 (cons char (loop (peek-char input-port))))))))))

(if animals-debug-output (display "defining `get-string'..."))
(define (get-string prompt)	; Ask a question, return the full answer.
  (display (string-append prompt "\n\t-->")) (force-output)
  (do ((lst '() (cons (read-char) lst)))
      ((and (not (null? lst)) (char=? (car lst) #\newline))
       (list->string (reverse (cdr lst))))))
(if animals-debug-output (display "done\n"))

;; algorithm

;; from qobi 1998-05-07 Thu 17:32:58 EDT
(define (beginning-positions string1 string2)
 (let ((m (string-length string1))
       (n (string-length string2)))
  (let loop ((i 0) (c '()))
   (cond ((> i (- n m)) c)
         ((string=? string1 (substring string2 i (+ i m)))
          (loop (+ i 1) (cons i c)))
         (else (loop (+ i 1) c))))))

(define (has-it string) ;; Return (beginning . end) of `it' in STRING, or #f.
  (let ((poss (beginning-positions "it" string)))
    (if (null? poss) #f (cons (car poss) (+ 2 (car poss))))))

;;(define anml-it-re (make-regexp "it"))
;;(define (has-it string) ;; Return (beginning . end) of `it' in STRING, or #f.
;;  (let ((ma (regexp-match? anml-it-re string)))
;;    (if ma (vector-ref ma 0) #f)))

(define (anml-get-question old new)
  ;; Get the question that will distinguish betwee OLD and NEW.
  ;; The question will contain `it'.
  (do ((quest #f (if (has-it quest) quest
		     (begin
		       (display "The question must contain `it'.\n") #f))))
      (quest quest)
    (set! quest (get-string (string-append
			     "What Yes/No question distinguishes between "
			     old " and " new "?")))))

(if animals-debug-output (display "defining `anml-finish'..."))

(define (add-article str)
  (if (or (string-ci=? "a " (substring str 0 2))
	  (string-ci=? "an " (substring str 0 3))
	  (string-ci=? "the " (substring str 0 4)))
      str
      (string-append (if (member (string-ref str 0) '(#\a #\e #\i #\o #\u))
			 "an " "a ") str)))

(define (anml-finish tail)
  (cond ((y-or-n? (string-append "Is it " tail "?"))
	 (display "I won!\n") tail)
	(else
	 (let* ((new (add-article
		      (get-string "I lost...\nWhat was your animal?...")))
		(quest (anml-get-question tail new)) (res (has-it quest)))
	   (set! animals-data-modified #t)
	   (if animals-debug-output
	       (display (string-append "\nquestion: " quest "\ntail: "
				       tail "\nnew: " new "\n")))
	   (if (y-or-n? (string-append (substring quest 0 (car res)) tail
				       (substring quest (cdr res)
						  (string-length quest)) "?"))
	       (cons quest (cons tail new))
	       (cons quest (cons new tail)))))))

(if animals-debug-output (display "done\nstarting the main loop...\n"))

(define anml-root animals-data)
(define anml-follow #f)
(define anml-continue #t)

(do () ((not anml-continue))
  (set! anml-root animals-data)
  ;; (set! anml-follow #f)
  (do () ((not (pair? anml-root)))
    (set! anml-follow anml-root)
    (set! anml-root (if (y-or-n? (car anml-root))
			(cadr anml-root) (cddr anml-root))))
  (set-cdr! (or anml-follow animals-data)
	    (if (eq? anml-root (cadr anml-follow))
		(cons (anml-finish anml-root) (cddr anml-follow))
		(cons (cadr anml-follow) (anml-finish anml-root))))
  (cond (animals-debug-output
	 (display "animals-data now: ")
	 (write animals-data)
	 (newline)))
  (set! anml-continue (not (y-or-n? "Quit?"))))

(if animals-debug-output (display "done - saving the information...\n"))
(cond (animals-debug-use-built-in-data
       (display "used built-in data -- no save!\n"))
      (animals-data-modified
       (set! animals-port (open-output-file animals-file-name))
       (write animals-data animals-port)
       (newline animals-port)
       (close-output-port animals-port)
       (display (string-append "Wrote file `" animals-file-name "'\n")))
      (else (display "You taught me no new animals this time...\n")))

;;(with-output-to-file animals-file-name (write animals-data) (newline))
