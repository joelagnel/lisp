;;; Solution to sample aliquot series problem, 93/94 Southern California
;;; ACM collegiate programming contest.  Took about 30 minutes.

(defvar max-prime 2000)
(defvar primes
  (let* ((sofar '(2)) (tail sofar) (n 3))
    (while (<= n max-prime)
      (if (catch 'prime
	    (mapcar
	     (lambda (q) 
	       (if (zerop (% n q)) (throw 'prime nil))
	       (if (< n (* q q)) (throw 'prime t)))
	     sofar))
	  (progn
	    (setcdr tail (list n))
	    (setq tail (cdr tail))))
      (setq n (+ n 2)))
    sofar)
  "List of primes up to `max-prime', computed at load time.")


(defun factor (n)
  "Return non-decreasing list of prime factors of N."
  (let ((pl primes) (ret) p)
    (while (> n 1)
      (if pl
	  (setq p (car pl) pl (cdr pl))
	(error "factor: factors of %d exceed max-prime" n))
      (if (< n (* p p))
	  (setq ret (cons n ret) n 1)
	(while (zerop (% n p)) (setq ret (cons p ret) n (/ n p)))))
    (nreverse ret)))
;; Note, sieve can handle single prime factors up to `max-prime' squared:
;; (factor 123123129)

(defun sigma (n)
  "Compute sum of divisors of N, including N itself."
  (let ((fl (factor n)) (ret 1) p pe)
    (while fl
      (setq p (car fl) fl (cdr fl) pe (* p p))
      (while (eq p (car fl))
	(setq fl (cdr fl) pe (* p pe)))
      (setq ret (* ret (/ (1- pe) (1- p)))))
    ret))
;; (factor (sigma 36))
	    
(defun s (n)
  "Compute sum of divisors of N, excluding N itself."
  (- (sigma n) n))
	     
(defun aliquot (n &optional max return-sequence)
  "Classify the aliquot sequence a[0] := N, a[k+1] := s(a[k])."
  (or max (setq max 10))
  (let ((k 0) (ak n) previous done)
    (while (not done)
      (cond
       ((equal ak 1)
	;; so a[k+1]=0:
	(setq done (format "%d" (1+ k))))
       ((memq ak previous)
	(setq done
	      (let* ((initial (1- (length (memq ak previous))))
		     (period (- k initial)))
		(cond
		 ((not (zerop initial))
		  (format "%d+%d" initial period))
		 ;; no initial sequence:
		 ((eq period 1) "perfect")
		 ((eq period 2) "pair")
		 (t (format "+%d" period))))))
       ((>= k max) (setq done (format "> %d" k)))
       (t (setq previous (cons ak previous)
		ak (s ak)
		k (1+ k)))))
    (if return-sequence
	(nreverse (cons ak previous))
      (message done))
    ))

;; (mapcar 'aliquot '(30 37 220 6 25 1184 12496 2950)) ->
;; ("> 10" "2" "pair" "perfect" "1+1" "pair" "+5" "5+2")
;; (aliquot 60 10) -> "11" [exceeds max!]
;; (aliquot 30 50 t) -> (30 42 54 66 78 90 144 259 45 33 15 9 4 3 1)
;; (aliquot 123542 30 t) -> (123542 63274 37274 18640 24884 18670
;; 14954 7480 11960 18280 22940 28132 24984 42876 68564 53824 56793
;; 25863 9705 5847 1953 1375 497 79 1)
