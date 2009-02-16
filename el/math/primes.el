;;; primes.el --- prime number support for Emacs Lisp library code

;; Copyright (C) 1999
;;        Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 27 February 1999
;; Version: 1.00
;; Keywords: prime numbers, primality testing

;; This file is part of GNU Emacs.

;;; Commentary:

;; A prime number is any integer greater than one which has no exact
;; integer divisors other than one and itself.
;;
;; Prime numbers have increasingly important practical applications
;; in cryptography, and are also useful in hashing, besides being of
;; fundamental importance in number theory.
;;
;; This is a small collection of functions for:
;;
;;	* testing integers for primality,
;;	* generating nearby primes,
;;	* finding the n-th prime,
;;	* generating lists of primes in a given range,
;;	* factoring a number into a product of primes,
;;	* finding the greatest common divisor of two numbers, and
;;	* finding the least common multiple of two numbers.
;;
;; The functions provided are:
;;
;;	(gcd n m)			[cost: O((12(ln 2)/pi^2)ln n)]
;;	(lcm n m)			[cost: O((12(ln 2)/pi^2)ln n)]
;;	(next-prime n)			[cost: O(sqrt(N))]
;;	(nth-prime n)			[cost: O(N*sqrt(N))]
;;	(prev-prime n)			[cost: O(sqrt(N))]
;;	(prime-factors n)		[cost: O(N)]
;;	(prime-p n)			[cost: O(sqrt(N))]
;;	(primes-between from to)	[cost: O((to - from + 1)*sqrt(N))]
;;	(this-or-next-prime n)		[cost: O(sqrt(N))]
;;	(this-or-prev-prime n)		[cost: O(sqrt(N))]
;;
;; The modest collection of functions here is likely to grow, and
;; perhaps may even be improved algorithmically.  The core of most of
;; these functions is the primality test, (prime-p N), whose running
;; time is O(sqrt(N)), which becomes excessive for large N.  Note that
;; sqrt(N) == 2^{(lg N)/2}, where lg N, the base-2 logarithm of N, is
;; the number of bits in N.  Thus O(sqrt(N)) means O(2^(bits in N)),
;; or O(10^(digits in N)).  That is, the running time increases
;; EXPONENTIALLY in the number of digits of N.
;;
;; Because knowledge of the cost of these functions may be critical to
;; the caller, each function's documentation string ends with a
;; bracketed cost estimate as a final paragraph.
;;
;; Faster algorithms capable of dealing with larger integers are
;; known.  For example, Maple V Release 5 (1997) implements a
;; probabilistic function, isprime(n), that is
;;
;;	``very probably'' prime - see Knuth ``The art of computer
;;	programming'', Vol 2, 2nd edition, Section 4.5.4, Algorithm P
;;	for a reference and H. Reisel, ``Prime numbers and computer
;;	methods for factorization''. No counter example is known and
;;	it has been conjectured that such a counter example must be
;;	hundreds of digits long.
;;
;; Robert Sedgewick also promises a fast prime test in Part 5 of his
;; book ``Algorithms in C'', not yet published at the time of writing
;; this in March 1999.
;;
;; Three algorithms for probabilistic primality tests for large
;; numbers are discussed in Bruce Schneier, ``Applied Cryptography'',
;; (Wiley, 1994, ISBN 0-471-59756-2), pp. 213--216.
;;
;; Other key references, described in more detail in the Emacs Lisp
;; Manual chapter for this library, include
;;
;;	Leonard M. Adleman, Algorithmic Number Theory --- The
;;	Complexity Contribution, Proc. 35th IEEE Symposium on the
;;	Foundations of Computer Science (FOCS'94), Shafi Goldwasser
;;	(Ed.), IEEE Computer Society Press (Silver Spring, MD),
;;	pp. 88--113, 1994, ISBN 0-8186-6582-3, ISSN 0272-5428.
;;
;;	Eric Bach and Jeffrey Shallit, Algorithmic Number Theory.
;;	Volume I: Efficient Algorithms, MIT Press (Cambridge, MA),
;;	1996, ISBN 0-262-02405-5.
;;
;;	Ronald L. Graham, Donald E. Knuth and Oren Patashnik, Concrete
;;	Mathematics, Addison-Wesley, Reading, MA, USA, 1989, ISBN
;;	0-201-14236-8.
;;
;;	Donald E. Knuth, Fundamental algorithms, The Art of Computer
;;	Programming, Volume 1, Third edition, Addison-Wesley (Reading,
;;	MA), 1997, ISBN 0-201-89683-4.
;;
;;	Donald E. Knuth, Seminumerical algorithms, The Art of Computer
;;	Programming, Volume 2, Third edition, Addison-Wesley (Reading,
;;	MA), 1997, ISBN 0-201-89684-2.
;;
;;	Steven S. Skiena, The Algorithm Design Manual, Springer-Verlag
;;	(New York, NY), 1998, ISBN 0-387-94860-0.
;;
;;; Code:

(provide 'primes)


(defconst primes-version "1.00"
  "Version number of primes library.")


(defconst primes-date "[27-Feb-1999]"
  "Revision date of primes library.")


(defun gcd (m n)
  "Return the Greatest Common Divisor of integers M and N, or nil if
they are invalid.

\[cost: O((12(ln 2)/pi^2)ln max(M,N)) == O(0.8427659... max(M,N))]"
  ;; For details of this 2300-year algorithm due to Euclid, see, e.g.
  ;; Ronald L. Graham, Donald E. Knuth and Oren Patashnik, ``Concrete
  ;; Mathematics'' (Addison-Wesley, Reading, MA, USA, 1989, ISBN
  ;; 0-201-14236-8), pp. 103--104.
  ;;
  ;; The complexity analysis is surprisingly difficult; see Donald
  ;; E. Knuth, ``Seminumerical algorithms, The Art of Computer
  ;; Programming, Volume 2'', third edition (Addison-Wesley, Reading,
  ;; MA, USA, 1997, ISBN 0-201-89684-2), pp. 356--373.

  (if (and (integerp m) (integerp n))	; check for integer args
      (progn
	(setq m (abs m))		; argument sign does not matter for gcd, so
	(setq n (abs n))		; force positive for the algorithm below
	(cond
	 ((and (= m 0) (= n 0)) 0)	; gcd(0,0) ==> 0 by definition, for convenience
	 ((and (= m 0) (> n 0)) n)	; gcd(0,n) ==> n
	 ((and (> m 0) (= n 0)) m)	; gcd(m,0) ==> m
	 ((and (> m n) (> n 0)) (gcd n m)) ; reinvoke with reversed arguments
	 (t (gcd (% n m) m))))		; else reduce recursively
    nil))				; non-integer args: invalid


(defun lcm (m n)
  "Return the Least Common Multiple of integers M and N, or nil if
they are invalid, or the result is not representable (e.g., the
product M*N overflows).

\[cost: O((12(ln 2)/pi^2)ln max(M,N)) == O(0.8427659... max(M,N))]"
  (cond
   ((and (integerp m) (integerp n))	; check for integer args
    (let ((mn) (the-gcd) (the-lcm))
      (if (or (= m 0) (= n 0))		; fast special case: lcm(0,anything) == 0
	  0
	;; else compute lcm from (m * n) / gcd(m,n)
	;;
	;; Problem: GNU Emacs Lisp integer multiply does not detect or
	;; trap overflow, which is a real possibility here, and it lacks
	;; a double-length integer type to represent the product m * n.
	;; Since the lcm may still be representable, we do the
	;; intermediate computation in (double-precision)
	;; floating-point, which is still not quite large enough to
	;; represent all products of Emacs 28-bit integers stored in
	;; 32-bit words, then convert back to integer results.  The
	;; floor function will signal an error if the result is not
	;; representable.  To try to avoid that, we first check that the
	;; equality gcd * lcm = m * n is satisfied, and only if it is,
	;; do we invoke floor.
	;;
	;; TO DO: find better algorithm without these undesirable
	;; properties.
	(setq m (abs m))		; argument sign does not matter for lcm, so
	(setq n (abs n))		; force positive for the algorithm below
	(setq the-gcd (gcd m n))
	(setq mn (* (float m) (float n)))
	(setq the-lcm (/ mn the-gcd))
	(if (= (* the-gcd the-lcm) mn)	; then got correct answer
	    (floor the-lcm)
	  nil))))			; else out-of-range or invalid
    (t nil)))				; non-integer args: invalid


(defun prime-factors (n)
  "Return a list of prime factors of N.

If N is prime, there are no factors, except the trivial one of N itself,
so the return value is the list (N).  Thus, if (length (prime-factors N))
is 1, N is prime.

Otherwise, if N is not an integer greater than 1, the return value is
nil, equivalent to an empty list.

\[cost: O(N)]"
  (interactive)
  (let ((result-list nil)
	(n-original n))
    (if (and (integerp n) (> n 1))
	(let ((limit (/ n 2))
	      (divisor 2))
	  (while (<= divisor limit)
	    ;; To correctly handle factors of multiplicity > 1, we must
	    ;; be careful to advance the divisor only when it is not a
	    ;; factor!  When n is replaced by n/divisor, we can reset
	    ;; limit, but only to n/2, not to n.  Consider
	    ;; (prime-factors 15): the first factor found is 3, which
	    ;; reduces n to 5, which will be the next prime factor
	    ;; found, but would be lost if we reset limit to 5/2 == 2.
	    ;;
	    ;; If this divisor is rejected, as long as it is greater
	    ;; than 2, and thus, odd, we can step it by 2, halving the
	    ;; number of loop iterations.
	    (if (= 0 (% n divisor))
		(setq n (/ n divisor)
		      limit n
		      result-list (append result-list (list divisor)))
	      (if (= divisor 2)
		  (setq divisor 3)
		(setq divisor (+ divisor 2)))))
	  ;; If we end the while loop with an empty result-list, then
	  ;; the input N was prime, so set result-list to a one-element
	  ;; list:
	  (if (null result-list)
	      (setq result-list (list n-original)))))
    result-list))


(defun prime-p (n)
  "Return N if it is a prime number, else nil.

Because Emacs integers are usually more limited in size than the host
word size would suggest, e.g.,

	\[-2^{27}, 2^{27} - 1] == [-134217728, 134217727]

on a 32-bit machine, avoid passing excessively large integers to this
function, otherwise you may experience this failure:

	\(next-prime 134217689)
	Arithmetic domain error: \"sqrt\", -134217728.0

While you may be able to use larger integers on some 64-bit machines,
the required run time for this function is then likely to be excessive.

\[cost: O(sqrt(N))]"
  (interactive)
  (if (integerp n)
      (cond ((< n 2) nil)		;there are no primes below 2
	    ((= n 2) 2)			;special case for the only even prime
	    ((= 0 (% n 2)) nil)		;there are no other even primes
	    (t (catch 'RESULT		;else we have a positive odd candidate value
		 (let ((limit (floor (sqrt n)))
		       (divisor 2))
		   (while (<= divisor limit)
		     (if (= 0 (% n divisor))
			 (throw 'RESULT nil))
		     (setq divisor (1+ divisor)))
		   n))))
    nil))


(defun next-prime (n)
  "Return the next prime number after N, or else nil.

\[cost: O(sqrt(N))]"
  (if (integerp n)
    (cond
     ((> n 1)
      (let* ((k (if (= 0 (% n 2));start k at next odd number after n
		    (1+ n)
		  (+ n 2))))
	(while (not (prime-p k)) ;and loop upward over odd numbers
	  (setq k (+ k 2)))
	k))
     (t 2))
    nil))


(defun nth-prime (n)
  "Return the N-th prime number, or else nil.

The first prime number is 2.

\[cost: O(N*sqrt(N))]"
  (if (integerp n)
      (cond
       ((<= n 0) nil)			;non-positive args are invalid
       ((= n 1) 2)			;special case: only even prime
       (t (let ((k 1))			;n > 1, so test only odd values 3, 5, ...
	    (while (> n 1)
	      (setq k (+ k 2))
	      (if (prime-p k)		;count down each prime found
		  (setq n (1- n))))
	    k)))			;at loop exit, k is the desired nth-prime
    nil))


(defun prev-prime (n)
  "Return the prime number before (i.e., less than) N, or else nil.

\[cost: O(sqrt(N))]"
  (if (integerp n)
      (cond ((<= n 2) nil)		;invalid: there are no primes before 2
	    ((= n 3) 2)			;special case: 2 is the only even prime
	    (t				;n > 3
	     (let* ((k (if (= 0 (% n 2)) ;start k at prev odd number before n
			   (1- n)
			 (- n 2))))
	       (while (not (prime-p k))	;and loop downward over odd numbers
		 (setq k (- k 2)))
	       k)))
    nil))


(defun primes-between (from to)
  "Return a list of prime numbers between FROM and TO, inclusive, else nil.

\[cost: O((to - from + 1)*sqrt(N)/2)]"
  (if (and (integerp from) (integerp to))
      (let ((k)
	    (primes '()))
	(if (and (<= from 2) (<= 2 to))	;handle special case of only even prime
	    (setq primes '(2)))
	(setq k 3)
	;; k is now odd, so we can loop over odd numbers only
	(while (<= k to)
	  (if (prime-p k)
	      (setq primes (append primes (list k))))
	  (setq k (+ k 2)))
	primes)				;final successful list
    nil))				;else invalid arguments


(defsubst this-or-next-prime (n)
  "Return N if it is prime, else return the next prime number after N,
else nil in N is invalid.

\[cost: O(sqrt(N))]"
  (or (prime-p n)
      (next-prime n)))


(defsubst this-or-prev-prime (n)
  "Return N if it is prime, else return the prime number before
(i.e., less than) N.

\[cost: O(sqrt(N))]"
  (or (prime-p n)
      (prev-prime n)))

;;; primes.el ends here
