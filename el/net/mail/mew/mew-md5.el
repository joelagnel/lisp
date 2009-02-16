;;; mew-md5.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: May 22, 2000

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic variables
;;;

(defvar mew-md5-mask 65535 "Mask for 16bit.")

(defvar mew-md5-ctx nil "MD5 context.")

(defconst mew-md5-init-ctx
  [(26437 . 8961) (61389 . 43913) (39098 . 56574) (4146 . 21622)]
  "MD5 initial context.")

(defconst mew-md5-Sa  7)
(defconst mew-md5-Sb 12)
(defconst mew-md5-Sc 17)
(defconst mew-md5-Sd 22)
(defconst mew-md5-Se  5)
(defconst mew-md5-Sf  9)
(defconst mew-md5-Sg 14)
(defconst mew-md5-Sh 20)
(defconst mew-md5-Si  4)
(defconst mew-md5-Sj 11)
(defconst mew-md5-Sk 16)
(defconst mew-md5-Sl 23)
(defconst mew-md5-Sm  6)
(defconst mew-md5-Sn 10)
(defconst mew-md5-So 15)
(defconst mew-md5-Sp 21)

(defconst mew-md5-T
  [(0 . 0)
   (55146 . 42104) (59591 . 46934) ( 9248 . 28891) (49597 . 52974)
   (62844 .  4015) (18311 . 50730) (43056 . 17939) (64838 . 38145)
   (27008 . 39128) (35652 . 63407) (65535 . 23473) (35164 . 55230)
   (27536 .  4386) (64920 . 29075) (42617 . 17294) (18868 .  2081)
   (63006 .  9570) (49216 . 45888) ( 9822 . 23121) (59830 . 51114)
   (54831 .  4189) (  580 .  5203) (55457 . 59009) (59347 . 64456)
   ( 8673 . 52710) (49975 .  2006) (62677 .  3463) (17754 .  5357)
   (43491 . 59653) (64751 . 41976) (26479 .   729) (36138 . 19594)
   (65530 . 14658) (34673 . 63105) (28061 . 24866) (64997 . 14348)
   (42174 . 59972) (19422 . 53161) (63163 . 19296) (48831 . 48240)
   (10395 . 32454) (60065 . 10234) (54511 . 12421) ( 1160 .  7429)
   (55764 . 53305) (59099 . 39397) ( 8098 . 31992) (50348 . 22117)
   (62505 .  8772) (17194 . 65431) (43924 .  9127) (64659 . 41017)
   (25947 . 22979) (36620 . 52370) (65519 . 62589) (34180 . 24017)
   (28584 . 32335) (65068 . 59104) (41729 . 17172) (19976 .  4513)
   (63315 . 32386) (48442 . 62005) (10967 . 53947) (60294 . 54161)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic functions
;;;

(defun mew-md5-plus (&rest nums)
  "A function to add dot-pairs of 16bit-number."
  (let ((x 0) (y 0))
    (mapcar (lambda (x-y) (setq x (+ x (car x-y))) (setq y (+ y (cdr x-y))))
	    nums)
    (setq x (logand (+ x (lsh y -16)) mew-md5-mask))
    (setq y (logand y mew-md5-mask))
    (cons x y)))

(defun mew-md5-shift (num shift)
  "32bit shift function for dot-pairs of 16bit-number."
  (let (x y)
    (if (<= shift 15) ;; xxx
	(setq x (car num) y (cdr num))
      (setq x (cdr num) y (car num))
      (setq shift (- shift 16)))
    (cons
     (logand (logior (lsh x shift) (lsh y (- shift 16))) mew-md5-mask)
     (logand (logior (lsh y shift) (lsh x (- shift 16))) mew-md5-mask))))

(defun mew-md5-F (X Y Z)
  "The function F for dot-pairs of 16bit-number."
  (let ((x1 (car X)) (x2 (cdr X))
	(y1 (car Y)) (y2 (cdr Y))
	(z1 (car Z)) (z2 (cdr Z)))
    (cons
     (logand (logior (logand x1 y1) (logand (lognot x1) z1)) mew-md5-mask)
     (logand (logior (logand x2 y2) (logand (lognot x2) z2)) mew-md5-mask))))

(defun mew-md5-G (X Y Z)
  "The function G for dot-pairs of 16bit-number."
  (let ((x1 (car X)) (x2 (cdr X))
	(y1 (car Y)) (y2 (cdr Y))
	(z1 (car Z)) (z2 (cdr Z)))
    (cons
     (logand (logior (logand x1 z1) (logand y1 (lognot z1))) mew-md5-mask)
     (logand (logior (logand x2 z2) (logand y2 (lognot z2))) mew-md5-mask))))

(defun mew-md5-H (X Y Z)
  "The function H for dot-pairs of 16bit-number."
  (let ((x1 (car X)) (x2 (cdr X))
	(y1 (car Y)) (y2 (cdr Y))
	(z1 (car Z)) (z2 (cdr Z)))
    (cons
     (logand (logxor x1 y1 z1) mew-md5-mask)
     (logand (logxor x2 y2 z2) mew-md5-mask))))

(defun mew-md5-I (X Y Z)
  "The function I for dot-pairs of 16bit-number."
  (let ((x1 (car X)) (x2 (cdr X))
	(y1 (car Y)) (y2 (cdr Y))
	(z1 (car Z)) (z2 (cdr Z)))
    (cons
     (logand (logxor y1 (logior x1 (lognot z1))) mew-md5-mask)
     (logand (logxor y2 (logior x2 (lognot z2))) mew-md5-mask))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Round functions
;;;
;;; X must be bound to the target string.

(defun mew-md5-round1 (a b c d k s i X)
  "The function ROUND1 for dot-pairs of 16bit-number."
  (let ((T mew-md5-T))
    (setq a (mew-md5-plus a (mew-md5-F b c d) (aref X k) (aref T i)))
    (setq a (mew-md5-shift a s))
    (mew-md5-plus b a)))

(defun mew-md5-round2 (a b c d k s i X)
  "The function ROUND2 for dot-pairs of 16bit-number."
  (let ((T mew-md5-T))
    (setq a (mew-md5-plus a (mew-md5-G b c d) (aref X k) (aref T i)))
    (setq a (mew-md5-shift a s))
    (mew-md5-plus b a)))

(defun mew-md5-round3 (a b c d k s i X)
  "The function ROUND3 for dot-pairs of 16bit-number."
  (let ((T mew-md5-T))
    (setq a (mew-md5-plus a (mew-md5-H b c d) (aref X k) (aref T i)))
    (setq a (mew-md5-shift a s))
    (mew-md5-plus b a)))

(defun mew-md5-round4 (a b c d k s i X)
  "The function ROUND4 for dot-pairs of 16bit-number."
  (let ((T mew-md5-T))
    (setq a (mew-md5-plus a (mew-md5-I b c d) (aref X k) (aref T i)))
    (setq a (mew-md5-shift a s))
    (mew-md5-plus b a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MD5 functions
;;;

(defun mew-md5 (str)
  "A function to cultrate an MD5 checksum of given a STRing.
STR MUST be small enough (e.g. less than 4096)."
  (mew-md5-init)
  (mew-md5-loop str)
  (mew-md5-result))

(defun mew-md5-str64-to-table (str n)
  "A function to create a vector of dot-pairs of 16bit-number from
substring of STR, whose length is 64 byte from the position N."
  (let ((i n) (j 0) (table (make-vector 16 nil)))
    (while (< j 16)
      (aset 
       table j
       (cons
	(+ (* (aref str (+ i 3)) 256) (aref str (+ i 2)))
	(+ (* (aref str (+ i 1)) 256) (aref str i))))
      (setq i (+ i 4))
      (setq j (1+ j)))
    table))

(defun mew-md5-init ()
  "MD5 initialize function."
  (setq mew-md5-ctx (copy-sequence mew-md5-init-ctx)))

(defun mew-md5-loop (str)
  "MD5 update function."
  (let* ((len (length str))
	 (blen (* len 8))
	 (n (/ len 64))
	 (r (% len 64))
	 (i 0) (j 0) pad)
    (while (< i n)
      (mew-md5-calc (mew-md5-str64-to-table str j))
      (setq i (1+ i))
      (setq j (+ j 64)))
    ;; 71 = 64 + 8 - 1
    (setq pad (concat (substring str j) "\200" (make-string 71 0)))
    (cond
     ((<= r 55)
      (setq n 1)
      (aset pad 56 (% blen 256))
      (aset pad 57 (/ blen 256)))
     (t 
      (setq n 2)
      (aset pad 120 (% blen 256))
      (aset pad 121 (/ blen 256))))
    (setq i 0)
    (setq j 0)
    (while (< i n)
      (mew-md5-calc (mew-md5-str64-to-table pad j))
      (setq i (1+ i))
      (setq j (+ j 64)))))

(defun mew-md5-calc (X)
  "MD5 calculation function."
  (let ((A (aref mew-md5-ctx 0))
	(B (aref mew-md5-ctx 1))
	(C (aref mew-md5-ctx 2))
	(D (aref mew-md5-ctx 3)))
    (setq A (mew-md5-round1 A B C D  0 mew-md5-Sa  1 X))
    (setq D (mew-md5-round1 D A B C  1 mew-md5-Sb  2 X))
    (setq C (mew-md5-round1 C D A B  2 mew-md5-Sc  3 X))
    (setq B (mew-md5-round1 B C D A  3 mew-md5-Sd  4 X))
    (setq A (mew-md5-round1 A B C D  4 mew-md5-Sa  5 X))
    (setq D (mew-md5-round1 D A B C  5 mew-md5-Sb  6 X))
    (setq C (mew-md5-round1 C D A B  6 mew-md5-Sc  7 X))
    (setq B (mew-md5-round1 B C D A  7 mew-md5-Sd  8 X))
    (setq A (mew-md5-round1 A B C D  8 mew-md5-Sa  9 X))
    (setq D (mew-md5-round1 D A B C  9 mew-md5-Sb 10 X))
    (setq C (mew-md5-round1 C D A B 10 mew-md5-Sc 11 X))
    (setq B (mew-md5-round1 B C D A 11 mew-md5-Sd 12 X))
    (setq A (mew-md5-round1 A B C D 12 mew-md5-Sa 13 X))
    (setq D (mew-md5-round1 D A B C 13 mew-md5-Sb 14 X))
    (setq C (mew-md5-round1 C D A B 14 mew-md5-Sc 15 X))
    (setq B (mew-md5-round1 B C D A 15 mew-md5-Sd 16 X))
    (setq A (mew-md5-round2 A B C D  1 mew-md5-Se 17 X))
    (setq D (mew-md5-round2 D A B C  6 mew-md5-Sf 18 X))
    (setq C (mew-md5-round2 C D A B 11 mew-md5-Sg 19 X))
    (setq B (mew-md5-round2 B C D A  0 mew-md5-Sh 20 X))
    (setq A (mew-md5-round2 A B C D  5 mew-md5-Se 21 X))
    (setq D (mew-md5-round2 D A B C 10 mew-md5-Sf 22 X))
    (setq C (mew-md5-round2 C D A B 15 mew-md5-Sg 23 X))
    (setq B (mew-md5-round2 B C D A  4 mew-md5-Sh 24 X))
    (setq A (mew-md5-round2 A B C D  9 mew-md5-Se 25 X))
    (setq D (mew-md5-round2 D A B C 14 mew-md5-Sf 26 X))
    (setq C (mew-md5-round2 C D A B  3 mew-md5-Sg 27 X))
    (setq B (mew-md5-round2 B C D A  8 mew-md5-Sh 28 X))
    (setq A (mew-md5-round2 A B C D 13 mew-md5-Se 29 X))
    (setq D (mew-md5-round2 D A B C  2 mew-md5-Sf 30 X))
    (setq C (mew-md5-round2 C D A B  7 mew-md5-Sg 31 X))
    (setq B (mew-md5-round2 B C D A 12 mew-md5-Sh 32 X))
    (setq A (mew-md5-round3 A B C D  5 mew-md5-Si 33 X))
    (setq D (mew-md5-round3 D A B C  8 mew-md5-Sj 34 X))
    (setq C (mew-md5-round3 C D A B 11 mew-md5-Sk 35 X))
    (setq B (mew-md5-round3 B C D A 14 mew-md5-Sl 36 X))
    (setq A (mew-md5-round3 A B C D  1 mew-md5-Si 37 X))
    (setq D (mew-md5-round3 D A B C  4 mew-md5-Sj 38 X))
    (setq C (mew-md5-round3 C D A B  7 mew-md5-Sk 39 X))
    (setq B (mew-md5-round3 B C D A 10 mew-md5-Sl 40 X))
    (setq A (mew-md5-round3 A B C D 13 mew-md5-Si 41 X))
    (setq D (mew-md5-round3 D A B C  0 mew-md5-Sj 42 X))
    (setq C (mew-md5-round3 C D A B  3 mew-md5-Sk 43 X))
    (setq B (mew-md5-round3 B C D A  6 mew-md5-Sl 44 X))
    (setq A (mew-md5-round3 A B C D  9 mew-md5-Si 45 X))
    (setq D (mew-md5-round3 D A B C 12 mew-md5-Sj 46 X))
    (setq C (mew-md5-round3 C D A B 15 mew-md5-Sk 47 X))
    (setq B (mew-md5-round3 B C D A  2 mew-md5-Sl 48 X))
    (setq A (mew-md5-round4 A B C D  0 mew-md5-Sm 49 X))
    (setq D (mew-md5-round4 D A B C  7 mew-md5-Sn 50 X))
    (setq C (mew-md5-round4 C D A B 14 mew-md5-So 51 X))
    (setq B (mew-md5-round4 B C D A  5 mew-md5-Sp 52 X))
    (setq A (mew-md5-round4 A B C D 12 mew-md5-Sm 53 X))
    (setq D (mew-md5-round4 D A B C  3 mew-md5-Sn 54 X))
    (setq C (mew-md5-round4 C D A B 10 mew-md5-So 55 X))
    (setq B (mew-md5-round4 B C D A  1 mew-md5-Sp 56 X))
    (setq A (mew-md5-round4 A B C D  8 mew-md5-Sm 57 X))
    (setq D (mew-md5-round4 D A B C 15 mew-md5-Sn 58 X))
    (setq C (mew-md5-round4 C D A B  6 mew-md5-So 59 X))
    (setq B (mew-md5-round4 B C D A 13 mew-md5-Sp 60 X))
    (setq A (mew-md5-round4 A B C D  4 mew-md5-Sm 61 X))
    (setq D (mew-md5-round4 D A B C 11 mew-md5-Sn 62 X))
    (setq C (mew-md5-round4 C D A B  2 mew-md5-So 63 X))
    (setq B (mew-md5-round4 B C D A  9 mew-md5-Sp 64 X))
    (aset mew-md5-ctx 0 (mew-md5-plus A (aref mew-md5-ctx 0)))
    (aset mew-md5-ctx 1 (mew-md5-plus B (aref mew-md5-ctx 1)))
    (aset mew-md5-ctx 2 (mew-md5-plus C (aref mew-md5-ctx 2)))
    (aset mew-md5-ctx 3 (mew-md5-plus D (aref mew-md5-ctx 3)))))

(defun mew-md5-result ()
  "16byte-binary to hexadecimal function."
  (let* ((A (aref mew-md5-ctx 0))
	 (B (aref mew-md5-ctx 1))
	 (C (aref mew-md5-ctx 2))
	 (D (aref mew-md5-ctx 3))
	 (a1 (car A)) (a2 (cdr A))
	 (b1 (car B)) (b2 (cdr B))
	 (c1 (car C)) (c2 (cdr C))
	 (d1 (car D)) (d2 (cdr D)))
    (format "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x"
	    (% a2 256) (/ a2 256) (% a1 256) (/ a1 256)
	    (% b2 256) (/ b2 256) (% b1 256) (/ b1 256)
	    (% c2 256) (/ c2 256) (% c1 256) (/ c1 256)
	    (% d2 256) (/ d2 256) (% d1 256) (/ d1 256))))

(provide 'mew-md5)

;;; Copyright Notice:

;; Copyright (C) 1999-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-md5.el ends here
