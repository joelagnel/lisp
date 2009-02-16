;;; calc.el --- A simple calculator

;; Copyright (C) 1997-2000 Sean MacLennan
;; Revision:   1.5

;;{{{ License agrement

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;{{{ Commentary

;; This package implements a simple calculator. It understands most of
;; the 'C' operators and the order of operation. It also does hex to/from
;; decimal conversions.

;; The calculator is based on the operator-precedence parsing algorithm
;; from "Compilers Principles, Techniques, and Tools"
;; by Alfred V. Aho, Ravi Sethi, and Jeffery D. Ullman.

;;}}}

;;{{{ Installation

;; (require 'calc)

;;}}}

;;{{{ Code

(eval-when-compile
  (require 'stack-m))

(provide 'calc)

(defconst my-calc-f-values
  '((?*  . 12) (?/  . 12) (?% . 12)
    (?+  . 10) (?-  . 10)
    (?<  .  8) (?>  .  8)
    (?&  .  6) (?^  .  4) (?| .  2)
    (?\( .  0) (?\) . 14)
    (?N  . 14)			; number
    (?=  .  0))			; terminator
  "Values for the precedence function `f'.")

(defconst my-calc-g-values
  '((?*  . 11) (?/  . 11) (?% . 11)
    (?+  .  9) (?-  .  9)
    (?<  .  7) (?>  .  7)
    (?&  .  5) (?^  .  3) (?| .  1)
    (?\( . 13) (?\) .  0)
    (?N  . 13)			; number
    (?=  .  0))			; terminator
  "Values for the precedence function `g'.")

(defconst my-calc-commands
  '((?* . *) (?/ . /) (?% . mod)
    (?+ . +) (?- . -)
    (?< . ash) (?> . rash)
    (?& . logand) (?^ . logeor) (?| . logior))
  "Command char to lisp command")

(defvar nums (stack-create)
  "Internal stack of numbers")

(defun rash (a b)
  (ash a (- 0 b)))

(defun my-calc-f (cmd)
  "Precedence function `f'."
  (let ((lookup (cdr (assoc cmd my-calc-f-values))))
    (if lookup
	lookup
      (error "Syntax error"))))

(defun my-calc-g (cmd &optional number)
  "Precedence function `g'."
  ;; Only the `g' function is ever looking at a number.
  ;; It reads the number, pushes it on the nums stack,
  ;; and replaces the number with the token `N' in the buffer.
  (if (and number (setq number (my-calc-get-number)))
      (progn
	;; Push on nums stack
	(stack-push nums number)
	;; Replace number with token
	(replace-match "N")
	(forward-char -1)
	;; Lookup N below.
	(setq cmd ?N)))

  (let ((lookup (cdr (assoc cmd my-calc-g-values))))
    (if lookup
	lookup
      (error "Syntax error"))))

(defun my-calc-get-number ()
  "Read the next number in the buffer and returns the number or nil.
Handles hex/float/integer."
  (cond
   ;; Hex?
   ((looking-at "$?\\(0[xX]\\)\\([0-9a-fA-F]+\\)$?")
    (string-to-number (match-string 2) 16))
   ;; Integer/float?
   ((looking-at "$?\\([0-9]*\\.?[0-9]+\\)$?")
    (string-to-number (match-string 1)))
   ;; Negative hex?
   ;; The preceding token must be an op (not a number)
   ((and (looking-at "$?-\\(0[xX]\\)\\([0-9a-fA-F]+\\)$?")
	 (or (not (eq (preceding-char) ?N)) (bobp)))
    (- 0 (string-to-number (match-string 2) 16)))
   ;; Negative integer/float?
   ;; The preceding token must be an op (not a number)
   ((and (looking-at "$?\\(-[0-9]*\\.?[0-9]+\\)")
	 (or (not (eq (preceding-char) ?N)) (bobp)))
    (string-to-number (match-string 1)))
   (t nil)))

;;;###autoload
(defun my-calc (command)
  "Simple calculator.

Supports the following operations:
	( )	grouping
	* / %	multiplication, division, modulo
	+  -	addition and subtraction
	<< >>	arithmetic shift left and right
	&	bitwise and
	^	bitwise exclusive or
	|	bitwise or

Numbers are as follows:
	[-]0xN	is a hex (base 16) number
	[-]N.N	is a floating point number
	[-]N	is an integer.

Output goes to the *calc* buffer and the echo line."
  (interactive "sCalc: ")
  (save-excursion
    (set-buffer (get-buffer-create "*calc*"))
    (save-restriction
      (narrow-to-region (point-max) (point-max))
      (insert command)
      (insert ?=)			; mark end of command

      ;; Remove all white space and Ns
      (goto-char (point-min))
      (while (re-search-forward "[ \tN]" nil t) (replace-match ""))
      (goto-char (point-min))		; go back to start

      (let (ops cmd result one two func)

	(stack-clear nums)
	(setq ops (stack-create))
	(stack-push ops ?=)

	;; Continue until all input parsed and command stack empty.
	(while (or (not (eq (following-char) ?=)) (not (eq (stack-top ops) ?=)))

	  ;; special case for two char tokens `>>' and `<<'
	  (if (looking-at "\\(<<\\)\\|\\(>>\\)") (forward-char))

	  ;; process a command char
	  (if (<= (my-calc-f (stack-top ops)) (my-calc-g (following-char) t))
	      ;; shift
	      (progn
		(stack-push ops (following-char))
		(if (not (eq (following-char) ?=)) (forward-char)))
	    ;; reduce
	    (while
		(progn
		  ;; Pop the top command.
		  (setq cmd (stack-pop ops))

		  (when (setq func (cdr (assoc cmd my-calc-commands)))
		    ;;Perform operation and push result
		    ;; All commands take two numbers
		    (setq two (stack-pop nums))
		    (setq one (stack-pop nums))
		    (stack-push nums (apply func one two nil)))

		  ;; Until
		  (>= (my-calc-f (stack-top ops)) (my-calc-g cmd))))))

	;; Done! Put the result in both the *calc* buffer and the echo area.
	(delete-region (point-min) (point-max))
	(setq result (stack-pop nums))
	(cond ((integerp result)
	       (insert (format "%s = %d (%x)\n" command result result))
	       (message "%d (%x)"  result result))
	      ((floatp result)
	       (insert (format "%s = %g\n" command result))
	       (message "%g" result))
	      (t	;; Huh? Should be integer or float...
	       (insert (format "%s = ?%S?\n" command result))
	       (message "?%S?" result)))))))

;;}}}

;;; calc.el ends here
