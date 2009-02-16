;;; db-isbn.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Bertrand Petit <elrond@imladris.frmug.fr.net>
;;	Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; ISBN type for EDB.
;; An ISBN number uniquely identifies a book; it is a string, one of
;;  * null string (unknown ISBN)
;;  * a dash (this book has not been assigned an ISBN)
;;  * a 13-character string computed of numbers, dashes, and X.
;;    The last digit is a check digit.


;;; Code:

(provide 'db-isbn)

;;;
;;; Display type
;;;

;; Define a new field type and display type for isbn numbers. This
;; type is based on string-or-nil. It should had be named
;; isbn-or-dash-or-nil.
(let ((ds (copy-displayspec (displaytype->displayspec 'string-or-nil))))
  ;; One line max
  (displayspec-set-max-height ds 1)
  (define-displaytype-from-displayspec 'isbn ds))

;;;
;;; Field type
;;;

(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec
				 'string-or-nil))))
  (recordfieldspec-set-type rs 'isbn)
  (recordfieldspec-set-default-value rs "")
  (recordfieldspec-set-help-info rs "International Standard Book Number")
  (recordfieldspec-set-constraint-function rs 'isbn-constraint)
  (define-recordfieldtype-from-recordfieldspec 'isbn rs))

;;;
;;; Constraints
;;;

(defun isbn-constraint (field record fieldnumber database)
  "Check an isbn."

  (if (not (stringp field))
      (error "ISBN field %s should be a string." field))
  ;; Return t if all the tests succeed.
  (or (string-equal field "")
      (string-equal field "-")
      (progn
	(if (not (= (length field) 13))
	    (error "Bad length %d for ISBN %s; should be 13"
		   (length field) field))
	(if (not (string-match "\\`[0-9]+-[0-9]+-[0-9]+-[0-9xX]\\'" field))
	    (error "Malformed ISBN %s" field))
	;; Verify the check digit.
	(let ((check-digit (SFf-isbn-check-sum field)))
	  (if (not (string= check-digit (upcase (substring field 12 13))))
	      (error "Check digit should be %s in ISBN %s" check-digit field)))
	t)))

;; Compute the isbn check sum
(defun SFf-isbn-check-sum (isbn)
  ;; From _Numbers, Groups and Codes_ by J.F. Humphreys and
  ;; M. Y. Prest, Cambridge University Press, Avon: 1989. page 233.
  ;; Example: A well-known example of error-correction is provided by
  ;; the ISBN (International Standard Book Number) of published
  ;; books. This is a sequence of nine digits a1a2...a9, where each ai
  ;; is one of the numbers 0,1,...,9, together with a check digit which
  ;; is one of the symbols 0,1,2,3,4,5,6,7,8,9, or X (with X
  ;; representing 10). This last digit is included so as to give a
  ;; check that the previous 9 digits have been correctly transcribed,
  ;; and is calculated as follows.  Form the integer 10a1 + 9a2 + 8a3
  ;; +... +2a9 and reduce this modulo 11 to obtain an integer b
  ;; between 1 and 11 inclusive. The check digit is obtained by
  ;; subtracting b from 11.

  (let ((factor 10)
	(checksum 0)
	(index 0)
	digit)
    ;; factor is the polynome factor for each digit. Since we already
    ;; checked the format of the isbn, we don't need to do extensive
    ;; tests here. Factor begins at 10 and goes down to 2 at the last
    ;; (non-dash) digit excluding the check digit. checksum is the
    ;; check sum and index is the current digit in the isbn string.
    (while (>= factor 2)
      ;; Extract the digit
      (setq digit (aref isbn index)
	    index (+ 1 index))
      ;; If the digit is not a dash then it's a part of the polynome.
      (if (not (= digit ?-))
	  (setq checksum (+ checksum (* factor (- digit ?0)))
		factor (- factor 1))))

    ;; Last part of the computation.
    (setq checksum (- 11 (% checksum 11)))
    ;; Hack due to difference in the modulo function.
    (if (= checksum 11)
	(setq checksum 0))

    ;; Convert this checksum to a string, if the sum is 10 then the
    ;; string is "X". This string is the value of this function.
    (if (= checksum 10)
	"X"
      (number-to-string checksum))))

;;; db-isbn.el ends here
