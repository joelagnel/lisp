;;; db-time.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;;	Alan K. Stebbens, UCSB <aks@hub.ucsb.edu>
;; Keywords: EDB

;;; Commentary:

;; Library of date and time types for EDB database fields.
;; This file is an extension of db-types.el.

;; This file defines the date and time record types, plus several kinds of
;; date- and time-related display types, with variations on formatting.

;; For efficiency, the types are defined in terms of the displayspec
;; abstraction instead of via format strings.  Improvements and additions
;; are welcome.

;;; Code:


(require 'db-util)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dates
;;;

;;; The definition of the displayspec and recordfieldspecs is at the end of
;;; the date section.

;;;
;;; Abstraction:  dotted list of (year month . day), all integers.
;;;

(defsubst make-date (year month day)
  "Make an EDB date object with arguments YEAR MONTH DAY."
  (cons year (cons month day)))

(defsubst date-year (date) (car date))
(defsubst date-month (date) (car (cdr date)))
(defsubst date-day (date) (cdr (cdr date)))

(defsubst make-empty-date ()
  "Return a date object containing no information."
  (make-date nil nil nil))

(defun zero-or-empty-date-p (date)
  "Return t if all the date's slots contain nil or 0, nil otherwise."
  (and (let ((year (date-year date))) (or (not year) (zerop year)))
       (let ((month (date-month date))) (or (not month) (zerop month)))
       (let ((day (date-day date))) (or (not day) (zerop day)))))

(defsubst date-year-short (date)
  "Extract the year and return it modulo 1900."
  (% (date-year date) 1900))

(defun date-year-long (date)
  "Extract the year as a four digit value."
  (let ((yy (date-year date)))
    (cond ((< yy 50) (+ 2000 yy))
	  ((< yy 99) (+ 1900 yy))
	  (t yy))))

;;; Years

(defun leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year."
  (or
   (and (=  (% year   4) 0)
	(/= (% year 100) 0))
   (= (% year 400) 0)))

(defun date->day-of-year (date)
  "Return the day number within the year for Gregorian DATE."
  ;;
  ;; An explanation of the calculation can be found in PascAlgorithms by
  ;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.
  ;;
  (let* ((month (date-month date))
	 (day   (date-day date))
	 (year  (date-year date))
         (day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
	(progn
	  (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
	  (if (leap-year-p year)
	      (setq day-of-year (1+ day-of-year)))))
    day-of-year))

(defun date->absolute-days (date)
  "Return the number of days elapsed between the Gregorian 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((mm (date-month date))
	(dd (date-day date))
	(yy (1- (date-year-long date))))
    (+ (date->day-of-year date)		;+ days in this year
       (* 365 yy)			;+ days in prior years
       (/ yy 4)				;+ Julian leap years
       (- (/ yy 100))			;- century years
       (/ yy 400)			;+ Gregorian leap years
       )))

;;; Weekdays

;; Sunday must come first -- absolute dates begin on Sunday, Dec 31, 1BC.
(defconst weekday-array
  '["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "An array of weekday names.")

(defconst weekday-alist
 '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
   ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
   ("Tues" . 2) ("Thurs" . 4)
   ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
   ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defsubst integer->weekday (dayx)
  "Convert INDEX into its corresponding weekday name."
  (aref weekday-array (% dayx 7)))

(defsubst integer->weekday-abbrev (dayx)
  "Convert INDEX into its corresponding three-letter abbreviated weekday name."
  (substring (integer->weekday dayx) 0 3))

(defsubst date->weekday-index (date)
  "Return the weekday index for DATE."
  (% (date->absolute-days date) 7))

(defsubst date->weekday-name (date)
  "Return the weekday name for the DATE."
  (integer->weekday (date->weekday-index date)))

(defsubst date->weekday-abbrev (date)
  "Return the abbreviated weekday name for the DATE"
  (substring (date->weekday-name date) 0 3))


;;; Months

(defconst monthlength-array
  [0 31 28 31 30 31 30 31 31 30 31 30 31])

;; I could add a fancy leap year check.
(defun date-month-day-compatible (date)
  (if (date-day date)
      (if (date-month date)
	  (<= (date-day date) (aref monthlength-array (date-month date)))
	(error "Date has a day but no month"))
    t))

;; These sub-alists aren't really necessary; they're only used to make the
;; associated arrays.  And the full alist is used, of course.  But it uses
;; different cons cells, which is a waste.
(defconst full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst monthname-alist
  (append monthabbrev-alist
	  full-monthname-alist
	  '(("Sept" . 9))))

;; Why do I need this in an array?  (Why not?)  (Well, it's extra space.)
(defconst monthabbrev-array
  (vconcat '("") (mapcar (function car) monthabbrev-alist)))

(defconst monthname-array
  (vconcat '("") (mapcar (function car) full-monthname-alist)))

;; MONTHNAME shouldn't include a trailing period, even if it's an abbreviation.
(defsubst monthname->integer (monthname)
  ;; (setq monthname (capitalize (string-right-trim "." monthname)))
  (cdr (assoc (capitalize monthname) monthname-alist)))

(defsubst integer->monthname (monthno)
  (aref monthname-array monthno))

(defsubst integer->monthabbrev (monthno)
  (aref monthabbrev-array monthno))


;;; Ordering functions

(defun date-order-absolute (date1 date2)
  (let ((result (number-or-nil-order-nil-greatest
		 (date-year date1) (date-year date2))))
    (if (zerop result)
	(date-order-within-year date1 date2)
      result)))

(defun date-order-within-year (date1 date2)
  (let ((result (number-or-nil-order-nil-greatest
 		 (date-month date1) (date-month date2))))
    (if (zerop result)
	(number-or-nil-order-nil-greatest (date-day date1) (date-day date2))
      result)))


;;;
;;; Regexps
;;;

(defconst monthname-regexp
  (concat "\\("
	  (mapconcat (function car)
		     monthname-alist
		     "\\|")
	  "\\)\\.?"))

(defconst weekday-regexp
  (concat "\\("
	  (mapconcat (function car)
		     weekday-alist
		     "\\|")
	  "\\)\\.?"))

(defconst monthnumber-regexp "\\(0?[1-9]\\|1[0-2]\\)")
(defconst monthnumber-regexp-two-char "\\(0[1-9]\\|1[0-2]\\)")

(defconst monthday-regexp "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
(defconst monthday-regexp-two-char "\\([0-2][0-9]\\|3[01]\\)")

;; Note no surrounding ()!
(defconst full-year-regexp "[0-2][0-9][0-9][0-9]")
(defconst short-year-regexp "[0-9][0-9]")

;; Note no internal grouping; is that intentional?
;; That is, this only counts as one grouping when counting regexp matches,
;; because I didn't use any internal \\( \\).
(defconst year-regexp (concat "\\(" full-year-regexp
			      "\\|" short-year-regexp "\\)"))

;; ;; I think this works; but I don't think I want to use it.
;; (defconst year-regexp-maybe (concat year-regexp "*"))

(defconst elt-separator-regexp "[ -.,/']+")


;; I could limit the separators some, but it's
;; easier to use the same ones everywhere.
(defconst date-regexps
  (list
   ;; MMDDYY
   (cons (concat monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 1 2))
   (cons (concat monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 1 nil 2))
   ;; DDMMYY
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(4 nil 2 1))
   (cons (concat "\\("
		 monthday-regexp
		 elt-separator-regexp
		 "\\)?"
		 monthname-regexp
		 elt-separator-regexp
		 year-regexp)
	 '(4 nil 3 2))
   (cons (concat monthday-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 "\\(" full-year-regexp "\\)")
	 '(3 2 nil 1))
   ;; YYMMDD
   ;; Using year-regexp instead of full-year-regexp is ambiguous (consider
   ;; 11-11-11), but we already tried MMDDYY and it failed.
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 nil 2 3))
   (cons (concat year-regexp
		 elt-separator-regexp
		 monthnumber-regexp
		 elt-separator-regexp
		 monthday-regexp)
	 '(1 2 nil 3))
   ;; YYMMDD, no separators
   ;; This is ambiguous.
   (cons (concat year-regexp
		 monthnumber-regexp-two-char "?"
		 monthday-regexp-two-char "?")
	 '(1 2 nil 3))
   ;; WWMMDDYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 elt-separator-regexp
		 monthday-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 2 3))
   ;; WWDDMMYY
   (cons (concat weekday-regexp
		 elt-separator-regexp
		 monthday-regexp
		 elt-separator-regexp
		 monthname-regexp
		 "\\("
		 elt-separator-regexp
		 year-regexp
		 "\\)?")
	 '(5 nil 3 2))
   ;; ctime
   (cons (concat
	  weekday-regexp
	  " "
	  monthname-regexp
	  "  ?"
	  monthday-regexp
	  ;; time of day
	  " [0-9:]+ "
	  "\\(" full-year-regexp "\\)")
	 '(4 nil 2 3))
   )
  "Assoc list of regexps and match locators.
A match locator is a list of four numbers indicating which submatch of the
regexp contains the year, month number, month name, and day of the month.
The list elements may be nil if that information is not available.")


;;;
;;; Parsing dates
;;;

(defun parse-date-string (date-string)
  "Parse DATE-STRING, and return a date object; err if the parse is invalid.
If DATE-STRING contains only whitespace, return a null date object.
If DATE-STRING is nil, use the result of `parse-date-default-function' instead."
  (let ((regexp-alist date-regexps)
	result
	match-list)
    (db-debug-message "parse-date-string `%s'" date-string)
    (if (null date-string)		;provide default date for nil strings
	(setq date-string (parse-date-default-function))
      (setq date-string (db-string-trim-whitespace date-string)))
    (if (zerop (length date-string))	;if empty string,
	(make-empty-date)		;return empty date
      ;; regexp-alist is nulled if a match is found
      (progn
	(while regexp-alist
	  (if (string-match (concat "^" (car (car regexp-alist)) "$")
			    date-string)
	      ;; Bug in version 18 save-match-data:  it's impossible
	      ;; to have a marker at 0, so this gets converted to 1.
	      (setq match-list (mapcar (function (lambda (match-no)
					 (and match-no
					      (db-match-string-maybe
					       match-no date-string))))
				       (cdr (car regexp-alist)))
		    ;; match-list is year, monthnumber, monthname, day
		    result
		    (make-date
		     (string-or-nil->number-or-nil (car match-list))
		     (or (string-or-nil->number-or-nil (car (cdr match-list)))
			 (and (car (cdr (cdr match-list)))
			      ;; match is non-nil; don't check match-beginning
			      ;; At one time this clobbered the match-data.
			      (monthname->integer (car (cdr (cdr match-list))))))
		     (string-or-nil->number-or-nil (nth 3 match-list)))
		    regexp-alist nil)
	    ;; string-match failed
	    (setq regexp-alist (cdr regexp-alist))))
	(db-debug-message "parse-date-string:  result = %s" result)
	(if result
	    (if (date-month-day-compatible result)
		(if (zero-or-empty-date-p result)
		    (make-empty-date)
		  result)
	      (error "There is no such day as %s %d!"
		     (integer->monthname (date-month result))
		     (date-day result)))
	  (error "`%s' is not a valid date." date-string))))))

(defun parse-date-string-or-nil (date-string)
  "Like `parse-date-string', but returns null date in case of nil arg."
  (if date-string
      (parse-date-string date-string)
    (make-empty-date)))

(defvar parse-date-default 'empty
  "One of the symbols 'empty or 'current-date, specifying what date string
`parse-date-default-function' should return, and `parse-date-string' should
use when passed a nil argument.")

(defun parse-date-default-function ()
  "Return a default value for `parse-date-string' to use if its input is nil."
  (cond ((eq parse-date-default 'empty)
	 "")
	((or (eq parse-date-default 'today)
	     (eq parse-date-default 'current-date)
	     (eq parse-date-default 'current-time)
	     (eq parse-date-default 'current-time-string))
	 (current-time-string))
	(t
	 (error "Unrecodgnized value `%s' for variable parse-date-default."
		parse-date-default))))



;; AKS, UCSB, 9/30/92
;;
;; General purpose date format routine


;; *WARNING*: If any new escape symbols are added, BE SURE that they are
;; placed in order of longest symbol first, so that the regexp computed
;; below works properly.

(defconst format-date-sub-syms-alist
  '(("day"     . ((date-day date) .   (date->weekday-abbrev date)))
    ("dd"      . ((date-day date) .   (format "%02d" (date-day date))))
    ("d"       . ((date-day date) .   (date-day date)))
    ("month"   . ((date-month date) . (integer->monthname (date-month date))))
    ("mon"     . ((date-month date) . (integer->monthabbrev (date-month date))))
    ("mm"      . ((date-month date) . (format "%02d" (date-month date))))
    ("m"       . ((date-month date) . (date-month date)))
    ("year"    . ((date-year date) .  (date-year-long date)))
    ("yy"      . ((date-year date) .  (format "%02d" (date-year-short date))))
    ("jday"    . ((and (date-day date)
		       (date-month date)
		       (date-year date)) . (date->day-of-year date)))
    ("wday"    . ((and (date-day date)
		       (date-month date)
		       (date-year date)) . (date->weekday-index date)))
    ("weekday" . ((and (date-day date)
		       (date-month date)
		       (date-year date)) . (date->weekday-name date)))
    )
  "An alist of (NAME . (TEST . SEXP)) used by `format-date'.  Each NAME
is a string, which, when prefixed by \"%\", will be substituted by the
value resulting from evalling the associated SEXP but only if TEST evals
to non-null.")

;; Build a regexp which matches the symbol names given above

(defconst format-date-sub-syms-regexp
  (concat "%\\("
	  (mapconcat (function car) format-date-sub-syms-alist "\\|")
	  "\\)")
  "A regexp pattern to parse format strings for symbol substition strings;
this variable is computed from the variable `format-date-sub-syms-alist'.")

(defun format-date (format-string &optional date)
  "Using FORMAT-STRING, format the DATE, which defaults to the current date
if nil.  FORMAT-STRING can contain the following symbol strings,
which are substituted by their corresponding value from the date; other
characters are inserted as is.

 String    Action
 ======    ======
  %d       day of month -- 1 to 31 (one or two digits)
  %dd	   day of month -- 01 to 31 (always two digits)
  %m       month of year - 1 to 12 (one or two digits)
  %mm	   month of year - 01 to 12 (always two digits)
  %mon	   month name (abbreviated) - Jun
  %month   full month name - June
  %yy	   last 2 digits of year - 00 to 99
  %year	   year as 4 digits -- 0000 to 9999?
  %jday	   Julian day of year -- 1 to 366
  %wday	   day of week -- 0 to 6 (Sunday = 0)
  %day	   day of week name -- \"Sun\" to \"Sat\"
  %weekday full day of week name -- \"Sunday\" to \"Saturday\"

See the variables `format-date-sub-syms-alist' and
`format-date-sub-syms-regexp'.

A special case: if an element of DATE is nil, its field is hidden.  A
DATE object of all nils is thus formatted as the empty string."

  (if (null date)
      (setq date (parse-date-string nil)))
  (let* ((ofs 0) (buf "") sym-alist x)
    (while (setq x (string-match
		    format-date-sub-syms-regexp format-string ofs))
      (if (not (setq sym-alist (assoc (db-match-string 1 format-string)
				      format-date-sub-syms-alist)))
	  (error "format-date: Symbol %s is not in format-date-sub-syms-alist!"
		 (db-match-string 1 format-string))
	(if (eval (car (cdr sym-alist)))	;does TEST work?
	    ;; Yes; insert its prefix string and its value
	    (setq buf (concat buf
			      (if (not (string= buf ""))
				  (substring format-string ofs x))
			      (eval (cdr (cdr sym-alist)))))
	  ))
      (setq ofs (match-end 0))		;skip past the variable
      )
    (concat buf (substring format-string ofs))))


(defconst simple-format-date-default "%month %d, %year"
  "*A default format used by simple-format-date.")

;; Note only one argument
(defun simple-format-date (date)
  "Format the DATE using a default format, defined by the variable
`simple-format-date-default'.
If DATE is nil, use the value of `parse-date-default-function'."
  (format-date simple-format-date-default date))

(defun simple-format-date-or-nil (date)
  "Format the DATE using a default format, defined by the variable
`simple-format-date-default'.
If DATE is nil, return the empty string."
  (if date
      (format-date simple-format-date-default date)
    ""))


;; AKS, format-date-xxx routines
;; The following display functions are used above in the displayspec
;; definitions:
;;
;;  Function             Sample    Separator var                   Default
;;  ==================   ========  ===========================     =======
;;  format-date-mmddyy   06/10/54  format-date-mmddyy-separator    "/"
;;  format-date-yymmdd   541006    format-date-yymmdd-separator    ""
;;  format-date-ddmmyy   10.6.54   format-date-ddmmyy-separator    "."
;;  format-date-ddmmmyy  6 Jun 54  format-date-ddmmmyy-separator   " "
;;  format-date-yyyymmdd 1954/6/6  format-date-yyyymmdd-separator  "/"
;;
;;  format-date-europe   10.6.54
;;  format-date-dec      06-Jun-54
;;  format-date-full     June 6, 1954
;;  format-date-unix     Sun Jun 6 1954
;;  format-date-all      Sunday, June 6, 1954

;; Is there a better way to do this?  How about something like:
;; /field,date="%w %m %d %y"
;; (Display specifications may not contain spaces.)
;; [Ok, how about: ``/field,date="%w_%m_%d_%y"'' if we make format-date
;; turn "_" into blanks??]

(defconst format-date-mmddyy-separator "/"
  "*A string used to separate the components of the MMDDYY date format.")

(defun format-date-mmddyy (date)
  "Format the DATE into a MM/DD/YY format.  The \"/\" separator is a user-
configuration variable in format-date-mmddyy-separator.
If DATE is nil, return the empty string."
  (if date
      (let ((sep format-date-mmddyy-separator))
	(format-date (format "%%mm%s%%dd%s%%yy" sep sep) date))
    ""))

(defconst format-date-ddmmyy-separator "."
  "*A string used to separate the components of the DDMMYY date format.")

(defun format-date-ddmmyy (date)
  "Format the DATE into a DD.MM.YY format.  The \".\" separator is configured
by the variable format-date-ddmmyy-separator.
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or format-date-ddmmyy-separator "")))
	(format-date (format "%%d%s%%m%s%%yy" sep sep) date))
    ""))

(defconst format-date-yymmdd-separator ""
  "*A string used to separate the componenets of the YYMMDD date format.")

(defun format-date-yymmdd (date)
  "Format the DATE into a YYMMDD format.  The components are separated by
the value of `format-date-yymmdd-separator', which is initially the null
string.
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or format-date-yymmdd-separator "")))
	(format-date (format "%%yy%s%%mm%s%%dd" sep sep) date))
    ""))

(defconst format-date-ddmmmyy-separator " "
  "*A string used to separate the components of the DD MMM YY date format.")

(defun format-date-ddmmmyy (date)
  "Format the DATE into a DD MMM YY format.  The components are separated by
the value of format-date-ddmmmyy-separator, which is initially a single space.
If DATE is nil, return the empty string."
  (if date
      (let ((sep format-date-ddmmmyy-separator))
	(format-date (format "%%d%s%%mon%s%%yy" sep sep) date))
    ""))

(defconst format-date-yyyymmdd-separator "/"
  "*A string used to separate the components of the YYYY/MM/DD date format.")

(defun format-date-yyyymmdd (date)
  "Format the DATE into a YYYY/MM/DD format.  The components are separated by
the value of `format-date-yyyymmdd-separator', which is initially a ``/''.
If DATE is nil, return the empty string."
  (if date
      (let ((sep format-date-yyyymmdd-separator))
	(format-date (format "%%year%s%%m%s%%d" sep sep) date))
    ""))


(defun format-date-full (date)
  "Format the DATE into the full format: MMMM DD, YYYY.
If DATE is nil, return the empty string."
  (if date
      (format-date "%month %d, %year" date)
    ""))

(defun format-date-unix (date)
  "Format the DATE into the standard Unix format: DAY MMM DD YYYY.
If DATE is nil, return the empty string."
  (if date
      (format-date "%day %mon %d %year" date)
    ""))

(defun format-date-all (date)
  "Format the DATE using all components, without abbreviations, in the format
DAYNAME, MMMM DD, YYYY.
If DATE is nil, return the empty string."
  (if date
      (format-date "%weekday, %month %d, %year" date)
    ""))

;; Some common variations on a theme

(defun format-date-dec (date)
  "Format the DATE into the standard DEC format: dd-mmm-yy.
If DATE is nil, return the empty string."
  (let ((format-date-ddmmmyy-separator "-"))
    (format-date-ddmmmyy date)))

(defun format-date-europe (date)
  "Format the DATE into the European standard, which is DD.MM.YY.
If DATE is nil, return the empty string."
  (let ((format-date-ddmmyy-separator "."))
    (format-date-ddmmyy date)))


;; Test routines
(defun test-date-formats ()
   (interactive)
   (let ((formats '(mmddyy ddmmyy yymmdd ddmmmyy yyyymmdd full
			   unix all dec europe))
	 form)
     (while formats
       (setq form (car formats)
	     formats (cdr formats))
       (message "Format %s = \"%s\"" form
		(funcall (intern (concat "format-date-" (symbol-name form)))
			 (parse-date-string (current-time-string))))
       (read-char))))


;;;
;;; EDB type definitions
;;;

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function simple-format-date))
  (displayspec-set-display->actual ds (function parse-date-string))
  (define-displaytype-from-displayspec 'date ds))
(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'date)
  (recordfieldspec-set-default-value rs (make-empty-date))
  (recordfieldspec-set-actual->stored rs (function date->storage-string))
  (recordfieldspec-set-stored->actual rs (function storage-string->date))
  (recordfieldspec-set-merge-function rs (function date-merge))
  (recordfieldspec-set-order-fn rs (function date-order-absolute))
  (recordfieldspec-set-match-function rs (function date-match-function))
  (recordfieldspec-set-help-info rs "A date.")
  (define-recordfieldtype-from-recordfieldspec 'date rs))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'date))))
  (recordfieldspec-set-stored->actual rs (function storage-string->date))
  (define-recordfieldtype-from-recordfieldspec 'date-efficient-storage rs))

(let ((ds (copy-displayspec (displaytype->displayspec 'date))))
  (displayspec-set-actual->display ds (function simple-format-date-or-nil))
  (displayspec-set-display->actual ds (function parse-date-string-or-nil))
  (define-displaytype-from-displayspec 'date-or-nil ds))
(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'date))))
  (recordfieldspec-set-type rs 'date-or-nil)
  (recordfieldspec-set-default-value rs nil)
  (recordfieldspec-set-stored->actual rs (function parse-date-string-or-nil))
  (recordfieldspec-set-help-info rs "(Optional) date.")
  (define-recordfieldtype-from-recordfieldspec 'date-or-nil rs))

;; AKS, 9/30/92
;; def-date-disptype is used to create alternative display types
;; on the date.

(defun def-date-disptype (type)
  "Construct a new date displaytype based on TYPE (a string).  A function named
`format-date-TYPE' must exist."
  (let* ((func (intern (concat "format-date-" type)))
	 (typename (intern (concat "date-" type)))
	 (ds (copy-displayspec (displaytype->displayspec 'date))))
    (displayspec-set-actual->display ds func)
    (define-displaytype-from-displayspec typename ds)))

(def-date-disptype "mmddyy")
(def-date-disptype "yymmdd")
(def-date-disptype "ddmmyy")
(def-date-disptype "ddmmmyy")
(def-date-disptype "yyyymmdd")
(def-date-disptype "europe")
(def-date-disptype "full")
(def-date-disptype "all")
(def-date-disptype "unix")
(def-date-disptype "dec")


(defun date-match-function (patterndate targetdate)
  (and (or (not (date-year patterndate))
	   (and (date-year targetdate)
		(= (date-year patterndate) (date-year targetdate))))
       (or (not (date-month patterndate))
	   (and (date-month targetdate)
		(= (date-month patterndate) (date-month targetdate))))
       (or (not (date-day patterndate))
	   (and (date-day targetdate)
		(= (date-day patterndate) (date-day targetdate))))))

;; Merge dates: if an item of a date is nil, use the other date's value.
(defun date-merge (date1 date2)
  (let ((mm (or (date-month date1) (date-month date2)))
	(dd (or (date-day date1) (date-day date2)))
	(yy (or (date-year date1) (date-year date2))))
    (make-date yy mm dd )))

;; File representation
(fset 'date->storage-string 'format-date-full)
(fset 'storage-string->date 'date-stored->actual)

;;; This is fairly human-readable, but ambiguous to Europeans.
(defun date->storage-string-mmddyyyy (date)
  (if (date-year date)
      (format "%02d/%02d/%02d"
	      (or (date-month date) 0)
	      (or (date-day date) 0)
	      (or (date-year date) 0))
    (if (not (or (date-month date) (date-day date)))
	""
      (format "%02d/%02d"
	      (or (date-month date) 0)
	      (or (date-day date) 0)))))

(defun storage-string-mmddyyyy->date (str)
  (let ((month (string->integer (substring str 0 2)))
	(day (string->integer (substring str 3 5)))
	(year (and (> (length str) 5)
		   (string->integer (substring str 6)))))
    (make-date (if (not (zerop month)) month)
	       (if (not (zerop day)) day)
	       (if (and year (not (zerop year))) year))))

;;; This is quite fast, but not very human-readable.
(defun date->storage-string-lisp (date)
  (format "%s" date))

(defun storage-string-lisp->date (str)
  (car (read-from-string str)))

;; Don't do anything if it's already a date.  The point of this is to do
;; the right thing even when the field didn't appear in the database file,
;; so the record field got set to the empty date rather than the empty string.
(defun date-stored->actual (date-stored)
  (if (stringp date-stored)
      ;; This is slow but general.
      (parse-date-string date-stored)
    date-stored))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Times
;;;
;;; Much of this was lifted from Dates
;;; Otherwise it was written by Alan K. Stebbens, UCSB <aks@hub.ucsb.edu>
;;;
;;; Display types:   time, time-12, time-24, time-12-hhmm, time-24-hhmm,
;;;		     and time-hhmm.
;;; Field Types:     time, time-12, time-24, and time-arb-storage
;;; Functions: 	     parse-time-string, format-time-12, format-time-24
;;;		     time->storage-string, storage-string->time,
;;;		     time-order, time-merge, time-match-function,
;;;		     time-default-constraint
;;;

;;; Abstraction:  TIME object is a 3-elt list (this allows n'th to work)
(defsubst make-time (hours mins secs)
  "Arguments HOURS MINS SECS."
  (list hours mins secs))

(defsubst time-hours (time) (car time))
(defsubst time-mins (time) (car (cdr time)))
(defsubst time-secs (time) (car (cdr (cdr time))))

(defsubst make-empty-time ()
  "Return a time object containing no information."
  (make-time nil nil nil))

(defun empty-time-p (time)
  "Return t if all the time's slots contain nil, nil otherwise."
  (and (null (time-hours time))
       (null (time-mins time))
       (null (time-secs time))))

;; time-default-constraint

(defun time-default-constraint (time record record-index database)
  "Enforce proper time values."
  (if (or (empty-time-p time)
	  (and (< (time-hours time) 23)
	       (< (time-mins time) 59)
	       (< (time-secs time) 59)))
      t
    (error "Invalid time value.")))


;; parse-time-string
;;
;; State-driven time parser; I converted this from my Perl time parser.
;; Alan K. Stebbens, UCSB <aks@hub.ucsb.edu>

(defconst parse-time-regexp-array
  [ "\\([0-9]?[0-9]\\)\\(:\\| ?[ap]m\\)"
    "\\([0-5][0-9]\\)\\(:\\| ?[ap]m\\|\\Sw\\|$\\)"
    "\\([0-5][0-9]\\)\\( *[ap]m\\|\\Sw\\|$\\)" ]
  "An array of regexps used by parse-time-string, indexed by the
current parse state to obtain the appropriate regexp.")


(defun parse-time-string (time-string)
  "Parse the first occurrence of hh:mm:ss in TIME-STRING; return a time object.
If \":ss\" is hidden in TIME-STRING, the seconds default to zero.
If TIME-STRING contains only whitespace, return an empty time object.
If TIME-STRING is nil, use the result of `parse-time-default-function' instead."
  (if (not time-string)			; provide default time for nil strings
      (setq time-string (parse-time-default-function))
    (setq time-string (db-string-trim-whitespace time-string)))
  (if (zerop (length time-string))
      (make-empty-time)
    (let ((str time-string)
	  (case-fold-search t)
	  ;; Initial regexp matches HH: or HHpm
	  (hh 0) (mm 0) (ss 0) pm
	  (vars '[hh mm ss])
	  (ofs 0)
	  (state 0))
      (while (and (< state 3)
		  (< ofs (length str))
		  (string-match (aref parse-time-regexp-array state) str ofs))
	(set (aref vars state) (string-to-int (db-match-string 1 str)))
	(setq state (if (equal ":" (db-match-string-maybe 2 str))
			(1+ state)
		      3))
	(setq ofs (match-end 0)))
      (make-time (if (and (setq pm (db-match-string-maybe 2 str))
			  (string-match "[ap]m" pm))
		     (+ (% hh 12) (if (string-match "pm" pm) 12 0))
		   hh)
		 mm ss))))

(defvar parse-time-default 'empty
  "One of the symbols 'empty or 'current-time, specifying what time string
`parse-time-default-function' should return, and `parse-time-string' should
use when passed a nil argument.")

(defun parse-time-default-function ()
  "Return a default value for `parse-time-string' to use if its input is nil."
  (cond ((eq parse-time-default 'empty)
	 "")
	((or (eq parse-time-default 'current-time)
	     (eq parse-time-default 'now))
	 (current-time-string))
	(t
	 (error "Unrecognized value `%s' for variable parse-time-default."
		parse-time-default))))

;; test routine for above

'(defun test-time-parser ()
   (interactive)
   (let ((times '("1am"
		  "1pm"
		  "1 pm"
		  "1 PM"
		  "1:01am"
		  "1:02pm"
		  "12:01:01am"
		  "12:01:01pm"
		  "12:34:45"
		  " 12:34:45"
		  " 12:34:45 "
		  "12:34:45 "
		  "May 12 14:34 1992"
		  "May 19 10:17:23pm 1992"
		  "May 19 10:17:23 pm 1992")))
     (while times
       (message "In = \"%s\" Out = \"%s\" (CR for next)"
		(car times)
		(parse-time-string (car times)))
       (read-char)
       (setq times (cdr times)))))


(defun format-time-24 (time)
  "Format TIME (a three element list) into a 24 hour time string in the
format HH:MM:SS.  If an element of the list is nil, that component is
not edited.  Typically, the seconds element is hidden or set to nil to
produce a time format with only HH:MM.  See `format-time-24-hhmm'."
  (let ((hh (time-hours time))
	(mm (time-mins time))
	(ss (time-secs time)))
    (concat
     (and hh (format "%d" hh))
     (and hh mm ":")
     (and mm (format "%02d" mm))
     (and mm ss (format ":%02d" ss))
     ;; no am/pm string
     )))


(defun format-time-12 (time)
  "Format TIME (a 3 element list) into a 12 hour time string in the
format HH:MM:SS PM.  If an element of the list is nil, that component is
not edited.  Typically, the seconds element is hidden or set to nil to
produce a time format with only HH:MM.  See `format-time-12-hhmm'."
  (let ((hh (time-hours time))
	(mm (time-mins time))
	(ss (time-secs time)))
    (concat
     (and hh (format "%d" (cond ((zerop hh) 12)
				((< hh 13) hh)
				(t (- hh 12)))))
     (and hh mm ":")
     (and mm (format "%02d" mm))
     (and mm ss (format ":%02d" ss))
     (and hh (if (>= hh 12) "pm" "am")))))

(fset 'format-time-hhmm 'format-time-12-hhmm)

(defun format-time-12-hhmm (time)
  "Format time in HH:MM PM format."
  (format-time-12 (make-time (time-hours time) (time-mins time) nil)))

(defun format-time-24-hhmm (time)
  "Format 24-hour time without seconds"
  (format-time-24 (make-time (time-hours time) (time-mins time) nil)))

;;;
;;; EDB type specifications
;;;

(defun def-time-disptype (type)
  "Construct a new time based on TYPE, which must exist as a function
of the name \"format-time-TYPE\"."
  (let* ((func (intern (concat "format-time-" type)))
	 (typename (intern (concat "time-" type)))
	 (ds (copy-displayspec (displaytype->displayspec 'time))))
    (displayspec-set-actual->display ds func)
    (define-displaytype-from-displayspec typename ds)))

(let ((ds (make-displayspec)))
  (displayspec-set-indent ds nil)
  (displayspec-set-actual->display ds (function format-time-12))
  (displayspec-set-display->actual ds (function parse-time-string))
  (define-displaytype-from-displayspec 'time ds))

(def-time-disptype "12")		;am/pm time
(def-time-disptype "24")		;military time
(def-time-disptype "hhmm")		;synonym for time-12-hhmm
(def-time-disptype "12-hhmm")		;am/pm w/o secs
(def-time-disptype "24-hhmm")		;military w/o secs

(let ((rs (make-recordfieldspec)))
  (recordfieldspec-set-type rs 'time)
  (recordfieldspec-set-default-value rs (make-empty-time))
  (recordfieldspec-set-constraint-function rs (function time-default-constraint))
  (recordfieldspec-set-actual->stored rs (function time->storage-string))
  (recordfieldspec-set-stored->actual rs (function storage-string->time))
  (recordfieldspec-set-merge-function rs (function time-merge))
  (recordfieldspec-set-order-fn rs (function time-order))
  (recordfieldspec-set-match-function rs (function time-match-function))
  (recordfieldspec-set-help-info rs "A time.")
  (define-recordfieldtype-from-recordfieldspec 'time rs))

(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'time))))
  (recordfieldspec-set-help-info rs "A 12-hour time, with AM/PM.")
  (define-recordfieldtype-from-recordfieldspec 'time-12 rs))

(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'time))))
  (recordfieldspec-set-help-info rs "A 24-hour time (a.k.a. military time).")
  (define-recordfieldtype-from-recordfieldspec 'time-24 rs))

(let ((rs (copy-recordfieldspec (recordfieldtype->recordfieldspec 'time))))
  (recordfieldspec-set-stored->actual rs (function parse-time-string))
  (define-recordfieldtype-from-recordfieldspec 'time-arb-storage rs))

;; time-order

(defun time-order (time1 time2)
  (let ((result 0) (n 0))
    (while (and (<= n 2) (zerop result))
      (setq result
            (number-or-nil-order-nil-greatest
             (nth n time1) (nth n time2))))
    result))

;; time-match-function

(defun time-match-function (patterntime targettime)
  (and (or (not (time-hours patterntime))
           (and (time-hours targettime)
       	 (= (time-hours patterntime) (time-hours targettime))))
       (or (not (time-mins patterntime))
           (and (time-mins targettime)
       	 (= (time-mins patterntime) (time-mins targettime))))
       (or (not (time-secs patterntime))
           (and (time-secs targettime)
       	 (= (time-secs patterntime) (time-secs targettime))))))

;; time->storage-string

(defun time->storage-string (time)
  (format "%02d:%02d:%02d"
          (or (time-hours time) 0)
          (or (time-mins time) 0)
          (or (time-secs time) 0)))

;; storage-string->time

(defun storage-string->time (str)
  (make-time (string->integer (substring str 0 2))
             (string->integer (substring str 3 5))
             (string->integer (substring str 6 8))))

;; time-merge
;; If an item in one time is nil, take the other time's value.

(defun time-merge (time1 time2)
  (let ((hh (or (time-hours time1) (time-hours time2)))
	(mm (or (time-mins time1) (time-mins time2)))
	(ss (or (time-secs time1) (time-secs time2))))
    (make-time hh mm ss)))

;;; db-time.el ends here
