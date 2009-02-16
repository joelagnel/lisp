;;;;;; Scheme-lookup: lookup documentation for Scheme symbols
;;;;;; Version $Version$

;;; This code is written by Trent W. Buck <trentbuck@gmail.com>
;;; (except where explicitly noted) and placed in the Public Domain.
;;; All warranties are disclaimed.

;;; This implementation uses the info version of R5RS; it requires the
;;; info manual to be installed on your system.  On Debian, you can
;;; get it by apt-get installing the r5rs-doc package.
;;;
;;; There is an HTML version of the R5RS available online at
;;; http://www.schemers.org/Documents/Standards/R5RS/HTML/, produced
;;; by tex2page.  The framework for adding tex2page HTML references
;;; already exists (see scheme-lookup-scheme48.el) -- someone just has
;;; to compile the index.

(require 'scheme-lookup)

(defun scheme-lookup-r5rs (symbol-name &optional redirect)
  (scheme-lookup-info "r5rs" (or redirect symbol-name)))

(put 'scheme-lookup-r5rs
     'scheme-lookup-pretty-name
     "Revised(5) Report on the Algorithmic Language Scheme")

(mapc
 (lambda (x) (scheme-lookup-add-reference (list 'scheme-lookup-r5rs x)))
 '("*" "+" "-" "/" "<" "<=" "<constant>" "<variable>" "=" ">" ">=" "abs" "acos"
   "and" "angle" "append" "apply" "asin" "assoc" "assq" "assv" "atan" "begin"
   "boolean?"  "call-with-current-continuation" "call-with-input-file"
   "call-with-output-file" "call-with-values" "car" "cdr" "caar" "cadr"
   "cdddar" "cddddr" "case" "ceiling" "char->integer" "char-alphabetic?"
   "char-ci<=?"  "char-ci<?"  "char-ci=?"  "char-ci>=?"  "char-ci>?"
   "char-downcase" "char-lower-case?"  "char-numeric?"  "char-ready?"
   "char-upcase" "char-upper-case?"  "char-whitespace?"  "char<=?"  "char<?"
   "char=?"  "char>=?"  "char>?"  "char?"  "close-input-port"
   "close-output-port" "complex?"  "cond" "cons" "cos" "current-input-port"
   "current-output-port" "delay" "denominator" "display" "dynamic-wind"
   "eof-object?"  "eq?"  "equal?"  "eqv?"  "eval" "even?"  "exact->inexact"
   "exact?"  "exp" "expt" "floor" "for-each" "force" "gcd" "if" "imag-part"
   "inexact->exact" "inexact?"  "input-port?"  "integer->char" "integer?"
   "interaction-environment" "lambda" "lcm" "length" "let" "let*" "let-syntax"
   "letrec" "letrec-syntax" "list" "list->string" "list->vector" "list-ref"
   "list-tail" "list?"  "load" "log" "magnitude" "make-polar"
   "make-rectangular" "make-string" "make-vector" "map" "max" "member" "memq"
   "memv" "min" "modulo" "negative?"  "newline" "not" "null-environment"
   "null?"  "number->string" "number?"  "numerator" "odd?"  "open-input-file"
   "open-output-file" "or" "output-port?"  "pair?"  "peek-char" "positive?"
   "procedure?"  "quasiquote" "quote" "quotient" "rational?"  "rationalize"
   "read" "read-char" "real-part" "real?"  "remainder" "reverse" "round"
   "scheme-report-environment" "set!"  "set-car!"  "set-cdr!"  "sin" "sqrt"
   "string" "string->list" "string->number" "string->symbol" "string-append"
   "string-ci<=?"  "string-ci<?"  "string-ci=?"  "string-ci>=?"  "string-ci>?"
   "string-copy" "string-fill!"  "string-length" "string-ref" "string-set!"
   "string<=?"  "string<?"  "string=?"  "string>=?"  "string>?"  "string?"
   "substring" "symbol->string" "symbol?"  "syntax-rules" "tan" "template"
   "transcript-off" "transcript-on" "truncate" "values" "vector" "vector->list"
   "vector-fill!"  "vector-length" "vector-ref" "vector-set!"  "vector?"
   "with-input-from-file" "with-output-to-file" "write" "write-char" "zero?"))

;;; A few symbols are hidden in the ellipsis between cadr and cdddar.
(mapc
 (lambda (x) (scheme-lookup-add-reference (list 'scheme-lookup-r5rs x "cadr")))
 '("cdar"  "cddr"
   "caaar" "caadr" "cadar" "caddr" "cdaar" "cdadr" "cddar" "cdddr"
   "caaaar"  "caaadr"  "caadar"  "caaddr" "cadaar" "cadadr" "caddar"
   "cadddr"  "cdaaar"  "cdaadr" "cdadar"  "cdaddr" "cddaar" "cddadr"))

(provide 'scheme-lookup-r5rs)
