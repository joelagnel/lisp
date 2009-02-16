;;; -*- Mode: Emacs-Lisp; outline-regexp: "\n;;;;+" -*-

;;;;;; Scheme-lookup: lookup documentation for Scheme symbols
;;;;;; Version $Version$

;;; This code is written by Trent W. Buck <trentbuck@gmail.com>
;;; (except where explicitly noted) and placed in the Public Domain.
;;; All warranties are disclaimed.

;;; Add this to your .emacs after placing this directory in
;;; /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/scheme-lookup/")
;;;   (autoload 'scheme-lookup "scheme-lookup" 
;;;     "View the documentation on the Scheme symbol SYMBOL-NAME."
;;;     t)
;;;   (eval-after-load 'scheme-lookup
;;;     '(mapc 'require '(scheme-lookup-r5rs scheme-lookup-srfi)))
;;;
;;; The `eval-after-load' form allows you to load only those
;;; references that you want.  The above example loads references for
;;; R5RS and the finalized SRFIs.
;;;
;;; This is developed with GNU Emacs 22.0.50.  It has not been tested
;;; with any other Emacs.

(defconst scheme-lookup-version '$Version$)

(require 'cl)
(require 'browse-url)
(require 'thingatpt)
(require 'button)                       ; new in Emacs 22

(defvar scheme-lookup-history nil)

;; A fast-lookup data structure to hold the references.
(defvar scheme-lookup-symbols (make-vector 67 0))


;;;; User interface
(defun scheme-lookup (symbol-name)
  "View the documentation on the Scheme symbol SYMBOL-NAME.
If SYMBOL-NAME has more than one reference, you will be asked to
choose one."
  (interactive
   (list
    (let ((x (thing-at-point 'symbol)))
      (let ((x (and x (downcase x))))
        (if (and x (intern-soft x scheme-lookup-symbols))
            ;; We're looking at a known scheme symbol.
            ;; Don't prompt, just use it.
            x
          ;; We're not looking at a known scheme symbol.
          ;; Prompt, where the selection must be a known scheme symbol.
          (completing-read "Look up Scheme symbol: "
                           scheme-lookup-symbols #'boundp t x
                           'scheme-lookup-history))))))
  ;; Now we've got a string to look up, bound to symbol-name.
  (let ((symbol (intern-soft (downcase symbol-name) scheme-lookup-symbols)))
    (let ((references (and symbol (boundp symbol) (symbol-value symbol))))
      (if (not (consp references))
          (error "Can't happen.")
          (if (= 1 (length references))
              (eval (car references))
              (apply 'scheme-lookup-choose symbol-name (reverse references)))))))

(defun scheme-lookup-choose (symbol-name &rest choices)
  "This is a helper function that `scheme-lookup' uses when there
is more than one reference to SYMBOL-NAME.  It pops up a help
buffer and with a button for each choice; clicking one will
complete the lookup."
  (with-output-to-temp-buffer "*Help*"
    (with-current-buffer standard-output
    (insert "Please select which reference to ‘" symbol-name "’ you intended:\n")
    (mapc
     (lambda (c)
       (let ((n 4))
         (insert "\n  • ")
         (insert-text-button
          (scheme-lookup-pretty-name c)
          'action (lambda (b)
                    (eval (button-get b 'expression)))
          'expression c)))
     choices)
    (switch-to-buffer standard-output))))

(defun scheme-lookup-pretty-name (expr) ; -> string
  "This is a helper function that `scheme-lookup-choose' uses to
get a readable label for its choice buttons.  EXPR is a lisp
expression of the form (scheme-lookup-REF . ARGS).

This function examines the `scheme-lookup-pretty-name' property
of scheme-lookup-REF.

- If it is a string, it returns that string.

- If it is a function, it returns the result of applying that
  function to ARGS.

- Otherwise, it returns a string representation of EXPR."
  (if (not (consp expr))
      (error "Can't happen.")
      (let ((x (get (car expr) 'scheme-lookup-pretty-name)))
        (cond ((stringp x)
               x)                       ; string constant
              ((functionp x)
               (apply x (cdr expr)))    ; lambda
              ((null x)
               (format "%S" expr))      ; not set
              (t
               (error "Can't happen."))))))


;;;; Generic lookup functions
;;;;
;;;; When providing scheme-lookup with a reference, define your
;;;; scheme-lookup-REF function in terms of one of these functions.

;;; Use this for info manuals.
(defun scheme-lookup-info (page node)
  (info page)
  (Info-index node))

;;; Use this for HTML references produced by tex2page.
(defun scheme-lookup-tex2page (base &optional chapter page)
  (browse-url
   (cond ((and chapter page)
          (format "%s-Z-H-%d.html#node_idx_%d" base chapter page))
         (chapter
          (format "%s-Z-H-%d.html" base chapter))
         (t
          (format "%s.html" base)))))


;;;; Lookup table population
;;;;
;;;; Use this function to tell scheme-lookup about a reference.
(defun scheme-lookup-add-reference (expr)
  "Given an expression of the form

    (LOOKUP-FUNCTION \"SYMBOL\" ARGUMENTS ...)

Add EXPR to the list of scheme references for SYMBOL."
  (assert (consp expr))
  (let ((symbol  (intern (cadr expr) scheme-lookup-symbols)))
    (if (boundp symbol)
        (push expr (symbol-value symbol))
        (set symbol (list expr)))))


(provide 'scheme-lookup)
