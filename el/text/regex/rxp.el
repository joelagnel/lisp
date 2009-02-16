;;; rxp.el --- regular expression parsing and predicates
;;
;; Author: Michelangelo Grigni <mic@mathcs.emory.edu>
;; Status: parsing ok, predicates are unfinished and daunting
;; Created: 9 July 1997

;; Emacs regexp tokens are either a single character, or two
;; characters starting with a slash.  However, \s and \S always gobble
;; the next character, and "[" has some simple rules to gobble a
;; character range terminated by a "]".
;;
;; More info: (emacs)Regexps, or (xemacs)Regexps

;; Old method: used a keymap and execute-kbd-macro to tokenize the
;; regexp.  Slick, but probably slower than the big loop here.
;;
;; Todo: emulate the unusual + ? * $ ^ implicit quoting rules (partly done)

(eval-and-compile
  (require 'cl))			; just push and pop

;; This should have no global value:
(makunbound 'rxp-stack)

(defun rxp-syntax (char)
  (interactive "c")
  ;; Check that char is listed in (syntax-designator-chars):
  (push (list this-command char) rxp-stack))
(defalias 'rxp-syntax-not 'rxp-syntax)

(defun rxp-range (char)
  (interactive "c")			; read first char unconditionally
  (let (clist done negated)
    ;; If first char is "^", note it and get another.
    (and (eq char ?^)
	 (setq negated t
	       clist (list char)
	       char (read-char)))
    (while (not done)
      (setq clist (cons char clist)
	    char (read-char)
	    ;; test every character after the first:
	    done (eq ?\] char)))
    (push (list
	   ;; (if negated 'rxp-range-not 'rxp-range)
	   'rxp-range
	   (concat (nreverse clist)))
	  rxp-stack)))

;; Tokenizer:
(defun rxp-tokify (regexp)
  "Given REGEXP, return it as a flat list of tokens.
Characters stand for themselves, symbols stand for operators.
Certain subexpressions \(character ranges and syntax matchers\)
are left as regular expression strings."
  ;; Could defvar some of these constants in this function.
  ;; Could use a syntax table to speed this up.
  (let ((i 0) (len (length regexp)) ch ret reg)
    (while (< i len)
      (setq ch (aref regexp i) i (1+ i))
      ;;  Is this ch an operator character?
      (if (or
	   ;; These chars are always operators (note no \]).
	   ;; Actually, $ is not always an operator, but for
	   ;; this round we assume that it is (see code below):
	   (memq ch '(?\. ?\$ ?\[))
	   ;; ^ is an operator only after a "start token":
	   (and
	    (eq ch ?^)
	    (memq (car ret) '(nil \( \|)))
	   ;; is fixed when those tokens are read.
	   ;; * + ? are operators when *not* after a "start token":
	   (and
	    (memq ch '(?*  ?+ ?\?))
	    (not (memq (car ret) '(nil \( |))))
	   ;; Escaped char?  May or may not be an operator char.
	   (and
	    (eq ch ?\\)
	    (progn
	      (or (< i len) (error "bad regexp: trailing \\"))
	      (setq ch (aref regexp i) i (1+ i))
	      ;; These are operators, the others are literals:
	      (memq ch '(?\( ?\) ?\| ?\< ?\> ?\` ?\' ?b ?B ?w ?W ?s ?S))
	      )))
	  ;; It is an operator! Special cases: [, s, and S:
	  (cond
	   ((eq ch ?\[)
	    (or (string-match "\\`\\^?]?[^]]*]" (substring regexp i))
		(error "bad regexp: unmatched ["))
	    (let ((len (match-end 0)))
	      ;; Push the range expression as a regexp string:
	      (push (substring regexp (1- i) (+ i len)) ret)
	      (setq i (+ i len))))
	   ((memq ch '(?s ?S))
	    (or (< i len) (error "bad regexp: trailing \\%c" ch))
	    (let ((sch (aref regexp i)))
	      ;; Check that it is a valid syntax character:
	      ;; (syntax-string-to-code (char-to-string sch))
	      (setq i (1+ i))
	      (push (substring regexp (- i 3) i) ret)))
	   ;; Default: ordinary operator char, push it as a symbol:
	   (t
	    (push (intern (char-to-string ch)) ret)))
	;; Not an operator, push as a literal char:
	;; (push ch ret)
	(push (make-string 1 ch) ret)
	)
      ;; Now maybe demote thee preceding $ symbol to a char, unless the
      ;; most recent token is an "end token":
      (and (eq (car (cdr ret)) '$)
	   (not (memq (car ret) '(\) |)))
	   (setcar (cdr ret) "$"))
      ;; end of while loop
      )
    (nreverse ret)))
;; (rxp-tokify "*abc*\\(def\\))")
;; (rxp-tokify "ab\\|[^]\\|cd]]\\|ef\\S_")
;; (rxp-tokify "$abc^")
;; (rxp-tokify "^abc$")
;; (rxp-parse "ab\\|[^]\\|cd]]\\|ef\\S_")

;; Parser:
(defun rxp-parse (regexp)
  "Given REGEXP, convert it to a structured expression."
  (let ((tokens (rxp-tokify regexp))
	(rxp-stack (list '\())	; fake left parenthesis
	token action)
    (while tokens
      (setq token (car tokens) tokens (cdr tokens))
      (if (and (symbolp token)
	       (setq action (get token 'rxp-action)))
	  (funcall action token)
	(push token rxp-stack)))
    (rxp-close '\))			; fake right parentheses
    (if (cdr rxp-stack)
	(error "bad regexp: unmatched open parenthesis"))
    (car rxp-stack)))

(put '* 'rxp-action 'rxp-postfix)
(put '+ 'rxp-action 'rxp-postfix)
(put '\? 'rxp-action 'rxp-postfix)
(put '\) 'rxp-action 'rxp-close)
(put '\| 'rxp-action nil)		; processed later, by rxp-ors

(defun rxp-postfix (tok)		; postfix operators: *, +, ?
  (push (list tok (pop rxp-stack)) rxp-stack))

(defun rxp-memq-trunc (mem list-sym)
  "Look for MEM in list value of LIST-SYM, and truncate there.
Returns the same sublist that (memq ...) would, but also removes
it from the list.  LIST-SYM is modified if MEM is first."
  (let* ((first '(17))			; a cons cell, car is ignored
	 (current (symbol-value list-sym))
	 (previous (setcdr first current)))
    (while (and current (not eq (car current) mem))
      (setq previous current current (cdr current)))
    (if current
	(if (eq (cdr previous) thelist)
	    ;; Truncate list-sym to nil:
	    (set list-sym nil)
	  ;; Truncate later in the list:
	  (setcdr previous nil)))
    current))

(defun rxp-reverse-to-cdr (cdr list)
  "Reverse list up to the specified CDR value, non-destructively.
Like this: (a b c . CDR) --> (c b a)"
  (let (ret)
    (while (and list (not (eq list cdr)))
      (setq ret (cons (car list) ret)
	    list (cdr list)))
    ret))

;; (defun rxp-memq-split (mem list)
;;   "Look for MEM in LIST, and split list if found, non-destructively.
;; Like this:  \(a b MEM c d\) --> \(\(b a\) c d\)"
;;   (let ((cdr (memq mem list)))
;;     (and cdr (cons (rxp-reverse-to-cdr cdr list) (cdr cdr)))))

(defun rxp-close (tok)		; argument is \), ignored
  (let ((tail (memq '\( rxp-stack)))
    (or tail (error "rxp-close: unmatched right parenthesis"))
    (setq rxp-stack
	  (cons (rxp-ors (rxp-reverse-to-cdr tail rxp-stack))
		(cdr tail)))))

(defun rxp-ors (parse)
  ;; Ok, so this is not good general-purpose precedence-driven parsing.
  (if (memq '\| parse)
      (let (ret top sub)
	(setq parse (nreverse parse))
	(while parse
	  (setq top (pop parse))
	  (if (eq top '\|)
	      (progn (push sub ret) (setq sub nil))
	    (push top sub)))
	(push sub ret)
	(cons 'or ret))
    parse))

;; Dumb motivation:

(defun rxp-unsafe-for-gnus-scoring (regexp)
  "Test whether REGEXP is unsafe for gnus SCORE files.
If unsafe, it returns a token indicating the problem."
  ;; The regexp should match within a single line, and we should not
  ;; care where that line is in the buffer.
  (let ((toks (rxp-tokify regexp)))
    (or (car (memq '\` toks))
	(car (memq '\' toks))
	(car (memq ?\n toks))
	(car (memq W toks))
	(apply 'or (mapcar (function
			    (lambda (tok)
			      (and (stringp tok)
				   (string-match tok "\n"))))
			   toks)))))

;; (rxp-safe-for-gnus-scoring ":[^:]*:")

;;; Predicates (probably need more):

(defun rxp-transitions (regexp)
  "Return list of possible `transitions' for REGEXP.
Each transition represents a class of one or more strings
matching the REGEXP, together with any restrictions on the
surrounding text.

A transiton is a four character string:
   LEFT-CONDITION: one of ^, `, W, w, ' '  [' ' means no restriction]
   RIGHT-CONDITON: one of $, ', W, w, ' '
   LEFT-CHAR:      any character
   RIGHT-CHAR:     any character

Or if a transition represents a match of the empty string, it may be
only two characters long."
  ;; ""    --> "  "
  ;; a     --> "  aa" (and maybe "  AA")
  ;; ^     --> "^ "
  ;; $     --> " $"
  ;; \<    --> "Ww"
  ;; \>    --> "wW"
  ;; \`    --> "` "
  ;; \'    --> " '"
  ;; \b    --> "Ww", "wW"
  ;; [a-z] --> "  aa", ..., "  zz"  (a long list)
  ;; \sw   --> "  aa", ... (another long list, depending on syntax table)
  ;;
  ;; Let A and B be regular expressions, T(A) the set of transitions for A:
  ;; A\|B  --> T(A) union T(B)
  ;; AB    --> take all "products" of T(A) T(B), product rules are determined
  ;;           by thinking about concatenation (empty/nonempty cases)
  ;; A*    --> closure of T(A) under product, including empty product "  "
  ;;
  ;; Note: to avoid long lists, we could replace LEFT-CHAR and
  ;; RIGHT-CHAR with single-char regexps of their own (probably just
  ;; ranges).  This would make the product operation a little trickier.
  ;;
  ;; Maybe something like:
  ;; [LEFT-CONSTRAINT RIGHT-CONSTRAINT LEFT-RANGE RIGHT-RANGE]
  ;; to indicate the product of all possible choices of a char from
  ;; the left range and a char from the right range.
  ;;
  ;; Also maybe:
  ;; [LEFT-CONSTRAINT RIGHT-CONSTRAINT LEFT-RANGE '=]
  ;; to indicate the "diagonal set", where the right character is
  ;; constrained to equal the left character.
  ;;
  ;; MAJOR PROBLEM: no well-bounded way to deal with \1, \2, etc.
  todo)

(defun rxp-matches-nothing (regexp)
  "Test whether REGEXP matches no strings at all."
  (not (rxp-transitions regexp)))

(defun rxp-matches-nonempty (regexp)
  "Test whether REGEXP matches some non-empty string."
  ;; Use rxp-transitions, or maybe write a faster test.
  ;; In fact, this returns all the non-empty-string transitions.
  (delete nil (mapcar (lambda (tr) (and (> (length tr) 2) t))
		      (rxp-transitions regexp))))

;;; Second self-contained version (ignores all the above code!)

(defun gnus-score-regexp-bad (regexp)
  "Test whether REGEXP is safe for Gnus scoring.
A regexp is unsafe if it matches newline or a buffer boundary.

If the regexp is good, return nil.  If the regexp is bad, return a
cons cell (SYM . STRING), where the symbol SYM is `new' or `bad'.
In the `new' case, the string is a safe replacement for REGEXP.
In the `bad' case, the string is a unsafe subexpression of REGEXP,
and we do not have a simple replacement to suggest.

See `(Gnus)Scoring Tips' for examples of good regular expressions."
  (let (case-fold-search)
    (and
     ;; First, try a relatively fast necessary condition.
     ;; Notice ranges (like [^:] or [\t-\r]), \s>, \Sw, \W, \', \`:
     (string-match "\n\\|\\\\[SsW`']\\|\\[\\^\\|[\0-\n]-" regexp)
     ;; Now break the regexp into tokens, and check each:
     (let ((tail regexp)		; remaining regexp to check
	   tok				; current token
	   bad				; nil, or bad subexpression
	   new				; nil, or replacement regexp so far
	   end)				; length of current token
       (while (and (not bad)
		   (string-match
		    "\\`\\(\\\\[sS]?.\\|\\[\\^?]?[^]]*]\\|[^\\]\\)"
		    tail))
	 (setq end (match-end 0)
	       tok (substring tail 0 end)
	       tail (substring tail end))
	 (if;; Is token `bad' (matching newline or buffer ends)?
	     (or (member tok '("\n" "\\W" "\\`" "\\'"))
		 ;; This next handles "[...]", "\\s.", and "\\S.":
		 (and (> end 2) (string-match tok "\n")))
	     (let ((newtok
		    ;; Try to suggest a replacement for tok ...
		    (cond ((string-equal tok "\\`") "^") ; or "\\(^\\)"
			  ((string-equal tok "\\'") "$") ; or "\\($\\)"
			  ((string-match "\\[\\^" tok) ; very common
			   (concat (substring tok 0 -1) "\n]")))))
	       (if newtok
		   (setq new
			 (concat
			  (or new
			      ;; good prefix so far:
			      (substring regexp 0 (- (+ (length tail) end))))
			  newtok))
		 ;; No replacement idea, so give up:
		 (setq bad tok)))
	   ;; tok is good, may need to extend new
	   (and new (setq new (concat new tok)))))
       ;; Now return a value:
       (cond
	(bad (cons 'bad bad))
	(new (cons 'new new))
	;; or nil
	)))))
;; (setq print-escape-newlines t)
;; (gnus-score-regexp-bad "^\\(Re\\)?[^a-z]*$")
;; (gnus-score-regexp-bad "^\\(Re\\)?[ A-Z\n]*$")
;; (gnus-score-regexp-bad "\\`abc\\'")
;; (gnus-score-regexp-bad "a\\Wb")

;;; regexpp.el ends here
