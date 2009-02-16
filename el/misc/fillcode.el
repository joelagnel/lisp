;;; fillcode.el --- Fillcode minor mode
;;
;; Fillcode
;; http://snarfed.org/space/fillcode
;; Copyright 2005 Ryan Barrett <fillcode@ryanb.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained at
;; http://www.gnu.org/licenses/gpl.html or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; This minor mode enhance the fill functions when in source code major modes,
;; such as c-mode, java-mode, and python-mode. Specifically, it provides a new
;; fill function that intelligently fills some parts of source code, like
;; function calls and definitions, if the language mode's fill function
;; doesn't already.
;;
;; M-x fillcode-mode toggles fillcode-mode on and off in the current buffer.
;;
;; TODO:
;; - fill expressions outside parentheses
;; - somehow include the assignment = operator and <, > in
;;   fillcode-fill-point-re. (how to handle <, > with e.g. templates?!?)
;; - add beginning-of-statement fns for more languages
;; - make it compatible with filladapt-mode
;; - handle c++ and python comments better. (the line after them doesn't get
;;   indented.)
;; - fillcode still fills previous statement in cc-mode multi-line comments
;; - if non-sticky, first arg goes to next line but indents to same place! boo.

(defconst fillcode-version "0.5")

(require 'cl)  ; for the case macro

(require 'cc-bytecomp)  ; for c-in-literal and c-literal-limits
(cc-require 'cc-engine)

; gnu emacs supports optional forms as the last arguments to
; define-minor-mode; they're evaluated when the minor mode is enabled or
; disabled. this is really nice, but xemacs' define-minor-mode doesn't have
; it, so i have to advise the fillcode-mode function instead (below).
(define-minor-mode fillcode-mode
  "Toggle fillcode mode.
With no argument, this command toggles the mode. Non-null prefix argument
turns on the mode. Null prefix argument turns off the mode.

Fillcode mode can intelligently fill some parts of source code, like function
calls and definitions, in many languages.

To see what version of fillcode you are running, enter `\\[fillcode-version]'.

For more information, see http://snarfed.org/space/fillcode"
 nil         ;; initial value
 " Fillcode" ;; mode line indicator
 nil         ;; keymap
 )

(defun fillcode-version ()
  "Echo the current version of fillcode mode in the minibuffer."
  (interactive)
  (message "Using fillcode mode version %s" fillcode-version))

(defadvice fillcode-mode (after fillcode-mode-setup-and-teardown)
 ;; run these when fillcode-mode is enabled or disabled. the fillcode-mode var
 ;; is set before these run.
 (make-local-variable              ;; The primary fill function. Fillcode only
  'fillcode-wrapped-fill-function) ;; runs if this returns nil.
 (make-local-variable 'fill-paragraph-function)

 (if fillcode-mode
     ; this runs when fillcode is enabled...
     (progn 
       (if (not (eq fill-paragraph-function 'fillcode-fill-paragraph))
           (setq fillcode-wrapped-fill-function fill-paragraph-function)
         (setq fillcode-wrapped-fill-function nil))
       (setq fill-paragraph-function 'fillcode-fill-paragraph)
       (ad-activate 'c-fill-paragraph))

   ; ...and this runs when it's disabled.
   (progn
     (if (eq fill-paragraph-function 'fillcode-fill-paragraph)
         (setq fill-paragraph-function fillcode-wrapped-fill-function))
     (ad-deactivate 'c-fill-paragraph))
   ))

(ad-activate 'fillcode-mode)


(defadvice c-fill-paragraph (around fillcode-if-in-code)
  "If in fillcode-mode, fill code when in `cc-mode'.

`cc-mode' replaces `fill-paragraph' with its own function, `c-fill-paragraph',
which only calls fill-paragraph if it's inside a comment or string literal, and
narrows to that comment or string literal. Fillcode operates on code itself, so
it needs a chance to run (without narrowing!), which this advice provides."
 (if fillcode-mode
     (progn
       (let ((fill-paragraph-function nil))
         ad-do-it)
       (fillcode-fill-paragraph arg))) ; arg is c-fill-paragraph's arg
  )


(defgroup fillcode nil
  "Fill code"
  :group 'fill)

(defcustom fillcode-open-paren-sticky nil
  "If non-nil, fillcode will not fill after an open parenthesis.
For example, if non-nil, fillcode will fill this:

foo(bar, baz(baj))

to

foo(bar,
    baz(baj))

If nil, it will fill to:

foo(bar, baz(
    baj))"
  :type 'boolean
  :group 'fillcode)

(defcustom fillcode-fill-point-re
  ;; note that inside a [...] group, - is used to specify ranges...so
  ;; to match it itself, it has to be at the beginning or end.
  (concat "\\(\\("
                 "[,/+]\\|"
                 "==\\|!=\\|||\\|&&\\|<=\\|>="
              "\\)"
              "[^=>/*+]"
          "\\|"
          ; an open paren is a fill point only if it's not followed by a close
          ; paren
          "([^)]\\)"
          "\\|"
          ; asterisks are used as pointers in c and c++, so to be
          ; conservative, they're only fill points if they're surrounded by
          ; whitespace
          "\\ \\*\\ "
          "\\|"
          ; minus signs are only fill points if they're not being used as a
          ; negative sign
          "-[ \t\n(]"
          )
  "A regular expression used to find the next fill point.
A fill point is a point in an expression where a newline can reasonably be
inserted. This regular expression identifies fill points. It must end one
character *after* the fill point ends.

You may modify this to allow fillcode to handle new languages.

Note that the single = (assignment) operator and < and > operators are
unfortunately absent."
  :type 'string
  :group 'fillcode)

(defcustom fillcode-whitespace-chars
  " \t\n"
  "The characters that fillcode considers whitespace."
  :type 'string
  :group 'fillcode)


(defun fillcode-fill-paragraph (arg &optional arg2 arg3 arg4)
  "Fill code at point if `fillcode-wrapped-fill-function' is nil.

If `fillcode-wrapped-fill-function' is nil, fills code. If it's
non-nil, runs it first, and only fills code if it returns nil.

Intended to be set as `fill-paragraph-function'."
  (save-excursion
    ; first, consider calling the wrapped fill function
    (let ((ret
           (cond
            ; if we're in cc-mode, this was called by the `c-fill-paragraph'
            ; advice. so, don't call it again, it'd recurse infinitely.
            ((eq fillcode-wrapped-fill-function 'c-fill-paragraph)
             nil)
            ; `python-fill-paragraph' in CVS Emacs' python.el always returns
            ; t (grr!), so instead of looking at its return value, we fill if
            ; the end of the line is not in a comment or string literal
            ((and (eq major-mode 'python-mode)
                  (not (save-excursion (end-of-line) (fillcode-in-literal))))
             nil)
            ; otherwise, if it's set, call the wrapped fill function
            (fillcode-wrapped-fill-function
             (funcall fillcode-wrapped-fill-function arg))
            )))


      ; if the wrapped fill function did something, don't do anything more
      (if ret
          ret
        ; otherwise, fill. use the `filled' var to remember if we filled
        ; anything, so we can correctly return t if we did, nil otherwise.
        (progn
          (fillcode-beginning-of-statement)
          (setq filled nil)
          (while (search-forward "(" (fillcode-end-of-statement) t)
            (fillcode arg)
            (setq filled t arg nil))
          filled)
        ))
    ))



(defun fillcode (arg)
  "Fill code at point.
The actual function-call-filling algorithm. Fills function calls and prototypes
if it thinks the point is on a statement that has one.

If a prefix argument is provided, the first token after the first open
parenthesis is automatically filled."
  (interactive)
  (fillcode-collapse-whitespace-forward)

  ; short-circuit: if it's an empty parenthetical expression, return
  (if (equal ")" (char-to-string (char-before)))
      t

    ; the main loop. advances through the statement, normalizing whitespace and
    ; deleting newlines along the way. the main loop should run once once and
    ; only once for each printable character. when we hit the fill-column, fill
    ; intelligently.
    (catch 'closeparen
      (while (char-after)
        (let ((c (char-to-string (char-after))))
;;           (edebug)
          ; fill if we need to
          (if (or arg (fillcode-should-fill))
              (progn
                (catch 'no-fill-point
                  (fillcode-find-fill-point-backward)
                  t)
                (insert "\n")
                (indent-according-to-mode)
                (setq arg nil)))
          ; open parenthesis is our recursive step; recurse!
          (if (equal c "(") (fillcode nil))
          ; close parenthesis is our base case; return!
          (if (equal c ")")
              (throw 'closeparen t))
          ; next!
          (fillcode-collapse-whitespace-forward)
          )))

    ; if this is a nested call, and we filled, newline after the next comma
;;     (if (and arg
;;              (save-excursion
;;                (skip-chars-backward "^(" (point-at-bol))
;;                (eq (point) (point-at-bol))))
;;         (progn
;;           (collapse-whitespace-forward) ; move past close paren
;;           (if (equal "," (char-to-string (char-after)))
;;               (progn
;;                 (delete-horizontal-space)
;;                 (forward-char)          ; move past comma
;;                 (collapse-whitespace-forward)
;;                 (newline-and-indent)))))

    ; return t to indicate that we filled something
    t))


(defun fillcode-beginning-of-statement ()
  "Go to the beginning of the statement that point is currently in.
Calls the major mode's beginning-of-statement function, if it has one.
Otherwise, for safety, just goes to the beginning of the line.
"
  (case major-mode
    ((c-mode c++-mode java-mode objc-mode perl-mode)
     ; if we're at the beginning of the statement, `c-beginning-of-statement-1'
     ; will go to the *previous* statement. so go to the end of the line first.
     (end-of-line)
     (condition-case nil (c-beginning-of-statement-1) (error nil))
     (beginning-of-line))
    ((python-mode)
     (if (functionp 'py-goto-statement-at-or-above)
         (py-goto-statement-at-or-above)
       (progn (python-beginning-of-statement) t)))

    ;`c-beginning-of-statement-1' might be a good fallback for unknown
    ;languages, but it occasionally fails badly, e.g. in `perl-mode'.
    (otherwise
     (beginning-of-line)))  ; default
  )


(defun fillcode-end-of-statement ()
  "Return the end position of the statement that point is currently in.
Uses the major mode's end-of-statement function, if it has one. Otherwise,
for safety, just uses the end of the line."
  (case major-mode
    ((c-mode c++-mode java-mode objc-mode perl-mode)
     ; TODO: what do do about this? c-end-of-statement looks for a semicolon,
     ; which is overly aggressive when you only want to fill a parenthesized
     ; expression (e.g. an if () condition) or a function prototype.
     ;(c-end-of-statement))
     (point-at-eol))

    ((python-mode)
     (save-excursion
       (let ((start (point)))
         (if (if (functionp 'py-goto-statement-below)
                 (py-goto-statement-below) (python-next-statement))
             (search-backward ")" start 'p)
           (condition-case nil (forward-char) (error nil)))
           (point-at-eol))))

    ;`c-end-of-statement' might be a good fallback for unknown languages,
    ; but it occasionally fails badly, e.g. in `perl-mode'.
    (otherwise
     (point-at-eol)))
  )


(defun fillcode-collapse-whitespace-forward ()
  "Delete newlines, normalize whitespace, and/or move forward one character.
Specifically, no spaces before commas or open parens or after close parens,
one space after commas, one space before and after arithmetic operators.
Except string literals and comments, they're left untouched. Then advance
point to next non-whitespace char."
  (let ((whitespace-re (concat "[" fillcode-whitespace-chars "]")))
    (cond
     ; if we're in a string literal or comment, skip to the end of it 
     ((fillcode-in-literal)
      ; TODO: maybe goto-char (cdr c-literal-limits) here would be faster?
      (forward-char))

     ; if we're at the end of the line, pull up the next line
     ((eolp)
      (delete-indentation t))

     ; if we're on whitespace, delete it. if that brings us to a fill point,
     ; fall down to the logic below. otherwise, normalize to exactly one space
     ; and continue.
     ((looking-at whitespace-re)
      (delete-horizontal-space)
      (if (and (not (looking-at fillcode-fill-point-re))
               (not (looking-at "(")))
          (progn (fixup-whitespace)
                 (if (looking-at whitespace-re) (forward-char)))))

     ; if we're before a non-comma/open paren fill point, insert a space
     ((and (looking-at fillcode-fill-point-re)
           (not (looking-at "[,(]")))
      (progn (insert " ") (goto-char (match-end 0))))

     ; if we're after a fill point, insert a space. (note that the fill point
     ; regexp ends at the first char *after* the operator.)
     ((and (save-excursion
             (condition-case nil
                 (progn (forward-char)
                        (re-search-backward fillcode-fill-point-re
                                            (point-at-bol)))
               (error nil)))
           (equal (point) (1- (match-end 0)))
           (not (save-excursion (backward-char) (fillcode-in-literal))))
      (progn (fixup-whitespace)
             ; skip *past* the char we were on originally. if we inserted a
             ; space, that's two chars forward, otherwise just one.
             (forward-char (if (looking-at " ") 2 1))))

     ; ...otherwise, base case: advance one char
     (t (forward-char)))))

(defun fillcode-should-fill ()
  "Return t if we should fill at the last fill point, nil otherwise.

We should fill if:

- there's a fill point on this line, AND EITHER

- the current char is at or beyond `fill-column' OR

- the current char is the close paren of a nested call, and the next char is a
  comma. (have to look ahead like this so that we don't end up past the close
  paren, and miss the close paren base case, which would screw up the stack.)"
  (and
   ; past fill-column?
   (or (>= (current-column) fill-column)
       ; this is a close paren, and next is a fill point past fill-column?
       (save-excursion
         (and (looking-at ")")
              (skip-chars-forward (concat ") " fillcode-whitespace-chars))
              (looking-at fillcode-fill-point-re)
              (>= (current-column) fill-column))))
   ; fill point on this line?
   (save-excursion
     (catch 'no-fill-point
       (fillcode-find-fill-point-backward)
       t))
   ))


;; (defun fillcode-find-fill-point ()
;;   "Move point to the closest fill point on the current line.
;; Fill points are commas, open parens (if fillcode-open-paren-sticky is nil) and
;; eventually arithmetic operators, ||s, &&s, etc. This function attempts to find
;; the closest fill point that's before point and before `fill-column'. If there
;; are no appropriate fill points before point, it settles for the closest one
;; after point.

;; If there's no fill point on the current line, throws `no-fill-point'."
;;   (move-to-column fill-column)

;;   (if (not (catch 'no-fill-point
;;              (fillcode-find-fill-point-backward)   
;; ;;              (if (>= (current-column) fill-column)
;;                  ; we started before fill-column. how'd we end up after it?!?
;; ;;                  (throw 'find-fill-point-backward-moved-past-fill-column nil))
;;              t))
;;       ; no fill point before fill-column! take the closest one after.
;;       (fillcode-find-fill-point-forward))

;; ;;   (goto-char (1- (match-beginning 0)))
;;   )


;; (defun fillcode-find-fill-point-forward ()
;;   (fillcode-find-fill-point-helper 're-search-forward (line-end-position)))

(defun fillcode-find-fill-point-backward (&optional prefixed)
  ; the fill point regexp ends at the first char *after* the
  ; operator...so, move forward one char before searching.
  (forward-char)
  (fillcode-find-fill-point-helper 're-search-backward (point-at-bol)
                                   prefixed))

(defun fillcode-find-fill-point-helper (re-search-fn bound &optional prefixed)
  "Move to the closest fill point on the current line.
Fill points are commas, open parens (if fillcode-open-paren-sticky is nil) and
eventually arithmetic operators, ||s, &&s, etc. This function finds the
closest one either before or after point, depending on `forward'.

If there's no fill point on the current line, throws `no-fill-point'."
  (condition-case nil
      (funcall re-search-fn fillcode-fill-point-re bound)
    (search-failed (throw 'no-fill-point nil)))

  (goto-char (1- (match-end 0)))

  ; can't fill if we're in or immediately after a comment or string literal,
  ; or - if we're sticky and not prefixed - in an open paren.
  (if (or (fillcode-in-literal)
          (and fillcode-open-paren-sticky (not prefixed)
               (equal "(" (substring (match-string 0) 0 1)))
          (save-excursion (backward-char) (fillcode-in-literal)))
      (fillcode-find-fill-point-helper re-search-fn bound))
  )


(defun fillcode-in-literal ()
  "Return non-nil if inside a comment or string literal, nil otherwise.
Determines whether point is inside a comment, string literal, or other segment
that shouldn't be normalized or filled. Piggybacks on the major modes, since
it will usually have its code for this.

Unfortunately, the major modes' in-literal functions (e.g. `c-in-literal' do
*not* consider literals' start tokens (\", ', /*, //, #) to be part of the
literal, so they return nil if point is on the start token. We want them to
return non-nil if we're past the first char of the start token, so
`fillcode-in-literal' returns non-nil instead."
  (let ((in-literal-fn
         (case major-mode
           ((python-mode) (if (functionp 'py-in-literal)
                              'py-in-literal 'python-in-string/comment))
           (otherwise 'c-in-literal)))
        (literal-start-tokens
         (case major-mode
           ((c-mode c++-mode java-mode objc-mode perl-mode)
            '("//" "/*"))
           (otherwise
            '()))))

    (or
     ; are we in a literal?
     (funcall in-literal-fn)
     ; are we in any of the literal start tokens?
     (eval (cons 'or (mapcar (lambda (x)
                               (equal x (buffer-substring
                                         (max (1- (point)) (point-min))
                                         (min (1+ (point)) (point-max)))))
                             literal-start-tokens))))
    ))


(defun fillcode-inside (str &optional moved)
  "Return non-nil if point is on the given string.
Here, \"on\" means that point is on any of the characters in the string."
  (let ((moved (if moved moved 0)))
    (condition-case nil
        (if (< moved (length str))      ; base case
            (or (equal str
                       (buffer-substring (point)
                                         (min (+ (point) (length str))
                                              (point-max))))
                (save-excursion
                  (backward-char)
                  (fillcode-inside str (1+ moved)))))
      (error nil))
      ))


(provide 'fillcode)

;;; fillcode.el ends here
