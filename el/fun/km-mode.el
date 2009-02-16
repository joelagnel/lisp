;;; km-mode.el --- Major mode for editing Knowledge Machine code.

;; Copyright (C) 2004 Joe Corneli  <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Tue Mar  2 19:45:25 CST 2004>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

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

;;; Commentary:

;; The major mode for editing KM code.  Since KM is based on Lisp,
;; this mode is based on Lisp mode.  This verision of km-mode.el
;; corresponds to KM ``2.0.4''. Thanks to German Monroy for
;; refering me to Scott Andrew Borton's mode creation
;; tutorial. Please contact me with suggestions.
;;
;; Some comments for on how you can set up Emacs to use this mode
;; follow.
;;
;; You can put this file in a directory called ~/site-lisp/ and add
;; these lines to your .emacs:
;;
;; (setq global-font-lock-mode t)
;; (add-to-list 'load-path "~/site-lisp/")
;; (load "km-mode")
;;
;; Files with the .km suffix or a "-*- Mode: KM -*-" string in the
;; first line will then be loaded in KM mode. You can also switch
;; into KM mode with the command M-x km-mode.

;;; Code:

(require 'lisp-mode)

(defvar km-mode-hook nil)

;; We will parallel lisp mode. The functions in the following list
;; won't quite work, but maybe we can write some code that does
;; something similar.  Some of the functions from the lisp mode
;; keymap will work, and those are bound just below.

;; C-c C-a         lisp-show-arglist
;; C-c C-c         lisp-compile-defun
;; C-c C-d         lisp-describe-sym
;; C-c C-e         lisp-eval-defun
;; C-c C-f         lisp-show-function-documentation
;; C-c C-k         lisp-compile-file
;; C-c C-v         lisp-show-variable-documentation

(defvar km-mode-map
  (let ((km-mode-map (make-keymap)))
    (define-key km-mode-map "\t" 'lisp-indent-line)
    (define-key km-mode-map "\C-c\C-r" 'lisp-eval-region)
    (define-key km-mode-map "\C-c\C-l" 'lisp-load-file)
    (define-key km-mode-map "\C-c\C-z" 'switch-to-lisp)
    km-mode-map)
  "Keymap for KM major mode")

;; is it bad form to set the auto-mode-alist inside the mode?  I
;; personally think it might be, but this makes it easier for
;; novice users.
(add-to-list 'auto-mode-alist '("\\.km\\'" . km-mode))

;; adapted from ttn's ricette-mode.el
;; Nota bene: new faces are automatically customizable.
(defmacro km-new-face (name color doc)
  (let ((prop (intern (concat "km-" (symbol-name name) "-face"))))
    `(progn
       (defvar  ,prop ',prop)
       (defface ,prop '((t (:foreground ,(symbol-name color)))) ,doc))))

(km-new-face anon gray50 "Face for anonymous instances.")
(km-new-face nonanon darkgreen "Face for non-anonymous instances.")
(km-new-face slot orange "Face for standard slot names.")

;;; Font lock keywords.

;; Please see the documentation for the function
;; font-lock-add-keywords.  The section Font Lock/Font Lock mode in
;; the emacs manual may also be helpful.

;; Simplest thing is just to highlight named instances.  It could
;; possibly be good to have the syntax for names of instances be
;; directly customizable - but then again, anyone using this will
;; have access to the code, so customizability of the regexp
;; probably isn't essential.
(defconst km-font-lock-keywords-1
  (list '(;; (\\*\\<\\sw+\\>\\) 
          ;; -- I was just using a word, but
          ;; that caused trouble with things like *Eng_lish. It
          ;; would be good to check the exact syntax from KM at
          ;; some point, and match it exactly here. For now, I just
          ;; say we stop when we enounter white space or a paren.
          "\\(\\*\\<[^ ')(]*\\>\\)" 
          . km-nonanon-face) 
        ;; I like to use grey here.
        '("\\(_\\<[^ ')(]*\\>\\)" 
          . km-anon-face))
  "Minimal highlighting expressions for KM mode")

(defconst km-font-lock-keywords-2
  (append km-font-lock-keywords-1
          (list
           (list 
            (concat
             "^\\s-*(" (regexp-opt
                        '(;; standard slot names
			  ;; (there are surely some missing)
                          "instance-of"
                          "domain"
                          "range"
                          "superclasses" ; always plural?
                          "subclasses"
                          "subslots"
                          "superslots"
                          "fluent-status"
                          "situation-specific"
                          "cardinality"
                          "called"       ; is this one relevant?
                          "inverse") t))
            ;; At first I was not sure what the numbers (like `1'
            ;; here) mean.. but they keep the parenthesis at the
            ;; beginning from being highlighted (and also don't let
            ;; me tack an optional `-of' onto the end of the
            ;; expressions). [See documentation referenced above.]
            '(1 km-slot-face))
           (list
            (concat
             ;; this leading regexp says that our key word either
             ;; follows a space or a leftparen -- there are a few
             ;; bugs in here with repeated keywords apparently --
             ;; for example, in the sequence "(where the has", not
             ;; all of the code is highlighted, but I think it
             ;; should be. Not sure what's going wrong.
             "\\([ (]\\)" (regexp-opt
                    '(;; You aren't allowed to ascribe your own meanings to
                      ;; these.  See the constant `*reserved-keywords*' in
                      ;; KM. There are a few other reserved keywords (mostly
                      ;; binary operators) to address.  At some point, it might
                      ;; be beneficial to sort this list into some smaller
                      ;; sub-categories.
                      "a" "a+" "a-prototype" "add-clones-to" "allof" "allof2"
                      "also-has" "an" "and" "andify" "anonymous-instancep" 
                      "append" "are" "at-least" "at-most" "bag" "clone" "comm"
                      "constraint" "constraints-for" "covers" "curr-situation"
                      "delete" "do" "do-and-next" "do-plan" "do-script" "else"
                      "end-situation" "end-theory" "evaluate" "evaluate-all"
                      "evaluate-paths" "every" "exactly" "excluded-values" 
                      "fluent-instancep" "forall" "forall-bag" "forall-bag2"
                      "forall-seq" "forall-seq2" "forall2" "format" "has"
                      "has-definition" "has-value" "hide-theory" "if"
                      "ignore-result" "in-every-situation" "in-situation" 
                      "in-theory" "includes" "is" "is-covered-by" "isa"
                      "is-subsumed-by" "is-superset-of" "km-format" 
                      "make-phrase" "make-sentence" "must" "must-be-a"
                      "mustnt-be-a" "new-context" "no-inheritance" "not"
                      "numberp" "of" "oneof" "oneof2" "or" "possible-values"
                      "print" "quote" "reverse" "rules-for" "sanity-check"
                      "see-theory" "seq" "set-constraint" "set-filter" "showme"
                      "showme-all" "showme-here" "some" "spy" "subsumes" "the"
                      "the+" "the-class" "the1" "the2" "the3" "theN" "theNth"
                      "thelast" "then" "theoneof" "theoneof2" "trace" "unspy"
                      "untrace" "visible-theories" "where" "with"
                      ) t)
             "\\(\\>[ \n]\\)")
            '(2 font-lock-keyword-face)))))

;; This is taken right out of lisp mode.  But it might be good to
;; add things like `Self', `It', etc.  ... and other important, but
;; not reserved, keywords at some point.
(defconst km-font-lock-keywords-3
  (append km-font-lock-keywords-2
		  (list
                   ;; Words in \\[] tend to be for `substitute-command-keys'.
                   '("\\\\\\\\\\[\\(\\sw+\\)]" 1 font-lock-constant-face prepend)
                   ;; Words inside `' tend to be symbol names.
                   '("`\\(\\sw\\sw+\\)'" 1 font-lock-constant-face prepend)
                   ;; Constant values.
                   '("\\<:\\sw+\\>" 0 font-lock-builtin-face)
                   ;; ELisp and CLisp `&' keywords as types. 
                   ;; (I don't know if this comes up in KM or not; 
                   ;; I don't think so, but maybe it does.) --jac
                   '("\\&\\sw+\\>" . font-lock-type-face))))

(defvar km-font-lock-keywords km-font-lock-keywords-3
  "Default highlighting expressions for KM mode")

;; at some point, more customizations might be done.
(defvar km-mode-syntax-table
  (let ((km-mode-syntax-table lisp-mode-syntax-table))
    km-mode-syntax-table))

(defun km-mode ()
  "Major mode for editing KM code."
  (interactive)
  ;; We want to use essentially everything from lisp mode except
  ;; for its font-lock.
  (kill-all-local-variables)
  (set-syntax-table km-mode-syntax-table)
  (use-local-map km-mode-map)
  (setq major-mode 'km-mode)
  (set (make-local-variable 'font-lock-defaults) '(km-font-lock-keywords))
  (setq major-mode 'km-mode)
  (setq mode-name "KM")
  ;; Now a bunch of stuff copied from the function
  ;; lisp-mode-variables! 
  ;; (See this function for some additional commentary.)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lisp-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;;;* [^ \t\n]\\|(")
  (make-local-variable 'outline-level)
  (setq outline-level 'lisp-outline-level)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-add)
  (setq comment-add 1)
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression)
  (make-local-variable 'multibyte-syntax-as-symbol)
  (setq multibyte-syntax-as-symbol t)
  ;; run hook before beginning
  (run-hooks 'km-mode-hook))

(provide 'km-mode)