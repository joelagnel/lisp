;;; eev-bounded.el -- support for bounded functions for eev.

;; Copyright (C) 2006 Free Software Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2006nov12
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-bounded.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-bounded.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>
;;
;; History: this used to be part of eev.el, but in sep/2006 I decided
;; to rewrite it and ended up splitting it into another file.
;; 2006nov12: eev.el and the "eev block" in ~/.emacs now load this
;; instead or loading "eev-bounded-old.el".

;;; Commentary:

;; This file adds support for "bounded functions" to eev. For example:
;; `M-x eev' saves the region between point and mark into the
;; temporary script file; `M-x eev-bounded' saves the region around
;; point, up to the first occurences of a certain delimiters before
;; and after point, into the temporary script file.

;; Naming conventions: eeb-something, eesomething-bounded.
;; Starting points:
;;   (find-efunctiondescr 'eeflash-new)
;;   (find-efunctiondescr 'ee-edelim-adjust)
;;   (find-efunctiondescr 'eeb-default-new)
;;   (find-efunctiondescr 'eeb-default)

;; Big letters courtesy of Figlet.



;;;
;;; __   ____ _ _ __ ___
;;; \ \ / / _` | '__/ __|
;;;  \ V / (_| | |  \__ \
;;;   \_/ \__,_|_|  |___/
;;;                      


(defvar ee-delimiter-hash      "\n#\n"  "See `eev-bounded'.")
(defvar ee-delimiter-percent   "\n%\n"  "See `eelatex-bounded'.")
(defvar ee-delimiter-semicolon "\n;;\n" "See `eeeval-boudned'.")

(put 'ee-delimiter-hash      'safe-local-variable 'stringp)
(put 'ee-delimiter-percent   'safe-local-variable 'stringp)
(put 'ee-delimiter-semicolon 'safe-local-variable 'stringp)

;; (setq eeb-highlight-spec '(highlight 0.2))
(defvar ee-highlight-spec  '(highlight 0.75)) ; to do: rename highlight->flash
(defvar eeb-highlight-spec '(highlight 0.5))
(defvar eek-highlight-spec '(region 0.75))
(defvar eeflash-default    '(highlight 0.5))

(defvar eeb-defaults '(eev ee-delimiter-hash nil t t)
  "A structure of the form (fun sdelim edelim flash-spec adjust).
See `eeb-default' and `eeb-define-try'.")

(defvar ee-flash-spec '(eev-glyph-face-yellow-on-red 1))



;;;             __ _           _     
;;;   ___  ___ / _| | __ _ ___| |__  
;;;  / _ \/ _ \ |_| |/ _` / __| '_ \ 
;;; |  __/  __/  _| | (_| \__ \ | | |
;;;  \___|\___|_| |_|\__,_|___/_| |_|
;;;                                  

(defun eeflash-new (s e)
  "Highlight temporarily the region between S and E.
The face and the duration are taken from `ee-flash-spec'.
This function creates an overlay with face FACE over the region
between S and E and prepares a timer that destroys that overlay
after its duration has expired.\n
This function returns the result of (ee-se-to-string S E).
As in `ee-se-to-string', S and E are usually numbers, but S can
also be a string; in that case E is ignored, and no overlay is
created.
Demos:
  (eeflash-new 2 5)
  (let ((ee-flash-spec eek-highlight-spec)) (eeflash-new 3 6))
  (let ((ee-flash-spec '(highlight 0.5)))   (eeflash-new 4 7))
  (let ((ee-flash-spec '((:background \"green\") 1.5)))
    (eeflash-new 4 7))"
  (interactive "r")
  (if (numberp s)
      (let ((ovl (make-overlay s e))
	    (face     (car  ee-flash-spec))
	    (duration (cadr ee-flash-spec)))
	(overlay-put ovl 'face face)
    (run-at-time duration nil 'delete-overlay ovl)))
  (ee-se-to-string s e))




;;;            _               _       __             _ _   
;;;   ___  ___| |__         __| | ___ / _| __ _ _   _| | |_ 
;;;  / _ \/ _ \ '_ \ _____ / _` |/ _ \ |_ / _` | | | | | __|
;;; |  __/  __/ |_) |_____| (_| |  __/  _| (_| | |_| | | |_ 
;;;  \___|\___|_.__/       \__,_|\___|_|  \__,_|\__,_|_|\__|
;;;                                                         

(defun ee-edelim-adjust (edelim adjust)
  "Return 1 if the first newline of EDELIM is to be treated specially.
This function is called by `ee-edelim-to-e' and `eeb-default-new'.
When the ending delimiter for some bounded function starts with a
newline the highlighting looks much better if we include that
newline in the highlighed region. Try:\n
#.
# (eeflash-new (ee-sdelim-to-s \"\\n#.\\n\") (ee-edelim-to-e \"\\n#,\\n\"))
# (eeflash-new (ee-sdelim-to-s \"\\n#.\\n\") (ee-edelim-to-e \"\\n#,\\n\" t))
foo
#,\n
The rule is: when ADJUST is t and EDELIM starts with \"\\n\" then
adjust the end of the delimited region one character forward
before doing the higlighting. Note that that last entry in the
`eeb-defaults' structure - and the last parameter for
`eeb-define' - are both \"ADJUST\"s..."
  (if (eq adjust t)			; special case:
      (if (ee-prefixp "\n" edelim)	; when edelim starts with a "\n"
	  1				; +1 -> include the "\n" in the region
	0)				; else 0
    (or adjust 0)))

(defun ee-sdelim-to-s (sdelim)
  "Search backwards for STR and return the position after STR.
This function does not move point. See `ee-edelim-adjust'."
  (+ (save-excursion (search-backward sdelim))
     (length sdelim)))

(defun ee-edelim-to-e (edelim &optional adjust)
  "Search forward for STR and return the position before STR.
This function does not move point. See `ee-edelim-adjust'."
  (+ (save-excursion (search-forward edelim))
     (- (length edelim))
     (ee-edelim-adjust edelim adjust)))

(defun ee-symbol-value (v &optional t-value)
  "If V is a symbol return (symbol-value V); else return V.
Note that the symbol-value of nil is nil - and the same for t.
A hack: V is t and T-VALUE is not nil then return (ee-symbol-value T-VALUE).
This function is used by `eeb-default'."
  (cond ((and (eq t v) t-value) (ee-symbol-value t-value))
	((symbolp v)            (symbol-value v))
	(t                      v)))

(defun eeb-default ()
  "Run the default action on a delimited region around point.
The default action is determined by the five entries in the list
stored in the variable `eeb-defaults'; see `eeb-define-try'."
  (interactive)
  (let* ((fun                            (nth 0 eeb-defaults))
	 (sdelim    (ee-symbol-value     (nth 1 eeb-defaults)))
	 (edelim    (ee-symbol-value (or (nth 2 eeb-defaults) sdelim)))
	 (flash-spec (ee-symbol-value    (nth 3 eeb-defaults) eeflash-default))
	 (adjust                         (nth 4 eeb-defaults))
	 (s         (ee-sdelim-to-s sdelim))
	 (e         (ee-edelim-to-e edelim))
	 (e+        (ee-edelim-to-e edelim adjust)))
    (let ((ee-flash-spec flash-spec))
      (eeflash-new s e+))
    (funcall fun s e)))





;; For compatibility
(defalias 'eeb-default-new    'eeb-default)
(defalias 'ee-search-forward  'ee-edelim-to-e)
(defalias 'ee-search-backward 'ee-sdelim-to-s)

;; (defun ee-search-backward (str)
;;   "Search backwards for STR and return the position after STR.
;; This function does not move point."
;;   (+ (save-excursion (search-backward str))
;;      (length str)))
;; 
;; (defun ee-search-forward (str &optional adjust)
;;   "Search forward for STR and return the position before STR, plus ADJUST.
;; The default value for ADJUST is 0.
;; This function does not move point."
;;   (+ (save-excursion (search-forward str))
;;      (- (length str))
;;      (or adjust 0)))






;;;            _               _       __ _            
;;;   ___  ___| |__         __| | ___ / _(_)_ __   ___ 
;;;  / _ \/ _ \ '_ \ _____ / _` |/ _ \ |_| | '_ \ / _ \
;;; |  __/  __/ |_) |_____| (_| |  __/  _| | | | |  __/
;;;  \___|\___|_.__/       \__,_|\___|_| |_|_| |_|\___|
;;;                                                    

(defun ee-add-quote (obj)
  "Return OBJ is OBJ is constant; else return 'OBJ."
  (if (or (numberp obj) (stringp obj)
	  (eq obj nil) (eq obj t) (keywordp obj))
      obj
    (list 'quote obj)))

(defun ee-pp0q (obj)
  "Like (ee-pp0 OBJ), but add a \"'\" in front if needed."
  (ee-pp0 (ee-add-quote obj)))

(defun ee-eeb-define-docstring
  (eeb-fun fun sdelim edelim flash-spec adjust extra-docs)
  "Used internally by `ee-eeb-define' to generate the docstring."
  (let ((args `(,eeb-fun ,fun ,sdelim ,edelim ,flash-spec ,adjust
		,@(if extra-docs (list extra-docs)))))
    (format "Run `%S' on a delimited region around point.
This is a wrapper function created by a sexp equivalent to first
one below (see `eeb-define'). To inspect the code that it
generates run the second sexp; and for an explanation of the
parameters, and a for a way of experimenting with them, see 
`eeb-define-try'.\n
  (eeb-define      %s)
  (find-eeb-define %s)%s"
  fun
  (mapconcat 'ee-pp0q args " ")
  (mapconcat 'ee-pp0q args " ")
  (if extra-docs (concat "\n\n" extra-docs) ""))))

(defun ee-eeb-define
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
  "See `eeb-define' and `eeb-define-try'.
This function generates the code for defining EEB-FUN, as a string,
and returns it without `read'ing or `eval'ing it. An example:\n
  (find-estring (ee-eeb-define 'eev-bounded 'eev 'ee-delimiter-hash nil t t))"
  (format
   "(defun %S ()
  %S
  (interactive)
  (setq eeb-defaults '%s)
  (eeb-default-new))"
   eeb-fun 
   (ee-eeb-define-docstring
    eeb-fun fun sdelim edelim flash-spec adjust extra-docs)
   (ee-pp0 (list fun sdelim edelim flash-spec adjust))))

;; Tests:
;; (find-eeb-define 'eev-bounded 'eev "\n#\n" nil t t)
;; (find-eeb-define 'eev-bounded 'eev "\n#\n" nil t t "Example\nHere")
;; (eeb-define      'eev-bounded 'eev "\n#\n" nil t t)
;; (eeb-define      'eev-bounded 'eev "\n#\n" nil t t "Example\nHere")
;; (eeb-define      'eev-bounded 'eev 'ee-delimiter-hash nil t t "Example\nHere")
;; (find-efunctiondescr 'eev-bounded)

;; Note: the sexps in the docstring might come out wrong if they
;; contain nasty unibyte characters (this is a known possible bug).

(defun eeb-define
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
  "Define EEB-FUN as a wrapper around FUN.
Use the delimiters SDELIM and EDELIM to find the region around
point where where FUN will operate; highlight the region using
FLASH-SPEC and ADJUST. If you want to add an example or extra
explanations to the docstring of EEB-FUN use EXTRA-DOCS.

See `eeb-define-try' for a detailed explanation of the parameters
and for a way of experimenting with them; see `find-eeb-define'
for a way to inspect to wrapper code."
  (eval (read (ee-eeb-define 
	       eeb-fun
	       fun sdelim edelim
	       flash-spec adjust extra-docs))))

(defun find-eeb-define (&rest rest)
  (find-estring (apply 'ee-eeb-define rest))
  (emacs-lisp-mode))


;;;            _               _       __ _                  _              
;;;   ___  ___| |__         __| | ___ / _(_)_ __   ___      | |_ _ __ _   _ 
;;;  / _ \/ _ \ '_ \ _____ / _` |/ _ \ |_| | '_ \ / _ \_____| __| '__| | | |
;;; |  __/  __/ |_) |_____| (_| |  __/  _| | | | |  __/_____| |_| |  | |_| |
;;;  \___|\___|_.__/       \__,_|\___|_| |_|_| |_|\___|      \__|_|   \__, |
;;;                                                                   |___/ 

(defun eeb-define-try
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
"This is similar to `eeb-define', but instead of defining EEB-FUN run it now.
The \"default action over bounded regions\" is determined by the
five entries in the list stored in the variable `eeb-defaults'
\(described below). All the \"bounded functions\", like
`eev-bounded', work by setting the variable `eeb-defaults' and
then calling the function `eeb-default-new', that interprets the
entries in `eeb-defaults' in a certain way and acts accordingly.


eeb-define
==========
Bounded functions like `eev-bounded' are defined by calling the
function `eeb-define' with the name of the function to define and
the five entries for the associated value for `eeb-defaults',
like this:

  (eeb-define 'eev-bounded 'eev 'ee-delimiter-hash nil t t)

`eeb-define-try' provides a nice way to test how functions
defined by `eeb-define' would behave after they are defined.
`eeb-define-try' expects the same arguments as `eeb-define', but
it ignores the first one - EEB-FUN -, and instead of defining a
function EEB-FUN that would set `eeb-defaults' and run
`eeb-default', it sets `eeb-defaults' immediately (temporarily,
using `let') and runs `eeb-default' on that.


eeb-defaults and eeb-default
============================
The variable `eeb-defaults' always holds a list of this form:

  (FUN SDELIM EDELIM FLASH-SPEC ADJUST)

where:
  FUN        is a function taking arguments \"s\" and \"e\", like `eev',
  SDELIM     is the starting delimiter (see `ee-edelim-adjust'),
  EDELIM     is the ending delimiter (default: same as sdelim),
  FLASH-SPEC tells how to highlight the region (see `eeflash-new'),
  ADJUST     should usually be t; see `ee-edelim-adjust'.

The \"default action on a delimited region\" is always something
composed of two \"standard actions\": first, highlight the region
temporarily, as described below; second, and most important, run
\"(FUN s e)\" on the region. FLASH-SPEC and ADJUST are only used
for the highlighting part; FUN is only used for the \"run (FUN s
e)\" part.

A nil at EDELIM means to use EDELIM := SDELIM; after
replacing the possible nil at EDELIM both SDELIM and
EDELIM are \"expanded\" with `ee-symbol-value' if their
values are symbols, and the results must be strings. Those
resulting strings are used as region delimiters by
`ee-sdelim-to-s' and `ee-edelim-to-e' to produce the \"s\" and
\"e\" arguments for the \"(FUN s e)\" call; see the documentation
for `ee-edelim-adjust' for an example that also shows how
ADJUST affects the highlighting.

A t at FLASH-SPEC means to use `eeflash-default' as FLASH-SPEC;
after treating the `t' case the value of FLASH-SPEC is
\"expanded\" with `ee-symbol-value' if it's a symbol, and the
result - that should be either nil or a list of the form \"(face
duration)\" - becomes temporarily the value of `ee-flash-spec',
and we invoke `eeflash-new' to highlight the region.


Examples
========
Here are some demos:\n
#.
# (eeb-define-try nil 'list            \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'ee-se-to-string \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'eeflash-new     \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'eev             \"\\n#.\\n\" nil t t)
echo $[1+2]
#.\n"
  (let ((eeb-defaults (list fun sdelim edelim flash-spec adjust)))
    (eeb-default-new)))

;; (find-efunctiondescr 'eeb-define-try)



;;;                                  _                           _          _ 
;;;   ___  _____  ____  ____  __    | |__   ___  _   _ _ __   __| | ___  __| |
;;;  / _ \/ _ \ \/ /\ \/ /\ \/ /____| '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` |
;;; |  __/  __/>  <  >  <  >  <_____| |_) | (_) | |_| | | | | (_| |  __/ (_| |
;;;  \___|\___/_/\_\/_/\_\/_/\_\    |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_|
;;;                                                                           

;; (find-eeb-define 'eev-bounded     'eev     'ee-delimiter-hash      nil t t)
;; (find-eeb-define 'eeg-bounded     'eeg     'ee-delimiter-hash      nil t t)
;; (find-eeb-define 'eegdb-bounded   'eegdb   'ee-delimiter-hash      nil t t)
;; (find-eeb-define 'eelatex-bounded 'eelatex 'ee-delimiter-percent   nil t t)
;; (find-eeb-define 'eeeval-bounded  'eeeval  'ee-delimiter-semicolon nil t t)
;; (find-eeb-define 'eeb-eval        'eeeval  'ee-delimiter-semicolon nil t t)

(eeb-define 'eev-bounded     'eev     'ee-delimiter-hash      nil t t)
(eeb-define 'eeg-bounded     'eeg     'ee-delimiter-hash      nil t t)
(eeb-define 'eegdb-bounded   'eegdb   'ee-delimiter-hash      nil t t)
(eeb-define 'eelatex-bounded 'eelatex 'ee-delimiter-percent   nil t t)
(eeb-define 'eeeval-bounded  'eeeval  'ee-delimiter-semicolon nil t t)
(eeb-define 'eeb-eval        'eeeval  'ee-delimiter-semicolon nil t t)

(provide 'eev-bounded)



;; (load (buffer-file-name))
;; (find-efunctiondescr 'eeb-define-try)
;; (find-efunctiondescr 'ee-edelim-adjust)
;; (find-efunctiondescr 'eeflash-new)
;; (find-fline "$VWT/eev-bounded.el")



;; Local Variables:
;; coding:          raw-text-unix
;; no-byte-compile: t
;; End:
