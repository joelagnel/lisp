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
;; Version:    2006sep19
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-bounded-old.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-bounded-old.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>
;;
;; History: this used to be part of eev.el... In sep/2006 I decided to
;; rewrite it; the new version is at eev-bounded.el, and it is not yet
;; loaded by default. I moved to old code corresponding to it to this
;; file, and I hope that in some days I will be able to keep this as
;; part of eev-0.95.1 but declare it obsolete.

;; Big letters courtesy of Figlet.

;; Sections:
;; variables
;; auxiliary functions for saving delimited ("bounded") regions
;; eeb-default: the default action on bounded regions
;; eeb-define: creating bounded versions of eev-like functions



;;;                  _       _     _           
;;; __   ____ _ _ __(_) __ _| |__ | | ___  ___ 
;;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;;                                            

(defvar ee-delimiter-hash      "\n#\n"  "See `eev-bounded'.")
(defvar ee-delimiter-percent   "\n%\n"  "See `eelatex-bounded'.")
(defvar ee-delimiter-semicolon "\n;;\n" "See `eeeval-boudned'.")

(put 'ee-delimiter-hash    'safe-local-variable 'stringp)
(put 'ee-delimiter-percent 'safe-local-variable 'stringp)

(defvar ee-once nil)			; overridden by `let's

(defvar eeb-defaults '(eev ee-delimiter-hash nil t t)
  "A structure that controls what `eeb-default' will do when invoked.
If its value is `(eev ee-delimiter-hash nil t t)', for example,
then `eeb-default' will use the region around point up to the
first occurrence of the string `ee-delimiter-hash' before point,
and up to the first occurrence of `ee-delimiter-hash' after
point (nil at the third position means to use the same ending
delimiter as the starting delimiter); t in the fourth position
means to use the flash-spec in `eeflash-default' to highlight the
region temporarily; and t in the fifth position means to include
the first char of the ending delimiter (a newline) in the
highliter region.

For the exact meaning of each field see the docs for the function
`ee-sedelims++-to-sedelims+'.")



;;;  _                           _          _    __                      
;;; | |__   ___  _   _ _ __   __| | ___  __| |  / _|_   _ _ __   ___ ___ 
;;; | '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` | | |_| | | | '_ \ / __/ __|
;;; | |_) | (_) | |_| | | | | (_| |  __/ (_| | |  _| |_| | | | | (__\__ \
;;; |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_| |_|  \__,_|_| |_|\___|___/
;;;                                                                      
;;; auxiliary functions for saving delimited ("bounded") regions
;;;

(defun ee-search-backward (str)
  "Search backwards for STR and return the position after STR.
This function does not move point."
  (+ (save-excursion (search-backward str))
     (length str)))

(defun ee-search-forward (str &optional adjust)
  "Search forward for STR and return the position before STR, plus ADJUST.
The default value for ADJUST is 0.
This function does not move point."
  (+ (save-excursion (search-forward str))
     (- (length str))
     (or adjust 0)))

(defun ee-prefixp (prefix str)
  "Return t if STR begins with PREFIX."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))))

;; (Anchor:) <<data_structures>>

;; Some notes about the data structures...
;;    flash-spec ->  (&optional face duration)
;;    se         ->  (s &optional e) 
;;    se+        ->  (s &optional e flash-spec add-to-e)
;;    sedelims+  ->  (sdelim &optional edelim flash-spec add-to-e)
;;    sedelims++ ->  (sdelim &optional edelim flash-spec add-to-e)
;;    eeb-spec   ->  (eexxx sdelim edelim flash-spec add-to-e)
;;
;; * In a `se' usually s and e are the start and the end of a region
;;   of text, but it is also possible to use a string for s. A `se's
;;   is converted to a string by `ee-se-to-string'.
;; * A `se+' is like a `se', but we can use `eeflash+' to flash the
;;   region when s is not a string. `add-to-e' is added to e for
;;   flashing.
;; * A `sedelims+' is converted to a `se+' by applying
;;   `ee-search-backward' to sdelim and `ee-search-forward' to edelim.
;; * A `sedelims++' is a `sedelims+' allowing many convenient defaults:
;;   . edelim may be nil; then it becomes a copy of sdelim.
;;   . sdelim and edelim may be symbols; then they're substituted by
;;     their values as variables.
;;   . if flash-spec is nil it becomes eeflash-default.
;;   . if flash-spec is a symbol it is substituted by its value as a
;;     variable.
;;   . if add-to-e is t then it becomes 0 or 1 according to the first
;;     character of edelim: if edelim starts with "\n" then flashing
;;     looks nicer if we include the "\n" after the region between s
;;     and e (the "\n" is technically the first character of the
;;     ending delimiter) in the flashed region; so, if edelim starts
;;     with "\n" then add-to-e becomes 1, otherwise it becomes 0.
;; * An `eeb-spec' is a `sedelims++' preceded by an `eexxx'; an eexxx
;;   is a symbol like `eev', `eelatex', etc - i.e., the name of a
;;   function that runs with arguments s and e. These `eeb-spec's are
;;   used to run eexxx functions on delimited regions; the variable
;;   `eeb-defaults' holds an eeb-spec.

(defun ee-sedelims+-to-se+ (sdelim &optional edelim flash-spec add-to-e)
  "Convert an \"sedelims+\" structure to an \"se+\" structure.
The conversion is done by searching backwards from point for the
first occurrence of SDELIM (a string), and replacing the first
field of the structure by the position after that string, and,
similarly, by replacing the second field by the position before
the first occurrence of EDELIM forwards from point.

In both cases the point is returned to its original position
after the search."
  (list (ee-search-backward sdelim) (ee-search-forward edelim)
	flash-spec add-to-e))

(defun ee-sedelims++-to-sedelims+ (sdelim &optional edelim flash-spec add-to-e)
  "Convert an \"sedelims++\" structure into a \"sedelims+\" structure.
The conversion is done by interpreting default values.
If EDELIM is nil, do (setq EDELIM SDELIM).
If SDELIM is a symbol, take its value; same for EDELIM.
If FLASH-SPEC is t, do (setq FLASH-SPEC 'eeflash-default).
If FLASH-SPEC is a symbol, take its value.
If ADD-TO-E is t, then make ADD-TO-E 1 if the first char of
EDELIM is a newline.

The variable `eeb-defaults' contains a \"sedelims++\".
See also `ee-sedelims+-to-se+' and `eeb-default'."
  (if (not edelim) (setq edelim sdelim))
  (if (symbolp sdelim) (setq sdelim (symbol-value sdelim)))
  (if (symbolp edelim) (setq edelim (symbol-value edelim)))
  (if (eq flash-spec t) (setq flash-spec 'eeflash-default))
  (if (symbolp flash-spec) (setq flash-spec (symbol-value flash-spec)))
  (if (eq add-to-e t) (setq add-to-e (if (ee-prefixp "\n" edelim) 1 0)))
  (list sdelim edelim flash-spec add-to-e))



;;;            _               _       __ _            
;;;   ___  ___| |__         __| | ___ / _(_)_ __   ___ 
;;;  / _ \/ _ \ '_ \ _____ / _` |/ _ \ |_| | '_ \ / _ \
;;; |  __/  __/ |_) |_____| (_| |  __/  _| | | | |  __/
;;;  \___|\___|_.__/       \__,_|\___|_| |_|_| |_|\___|
;;;                                                    
;;; eeb-default: the default action on bounded regions
;;; eeb-define: creating bounded versions of eev-like functions
;;;

(defun eeb-default ()
  "Run the default action on a bounded region around point."
  (interactive)
  (let* ((fun (car eeb-defaults))
	 (sedelims++ (cdr eeb-defaults))
	 (sedelims+ (apply 'ee-sedelims++-to-sedelims+ sedelims++))
	 (se+ (apply 'ee-sedelims+-to-se+ sedelims+)))
    (apply 'eeflash+ se+)
    (funcall fun (car se+) (cadr se+))))

;;;
;;; saving delimited regions
;;;

(defun eeb-define (eexxx-bounded
		   eexxx sdelim &optional   edelim flash-spec add-to-e)
  "Define EEXXX-BOUNDED as a \"bounded version\" of the function EEXXX.
EEXXX-BOUNDED will use SDELIM as starting delimiter and
EDELIM (or SDELIM again, when EDELIM is nil) as endining
delimiters for the bounded region; it will use
  <<<unfinished!!!>>>"
  (let ((eexxx-sedelims+ (list eexxx sdelim edelim flash-spec add-to-e)))
    (set eexxx-bounded eexxx-sedelims+))
  (eval `(defun ,eexxx-bounded ()
	   (interactive)
	   (setq eeb-defaults ,eexxx-bounded)
	   (eeb-default))))

(defmacro eeb-once (&rest body) `(let (eeb-defaults) . ,body))
(defalias 'ee-once 'eeb-once)

(eeb-define 'eev-bounded     'eev     'ee-delimiter-hash      nil t t)
(eeb-define 'eeg-bounded     'eeg     'ee-delimiter-hash      nil t t)
(eeb-define 'eegdb-bounded   'eegdb   'ee-delimiter-hash      nil t t)
(eeb-define 'eelatex-bounded 'eelatex 'ee-delimiter-percent   nil t t)
(eeb-define 'eeeval-bounded  'eeeval  'ee-delimiter-semicolon nil t t)
(eeb-define 'eeb-eval        'eeeval  'ee-delimiter-semicolon nil t t)


(provide 'eev-bounded-old)








;; Local Variables:
;; coding:          raw-text-unix
;; no-byte-compile: t
;; End:
