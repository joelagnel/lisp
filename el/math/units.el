;;; Debugging info for self: Saved through ges-version 1.5dev
;;; From: Linh Dang <linh@linhdang.home>
;;; Subject: units.el (units conversion using units.dat)
;;; Newsgroups: gnu.emacs.sources
;;; Date: Tue, 17 Sep 2002 07:40:56 -0400
;;; Organization: Null


;;; UNITS.EL --- units conversion

;; Copyright (C) 2002 Linh Dang

;; Author: Linh Dang <linhd@>
;; Maintainer: Linh Dang <linhd@>
;; Created: 16 Sep 2002
;; Version: 1.0
;; Keywords: conversion

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <linhd@>) or from the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;; LCD Archive Entry:
;; units|Linh Dang|<linhd@>
;; |units conversion
;; |$Date: 2002/09/17 11:34:13 $|$Revision: 1.8 $|~/packages/units.el

;;; Commentary:
;;
;; Dirty hack to do units conversion using units.dat from units package.
;; likely buggy. Fixes/patches/flames/comments are welcome.
;;
;; only tested on ntemacs 21.2

;;; Change log:
;; $Log: units.el,v $
;; Revision 1.8  2002/09/17 11:34:13  linhd
;; huh
;;
;; Revision 1.7  2002/09/17 11:27:57  linhd
;; clean
;;
;; Revision 1.6  2002/09/16 18:48:31  linhd
;; ok
;;
;; Revision 1.5  2002/09/16 16:27:33  linhd
;; works
;;
;; Revision 1.4  2002/09/16 16:21:31  linhd
;; seems to work
;;
;; Revision 1.3  2002/09/16 14:41:38  linhd
;; good
;;
;; Revision 1.2  2002/09/16 14:21:22  linhd
;; huh
;;
;; Revision 1.1  2002/09/16 14:07:59  linhd
;; Initial revision
;;

;;; Code:

(defconst units-version (substring "$Revision: 1.8 $" 11 -2)
  "$Id: units.el,v 1.8 2002/09/17 11:34:13 linhd Exp $

Report bugs to: Linh Dang <linhd@>")
(defvar units-load-hook nil
  "*Hooks run after loading units.")

(defcustom units-dat-file "/usr/share/units/units.dat"
  "Dat file for UNITS."
  :group 'emacs
  :type '(file :must-match t))

(defun units-buffer () (find-file-noselect units-dat-file))

(defun units-s-to-n (s)
  "convert a quantity string in units.dat to a number."
  (if (memq ?| (mapcar 'identity s))
      (apply '/ (mapcar 'string-to-number (split-string s "|")))
    (string-to-number s)))

(defun units-prefix-convert (prefix)
  "convert PREFIX such as centi or mega to a number."
  (goto-char (point-min))
  (if (re-search-forward (concat "^" prefix "\\s-+\\(\\S-+\\)\\(\\s-+#?\\)?") nil t)
      (if (= (units-s-to-n (match-string-no-properties 1)) 0)
          (units-prefix-convert (concat (match-string-no-properties 1) "-"))
        (units-s-to-n (match-string-no-properties 1)))
      0))

(defvar units-si-prefix-list 
  '("yotta" "zetta" "exa" "peta" "tera" "giga" "mega" "myria" "kilo"
    "hecto" "deca" "deka" "deci" "centi" "milli" "micro" "nano" "pico"
    "femto" "atto" "zepto" "yocto" "quarter" "semi" "demi" "hemi"
    "half" "double" "triple" "treble" )
  "multi-char prefixes used in SI.")

(defvar units-si-short-prefix-list
  '(?Y ?Z ?E ?P ?T ?G ?M ?k ?h ?d ?c ?m ?n ?p ?f ?a ?z ?y)
  "single car prefixes used in SI (not including da)")

(defun units-convert-1 (in quantity out)
  "convert QUANTITY in IN units to OUT units.
return the amount in OUT units. This function assumed that
the current buffer contains units.dat."
  (if (or (= quantity 0) (string-equal in out))
      quantity
    (let (n next prefix)
      (goto-char (point-min))
      (if (re-search-forward 
           (concat "^" in 
                   "\\> +\\(\\([a-zA-Z]\\S-*\\)\\|!\\|\\([0-9]\\S-*\\) +\\([a-zA-Z]\\S-*\\)\\)")
           nil t)
          (cond ((match-beginning 4)
                 (setq next (match-string-no-properties 4))
                 (setq n (units-s-to-n (match-string-no-properties 3)))
                 (if (string-equal next out)
                     (* n quantity) 
                   (units-convert-1 next (* n quantity) out)))

                ((match-beginning 2)
                 (setq next (match-string-no-properties 2))
                 (if (string-equal next out)
                     quantity
                   (units-convert-1 next quantity out)) )

                ((string-equal (match-string-no-properties 1) "!")
                 (/ quantity (units-convert-1 out 1 in)))
                (t
                 (error "internal error 1") ))
        (unless (or (and (= (length in) 2)
                         (memq (aref in 0) units-si-short-prefix-list)
                         (setq prefix (concat (list (aref in 0) ?-))
                               in (substring in 1)))
                    (and (= (length in) 3)
                         (= (aref in 0) ?d)
                         (= (aref in 1) ?a)
                         (setq prefix "da-"
                               in (substring in 2)))
                    (and (progn
                           (mapcar (lambda (pre)
                                     (if (string-match (concat "\\`" pre) in)
                                         (setq prefix (concat (match-string 0 in) "-")
                                               in (substring in (match-end 0)))))
                                   units-si-prefix-list)
                           prefix)))
          (error "don't know how to convert %g %s to %s" quantity in out))
        (setq quantity (* (units-prefix-convert prefix) quantity))
        (if (= quantity 0)
            (error "don't know how to handle %s" prefix)
          (units-convert-1 in quantity out))))))

(defun units-convert (in quantity out)
  "command to convert QUANTITY in IN units to OUT units."
  (interactive "sinput unit: \nnquantity: \nsoutput unit: ")
  (let ((buffer (units-buffer)))
    (save-excursion
      (set-buffer buffer)
      (toggle-read-only 1)
      (message "%g %s = %g %s" quantity in
               (units-convert-1 in quantity out) out))))

(provide 'units)
(run-hooks 'units-load-hook)
;;; UNITS.EL ends here


;;; -- 
;;; Linh Dang

