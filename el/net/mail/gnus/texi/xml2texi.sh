#! /bin/sh
#|
exec mzscheme -mr $0 ${1+"$@"}
|#
;; Copyright (C) 2005  Free Software Foundation, Inc.
;; 
;;  Author:  Karl Pflästerer <sigurd@12move.de>
;;  Keywords: tools
;; 
;;  This file is not part of GNU Emacs.
;; 
;;  GNU Emacs is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2, or (at your option)
;;  any later version.
;; 
;;  GNU Emacs is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;; 
;;  You should have received a copy of the GNU General Public License
;;  along with GNU Emacs; see the file COPYING.  If not, write to the
;;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;  Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Requirements:
;;
;; To run this scheme program you need MzScheme and DrScheme.
;;
;; Some URLs:
;; http://www.plt-scheme.org/software/mzscheme/
;; http://www.drscheme.org/
;; http://www.plt-scheme.org/software/
;; 
;; ,----[ http://www.plt-scheme.org/license/ ]
;; | PLT software is distributed under the
;; |  GNU Lesser General Public License (LGPL).
;; `----

;;; Code:

(require (lib "cmdline.ss"))
(define +scmfile+ "xml2texi.scm")
(define +in+ "gnus-faq.xml")
(define +out+ "gnus-faq.texi")

(command-line
 "xml2texi"
 (current-command-line-arguments)
 (once-each
  (("-i" "--input") in "Name of XML data file (default gnus-faq.xml)"
   (set! +in+ in))
  (("-o" "--output") out "Name of output file (default gnus-faq.texi)"
   (set! +out+ out))
  (("-l" "--library") lib "Name of Scheme library to load (default xml2texi.scm)"
   (set! +scmfile+ lib)))
 (help-labels "The first (or only) remaining argument is used as the name of the Input file"
              "The second argument is used as the name of the Output file")
 (args infile+outfile
       (cond ((= (length infile+outfile) 2)
               (set! +in+ (car infile+outfile))
               (set! +out+ (cadr infile+outfile)))
             ((= (length infile+outfile) 1)
               (set! +in+ (car infile+outfile)))
             (else #f))))

(load +scmfile+)
(main +in+ +out+)

;; Local Variables:
;; mode: scheme
;; coding: iso-8859-1
;; End:

;; arch-tag: 582279c5-b7bf-44b0-ba09-d243a7d7f6e0
