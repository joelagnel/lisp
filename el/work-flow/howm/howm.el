;;; howm.el --- Wiki-like note-taking tool
;;; Copyright (c) 2002, 2003, 2004, 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm.el,v 1.12.2.1 2006/12/16 11:05:44 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; DESCRIPTION:
;;; 
;;; See README.
;;; HOWM is acronym of "Hitori Otegaru Wiki Modoki".

;;; official

(require 'easy-mmode)
(require 'font-lock)

;;; cl

(require 'howm-cl)

;;; my tools

;; This doesn't work in byte-compilation. I don't understand it. Sigh...
;; (mapcar #'require howm-required-features)

(require 'cheat-font-lock)
(require 'illusion)
(require 'gfunc)
(require 'riffle)
(require 'action-lock)
(require 'honest-report)

;;; howm

(require 'howm-version)
(require 'howm-vars)
(require 'howm-common)
(require 'howm-backend)
(require 'howm-view)
(require 'howm-mode)
(require 'howm-misc)
(require 'howm-date)
(require 'howm-reminder)
(require 'howm-menu)

;;; for howmz.el [2006-02-02]
;;; http://noir.s7.xrea.com/archives/000136.html
;;; http://noir.s7.xrea.com/pub/zaurus/howmz.el

(howm-require-lang 'en)
(howm-require-lang 'ja)

;;; security fix [2006-12-16]
(mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
        (howm-symbols))

;;; provide

(provide 'howm)

;;; howm.el ends here
