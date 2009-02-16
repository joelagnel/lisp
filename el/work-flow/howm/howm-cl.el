;;; howm-cl.el --- Wiki-like note-taking tool
;;; Copyright (c) 2002, 2003, 2004, 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-cl.el,v 1.4 2006/01/16 14:59:51 hira Exp $
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
;;;--------------------------------------------------------------------

;; I know requiring cl is evil.
;; Each defalias should be replaced to defun without cl.

;; (personal note [2005-06-09])
;; $ grep howm-cl *.el | grep -v '^howm' | grep -v '^bcomp'
;; action-lock.el:(require 'howm-cl)
;; action-lock.el:  (setq action-lock-rules (howm-cl-remove-duplicates rules))
;; riffle.el:(require 'howm-cl)
;; riffle.el:         (stops (howm-cl-remove-duplicates
;; riffle.el:         (pos (howm-cl-position c stops))

(require 'cl)

(defalias 'howm-cl-assoc-if          'assoc-if)
(defalias 'howm-cl-find-if           'find-if)
(defalias 'howm-cl-find-if-not       'find-if-not)
(defalias 'howm-cl-mapcan            'mapcan)
(defalias 'howm-cl-member*           'member*)
(defalias 'howm-cl-member-if         'member-if)
(defalias 'howm-cl-position          'position)
(defalias 'howm-cl-position-if       'position-if)
(defalias 'howm-cl-remove-duplicates 'remove-duplicates)
(defalias 'howm-cl-remove-if         'remove-if)
(defalias 'howm-cl-remove-if-not     'remove-if-not)
(defalias 'howm-cl-subseq            'subseq)

(provide 'howm-cl)

;;; howm-cl.el ends here
