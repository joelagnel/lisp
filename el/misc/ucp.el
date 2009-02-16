;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun unencodable-char-position (start end coding-system)
  "Return position of first un-encodable character in a region.
START and END specfiy the region and CODING-SYSTEM specifies the
encoding to check.  Return nil if CODING-SYSTEM does encode the region.

CODING-SYSTEM may also be a list of coding systems, in which case return
the first position not encodable by any of them.

This function is fairly slow."
  ;; Use recursive calls in the binary chop below, since we're
  ;; O(logN), and the call overhead shouldn't be a bottleneck.
  (unless enable-multibyte-characters
    (error "unibyte buffer"))
  ;; Recurse if list of coding systems.
  (if (consp coding-system)
      (let ((end end) res)
	(dolist (elt coding-system (and res (>= res 0) res))
	  (let ((pos (unencodable-char-position start end elt)))
	    (if pos
		(setq end pos
		      res pos)))))
    ;; Skip ASCII initially.
    (save-excursion
      (goto-char start)
      (skip-chars-forward "\000-\177" end)
      (setq start (point))
      (unless (= start end)
	(setq coding-system (coding-system-base coding-system))	; canonicalize
	(let ((codings (find-coding-systems-region start end)))
	  (unless (or (equal codings '(undecided))
		      (memq coding-system
			    (find-coding-systems-region start end)))
	    ;; Binary chop.
	    (if (= start (1- end))
		start
	      (or (unencodable-char-position start (/ (+ start end) 2)
					     coding-system)
		  (unencodable-char-position (/ (+ start end) 2) end
					     coding-system)))))))))
