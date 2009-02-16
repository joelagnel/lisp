;;; uniq.el --- Remove duplicate lines

;; Copyright (C) 2003 Art Taylor

;; Filename: uniq.el
;; Author: Art Taylor <reeses@astrogoth.com>
;; Version: 1.0
;; Keywords: uniq, duplicates

;; [Commentary]
;;
;; Remove duplicate lines from a region.  uniq-region behaves in a
;; fashion similar to the Unix utility 'uniq', only removing a line
;; duplicating the immediate antecedent.  uniq-remove-dup-lines will
;; remove all duplicate lines from the region, leaving only the first
;; instance. 

;; [License]
;;
;; This software is provided 'as-is', without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;;
;; Note that this license is borrowed from zlib via nullsoft.
;;
;; Written 12-Feb-2003, Washington, DC
;;


(defun uniq-region (beg end)
  "Remove duplicate lines, a` la Unix uniq.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "r")
  (let ((ref-line nil))
      (uniq beg end 
	       (lambda (line) (string= line ref-line)) 
	       (lambda (line) (setq ref-line line)))))

(defun uniq-remove-dup-lines (beg end)
  "Remove all duplicate lines wherever found in a file, rather than
   just contiguous lines."
  (interactive "r")
  (let ((lines '()))
    (uniq beg end 
	     (lambda (line) (assoc line lines)) 
	     (lambda (line) (add-to-list 'lines (cons line t))))))

(defun uniq (beg end test-line add-line)
  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (if (funcall test-line (thing-at-point 'line))
	  (kill-line 1)
	(progn
	  (funcall add-line (thing-at-point 'line))
	  (forward-line))))
    (widen)))



	
