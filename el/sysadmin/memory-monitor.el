;;;; Time-stamp: <2004-12-04 13:40:27 jcgs>


;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'memory-monitor)

(defmacro with-memory-monitor (label &rest forms)
  "With LABEL on the output, evaluate FORMS
reporting changes in memory use during their evaluation."
  `(let ((gc-before (garbage-collect)))
     (prog1
	 (progn
	   ,@forms)
       (let ((gc-after (garbage-collect)))
	 (message "%s %d new conses, %d new symbols, %d more string chars"
		  ,label
		  (- (car (first gc-after))
		     (car (first gc-before)))
		  (- (car (second gc-after))
		     (car (second gc-before)))
		  (- (fourth gc-after) (fourth gc-before)))))))

	 
