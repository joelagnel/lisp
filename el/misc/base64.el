;;; base64.el,v --- Base64 encoding functions
;; Author: Kyle E. Jones
;; Created: 1999/03/25 05:30:03
;; Version: 1.3
;; Keywords: extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1997 Kyle E. Jones
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For non-MULE
(if (not (fboundp 'char-int))
    (fset 'char-int 'identity))

(defvar base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defvar base64-decoder-program nil
  "*Non-nil value should be a string that names a MIME base64 decoder.
The program should expect to read base64 data on its standard
input and write the converted data to its standard output.")

(defvar base64-decoder-switches nil
  "*List of command line flags passed to the command named by
base64-decoder-program.")

(defvar base64-encoder-program nil
  "*Non-nil value should be a string that names a MIME base64 encoder.
The program should expect arbitrary data on its standard
input and write base64 data to its standard output.")

(defvar base64-encoder-switches nil
  "*List of command line flags passed to the command named by
base64-encoder-program.")

(defconst base64-alphabet-decoding-alist
  '(
    ( ?A . 00) ( ?B . 01) ( ?C . 02) ( ?D . 03) ( ?E . 04) ( ?F . 05)
    ( ?G . 06) ( ?H . 07) ( ?I . 08) ( ?J . 09) ( ?K . 10) ( ?L . 11)
    ( ?M . 12) ( ?N . 13) ( ?O . 14) ( ?P . 15) ( ?Q . 16) ( ?R . 17)
    ( ?S . 18) ( ?T . 19) ( ?U . 20) ( ?V . 21) ( ?W . 22) ( ?X . 23)
    ( ?Y . 24) ( ?Z . 25) ( ?a . 26) ( ?b . 27) ( ?c . 28) ( ?d . 29)
    ( ?e . 30) ( ?f . 31) ( ?g . 32) ( ?h . 33) ( ?i . 34) ( ?j . 35)
    ( ?k . 36) ( ?l . 37) ( ?m . 38) ( ?n . 39) ( ?o . 40) ( ?p . 41)
    ( ?q . 42) ( ?r . 43) ( ?s . 44) ( ?t . 45) ( ?u . 46) ( ?v . 47)
    ( ?w . 48) ( ?x . 49) ( ?y . 50) ( ?z . 51) ( ?0 . 52) ( ?1 . 53)
    ( ?2 . 54) ( ?3 . 55) ( ?4 . 56) ( ?5 . 57) ( ?6 . 58) ( ?7 . 59)
    ( ?8 . 60) ( ?9 . 61) ( ?+ . 62) ( ?/ . 63)
   ))

(defvar base64-alphabet-decoding-vector
  (let ((v (make-vector 123 nil))
	(p base64-alphabet-decoding-alist))
    (while p
      (aset v (car (car p)) (cdr (car p)))
      (setq p (cdr p)))
    v))

(defun base64-run-command-on-region (start end output-buffer command
					   &rest arg-list)
  (let ((tempfile nil) status errstring)
    (unwind-protect
	(progn
	  (setq tempfile (make-temp-name "base64"))
	  (setq status
		(apply 'call-process-region
		       start end command nil
		       (list output-buffer tempfile)
		       nil arg-list))
	  (cond ((equal status 0) t)
		((zerop (save-excursion
			  (set-buffer (find-file-noselect tempfile))
			  (buffer-size)))
		 t)
		(t (save-excursion
		     (set-buffer (find-file-noselect tempfile))
		     (setq errstring (buffer-string))
		     (kill-buffer nil)
		     (cons status errstring)))))
      (condition-case ()
	  (delete-file tempfile)
	(error nil)))))

(defun base64-insert-char (char &optional count ignored buffer)
  (condition-case nil
      (progn
	(insert-char char count ignored buffer)
	(fset 'base64-insert-char 'insert-char))
    (wrong-number-of-arguments
     (fset 'base64-insert-char 'base64-xemacs-insert-char)
     (base64-insert-char char count ignored buffer))))

(defun base64-xemacs-insert-char (char &optional count ignored buffer)
  (if (and buffer (eq buffer (current-buffer)))
      (insert-char char count)
    (save-excursion
      (set-buffer buffer)
      (insert-char char count))))

(defun base64-decode-region (start end)
  (interactive "r")
  (message "Decoding base64...")
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(bits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" base64-alphabet)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base64-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base64-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     (status (apply 'base64-run-command-on-region
				   start end work-buffer
				   base64-decoder-program
				   base64-decoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (goto-char start)
	    (skip-chars-forward non-data-chars end)
	    (while (not done)
	      (setq inputpos (point))
	      (cond
	       ((> (skip-chars-forward base64-alphabet end) 0)
		(setq lim (point))
		(while (< inputpos lim)
		  (setq bits (+ bits 
				(aref base64-alphabet-decoding-vector
				      (char-int (char-after inputpos)))))
		  (setq counter (1+ counter)
			inputpos (1+ inputpos))
		  (cond ((= counter 4)
			 (base64-insert-char (lsh bits -16) 1 nil work-buffer)
			 (base64-insert-char (logand (lsh bits -8) 255) 1 nil
					 work-buffer)
			 (base64-insert-char (logand bits 255) 1 nil
					     work-buffer)
			 (setq bits 0 counter 0))
			(t (setq bits (lsh bits 6)))))))
	      (cond
	       ((= (point) end)
		(if (not (zerop counter))
		    (error "at least %d bits missing at end of base64 encoding"
			   (* (- 4 counter) 6)))
		(setq done t))
	       ((= (char-after (point)) ?=)
		(setq done t)
		(cond ((= counter 1)
		       (error "at least 2 bits missing at end of base64 encoding"))
		      ((= counter 2)
		       (base64-insert-char (lsh bits -10) 1 nil work-buffer))
		      ((= counter 3)
		       (base64-insert-char (lsh bits -16) 1 nil work-buffer)
		       (base64-insert-char (logand (lsh bits -8) 255)
					   1 nil work-buffer))
		      ((= counter 0) t)))
	       (t (skip-chars-forward non-data-chars end)))))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Decoding base64... done"))

(defun base64-encode-region (start end)
  (interactive "r")
  (message "Encoding base64...")
  (let ((work-buffer nil)
	(counter 0)
	(cols 0)
	(bits 0)
	(alphabet base64-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base64-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base64-encoder-program
	      (let ((status (apply 'base64-run-command-on-region
				   start end work-buffer
				   base64-encoder-program
				   base64-encoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq bits (+ bits (char-int (char-after inputpos))))
	      (setq counter (1+ counter))
	      (cond ((= counter 3)
		     (base64-insert-char (aref alphabet (lsh bits -18)) 1 nil
					 work-buffer)
		     (base64-insert-char
		      (aref alphabet (logand (lsh bits -12) 63))
		      1 nil work-buffer)
		     (base64-insert-char
		      (aref alphabet (logand (lsh bits -6) 63))
		      1 nil work-buffer)
		     (base64-insert-char
		      (aref alphabet (logand bits 63))
		      1 nil work-buffer)
		     (setq cols (+ cols 4))
		     (cond ((= cols 72)
			    (base64-insert-char ?\n 1 nil work-buffer)
			    (setq cols 0)))
		     (setq bits 0 counter 0))
		    (t (setq bits (lsh bits 8))))
	      (setq inputpos (1+ inputpos)))
	    ;; write out any remaining bits with appropriate padding
	    (if (= counter 0)
		nil
	      (setq bits (lsh bits (- 16 (* 8 counter))))
	      (base64-insert-char (aref alphabet (lsh bits -18)) 1 nil
				  work-buffer)
	      (base64-insert-char (aref alphabet (logand (lsh bits -12) 63))
				  1 nil work-buffer)
	      (if (= counter 1)
		  (base64-insert-char ?= 2 nil work-buffer)
		(base64-insert-char (aref alphabet (logand (lsh bits -6) 63))
				    1 nil work-buffer)
		(base64-insert-char ?= 1 nil work-buffer)))
	    (if (> cols 0)
		(base64-insert-char ?\n 1 nil work-buffer)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Encoding base64... done"))

(defun base64-encode (string)
  (save-excursion
    (set-buffer (get-buffer-create " *base64-encode*"))
    (erase-buffer)
    (insert string)
    (base64-encode-region (point-min) (point-max))
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(defun base64-decode (string)
  (save-excursion
    (set-buffer (get-buffer-create " *base64-decode*"))
    (erase-buffer)
    (insert string)
    (base64-decode-region (point-min) (point-max))
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))  

(defalias 'base64-encode-string 'base64-encode)
(defalias 'base64-decode-string 'base64-decode)

(provide 'base64)
