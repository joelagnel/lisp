;;; mew-mule0.el --- Environment of non-Mule for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997

;;; Code:

;; must be here
(defun mew-coding-system-p (cs)
  (cond
   ((null cs) t)
   ((eq cs 'iso-8859-1) t)
   (t nil)))

;; In the context of Mew, 'charset' means MIME charset.
;; 'cs' means the internal representation of Emacs (was known as Mule).

(defun mew-cs-raw-p (cs) ())

;;
;; User CS definitions
;;

(defvar mew-cs-dummy          nil)
(defvar mew-cs-binary         nil)
(defvar mew-cs-text-for-read  nil)
(defvar mew-cs-text-for-write nil)
(defvar mew-cs-text-for-net   nil)
(defvar mew-cs-autoconv       nil)
(defvar mew-cs-m17n           nil)
(defvar mew-cs-utf-16be       nil)

(defvar mew-cs-eol "\r\n")

(defun mew-eol-fix-for-read ()
  (goto-char (point-min))
  (mew-crlf-to-lf))

(defun mew-eol-fix-for-write ()
  (goto-char (point-min))
  (mew-lf-to-crlf)
  t) ;; return value

(defvar mew-cs-database-for-encoding
  '(((ascii)                 nil        "7bit"             "7bit")
    ((ascii latin-iso8859-1) iso-8859-1 "quoted-printable" "Q")))

(defvar mew-cs-database-for-decoding
  '(("us-ascii"   . nil)
    ("iso-8859-1" . iso-8859-1)))

;;
;; CS
;;

(defun mew-find-cs-region (beg end &optional dummy)
  (let (ret)
    (save-excursion
      (goto-char beg)
      (if (re-search-forward "[\000-\177]" end t)
	  (setq ret (cons 'ascii ret)))
      (goto-char beg)
      (if (re-search-forward "[\200-\377]" end t)
	  (setq ret (cons 'latin-iso8859-1 ret))))
    ret))

;; to internal
(defsubst mew-cs-decode-region (beg end cs) nil)

;; to external
(defsubst mew-cs-encode-region (beg end cs) nil)

;; to internal
(defsubst mew-cs-decode-string (str cs) str)

;; to external
(defsubst mew-cs-encode-string (str cs) str)

;;
;; Process environment
;;

(defsubst mew-set-process-cs (pro from-pro to-pro) nil)

(defsubst mew-set-buffer-cs (write) nil)

(defmacro mew-plet (&rest body)
  `(progn ,@body))

(defmacro mew-piolet (input output &rest body)
  `(progn ,@body))

(defmacro mew-flet (&rest body)
  `(let ((jka-compr-inhibit t))
     ,@body))

(defmacro mew-frwlet (read write &rest body)
  `(let ((jka-compr-inhibit t))
     ,@body))

(defmacro mew-alet (&rest body)
  `(progn ,@body))

;;
;;
;;

(defun mew-substring (str width &optional cnt nopad)
  (let ((sw (length str)))
    (cond
     ((= sw width)
      str)
     ((< sw width)
      (if nopad
          str
        (concat str (make-string (- width sw) mew-sp))))
     (t
      (if cnt
	  (concat (substring str 0 (- width 2)) "..")
	(substring str 0 width))))))

;;
;; Language specific
;;

(defvar mew-lc-kana nil) ;; dummy

;;
;;
;;

(require 'mew-mule)
(provide 'mew-mule0)

;;; Copyright Notice:

;; Copyright (C) 1997-2000 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-mule0.el ends here
