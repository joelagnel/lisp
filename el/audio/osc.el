;;; osc.el --- Open Sound Control

;; Copyright (C) 2005  Mario Lang

;; Author: Mario Lang <mlang@delYsid.org>
;; Keywords: comm, processes, multimedia

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

;;; Commentary:

;; OpenSound Control ("OSC") is a protocol for communication among
;; computers, sound synthesizers, and other multimedia devices that is
;; optimized for modern networking technology and has been used in many
;; application areas.

;; BUGS/TODO:
;;
;; * Bundles, timetags and binary blobs are not supported yet.

;; Usage:
;;
;; Client: (osc-send "localhost" 7770 "/osc/path" 1.5 1.0 5 "done")
;;          OR
;;         (let ((osc-host "localhost")
;;               (osc-port 7770))
;;           (osc-send-message "/jamin/scene" 5)
;;           (sleep 10)
;;           (osc-send-message "/jamin/scene" 2))
;;
;; Server: (setq my-server (osc-make-server "localhost" 7770
;;          (lambda (path &rest args)
;;            (message "OSC %s: %S" path args))))


;; or
;; (osc-send "localhost" 7770 "/path/to/something" 5.0 0.5 22.1 5 "no")

;; To create a server, do something like this:

;; (setq my-server
;;       (osc-make-server
;;        "localhost" 7770
;;        (lambda (path &rest args)
;;          (message "Unhandled: %s %S" path args))))
;; (osc-server-set-handler my-server "/a/b/c/d" (lambda (path &rest args) ...))

;;; Code:

(defvar osc-host "localhost"
  "*OSC host to contact if `osc-send-message' is called.")

(defvar osc-port 7770
  "*OSC port to use if `osc-send-message' is called.")
  
(defun osc-insert-string (string)
  (insert string 0 (make-string (- 3 (% (length string) 4)) 0)))

(defun osc-insert-float32 (value)
  (let (s (e 0) f)
    (cond
     ((string= (format "%f" value) (format "%f" -0.0))
      (setq s 1 f 0))
     ((string= (format "%f" value) (format "%f" 0.0))
      (setq s 0 f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((string= (format "%f" value) (format "%f" 0.0e+NaN))
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (unless (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (insert (+ (lsh s 7) (lsh (logand e #XFE) -1))
	    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
	    (lsh (logand f #XFF00) -8)
	    (logand f #XFF))))

(defun osc-insert-int32 (value)
  (let (bytes)
    (dotimes (i 4)
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun osc-send-message (path &rest args)
  "Send an OSC message to the specified PATH with ARGS.
`osc-host' and `osc-post' are used to determine the target address."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (osc-insert-string path)
    (osc-insert-string
     (apply 'concat "," (mapcar (lambda (arg)
				  (cond
				   ((floatp arg) "f")
				   ((integerp arg) "i")
				   ((stringp arg) "s")
				   (t (error "Invalid argument: %S" arg))))
				args)))
    (dolist (arg args)
      (cond
       ((floatp arg) (osc-insert-float32 arg))
       ((integerp arg) (osc-insert-int32 arg))
       ((stringp arg) (osc-insert-string arg))))
    (let ((proc (make-network-process
		 :name "OSCclient"
		 :coding 'binary
		 :host osc-host
		 :service osc-port
		 :type 'datagram)))
      (process-send-string proc (buffer-string))
      (delete-process proc))))

(defun osc-send (host port path &rest args)
  "Send an OSC message to HOST and PORT with the specified PATH and ARGS."
  (let ((osc-host host)
	(osc-port port))
    (apply 'osc-send-message path args)))

(defun osc-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun osc-read-int32 ()
  (let ((value 0))
    (dotimes (i 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun osc-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s)))
     ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
      (* 1.0e+INF (expt -1 s)))
     ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
      0.0e+NaN)
     (t
      (* (round (* (* (expt -1 s)
		      (expt 2.0 (- e 127))
		      (1+ (/ f (expt 2.0 23))))
		   (expt 10.0 7.0)))
	 (expt 10.0 -7.0))))))

(defun osc-server-set-handler (server path handler)
  "Set HANDLER for PATH on SERVER.
IF HANDLER is nil, remove previously defined handler and fallback to
the generic handler for SERVER."
  (let* ((handlers (plist-get (process-plist server) :handlers))
	 (slot (assoc-string path handlers)))
    (if slot
	(setcdr slot handler)
      (plist-put
       (process-plist server)
       :handlers (nconc (list (cons path handler)) handlers)))))

(defun osc-server-get-handler (server path)
  (or (cdr (assoc path (plist-get (process-plist server) :handlers)))
      (plist-get (process-plist server) :generic)))

(defun osc-filter (proc string)
  (when (= (% (length string) 4) 0)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      (let ((path (osc-read-string)))
	(when (looking-at ",")
	  (apply (osc-server-get-handler proc path)
		 path
		 (mapcar
		  (lambda (type)
		    (case type
		      (?f (osc-read-float32))
		      (?i (osc-read-int32))
		      (?s (osc-read-string))))
		  (string-to-list (substring (osc-read-string) 1)))))))))

(defun osc-make-server (host port default-handler)
  (make-network-process
   :name "OSCserver"
   :filter #'osc-filter
   :host host
   :service port
   :server t
   :type 'datagram
   :plist (list :generic default-handler)))

(defun osc-test-transport-equality (value)
  "Test if transporting a certain VALUE via OSC results in equality.
This is mostly for testing the implementation robustness."
  (let ((osc-test-value value)
	(osc-test-func (cond ((or (floatp value) (integerp value)) '=)
			     ((stringp value) 'string=)))
	(osc-test-done nil)
	(osc-test-ok nil)
	(server (osc-make-server
		 "localhost" t
		 (lambda (path v)
		   (setq osc-test-done t
			 osc-test-ok (funcall osc-test-func
					      osc-test-value v))))))
    (osc-send (nth 0 (process-contact server)) (nth 1 (process-contact server))
	      "/test" osc-test-value)
    (while (not osc-test-done)
      (accept-process-output server 0 500))
    (delete-process server)
    osc-test-ok))

(provide 'osc)
;;; osc.el ends here
