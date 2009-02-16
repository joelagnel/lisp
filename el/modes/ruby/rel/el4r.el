;; el4r - EmacsLisp for Ruby 
;; Copyright (C) 2005 rubikitch <rubikitch@ruby-lang.org>
;; Version: $Id: el4r.el 1280 2006-06-24 08:33:17Z rubikitch $

;; Major changes by Paul Stickney <pstickne@gmail.com>, Sep 2006.

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;

(or (>= emacs-major-version 21)
    (error "Sorry, el4r requires (X)Emacs21 or later, because it uses weak hash."))

(put 'el4r-ruby-error
     'error-conditions                        
     '(error el4r-ruby-error))
(put 'el4r-ruby-error 'error-message "Error raised in Ruby")

;; What is this useful for?
;; (defun call-process-to-string (program &rest args)
;;   (with-temp-buffer
;;     (apply 'call-process program nil t nil args)
;;     (buffer-string)))
;; (defun call-process-and-eval (program &rest args)
;;   (eval (read (apply 'call-process-to-string program args))))

;; Fix for something - what? -pst
(unless (fboundp 'process-send-signal)
  (defun process-send-signal (signal process-or-name)
    (signal-process (process-id (get-process process-or-name)) signal)))


(defconst el4r-ruby-program "ruby"
  "Ruby executable to use to start `el4r-server-program'.")

(defconst el4r-server-program "/home/pstickne/el4rim.rb"
  "Full path of el4r server.")

(defvar el4r-instance-args nil)
(defvar el4r-debug-on-error nil)
(defvar el4r-coding-system nil)

(defconst el4r-log-bufname "log")

(require 'el4r-log)

(defvar el4r-process-name "el4r")
(defconst el4r-process-buffer-name "process")

(defvar el4r-call-level 0)
(defvar el4r-last-error-desc nil)

(defvar el4r-ruby-object-ids nil)
(defvar el4r-ruby-object-weakhash nil)
;(defvar el4r-defun-hash nil)
(defvar el4r-defun-lambdas nil)
(defvar el4r-lisp-object-hash nil)
(defvar el4r-lisp-object-lastid 0)
(defvar el4r-lisp-object-gc-trigger-count 100)
(defvar el4r-lisp-object-gc-trigger-increment 100)


(defun el4r-process-buffer ()
  (get-buffer-create el4r-process-buffer-name))

;;(defun el4r-load (script)
;;  "Loads Ruby script from ~/.el4r directory."
;;  (el4r-ruby-call nil "el4r_load" script))
;; (defun el4r-override-variables ())

(defun el4r-set-process-filter ()
  (set-process-filter el4r-process 'el4r-process-filter))

(defun el4r-remove-process-filter ()
  (set-process-filter el4r-process nil))

(defun el4r-prepare-data-structures ()
  (setq el4r-lisp-object-hash (make-hash-table :test 'eq))
  (setq el4r-ruby-object-weakhash (make-hash-table :test 'eq :weakness 'value))
  ;;  (setq el4r-defun-hash (make-hash-table :test 'eq))
  (setq el4r-defun-lambdas nil))


(require 'el4r-process)

(defun el4r-start ()
  "Intialize the connection with the Server."
  (interactive)

  (if (el4r-running-p) ;; don't allow double-starts
      (el4r-stop))

  (el4r-log-info "-- SERVER STARTING --")
  (el4r-connect)
  (if (el4r-alive-or-error-p)
      (el4r-prepare-state)
    ;; something failed -- how do we know what? A signal?
    (el4r-log-error "Can't start server")))


(defun el4r-prepare-state ()
  (el4r-prepare-data-structures)
  (el4r-set-process-filter)
  (el4r-reset-chunks)
  (el4r-announce-alive))

;; I have no idea what a coding system is.
;; (if el4r-coding-system
;;   (set-process-coding-system
;;   (el4r-process el4r-coding-system el4r-coding-system)))

(defun el4r-connect ()
  ;; XXX ??
  ;;   (with-current-buffer (el4r-process-buffer)
  ;;     (set (make-local-variable 'process-adaptive-read-buffering) nil)
  (let ((process-connection-type nil) ; use a pipe
	(proc-name el4r-process-name)
	(proc-buffer (el4r-process-buffer))
	(proc-args (apply 'list
			  el4r-ruby-program
			  el4r-server-program
			  el4r-instance-args)))
    (el4r-attach-process
     (apply 'start-process proc-name proc-buffer proc-args))
    ))


(defun el4r-stop (&optional silent)
  "Shutdown el4r."
  (interactive)
  (el4r-log-info "-- SERVER STOPPING --")
  ;; Clean-up filter, not sure if there is a need.
  (if (processp el4r-process)
      (el4r-remove-process-filter))
  (cond ((el4r-running-p)
;;	 (el4r-ruby-eval "el4r_shutdown")
	 (el4r-kill-process)
	 (el4r-log-info "Shutdown complete"))
	((not silent)
	 (el4r-log-warn "Server is not running: can't shutdown"))))

(defun el4r-restart ()
  "Restart el4r. Most useful for clearing state."
  (interactive)
  (el4r-stop)
  (el4r-start))


;;;
(defun el4r-announce-dead (&optional suppress-error)
  "Puts various messages that the process is dead and throws an error."
  (let ((message (format "Server is not running: status=%s exit-status=%d"
			 (symbol-name (el4r-process-status))
			 (process-exit-status el4r-process))))
    ;; Overkill?
    (el4r-log-warn message)))

(defun el4r-announce-alive ()
  "Put various messages saying we are alive."
  (el4r-log-info (format "Process created with ID %d" (process-id el4r-process)))
  (el4r-log-info (format "Process status is: %s" (symbol-name (process-status el4r-process)))))

(defun el4r-alive-or-error-p ()
  "Raises an error if the process is not alive or returns t."
  (if (el4r-is-alive-p)
      t
    (el4r-announce-process-dead)
    (error "Server is not running")))


;;;
;;; Basic protocol functions
;;;

(defconst el4r-to-ruby-sep "\n")
(defconst el4r-from-ruby-sep "\n")

(require 'el4r-chunks)

(setq el4r-temp-chunk "")
(defun el4r-process-filter (process input)
  "Process filter for responses from the el4r server.
Will add chunks which can be read later with `el4r-next-chunk'."
  (el4r-log-debug "Recieved input: [[%s]]" input)
  (setq el4r-temp-chunk (concat el4r-temp-chunk input))
  (save-match-data
    (while (string-match el4r-from-ruby-sep el4r-temp-chunk)
      (let* ((next-chunk-start (+ (match-end 0)))
	     (chunk-end (- next-chunk-start (length el4r-from-ruby-sep)))
             (chunk (substring el4r-temp-chunk 0 chunk-end)))
	(el4r-add-chunk (el4r-read-entire chunk))
	(setq el4r-temp-chunk (substring el4r-temp-chunk next-chunk-start))
	))))

(defun el4r-recv (&optional timeout)
  "Read data from the Server.
This probably shouldn't be called directly."
  (el4r-alive-or-error-p)
  (setq timeout (or timeout 5))
  (with-timeout (timeout "(el4ri-timeout-error)")
    (while (not (el4r-chunk-ready-p))
      (accept-process-output el4r-process))
    (el4r-next-chunk)))

(defun el4r-send (payload)
  "Actually send the data to the Server.
This probably shouldn't be called directly.  PAYLOAD is a string.  May
signal an error."
  (el4r-log-debug "Sending data: [[%s]]" payload)
  (let ((packet (concat payload el4r-from-ruby-sep)))
    (condition-case the-error
	(process-send-string el4r-process packet)
      (error (el4r-announce-dead t)
             (resignal the-error)))))

(defconst el4r-debug-on-eval nil)
(defconst el4r-debug-on-read nil)

(defun el4r-read-eval (sexp)
  "Wrapper to read and eval a sexp."
  (el4r-eval
   (el4r-read-entire sexp)))

(defun el4r-eval (sexpr)
  "Eval a sexpr.  May signal an error."
    (condition-case err
	(eval sexpr)
      (error (progn
	       (el4r-log-error (format "Error evaling: %s" sexpr))
	       (if el4r-debug-on-read
		   (resignal err))
	       ))))

(defun el4r-read-entire (string)
  "Reads a sexpr from SEXPR-STRING like `read-from-string'.  However this
will all signal an error if the sexpr does not cover the entire string."
  (condition-case err
      (let ((read-data (read-from-string string)))
        (if (and read-data
                 (eq (cdr read-data) (length string)))
            (car read-data)
          (error "Error reading: %s" string)))
    (end-of-file
     (el4r-log-error "Error reading: %s" string)
     (resignal err))))

;;;
;;; One level higher protocol
;;;

(defun el4r-make-header (header-alist)
  "HEADER-ALIST is an alist of (OPTION . VALUE).  OPTION must be a symbol.
VALUE must be a string."
  (mapconcat
   (lambda (cons)
     (concat (symbol-name (car cons))
             "=" (cdr cons)))
   header-alist "/"))

(defun el4r-server-echo (string)
  "Ask server to echo STRING back."
  (el4r-do '(("c" . "echo")) string))

(defun el4r-do (header data)
  "Send a command to the server and get some data back. See
`el4r-make-header'."
  (el4r-do-raw (concat "#," header "#" data)))

(defun el4r-do-raw (data)
  "Send DATA (string) to the server. Returns response."
  (el4r-send data)
  (el4r-recv))

(defun el4r-response-status (response)
  (car response))

(defun el4r-response-options (response)
  (nth 1 response))

(defun el4r-response-data (response)
  (nth 2 response))

;; XXX need better doc string
(defun el4r-ruby-eval (ruby-expr)
  "Send a RUBY-EXPR to the server to evaluate.  The result is returned or an
error is raised.  Because this also evaluates the result as elisp there may
be side-effects.  This also allows for elisp callbacks from ruby."
  ;; XXX What does this do?
  ;;(if (el4r-callback-p)
  ;; (el4r-send-interrupt))
  (let ((outgoing-header (el4r-make-header '((c . "run"))))
        (outgoing-data ruby-expr)
        finished
        result)
    (while (not finished)
      ;; el4r-do is where the communication occurs
      (let* ((response (el4r-do outgoing-header outgoing-data))
             (status (el4r-response-status response))
             (data (el4r-response-data response))) ;;already sexpr, don't read
        (case status
          (= ;; result
           (setq result data)
           (setq finished t))
          (* ;; continuing
           (let* ((options (el4r-response-options response))
                  (transaction (cdr (assq 't options))))
             (setq outgoing-header
                   (el4r-make-header (list
                                      (cons 'c "run")
                                      (cons 't transaction))))
             (condition-case err
                 ;; XXX should be able to specify how to pass back response
                 ;; response converted to ruby
                 (setq outgoing-data (el4r-to-ruby (el4r-eval-sexpr data)))
               (error ;; errors get passed back to Ruby
                (el4r-log-error "Error with elisp: %S" err)
                (setq outgoing-data "raise 'An error occured in elisp'")))
             ))
          (! ;; error
           (let* ((err data)
                  (err-type    (nth 0 err))
                  (err-message (nth 1 err))
                  (err-trace   (nth 2 err)))
             (el4r-log-error "ERROR: %s" err-type)
             (el4r-log-error "    %s" err-message)
             (dolist (trace-item err-trace)
               (el4r-log-error "    %s" trace-item))
;;                  (message (apply 'concat "\n" err-message err-trace)))
             (error "Error recieved: %s" err-message)))
          (t ;; unknown
           (error "Unknown/unsupported status: %S" status)))
        ))
    result
    ))


(defun ruby-eval (ruby-expr)
  (interactive "sRuby:")
  (condition-case err
      (message
       (prin1-to-string
        (el4r-ruby-eval ruby-expr)))
    (error
     (el4r-log-error "%S" err)
     (message "An error occured."))))

;;;
;;; el4ri-* functions are for interaction from the el4r Server
;;;

;; XXX simulate via a faked response?
(defun el4ri-timeout-error (&optional seconds)
  (el4r-log-error "Taking too long: stopping"))



(provide 'el4r)