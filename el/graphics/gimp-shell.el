;;; gimp-shell.el --- shell for Gimp Script Fu Server.

;; Copyright (C) 2000 Olaf Sylvester
;;
;; Author: Olaf Sylvester <olaf@geekware.de>
;; Web site: http://www.geekware.de/software/emacs
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.
;;

;; For upgrading see web site: http://home.netsurf.de/olaf.sylvester/emacs
;; or watch news group gnu.emacs.sources

;;; Commentary:
;; Version: 0.1
;; 
;; Install:
;; 0. Start GIMP and Script-FU Server
;; 1. Load gimp-shell.el
;; 2. Start Gimp-Shell with interactive command gimp-shell
;; 
;; Features:
;; - Command History with M-p and M-n
;; - GIMP-Function description by interactive command gimp-describe-function
;;   (use SPACE for completion)
;; - load current scheme file by interactive command gimp-load-current-file
;; - load any scheme file by interactive command gimp-load-file


;;; History:
;; 

;;; Code:

(require 'cmuscheme)

(defvar gimp-user-prompt "You> "
  "Prompt of user.")

(defvar gimp-gimp-prompt "Gimp> "
  "Prompt of Gimp process.")

(defvar gimp-process nil)

(defvar gimp-process-string "")
;;(setq gimp-process-string "")

(defvar gimp-result-handler 'gimp-handler-insert
  "Handler to handle results comming from script-fu server.")

(defvar gimp-mode-map nil)

(defvar gimp-users-input-history-index 0)

(defvar gimp-users-input-history '())

(defvar gimp-interactive-buffer-name "*gimp*")

(defvar gimp-info-buffer-name "*gimp-info*")

(defvar gimp-db-cache ())
(defvar gimp-db-cache-for-completing ())

(defun gimp-shell ()
  "Switch to gimp-shell buffer."
  (interactive)
  (if (and gimp-process
	   (eq 'open (process-status gimp-process)))
      (switch-to-buffer gimp-interactive-buffer-name)
    (call-interactively 'gimp-start-client)))

      
(defun gimp-start-client (host port)
  (interactive 
   (list 
    (read-from-minibuffer "Host: " "localhost")
    (read-from-minibuffer "Port: " "10008")
    ))
  (if (string= host "")
      (setq host "localhost"))
  (if (string= port "")
      (setq port 10008)
    (setq port (string-to-int port)));; 
  (condition-case error-data
      (setq gimp-process  
	    (open-network-stream "gimp" gimp-interactive-buffer-name host port))
    (error (progn
	     (error "Gimp server running ... no ??? Cannot start gimp client because of %S" error-data)
	     )))
  (set-process-filter gimp-process 'gimp-filter)
  (switch-to-buffer gimp-interactive-buffer-name)
  (gimp-mode)
  (goto-char (point-max))
  (insert-string gimp-user-prompt)
  (setq gimp-process-string "")
  (gimp-startup)
  )

(defun gimp-startup ()
  "Startup commands"
  (gimp-send-string 
   "(define (int--emacs-tostring item)
      (cond ((number? item) (number->string item 10))
 	((string? item) (print-to-string item (string-append (substring item 0)
                                                             \"                                 \" )))
 	((null? item)   \"()\")
        ((eq? t item)    \"t\")
 	((pair? item) 
 	 (string-append \"(\" 
		(unbreakupstr 
 			 (map int--emacs-tostring item) \" \") \")\"))
        (TRUE item)))")
  (gimp-cache-functions)
  (gimp-send-string "\"Script-fu is running ...\"")
  )
;;(gimp-startup)

(defun gimp-result-complete (string)
  (and (>= (length string) 4)
       (let* ((magik-char (aref string 0))
	      (error-p (not (eq 0 (aref string 1))))
	      (high (aref string 2))
	      (low (aref string 3))
	      (len (+ 4 low (* 256 high))))
	 (if (not (eq magik-char ?G))
	     (message "No MAGIK in %S" string))
	 ;;(message "%S of %S. Missing: %S" (length string) len
	;;	  (- len (length string)))
	 (if (>= (length string) len)
	     (list (subseq string 4 len)
		   (subseq string len) 
		   error-p)))))

(defun gimp-filter (proc str)
  (if (fboundp 'string-make-unibyte)
      (setq str (string-make-unibyte str)))
  ;;(message "Got: %s" str)
  (setq gimp-process-string (concat gimp-process-string str))
  (let ((ok nil))
    (while (setq ok (gimp-result-complete gimp-process-string))
      (condition-case err
	  (apply gimp-result-handler proc ok)
	(error (progn
		 (message "gimp result eval error: %S" err))))
      (setq gimp-process-string (nth 1 ok))
      ;;(setq gimp-process-string "")
      )))
	   
(defun gimp-cache-functions ()
  (interactive)
  (gimp-call-with-callback "(gimp-procedural-db-query \"\" \"\" \"\" \"\" \"\" \"\" \"\")"
			   'gimp-set-cache-functions))


(defun gimp-call-with-callback (gimp-call emacs-callback-function)
  (gimp-send-string (format "(int--emacs-tostring (list \"EMACS-CALL %s\" %s))"
			    emacs-callback-function
			    gimp-call)))


(defun gimp-set-cache-functions (list)
  (setq gimp-db-cache (car (cdr list)))
  (setq gimp-db-cache-for-completing (mapcar (function list) gimp-db-cache)))

(defun gimp-handler-insert (proc result-string rest-string error-p)
  (if (string-match "(\"EMACS-CALL \\([^\"]+\\)\""
		    result-string)
	(let ((fun (car (read-from-string (match-string 1 result-string)))))
	  ;;(y-or-n-p (format "%S" fun))
	  ;;(message "Special function %s" fun)
	  ;;(message (substring result-string (+ 2 (match-end 1))))
	  ;;(message (gimp-string-sub (substring result-string (+ 2 (match-end 1)))
	  ;; "\"" "\\\""))
	  (funcall fun 
		   (car (read-from-string 
			 (substring result-string (+ 2 (match-end 1)))))))
    (save-excursion
      (set-buffer gimp-interactive-buffer-name)
      (goto-char (process-mark proc))
      (insert-string gimp-gimp-prompt result-string)
      (insert-string "\n")
      (set-marker (process-mark proc) (point)))))

(defun gimp-send-string (string)
  (let* ((pre "G")
	 (len (length string))
	 (high (/ len 256))
	 (low (mod len 256)))
    (if (> len 65535)
	(error "GIMP send-string: String to long: %d" len))
    (if (> low 0)
	;; arghh Problems with multibyte and send string. Assert low length of 0
	(setq string (concat string (make-string (- 256 low) ? )) 
	      low 0
	      high (1+ high)))
    (setq pre (concat pre 
		      (char-to-string high) 
		      (char-to-string low)))
    ;;(message "to GIMP: %d %d %S %S %s" low high pre (string-make-unibyte pre) string)
    (if (fboundp 'string-as-unibyte)
	(setq pre (string-as-unibyte pre)))
    (send-string gimp-process pre)
    (send-string gimp-process string)))

(defun gimp-send-region (start ende)
  (interactive "r")
  (gimp-send-string (format "(int--emacs-tostring %s)"
			    (buffer-substring start ende))))

(setq gimp-mode-map
      (make-sparse-keymap))

(define-key gimp-mode-map [return] 'gimp-newline)
(define-key gimp-mode-map (read-kbd-macro "M-p") 'gimp-previous-command)
(define-key gimp-mode-map (read-kbd-macro "M-n") 'gimp-next-command)

(defun gimp-previous-command (n)
  (interactive "p")
  (gimp--previous-next-command n))

(defun gimp-next-command (n)
  (interactive "p")
  (gimp--previous-next-command (* -1 n)))
  
(defun gimp--previous-next-command (offset)
  (unless (memq last-command '(gimp-previous-command gimp-next-command))
    (setq gimp-users-input-history-index -1))
  (gimp-add-current-history-index offset)
  (when gimp-users-input-history
    (apply 'delete-region (gimp-region-to-send))
    (insert (nth gimp-users-input-history-index
		 gimp-users-input-history))))

(defun gimp-add-current-history-index (offset)
  (setq gimp-users-input-history-index 
	(+ offset gimp-users-input-history-index))
  (if (< gimp-users-input-history-index 0)
      (setq gimp-users-input-history-index 
	    (1- (length gimp-users-input-history)))
    (if (<= (length gimp-users-input-history) 
	    gimp-users-input-history-index)
	(setq gimp-users-input-history-index 0))))

(defun gimp-string-sub (str from to)
  "return a string with any matches for the regexp, `from', replaced by `to'."
  (save-match-data
    (prog1
        (if (string-match from str)
            (concat (substring str 0 (match-beginning 0))
                    to
                    (gimp-string-sub (substring str (match-end 0)) from to))
          str))))

(defun gimp-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gimp-mode)
  (setq mode-name "Gimp")
  (use-local-map gimp-mode-map)
  (make-local-variable 'kill-buffer-hook)
  ;;(add-hook 'kill-buffer-hook 'gimp-say-goodbye)
  (set-marker (process-mark (get-buffer-process (current-buffer))) 
	      (point-max)))

(defun gimp-newline ()
  (interactive)
  ;;(if (not (string= "" gimp-process-string))
  ;;    (error "Waiting for result ..."))
  (let ((str (gimp-string-to-send)))
    (if (> (length str) 0)
	(progn
	  ;;(y-or-n-p str)
	  (if (or (null gimp-users-input-history)
		  (not (string= (car gimp-users-input-history)
				str)))
	      (setq gimp-users-input-history
		    (cons str gimp-users-input-history)))
	  (gimp-send-string 
	   (concat "(int--emacs-tostring " str ")"))))
    (newline)
    (set-marker (process-mark (get-buffer-process (current-buffer))) 
		(point-max))
    (insert-string gimp-user-prompt)))

(defun gimp-string-to-send ()
  "Return users input for script-fu evaluation."
  (apply 'buffer-substring (gimp-region-to-send)))

(defun gimp-region-to-send ()
  "Return region for script-fu evaluation."  
  (save-excursion
    (goto-char (process-mark gimp-process))
    (search-forward-regexp gimp-user-prompt nil t)
    (list (point)
	  (point-max))))

(defun gimp-load-file (filename)
  (interactive "fFile: ")
  (gimp-send-string (concat "(load \"" filename "\")")))

(defun gimp-load-current-file ()
  (interactive)
  (gimp-load-file (buffer-file-name)))

(defun gimp-append-to-info (&rest strings)
  (save-excursion
    (while strings
      (let ((string (car strings)))
	(set-buffer gimp-info-buffer-name)
	(goto-char (point-max))
	(cond ((listp string)
	       (mapcar (lambda (x)
			 (insert-string (format "%s\n" x)))
		       string))
	      (t (insert (format "%s" string)))))
      (setq strings (cdr strings)))))

(defun gimp-pop-to-info (string)
  (pop-to-buffer gimp-info-buffer-name)
  (save-excursion 
    (set-buffer gimp-info-buffer-name)
    (erase-buffer)
    (gimp-append-to-info string)))

(defun gimp-info-function (tupel &optional append-p)
  "Callback function to create a description for a Gimp-PD-function."
  (let ((fun (car tupel))
	(description (cadr tupel)))
    (if append-p
	(gimp-append-to-info (format "Name  : %s\n" fun))
      (gimp-pop-to-info (format "Name  : %s\n" fun)))
    (gimp-append-to-info "Blurb : " (car description) "\n")
    (gimp-append-to-info "Help  : " (nth 1 description) "\n")
    (gimp-append-to-info "Author: " (nth 2 description) "\n")
    (gimp-append-to-info "(C)   : " (nth 3 description) "\n")
    (gimp-append-to-info "Date  : " (nth 4 description) "\n")
    (gimp-append-to-info "\n")
    ;(gimp-append-to-info description "\n")
    ;(message "%S" description)
    (dotimes (i (nth 6 description))
      (gimp-call-with-callback (format "(gimp-procedural-db-proc-arg \"%s\" %d)" 
				       fun i)
			       "gimp-info-arg"))
    (dotimes (i (nth 7 description))
      (gimp-call-with-callback (format "(gimp-procedural-db-proc-val \"%s\" %d)" 
				       fun i)
			       "gimp-info-result"))
    ))

(defun gimp-show-infos-to-function (pdb-function)
  (interactive "sName: ")
  (gimp-call-with-callback
   (format "(list \"%s\" (gimp-procedural-db-proc-info \"%s\"))" 
	   pdb-function
	   pdb-function)
   "gimp-info-function"))

(defvar gimp-type-to-readable-type-map 
  '((0  . "INT32")
    (1  . "INT16")
    (2  . "INT8")
    (3  . "FLOAT") 
    (4  . "STRING")
    (5  . "INT32ARRAY")
    (6  . "INT16ARRAY") 
    (7  . "INT8ARRAY")
    (8  . "FLOATARRAY") 
    (9  . "STRINGARRAY") 
    (10 . "COLOR")
    (11 . "REGION")
    (12 . "DISPLAY") 
    (13 . "IMAGE") 
    (14 . "LAYER")
    (15 . "CHANNEL") 
    (16 . "DRAWABLE") 
    (17 . "SELECTION") 
    (18 . "BOUNDARY") 
    (19 . "PATH") 
    (20 . "STATUS")))

(defun gimp-type-to-readable-type (type)
  (or (cdr (assoc type gimp-type-to-readable-type-map))
      "UNKNOWN"))

(defun gimp-info-arg (liste)
  (gimp-append-to-info (format "Arg   : %-10s  %-10s  %s\n" 
			       (car (cdr liste)) 
			       (gimp-type-to-readable-type (car liste))
			       (car (cdr (cdr liste)))
			       (length liste))))

(defun gimp-info-result (liste)
  (gimp-append-to-info (format "Result: %-10s  %-10s  %s\n" 
			       (car (cdr liste)) 
			       (gimp-type-to-readable-type (car liste))
			       (car (cdr (cdr liste)))
			       (length liste))))


(defun gimp-describe-function (function)
  "Display the full documentation of Gimp FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Describe gimp function (default %s): " fn)
				  "Describe gimp function: ")
				gimp-db-cache-for-completing
				nil nil nil nil (symbol-name fn)))
     (list (if (equal val "")
	       fn (intern val)))))
  (gimp-show-infos-to-function function))


;;; Scheme mode modifications

(defun scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (gimp-send-region start end))

(defun scheme-compile-region (start end)
  "Compile the current region in the inferior Scheme process.
\(A BEGIN is wrapped around the region: (BEGIN <region>))"
  (interactive "r")
  (error "Not implemented yet"))

(defun scheme-load-file (file-name)
  "Load a Scheme file into the inferior Scheme process."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
				  scheme-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (error "Not implemented yet"))


;; (gimp-start-client)

;; 
;; (gimp-send-string "(int--emacs-tostring (+ 2 3 4))")
;; (gimp-send-string "")


(provide 'gimp)

;;; gimp-shell.el ends here
