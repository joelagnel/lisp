;; @(#)@ snefru.el 1.3 - Snefru support for GNUS
;;
;; This file defines functions to calculate a Snefru checksum, add
;; it to outgoing postings, and validate it on incoming postings.
;;
;; It uses "gnus-Inews-article-hook", called by GNUS just before passing
;; the articel to inews, to install the checksum.
;;
;; "gnus-Article-prepare-hook" is used to validate the checksum on
;; an article if you read it.
;;
;; This file, if useful, is covered by the GPL.
;;
;;	Johan Vromans <jv@mh.nl>

;; Customizations

(defvar snefru-command "snefru" "*Where to find snefru")

(defvar snefru-checksum-header "X-Checksum-Snefru")

(defvar snefru-insertion t
  "*Controls Snefru checksum insertion. If nil, no checksum is
  calculated nor inserted.")

(defvar snefru-validation 1
  "*Controls Snefru checksum validation. If nil, no validation is
  performed. If t, validation is performed, and failures are reported.
  Any other value causes validation to be performed, and failures as
  well as successes to be reported.")

;; Hook definitions and insertions.

(defvar gnus-Inews-article-hook nil)

(defvar gnus-Article-prepare-hook nil)

(if (memq 'snefru-add-checksum gnus-Inews-article-hook)
    nil
  (setq gnus-Inews-article-hook 
	(cons 'snefru-add-checksum gnus-Inews-article-hook)))
(if (memq 'snefru-validate-checksum gnus-Article-prepare-hook)
    nil
  (setq gnus-Article-prepare-hook 
	(cons 'snefru-validate-checksum gnus-Article-prepare-hook)))
;;
;; Calcuates the Snefru checksum for the article to be posted, which
;; is assumed to be in the current buffer.
;;
(defun snefru-add-checksum ()
  "Adds a Snefru-checksum to the article being posted. Must be called
from gnus-Inews-article-hook."
  (interactive)

  (if (null snefru-insertion)
      nil
    (let (start-of-body)
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq start-of-body (point-marker))	; remember where
      
      ;; Run snefru and add the checksum.
      (forward-line -1)
      (insert snefru-checksum-header ": ")
      (insert (snefru-checksum-region start-of-body (point-max)))
      (insert "\n")
      )))

;;
;; Validate Snefru checksum. A message is shown with the result.
;; If the checksum does not match, buffer "*Snefru Buffer*" holds more
;; information.
;;
(defun snefru-validate-checksum ()
  "Checks a Snefru-checksum in the article being read. May be called
from gnus-article-prepare-hook."
  (interactive)

  (if (null snefru-validation)
      nil
    (let (start-of-body)
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq start-of-body (point-marker))	; remember where

      ;; Check if a checksum header is present
      (goto-char (point-min))
      (if (search-forward 
	   (concat "\n" snefru-checksum-header ": ")
	   start-of-body t)
	  (let (checksum (here (point)))
	    (forward-line 1)
	    (setq checksum (buffer-substring here (1- (point))))

	    ;; Validate
	    (if (string= 
		 checksum
		 (snefru-checksum-region start-of-body (point-max)))
		(progn
		  (if (not (equal snefru-validation t))
		      (message "Snefru checksum valid."))
		  (bury-buffer snefru-buffer))
	      (beep)
	      (save-excursion
		(set-buffer snefru-buffer)
		(goto-char (point-min))
		(insert (message "Snefru checksum mismatch!")
			"\nPosted:     " checksum
			"Calculated: ")
		(goto-char (point-min))))
	    )))))

(defun snefru-checksum-region (start end)
  "Calculates Snefru checksum."

  ;; Get buffer and clear it
  (setq snefru-buffer (get-buffer-create "*Snefru Buffer*"))
  (save-excursion
    (set-buffer snefru-buffer)
    (erase-buffer))

  ;; Run snefru
  (call-process-region start end
		       snefru-command nil snefru-buffer nil)

  ;; Verify normal result
  (save-excursion
    (set-buffer snefru-buffer)
    (if (= (buffer-size) 36)
	(buffer-substring (point-min) (1- (point-max)))
      (error "Unexpected result from %s: %s" snefru-command
	     (buffer-substring (point-min) (point-max))))))
