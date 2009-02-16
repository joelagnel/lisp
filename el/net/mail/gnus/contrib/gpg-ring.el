;;; gpg-ring.el --- Major mode for editing GnuPG key rings.

;; Copyright (C) 2000 RUS-CERT, University Of Stuttgart

;; Author: Florian Weimer <Florian.Weimer@RUS.Uni-Stuttgart.DE>
;; Maintainer: Florian Weimer <Florian.Weimer@RUS.Uni-Stuttgart.DE>
;; Keywords: crypto
;; Created: 2000-04-28

;; This file is NOT (yet?) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



;;; Code:

(require 'gpg)
(eval-when-compile (require 'cl))

;;;; Customization:

;;; Customization: Groups:

(defgroup gpg-ring nil
  "GNU Privacy Guard user interface."
  :tag "GnuPG user interface"
  :group 'gpg)

;;; Customization: Variables:

(defface gpg-ring-key-invalid-face 
  '((((class color))
     (:foreground "yellow" :background "red"))
    (t (:bold t :italic t :underline t)))
  "Face for strings indicating key invalidity."
  :group 'gpg-ring)

(defface gpg-ring-uncertain-validity-face
  '((((class color)) (:foreground "red"))
    (t (:bold t)))
  "Face for strings indicating uncertain validity."
  :group 'gpg-ring)

(defface gpg-ring-full-validity-face
  '((((class color)) (:foreground "ForestGreen" :bold t))
    (t (:bold t)))
  "Face for strings indicating key invalidity."
  :group 'gpg-ring)

(defvar gpg-ring-mode-hook nil
  "Normal hook run when entering GnuPG ring mode.")

;;; Constants

(defconst gpg-ring-algo-alist
  '((rsa . "RSA")
    (rsa-encrypt-only . "RSA-E")
    (rsa-sign-only . "RSA-S")
    (elgamal-encrypt-only . "ELG-E")
    (dsa . "DSA")
    (elgamal . "ELG-E"))
  "Alist mapping algorithm IDs to algorithm abbreviations.")
    
(defconst gpg-ring-trust-alist
  '((not-known       "???" gpg-ring-uncertain-validity-face)
    (disabled        "DIS" gpg-ring-key-invalid-face)
    (revoked         "REV" gpg-ring-key-invalid-face)
    (expired         "EXP" gpg-ring-key-invalid-face)
    (trust-undefined "QES" gpg-ring-uncertain-validity-face)
    (trust-none      "NON" gpg-ring-uncertain-validity-face)
    (trust-marginal  "MAR")
    (trust-full      "FUL" gpg-ring-full-validity-face)
    (trust-ultimate  "ULT" gpg-ring-full-validity-face))
  "Alist mapping trust IDs to trust abbrevs and faces.")

(defvar gpg-ring-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    map)
  "Keymap for `gpg-ring-mode'.")

(define-key gpg-ring-mode-map "0" 'delete-window)
(define-key gpg-ring-mode-map "1" 'delete-other-windows)
(define-key gpg-ring-mode-map "M" 'gpg-ring-mark-process-all)
(define-key gpg-ring-mode-map "U" 'gpg-ring-unmark-all)
(define-key gpg-ring-mode-map "a" 'gpg-ring-toggle-show-unusable)
(define-key gpg-ring-mode-map "d" 'gpg-ring-mark-delete)
(define-key gpg-ring-mode-map "f" 'gpg-ring-update-key)
(define-key gpg-ring-mode-map "g" 'gpg-ring-update)
(define-key gpg-ring-mode-map "i" 'gpg-ring-show-key)
(define-key gpg-ring-mode-map "l" 'gpg-ring-toggle-show-all-ids)
(define-key gpg-ring-mode-map "m" 'gpg-ring-mark-process)
(define-key gpg-ring-mode-map "n" 'gpg-ring-next-record)
(define-key gpg-ring-mode-map "p" 'gpg-ring-previous-record)
(define-key gpg-ring-mode-map "q" 'gpg-ring-quit)
(define-key gpg-ring-mode-map "u" 'gpg-ring-unmark)
(define-key gpg-ring-mode-map "x" 'gpg-ring-extract-keys)
(define-key gpg-ring-mode-map "X" 'gpg-ring-extract-keys-to-kill)

(define-key gpg-ring-mode-map "\C-c\C-c" 'gpg-ring-action)

;;; Internal functions:

(defvar gpg-ring-key-list
  nil
  "List of keys in the key list buffer.")
(make-variable-buffer-local 'gpg-ring-key-list)

(defvar gpg-ring-update-funcs
  nil
  "List of functions called to obtain the key list.")
(make-variable-buffer-local 'gpg-ring-update-funcs)

(defvar gpg-ring-show-unusable
  nil
  "If t, show expired, revoked and disabled keys, too.")
(make-variable-buffer-local 'gpg-ring-show-unusable)

(defvar gpg-ring-show-all-ids
  nil
  "If t, show all user IDs.  If nil, show only the primary user ID.")
(make-variable-buffer-local 'gpg-ring-show-all-ids)

(defvar gpg-ring-marks-alist
  nil
  "Alist of (UNIQUE-ID MARK KEY).
UNIQUE-ID is a unique key ID from GnuPG.  MARK is either `?D'
(marked for deletion), or `?*' (marked for processing).")
(make-variable-buffer-local 'gpg-ring-marks-alist)

(defvar gpg-ring-action
  nil
  "Function to call when `gpg-ring-action' is invoked.
A list of the keys which are marked for processing is passed as argument.")
(make-variable-buffer-local 'gpg-ring-action)

(defun gpg-ring-mode ()
  "Mode for editing GnuPG key rings.
\\{gpg-ring-mode-map}
Turning on gpg-ring-mode runs `gpg-ring-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map gpg-ring-mode-map)
  (setq mode-name "Key Ring")
  (setq major-mode 'gpg-ring-mode)
  (run-hooks 'gpg-ring-mode-hook))


(defmacro gpg-ring-record-start (&optional pos)
  "Return buffer position of start of record containing POS."
  `(get-text-property (or ,pos (point)) 'gpg-record-start))
					 
(defun gpg-ring-current-key (&optional pos)
  "Return GnuPG key at POS, or at point if ommitted."
  (or (get-text-property (or pos (point)) 'gpg-key)
      (error "No record on current line")))

(defun gpg-ring-goto-record (pos)
  "Go to record starting at POS.
Position point after the marks at the beginning of a record."
  (goto-char pos)
  (forward-char 2))

(defun gpg-ring-next-record ()
  "Advances point to the start of the next record."
  (interactive)
  (let ((start (next-single-property-change 
		(point) 'gpg-record-start nil (point-max))))
    ;; Don't advance to the last line of the buffer.
    (when (/= start (point-max))
	(gpg-ring-goto-record start))))

(defun gpg-ring-previous-record ()
  "Advances point to the start of the previous record."
  (interactive)
  ;; The last line of the buffer doesn't contain a record.
  (let ((start (gpg-ring-record-start)))
    (if start
	(gpg-ring-goto-record (previous-single-property-change 
				    start 'gpg-record-start nil (point-min)))
      (gpg-ring-goto-record
       (gpg-ring-record-start (1- (point-max)))))))
      
(defun gpg-ring-set-mark (&optional pos mark)
  "Set MARK on record at POS, or at point if POS is omitted.
If MARK is omitted, clear it."
  (save-excursion
    (let* ((start (gpg-ring-record-start pos))
	   (key (gpg-ring-current-key start))
	   (id (gpg-key-unique-id key))
	   (entry (assoc id gpg-ring-marks-alist))
	   buffer-read-only)
      (goto-char start)
      ;; Replace the mark character.
      (subst-char-in-region (point) (1+ (point)) (char-after) 
			    (or mark ? ))
      ;; Store the mark in alist.
      (if entry
	  (setcdr entry (if mark (list mark key)))
	(when mark
	  (push (list id mark key) gpg-ring-marks-alist))))))

(defun gpg-ring-marked-keys (&optional only-marked mark)
  "Return list of key specs which have MARK.
If no marks are present and ONLY-MARKED is not nil, return singleton
list with key of the current record.  If MARK is omitted, `?*' is
used."
  (let ((the-marker (or mark ?*))
	(marks gpg-ring-marks-alist)
	key-list)
    (while marks
      (let ((mark (pop marks)))
	;; If this entry has got the right mark ...
	(when (equal (nth 1 mark) the-marker)
	  ;; ... rember the key spec.
	  (push (nth 2 mark) key-list))))
    (or key-list (if (not only-marked) (list (gpg-ring-current-key))))))

(defun gpg-ring-mark-process ()
  "Mark record at point for processing."
  (interactive)
  (gpg-ring-set-mark nil ?*)
  (gpg-ring-next-record))

(defun gpg-ring-mark-delete ()
  "Mark record at point for processing."
  (interactive)
  (gpg-ring-set-mark nil ?D)
  (gpg-ring-next-record))

(defun gpg-ring-unmark ()
  "Mark record at point for processing."
  (interactive)
  (gpg-ring-set-mark)
  (gpg-ring-next-record))

(defun gpg-ring-mark-process-all ()
  "Put process mark on all records."
  (interactive)
  (setq gpg-ring-marks-alist 
	(mapcar (lambda (key)
		  (list (gpg-key-unique-id key) ?* key))
		gpg-ring-key-list))
  (gpg-ring-regenerate))

(defun gpg-ring-unmark-all ()
  "Remove all record marks."
  (interactive)
  (setq gpg-ring-marks-alist nil)
  (gpg-ring-regenerate))

(defun gpg-ring-toggle-show-unusable ()
  "Toggle value if `gpg-ring-show-unusable'."
  (interactive)
  (setq gpg-ring-show-unusable (not gpg-ring-show-unusable))
  (gpg-ring-regenerate))
  
(defun gpg-ring-toggle-show-all-ids ()
  "Toggle value of `gpg-ring-show-all-ids'."
  (interactive)
  (setq gpg-ring-show-all-ids (not gpg-ring-show-all-ids))
  (gpg-ring-regenerate))

(defvar gpg-ring-output-buffer-name "*GnuPG Output*"
  "Name buffer to which output from GnuPG is sent.")

(defmacro gpg-ring-with-output-buffer (&rest body)
  "Erase GnuPG output buffer, evaluate BODY in it, and display it."
  `(with-current-buffer (get-buffer-create gpg-ring-output-buffer-name)
     (erase-buffer)
     (setq truncate-lines t)
     ,@body
     (goto-char (point-min))
     (display-buffer gpg-ring-output-buffer-name)))

(defun gpg-ring-quit ()
  "Bury key list buffer and kill GnuPG output buffer."
  (interactive)
  (let ((output (get-buffer gpg-ring-output-buffer-name)))
    (when output
      (kill-buffer output)))
  (when (eq 'gpg-ring-mode major-mode)
    (bury-buffer)))

(defun gpg-ring-show-key ()
  "Show information for current key."
  (interactive)
  (let ((keys (gpg-ring-marked-keys)))
    (gpg-ring-with-output-buffer
     (gpg-key-insert-information (gpg-key-unique-id-list keys)))))

(defun gpg-ring-extract-keys ()
  "Export currently selected public keys in ASCII armor."
  (interactive)
  (let ((keys (gpg-ring-marked-keys)))
    (gpg-ring-with-output-buffer
     (gpg-key-insert-public-key (gpg-key-unique-id-list keys)))))

(defun gpg-ring-extract-keys-to-kill ()
  "Export currently selected public keys in ASCII armor to kill ring."
  (interactive)
  (let ((keys (gpg-ring-marked-keys)))
    (with-temp-buffer
      (gpg-key-insert-public-key (gpg-key-unique-id-list keys))
      (copy-region-as-kill (point-min) (point-max)))))

(defun gpg-ring-update-key ()
  "Fetch key information from key server."
  (interactive)
  (let ((keys (gpg-ring-marked-keys)))
    (gpg-ring-with-output-buffer
     (gpg-key-retrieve (gpg-key-unique-id-list keys)))))

(defun gpg-ring-insert-key-stat (key)
  (let* ((validity (gpg-key-validity key))
	 (validity-entry (assq validity gpg-ring-trust-alist))
	 (trust (gpg-key-trust key))
	 (trust-entry (assq trust gpg-ring-trust-alist)))
    ;; Insert abbrev for key status.
    (let ((start (point)))
      (insert (nth 1 validity-entry))
      ;; Change face if necessary.
      (when (nth 2 validity-entry)
	(add-text-properties start (point) 
			     (list 'face (nth 2 validity-entry)))))
    ;; Trust, key ID, length, algorithm, creation date.
    (insert (format "/%s %-8s/%4d/%-5s created %s"
		    (nth 1 trust-entry)
		    (gpg-short-key-id key)
		    (gpg-key-length key) 
		    (cdr (assq (gpg-key-algorithm key) gpg-ring-algo-alist))
		    (gpg-key-creation-date key)))
    ;; Expire date.
    (when (gpg-key-expire-date key)
      (insert ", ")
      (let ((start (point))
	    (expired (eq 'expired validity))
	    (notice (concat )))
	(insert (if expired "EXPIRED" "expires")
		" " (gpg-key-expire-date key))
	(when expired
	  (add-text-properties start (point) 
			       '(face gpg-ring-key-invalid-face)))))))

(defun gpg-ring-insert-key (key &optional mark)
  "Inserts description for KEY into current buffer before point."
  (let ((start (point)))
    (insert (if mark mark " ")
            " " (gpg-key-primary-user-id key) "\n"
	    "    ")
    (gpg-ring-insert-key-stat key)
    (insert "\n")
    (when gpg-ring-show-all-ids
      (let ((uids (gpg-key-user-ids key)))
	(while uids
	  (insert "     ID " (pop uids) "\n"))))
    (add-text-properties start (point)
			 (list 'gpg-record-start start
			       'gpg-key key))))

(defun gpg-ring-regenerate ()
  "Regenerate the key list buffer from stored data."
  (interactive)
  (let* ((key-list gpg-ring-key-list)
	 ;; Record position of point.
	 (old-record (if (eobp)		; No record on last line.
			 nil 
		       (gpg-key-unique-id (gpg-ring-current-key))))
	 (old-pos (if old-record (- (point) (gpg-ring-record-start))))
	 found new-pos new-pos-offset buffer-read-only new-marks)
    ;; Replace buffer contents with new data.
    (erase-buffer)
    (while key-list
      (let* ((key (pop key-list))
	     (id (gpg-key-unique-id key))
	     (mark (assoc id gpg-ring-marks-alist)))
	(when (or gpg-ring-show-unusable
		  (not (memq (gpg-key-validity key) 
			     '(disabled revoked expired))))
	  ;; Check if point was in this record.
	  (when (and old-record 
		     (string-equal old-record id))
	    (setq new-pos (point))
	    (setq new-pos-offset (+ new-pos old-pos)))
	  ;; Check if this record was marked.
	  (if (nth 1 mark)
	      (progn
		(push mark new-marks)
		(gpg-ring-insert-key key (nth 1 mark)))
	    (gpg-ring-insert-key key)))))
    ;; Replace mark alist with the new one (which does not contain
    ;; marks for records which vanished during this update).
    (setq gpg-ring-marks-alist new-marks)
    ;; Restore point.
    (if (not old-record)
	;; We were at the end of the buffer before.
	(goto-char (point-max))
      (if new-pos
	  (if (and (< new-pos-offset (point-max))
		   (equal old-record (gpg-key-unique-id 
				      (gpg-ring-current-key new-pos-offset))))
	      ;; Record is there, with offset.
	      (goto-char new-pos-offset)
	    ;; Record is there, but not offset.
	    (goto-char new-pos))
	;; Record is not there.
	(goto-char (point-min))))))

(defun gpg-ring-update ()
  "Update the key list buffer with new data."
  (interactive)
  (let ((funcs gpg-ring-update-funcs)
	old)
    ;; Merge the sorted lists obtained by calling elements of
    ;; `gpg-ring-update-funcs'.
    (while funcs 
      (let ((additional (funcall (pop funcs)))
	    new)
	(while (and additional old)
	  (if (gpg-key-lessp (car additional) (car old))
	      (push (pop additional) new)
	    (if (gpg-key-lessp (car old) (car additional))
		(push (pop old) new)
	      ;; Keys are perhaps equal.  Always Add old key.
	      (push (pop old) new)
	      ;; If new key is equal, drop it, otherwise add it as well.
	      (if (string-equal (gpg-key-unique-id (car old))
				(gpg-key-unique-id (car additional)))
		  (pop additional)
		(push (pop additional) new)))))
	;; Store new list as old one for next round.
	(setq old (nconc (nreverse new) old additional))))
    ;; Store the list in the buffer.
    (setq gpg-ring-key-list old))
  (gpg-ring-regenerate))

(defun gpg-ring-action ()
  "Perform the action associated with this buffer."
  (interactive)
  (if gpg-ring-action
      (funcall gpg-ring-action (gpg-ring-marked-keys))
    (error "No action for this buffer specified")))
     
;;;###autoload
(defun gpg-ring-keys (&optional key-list-funcs action)
  (interactive)
  (let ((buffer (get-buffer-create "*GnuPG Key List*")))
    (with-current-buffer buffer
      (gpg-ring-mode)
      (setq gpg-ring-action action)
      (setq gpg-ring-update-funcs key-list-funcs key-list-funcs)
      (gpg-ring-update)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;;###autoload
(defun gpg-ring-public (key-spec)
  "List public keys matching keys KEY-SPEC."
  (interactive "sList public keys containing: ")
  (gpg-ring-keys  `((lambda () (gpg-key-list-keys ,key-spec)))))

(provide 'gpg-ring)

;;; arch-tag: a4c5b2d1-aff0-4ab6-96e9-267727226c2d
;;; gpg-ring.el ends here
