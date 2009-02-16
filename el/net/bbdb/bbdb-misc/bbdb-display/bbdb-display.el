;;; bbdb-display.el -- Mouse sensitivity & menus for Lucid and FSF Emacs.

;; This file is an extension to the Insidious Big Brother Database (aka BBDB),
;; Copyright (c) 1994 Boris Goldowsky <boris@cs.rochester.edu>
;; Derived from bbdb-lucid.el, (c) 1992 Jamie Zawinski <jwz@lucid.com>.
;; Thanks also to Frederic Devernay for fixes and refinements.
;; Last change 22-Sep-94.

;; This is free software; you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 1, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This code is kind of kludgey, mostly because it needs to parse the contents
;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;; various fields when it fills in that buffer (doing that would be slow and
;; cons a lot, so it doesn't seem to be worth it.)

;;; Installation:
;; 
;; Include the following in your .emacs file.  This should work with both
;; GNU Emacs and XEmacs.
;;
;; (add-hook 'bbdb-load-hook
;;   '(lambda () (if window-system (require 'bbdb-display))))
;;
;; Or, to delay loading this file until it is used:
;;
;; (add-hook 'bbdb-load-hook
;;   '(lambda ()
;;	 (if window-system
;;	     (progn
;;		(autoload 'bbdb-maybe-fontify-buffer "bbdb-display" "")
;;		(autoload 'bbdb-fontify-buffer "bbdb-display" "")
;;		(autoload 'bbdb-menu "bbdb-display" "")
;;		(add-hook 'bbdb-list-hook 'bbdb-maybe-fontify-buffer)
;;		(define-key bbdb-mode-map "F" 'bbdb-fontify-buffer)
;;		(if (string-match "Lucid" emacs-version)
;;	            (define-key bbdb-mode-map 'button3 'bbdb-menu)
;;		  (define-key bbdb-mode-map [down-mouse-3] 'bbdb-menu))))))

(require 'bbdb)
(require 'bbdb-com)

;; Version-specific set up:
(cond ((string-match "Lucid" emacs-version)
       (require 'bbdb-display-lucid))
      ((string-match "\\(19\\|20\\|21\\)\\." emacs-version)
       (require 'bbdb-display-fsf))
      (t (error "Sorry, this version of emacs is not supported.")))
  
(defvar bbdb-fontify-max 25
  ;; Functionality enhancement by Bng
  "*Don't auto-fontify BBDB buffer if showing more than this many records.")

;;;###autoload
(add-hook 'bbdb-list-hook 'bbdb-maybe-fontify-buffer)

;;;###autoload
(define-key bbdb-mode-map "F" 'bbdb-fontify-buffer)

;;;###autoload
(defun bbdb-maybe-fontify-buffer ()
  "Add distinctive faces to BBDB buffer if it is not too long.
Does nothing if there are more than `bbdb-fontify-max' records showing."
  (if (<= (length bbdb-records) bbdb-fontify-max)
      (bbdb-fontify-buffer)))

;;;###autoload
(defun bbdb-fontify-buffer ()
  "Add distinctive faces to BBDB buffer.
You can modify the faces called `bbdb-name', `bbdb-company',
`bbdb-field-name', and `bbdb-field-value' to change the appearance of
the output."
  (interactive)
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (bbdb-delete-extents)
    (let ((rest bbdb-records)
	  record start end elided-p p e)
      (while rest
	(setq record (car (car rest))
	      elided-p (eq (nth 1 (car rest)) t)
	      start (marker-position (nth 2 (car rest)))
	      end (1- (or (nth 2 (car (cdr rest))) (point-max))))
	(bbdb-make-extent start end nil 'region)
	(goto-char start)
	(if elided-p
	    (progn
	      (move-to-column 48)
	      (skip-chars-backward " \t"))
	  (end-of-line))
	(setq p (point))
	(goto-char start)
	(if (search-forward " - " p t)
	    (progn
	      (bbdb-make-extent (point) p 'bbdb-company nil)
	      (forward-char -3))
	  (goto-char p))
	(bbdb-make-extent start (point) 'bbdb-name 'highlight)
	(forward-line 1)
	(while (< (point) end)
	  (skip-chars-forward " \t")
	  (setq p (point))
	  (and (looking-at "[^:\n]+:")
	       (progn
		 (bbdb-make-extent p (match-end 0) 'bbdb-field-name nil)))
	  (while (progn (forward-line 1)
			(looking-at "^\\(\t\t \\|                 \\)")))
	  (bbdb-make-extent p (1- (point)) 'bbdb-field-value 'highlight))
	(setq rest (cdr rest))))))


;; modified by Bng in an attempt to make it work.
;; referenced bbdb-finger-records, which doesn't exist.
(defvar global-bbdb-menu-commands
  '(["Save BBDB" bbdb-save-db t]
    ["Elide All Records" (bbdb-elide-all-records-internal nil) t]
    ["Finger All Records" (bbdb-finger (mapcar 'car bbdb-records)) t]
    ["BBDB Manual" bbdb-info t]
    ["BBDB Quit" bbdb-bury-buffer t]
    ))

(defun build-bbdb-finger-menu (record)
  (let ((addrs (bbdb-record-net record)))
    (if (cdr addrs)
	(cons "Finger..."
	      (nconc
	       (mapcar '(lambda (addr)
			  (vector addr (list 'bbdb-finger record addr)
				  t))
		       addrs)
	       (list "----"
		     (vector "Finger all addresses"
			     (list 'bbdb-finger record ''(4)) t))))
      (vector (concat "Finger " (car addrs))
	      (list 'bbdb-finger record (car addrs)) t))))


(defun build-bbdb-sendmail-menu (record)
  (let ((addrs (bbdb-record-net record)))
    (if (cdr addrs)
	(cons "Send Mail..."
	      (mapcar '(lambda (addr)
			 (vector addr (list 'bbdb-send-mail-internal
					    (bbdb-dwim-net-address record addr))
				 t))
		      addrs))
      (vector (concat "Send mail to " (car addrs))
	      (list 'bbdb-send-mail-internal
		    (bbdb-dwim-net-address record (car addrs)))
	      t))))
      

(defun build-bbdb-field-menu (record field)
  (let ((type (car field)))
    (nconc
     (list
      (concat "Commands for "
	      (cond ((eq type 'property)
		     (concat "\""
			     (symbol-name (if (consp (car (cdr field)))
					      (car (car (cdr field)))
					    (car (cdr field))))
			     "\" field:"))
		    ((eq type 'name) "Name field:")
		    ((eq type 'company) "Company field:")
		    ((eq type 'net) "Network Addresses field:")
		    ((eq type 'aka) "Alternate Names field:")
		    (t
		     (concat "\"" (aref (nth 1 field) 0) "\" "
			     (capitalize (symbol-name type)) " field:"))))
      "-----"
      ["Edit Field" bbdb-edit-current-field t]
      )
     (if (memq type '(name company))
	 nil
       (list ["Delete Field" bbdb-delete-current-field-or-record t]))
     (cond ((eq type 'phone)
	    (list (vector (concat "Dial " (bbdb-phone-string (car (cdr field))))
			  (list 'bbdb-dial (list 'quote field) nil) t)))
	   )
     )))


(defun build-bbdb-insert-field-menu (record)
  (cons "Insert New Field..."
	(mapcar
	 '(lambda (field)
 	    (let ((type (if (string= (car field) "AKA")
			    'aka
			  (intern (car field)))))
	      (vector (car field)
		      (list 'bbdb-insert-new-field (list 'quote type)
			    (list 'bbdb-prompt-for-new-field-value
				  (list 'quote type)))
		      (not
		       (or (and (eq type 'net) (bbdb-record-net record))
			   (and (eq type 'aka) (bbdb-record-aka record))
			   (and (eq type 'notes) (bbdb-record-notes record))
			   (and (consp (bbdb-record-raw-notes record))
				(assq type (bbdb-record-raw-notes record))))))))
	 (append '(("phone") ("address") ("net") ("AKA") ("notes"))
		 (bbdb-propnames)))))


(defun build-bbdb-menu (record field)
  (append
   '("bbdb-menu" "Global BBDB Commands" "-----")
   global-bbdb-menu-commands
   (if record
       (list
	"-----"
	(concat "Commands for record \""
		(bbdb-record-name record) "\":")
	"-----"
	(vector "Delete Record"
		(list 'bbdb-delete-current-record record) t)
	(if (nth 1 (assq record bbdb-records))
	    ["Unelide Record" bbdb-elide-record t]
	  ["Elide Record" bbdb-elide-record t])
	["Omit Record" bbdb-omit-record t]
	["Refile (Merge) Record" bbdb-refile-record t]
	))
   (if record
       (list (build-bbdb-finger-menu record)))
   (if (and record (bbdb-record-net record))
       (list (build-bbdb-sendmail-menu record)))
   (if record
       (list (build-bbdb-insert-field-menu record)))
   (if field
       (cons "-----" (build-bbdb-field-menu record field)))
   ))

;;;###autoload
(defun bbdb-menu (e)
  (interactive "e")
  (require 'bbdb-com)
  (mouse-set-point e)
  (beginning-of-line)
  (popup-menu
   (save-window-excursion
     (save-excursion
       (mouse-set-point e)
       (let ((extent (bbdb-extent-at (point) (current-buffer) 'bbdb))
	     record field face)
	 (if (null extent)
	     nil
	   (goto-char (bbdb-extent-start-position extent))
	   (beginning-of-line)
	   (setq record (bbdb-current-record)
		 face (bbdb-extent-face extent)
		 field (cond ((memq face
				    '(bbdb-name bbdb-field-value
						bbdb-field-name))
			      (bbdb-current-field))
			     ((eq face 'bbdb-company)
			      (cons 'company (cdr (bbdb-current-field))))
			     (t nil))))
	 (build-bbdb-menu record field))))))

(provide 'bbdb-display)
