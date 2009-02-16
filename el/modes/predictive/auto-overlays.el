
;;; auto-overlays.el --- automatic regexp-delimited overlays for emacs


;; Copyright (C) 2005 2006 2007 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.7.2
;; Keywords: automatic, overlays
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Automatic Overlays package.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.7.2
;; * added md5 sum check for regexps to `auto-overlay-load-overlays', to
;;   ensure regexp definitions haven't changed since overlays were saved
;;
;; Version 0.7.1
;; * fixed sharing of regexp sets
;;
;; Version 0.7
;; * fixed bugs in `auto-o-update-exclusive' that caused it to fail if called
;;   during a suicide when parentless overlays can exist, and that caused it
;;   to infinitely recurse if an exclusive overlay partially overlapped with
;;   its match overlay (thanks to Aemon Cannon for pointing this out)
;; * removed `auto-overlay-functions' variable, and implemented new interface
;;   based on symbol properties
;;
;; Version 0.6.1
;; * fixed minor bug in `auto-overlay-save-overlays'
;;
;; Version 0.6
;; * rationalised terminology: type -> entry, sequence -> subentry/rank
;; * got rid of mostly useless `auto-overlay-list'
;; * regexp entries and corresponding overlays are now identified by a unique
;;   ID instead of simply their position in the `auto-overlay-regexps' list
;; * added functions for loading and unloading regexps on the fly, possible
;;   with the new ID identifiers
;; * finally added functions for saving and loading overlays to a file (though
;;   the small change to `auto-o-self-list' in auto-overlay-self.el makes a
;;   bigger difference to the load time)
;;
;; Version 0.5
;; * changed the way suicide, update and other functions are called after a
;;   buffer modification: now called from `auto-o-run-after-change-functions'
;;
;; Version 0.4.2
;; * moved compatability code to auto-overlays-compat.el
;;
;; Version 0.4.1
;; * moved defmacros before their first use so byte-compilation works
;;
;; Version 0.4
;; * (a lot of) bug fixes
;;
;; Version 0.3
;; * completely re-written after realising that the match overlays, not the
;;   auto overlays themselves, should be the "primary" objects - much better!
;; * moved code for specific overlay types into separate files
;; * as a side effect, created a mechanism for defining new overlay types
;;   without modifying the auto-overlay code itself
;;
;; Version 0.2:
;; * added exclusive overlay support
;; * major code tidying and bug fixes
;;
;; Version 0.1
;; * initial version created by copying code from Predictive Completion
;;   package, with minor modifications



;;; Code:


(defvar auto-overlay-regexps nil)
(make-variable-buffer-local 'auto-overlay-regexps)
(defvar auto-overlay-load-hook nil)
(defvar auto-overlay-unload-hook nil)


(require 'auto-overlay-common)
(provide 'auto-overlays)


;; (defvar auto-overlay-list nil)
;; (make-variable-buffer-local 'auto-overlay-list)
(defvar auto-o-pending-updates nil)
(make-variable-buffer-local 'auto-o-pending-updates)
(defvar auto-o-pending-suicides nil)
(make-variable-buffer-local 'auto-o-pending-suicides)
(defvar auto-o-pending-pre-suicide nil)
(make-variable-buffer-local 'auto-o-pending-pre-suicide)
(defvar auto-o-pending-post-suicide nil)
(make-variable-buffer-local 'auto-o-pending-post-suicide)
(defvar auto-o-pending-post-update nil)
(make-variable-buffer-local 'auto-o-pending-post-update)




;;;========================================================
;;;                 Code-tidying macros

(defmacro auto-o-enabled-p (set-id)
  ;; Return non-nil if regexp set identified by SET-ID is enabled.
  `(let ((set (assq ,set-id auto-overlay-regexps)))
     (and set (cadr set))))


(defmacro auto-o-get-buffer-list (set-id)
  ;; Return the list of buffers that use the regexp set SET-ID.
  `(let ((set (assq ,set-id auto-overlay-regexps)))
     (and set (caddr set))))


(defmacro auto-o-set-buffer-list (set-id list)
  ;; Set the list of buffers that use the regexp set SET-ID to LIST.
  `(let ((set (assq ,set-id auto-overlay-regexps)))
     (and set (setcar (cddr set) ,list))))


(defmacro auto-o-add-to-buffer-list (set-id buffer)
  ;; Add BUFFER to the list of buffers using regexp set SET-ID.
  `(let ((set (assq ,set-id auto-overlay-regexps)))
     (and set
	  (null (assq ,buffer (caddr set)))
	  (setcar (cddr set) (cons ,buffer (caddr set))))))


(defmacro auto-o-delete-from-buffer-list (set-id buffer)
  ;; Remove BUFFER from the list of buffers using regexp set SET-ID.
  `(let ((set (assq ,set-id auto-overlay-regexps)))
     (and set (setcar (cddr set) (delq ,buffer (caddr set))))))


(defmacro auto-o-enable-set (set-id)
  ;; Set enabled flag for regexp set identified by SET-ID.
  `(setcar (cdr (assq ,set-id auto-overlay-regexps)) t))


(defmacro auto-o-disable-set (set-id)
  ;; Unset enabled flag for regexp set identified by SET-ID.
  `(setcar (cdr (assq ,set-id auto-overlay-regexps)) nil))


(defmacro auto-o-entry (set-id entry-id &optional subentry-id)
  ;; Return regexp entry identified by SET-ID, ENTRY-ID and SUBENTRY-ID id's.
  `(if ,subentry-id
       (cdr (assq ,subentry-id
		  (cdr (assq ,entry-id
			     (cddr (assq ,set-id auto-overlay-regexps))))))
     (cdr (assq ,entry-id (cddr (assq ,set-id auto-overlay-regexps))))))


(defmacro auto-o-class (o-match)
  ;; Return class of match overlay O-MATCH.
  `(cadr (assq (overlay-get ,o-match 'entry-id)
	      (cddr (assq (overlay-get ,o-match 'set-id)
			  auto-overlay-regexps)))))


(defmacro auto-o-entry-regexp (set-id entry-id &optional subentry-id)
  ;; Return regexp corresponsing to SET-ID, ENTRY-ID and SUBENTRY-ID.
  `(let ((regexp (nth 1 (auto-o-entry ,set-id ,entry-id ,subentry-id))))
     (if (atom regexp) regexp (car regexp))))


(defmacro auto-o-regexp (o-match)
  ;; Return match overlay O-MATCH's regexp.
  `(auto-o-entry-regexp (overlay-get ,o-match 'set-id)
		      (overlay-get ,o-match 'entry-id)
		      (overlay-get ,o-match 'subentry-id)))


(defmacro auto-o-entry-regexp-group (set-id entry-id &optional subentry-id)
  ;; Return regexp group corresponsing to SET-ID, ENTRY-ID and SUBENTRY-ID, or
  ;; 0 if none is specified.
  `(let ((regexp (nth 1 (auto-o-entry ,set-id ,entry-id ,subentry-id))))
     (cond
      ((atom regexp) 0)
      ((atom (cdr regexp)) (cdr regexp))
      (t (cadr regexp)))))


(defmacro auto-o-regexp-group (o-match)
  ;; Return match overlay O-MATCH's regexp group.
  `(auto-o-entry-regexp-group (overlay-get ,o-match 'set-id)
			    (overlay-get ,o-match 'entry-id)
			    (overlay-get ,o-match 'subentry-id)))


(defmacro auto-o-entry-regexp-group-nth (n set-id entry-id
					   &optional subentry-id)
  ;; Return Nth regexp group entry corresponsing to SET-ID, ENTRY-ID and
  ;; SUBENTRY-ID, or 0 if there is no Nth entry.
  `(let ((regexp (nth 1 (auto-o-entry ,set-id ,entry-id ,subentry-id))))
     (cond
      ((atom regexp) 0)
      ((> (1+ ,n) (length (cdr regexp))) 0)
      (t (nth ,n (cdr regexp))))))


(defmacro auto-o-regexp-group-nth (n o-match)
  ;; Return match overlay O-MATCH's Nth regexp group entry, or 0 if there is
  ;; no Nth entry.
  `(auto-o-entry-regexp-group-nth ,n
				(overlay-get ,o-match 'set-id)
				(overlay-get ,o-match 'entry-id)
				(overlay-get ,o-match 'subentry-id)))


(defmacro auto-o-entry-props (set-id entry-id &optional subentry-id)
  ;; Return properties of regexp corresponding to SET-ID, ENTRY-ID and
  ;; SUBENTRY-ID.
  `(if (auto-o-entry-compound-class-p ,set-id ,entry-id)
       (nthcdr 2 (auto-o-entry ,set-id ,entry-id ,subentry-id))
     (nthcdr 2 (auto-o-entry ,set-id ,entry-id))))


(defmacro auto-o-props (o-match)
  ;; Return properties associated with match overlay O-MATCH.
  `(auto-o-entry-props (overlay-get ,o-match 'set-id)
		      (overlay-get ,o-match 'entry-id)
		      (overlay-get ,o-match 'subentry-id)))


(defmacro auto-o-entry-edge (set-id entry-id subentry-id)
  ;; Return edge ('start or 'end) of regexp with SET-ID, ENTRY-ID and
  ;; SUBENTRY-ID (assumes that entry has a compound class).
  `(car (auto-o-entry ,set-id ,entry-id ,subentry-id)))


(defmacro auto-o-edge (o-match)
  ;; Return edge ('start or 'end) of match overlay O-MATCH (assumes that
  ;; O-MATCH's class is a compound class).
  `(auto-o-entry-edge (overlay-get ,o-match 'set-id)
		      (overlay-get ,o-match 'entry-id)
		      (overlay-get ,o-match 'subentry-id)))


(defmacro auto-o-parse-function (o-match)
  ;; Return appropriate parse function for match overlay O-MATCH.
  `(get (auto-o-class ,o-match) 'auto-overlay-parse-function))


(defmacro auto-o-suicide-function (o-match)
  ;; Return appropriate suicide function for match overlay O-MATCH.
  `(get (auto-o-class ,o-match) 'auto-overlay-suicide-function))


(defmacro auto-o-match-function (o-match)
  ;; Return match function for match overlay O-MATCH, if any.
  `(get (auto-o-class ,o-match) 'auto-overlay-match-function))


(defmacro auto-o-edge-matched-p (overlay edge)
  ;; test if EDGE of OVERLAY is matched
  `(overlay-get ,overlay ,edge))


(defmacro auto-o-start-matched-p (overlay)
  ;; test if OVERLAY is start-matched
  `(overlay-get ,overlay 'start))


(defmacro auto-o-end-matched-p (overlay)
  ;; test if OVERLAY is end-matched
  `(overlay-get ,overlay 'end))


(defmacro auto-o-entry-compound-class-p (set-id entry-id)
  ;; Return non-nil if regexp corresponding to SET-ID and ENTRY-ID contains a
  ;; list of regexp entries rather than a single entry.
  `(let ((entry (cadr (auto-o-entry ,set-id ,entry-id))))
    (and (listp entry)
	 (or (symbolp (cdr entry))
	     (and (listp (cdr entry)) (symbolp (cadr entry)))))))


(defmacro auto-o-compound-rank (o-match)
  ;; Return the rank of match overlay O-MATCH, which should have a compound
  ;; class.
  `(auto-o-position
    (overlay-get ,o-match 'subentry-id)
    (cddr (assq (overlay-get ,o-match 'entry-id)
		(cddr (assq (overlay-get ,o-match 'set-id)
			    auto-overlay-regexps))))))


(defmacro auto-o-overlay-filename (set-id)
  ;; Return the default filename to save overlays in
  `(concat "auto-overlays-"
	   (replace-regexp-in-string
	    "\\." "-" (file-name-nondirectory (or (buffer-file-name)
						  (buffer-name))))
	   "-" (symbol-name ,set-id)))




;;;============================================================
;;;           Replacements for CL functions

(defun auto-o-position (key alist)
  "Find the first association of KEY in ALIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'eq."
  (let (el (i 0))
    (catch 'found
      (while (setq el (nth i alist))
	(when (eq key (car el)) (throw 'found i))
	(setq i (1+ i))
	nil)))
)




;;;=========================================================
;;;             auto-overlay regexp functions

(defun auto-overlay-load-regexp (entry set-id &optional pos entry-id)
  "Load ENTRY into the list of regexps named SET-ID.
If SET-ID does not exist, it is created.

If POS is nil, REGEXP is added at the end of the list. If it is
t, it is added at the beginning. If it is an integer, it is added
at that position.

If ENTRY-ID is supplied, it should be a symbol that can be used
to uniquely identify the ENTRY."

  (let ((regexp-set (assq set-id auto-overlay-regexps)))
    ;; if SET-ID doesn't exist in regexp list, create empty set
    (when (null regexp-set)
      (push (list set-id nil (list (current-buffer))) auto-overlay-regexps)
      (setq regexp-set (car auto-overlay-regexps)))
    ;; if ENTRY-ID is not specified, create a unique numeric ENTRY-ID
    (unless entry-id
      (setq entry-id
	    (1+ (apply 'max -1
		       (mapcar (lambda (elt)
				 (if (integerp (car elt)) (car elt) -1))
			       (cdr regexp-set))))))
    (cond
     ;; adding at end
     ((or (null pos) (and (integerp pos) (>= pos (length (cddr regexp-set)))))
      (if (= (length (cddr regexp-set)) 0)
	  (setcdr (cdr regexp-set) (list (cons entry-id (copy-sequence entry))))
	(nconc (cddr regexp-set) (list (cons entry-id (copy-sequence entry))))))
     ;; adding at start
     ((or (eq pos t) (and (integerp pos) (<= pos 0)))
      (setcdr (cdr regexp-set)
	      (nconc (list (cons entry-id (copy-sequence entry)))
		     (cddr regexp-set))))
     ;; adding at POS
     ((integerp pos)
      (setcdr (nthcdr (1- pos) (cddr regexp-set))
	      (nconc (list (cons entry-id (copy-sequence entry)))
		     (nthcdr pos (cddr regexp-set)))))))
  ;; return new entry ID
  entry-id
)



(defun auto-overlay-load-compound-regexp (entry set-id entry-id
						&optional pos subentry-id)
  "Load ENTRY into the compound regexp entry identified by ENTRY-ID
in the regexp list named SET-ID in the current buffer.

If POS is nil, REGEXP is added at the end of the entry. If it is
t, it is added at the beginning. If it is an integer, it is added
at that position.

If SUBENTRY-ID is supplied, it should be a symbol that can be
used to uniquely identify ENTRY."

  (let ((regexps (assq entry-id (cddr (assq set-id auto-overlay-regexps)))))
    (when (null regexps)
      (error "Compound regexp %s not found in auto-overlay regexp list %s"
	     (symbol-name entry-id) (symbol-name set-id)))
    ;; if ID is not specified, create a unique numeric ID
    (unless subentry-id
      (setq subentry-id
	    (1+ (apply 'max -1
		       (mapcar (lambda (elt)
				 (if (integerp (car elt)) (car elt) -1))
			       (cddr regexps))))))
    (cond
     ;; adding at end
     ((or (null pos) (and (integerp pos) (>= pos (length (cddr regexps)))))
      (if (= (length (cddr regexps)) 0)
	  (setcdr (cdr regexps) (list (cons subentry-id (copy-sequence entry))))
	(nconc (cddr regexps) (list (cons subentry-id (copy-sequence entry))))))
     ;; adding at start
     ((or (eq pos t) (and (integerp pos) (<= pos 0)))
      (setcdr (cdr regexps)
	      (nconc (list (cons subentry-id (copy-sequence entry)))
		     (cddr regexps))))
     ;; adding at POS
     ((integerp pos)
      (setcdr (nthcdr (1- pos) (cddr regexps))
	      (nconc (list (cons subentry-id (copy-sequence entry)))
		     (nthcdr pos (cddr regexps)))))))
  
  ;; return new subentry ID
  subentry-id
)



(defun auto-overlay-unload-regexp (set-id &optional entry-id subentry-id)
  "Unload the regexp entry identified by SET-ID, ENTRY-ID and SUBENTRY-ID
from the current buffer. If only SET-ID and ENTRY-ID are
supplied, delete that entry. If only SET-ID is supplied, delete
that entire set."

  (save-excursion
    ;; delete the regexp entry
    (cond
     
     ;; delete one subentry of a compound entry
     (subentry-id
      ;; call suicide function for corresponding overlays in all buffers in
      ;; which the set is enabled
      (dolist (buff (auto-o-get-buffer-list set-id))
	(set-buffer buff)
	(mapc (lambda (o) (auto-o-suicide o 'force))
	      (auto-overlays-in (point-min) (point-max)
				`((identity auto-overlay-match)
				  (eq set-id ,set-id)
				  (eq entry-id ,entry-id)
				  (eq subentry-id ,subentry-id)))))
      ;; delete regexp entry
      (assq-delete-all subentry-id
		       (cdr (assq entry-id
				  (cddr (assq set-id
					      auto-overlay-regexps))))))
     
     ;; delete one entry
     (entry-id
      ;; call suicide function for corresponding overlays in all buffers in
      ;; which the set is enabled
      (dolist (buff (auto-o-get-buffer-list set-id))
	(set-buffer buff)
	(mapc (lambda (o) (auto-o-suicide o 'force))
	      (auto-overlays-in (point-min) (point-max)
				`((eq set-id ,set-id)
				  (eq entry-id ,entry-id)
				  (eq subentry-id ,subentry-id)))))
      ;; delete regexp entry
      (assq-delete-all entry-id (cddr (assq set-id auto-overlay-regexps))))
   
   ;; delete entire set
   (t
    ;; disable regexp set to delete overlays, reset enabled flag in case
    ;; regexps are shared with other buffers, then delete regexp set from
    ;; current buffer
    (when (auto-o-enabled-p set-id)
      (auto-overlay-stop set-id)
      (auto-o-enable-set set-id))
    (auto-o-delete-from-buffer-list set-id (current-buffer))
    (setq auto-overlay-regexps
	  (assq-delete-all set-id auto-overlay-regexps)))
   ))
  
;;   ;; run any required updates
;;   (auto-o-run-after-change-functions)
)



(defun auto-overlay-share-regexp-set (set-id from-buffer &optional to-buffer)
  "Make TO-BUFFER share the regexp set identified by SET-ID with FROM-BUFFER.
Any changes to that regexp set in either buffer will be reflected in the
other. TO-BUFFER defaults to the current buffer."

  (unless to-buffer (setq to-buffer (current-buffer)))
  (let (regexps)
    ;; get regexp set from FROM-BUFFER
    (save-excursion
      (set-buffer from-buffer)
      (setq regexps (assq set-id auto-overlay-regexps))
      ;; delete any existing set with same ID, and add regexp set to TO-BUFFER
      (set-buffer to-buffer)
      (setq auto-overlay-regexps
	    (assq-delete-all set-id auto-overlay-regexps))
      (push regexps auto-overlay-regexps)
      ;; add TO-BUFFER to list of buffers using regexp set SET-ID
      (auto-o-add-to-buffer-list set-id to-buffer)
      ))
)



(defun auto-overlay-start (set-id &optional buffer ignore-save-file
				  no-regexp-check)
  "Activate the set of auto-overlay regexps identified by SET-ID
in BUFFER, or the current buffer if none is specified.

If optional argument IGNORE-SAVE-FILE is non-nil, it will ignore
any file of saved overlays. If it is null, but optional argument
NO-REGEXP-CHECK is non-nil, the file of saved overlays will be
used, but no check will be made to ensure regexp refinitions are
the same as when the overlays were saved."

  (save-excursion
    (when buffer (set-buffer buffer))
    ;; run initialisation hooks
    (run-hooks 'auto-overlay-load-hook)
    ;; add hook to runs all the various functions scheduled be run after a
    ;; buffer modification
    (add-hook 'after-change-functions 'auto-o-run-after-change-functions
	      nil t)
    ;; add hook to schedule an update after a buffer modification
    (add-hook 'after-change-functions 'auto-o-schedule-update nil t)

    ;; set enabled flag for regexp set, and make sure buffer is in buffer list
    ;; for the regexp set
    (auto-o-enable-set set-id)

    
    ;; search for regexp matches in all buffers in which set is enabled
    (save-excursion
      (dolist (buff (auto-o-get-buffer-list set-id))
	(set-buffer buff)
	
	;; try to load overlays from file
	(unless (and (null ignore-save-file)
		     (file-exists-p (auto-o-overlay-filename set-id))
		     (auto-overlay-load-overlays set-id nil nil
						 no-regexp-check))
	  
	  ;; if loading was unsuccessful, search for new auto overlays
	  (let ((lines (count-lines (point-min) (point-max))))
	    (goto-char (point-min))
	    (message "Scanning for auto-overlays...(line 1 of %d)"
		     lines)
	    (dotimes (i lines)
	      (when (= 9 (mod i 10))
		(message
		 "Scanning for auto-overlays...(line %d of %d)"
		 (+ i 1) lines))
	      (auto-overlay-update nil nil set-id)
	      (forward-line 1))
	    (message "Scanning for auto-overlays...done"))
	  ))
      ))
)



(defun auto-overlay-stop (set-id &optional buffer save leave-overlays)
  "Clear all auto-overlays in the set identified by SET-ID
from BUFFER, or the current buffer if none is specified.

If SAVE is non-nil, save the overlays to a file to speed up
loading if the same set of regexp definitions is enabled
again. If LEAVE-OVERLAYS is non-nil, don't bother deleting the
overlays from the buffer \(this is generally a bad idea, unless
the buffer is about to be killed in which case it speeds things
up a bit\)."

  (save-excursion
    (when buffer (set-buffer buffer))
    ;; disable overlay set
    (auto-o-disable-set set-id)

    ;; delete overlays from all buffers in which set is enabled
    (save-excursion
      (dolist (buff (auto-o-get-buffer-list set-id))
	(set-buffer buff)
	
	;; if SAVE is non-nil, save overlays to a file
	(when save (auto-overlay-save-overlays set-id))
	;; delete overlays unless told not to bother
	(unless leave-overlays
	  (mapc 'delete-overlay
		(auto-overlays-in
		 (point-min) (point-max)
		 (list
		  (list (lambda (overlay match) (or overlay match))
			'(auto-overlay auto-overlay-match))
		  (list 'eq 'set-id set-id))
		 nil 'inactive)))
	
	;; if there are no more active auto-overlay definitions...
	(unless (catch 'enabled
		  (dolist (set auto-overlay-regexps)
		    (when (auto-o-enabled-p (car set))
		      (throw 'enabled t)))
		  nil)
	  ;; run clear hooks
	  (run-hooks 'auto-overlay-unload-hook)
	  ;; reset variables
	  (remove-hook 'after-change-functions 'auto-o-schedule-update t)
	  (remove-hook 'after-change-functions
		       'auto-o-run-after-change-functions t)
	  (setq auto-o-pending-suicides nil
		auto-o-pending-updates nil
		auto-o-pending-post-suicide nil)))
      ))
)



(defun auto-overlay-save-overlays (set-id &optional buffer file)
  "Save overlays in set SET-ID in BUFFER to FILE.
Defaults to the current buffer. If FILE is nil, the filename is
constructed from the buffer name and SET-ID.

They can be loaded again later using `auto-overlay-load-overlays'."

  (save-excursion
    (when buffer (set-buffer buffer))
    
    ;; construct filename if none specified
    (unless file (setq file (auto-o-overlay-filename set-id)))
    
    ;; create temporary buffer
    (let ((buff (generate-new-buffer " *auto-overlay-save*"))
	  overlay-list)
      ;; write md5 digests to first two lines
      (prin1 (md5 (current-buffer)) buff)
      (terpri buff)
      (prin1 (md5 (prin1-to-string (assq set-id auto-overlay-regexps))) buff)
      (terpri buff)
				    
      ;; get sorted list of all match overlays in set SET-ID
      (setq overlay-list
	    (auto-overlays-in (point-min) (point-max)
			      (list '(identity auto-overlay-match)
				    (list 'eq 'set-id set-id))))
      (setq overlay-list
	    (sort overlay-list
		  (lambda (a b) (or (< (overlay-start a) (overlay-start b))
				    (and (= (overlay-start a) (overlay-start b))
					 (> (overlay-end a) (overlay-end b)))))
		  ))
      
      ;; write overlay data to temporary buffer
      (mapc (lambda (o)
	      (prin1 (list (overlay-get o 'entry-id)
			   (overlay-start o)
			   (overlay-end o)
			   (overlay-get o 'subentry-id)
			   (marker-position (overlay-get o 'delim-start))
			   (marker-position (overlay-get o 'delim-end)))
		     buff)
	      (terpri buff))
	    overlay-list)
      
      ;; save the buffer and kill it
      (save-excursion
	(set-buffer buff)
	(write-file file))
      (kill-buffer buff))
    )
)



;; (put 'auto-overlay-md5-mismatch
;;      'error-conditions '(error auto-overlay-md5-mismatch))
;; (put 'auto-overlay-md5-mismatch
;;      'error-message "Buffer has changed since overlays were saved to file")


(defun auto-overlay-load-overlays (set-id &optional buffer file
					  no-regexp-check)
  "Load overlays for BUFFER from FILE.
Defaults to the current buffer. If FILE is not specified,
construct it from buffer name and SET-ID. Returns t if successful, nil
otherwise.

The FILE should be generated by `auto-overlay-save-overlays'. By
default, the buffer contents and regexp definitions cfor SET-ID
will be checked to make sure neither have changed since the
overlays were saved. If they don't match, the saved overlay data
will not be loaded, and the function will return nil.

If NO-REGEXP-CHECK is non-nil, the check for matching regexp
definitions will be skipped; the saved overlays will be loaded
even if different regexp definitions were active when the data
was saved."

  (save-excursion
    (when buffer (set-buffer buffer))
    
    ;; construct filename if none specified
    (unless file (setq file (auto-o-overlay-filename set-id)))
    ;; check FILE exists
    (if (not (file-exists-p file))
	(error "File %s does not exist" file)
      (let ((buff (find-file-noselect file t))
	    md5-buff md5-regexp data o-match o-new lines
	    (i 0))
	
	;; read md5 digests from first two lines of FILE
	(save-excursion
	  (set-buffer buff)
	  (goto-char (point-min)))
	(setq md5-buff (read buff))
	(setq md5-regexp (read buff))
	
	
	;; if saved buffer md5 sum doesn't match buffer contents, or if saved
	;; regexp md5 sum doesn't match regexp definitions and checking is not
	;; overridden, return nil
	(if (not (and (string= md5-buff (md5 (current-buffer)))
		      (or no-regexp-check
			  (string= md5-regexp
				   (md5 (prin1-to-string
					 (assq set-id
					       auto-overlay-regexps)))))))
	    (progn (kill-buffer buff) nil)

	  ;; count number of overlays, for progress message
	  (save-excursion
	    (set-buffer buff)
	    (setq lines (count-lines (point) (point-max))))
	  
	  ;; read overlay data from FILE until we reach the end
	  (message "Loading auto-overlays...(1 of %d)" lines)
	  (while (condition-case nil (setq data (read buff)) ('end-of-file))
	    ;; create a match overlay corresponding to the data
	    (setq o-match (auto-o-make-match
			   set-id (nth 0 data) (nth 1 data) (nth 2 data)
			   (nth 3 data)(nth 4 data) (nth 5 data)))
	    ;; call the appropriate parse function
	    (setq o-new
		  (funcall (auto-o-parse-function o-match) o-match))
	    (unless (listp o-new) (setq o-new (list o-new)))
	    ;; give any new overlays some basic properties
	    (mapc (lambda (o)
		    (overlay-put o 'auto-overlay t)
		    (overlay-put o 'set-id set-id)
		    (overlay-put o 'entry-id (overlay-get o-match 'entry-id)))
		  o-new)
	    ;; run match function if there is one
	    (let ((match-func (auto-o-match-function o-match)))
	      (when match-func (funcall match-func o-match)))
	    ;; display progress message
	    (setq i (1+ i))
	    (when (= 0 (mod i 10))
	      (message "Loading auto-overlays...(%d of %d)" i lines)))
	  
	  (kill-buffer buff)
	  t))))  ; return t to indicate successful loading)
)




;;;=============================================================
;;;               auto-overlay overlay functions

(defun auto-o-run-after-change-functions (&rest unused)
  ;; Assigned to the `after-change-functions' hook. Run all the various
  ;; functions that should run after a change to the buffer, in the correct
  ;; order.

  ;; run pending pre-suicide functions
  (when auto-o-pending-pre-suicide
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-pre-suicide)
    (setq auto-o-pending-pre-suicide nil))  
  ;; run pending suicides
  (when auto-o-pending-suicides
    (mapc 'auto-o-suicide auto-o-pending-suicides)
    (setq auto-o-pending-suicides nil))
  ;; run pending post-suicide functions
  (when auto-o-pending-post-suicide
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-post-suicide)
    (setq auto-o-pending-post-suicide nil))
  ;; run updates
  (when auto-o-pending-updates
    (mapc (lambda (l) (apply 'auto-overlay-update l)) auto-o-pending-updates)
    (setq auto-o-pending-updates nil))
  ;; run pending post-update functions
  (when auto-o-pending-post-update
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-post-update)
    (setq auto-o-pending-post-update nil))
)



(defun auto-o-schedule-update (start &optional end unused set-id)
  ;; Schedule `auto-overlay-update' of lines between positions START and END
  ;; (including lines containing START and END), optionally restricted to
  ;; SET-ID. If END is not supplied, schedule update for just line containing
  ;; START. The update will be run by `auto-o-run-after-change-functions'
  ;; after buffer modification is complete. This function is assigned to
  ;; `after-change-functions'.

  ;; FIXME: we should do more to avoid doing multiple, redundant
  ;;        updates. Currently, only updates for identical regions are
  ;;        filtered, not updates for overlapping regions.
  (add-to-list 'auto-o-pending-updates
	       (list (line-number-at-pos start)
		     (when end (line-number-at-pos end))
		     set-id))
)



(defun auto-o-schedule-suicide (o-self &optional modified &rest unused)
  ;; Schedule `auto-o-suicide' to run after buffer modification is
  ;; complete. It will be run by `auto-o-run-after-change-functions'. Assigned
  ;; to overlay modification and insert in-front/behind hooks.
  (unless modified (add-to-list 'auto-o-pending-suicides o-self))
)



(defun auto-overlay-update (&optional start end set-id)
  ;; Parse lines from line number START to line number END. If only START is
  ;; supplied, just parse that line. If neither are supplied, parse line
  ;; containing the point. If SET-ID is specified, only look for matches in
  ;; that set of overlay regexps definitions.
  
  (let (regexp-list entry-id class regexp group priority set-id subentry-id
		    o-match o-overlap o-new)
    (unless start (setq start (line-number-at-pos)))
    (save-excursion
      (save-match-data
	(goto-line start)
	(dotimes (i (if end (1+ (- end start)) 1))
	  
	  ;; check each enabled set of overlays, or just the specified set
	  (dotimes (s (if set-id 1 (length auto-overlay-regexps)))
	    (setq set-id (or set-id (car (nth s auto-overlay-regexps))))
	    (when (auto-o-enabled-p set-id)
	      ;; check each regexp entry in regexp set
	      (dotimes (tp (length (cddr (nth s auto-overlay-regexps))))
		(setq regexp-list (nth tp (cddr (nth s auto-overlay-regexps))))
		(setq entry-id (car regexp-list))
		(setq class (nth 1 regexp-list))
		(setq regexp-list (cdr regexp-list)) ; remove entry-id
		(if (auto-o-entry-compound-class-p set-id entry-id)
		    (pop regexp-list)		       ; remove class
		  (setq regexp-list (list regexp-list))) ; bundle in list
	      
		;; check all regexps for current entry if it has a compound
		;; class
		(dotimes (rank (length regexp-list))
		  (if (> (length regexp-list) 1)
		      (setq subentry-id (car (nth rank regexp-list)))
		    (setq subentry-id nil))
		  
		  ;; extract regexp properties from current entry
		  (setq regexp (auto-o-entry-regexp set-id entry-id
						    subentry-id))
		  (setq group (auto-o-entry-regexp-group
			       set-id entry-id subentry-id))
		  (setq priority
			(cdr (assq 'priority
				   (auto-o-entry-props
				    set-id entry-id subentry-id))))
		  
		  
		  ;; look for matches in current line, ensuring case *is*
		  ;; significant
		  (forward-line 0)
		  (while (let ((case-fold-search nil))
			   (re-search-forward regexp (line-end-position) t))
		    (cond
		     ;; ignore match if it already has a match overlay
		     ((auto-o-matched-p (match-beginning 0) (match-end 0)
					set-id entry-id subentry-id))
		     
		     
		     ;; if existing match overlay corresponding to same entry
		     ;; and edge but different subentry overlaps new match...
		     ((and (auto-o-entry-compound-class-p set-id entry-id)
			   (setq o-overlap
				 (auto-o-overlapping-match
				  (match-beginning group) (match-end group)
				  set-id entry-id subentry-id
				  (auto-o-entry-edge set-id entry-id
						     subentry-id))))
		      ;; if new match takes precedence, replace existing one
		      ;; with new one, otherwise ignore new match
		      (when (< rank (auto-o-compound-rank o-overlap))
			(delete-overlay o-overlap)
			(setq o-match (auto-o-make-match
				       set-id entry-id
				       (match-beginning 0) (match-end 0)
				       subentry-id (match-beginning group)
				       (match-end group)))
			(when (overlay-get o-overlap 'parent)
			  (auto-o-match-overlay (overlay-get o-overlap 'parent)
						o-match))
			;; run match function if there is one
			(let ((match-func (auto-o-match-function o-match)))
			  (when match-func (funcall match-func o-match)))))
		     
		     ;; if match is within a higher priority exclusive
		     ;; overlay, create match overlay but don't parse it
		     ((auto-o-within-exclusive-p (match-beginning group)
						 (match-end group)
						 priority)
		      (auto-o-make-match set-id entry-id
					 (match-beginning 0) (match-end 0)
					 subentry-id (match-beginning group)
					 (match-end group)))
		     
		     
		     ;; if we're going to parse the new match...
		     (t
		      ;; create a match overlay for it
		      (setq o-match (auto-o-make-match
				     set-id entry-id
				     (match-beginning 0) (match-end 0)
				     subentry-id
				     (match-beginning group)
				     (match-end group)))
		      ;; call the appropriate parse function
		      (setq o-new
			    (funcall (auto-o-parse-function o-match) o-match))
		      (unless (listp o-new) (setq o-new (list o-new)))
		      ;; give any new overlays some basic properties
		      (mapc (lambda (o)
			      (overlay-put o 'auto-overlay t)
			      (overlay-put o 'set-id set-id)
			      (overlay-put o 'entry-id entry-id))
			    o-new)
		      ;; run match function if there is one
		      (let ((match-func (auto-o-match-function o-match)))
			(when match-func (funcall match-func o-match)))))
		    
		    
		    ;; go to character one beyond the start of the match, to
		    ;; make sure we don't miss the next match (if we find the
		    ;; same one again, it will just be ignored)
		    (goto-char (+ (match-beginning 0) 1)))))
	      (forward-line 1))
	    )))))
)




(defun auto-o-suicide (o-self &optional force)
  ;; This function is assigned to all match overlay modification hooks, and
  ;; calls the appropriate suicide function for match overlay O-SELF.
  ;; If FORCE is non-nil, O-SELF is deleted irrespective of whether its
  ;; overlay still matches.
  
  ;; this is here to avoid a weird bug(?) where the modification-hooks seem
  ;; to be called occasionally for overlays that have already been deleted
  (when (overlay-buffer o-self)
    ;; if match overlay no longer matches the text it covers...
    (unless (and (not force)
		 (save-excursion
		  (goto-char (overlay-start o-self))
		  (looking-at (auto-o-regexp o-self)))
		 (= (match-end 0) (overlay-end o-self)))
      ;; if we have a parent overlay, call appropriate suicide function,
      ;; schedule an update (necessary for complicated reasons!) then delete
      ;; ourselves
      (when (overlay-get o-self 'parent)
	(funcall (auto-o-suicide-function o-self) o-self))
      ;; Note: not supplying the 'set-id can avoid multiple, effectively
      ;; identical auto-overlay-update calls
      (auto-o-schedule-update (overlay-start o-self))
      (delete-overlay o-self)))
)




(defun auto-o-update-exclusive (set-id beg end old-priority new-priority)
  ;; If priority has increased, delete all overlays between BEG end END that
  ;; have priority lower than NEW-PRIORITY. If priority has decreased, re-parse
  ;; all matches with priority lower than OLD-PRIORITY.

  (let (overlay-list)
    (cond
     ;; if priority has increased...
     ((and new-priority
	   (or (null old-priority) (> new-priority old-priority)))
      ;; find overlays entirely within BEG and END that are both start and end
      ;; matched and have priority lower than NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay)
		   (list 'eq 'set-id set-id)
		   '(identity start)
		   (list (lambda (entry-id start end)
			   (or (null (auto-o-entry-compound-class-p
				      set-id entry-id))
			       (and start end)))
			 '(entry-id start end))
		   (list (lambda (pri new) (or (null pri) (< pri new)))
			 'priority new-priority))
	     'within))
      ;; mark overlays in list as inactive (more efficient than calling
      ;; suicide functions or deleting the overlays, and leaves them intact in
      ;; case the exclusivity of the region is later reduced - see below)
      (dolist (o overlay-list) (overlay-put o 'inactive t))
      
      ;; find match overlays between BEG and END that have priority lower then
      ;; NEW-PRIORITY but still have an active parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay-match)
		   (list 'eq 'set-id set-id)
		   ;; note: parentless overlays are possible if a suicide is
		   ;; in progress, so need to check overlay has a parent first
		   '(identity parent)
		   (list (lambda (parent)
			   (null (overlay-get parent 'inactive)))
			 'parent)
		   (list (lambda (set-id entry-id subentry-id new-pri)
			   (let ((pri (cdr (assq
					    'priority
					    (auto-o-entry-props
					     set-id entry-id subentry-id)))))
			     (or (null pri) (< pri new-pri))))
			 '(set-id entry-id subentry-id)
			 (list new-priority)))))
      ;; call appropriate suicide function for each match overlay in list
      (dolist (o overlay-list) (funcall (auto-o-suicide-function o) o)))
     
     
     ;; if priority has decreased...
     ((and old-priority
	   (or (null new-priority) (< new-priority old-priority)))
      ;; find inactive overlays entirely within BEG and END that have priority
      ;; higher or equal to NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay)
		   (list 'eq 'set-id set-id)
		   '(identity inactive)
		   (list (lambda (pri new) (or (null new) (>= pri new)))
			 'priority new-priority))
	     'within 'inactive))
      ;; mark overlays in list as active again
      (dolist (o overlay-list) (overlay-put o 'inactive nil))
      
      ;; find match overlays between BEG and END that have priority higher or
      ;; equal to NEW-PRIORITY but no parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay-match)
		   (list 'eq 'set-id set-id)
		   '(null parent)
		   (list (lambda (set-id entry-id subentry-id new-pri)
			   (let ((pri (cdr (assq
					    'priority
					    (auto-o-entry-props
					     set-id entry-id subentry-id)))))
			     (or (null new-pri) (>= pri new-pri))))
			 '(set-id entry-id subentry-id)
			 (list new-priority)))))
      ;; call appropriate parse function for each match overlay in list
      (dolist (o-match overlay-list)
	(when (not (auto-o-within-exclusive-p o-match))
	  (let ((o-new (funcall (auto-o-parse-function o-match) o-match)))
	    ;; give any new overlays the basic properties and add them to
	    ;; `auto-overlay-list'
	    (unless (listp o-new) (setq o-new (list o-new)))
	    (mapc (lambda (o)
		    (overlay-put o 'auto-overlay t)
		    (overlay-put o 'set-id set-id)
		    (overlay-put o 'entry-id (overlay-get o-match 'entry-id)))
		  o-new)))))
     ))
)




(defun auto-o-make-match (set-id entry-id start end
			      &optional subentry-id delim-start delim-end)
  ;; Create a new match overlay and give it the appropriate properties.
  (let ((o-match (make-overlay start end nil 'front-advance nil)))
    (overlay-put o-match 'auto-overlay-match t)
    (overlay-put o-match 'set-id set-id)
    (overlay-put o-match 'entry-id entry-id)
    (overlay-put o-match 'delim-start
		 (set-marker (make-marker)
			     (if delim-start delim-start start)))
    (overlay-put o-match 'delim-end
		 (set-marker (make-marker)
			     (if delim-end delim-end end)))
    (set-marker-insertion-type (overlay-get o-match 'delim-start) t)
    (set-marker-insertion-type (overlay-get o-match 'delim-end) nil)
    (overlay-put o-match 'modification-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-in-front-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-behind-hooks '(auto-o-schedule-suicide))
    ;; when regexp entry is a list of regexps, store subentry property
    (when (auto-o-entry-compound-class-p set-id entry-id)
      (overlay-put o-match 'subentry-id subentry-id))
    ;; return the new match overlay
    o-match)
)




(defun auto-o-match-overlay (overlay start &optional end
				     no-props no-parse protect-match)
  "Match start and end of OVERLAY with START and END match overlays.
If START or END are numbers or markers, move that edge to the
buffer location specified by the number or marker and make it
unmatched.  If START or END are non-nil but neither of the above,
make that edge unmatched.  If START or END are null, don't change
that edge. However, if END is null, and START is an 'end overlay,
match end of OVERLAY rather than start.
  
If NO-PARSE is non-nil, block re-parsing due to exclusive overlay
changes. If NO-PROPS is non-nil, block updating of overlay's
properties. If PROTECT-MATCH is non-nil, don't modify any match
overlays associated with OVERLAY (i.e. don't modify their 'parent
properties)."
  
  (let ((old-start (overlay-start overlay))
	(old-end (overlay-end overlay))
	(old-exclusive (overlay-get overlay 'exclusive))
	(old-priority (overlay-get overlay 'priority)))
    
    ;; if END is null, we're not unmatching, and START is an end overlay,
    ;; match end of overlay instead of start (Note: assumes we're matching an
    ;; overlay class with 'start and 'end regexps)
    (when (and (null end) (overlayp start) (eq (auto-o-edge start) 'end))
      (setq end start)
      (setq start nil))
    
    
    ;; move overlay to new location
    (move-overlay overlay
		  (cond
		   ((overlayp start) (overlay-get start 'delim-end))
		   ((number-or-marker-p start) start)
		   (start (point-min))
		   (t (overlay-start overlay)))
		  (cond
		   ((overlayp end) (overlay-get end 'delim-start))
		   ((number-or-marker-p end) end)
		   (end (point-max))
		   (t (overlay-end overlay))))
    ;; sort out start and end properties
    (let (o-match)
      ;; if unmatching start...
      (when (and start (not (overlayp start)))
	(setq o-match (overlay-get overlay 'start))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'start nil))
      ;; if unmatching end...
      (when (and end (not (overlayp end)))
	(setq o-match (overlay-get overlay 'end))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'end nil))
      ;; if matching start...
      (when (overlayp start)
	(setq o-match (overlay-get overlay 'start))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'start start)
	(overlay-put start 'parent overlay))
      ;; if matching end...
      (when (overlayp end)
	(setq o-match (overlay-get overlay 'end))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'end end)
	(overlay-put end 'parent overlay)))
    
    
    ;; unless it's blocked, update properties if new match takes precedence
    ;; (Note: this sometimes sets the overlay's properties to the ones it
    ;; already had, but it hardly seems worth checking for that)
    (unless no-props
      (let (props)
	(cond
	 ;; if start has been unmatched, use properties of end match
	 ((null (overlay-get overlay 'start))
	  (setq props (auto-o-props (overlay-get overlay 'end))))
	 ;; if end has been unmatched, use properties of start match
	 ((null (overlay-get overlay 'end))
	  (setq props (auto-o-props (overlay-get overlay 'start))))
	 (t  ;; otherwise, use properties of whichever match takes precedence
	  (let ((o-start (overlay-get overlay 'start))
		(o-end (overlay-get overlay 'end)))
	    (if (<= (auto-o-compound-rank o-start)
		    (auto-o-compound-rank o-end))
		(setq props (auto-o-props o-start))
	      (setq props (auto-o-props o-end))))))
	;; bundle properties inside a list if not already, then update them
	(when (symbolp (car props)) (setq props (list props)))
	(dolist (p props) (overlay-put overlay (car p) (cdr p)))))
    
    
    ;; unless it's blocked or overlay is inactive, check if anything needs
    ;; reparsing due to exclusive overlay changes
    (unless (or no-parse (overlay-get overlay 'inactive))
      (let ((set-id (overlay-get overlay 'set-id))
	    (start (overlay-start overlay))
	    (end (overlay-end overlay))
	    (exclusive (overlay-get overlay 'exclusive))
	    (priority (overlay-get overlay 'priority)))
	(cond
	 
	;; if overlay wasn't and still isn't exclusive, do nothing
	 ((and (null exclusive) (null old-exclusive)))
	 
	 ;; if overlay has become exclusive, delete lower priority overlays
	 ;; within it
	 ((and (null old-exclusive) exclusive)
	  (auto-o-update-exclusive set-id start end nil priority))
	 
	 ;; if overlay was exclusive but no longer is, re-parse region it
	 ;; used to cover
	 ((and old-exclusive (null exclusive))
	  (auto-o-update-exclusive set-id old-start old-end old-priority nil))
	 
	 ;; if overlay was and is exclusive, and has been moved to a
	 ;; completely different location re-parse old location and delete
	 ;; lower priority overlays within new location
	 ((or (< end old-start) (> start old-start))
	  (auto-o-update-exclusive set-id start end old-priority nil)
	  (auto-o-update-exclusive set-id start end nil priority))

	 ;; if overlay was and is exclusive, and overlaps its old location...
	 (t
	  ;; if priority has changed, re-parse/delete in overlap region
	  (when (/= old-priority priority)
	    (auto-o-update-exclusive set-id
				     (max start old-start) (min end old-end)
				     old-priority priority))
	  (cond
	   ;; if overlay was exclusive and start has shrunk, re-parse
	   ;; uncovered region
	   ((and (> start old-start) old-exclusive)
	    (auto-o-update-exclusive set-id old-start start old-priority nil))
	   ;; if overlay is exclusive and has grown, delete lower priority
	   ;; overlays in newly covered region
	   ((and (< start old-start) exclusive)
	    (auto-o-update-exclusive set-id start old-start nil priority)))
	  (cond
	   ;; if overlay was exclusive and end has shrunk, re-parse
	   ((and (< end old-end) old-exclusive)
	    (auto-o-update-exclusive set-id end old-end old-priority nil))
	    ;; if overlay is exclusive and has grown, delete lower priority
	   ((and (> end old-end) exclusive)
	    (auto-o-update-exclusive set-id old-end end nil priority))))
	 )))
    )
)




(defun auto-o-delete-overlay (overlay &optional no-parse protect-match)
  "Delete OVERLAY from buffer.

If PROTECT-MATCH is non-nil, don't modify any match overlays
associated with OVERLAY (i.e. leave their 'parent properties
alone). If NO-PARSE is non-nil, block re-parsing due to exclusive
overlay changes."
  
  (let ((start (overlay-start overlay))
	(end (overlay-end overlay))
	o-match)
    ;; delete overlay from buffer and `auto-overlay-list'
    (delete-overlay overlay)
    (unless (setq o-match (overlay-get overlay 'start))
      (setq o-match (overlay-get overlay 'end)))
;;    (auto-o-delete-from-overlay-list overlay)
    
    ;; unless blocked, if overlay's exclusive flag was set, re-parse region it
    ;; covered
    (when (and (null no-parse) (overlay-get overlay 'exclusive))
      (auto-o-update-exclusive (overlay-get overlay 'set-id) start end
			       (overlay-get overlay 'priority) nil))
    
    ;; Note: it's vital that the match overlays' parent properties are only
    ;; set to nil *after* `auto-update-exclusive' is run: if the overlay
    ;; overlapped one of its match overlays, the newly parentless match
    ;; overlay would be re-parsed by `auto-update-exclusive', which would
    ;; re-create the parent overlay that's just been deleted!
    
    ;; unmatch match overlays
    (unless protect-match
      (when (setq o-match (overlay-get overlay 'start))
	(overlay-put o-match 'parent nil))
      (when (setq o-match (overlay-get overlay 'end))
	(overlay-put o-match 'parent nil)))
    )
)




(defun auto-o-matched-p (beg end set-id entry-id &optional subentry-id)
  ;; Determine if characters between BEG end END are already matched by a
  ;; match overlay corresponding to ENTRY-ID (and optionally SUBENTRY-ID) of
  ;; regexp set SET-ID.
  (let (o-match)
    (catch 'match
      (mapc (lambda (o)
	      (when (and (overlay-get o 'auto-overlay-match)
			 (eq (overlay-get o 'set-id) set-id)
			 (eq (overlay-get o 'entry-id) entry-id)
			 (or (not (auto-o-entry-compound-class-p
				   set-id entry-id))
			     (eq (overlay-get o 'subentry-id) subentry-id))
			 (= (overlay-start o) beg)
			 (= (overlay-end o) end))
		(setq o-match o)
		(throw 'match t)))
	    (overlays-in beg end)))
    o-match)
)




(defun auto-o-within-exclusive-p (match &optional end priority)
  ;; If MATCH is an overlay, determine if it is within a higher priority
  ;; exclusive overlay. If MATCH is a number or marker, determine whether
  ;; region between MATCH and END is within an exclusive overlay with higher
  ;; priority than PRIORITY.

  (when (null end)
    (setq end (overlay-get match 'delim-end))
    (setq priority (overlay-get match 'priority))
    (setq match (overlay-get match 'delim-start)))
  
  ;; look for higher priority exclusive overlays
  (auto-overlays-in
   match end
   (list '(identity auto-overlay)
	 '(identity exclusive)
	 (list (lambda (p q) (and p (or (null q) (> p q))))
	       'priority priority)))
)
  



(defun auto-o-overlapping-match (beg end set-id entry-id subentry-id edge)
  ;; Returns any match overlay corresponding to same SET-ID, ENTRY-ID and EDGE
  ;; but different SUBENTRY-ID whose delimeter overlaps region from BEG to
  ;; END. (Only returns first one it finds; which is returned if more than one
  ;; exists is undefined.)
  (let (o-overlap)
    (catch 'match
      (mapc (lambda (o)
	      (when (and (overlay-get o 'auto-overlay-match)
			 (eq (overlay-get o 'set-id) set-id)
			 (eq (overlay-get o 'entry-id) entry-id)
			 (not (eq (overlay-get o 'subentry-id) subentry-id))
			 (eq (auto-o-edge o) edge)
			 ;; check delimeter (not just o) overlaps BEG to END
			 (<= (overlay-get o 'delim-start) end)
			 (>= (overlay-get o 'delim-end) beg))
		(setq o-overlap o)
		(throw 'match t)))
	    (overlays-in beg end)))
    o-overlap)
)




;;; ===============================================================
;;;                       Compatibility Stuff

(unless (fboundp 'line-number-at-pos)
  (require 'auto-overlays-compat)
  (defalias 'line-number-at-pos
            'auto-overlays-compat-line-number-at-pos)
)


(unless (fboundp 'replace-regexp-in-string)
  (require 'auto-overlays-compat)
  (defalias 'replace-regexp-in-string
            'auto-overlays-compat-replace-regexp-in-string)
)

;;; auto-overlays.el ends here
