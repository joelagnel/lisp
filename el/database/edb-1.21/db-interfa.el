;;; db-interfa.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Commands for operating on the current database.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(deflocalvar dbc-database nil
  "The database associated with this format.
This variable is also set in the summary format.")

;;; Database modification

(deflocalvar dbc-database-modified-p nil
  "T if the database has been modified, nil otherwise.
Mirrors the value of the modified-p slot of the database.
This has to be a real variable so it can go in mode-line-format.
Set it using `dbc-set-database-modified-p'.")
;; Usually dbc-set-database-modified-p is used instead.
(defsubst dbc-update-database-modified-p ()
  (setq dbc-database-modified-p (database-modified-p dbc-database)))
(defsubst dbc-set-database-modified-p (value)
  (database-set-modified-p dbc-database value)
  (setq dbc-database-modified-p value))

;;; Hiding

;; Making this default to t affects all buffers, even non-EDB ones,
;; resulting in an ugly "Hide" in their mode lines.
(deflocalvar dbc-hide-p nil
  "Non-nil if hiding is in effect, nil otherwise.
Use function `dbc-set-hide-p', which works in either a data display buffer or
a summary buffer and sets the variable's value in both, instead of setting
this directly.
Setting this to nil is cheaper than changing the hide function to the empty
one, since no hide bits are recomputed.
This variable is automatically set by the hiding functions.")
;; Beware, however, that if records have changed, then whether they should
;; still be hidden may change too.  Perhaps set the hide bit to 'recompute
;; or something like that if dbf-hide-function is set but this is nil.
;; Nah, I don't like that: just pay the price to recompute the bit.  This
;; should be set in the format, not the summary, buffer.

;; At present this is only called from the data display buffer, so some of
;; this is extraneous, but it needs to be callable from anywhere.
;; Other necessary work includes updating the mode line, calling
;; dbf-update-summary-marks, etc.
(defun dbc-set-hide-p (value)
  "Set `dbc-hide-p' to VALUE in both data display buffer and summary buffer.
Does no other housekeeping."
  (db-in-data-display-buffer
    (setq dbc-hide-p value)
    (dbf-in-summary-buffer
      (setq dbc-hide-p value))))
;; (proclaim-inline dbc-set-hide-p)

;;; Location in the database

;; The current link and its index.
(deflocalvar dbc-link nil
  "The link of the record currently being displayed, or nil.")
(deflocalvar dbc-index nil
  "The index of the record currently being displayed (and of its link), or nil.
Use `dbc-set-index' to set this value unless you know what you are doing.")

(deflocalvar dbc-index-fraction nil
  "A string of the form dbc-index/database-no-of-records.
Variables with numeric values aren't allowed in mode-line-format.
An asterisk (*) precedes dbc-index if the current record is marked.
The fraction is surrounded by square brackets if the current record is hidden.
This variable should only be set by calling `dbc-set-index'.")

;;; Movement behavior

(deflocalvar dbc-wraparound-p 'delay
  "Value t, nil, or 'delay determines whether going forward from the last
record (or backward from the first) wraps, is prohibited, or denies on the
first attempt only and then wraps.")

(deflocalvar dbf-stay-in-edit-mode-p t
  "*Whether edit mode is preserved when switching records in EDB.
Automatically becomes local to the current buffer when set in any fashion.
Only has an effect when set in an EDB data display buffer.")

;;; Etc.

(deflocalvar db-new-record-function nil
  "Function called on empty records before they're inserted in the database.
Takes two arguments, the record and the database.")

;;   "Non-nil if `db-kill-buffer-hook' shouldn't do anything."
(defvar db-kill-buffer-hook-inhibit-p nil)


(defvar db-delete-record-modifies-database-p t
  "Non-nil if deleting a record should mark the database as modified.")

;;   "Non-nil if a database's print-name or filename should be mentioned when
;; it is saved to disk."
(defvar db-mention-filename-on-save-p t)


(defvar db-auto-edit-mode t
  "nil if movement around the data display buffer is permitted in view mode.
When this variable is non-nil \(it defaults to t\), mousing and most
movement commands cause edit mode to be entered on the appropriate field.
Don't set this variable directly; use command `db-toggle-auto-edit-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps
;;;

;; Probably this shouldn't be a sparse keymap after all.
;;   "Keymap for database data display buffer in view mode."
(defvar database-view-mode-map (make-keymap))

(suppress-keymap database-view-mode-map)

;; Moving around in the database
(define-key database-view-mode-map "n" 'db-next-record)
(define-key database-view-mode-map "p" 'db-previous-record)
(define-key database-view-mode-map "<" 'db-first-record)
(define-key database-view-mode-map ">" 'db-last-record)
(define-key database-view-mode-map (db-meta-prefix-ify "<") 'db-first-record)
(define-key database-view-mode-map (db-meta-prefix-ify ">") 'db-last-record)
(define-key database-view-mode-map "j" 'db-jump-to-record)
(define-key database-view-mode-map " " 'db-next-screen-or-record)
(define-key database-view-mode-map "\177" 'db-previous-screen-or-record)
(define-key database-view-mode-map (db-meta-prefix-ify "n") 'db-next-record-ignore-hiding)
(define-key database-view-mode-map (db-meta-prefix-ify "p") 'db-previous-record-ignore-hiding)
(define-key database-view-mode-map (db-meta-prefix-ify "\C-n") 'db-next-marked-record)
(define-key database-view-mode-map (db-meta-prefix-ify "\C-p") 'db-previous-marked-record)

;; Changing to edit mode
(define-key database-view-mode-map "\t" 'db-first-field)
(define-key database-view-mode-map (db-meta-prefix-ify "\t") 'db-last-field)
(define-key database-view-mode-map "\C-n" 'db-first-field)
(define-key database-view-mode-map "\C-p" 'db-last-field)
(define-key database-view-mode-map "e" 'db-first-field)
;; These could be db-first-field and db-last-field, but that wouldn't fit in:
;; nowhere else are these keystrokes inter-field-movement keystrokes.
(define-key database-view-mode-map "\C-f" 'undefined)
(define-key database-view-mode-map "\C-b" 'undefined)
(define-key database-view-mode-map "\C-a" 'undefined)
(define-key database-view-mode-map "\C-e" 'undefined)
;; What's the point of this?
;; (define-key database-view-mode-map "v" 'db-view-mode)
(define-key database-view-mode-map "\C-v" 'db-scroll-up)
;; In view-mode, we're at the top of the buffer (not after db-next-screen).
(define-key database-view-mode-map (db-meta-prefix-ify "v") 'db-scroll-down)

;; Undoing changes
(define-key database-view-mode-map "\C-xu" 'db-revert-record)
;; (define-key database-view-mode-map "u" 'db-revert-record)
(define-key database-view-mode-map "\C-xr" 'db-revert-database)

;; Adding and removing records
(define-key database-view-mode-map "a" 'db-add-record)
(define-key database-view-mode-map "i" 'db-add-record)
(define-key database-view-mode-map "d" 'db-delete-record)
(define-key database-view-mode-map "k" 'db-delete-record)
(define-key database-view-mode-map "y" 'db-yank-record)
(define-key database-view-mode-map "o" 'db-output-record-to-db)
(define-key database-view-mode-map "c" 'db-copy-record)

;; Searching commands
(define-key database-view-mode-map (db-meta-prefix-ify "S") 'db-search)
(define-key database-view-mode-map (db-meta-prefix-ify "s") 'db-search)
(define-key database-view-mode-map "s" 'db-search)
(define-key database-view-mode-map "S" 'db-incremental-search)
(define-key database-view-mode-map "\C-s" 'db-isearch-forward)
(define-key database-view-mode-map "\C-r" 'db-isearch-backward)

;; Exiting database mode
(define-key database-view-mode-map "q" 'db-quit)
(define-key database-view-mode-map "x" 'db-exit)


(define-key database-view-mode-map "m" 'db-mark-record)



(define-key database-view-mode-map "?" 'describe-mode)

;; Gross key bindings.
(define-key database-view-mode-map "O" 'db-hide-record)
(define-key database-view-mode-map (db-meta-prefix-ify "o") 'db-hiding-toggle)
(define-key database-view-mode-map (db-meta-prefix-ify "O") 'db-hiding-set)
(define-key database-view-mode-map (db-meta-prefix-ify "\C-o") 'db-toggle-show-hidden-records)


(define-key database-view-mode-map "D" 'db-summary) ; mnemonic for Directory
(define-key database-view-mode-map "h" 'db-summary) ; mnemonic for Headers
(define-key database-view-mode-map "H" 'db-summary) ; mnemonic for Headers

(define-key database-view-mode-map "r" 'db-report)
(define-key database-view-mode-map "\r" 'db-accept-record)

(define-key database-view-mode-map "b" 'undefined)
(define-key database-view-mode-map "f" 'undefined)
(define-key database-view-mode-map "g" 'undefined)
(define-key database-view-mode-map "l" 'undefined)
(define-key database-view-mode-map "t" 'undefined)
(define-key database-view-mode-map "w" 'undefined)
(define-key database-view-mode-map "z" 'undefined)


;;   "Keymap for database data display buffer in edit mode."
(defvar database-edit-mode-map (make-keymap))

;; Obviously don't do suppress-keymap on this one; we want to be able to edit.
;; The view-mode commands should be available via C-c and many (such as
;; next-record) available via M- commands as well, espcially those not
;; ordinarily bound in text mode (eg M-n and M-p).

;; Lucid Emacs's mouse-handling is completely different from version 18's.
(if (not db-running-lucid-emacs)
    ;; This needs to be global because we might mouse in the data display
    ;; buffer while point is in some other buffer (which has its own binding
    ;; for \C-x\C-@).  \C-x\C-@ is what mouse clicks send to the buffer.
    (global-set-key "\C-x\C-@" 'db-x-jump-to-point))

;; Exiting edit mode
(define-key database-edit-mode-map "\C-c\C-c" 'db-view-mode)

;; Undoing changes
(define-key database-edit-mode-map "\C-xU" 'db-revert-field)

;; Moving from record to record
(define-key database-edit-mode-map (db-meta-prefix-ify "n") 'db-next-record)
(define-key database-edit-mode-map (db-meta-prefix-ify "p") 'db-previous-record)

;; Moving from field to field
(define-key database-edit-mode-map "\t" 'db-next-field)
(define-key database-edit-mode-map (db-meta-prefix-ify "\t") 'db-previous-field)
(define-key database-edit-mode-map (db-meta-prefix-ify "<") 'db-first-field)
(define-key database-edit-mode-map (db-meta-prefix-ify ">") 'db-last-field)
(define-key database-edit-mode-map "\C-v" 'db-scroll-up)
(define-key database-edit-mode-map (db-meta-prefix-ify "v") 'db-scroll-down)


;; Movement within a field
(define-key database-edit-mode-map "\C-n" 'db-next-line-or-field)
(define-key database-edit-mode-map "\C-p" 'db-previous-line-or-field)
;; almost-the-same-as-before commands
(define-key database-edit-mode-map "\C-f" 'db-forward-char)
(define-key database-edit-mode-map "\C-b" 'db-backward-char)
(define-key database-edit-mode-map (db-meta-prefix-ify "f") 'db-forward-word)
(define-key database-edit-mode-map (db-meta-prefix-ify "b") 'db-backward-word)
(define-key database-edit-mode-map "\C-a" 'db-beginning-of-line-or-field)
(define-key database-edit-mode-map "\C-e" 'db-end-of-line-or-field)

;; Editing a field
;;insertion
(define-key database-edit-mode-map "\r" 'db-newline)
(define-key database-edit-mode-map "\n" 'db-newline)
(define-key database-edit-mode-map "\C-o" 'db-open-line)
;;deletion
(define-key database-edit-mode-map "\C-d" 'db-delete-char)
(define-key database-edit-mode-map "\177" 'db-backward-delete-char)
(define-key database-edit-mode-map (db-meta-prefix-ify "d") 'db-kill-word)
(define-key database-edit-mode-map (db-meta-prefix-ify "\177") 'db-backward-kill-word)
(define-key database-edit-mode-map "\C-k" 'db-kill-line)
(define-key database-edit-mode-map (db-meta-prefix-ify "k") 'db-kill-to-end)
(define-key database-edit-mode-map "\C-w" 'db-kill-region)
(define-key database-edit-mode-map (db-meta-prefix-ify "w") 'db-copy-region-as-kill)

;; Other commands
(define-key database-edit-mode-map (db-meta-prefix-ify "s") 'db-search-field)
;; (define-key database-edit-mode-map (db-meta-prefix-ify "S") 'db-search-field)


(define-key database-edit-mode-map "\C-s" 'db-isearch-forward)
(define-key database-edit-mode-map "\C-r" 'db-isearch-backward)

(define-key database-edit-mode-map (db-meta-prefix-ify "?") 'db-field-help)


;;; Bindings for both keymaps

;; Saving the database
(define-key database-view-mode-map "\C-x\C-s" 'db-save-database)
(define-key database-edit-mode-map "\C-x\C-s" 'db-save-database)
(define-key database-view-mode-map "\C-x\C-w" 'db-write-database-file)
(define-key database-edit-mode-map "\C-x\C-w" 'db-write-database-file)

;; Toggling modifiable-p
(define-key database-view-mode-map "\C-x\C-q" 'db-toggle-modifiable-p)
(define-key database-edit-mode-map "\C-x\C-q" 'db-toggle-modifiable-p)

;; Wipe out dangerous commands
(define-key database-view-mode-map "\C-xn" 'undefined)
(define-key database-edit-mode-map "\C-xn" 'undefined)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

;; Menus by Alastair Burt <burt@dfki.uni-kl.de>,
;;	    Michael Ernst <mernst@theory.lcs.mit.edu>
;;	    John Overton <overton@cs.uchicago.edu>

(defvar database-view-mode-menu
  '("Database"
    "VIEW Mode:"
    ["Edit"            db-first-field                 t]
    ["Report"          db-report                      t]
    ["Summary"         db-summary                     t]
    "---"
    ["Revert  record" db-revert-record t]
    ["Accept  record" db-accept-record t]
    ["Add     record" db-add-record t]
    ["Copy    record" db-copy-record t]
    ["Delete  record" db-delete-record t]
    ["Output  record" db-output-record-to-db t]
    ["Mark    record" db-mark-record t]
    ["Hide    record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    ("Motion"
     ["jump to"  db-jump-to-record t]
     ["first" db-first-record   t]
     ["last"  db-last-record    t]
     "---"
     ["next"               db-next-record                     t]
     ["next (screen)"      db-next-screen-or-record           t]
     ["next (marked)"      db-next-marked-record              t]
     ["next (ignore hiding)" db-next-record-ignore-hiding     t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen)"      db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ignore hiding)" db-previous-record-ignore-hiding t]
     )
    "----"
    ["Create Report" db-report t]
    ["Toggle Hiding" db-hiding-toggle t]
    ["Summary" db-summary t]
    "----"
    ["Edit Mode" db-first-field t]
    "----"
    ["Sort    database" db-sort t]
    ["Revert  database" db-revert-database t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    ["Internal Layout" db-toggle-internal-file-layout t]
    "----"
    ["Quit" db-quit t]
    )
  "Menu for Database View mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
		  database-view-mode-map
		  "ignored-doc-string"
		  database-view-mode-menu)

;; (defun database-view-mode-menu (e)
;;   (interactive "@e")
;;   (setq zmacs-region-stays 't)
;;   (popup-menu database-view-mode-menu))


(defvar database-edit-mode-menu
  '("Database"
    "EDIT Mode:"
    ["View mode"        db-view-mode      t]
    ["Report"           db-report         t]
    ["Summary"          db-summary        t]
    ["Summary   subset" db-summary-subset t]
    "---"
    ["Revert    record"  db-revert-record     t]
    "---"
    ["Revert     field"  db-revert-field   t]
    ["Help    on field" db-field-help t]
    ["Search  in field" db-search-field t]
    "---"
    ("Motion"
     ["Next       field"  db-next-field     t]
     ["Prev       field"  db-previous-field t]
     ["Last       field"  db-last-field     t]
     ["First      field"  db-first-field    t]
     ["Next      record" db-next-record t]
     ["Previous  record" db-previous-record t])
    "---"
    ["Revert  database" db-revert-database t]
    ["Search  database"  db-search-field        t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    "---"
    ["Quit" db-quit t]
    )
  "Menu for Database Edit mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
		  database-edit-mode-map
		  "ignored-doc-string"
		  database-edit-mode-menu)

;; (defun database-edit-mode-menu (e)
;;   (interactive "@e")
;;   (setq zmacs-region-stays 't)
;;   (popup-menu database-edit-mode-menu))


(if db-running-lucid-emacs
    (progn
      (define-key database-view-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
      (define-key database-view-mode-map [mouse3] 'database-view-mode-menu)

      (define-key database-edit-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
      (define-key database-edit-mode-map [mouse3] 'database-edit-mode-menu)

      (define-key database-summary-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)

      (defun db-lucid-mouse-jump-to-point (e)
	"Move to the field or record nearest the mouse position.
See `db-jump-to-point' for more details."
	(interactive "@e")		; @ = select buffer, e = event
	(mouse-track e)			; set point to where the mouse is
	(db-jump-to-point))

      (defun dbs-lucid-mouse-view (e)
	"Visit record under mouse in Database View mode."
	(interactive "@e")
	(mouse-set-point e)
	(db-jump-to-point)
	(dbs-view))))

;; ;; These functions put the mode menus (bound to button3) onto the menubar.
;; ;; This makes EDB more like VM, GNUS, etc.
;; (if db-running-lucid-emacs
;;     (progn
;;       (defun db-lucid-view-mode-menubar ()
;; 	(if current-menubar
;; 	    (if (assoc "DB:View" current-menubar)
;; 		nil
;; 	      (if (assoc "DB:Edit" current-menubar)
;; 		  (delete-menu-item (list "DB:Edit"))
;; 		(set-buffer-menubar (copy-sequence current-menubar)))
;; 	      (add-menu nil "DB:View"
;; 			(cdr database-view-mode-menu)))))
;; 
;;       (defun db-lucid-edit-mode-menubar ()
;; 	(if current-menubar
;; 	    (if (assoc "DB:Edit" current-menubar)
;; 		nil
;; 	      (if (assoc "DB:View" current-menubar)
;; 		  (delete-menu-item (list "DB:View"))
;; 		(set-buffer-menubar (copy-sequence current-menubar)))
;; 	      (add-menu nil "DB:Edit"
;; 			(cdr database-edit-mode-menu)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help
;;;

(defun db-field-help ()
  "Display help for current field using the recordfieldspec help-info field.
If this is a string, display it.  If it is a form, eval it and display the
result."
  (interactive)
  (if (not dbf-this-displayspec)
      (error "Not on a field."))
  (let* ((field-index (displayspec-record-index dbf-this-displayspec))
	 (help-text (recordfieldspec-help-info
		     (database-recordfieldspec dbc-database field-index))))
    (db-best-fit-message
     (if help-text
	 (if (stringp help-text)
	     help-text
	   (condition-case err
	       (eval help-text)
	     (error
	      (format
	       "This help form:\n\n  %s\n\nfailed with this error:\n\n%s"
	       help-text err))))
       (format "No help available for `%s'."
	       (fieldnumber->fieldname field-index dbc-database)))
     " *DB Help*")))

;;; Is this the right thing to do?
;; Install a help string for a given field.  Construct a new type by
;; copying the current one, changing help string, and installing the new
;; type.
(defun db-set-field-help (field help-str database)
  (let* ((fnum (fieldname->fieldnumber field database))
         (rs (copy-recordfieldspec (database-recordfieldspec database fnum))))
    (recordfieldspec-set-help-info rs help-str)
    (database-set-recordfieldspec database fnum rs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quitting
;;;

;; I have made these work in both the data display buffer and the summary
;; buffer, for folks who enjoy spending most of their time in the summary
;; who will rebind its keystrokes to call these functions instead of
;; dbs-exit (for instance).

(defun db-quit ()
  "Quit editing the database for now; bury its buffers."
  (interactive)
  (db-bury))

(defun db-exit (&optional kill)
  "Be done with the database; like `db-quit', but offers to save any changes.
With prefix argument, kills the data display buffer, and the database, if that
was its only data display buffer."
  (interactive "P")
  (db-save-database t)
  (if kill
      (db-kill-buffers)
    (db-quit)))

(defun db-kill-buffers ()
  "Kill this buffer, and the associated summary or data display buffer, if any.
If its last data display buffer is killed, the database is killed too.
Does not offer to save changes to the database or to this record; use `db-exit'
with optional argument to do so."
  (db-in-data-display-buffer
    ;; I don't call database-clean-data-display-buffers here; should I?
    (dbf-kill-summary)
    (let ((remaining-buffers (delq (current-buffer)
				   (database-data-display-buffers dbc-database))))
      (if remaining-buffers
	  (database-set-data-display-buffers dbc-database remaining-buffers)
	(setq db-databases (delq dbc-database db-databases))))
    (let ((db-kill-buffer-hook-inhibit-p t))
      (kill-buffer (current-buffer)))))

;;   "Kill this data display buffer's associated database summary buffer."
(defun dbf-kill-summary ()
  (dbf-in-summary-buffer
    (delete-windows-on (current-buffer))
    (let ((db-kill-buffer-hook-inhibit-p t))
      (kill-buffer (current-buffer)))))

;; Does nothing if db-kill-buffer-hook-inhibit-p is non-nil.
(defun db-kill-buffer-hook ()
  (cond ((and (not db-kill-buffer-hook-inhibit-p)
	      (or (db-data-display-buffer-p)
		  (db-summary-buffer-p)))
	 (if (or dbf-this-record-modified-p
		 (dbf-this-field-modified-p))
	     (if (y-or-n-p (concat "Commit the current record before killing "
				   (database-print-name dbc-database)
				   "? "))
		 (progn
		   (dbf-process-current-record-maybe t)
		   ;; Ask whether to save the database.
		   (db-save-database t)))
	   ;; Ask whether to save the database.
	   (db-save-database t))
	 ;; We have asked whether to save the database unless this record was
	 ;; modified and the user didn't want to commit it.
	 (db-kill-buffers))))

;;   "Bury the data display and summary buffers.
;; Spare either or both of these buffers by specifying optional arguments
;; NOT-DATA-DISPLAY and NOT-SUMMARY."
(defun db-bury (&optional not-data-display not-summary)
  (let (data-display-buffer
	summary-buffer)
    (db-in-data-display-buffer
      (setq data-display-buffer (and (not not-data-display) (current-buffer))
	    summary-buffer (and (not not-summary) (dbf-summary-buffer))))
    (if data-display-buffer
	(progn
	  (delete-windows-on data-display-buffer)
	  (bury-buffer data-display-buffer)))
    (if summary-buffer
	(progn
	  (delete-windows-on summary-buffer)
	  (bury-buffer summary-buffer)))))

(if (not (fboundp 'db-old-save-some-buffers))
    (progn
      (fset 'db-old-save-some-buffers (symbol-function 'save-some-buffers))
      (fset 'save-some-buffers 'db-save-some-buffers)))

(defun db-save-some-buffers (&optional quietly exiting)
  "Save some modified databases and file-visiting buffers.
Asks user about each one.  With argument, saves all with no questions."
  (interactive "P")
  (db-save-some-databases quietly)
  (db-old-save-some-buffers quietly exiting))

;; This isn't quite right because it should modify the ???.
(defun db-save-some-databases (&optional quietly)
  "Save some modified databases.  Asks user about each one.
With argument, saves all with no questions."
  (interactive "P")
  (let ((databases db-databases)
	this-database
	buffers
	buffers-remaining)
    (while databases
      (setq this-database (car databases)
	    databases (cdr databases)
	    buffers (database-clean-data-display-buffers this-database))
      ;; Could use map-data-display-buffers here.
      (if buffers
	  (progn
	    (setq buffers-remaining buffers)
	    (while buffers-remaining
	      (in-buffer (car buffers-remaining)
		(dbf-process-current-record-maybe t))
	      (setq buffers-remaining (cdr buffers-remaining)))

	    (in-buffer (car buffers)
	      (db-save-database (not quietly)))

	    (setq buffers-remaining buffers)
	    (while buffers-remaining
	      (in-buffer (car buffers-remaining)
		(force-mode-line-update))
	      (setq buffers-remaining (cdr buffers-remaining))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File I/O
;;;

;; This should check whether the file is currently read in.

;;;###autoload
(defun db-find-file (database-file &optional prompt-for-format)
  "Read a database from DATABASE-FILE; prompts when called interactively.
If the database file doesn't specify a format and the format file can't be
inferred from DATABASE-FILE, the user is prompted for it too.
The user is always prompted for the format if prefix argument
PROMPT-FOR-FORMAT is non-nil.
If the database is already read in and PROMPT-FOR-FORMAT is nil, the existing
database buffer is merely selected.
When called non-interactively, argument PROMPT-FOR-FORMAT may be a string, the
name of a format file to use."
  (interactive "fDatabase file: \nP")
  (setq database-file (expand-file-name database-file))
  (let ((format-file (if (stringp prompt-for-format) prompt-for-format))
	database data-display-buffer)
    (if (stringp prompt-for-format)
	(setq prompt-for-format nil))

    (if (not prompt-for-format)
	(progn
	  (setq database (db-find-read-in-database database-file))
	  (if (and database
		   (not (database-clean-data-display-buffers database)))
	      (setq database nil))
	  ;; Find an appropriate data display buffer
	  (if (and database format-file)
	      (let ((ddbs (database-data-display-buffers database))
		    ddb-format-file)
		(while ddbs
		  (if (db-same-file-p format-file
				   (in-buffer (car ddbs) dbf-format-file))
		      (setq data-display-buffer (car ddbs)
			    ddbs nil)
		    (setq ddbs (cdr ddbs))))))))
    (if (not database)
	;; Either prompt-for-format is non-nil, or we couldn't find an
	;; appropriate read-in-database.
	(setq database (read-database-file
			database-file format-file prompt-for-format)))
    (if (not data-display-buffer)
	(setq data-display-buffer
	      (car (database-clean-data-display-buffers database))))
    (switch-to-buffer data-display-buffer)
    (setq dbc-database database))
  (db-first-record))

(defun db-this-buffer ()
  "Run EDB on the file corresponding to the current buffer.
The current buffer is killed first."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (kill-buffer (current-buffer))
    (db-find-file file)))

;; This is too simplistic:  I'd like to check to see if it has another
;; non-killed data display buffer, and if not, I'd like to remove it from
;; db-databases altogether.
;; Plus, it could be the same database even if the filenames aren't the
;; same, if one had /u/mernst and the other had ~.  (Is this possible?)
;;   "Return the database most recently read in from DATABASE-FILE, or nil."
(defun db-find-read-in-database (database-filename)
  (let ((databases db-databases)
	result)
    (while (and (not result) databases)
      (setq result (if (and (db-same-file-p database-filename
					 (database-file (car databases)))
			    (car (database-clean-data-display-buffers (car databases))))
		       (car databases))
	    databases (cdr databases)))
    result))

;; Perhaps instead I want a for-each-data-display-buffer primitive which
;; also updates the database slot as it goes.  Actually, there is a
;; map-data-display-buffers function.
;;   "Remove killed buffers from DATABASE's data-display-buffers slot.
;; Returns a list of the remaining data display buffers.
;; If there are none, kills DATABASE as well."
(defun database-clean-data-display-buffers (database)
  (let* ((ddbs (database-data-display-buffers database))
	 (remaining ddbs))
    (while remaining
      (if (not (buffer-name (car remaining)))
	  (setq ddbs (delq (car remaining) ddbs)))
      (setq remaining (cdr remaining)))
    ;; Kill the database
    (if (null ddbs)
	(progn
	  (db-save-database-no-buffer database t nil)
	  (setq db-databases (delq database db-databases)))
      (database-set-data-display-buffers database ddbs))
    ddbs))

(defun db-revert-database ()
  "Replace the database with the data on disk.
This undoes all changes since the database was last saved."
  (interactive)
  (if (yes-or-no-p (format "Revert database from file %s? "
			   (database-file dbc-database)))
      (let ((database dbc-database)
	    (data-display-buffer (current-buffer))
	    (db-buffer (generate-new-buffer "read-database-file")))

	(set-buffer db-buffer)
	(insert-file-contents (database-file database) nil)
	(setq database (read-database-internal-file-layout-maybe))
	(read-database-file-helper db-buffer database)

	(mapcar (function (lambda (data-display-buffer)
		  (set-buffer data-display-buffer)
		  (dbc-update-database-modified-p)

		  ;; abandon any changes
		  (dbf-set-this-field-modified-p nil)
		  (setq dbf-this-record-modified-p nil)
		  (db-jump-to-record dbc-index nil)))
		(database-clean-data-display-buffers database))

	(db-message "Reverted database from disk.")
	)))


;; I'd like to complain if file write time is more current than it was when
;; the database was read; but all that is done in C and I don't feel like
;; reimplementing it yet.

;; I'd like a way to avoid saying "No changes..." without suppressing the
;; messages when the database does need to be saved.
(defun db-save-database (&optional query quietly)
  "Save the database to disk in the default save file.
Any changes to the current record are processed first.
The default save file is the file it was last saved to or read from.
If optional arg QUERY is specified, the user is asked first.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  (db-in-data-display-buffer
    ;; This is also done by db-write-database-file, but dbc-database-modified-p
    ;; won't be set if only the current record has changed,
    (db-debug-message "Current record about to be processed.")
    (dbf-process-current-record-maybe t)
    (db-debug-message "Current record processed.")
    (db-save-database-helper query quietly)))

(defun db-save-database-helper (query quietly)
  (if dbc-database-modified-p
      (if (or (not query) (y-or-n-p (concat "Save database "
					    (database-print-name dbc-database)
					    "? ")))
	  (db-write-database-file (database-file dbc-database) quietly))
    (if (not quietly)
	(db-message "No changes need to be saved%s."
		    (if (and db-mention-filename-on-save-p
			     (not (database-unnamed-p dbc-database)))
			(format " in %s" (database-print-name dbc-database))
		      "")))))

(defun db-save-database-no-buffer (database query quietly)
  ;; This is a hack.  Avoid calling dbf-process-current-record-maybe.
  (let ((dbc-index nil)
	(dbc-database database))
    (db-save-database-helper query quietly)))

(defun db-write-database-file (&optional filename quietly)
  "Save the database to disk in file FILENAME; it becomes the default save file.
Any changes to the current record are processed first.
If FILENAME is not specified, the user is prompted for it.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  ;; Do this before asking for the filename.
  (dbf-process-current-record-maybe t)
  ;; Save even if the database is not modified.
  (if (not filename)
      (setq filename (read-file-name
		      (format "Save database %s into file: "
			      (database-print-name dbc-database)))))
  (if (not (equal filename (database-file dbc-database)))
      (progn
	(database-set-file dbc-database filename)
	;; Rename the buffer
	(rename-buffer (generate-new-buffer-name
			(file-name-nondirectory filename)))))
  (let ((message-filename (if db-mention-filename-on-save-p
			      (concat " to file " filename)
			    "")))
    (db-debug-message "Saving database to file %s..." filename)
    (if (not quietly) (db-message "Saving database%s..." message-filename))
    (write-database-file dbc-database filename)
    ;; (dbc-update-database-modified-p)
    ;; (force-mode-line-update)
    (if (not (or quietly db-io-error-p))
	(db-message "Saving database%s...done" message-filename))))

(defun db-toggle-internal-file-layout (&optional arg)
  "Toggle whether the database will be saved in EDB's internal file layout.
With a nonzero prefix argument, set it to use internal file layout.
With a zero prefix argument, set it not to use internal file layout."
  (interactive "P")
  (database-set-internal-file-layout-p
   dbc-database
   (if arg
       (not (zerop (prefix-numeric-value arg)))
     (not (database-internal-file-layout-p dbc-database)))))

(defun db-toggle-modifiable-p (&optional arg)
  "Toggle whether the database may be modified by the user.
With a nonzero prefix argument, set it modifiable.
With a zero prefix argument, set it non-modifiable."
  (interactive "P")
  (let ((modifiable-p (if arg
			  (not (zerop (prefix-numeric-value arg)))
			(not (database-modifiable-p dbc-database)))))
    (database-set-modifiable-p dbc-database modifiable-p)

    ;; Suppose the record or field is modified and modifiable-p is nil.
    ;; Should I dbf-process-current-record-maybe, or throw away the
    ;; changes, or leave them to be dealt with later?
    (db-in-data-display-buffer
      (if (eq dbf-minor-mode 'edit)
	  (setq buffer-read-only (not modifiable-p))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record selection
;;;

;; This comes before its usages so it can be compiled as a macro
;; properly even when EDB not already loaded.

;; This is the common body of db-next-record and db-jump-to-record.
;; I shouldn't do the stuff in the setq if I end up at the same record
;; as I was at before.  Actually it doesn't hurt since I just did
;; dbf-process-current-record-maybe.
(defmacro dbf-goto-record-internal (&rest record-selector-body)
  (` (progn
       ;; This used to be at the end; I moved it forward and commented out
       ;; a whole lot of code.  Does this hurt anything?  I don't think so...
       ;; In some cases I will be going right back to edit mode, in which
       ;; case I don't want to change the local map and so forth -- then
       ;; I should really be using something closer to the original, and
       ;; worrying about the mode later on.  Maybe have an arg saying which
       ;; mode I want to end up in (or whether I want to change modes).
       ;; *** This line will cause problems if some hook function
       ;; *** deliberately raises an error (like Joe Wells' do).
       (db-view-mode)
       (dbf-process-current-record-maybe nil)
       (,@ record-selector-body)
       ;; (setq dbf-this-record-modified-p nil)
       ;; (setq dbf-this-field-index nil)
       ;; (setq dbf-this-displayspec nil)
       (setq dbf-this-record-original (link-record dbc-link))
       ;; jbw: commented this out
       ;; (setq buffer-read-only nil)
       (display-record (dbf-displayed-record) t)
       ;; (db-view-mode)
       )))
(put 'dbf-goto-record-internal 'lisp-indent-hook 0)
(put 'dbf-goto-record-internal 'edebug-form-spec '(&rest form))

;; Take note!  This opional argument has the opposite effect from all the other
;; optional arguments, which are called IGNORE-HIDING.
;; Perhaps optionally go directly into edit mode (ie, add another argument
;; for that).
(defun db-jump-to-record (arg &optional respect-hiding)
  "Show the database's ARGth record.
Hiding is ignored unless optional argument RESPECT-HIDING is specified."
  (interactive "NJump to record number: ")
  (db-in-data-display-buffer
    (dbf-goto-record-internal
      (db-select-record arg (not respect-hiding))))
  (if (db-summary-buffer-p)
      (dbs-synch-summary-with-format)))

(defun db-first-record (&optional ignore-hiding)
  "Show the database's first record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
	 (db-jump-to-record 1 (not ignore-hiding)))
	((db-summary-buffer-p)
	 ;; Is this right wrt hiding?
	 (goto-char (point-min))
	 (db-jump-to-point))
	(t
	 (error "db-first-record called in wrong context."))))

(defun db-last-record (&optional ignore-hiding)
  "Show the database's last record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
	 (db-jump-to-record (database-no-of-records dbc-database) (not ignore-hiding)))
	((db-summary-buffer-p)
	 (goto-char (point-max))
	 (db-jump-to-point))
	(t
	 (error "db-last-record called in wrong context"))))

(defun db-next-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth next record.
In that record, go to the current field, if any."
  (interactive "p")
  (if (db-summary-buffer-p)
      (dbs-synch-format-with-summary))
  (db-in-data-display-buffer
    (let ((this-field-index dbf-this-field-index))
      (dbf-goto-record-internal
	(db-select-next-record arg ignore-hiding markedp))
      ;; If in edit mode, stay in edit mode in the same field.
      (if (and this-field-index
	       dbf-stay-in-edit-mode-p)
	  (db-move-to-field-exact this-field-index))))
  (if (db-summary-buffer-p)
      (let ((index (dbs-in-data-display-buffer dbc-index)))
	;; This might not be right, depending on what records are summarized.
	(dbs-forward-record (- index dbs-index))
	(dbs-set-index index))))

(defsubst db-previous-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth previous record.
In that record, go to the current field, if any."
  (interactive "p")
  (db-next-record (- arg) ignore-hiding markedp))

(defsubst db-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record arg t))

(defsubst db-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record-ignore-hiding (- arg)))

(defsubst db-next-marked-record (arg)
  "Go to the ARGth next marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-record arg nil t))

(defsubst db-previous-marked-record (arg)
  "Go to the ARGth previous marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-marked-record (- arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from record to record (setting dbc-link)
;;;

;; Finish this.
(defun db-set-auto-edit-mode (arg)
  "Set `db-auto-edit-mode' to ARG."
  (setq db-auto-edit-mode arg)
  (if db-auto-edit-mode
      ))


(defun db-toggle-auto-edit-mode (&optional arg)
  "Change whether cursor movement in view mode causes edit mode to be entered.
See variable `db-auto-edit-mode'.
With a nonzero prefix argument, set  db-auto-edit-mode to t.
With a zero prefix argument, set  db-auto-edit-mode to nil."
  (interactive "P")
  (db-set-auto-edit-mode (if arg
			     (not (zerop (prefix-numeric-value arg)))
			   (not db-auto-edit-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from record to record (setting dbc-link)
;;;

;; These don't display, but they do set dbc-link and dbc-index.

;; Don't forget that when moving off a record, must check whether it has
;; been modified and, if so, call an update function.

;; I think this a bit too big to inline; though I'd like to.
(defun dbc-set-index (index)
  (setq dbc-index index
	dbc-index-fraction
	(let ((frac (format "%s%d/%d"
			    (if (link-markedp dbc-link) "+" "")
			    dbc-index
			    (database-no-of-records dbc-database))))
	  (if (and dbc-hide-p (link-hiddenp dbc-link))
	      (concat "[" frac "]")
	    frac))))

;; This has no checks about whether there are any unhidden records
;; (ie, it can infinite-loop).  I should probably keep a record of the
;; number of unhidden records, for use here and being clever
;; elsewhere.  It would allow me to run short distances backwards instead
;; of far forwards as well.

;;   "Advance to the ARGth following record.  Does no display."
(defun db-select-next-record (arg &optional ignore-hiding markedp)
  (interactive "p")

  (let ((link-index-list
	 (next-link-and-index dbc-database dbc-link dbc-index
			      arg (and dbc-hide-p (not ignore-hiding))
			      markedp
			      (or (eq dbc-wraparound-p t)
				  (and (eq dbc-wraparound-p 'delay)
				       (eq last-command 'db-next-record-failed))))))
    (setq dbc-link (car link-index-list))
    (dbc-set-index (car (cdr link-index-list)))
    (if (cdr (cdr link-index-list))
	(progn
	  (setq this-command 'db-next-record-failed)
	  ;; (beep)
	  (if (< (car (cdr (cdr link-index-list))) 0)
	      (db-message "First record.")
	    (db-message "Last record."))))))

;;   "Advance to the ARGth previous record.  Does no display."
(defsubst db-select-prev-record (arg &optional ignore-hiding)
  (interactive "p")
  (db-select-next-record (- arg) ignore-hiding))

;;   "Select first record.  Does no display.
;; If hiding is in effect, select the first unhidden record, unless
;; optional argument IGNORE-HIDING is non-nil."
(defun db-select-first-record (&optional ignore-hiding)
  (interactive)
  (setq dbc-link (database-first-link dbc-database))
  (if (and dbc-hide-p (link-hiddenp dbc-link) (not ignore-hiding))
      (progn
	(setq dbc-index 1)
	(db-select-next-record 1))
    (progn
      (dbc-set-index 1))))

;;   "Select last record.  Does no display.
;; If hiding is in effect, select the last unhidden record, unless
;; optional argument IGNORE-HIDING is non-nil."
(defsubst db-select-last-record (&optional ignore-hiding)
  (interactive)
  (setq dbc-link (database-first-link dbc-database)
	dbc-index 1)
  (let ((dbc-wraparound t))
    (db-select-next-record -1 ignore-hiding)))

;;   "Select record ARG.  Does no display.
;; If record ARG is hidden, selects the first following non-hidden record,
;; unless optional argument IGNORE-HIDING is non-nil."
(defun db-select-record (arg &optional ignore-hiding)
  (interactive "nRecord number: ")

  (if (database-index-in-range arg dbc-database)
      (progn
	(db-select-first-record ignore-hiding)
	(db-select-next-record (1- arg) ignore-hiding))
    (progn
      (db-debug-message "db-select-record:  %s out of range" arg)
      (beep)
      ;; This should test on the number of unhidden records, if
      ;; hiding is on and IGNORE-HIDING is non-nil.
      (db-message "Record number %d out of range 1..%d"
	       arg (database-no-of-records dbc-database)))))

;; Overall this isn't worth it:  it doesn't take that long to get somewhere, and we don't know how many records are hidden, etc.

;; For very large databases, of course, it certainly pays to do a bit of
;; thinking before acting.  So probably keep this here if not in the above
;; section.

;; This could be even cleverer and possibly search from the current record
;; as well.  I'm not sure that would be worth it for an average speedup of
;; half.  The current scheme, however, doesn't hurt performance much on
;; small databases but helps plenty for going to last record.  Then again,
;; I could just special-case that argument.  The cost of this hack is:
;; floor, division (for computing fraction; I could do this when
;; adding/deleting records, but that would be a pain); addition, mod,
;; subtraction.  Where is the break-even point?

;; (defun db-select-record (arg)
;;   "Select record ARG.  Does no display"
;;   (interactive "nRecord number: ")
;;  
;;   (if (record-no-out-of-range arg)
;;       (progn
;; 	(beep)
;; 	(db-message "Record number %d out of range 1..%d"
;; 		 arg (database-no-of-records dbc-database)))
;;     (progn
;;       (db-first-record)
;;       (setq record-no arg)
;;      
;;       ;; For this many records we'll go backward toward them; for the
;;       ;; others, go forwards toward them.  (I'm assuming that going forward
;;       ;; is 1/3 as costly as going backward.)
;;       ;; Don't need floor because / always returns an integer.
;;       (let ((db-backward-fraction (/ records 4)))
;; 
;; 	(setq offset (- (mod (+ (1- arg) db-backward-fraction) records)
;; 			db-backward-fraction))
;; 
;; 	;; Actually we probably don't want to skip over hidden records.
;; 	;; If it's hidden, permit it to be selected, but warn the user.
;; 	(db-next-record-internal offset t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid field/record movement commands
;;;

(defun db-next-screen-or-record (arg)
  "Go to the ARGth next screenful of this display, or to the ARGth
next record, if this is the last screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the next record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
	 (dbf-next-screen-or-record arg))
	((db-summary-buffer-p)
	 (let ((ddb-window (get-buffer-window dbs-data-display-buffer)))
	   (if ddb-window
	       (progn
		 (in-window ddb-window
		   (dbf-next-screen-or-record arg))
		 (dbs-synch-summary-with-format))
	     (db-next-record arg))))))

(defun dbf-next-screen-or-record (arg)
  (if (eob-visible-p)
      (db-next-record arg)
    (while (and (> arg 0) (not (eob-visible-p)))
      (scroll-up nil)
      (setq arg (1- arg)))))

;; (defun db-next-screen-or-record (arg)
;;   "Go to the ARGth next screenful of this display, or to the ARGth
;; next record, if this is the last screenful of this display.
;; If point is in the summary buffer and the data display buffer is not visible,
;; then move to the next record."
;;   (interactive "p")
;;   (if (buffer-visible-p (db-data-display-buffer))
;;       (db-in-data-display-buffer
;;        (if (eob-visible-p)
;; 	   (db-next-record arg)
;; 	 (while (and (> arg 0) (not (eob-visible-p)))
;; 	   (scroll-up nil)
;; 	   (setq arg (1- arg)))))
;;     (db-next-record arg)))


(defun db-previous-screen-or-record (arg)
  "Go to the ARGth previous screenful of this display, or to the ARGth
previous record, if this is the first screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the previous record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
	 (dbf-previous-screen-or-record arg))
	((db-summary-buffer-p)
	 (let ((ddb-window (get-buffer-window dbs-data-display-buffer)))
	   (if ddb-window
	       (progn
		 (in-window ddb-window
		   (dbf-previous-screen-or-record arg))
		 (dbs-synch-summary-with-format))
	     (db-previous-record arg))))))

(defun dbf-previous-screen-or-record (arg)
  (if (bob-visible-p)
      (db-previous-record arg)
    (progn
      (while (and (> arg 0) (not (bob-visible-p)))
	(scroll-down nil)
	(setq arg (1- arg)))
      (if (bob-visible-p)
	  (goto-char (point-min))))))


;; (defun db-previous-screen-or-record (arg)
;;   "Go to the ARGth previous screenful of this display, or to the ARGth
;; previous record, if this is the first screenful of this display.
;; If point is in the summary buffer and the data display buffer is not visible,
;; then move to the previous record."
;;   (interactive "p")
;;   (if (buffer-visible-p (db-data-display-buffer))
;;       (db-in-data-display-buffer
;;        (if (bob-visible-p)
;; 	   (db-previous-record arg)
;; 	 (progn
;; 	   (while (and (> arg 0) (not (bob-visible-p)))
;; 	     (scroll-down nil)
;; 	     (setq arg (1- arg)))
;; 	   (if (bob-visible-p)
;; 	       (goto-char (point-min))))))
;;     (db-previous-record arg)))

;; These are still too annoying.  Perhaps they should go to the first/last
;; field if they're on the first/last line of the current field rather than
;; forcing point to be at an extremum of the current field.

(defun db-beginning-of-field-or-record ()
  "Move to the beginning of this field; if at its beginning, to the first field."
  (interactive)
  (if (= (point) dbf-this-field-beginning-pos)
      (db-first-field)
    (db-beginning-of-field)))

(defun db-end-of-field-or-record ()
  "Move to the end of this field; if at its end, to the last field."
  (interactive)
  (if (= (point) (dbf-this-field-end-pos))
      (db-last-field)
    (db-end-of-field)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and deleting records
;;;

(defun db-add-record ()
  "Add a new record to the database immediately before the current record."
  (interactive)
  (if (db-summary-buffer-p)
      ;; (switch-to-buffer dbs-data-display-buffer)
      (pop-to-buffer dbs-data-display-buffer)
      )
  (let ((new-record (make-default-record dbc-database)))
    (if db-new-record-function
	(funcall db-new-record-function new-record dbc-database))
    (database-add-record new-record dbc-database dbc-index))
  (setq dbc-index (1+ dbc-index))
  ;; Why doesn't this need to be (dbc-set-modified-p t)?
  (database-set-modified-p dbc-database t)
  ;; Probably unnecessary, as database-add-record has done the trick.
  (dbf-set-summary-out-of-date-p)
  ;; Actually I only want to add one summary line rather than fully synching.
  ;; And I don't ordinarily update this buffer unless I was in it to begin
  ;; with.  (That is, at present changes made in the data display buffer
  ;; aren't automatically reflected in the summary buffer.)
  (if (and dbf-summary-buffer (get-buffer-window dbf-summary-buffer))
      (dbf-in-summary-buffer
	(dbs-synch-summary-with-format)))
  (db-message "Added a new record.")
  (db-previous-record 1)
  ;; Begin editing the new record.  (db-edit-mode) is the wrong way to do this.
  (db-first-field)
  )

(defun db-delete-record (&optional force)
  "Remove the current record from the database.
With a prefix argument, doesn't verify."
  (interactive "P")

  (if (or force (y-or-n-p "Delete this record? "))
      (progn
        (setq dbc-deleted-record (link-record dbc-link))
	(database-delete-link dbc-database dbc-link)
	;; set some links-changed variable, or update the summary directly
	(setq dbc-index (1- dbc-index))
	(if db-delete-record-modifies-database-p
	    (dbc-set-database-modified-p t))
	(db-message "Record deleted.")
	(db-next-record 1))))

;; This should be put with the rest of the variables.
(defvar dbc-deleted-record nil
  "The record most recently deleted by \\[db-delete-record].")

(defun db-yank-record (endp)
  "Insert, and make current, the most recently deleted record.
The deleted record is inserted before the current record.
With prefix argument ENDP, insert at end of database and don't select it."
  (interactive "P")
  (if (not dbc-deleted-record)
      (error "No deleted record to yank."))
  (db-in-data-display-buffer
    ;; This is inelegant in the extreme, but the interaction of
    ;; dbc-set-index and db-{previous-next}-record and
    ;; database-add-record mystifies me.  --karl@owl.hq.ileaf.com (Karl Berry)
    (if endp
	(progn
	  (database-add-record dbc-deleted-record dbc-database)
	  ;; We go back to the current record below.
	  (db-next-record 1))
      (progn
	(database-add-record dbc-deleted-record dbc-database dbc-index)
	(dbc-set-index (1+ dbc-index)))
    (db-previous-record 1)
    (dbc-set-database-modified-p t)))
  (if (db-summary-buffer-p)
      (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record yanked."))

(defun db-copy-record (&optional arg)
  "Insert a copy of the current record in the database immediately after it.
The second of the two records is made the current record.
With a prefix argument, inserts that many copies."
  (interactive "p")
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (while (> arg 0)
      (database-add-record (copy-record (link-record dbc-link))
			   dbc-database dbc-index)
      (dbc-set-index (1+ dbc-index))
      (setq arg (1- arg))))
  (if (db-summary-buffer-p)
      (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record copied."))

(deflocalvar db-for-output nil
  "Default database to which to output records.")

(defun db-output-record-to-db (database)
  "Copy (output) the current record to DATABASE.
DATABASE must be read in and compatible with the current database."
  ;; Make a list of databases compatible with this one.
  (interactive
   (list
    (let ((db-alist (delq nil (mapcar
			       (function (lambda (database)
				  (if (and (not (eq database dbc-database))
					   (databases-compatible database
								 dbc-database))
				      (cons
				       (or (database-print-name database)
					   (database-file database))
				       database))))
			       db-databases))))
      (if db-alist
	  (cdr (assoc (completing-read
		       "Output record to which database (? for choices): "
		       db-alist nil t db-for-output)
		      db-alist))
	(progn
	  (error "No compatible databases are currently read in!")
	  nil)))))
  (if database
      (progn
	(if (db-summary-buffer-p)
	    (dbs-synch-format-with-summary))
	(db-in-data-display-buffer
	  (database-add-record (link-record dbc-link) database)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting
;;;

;;; How to deal with:
;; With a prefix argument, put hidden records at the end; otherwise they are
;; sorted like all other records.
;;; How to get a default from the database?  Maybe only permit the
;;; database's value to be used.  (That souds good.)

(defun db-sort (&optional dont-confirm)
  "Sort the database.  With a prefix argument, don't confirm the sort order."
  (interactive "P")

  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (if dont-confirm
	(progn
	  (database-sort dbc-database)
	  (dbf-finished-sorting))
      (database-sort-interface dbc-database))))

;; Call this after sorting the database.
(defun dbf-finished-sorting ()

  ;; Need to recompute the current record's index.
  ;; [This is the sort of thing that perhaps should be done for each format
  ;; that accesses the database.]
  (dbc-compute-index)

  (delete-windows-on (dbf-summary-buffer))

  ;; Force summary refresh.
  ;; Set the summary buffer out of order but don't set the
  ;; must-recompute-something bit.  There ought to be special variables
  ;; for this rather than using dbs-no-of-records.
  (dbf-in-summary-buffer (setq dbs-no-of-records -1))

  ;; The index shown in the mode line is correct, but the database may have
  ;; been marked as modified, and that change hasn't made it to the mode line.
  (dbc-update-database-modified-p)
  (force-mode-line-update)
  )

;; With the macro expanded, this is too big to inline.
(defun dbc-compute-index ()
  (let (this-index)
    (maplinks-macro
      (if (eq maplinks-link dbc-link)
	  (progn
	    (setq this-index maplinks-index)
	    (maplinks-break)))
      dbc-database)
    (dbc-set-index this-index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

;; Should have a better way of adding a field.

(defun db-field-query-replace ()
  "Replace some instances of a value in this field with some other value.
Confirms before each replacement."
  (interactive)
  (if (not dbf-this-field-index)
      (error "Call this when on a field."))
  (let* ((old-dbc-index dbc-index)
	 (displayspec dbf-this-displayspec)
	 (fsno dbf-this-field-index)
	 (record-index (displayspec-record-index dbf-this-displayspec))
	 (fieldname (fieldnumber->fieldname record-index dbc-database))
	 (order-function (recordfieldspec-order-function
			  (database-recordfieldspec dbc-database record-index)))
	 original-value
	 ov-printed
	 replacement-value
	 rv-printed)
    (dbf-process-current-record-maybe nil)
    (setq original-value (display->actual-call
			  (displayspec-display->actual displayspec)
			  (read-string "Query replace: ")
			  ;; No previous value or record.
			  nil nil
			  record-index))
    (record-check-constraint original-value nil record-index dbc-database)
    ;; Must keep in mind that this is not necessarily what the user typed.
    (setq ov-printed (actual->display-call
		      (displayspec-actual->display displayspec)
		      original-value
		      nil
		      record-index))

    (setq replacement-value (display->actual-call
			     (displayspec-display->actual displayspec)
			     (read-string (format "Query replace %s with: "
						  ov-printed))
			     nil nil record-index))
    (record-check-constraint replacement-value nil record-index dbc-database)
    (setq rv-printed (actual->display-call
		      (displayspec-actual->display displayspec)
		      replacement-value
		      nil
		      record-index))

    (maprecords (function
		 (lambda (record)
		   (if (= 0 (funcall order-function
				     original-value
				     (aref record record-index)))
		       (progn
			 (display-record record t)
			 ;; I should put the cursor on the field in question
			 ;; and not name it in the question.
			 (db-skip-string-forward (aref dbf-inter-field-text 0))
			 (setq dbf-this-field-index 0)
			 (db-next-field-internal fsno)
			 ;; *** must handle case where replace strings
			 ;; *** don't fit in minibuffer.
			 ;; *** Maybe use db-best-fit-message somehow?
			 (if (y-or-n-p (format "Replace `%s' with `%s'? "
					       fieldname ov-printed rv-printed))
			     ;; It's a bit extreme that this errs if the value
			     ;; fails to meet the constraint.
			     (record-set-field-from-index
			      record record-index replacement-value
			      dbc-database))))))
		dbc-database)
    (db-message "Replacement done.")
    (db-jump-to-record old-dbc-index t)))

(defun db-accept-record ()
  "Install the current record in the database; make any changes permanent."
  (interactive)
  (dbf-process-current-record-maybe t))
(fset 'db-commit-record 'db-accept-record)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching
;;;

;; This must get significantly faster; right now it's awful.

(defun db-search-field (pattern &optional mark)
  "Search for occurrences of PATTERN in the current field of any record.
Finds the first match after the current record; wraps around automatically.
With prefix argument, marks all matches in addition to going to the first one.
If hiding is in effect, hidden records are ignored."
;;   (interactive (list (read-string "Pattern to search for in current field: "
;; 				  (aref dbf-field-search-defaults
;; 					dbf-this-field-index))
;; 		     current-prefix-arg))
  (interactive
   (list (let ((fieldname (dbf-this-field-name)))
	   (read-string
;;; Which type of defaulting is better is a matter of debate.
;; 	    (if (if dbf-this-field-index
;; 		    (aref dbf-field-search-defaults dbf-this-field-index)
;; 		  (error "Only call db-search-field when on a field in a data display buffer."))
;; 		(format "Search in %s for [%s]: "
;; 			fieldname
;; 			(aref dbf-field-search-defaults dbf-this-field-index))
;; 	      (format "Search in %s for: " fieldname))
	    (format "Search in %s for: " fieldname)
	    (if dbf-this-field-index
		(aref dbf-field-search-defaults dbf-this-field-index)
	      (error "Only call db-search-field when on a field in a data display buffer."))))
	 current-prefix-arg))
;;   (if (equal "" pattern)
;;       (if (aref dbf-field-search-defaults dbf-this-field-index)
;; 	  (setq pattern (aref dbf-field-search-defaults dbf-this-field-index))
;; 	(error "You didn't enter a pattern, and there was no default.")))
  (if (equal "" pattern)
      (error "You didn't enter a pattern for which to search."))

  (let* ((pat (db-parse-match-pattern pattern dbf-this-displayspec))
	 (pat-display-rep (db-print-match-pattern pat dbf-this-displayspec))
	 (this-record-index (displayspec-record-index dbf-this-displayspec))
	 (recordfieldspec (database-recordfieldspec dbc-database this-record-index))
	 (this-field-index dbf-this-field-index)
	 this-field
	 (fieldname (dbf-this-field-name))
	 ;; success is t if we've already found some match.
	 ;; The idea is that we'll move to success-link when we're done with
	 ;; the search; if success is nil then we're looking for such a link.
	 ;; This is either because we haven't found one or because we have
	 ;; only found one before dbc-link in the database.
	 success success-link success-index
	 (matches 0))
    (aset dbf-field-search-defaults dbf-this-field-index pat-display-rep)
    (if mark
	(db-message "Marking all %s in %s..." pat-display-rep fieldname)
      (db-message "Searching in %s for %s..." fieldname pat-display-rep))
    (maplinks-macro
      (progn
	(if (or mark (not success))
	    (progn
	      (setq this-field (aref (link-record maplinks-link)
				     this-record-index))
	      (db-debug-message "db-search-field:  this-field = %s" this-field)
	      ;; When the pattern isn't a combination, this is slower than
	      ;; a hard-coded "just use recordfieldspec-match-function"; but
	      ;; I'm not sure that speed would be worth the extra complexity.
	      (if (db-match pat this-field recordfieldspec)
		  (progn
		    (if (not success)
			(setq success-link maplinks-link
			      success-index maplinks-index
			      success t))
		    (if mark
			(progn (setq matches (1+ matches))
			       (link-set-markedp maplinks-link t)))))))
	;; We're looking for a match in some record besides the displayed
	;; one and, preferrably, after it.  This permits the first success
	;; succeeding the current record to overwrite the first success
	;; preceding the current record.  This means that searches can't
	;; abort after a success, since that success might be before the
	;; current record.  Perhaps I should have a version of maplinks
	;; that starts from the current record, for efficiency in
	;; searching.
	(if (eq maplinks-link dbc-link)
	    (setq success nil)))
      dbc-database
      dbc-hide-p)
    (if success-index
	(if (eq dbc-link success-link)
	    (db-message "This record has the only match for %s." pat-display-rep)
	  (progn
	    ;; This takes care of committing any changes to the current record.
	    (dbf-goto-record-internal
	      (setq dbc-link success-link)
	      (dbc-set-index success-index))
	    (db-move-to-field-exact this-field-index)
	    (if mark
		;; *** update each summary item as it is marked???
		(progn (dbf-set-summary-out-of-date-p)
		       (db-message "Searching for %s...marked %s matches."
				pat-display-rep matches))
	      (db-message "Searching for %s...found." pat-display-rep))))
      (db-message "Couldn't find a match in %s for %s."
	       fieldname pat-display-rep))))


(if nil
;; This has lots of problems.  I may want to rethink a lot of the search
;; mechanism before doing this in earnest.
(defun db-search (pattern &optional mark)
  "Search for occurrences of PATTERN in any field of any record.
Finds the first match after the current record; wraps around automatically.
With prefix argument, marks all matches in addition to going to the first one.
If hiding is in effect, hidden records are ignored."
  (interactive
   (list (read-string "Search in all fields for: "
		      (aref dbf-field-search-defaults dbf-displayspecs-length))
	 current-prefix-arg))
  (if (equal "" pattern)
      (error "You didn't enter a pattern for which to search."))


  ;; This was lifted from db-search-field.  See there for comments.
  (let* ((pats (vconcat
		(mapcar (function (lambda (displayspec)
				    (db-parse-match-pattern pattern displayspec)))
			dbf-displayspecs)))
	 (pat-display-rep pattern)
	 (record-indexes (vconcat
			  (mapcar (function (lambda (displayspec)
					      (displayspec-record-index displayspec)))
				  dbf-displayspecs)))
	 (recordfieldspecs (vconcat
			    (mapcar (function (lambda (record-index)
						(database-recordfieldspec
						 dbc-database record-index)))
				    record-indexes)))
	 this-record
	 this-field
	 success success-link success-index success-field-index
	 (matches 0))
    (aset dbf-field-search-defaults dbf-displayspecs-length pat-display-rep)
    (if mark
	(db-message "Marking all %s..." pat-display-rep)
      (db-message "Searching for %s..." pat-display-rep))
    (maplinks-macro
      (progn
	(if (or mark (not success))
	    (progn
	      (setq this-record (link-record maplinks-link))
	      (db-debug-message "db-search:  this-record = #%d, %s"
				maplinks-index this-record)

	      (mapfields-macro
	       (progn
		 (db-debug-message "db-search:  this-field = #%d, %s" this-field-index this-field)
		 (if (db-match (aref pats this-field-index)
			       this-field
			       (aref recordfieldspecs this-field-index))
		     (progn
		       (if (not success)
			   (setq success-link maplinks-link
				 success-index maplinks-index
				 success t
				 success-field-index this-field-index
				 this-field-index field-index-max))
		       (if mark
			   (progn (setq matches (1+ matches))
				  (link-set-markedp maplinks-link t))))))
	       this-record dbc-database)))
	(if (eq maplinks-link dbc-link)
	    (setq success nil)))
      dbc-database
      dbc-hide-p)
    (if success-index
	(if (eq dbc-link success-link)
	    (db-message "This record has the only match for %s." pat-display-rep)
	  (progn
	    ;; This takes care of committing any changes to the current record.
	    (dbf-goto-record-internal
	      (setq dbc-link success-link)
	      (dbc-set-index success-index))
	    (db-move-to-field-exact success-field-index)
	    (if mark
		;; *** update each summary item as it is marked???
		(progn (dbf-set-summary-out-of-date-p)
		       (db-message "Searching for %s...marked %s matches."
				pat-display-rep matches))
	      (db-message "Searching for %s...found." pat-display-rep))))
      (db-message "Couldn't find any match for %s."
		  pat-display-rep))))
)

(defun db-search ()
  "`db-search' is not yet implemented; use `db-search-field' instead.
In a future version of EDB, `db-search' will permit searching on all fields
of a record simultaneously."
  (interactive)
  (error "db-search is unimplemented; use db-search-field instead (M-s from Edit mode).")
  )


;; In Emacs 19, this is the wrong way to do this!  I want an
;; after-command-hook, like Rmail has.

;; These should perhaps just be wrappers of some sort.
(defun db-isearch-forward ()
  "Like isearch-forward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-forward)
  (db-jump-to-point))

(defun db-isearch-backward ()
  "Like isearch-backward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-backward)
  (db-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding
;;;

;; These first two aren't very convenient for calling non-interactively.

;; Should update the summary.
(defun db-mark-record (&optional arg)
  "Toggle whether the current record is marked.
With a nonzero prefix argument, set it to be marked.
With a zero prefix argument, set it to be unmarked."
  (interactive "P")
  (db-in-data-display-buffer
    (link-set-markedp dbc-link (if arg
				   (not (zerop (prefix-numeric-value arg)))
				 (not (link-markedp dbc-link))))
    ;; (dbf-set-summary-out-of-date-p)
    (dbf-update-summary-item dbc-index dbc-link)
    (dbc-set-index dbc-index)		; sets dbc-index-fraction
    (force-mode-line-update)))

(defun db-hide-record (&optional arg)
  "Change whether the current record is hidden.
With a nonzero prefix argument, set it to be hidden.
With a zero prefix argument, set it to be unhidden."
  (interactive "P")
  (db-in-data-display-buffer
    (link-set-hiddenp dbc-link (if arg
				    (not (zerop (prefix-numeric-value arg)))
				  (not (link-hiddenp dbc-link))))
    ;; (dbf-set-summary-out-of-date-p)
    (if dbc-hide-p
	(dbf-update-summary-item dbc-index dbc-link)
      ;; Automatically turn on the effect of hiding.  I think this is
      ;; most intuitive for the user.  And now the user doesn't have to
      ;; remember what the command for enabling hiding is.
      (dbc-set-hide-p t)
      ;; Update all marks, since potentially all have to be displayed now.
      (dbf-update-summary-marks))
    (dbc-set-index dbc-index)		; sets dbc-index-fraction
    (force-mode-line-update)))

;; Perhaps PREDICATE should take the database as an argument as well.
(defun db-hide-records (predicate)
  "Evaluate PREDICATE on each record in the database in turn.
PREDICATE takes one argument, a record, and returns a non-nil value if
the record is to be hidden."
  (interactive)
  (dbc-set-hide-p t)
  (db-in-data-display-buffer
    (maplinks '(lambda (link)
		 (link-set-hiddenp link
				   (funcall predicate
					    (link-record link))))
	      dbc-database)))

(defun db-mark-searched-records ()
  "Mark all records found in search STRING of FIELD."
  (interactive)
  (setq current-prefix-arg "1")
  (call-interactively 'db-search-field "1")
  (save-window-excursion
    (switch-to-buffer-other-window (db-summary-buffer))
    (if (db-summary-buffer-p)
	(dbs-synch-format-with-summary))))


;; These could remember what the marks used to be...  That would require
;; another slot in the link, or much more complicated manipulation of the
;; current hiddenp slot, and it doesn't sound entirely feasible, or
;; worthwhile for that matter.  But I could get back the old value of
;; db-hide-p.

(defun db-hide-unmarked-records ()
  "Hide all unmarked records.  Also clears all mark bits and sets `dbc-hide-p'."
  (interactive)
  (db-in-data-display-buffer
    (maplinks-macro
     (if (link-markedp maplinks-link)
	 (link-set-markedp maplinks-link nil)
       (link-set-hiddenp maplinks-link t))
     dbc-database
     t)
    (dbc-set-hide-p t)
    ;; (dbf-set-summary-out-of-date-p)
    (dbf-update-summary-marks)
    ;; *** Do some redisplay here as well, especially of the summary buffer.
    ))

(defun db-mark-unhidden-records ()
  "Mark all unhidden records.  Also clears all hide bits."
  (interactive)
  (db-in-data-display-buffer
    (maplinks-macro
     (if (link-hiddenp maplinks-link)
	 (link-set-hiddenp maplinks-link nil)
       (link-set-markedp maplinks-link t))
     dbc-database)
    (dbc-set-hide-p t)
    ;; (dbf-set-summary-out-of-date-p)
    (dbf-update-summary-marks)
    ;; *** Do some redisplay here as well.
    ))

(defun db-unhide-all ()
  "Clear the hide bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (maplinks-macro
     (link-set-hiddenp maplinks-link nil)
     dbc-database)
    (dbc-set-index dbc-index)		; sets dbc-index-fraction
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-unmark-all ()
  "Clear the mark bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (maplinks-macro
     (link-set-markedp maplinks-link nil)
     dbc-database)
    (dbc-set-index dbc-index)		; sets dbc-index-fraction
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-hiding-toggle (&optional arg)
  "Change whether hiding is in effect.
With a nonzero prefix argument, turn hiding on.
With a zero prefix argument, turn hiding off.

This does not change the current hide-function, and a hide bit is always
computed for each record, but hide bits have no effect on any operations
if hiding is not in effect."
  (interactive "P")
  (db-in-data-display-buffer
    (dbc-set-hide-p (if arg
			(not (zerop (prefix-numeric-value arg)))
		      (not dbc-hide-p)))
    ;; Must refill summary buffer whenever displayed set of records
    ;; changes, including when switching to no hiding and showing hidden
    ;; records.
    (cond
     ((not dbf-summary-show-hidden-records-p)
      ;; If the hidden records weren't being shown, the records that
      ;; should be displayed in the summary buffer just changed.  We need
      ;; to refill the summary.
      (dbf-fill-summary-buffer-and-move-to-proper-record))
     (t
      (dbf-update-summary-marks)
      ;;(if dbc-hide-p
      ;;    (dbf-update-summary-marks)
      ;;  ;; Is there any real speed advantage to this:
      ;;  (dbf-in-summary-buffer
      ;;   (let ((buffer-read-only nil))
      ;;     (goto-char (point-min))
      ;;     (replace-regexp-noninteractive "^\\(.\\)\\[" "\\1 ")
      ;;     (dbs-move-to-proper-record)
      ;;     (set-buffer-modified-p nil))))
      ))
    (force-mode-line-update)
    (db-message "Hiding is now %sin effect." (if dbc-hide-p "" "not "))))

;; Perhaps rename this db-hiding-set-criteria.
(defun db-hiding-set ()
  "Set the criteria for automatically determining whether to hide a record.
This isn't implemented yet."
  (interactive)
  (error "db-hiding-set is not yet implemented.")
  )

(defun db-toggle-show-hidden-records (&optional arg)
  "Toggle whether hidden records are shown in the summary.
With a nonzero prefix argument, show hidden records in the summary.
With a zero prefix argument, don't show hidden records in the summary."
  (interactive "P")
  (db-in-data-display-buffer
    (setq dbf-summary-show-hidden-records-p
	  (if arg
	      (not (zerop (prefix-numeric-value arg)))
	    (not dbf-summary-show-hidden-records-p)))
    (if dbf-summary-show-hidden-records-p
	;; If we weren't showing hidden records, we might as well start from
	;; scratch in filling the summary buffer.
	(dbf-fill-summary-buffer-and-move-to-proper-record)
      (dbf-in-summary-buffer
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (delete-matching-lines "^.\\[")
	  (dbs-move-to-proper-record))))
    (if dbf-summary-show-hidden-records-p
	(db-message "Hidden records will now be shown.")
      (db-message "Hidden records will not now be shown."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reporting
;;;

;; Could make an alist so I don't have to read in the report name every time.
(defun db-report (report-filename &optional markedp)
  "Create a report according to REPORT-FILENAME.
Prefix argument MARKEDP, if non-nil, means report on only marked records.
If hiding is in effect, hidden records are not reported upon.
When called interactively, prompts for REPORT-FILENAME."
  (interactive "fReport format file: \nP")
  (dbf-process-current-record-maybe t)
  (let ((database dbc-database)
	report-format report-function)
    (save-window-excursion
      (set-buffer (get-buffer-create " *Database work buffer*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-file-contents report-filename)
      (setq report-format (buffer-substring (point-min) (point-max))))
    (let ((lasfl (format->lines-and-stringform-list
		  report-format dbc-database nil nil t)))
      (setq report-function
	    (` (lambda (formatted-record)
		 (insert (,@ (cdr lasfl)))))))
    (let ((hide-p dbc-hide-p))
      (switch-to-buffer (get-buffer-create "*Database Report*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (maplinks-macro
       (if (or (not markedp) (link-markedp maplinks-link))
	   (funcall report-function (link-record maplinks-link)))
       database
       hide-p)
    (goto-char (point-min)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc.
;;;

;; Probably get rid of these.

(defsubst data-display-buffer-database (data-display-buffer)
  (in-buffer data-display-buffer
     dbc-database))

(defsubst display-current-record (recompute)
  (display-record (dbf-displayed-record) recompute))

;;; db-interfa.el ends here
