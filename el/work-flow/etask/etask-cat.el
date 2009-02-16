;;; etask-cat.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask-cat.el,v 1.38 2004/10/31 22:27:57 rene Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


;;; Commentary:

;; This software component implements the EtaskMode category management
;; features.


;; _________________


;;; Code:

(defconst etask-category-projectid 1000
  "Project category ID.")

(defconst etask-category-todoid 1001
  "ToDo category ID.")

(defconst etask-category-eventid 1002
  "Event category ID.")

(defconst etask-category-currprojectid 1004
  "Current project category ID.")

(defconst etask-category-currtodoid 1005
  "Current ToDo category ID.")

(defconst etask-category-curreventid 1006
  "Current event category ID.")

(defvar etask-cat-alist nil "")


;;; Interactive

(defun etask-cat-show-elements(&optional level)
  "Show ELEMENTS of ITEM and the status of the current element in
Gantt chart.  If optional LEVEL is non-nil, show only ELEMENTS with
levels not greater than LEVEL.  One means top level, 2 is the first
subelement level and so on."
  (interactive "P")
  (let* ((curritem (etask-cat-get-current-item))
         (catid (car curritem))
         (item (car (cdr curritem)))
         (line (etask-current-line))
         (helpp
          (if (and (interactive-p)
                   (etask-get-control-item etask-ctrl-onlinehelp 'main))
              (progn
                (etask-set-control-item etask-ctrl-onlinehelp nil 'main)
                nil)
            (etask-get-control-item etask-ctrl-onlinehelp 'main))))
    (etask-states-update)
    (setq buffer-read-only nil)
    (erase-buffer)

    (if (etask-cat-civalid-p catid item)
        (let* ((maxnamelen )
               (maxbarlen )
               (maxwidth (+ 1           ;delimiter
                            (etask-state-get etask-stateid-maxtasklen)
                            (etask-state-get etask-stateid-maxbarlen)))
               (helpkeys (etask-generate-helpshortcut helpp))
               (executekey (concat
                            (etask-lang-msg 782 etask-language)
                            ": x"))
               (repeatkey (concat
                            (etask-lang-msg 783 etask-language)
                            ": r"))
               (currlevel (etask-state-get etask-stateid-level))
               msg daylen)
          (setq msg (etask-generate-item-info))
          (setq daylen (etask-get-daylen))

          (if (and (natnump level) (> level 0))
              (etask-state-set etask-stateid-level level)
            (if (natnump currlevel)
                (setq level currlevel)
              (setq level 1)))
          (etask-generate-chart 
           (etask-cat-get-current-elementlist) 0 (1- level))
          (etask-insert-datelabels daylen)
          (insert "\n\n")
          (insert (concat
                   msg
                   (make-string
                    (- maxwidth (length msg) (length helpkeys)) ? )
                   helpkeys
                   "\n"
                   (when helpp
                     (concat
                      (make-string
                       (- maxwidth 
                          (length executekey)
                          (length repeatkey)
                          2) ? )
                      repeatkey
                      "  "
                      executekey))
                   "\n"))
          (insert (etask-apply-face 
                   etask-statusheader 'etask-face-statusheader-onscreen))
          (etask-fit-window)
          (etask-goto-line line)
          (etask-cat-show-elementstatus
           (etask-cat-get-current-elementindex)))
      (erase-buffer)
      (setq buffer-read-only t))))

(defun etask-cat-show-elements-with-help()
  "Show ELEMENTS of ITEM in Gantt chart and display help information
below status information."
  (interactive)
  (etask-set-control-item etask-ctrl-onlinehelp t 'main)
  (etask-cat-show-elements))

(defun etask-cat-sort-elements()
  "Sort elements of current item."
  (interactive)
  (when (etask-interactive-preconditions)
    (let ((catid (car (etask-cat-get-current-item))))
      (when (= catid etask-category-todoid)
        (let* ((elements (etask-cat-get-current-elementlist))
               (prio-date (etask-lang-msg 650 etask-language))
               (date-prio (etask-lang-msg 655 etask-language))
               (s (etask-get-chosen-list-element
                   (list prio-date date-prio)
                   (etask-lang-msg 640 etask-language)))
               sortedp)
          (cond ((string= s prio-date)
                 (setq sortedp t)
                 (setq elements
                       (sort
                        elements
                        'etask-cat-sortfunc-priority-taskendtime)))
                ((string= s date-prio)
                 (setq sortedp t)
                 (setq elements
                       (sort
                        elements
                        'etask-cat-sortfunc-taskendtime-priority)))
                (t
                 ()))
          (when sortedp
            (etask-write-elements elements nil nil 'erase)
            (etask-cat-show-elements)))))))

(defun etask-cat-edit-element()
  "Edit the current element."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((currix (etask-cat-get-current-elementindex))
           (element (etask-cat-get-current-element))
           (elementtodelete element))
      (when (listp element)
        (let ((catid (car (etask-cat-get-current-item)))
              (item (car (cdr (etask-cat-get-current-item)))))
          (when (etask-cat-civalid-p catid item)
            (let ((name (etask-get-taskname 
                         (etask-db-get element etask-db-attr-taskname))))
                  
                  ;; Element data update: 
                  ;;  1. Collect the data
                  ;;  2. Delete the old element
                  ;;  3. Insert the updated element at the old position

                  ;; When updating the last element (in terms of
                  ;; occurrence) the current cursor position changes
                  ;; after the deletion and is now one line above.
                  ;; Therefore, when inserting the updated data we
                  ;; must not insert at the current line but 1 line
                  ;; below.  `insertpos' stores the position that is
                  ;; current before the delete operation.

              (setq element
                    (etask-db-set element etask-db-attr-taskname name))
              (cond ((= catid etask-category-projectid)
                     (setq element (etask-proj-edit-element element)))
                    ((= catid etask-category-todoid)
                     (setq element (etask-todo-edit-element element)))
                    ((= catid etask-category-eventid)
                     (setq element (etask-ev-edit-element element)))
                    (t
                     ()))

              (etask-cat-delete-element currix)
              (etask-cat-store-element element currix)

              ;; move notes
              (when (etask-cat-elementnotes-p catid item elementtodelete)
                (let ((oldfname
                       (etask-cat-get-elementnotes-filename
                        catid item elementtodelete))
                      (newfname
                       (etask-cat-get-elementnotes-filename
                        catid item element)))
                  (when (file-readable-p oldfname)
                    (copy-file oldfname newfname 1)
                    (delete-file oldfname))))

              (etask-cat-goto-elementindex currix)
              (etask-cat-show-elements))))))))

(defun etask-cat-link-elements()
  "Link marked elements according to lead or lag time values entered
by the user via minibuffer.  A negative number it is interpreted as
lead time.

LEAD TIME: successor element overlaps with its predecessor element

LAG TIME: waiting period after an element before the next linked
element can begin"
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let ((marked (etask-cat-get-marked-elements)))
      (when (listp marked)
        (let* ((curritem (etask-cat-get-current-item))
               (catid (car curritem))
               (item (car (cdr curritem))))
          (when (etask-cat-civalid-p catid item)
            (cond ((= catid etask-category-projectid)
                   (etask-proj-link-elements marked))
                  ((= catid etask-category-todoid)
                   (etask-todo-link-elements marked))
                  ((= catid etask-category-eventid)
                   (etask-ev-link-elements marked))
                  (t
                   ()))))))))

(defun etask-cat-elementnotes()
  "Add notes to the current element."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (save-current-buffer
      (let ((filename (etask-cat-get-elementnotes-filename)))
        (when filename 
          (set-buffer (find-file filename)))))))

(defun etask-cat-admin-categories(&optional prompt)
  "Add, delete, or rename a project."
  (interactive)
  (etask-states-save)
  (let* ((catname
          (etask-get-chosen-list-element
           (etask-cat-get-catnamelist)
           (concat prompt
                   (etask-lang-msg 380 etask-language))))
         (catid (etask-cat-get-catid catname))
         (itemnum (length (etask-cat-get-itemlist catid)))
         (promptforname 
          (concat 
           (etask-cat-get-catitemname catid)
           " [ a-zA-Z0-9_?!-]: "))
         (prompt (concat
                  (if (= itemnum 0)
                      promptforname
                    (concat
                     (etask-cat-get-catitemname catid)
                     "   (1) "
                     (etask-lang-msg 300 etask-language) ;new
                     "   (2) "
                     (etask-lang-msg 301 etask-language) ;delete
                     "   (3) "
                     (etask-lang-msg 299 etask-language) ;rename
                     "   ? ")))))
    (when (etask-state-get etask-stateid-archive)
      (etask-cat-toggle-archive))
    (let ((cmd (if (= itemnum 0)    ;no elements, insert without query
                   1
                 (string-to-number
                  (etask-read prompt 
                              (lambda (x) 
                                (and (>= (string-to-number x) 1) 
                                     (<= (string-to-number x) 3))))))))
      (cond ((= cmd 1)
             (let ((name
                    (etask-read promptforname
                                (lambda (x) 
                                  (string-match "^[ a-zA-Z0-9_?!-]+$" x)))))
               (if (etask-cat-insert-item catid name)
                   (progn
                     (etask-cat-set-current-item catid name)
                     (if (etask-cat-is-current-elementlist-empty-p)
                         (etask-cat-insert-elements)
                       (etask-cat-show-elements)))
                 (error "%s" (etask-lang-msg 1006 etask-language)))))
            ((= cmd 2)
             (let ((itemlist (etask-cat-get-itemlist catid)))
               (setq prompt 
                     (concat 
                      (etask-get-list-prompt itemlist)
                      "  ? "))
               (if (etask-cat-delete-item-p
                    catid
                    (nth
                     (1- (string-to-number
                          (etask-read 
                           prompt 
                           (lambda (x) 
                             (and (>= (string-to-number x) 1) 
                                  (<= (string-to-number x) itemnum))))))
                     itemlist))
                   (progn
                     (etask-cat-show-elements)
                     (when (not (car (cdr (etask-cat-get-current-item))))
                       (etask-cat-switch-item)))
                 (error "%s" (concat
                              (etask-cat-get-catitemname catid)
                              (etask-lang-msg 1007 etask-language))))))
            ((= cmd 3)
             (save-some-buffers)        ;notes not saved get lost
             (let ((itemlist (etask-cat-get-itemlist catid))
                   (input))
               (setq prompt 
                     (concat 
                      (etask-get-list-prompt itemlist)
                      "  ? "))
               (setq input (string-to-number
                            (etask-read 
                             prompt 
                             (lambda (x) 
                               (and (>= (string-to-number x) 1) 
                                    (<= (string-to-number x) itemnum))))))

               (if (etask-cat-rename-item-p 
                    catid 
                    (nth (1- input) itemlist)
                    (etask-read promptforname
                                (lambda (x) 
                                  (string-match "^[ a-zA-Z0-9_?!-]+$" x))))
                   (etask-cat-show-elements)
                 (error "%s" (concat
                              (etask-cat-get-catitemname catid)
                              (etask-lang-msg 1011 etask-language))))))
            (t
             ())))))

(defun etask-cat-switch-item(&optional prompt)
  "Switch to another category item."
  (interactive)
  (etask-states-save)
  (let* ((catname
          (etask-get-chosen-list-element 
           (etask-cat-get-active-categorynames)
           (concat prompt
                   (etask-lang-msg 380 etask-language))))
         (catid (etask-cat-get-catid catname))
         (itemlist
          (etask-cat-get-itemlist catid))
         (item
          (when itemlist
            (etask-get-chosen-list-element 
             itemlist
             (etask-lang-msg 380 etask-language))))
         (len 
          (length itemlist)))
    (if (and (stringp item) (> len 0))
        (progn
          (etask-cat-set-current-item catid item)
          (if (etask-state-get etask-stateid-archive)
              (etask-cat-toggle-archive)
            (etask-cat-show-elements))
          (when (etask-cat-is-current-elementlist-empty-p)
            (etask-cat-insert-elements)))
      (error "%s" (etask-lang-msg 1005 etask-language)))))

(defun etask-cat-toggle-archive()
  "Switch to current archive or, if archive is already displayed,
switch back to current item."
  (interactive)
  (let* ((archivep (etask-state-get etask-stateid-archive))
         (curritem (etask-cat-get-current-item))
         (catid (car curritem))
         (item (car (cdr curritem))))
    (if archivep
        (progn
          (etask-state-clear etask-stateid-archive)
          (when (string-match "\\." item)
            (setq item (substring item 0 (match-beginning 0))))) 
      (etask-state-set etask-stateid-archive "archive")
      (setq item (concat item ".archive")))
    (etask-cat-set-current-item catid item)
    (etask-cat-show-elements)))
        
(defun etask-cat-set-planned-effort()
  "Set planned effort for current element according to values
collected via minibuffer.  If project category is current and if there
are marked tasks, a new planned effort is set for all marked tasks."
  (interactive)
  (when (and (etask-interactive-preconditions)
             (not (etask-state-get etask-stateid-archive)))
    (let* ((curritem (etask-cat-get-current-item))
           (catid (car curritem))
           (item (car (cdr curritem)))
           (element (etask-cat-get-current-element))
           (peffort))
      (cond ((= catid etask-category-todoid)
             (setq peffort (etask-todo-get-peffort element))
             (setq element
                   (etask-db-set element etask-db-attr-peffort peffort))
             (setq element
                   (etask-db-set
                    element etask-db-attr-taskbegin
                    (etask-todo-get-begin
                     (list
                      (etask-db-get 
                       element etask-db-attr-taskend)
                      (etask-db-get 
                       element etask-db-attr-taskendtime))
                     peffort)))
             (etask-write-element
              element
              (etask-cat-get-current-elementindex)))
            ((= catid etask-category-eventid)
             (setq element (etask-ev-get-peffort element))
             (etask-cat-insert-element
              element (etask-cat-get-current-elementindex)))
            (t
             (etask-proj-set-peffort)))
      (etask-cat-show-elements))))


;;; Data structrue init

(defun etask-cat-create-cat-alist()
  "Create associated list of different categories EtaskMode supports.

Structure: 
 \( categoryid . 
      \( filename categoryname newelementstr itemname elementname) )"
  (setq etask-cat-alist
        (list
         (cons etask-category-projectid
               (list etask-tasks-filename
                     (etask-lang-msg 400 etask-language 'init)
                     (etask-lang-msg 410 etask-language 'init)
                     (etask-lang-msg 420 etask-language 'init)
                     (etask-lang-msg 430 etask-language 'init)))
         (cons etask-category-todoid
               (list etask-todos-filename
                     (etask-lang-msg 401 etask-language 'init)
                     (etask-lang-msg 411 etask-language 'init)
                     (etask-lang-msg 421 etask-language 'init)
                     (etask-lang-msg 431 etask-language 'init)))
         (cons etask-category-eventid
               (list etask-events-filename
                     (etask-lang-msg 402 etask-language 'init)
                     (etask-lang-msg 412 etask-language 'init)
                     (etask-lang-msg 422 etask-language 'init)
                     (etask-lang-msg 432 etask-language 'init))))))


;;; Low level functions

(defun etask-cat-civalid-p(catid item)
  "Return true if CATID is a valid category identifier and ITEM is a
valid item name."
  (and 
   (etask-cat-is-valid-catid-p catid)
   (etask-cat-is-valid-item-p item)))

(defun etask-cat-is-valid-catid-p(catid)
  "Return true if CATID is a valid category identifier, nil otherwise."
  (when (natnump catid)
    (let ((list etask-cat-alist)
          (valid nil)
          (cat))
      (while list
        (when (= (etask-cat-get-catid-alist (car list)) catid)
          (setq valid t)
          (setq list nil))
        (setq list (cdr list)))
      valid)))

(defun etask-cat-is-valid-item-p(item)
  "Return true if ITEM is a valid item name, nil otherwise."
  (and 
   (stringp item)
   (if (etask-state-get etask-stateid-archive)
       (string-match "^[ \\.a-zA-Z0-9_?!-]+$" 
                     (etask-cat-trim-current-id item))
     (string-match "^[ a-zA-Z0-9_?!-]+$" 
                   (etask-cat-trim-current-id item)))))

(defun etask-cat-get-catid-alist(alistel)
  "Return the category id in ALISTEL, the alist element of a
category returned by assoc."
  (when (listp alistel)
    (car alistel)))

(defun etask-cat-get-catfile-alist(alistel)
  "Return the file name in ALISTEL, the alist element of a
category returned by assoc."
  (when (listp alistel)
    (car (cdr alistel))))

(defun etask-cat-get-catname-alist(alistel)
  "Return the category name in ALISTEL, the alist element of a
category returned by assoc."
  (when (listp alistel)
    (car (cdr (cdr alistel)))))

(defun etask-cat-get-catnamequery-alist(alistel)
  "Return the category name query in ALISTEL, the alist element of a
category returned by assoc."
  (when (listp alistel)
    (car (cdr (cdr (cdr alistel))))))

(defun etask-cat-get-catitemname-alist(alistel)
  "Return the general name of an item in a category in ALISTEL, the
alist element of a category returned by assoc."
  (when (listp alistel)
    (car (cdr (cdr (cdr (cdr alistel)))))))

(defun etask-cat-get-catelementname-alist(alistel)
  "Return the general name of an element in a category in ALISTEL, the
alist element of a category returned by assoc."
  (when (listp alistel)
    (car (cdr (cdr (cdr (cdr (cdr alistel))))))))

(defun etask-cat-get-catidlist()
  "Return list containing all category IDs."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist (cons 
                     (etask-cat-get-catid-alist (car list))
                     catlist))
      (setq list (cdr list)))
    (nreverse catlist)))

(defun etask-cat-get-catfilelist()
  "Return string list containing all category file names."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist (cons 
                     (etask-cat-get-catfile-alist (car list))
                     catlist))
      (setq list (cdr list)))
    (nreverse catlist)))

(defun etask-cat-get-catnamelist()
  "Return string list containing all category names."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist (cons 
                     (etask-cat-get-catname-alist (car list))
                     catlist))
      (setq list (cdr list)))
    (nreverse catlist)))

(defun etask-cat-get-catnamequerylist()
  "Return string list containing all category name queries."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist (cons 
                     (etask-cat-get-catnamequery-alist (car list))
                     catlist))
      (setq list (cdr list)))
    (nreverse catlist)))

(defun etask-cat-get-catitemnamelist()
  "Return string list containing all general item names of their
corresponding categories."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist 
            (cons (etask-cat-get-catitemname-alist (car list))
                  catlist))
      (setq list (cdr list)))
    (nreverse catlist)))

(defun etask-cat-get-catelementnamelist()
  "Return string list containing all general element names of their
corresponding items and categories."
  (let ((list etask-cat-alist)
        (catlist))
    (while list
      (setq catlist 
            (cons (etask-cat-get-catelementname-alist (car list))
                  catlist))
      (setq list (cdr list)))
    (nreverse catlist)))
 
(defun etask-cat-get-catid(catname)
  "Return category ID for category name CATNAME or nil as failure
indicator."
  (when (stringp catname)
    (let ((list etask-cat-alist)
          (catid)
          (listname))
      (while list
        (setq listname (etask-cat-get-catname-alist (car list)))
        (if (and (stringp listname) (string= listname catname))
            (progn
              (setq catid (etask-cat-get-catid-alist (car list)))
              (setq list nil))
          (setq list (cdr list))))
      catid)))

(defun etask-cat-get-catfilename(catid)
  "Return category control file name for category CATID or nil if no
category found."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((file 
           (etask-cat-get-catfile-alist
            (assoc catid etask-cat-alist))))
      (when (and (stringp file) (> (length file) 0))
        file))))

(defun etask-cat-get-catfilenamequery(catid)
  "Return category name query for category CATID or nil if no category
found."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((query
           (etask-cat-get-catnamequery-alist
            (assoc catid etask-cat-alist))))
      (when (and (stringp query) (> (length query) 0))
        query))))

(defun etask-cat-get-catitemname(catid)
  "Return general item name for category CATID or nil if no category
found."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((itemname
           (etask-cat-get-catitemname-alist
            (assoc catid etask-cat-alist))))
      (when (and (stringp itemname) (> (length itemname) 0))
        itemname))))

(defun etask-cat-get-catelementname(catid)
  "Return general element name for category CATID or nil if no category
found."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((elementname
           (etask-cat-get-catelementname-alist
            (assoc catid etask-cat-alist))))
      (when (and (stringp elementname) (> (length elementname) 0))
        elementname))))

(defun etask-cat-get-catname(catid)
  "Return category name for category CATID or nil if no category
found."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((catidlist (etask-cat-get-catidlist))
          (catnamelist (etask-cat-get-catnamelist))
          (catname))
      (while catidlist
        (when (= (car catidlist) catid)
          (setq catname (car catnamelist))
          (setq catidlist nil))
        (setq catidlist (cdr catidlist))
        (setq catnamelist (cdr catnamelist)))
      catname)))


;;; Category

(defun etask-cat-get-active-categories()
  "Return list of category IDs that have active items, nil if no such
categories exist."
  (let ((list etask-cat-alist)
        (catlist)
        (catid))
    (while list
      (setq catid (etask-cat-get-catid-alist (car list)))
      (when (not (etask-cat-is-itemlist-empty-p catid))
        (setq catlist (cons catid catlist)))
      (setq list (cdr list)))
    catlist))

(defun etask-cat-get-active-categorynames()
  "Return list of category names that have active items, nil if no such
categories exist."
  (let ((list (etask-cat-get-active-categories))
        (names))
    (while list
      (setq names (cons (etask-cat-get-catname (car list)) names))
      (setq list (cdr list)))
    names))


;;; Item

(defun etask-cat-get-itemlist(catid)
  "Return string list containing all items of CATEGORY or nil as
failure indicator."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((filename (etask-cat-get-catfilename catid)))
      (when (and (stringp filename) (> (length filename) 0))
        (let ((filecontent))
          (save-current-buffer
            (set-buffer (find-file-noselect 
                         (concat etask-working-dir filename)))
            (setq filecontent
                  (if (> (buffer-size) 0)
                      (buffer-substring-no-properties
                       (point-min) (search-forward-regexp "$"))
                    nil))
            (when (> (length filecontent) 0)
              (car (read-from-string filecontent)))))))))

(defun etask-cat-get-current-item()
  "Return current item in a list: (category item) or nil if no current
item found."
  (let ((catid (etask-state-get etask-stateid-currcat))
        (item (etask-state-get etask-stateid-curritem)))
    (when (etask-cat-civalid-p catid item)
      (list catid item))))

(defun etask-cat-get-current-itemfilename()
  "Return file name that contains current ITEMs or nil if category
item not found."
  (let ((catid (car (etask-cat-get-current-item)))
        (item (car (cdr (etask-cat-get-current-item)))))
    (etask-cat-get-itemfilename catid item)))

(defun etask-cat-get-itemfilename(catid item)
  "Return file name that contains category CATID's ITEMs or nil if
category item not found."
  (when (etask-cat-civalid-p catid item)
    (let ((catfile (etask-cat-get-catfilename catid)))
      (when (and (stringp catfile) (> (length catfile) 0))
        (concat catfile "." item)))))

(defun etask-cat-is-current-itemlist-empty-p()
  "Return true if current item list is empty, nil otherwise."
  (> (length 
     (etask-cat-get-itemlist 
      (etask-state-get etask-ctrl-curritem))
     0)))

(defun etask-cat-is-itemlist-empty-p(catid)
  "Return true if item list of category CATID is empty, nil
otherwise."
  (when (etask-cat-is-valid-catid-p catid)
    (let ((itemlist (etask-cat-get-itemlist catid)))
      (= (length itemlist) 0))))

(defun etask-cat-is-item-p(catid item)
  "Return true if ITEM is in itemlist or nil otherwise."
  (when (etask-cat-civalid-p catid item)
    (let ((itemlist (etask-cat-get-itemlist catid))
          (itemp nil))
      (while itemlist
        (setcar itemlist 
                (etask-cat-trim-current-id (car itemlist)))
        (when (string= (car itemlist) 
                       (etask-cat-trim-current-id item))
          (setq itemp t)
          (setq itemlist nil))
        (setq itemlist (cdr itemlist)))
      itemp)))

(defun etask-cat-set-current-item(catid item)
  "Make category ITEM with id CATID the current item."
  (when (etask-cat-civalid-p catid item)
    (let ((itemlist (etask-cat-get-itemlist catid))
          (item (etask-cat-trim-current-id item))
          (newitemlist))
      (etask-state-set etask-stateid-currcat catid)
      (etask-state-set etask-stateid-curritem item)
      (etask-set-control-item etask-ctrl-currcategory catid 'main)
      (etask-set-control-item etask-ctrl-curritem item 'main)
      (while itemlist
        (setcar itemlist 
                (etask-cat-trim-current-id (car itemlist)))
        (if (and (string= (car itemlist) 
                          (etask-cat-trim-current-id item)))
            (setq newitemlist
                  (cons 
                   (etask-cat-get-current-itemstr item)
                   newitemlist))
          (setq newitemlist
                (cons (car itemlist) newitemlist)))
        (setq itemlist (cdr itemlist)))
      (etask-cat-set-itemlist-p catid (nreverse newitemlist)))))

(defun etask-cat-insert-item(catid item)
  "Add ITEM to category with id CATID."
  (when (and (etask-cat-civalid-p catid item)
             (not (etask-cat-is-item-p catid item)))
    (etask-cat-set-itemlist-p 
     catid (cons item (etask-cat-get-itemlist catid)))))

(defun etask-cat-delete-item-p(catid item)
  "Delete ITEM from category with ID CATID."
  (when (etask-cat-civalid-p catid item)
    (let ((itemlist (etask-cat-get-itemlist catid))
          (currp (etask-is-current-str item))
          (item (etask-cat-trim-current-id item)))
      (when (and itemlist (etask-cat-is-item-p catid item))
        (when currp
          (setq item (etask-cat-get-current-itemstr item))
          (etask-state-clear etask-stateid-currcat)
          (etask-state-clear etask-stateid-curritem)
          (etask-set-control-item etask-ctrl-currcategory nil 'main)
          (etask-set-control-item etask-ctrl-curritem nil 'main))
        (etask-cat-set-itemlist-p catid (remove item itemlist))))))

(defun etask-cat-rename-item-p(catid olditem newitem)
  "Delete category-item-link between category with ID CATID and item
OLDITEM and copy all item contents to NEWITEM."
  (when (and (etask-cat-civalid-p catid olditem)
             (etask-cat-is-valid-item-p newitem))
    (let ((itemlist (etask-cat-get-itemlist catid))
          (currp (etask-is-current-str olditem)))
      (when (and itemlist
                 (etask-cat-is-item-p catid olditem)
                 (not (etask-cat-is-item-p catid newitem)))
        (let ((elements (etask-cat-get-elementlist catid olditem))
              (newelements)
              (el))
          (etask-cat-insert-item catid newitem)
          (while elements
            (setq newelements (cons (etask-db-set 
                                     (car elements)
                                     etask-db-attr-projname
                                     newitem)
                                    newelements))
            (etask-cat-copy-file
             "notes" catid olditem newitem (car elements))
            (setq elements (cdr elements)))
          (etask-cat-copy-file "archive" catid olditem newitem)
          (etask-cat-copy-file "etaskctrl" catid olditem newitem)
          (etask-cat-delete-item-p catid olditem)
          
          (etask-write-elements
           (nreverse newelements) catid newitem 'erase)
          (when currp
            (etask-cat-set-current-item catid newitem))
          (etask-cat-show-elements))))))

(defun etask-cat-set-itemlist-p(catid list)
  "Store items of category with id CATID provided in LIST."
  (when (and (etask-cat-is-valid-catid-p catid) (listp list))
    (save-current-buffer
      (let ((filename (etask-cat-get-catfilename catid)))
        (when (and (stringp filename) (> (length filename) 0))
          (set-buffer (find-file-noselect 
                       (concat etask-working-dir filename)))
          (erase-buffer)
          (prin1 list (current-buffer))
          t)))))

(defun etask-cat-get-current-itembuffer()
  "Read file of current item and return its buffer."
  (let* ((curritem (etask-cat-get-current-item))
         (catid (car curritem))
         (item (car (cdr curritem))))
    (etask-cat-get-itembuffer catid item)))

(defun etask-cat-get-itembuffer(catid item)
  "Read file of ITEM belonging to category CATID and return its
buffer."
  (let ((filename (etask-cat-get-itemfilename catid item)))
    (when (and (stringp filename) (> (length filename) 0))
      (find-file-noselect
       (concat etask-working-dir filename) t))))

(defun etask-cat-get-current-itemarchivebuffer()
  "Read archive file of current non-archived item and return its
buffer."
  (let ((filename (etask-cat-get-current-itemarchivefile)))
    (when (and (stringp filename) (> (length filename) 0))
      (find-file-noselect filename t))))


;;; Element

(defun etask-cat-get-elementlist(&optional catid item names)
  "Return list containing all elements of current or specified
ITEM in CATEGORY or nil as failure indicator.  If optional NAMES is
non-nil, return a list with all top level element names instead."
  (when (not (and catid item))
    (let ((curritem (etask-cat-get-current-item)))
      (setq catid (car curritem))
      (setq item (car (cdr curritem)))))
  (when (etask-cat-civalid-p catid item)
    (let* ((item (etask-cat-trim-current-id item))
           (filename (etask-cat-get-itemfilename catid item)))
      (when (and (stringp filename) (> (length filename) 0))
        (let ((pos)
              (line)
              (element)
              (elementlist))
          (save-current-buffer
            (set-buffer (find-file-noselect
                         (concat etask-working-dir filename)))
            (goto-char (point-max))
            (while (re-search-backward etask-elementfind-regexp nil t)
              (setq pos (point))
              (setq line (buffer-substring 
                          (match-beginning 0) 
                          (search-forward-regexp "$")))
              ;; we found `etask-elementfind-regexp' => (length line) is > 0 
              (setq element (car (read-from-string line)))
              (setq elementlist
                    (if names
                        (cons
                         (etask-db-get element etask-db-attr-taskname)
                         elementlist)
                      (cons element elementlist)))
              (goto-char pos)))
          elementlist)))))

(defun etask-cat-is-element-marked-p()
  "Return true if current element is marked, nil otherwise."
  (let ((pos (point))
        markp)
    (forward-line 0)
    (setq markp (not (string= (char-to-string (char-after)) " ")))
    (goto-char pos)
    markp))

(defun etask-cat-get-marked-elements(&optional ixlist)
  "Return list of all marked elements in buffer.  Return nil if no
marked elements found.  If optional IXLIST is non-nil return list of
all index lists."
  (let ((pos (point))
        (elnum (etask-cat-get-elementnum))
        marked)
    (goto-char (point-min))
    (while (> elnum 0)
      (when (etask-cat-is-element-marked-p)
        (if ixlist
            (setq marked (cons (etask-cat-get-current-elementindex) marked))
          (setq marked (cons (etask-cat-get-current-element) marked))))
      (forward-line 1)
      (setq elnum (1- elnum)))
    (goto-char pos)
    (nreverse marked)))

(defun etask-cat-get-current-elementlist()
  "Return all current elements in ixlist structure or nil if a failure
occurred."
  (let ((catid (car (etask-cat-get-current-item)))
        (item (car (cdr (etask-cat-get-current-item)))))
    (etask-cat-get-elementlist catid item)))

(defun etask-cat-transform-elt-top(element &optional names)
  "Return ELEMENT with all its subelements appended at top level and
subelement list set to nil.  If optional NAMES is non-nil return just
the names.  The result is in the car of the return value."
  (when element
     (if names
         (nconc
          (list (etask-db-get element etask-db-attr-taskname))
          (etask-cat-transform-elements-top 
           (etask-db-get element etask-db-attr-subtasklist)
           names))
       (nconc
        (list (etask-db-set element etask-db-attr-subtasklist nil))
        (etask-cat-transform-elements-top 
         (etask-db-get element etask-db-attr-subtasklist))))))

(defun etask-cat-transform-elements-top(elements &optional names)
  "Return ELEMENT with all its subelements appended at top level and
subelement list set to nil.  If optional NAMES is non-nil return just
the names."
  (let ((ret))
    (while elements
      (setq ret
            (if names
                (nconc
                 (etask-cat-transform-elt-top (car elements) names)
                 ret)
              (nconc
               ret (etask-cat-transform-elt-top (car elements)))))
      (setq elements (cdr elements)))
    ret))

(defun etask-cat-get-current-elts(&optional names)
  "Return all current elements and its subelements as top level list,
ie \(1\).  If optional NAMES is non-nil, return the name list."
  (etask-cat-transform-elements-top
   (etask-cat-get-current-elementlist) names))

(defun etask-cat-get-elementindex(element)
  "Return index list of ELEMENT.  The index list contains the
ELEMENT's index of all levels: \( ix-level1 ix-level2 ix-level3 ...)\.
Example: \(1 2 3\) is the 3rd subelement of the 2nd subelement of the
first element."
  (let ((elementname (etask-db-get element etask-db-attr-taskname)))
    (let* ((catid (car (etask-cat-get-current-item)))
           (item (car (cdr (etask-cat-get-current-item))))
           (elements (etask-cat-get-elementlist catid item)))
      (etask-cat-get-ixlist elements elementname))))

(defun etask-cat-get-current-elementindex()
  "Return index list of current element."
  (etask-cat-get-ixlist-buf (etask-current-line)))

(defun etask-cat-get-ixlist(elements name &optional sublevel ixlist)
  "Return index list of \(sub\)element named NAME in list ELEMENTS."
  (when elements
    (let* ((sublevel (if (natnump sublevel) sublevel 0))
           (ixlist (etask-get-sublist ixlist 1 (1+ sublevel)))
           (craw (nth sublevel ixlist))
           (curr  (if (natnump craw) craw 0))
           (elname (etask-db-get 
                    (car elements) etask-db-attr-taskname))
           (subelements (etask-db-get 
                         (car elements) etask-db-attr-subtasklist)))
    
      (setq ixlist (nconc
                    (etask-get-sublist ixlist 1 sublevel)
                    (list (1+ curr))
                    (nthcdr (1+ sublevel) ixlist)))

      (cond ((string= name elname)
             ixlist)
            (t
             (let ((ixlisttmp ixlist))
               (setq ixlist 
                     (etask-cat-get-ixlist
                      subelements name (1+ sublevel) ixlist))
               (if ixlist
                   ixlist
                 (etask-cat-get-ixlist
                  (cdr elements) name sublevel ixlisttmp))))))))

(defun etask-cat-get-ixlist-buf(line)
  "Return index list for element in LINE in current buffer."
  (when (natnump line)
    (let ((pos (point))
          (prefixlen (length (etask-get-infostring)))
          str i ixlist craw curr p)
      (goto-char (point-min))
      (while (> line 0)
        (setq p (point))
        (setq endstr 
              (search-forward-regexp etask-delimiter-regexp nil t))
        (when (natnump endstr)
          (setq str
                (buffer-substring-no-properties
                 (+ p prefixlen) endstr))
          (setq i (etask-leading-char-num str ? ))
          (setq ixlist (etask-get-sublist ixlist 1 (1+ i)))
          (setq craw (nth i ixlist))
          (setq curr (if (natnump craw) craw 0))
          (setq ixlist (nconc
                        (etask-get-sublist ixlist 1 i)
                        (list (1+ curr))
                        (nthcdr (1+ i) ixlist)))
          (forward-line 1))
        (setq line (1- line)))
      (goto-char pos)
      ixlist)))

(defun etask-cat-get-nth-element(elements ixlist)
  "Return element in list ELEMENTS with index list IXLIST or nil if
element not found."
  (when (and elements (listp ixlist)
             (natnump (car ixlist)) (> (car ixlist) 0))
    (if (= (length ixlist) 1)
        (nth (1- (car ixlist)) elements)
      (etask-cat-get-nth-element
       (etask-db-get
        (nth (1- (car ixlist)) elements) etask-db-attr-subtasklist)
       (cdr ixlist)))))

(defun etask-cat-get-element(ixlist)
  "Return element list specified by IXLIST or nil if element not
found."
  (etask-cat-get-nth-element
   (etask-cat-get-current-elementlist) ixlist))

(defun etask-cat-set-nth-element(elements ixlist elt &optional insert)
  "Return ELEMENTS with element ELT at position specified by IXLIST.
If position is already occupied by another element, replace it with
ELT.  If optional INSERT is non-nil, do not overwrite existing data
but move it instead to another index."
  (when (and (listp ixlist) (natnump (car ixlist)) (> (car ixlist) 0))
    (let* ((pos (1- (car ixlist)))
           (currel (nth pos elements)))
      (if (= (length ixlist) 1)
          (nconc
           (etask-get-sublist elements 1 pos)
           (when (etask-element-is-valid-p elt) ;otherwise delete
             (list elt))
           (if insert
               (nthcdr pos elements)
             (nthcdr (1+ pos) elements)))
        (nconc
         (etask-get-sublist elements 1 pos)
         (list
          (etask-db-set
           currel 
           etask-db-attr-subtasklist
           (etask-cat-set-nth-element
            (etask-db-get currel etask-db-attr-subtasklist)
            (cdr ixlist)
            elt
            insert)))
         (nthcdr (1+ pos) elements))))))

(defun etask-cat-store-element(element &optional ixlist insert buf)
  "Store ELEMENT at the position specified by IXLIST.  Overwrite
existing data unless optional INSERT is non-nil.  If optional BUF is
non-nil, store element in buffer BUF.  Return nil if operation fails."
  (when (etask-element-is-valid-p element)
    (let* ((elix (etask-cat-get-elementindex element))
           (ixlist (if (and (listp ixlist) (> (length ixlist) 0))
                       ixlist
                     (if elix elix '(1))))
           (newel (etask-cat-set-nth-element
                   (etask-cat-get-current-elementlist)
                   ixlist element insert)))
      (etask-write-element
       (nth (1- (car ixlist)) newel) (car ixlist) nil nil buf))))

(defun etask-cat-insert-element(element ixlist &optional checkp buf)
  "Store ELEMENT at the position specified by IXLIST.  Move existing
data to new position if necessary - do not overwrite existing data.
If optional CHECKP is non-nil, uniqueness of element's name is
checked.  If optional BUF is non-nil, store element in buffer BUF."
  (when (or (not checkp)
            (not (member 
                  (etask-db-get element etask-db-attr-taskname)
                  (etask-cat-get-current-elts 'names))))
    (etask-cat-store-element element ixlist 'insert buf)))

(defun etask-cat-delete-element(ixlist)
  "Delete the element and all its subelements at IXLIST."
  (let ((elements (etask-cat-get-current-elementlist)))
    (if (and (listp ixlist) (= (length ixlist) 1) (natnump (car ixlist)))
        (save-current-buffer
          (let ((elementname
                 (etask-db-get
                  (etask-cat-get-element (list (car ixlist)))
                  etask-db-attr-taskname)))
            (set-buffer (etask-cat-get-current-itembuffer))
            (goto-char (point-min))
            (flush-lines (etask-get-elementregexp elementname))))
      (etask-write-element 
       (nth (1- (car ixlist)) 
            (etask-cat-set-nth-element elements ixlist nil))
       (car ixlist)))))

(defun etask-cat-archive-element(ixlist)
  "Archive the element and all its subelements at IXLIST."
  (when (and (listp ixlist) (= (length ixlist) 1) (natnump (car ixlist)))
    (let ((elt (etask-cat-get-element ixlist)))
      (etask-cat-delete-element ixlist)
      (etask-cat-insert-element
       elt '(1) nil (etask-cat-get-current-itemarchivebuffer)))))

(defun etask-cat-get-current-element()
  "Return current element list: (name ...) or nil if current element
not found."
  (etask-cat-get-nth-element
   (etask-cat-get-current-elementlist)
   (etask-cat-get-current-elementindex)))

(defun etask-cat-get-named-element(name)
  "Return element NAME of current item or nil if NAME not found."
  (let ((buf (etask-cat-get-current-itembuffer)))
    (when (bufferp buf)
      (save-current-buffer
        (set-buffer buf)
        (goto-char (point-min))
        (when (and (> (length name) 0)
                   (search-forward (etask-get-elementregexp name) nil t))
          (let ((line)
                (task))
            (forward-line 0)
            (setq line (buffer-substring-no-properties 
                        (point) 
                        (re-search-forward "$")))
            (when (> (length line) 0)
              (car (read-from-string line)))))))))

(defun etask-cat-get-topelementnum()
  "Return the number of top elements in `etask-buffer'.  Note that the
length of a top element is 1."
  (save-current-buffer
    (set-buffer (etask-cat-get-current-itembuffer))
    (count-lines (point-min) (point-max))))

(defun etask-cat-get-elementnum()
  "Return the number of elements displayed in `etask-buffer'."
  (let ((pos (point))
        (num 0))
    (goto-char (point-min))
    (while (re-search-forward etask-delimiter-regexp nil t)
      (setq num (1+ num)))
    (goto-char pos)
    num))

(defun etask-cat-get-subelement-num-after(&optional l)
  "Return number of subelements of current element displayed after
current line.  If optional L is a positive integer then number of
subelements after line L is calculated."
  (let* ((line (if (and (natnump l) (> l 0) (< l (etask-cat-get-elementnum)))
                   l
                 (etask-current-line)))
         (currlevel (length (etask-cat-get-ixlist-buf line)))
         (pos (point))
         (num 0)
         (i (- (etask-cat-get-elementnum) line)))
    (forward-line 1)
    (while (and (< currlevel 
                   (length (etask-cat-get-current-elementindex)))
                (> i 0))
      (setq num (1+ num))
      (setq i (1- i))
      (forward-line 1))
    (goto-char pos)
    num))

(defun etask-cat-get-elementnum-samelevel-before(&optional level)
  "Return distance in number of lines from current element to the
preceding element with the same level or, if LEVEL is non-nil, with
level LEVEL."
  (if (> (etask-current-line) 1)
      (let* ((i (etask-current-line))
             (currlevel (if level level
                          (length (etask-cat-get-ixlist-buf i))))
             (pos (point))
             (num 0))
        (forward-line -1)
        (while (and (> i 0)
                    (>= (length
                         (etask-cat-get-current-elementindex))
                        currlevel))
          (setq num (1+ num))
          (if (= (length (etask-cat-get-current-elementindex))
                 currlevel)
              (setq i -1)
            (setq i (1- i))
            (forward-line -1)))
        (goto-char pos)
        (if (= i -1)
            num
          0))
    0))

(defun etask-cat-get-elementnum-samelevel-after()
  "Return distance in number of lines from current element to the
following element with the same level."
  (let* ((line (etask-current-line))
         (currlevel (length (etask-cat-get-ixlist-buf line)))
         (pos (point))
         (num 0)
         (i (- (etask-cat-get-elementnum) line)))
    (forward-line 1)
    (while (and (> i 0)
                (>= (length
                     (etask-cat-get-current-elementindex))
                    currlevel))
      (setq num (1+ num))
      (if (= (length (etask-cat-get-current-elementindex))
             currlevel)
          (setq i -1)
        (setq i (1- i))
        (forward-line 1)))
    (goto-char pos)
    (if (= i -1)
        num
      0)))

(defun etask-cat-get-elementnum-currenttop()
  "Return distance in number of lines from current element to the
preceding top level element."
  (etask-cat-get-elementnum-samelevel-before 1))

(defun etask-cat-is-current-elementlist-empty-p()
  "Return true if current item list is empty, nil otherwise."
  (= (length (etask-cat-get-current-elementlist)) 0))

(defun etask-cat-is-elementlist-empty-p(catid item)
  "Return true if item list of category CATID is empty, nil
otherwise."
  (when (etask-cat-civalid-p catid item)
    (let ((elementlist (etask-cat-get-elementlist catid item)))
      (= (length elementlist) 0))))

(defun etask-cat-is-element-behind-schedule-p(element)
  "Return t if ELEMENT is behind schedule, nil otherwise."
  (let* ((catid (car (etask-cat-get-current-item)))
         (item (car (cdr (etask-cat-get-current-item)))))
    (when (etask-cat-civalid-p catid item)
      (cond ((= catid etask-category-todoid)
             (not (etask-todo-is-onschedule-p element)))
            ((= catid etask-category-projectid)
             (etask-is-task-behind-schedule-p element))
            (t
             t)))))

(defun etask-cat-elementnotes-p(&optional catid item element)
  "Return true if current ELEMENT's notes file exists, nil otherwise.
If CATID, ITEM and ELEMENT are non-nil, look for this specific
ELEMENT's notes file."
  (let* ((filename
          (if (and catid item element (etask-cat-civalid-p catid item))
              (etask-cat-get-elementnotes-filename catid item element)
            (etask-cat-get-elementnotes-filename)))
         (buf (get-buffer (substring filename (length etask-working-dir)))))
    (if buf 
        (if (> (buffer-size buf) 0) t nil)
      (and (file-readable-p filename)
           (> (nth 7 (file-attributes filename)) 0)))))

(defun etask-cat-goto-elementindex(ixlist)
  "Move point to delimiter in line specified by IXLIST."
  (let ((sublevel 0)
        (num 0))
    (goto-char (point-min))
    (etask-goto-delimiter)
    (while ixlist
      (setq num (if (= sublevel 0) (1- (car ixlist)) (car ixlist)))
      (etask-cat-fwd-levels sublevel num)
      (setq sublevel (1+ sublevel))
      (setq ixlist (cdr ixlist)))))

(defun etask-cat-is-sublevel-p(l1 l2)
  "Return true if L1 is a sublevel of L2, nil otherwise."
  (when (and (listp l1) (listp l2))
    (let ((len1 (length l1))
          (len2 (length l2)))
      (when (> len1 len2)
        (equal (etask-get-sublist l1 1 len2) l2)))))

(defun etask-cat-get-sublevelnum-buf()
  "Return maximum sublevel shown in buffer."
  (let ((pos (point))
        (prefixlen (length (etask-get-infostring)))
        (maxsub 0)
        substr p)
    (goto-char (point-min))
    (while (search-forward-regexp etask-subelements-prefix nil t)
      (forward-line 0)
      (setq p (point))
      (setq substr (buffer-substring-no-properties
                    (+ p prefixlen) (match-end 0)))
      (setq maxsub (max maxsub (etask-leading-char-num substr ? )))
      (forward-line 1))
    (goto-char pos)
    maxsub))

(defun etask-cat-get-line(ix1 &optional ix2 ix3 ix4)
  "Return line number of ix1 in a list.  If IX2 or other optional
parameters are non-nil, include their line numbers in the list."
  (let ((max (etask-cat-get-elementnum))
        (line 1)
        (i 1)
        (f2 (if ix2 nil t))
        (f3 (if ix3 nil t))
        (f4 (if ix4 nil t))
        f1 l1 l2 l3 l4)
    (while (<= i max)
      (setq currix (etask-cat-get-ixlist-buf i))
      (when (and (not f1) (equal currix ix1))
        (setq f1 t)
        (setq l1 i))
      (when (and (not f2) (equal currix ix2))
        (setq f2 t)
        (setq l2 i))
      (when (and (not f3) (equal currix ix3))
        (setq f3 t)
        (setq l3 i))
      (when (and (not f4) (equal currix ix4))
        (setq f4 t)
        (setq l4 i))
      (if (and f1 f2 f3 f4)
          (setq i (1+ max))
        (setq i (1+ i))))
    (list l1 l2 l3 l4)))

(defun etask-cat-compare-sublevels-p(l1 l2)
  "Return true if L1 is before L2 on screen, nil otherwise."
  (let ((lines (etask-cat-get-line l1 l2)))
    (< (car lines) (car (cdr lines)))))

(defun etask-cat-insert-elements(&optional sub)
  "Add new elements to current item of current category.  Insert
before current element at the same level.  If there are marked
elements, move first marked element to current ixlist.  If optional
SUB is non-nil, create a subelement of current element."
  (interactive "P")
  (let* ((catid (car (etask-cat-get-current-item)))
         (item (car (cdr (etask-cat-get-current-item))))
         (fmeltix (car (etask-cat-get-marked-elements 'ixlist)))
         (fmelt (when fmeltix
                  (etask-cat-get-element fmeltix)))
         (ixlist (if sub
                     (nconc (etask-cat-get-current-elementindex) '(1))
                   (etask-cat-get-current-elementindex))))
    (cond ((and fmelt
                (not (etask-cat-is-sublevel-p ixlist fmeltix))
                (not (equal ixlist fmeltix)))
           (let ((dummy "!!!!DummY!!!!")
                 (insp (etask-cat-compare-sublevels-p
                        fmeltix
                        (if sub (butlast ixlist) ixlist))))

             ;; `etask-cat-delete-element' flushes all lines containing
             ;; \(etask-get-elementregexp elementname\)
             (etask-cat-delete-element fmeltix)

             ;; restore ixlist via dummy if element is to insert below
             ;; mark, otherwise failure if length of fmeltix = 1
             (when insp
               (etask-cat-insert-element
                (etask-db-set fmelt  etask-db-attr-taskname dummy)
                fmeltix))

             ;; insert
             (etask-cat-insert-element fmelt ixlist)

             ;; delete dummy if inserted earlier
             (when insp
               (etask-cat-delete-element fmeltix))))
          
          ((not fmelt)
           (when (etask-cat-civalid-p catid item)
             (cond ((= catid etask-category-projectid)
                    (etask-proj-insert-elements item ixlist))
                   ((= catid etask-category-todoid)
                    (etask-todo-insert-elements item ixlist))
                   ((= catid etask-category-eventid)
                    (etask-ev-insert-elements item ixlist))
                   (t
                    ()))))
          (t
           ()))
    (etask-cat-show-elements)))

(defun etask-cat-delete-elements(&optional element update)
  "Delete the current ELEMENT or, if there are marked elements, all
marked elements from current ITEM.

If called interactively (then optional parameter ELEMENT is nil) the
user must confirm the deletion.  UPDATE, if non-nil, indicates that
the task is just updated - only possible if taskname not changed.
However, its old data must still be deleted but the deletion
confirmation messages must not appear."
  (interactive)
  (let ((catid (car (etask-cat-get-current-item)))
        (item (car (cdr (etask-cat-get-current-item)))))
    (when (and (etask-cat-civalid-p catid item)
               (etask-interactive-preconditions))
      (let* ((deletelist (if element (list element)
                           (etask-get-ixlist-to-be-edited)))
             reallydelete elementname buf regexp)
        (while deletelist
          
          (setq elementname
                (etask-db-get (car deletelist) etask-db-attr-taskname))
          (setq elementix
                (etask-cat-get-elementindex elementname))
          (etask-cat-goto-elementindex elementix)

          (etask-cat-show-elements)
          (setq reallydelete
                (if update
                    t
                  (yes-or-no-p 
                   (concat
                    (etask-lang-msg 260 etask-language)
                    (format " -- '%s' " elementname)
                    "? "))))
          (setq buf
                (if reallydelete (etask-cat-get-current-itembuffer)))
          (setq regexp
                (if reallydelete
                    (etask-get-elementregexp elementname)))
          (when reallydelete
            (save-current-buffer
              (set-buffer buf)
              (goto-char (point-min))
              (flush-lines regexp))
            (when (interactive-p)
              (etask-cat-show-elements)
              (message 
               (concat 
                (format "'%s': " elementname)
                (etask-lang-msg 270 etask-language)
                "!"))
              (sit-for 1)))
          (setq deletelist (cdr deletelist)))

        (etask-goto-delimiter)
        (etask-cat-show-elements)))))

(defun etask-cat-get-elementnotes-filename(&optional catid item element)
  "Return file name for current ELEMENT's notes.  If CATID, ITEM and
ELEMENT are non-nil, return the notes file name of this specified
element."
  (if (and catid item element
           (etask-cat-civalid-p catid item)
           (etask-element-is-valid-p element))
      (concat
       etask-working-dir
       (etask-cat-get-itemfilename catid (etask-cat-trim-current-id item))
       "."
       (etask-db-get element etask-db-attr-taskname))
    (concat
     etask-working-dir
     (etask-cat-get-current-itemfilename)
     "."
     (etask-db-get
      (etask-cat-get-current-element)
      etask-db-attr-taskname))))
                     
(defun etask-cat-get-itemarchivefile(catid item)
  ""
  (let ((filename
         (etask-cat-get-itemfilename
          catid (etask-cat-trim-current-id item))))
    (when (and (stringp filename) (> (length filename) 0))
      (concat etask-working-dir filename ".archive"))))

(defun etask-cat-get-current-itemarchivefile()
  ""
  (let ((curritem (etask-cat-get-current-item)))
    (etask-cat-get-itemarchivefile (car curritem) (car (cdr curritem)))))

(defun etask-cat-copy-file(id catid fromitem toitem &optional element)
  "Copy file specified by ID, CATID, and optionally ELEMENT, from item
FROMITEM to item TOITEM.  ID is in {notes, archive, etaskctrl}"
  (let ((oldfname
         (cond ((string= id "notes")
                (etask-cat-get-elementnotes-filename catid fromitem element))
               ((string= id "archive")
                (etask-cat-get-itemarchivefile catid fromitem))
               ((string= id "etaskctrl")
                (etask-get-item-ctrl-filename catid fromitem))
               (t
                ())))
        (newfname
         (cond ((string= id "notes")
                (etask-cat-get-elementnotes-filename catid toitem element))
               ((string= id "archive")
                (etask-cat-get-itemarchivefile catid toitem))
               ((string= id "etaskctrl")
                (etask-get-item-ctrl-filename catid toitem))
               (t
                ()))))
    (when (file-readable-p oldfname)
      (copy-file oldfname newfname 1))))

(defun etask-cat-show-elementstatus(ixlist &optional reportingp)
  "Print element status - at the bottom of the window or, if optional
REPORTINGP is non-nil, in report file."
  (interactive)
  (when (and (listp ixlist) (> (length ixlist) 0))
    (let* ((curritem (etask-cat-get-current-item))
           (catid (car curritem))
           (item (car (cdr curritem))))
      (when (etask-cat-civalid-p catid item)
        (let ((element (etask-cat-get-element ixlist))
              (status "")
              (pos (point)))
          (cond ((= catid etask-category-projectid)
                 (setq status
                       (etask-proj-show-elementstatus element reportingp)))
                ((= catid etask-category-todoid)
                 (setq status
                       (etask-todo-show-elementstatus element reportingp)))
                ((= catid etask-category-eventid)
                 (setq status
                       (etask-ev-show-elementstatus element reportingp)))
                (t
                 ()))
          (if reportingp
              status
            (progn
              (etask-insert-status status)
              (goto-char pos))))))))

(defun etask-cat-insert-elementbar-p(catid element)
  "Return true if ELEMENT's bar is to insert into chart, nil
otherwise."
  (when (etask-cat-is-valid-catid-p catid)
    (cond ((= catid etask-category-projectid)
           (etask-proj-insert-elementbar-p element))
          ((= catid etask-category-todoid)
           (etask-todo-insert-elementbar-p element))
          ((= catid etask-category-eventid)
           (etask-ev-insert-elementbar-p element))
          (t
           ()))))

(defun etask-cat-generate-default-element(item name)
  "Return todo NAME of todo category TODOCAT with default values."
  (let ((element))
    (setq element (etask-db-set element etask-db-attr-taskname name))
    (setq element (etask-db-set element etask-db-attr-projname item))
    (setq element (etask-db-set element etask-db-attr-tracking
                             etask-default-tracking))
    (setq element (etask-db-set element etask-db-attr-tasktype
                             etask-default-type))
    (setq element (etask-db-set element etask-db-attr-peffort 0))
    (setq element (etask-db-set element etask-db-attr-eeffort 0))
    (setq element (etask-db-set element etask-db-attr-taskbegin
                             (calendar-current-date)))
    (setq element (etask-db-set element etask-db-attr-taskend
                             (calendar-current-date)))
    (setq element (etask-db-set element etask-db-attr-mark
                             etask-unmarkedtask-string))
    (setq element (etask-db-set element etask-db-attr-priority
                             etask-default-priority))
    (setq element (etask-db-set element etask-db-attr-category nil))
    (setq element (etask-db-set element etask-db-attr-policy nil))
    (setq element (etask-db-set element etask-db-attr-people nil))
    (setq element (etask-db-set element etask-db-attr-arglist nil))
    (setq element (etask-db-set element etask-db-attr-subtasklist nil))
    element))

(defun etask-cat-sortfunc-priority(elt1 elt2 &optional equal)
  "Return true if ELT1's priority < ELT2's priority.  If optional
EQUAL is non-nil return true if ELT1's priority = ELT2's priority"
  (let ((prio1 (etask-db-get elt1 etask-db-attr-priority))
        (prio2 (etask-db-get elt2 etask-db-attr-priority)))
    (if equal
        (= prio1 prio2)
      (< prio1 prio2))))

(defun etask-cat-sortfunc-taskendtime(elt1 elt2 &optional equal)
  "Return true if ELT1's due date and time earlier than ELT2's due
date and time."
  (let* ((end1 (etask-make-duedatetime elt1))
         (end2 (etask-make-duedatetime elt2)))
    (if equal
        (etask-datetime-equal-p end1 end2)
      (etask-datetime-less-p end1 end2))))

(defun etask-cat-sortfunc-priority-taskendtime(elt1 elt2)
  "Return true if ELT1's priority < ELT2's priority.  If ELT1's
priority = ELT2's priority return true if ELT1's due date and time
earlier than ELT2's due date and time."
  (or (etask-cat-sortfunc-priority elt1 elt2)
      (and (etask-cat-sortfunc-priority elt1 elt2 'equal)
           (etask-cat-sortfunc-taskendtime elt1 elt2))))

(defun etask-cat-sortfunc-taskendtime-priority(elt1 elt2)
  "Return true if ELT1's due date and time < ELT2's due date and time.
If ELT1's due date and time = ELT2's due date and time return true if
ELT1's priority < ELT2's priority."
  (or (etask-cat-sortfunc-taskendtime elt1 elt2)
      (and (etask-cat-sortfunc-taskendtime elt1 elt2 'equal)
           (etask-cat-sortfunc-priority elt1 elt2))))


;;; Utilities

(defun etask-cat-get-current-itemstr(item)
  "Return ITEM with enclosing `etask-current-category-id'."
  (concat etask-current-category-id item etask-current-category-id))

(defun etask-cat-trim-current-id(str)
  "Return STR without enclosing `etask-current-category-id'.  If
STR is not enclosed by this id return STR unchanged."
  (if (etask-is-current-str str)
      (substring str
                 (length etask-current-category-id)
                 (- 0 (length etask-current-category-id)))
    str))

(defun etask-is-current-str(str)
  "Return true if CATEGORY is enclosed by `etask-current-category-id',
nil otherwise."
  (when (and (stringp str)
             (> (length str) (* 2 (length
                                   etask-current-category-id))))
    (and (string= (substring str
                             0 (length etask-current-category-id))
                  etask-current-category-id)
         (string= (substring str
                             (- 0 (length etask-current-category-id)))
                  etask-current-category-id))))

(defun etask-leading-char-num(str char)
  "Return number of leading characters CHAR of string STR."
  (let ((ix 0))
    (while (eq (aref str ix) char)
      (setq ix (1+ ix)))
    ix))

(defun etask-cat-fwd-levels(sublevel num)
  "Move point to delimiter in line containing NUMth LEVEL.  SUBLEVEL
zero = no sublevel.  If there are less than NUM sublevels do not move."
  (when (and (natnump sublevel) (natnump num) (> num 0))
    (let ((prefixlen (length (etask-get-infostring)))
          (pos (point))
          p endstr)
      (while (> num 0)
        (forward-line 1)
        (setq p (point))
        (setq endstr 
              (search-forward-regexp etask-delimiter-regexp nil t))
        (if (natnump endstr)
            (progn
              (setq str
                    (buffer-substring-no-properties
                     (+ p prefixlen) endstr))
              (when (= (etask-leading-char-num str ? ) sublevel)
                (setq num (1- num))
                (goto-char (1- (point)))))
          (goto-char pos)
          (etask-goto-delimiter)
          (setq num -1)))
      (if (< num -1) nil 0))))


;;; Module Initialization

(setq etask-cat-loaded-p t)
(provide 'etask-cat)


;;; etask-cat.el  end of file