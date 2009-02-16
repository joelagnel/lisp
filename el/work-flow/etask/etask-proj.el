;;; etask-proj.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum <rene (at) reneweichselbaum (dot) com>

;; $Id: etask-proj.el,v 1.19 2004/10/29 11:05:44 rene Exp $

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

;; This software component implements the EtaskMode project management
;; features.


;; _________________


;;; Code:

(defvar etask-task-alist nil "")

(defun etask-proj-insert-elements(project ixlist)
  "Add a new task to PROJECT at IXLIST.  Return inserted element or
nil if operation failed."
  (when (etask-cat-is-item-p etask-category-projectid project)
    (etask-proj-get-initial-elementdata
     project (etask-get-labels) ixlist)))

(defun etask-proj-edit-element(task)
  "Edit task data not covered by etask-cat-edit-element."
  task)

(defun etask-proj-insert-elementbar-p(element)
  "Return true if ELEMENT's bar is to insert into Gantt chart, nil
otherwise."
  (let ((chartstart (etask-state-get etask-stateid-chartstart))
        (chartend (etask-state-get etask-stateid-chartend)))
    (not (or (calendar-date-compare 
              (list
               (etask-db-get element etask-db-attr-taskend))
              (list chartstart))
             (calendar-date-compare 
              (list chartend)
              (list
               (etask-db-get element etask-db-attr-taskbegin)))))))

(defun etask-proj-get-initial-elementdata(proj tasknames ixlist)
  "Get all initial task data, store it at IXLIST, and return it."
  (when tasknames
    (let ((tasks
           (etask-cat-generate-default-element proj (car tasknames))))
      (when (null ixlist)
        (setq ixlist '(1)))
      (etask-cat-insert-element tasks ixlist 'checkuniqueness)
      (when (cdr tasknames)
        (setq tasks
              (cons tasks
                    (etask-proj-get-initial-elementdata
                     proj 
                     (cdr tasknames)
                     (nconc (butlast ixlist)
                            (list (1+ (car (last ixlist)))))))))
      tasks)))

(defun etask-proj-link-elements(markedtasks)
  "Link marked tasks according to lead or lag time values entered by
the user via minibuffer."
  (let* ((linklist (cdr markedtasks))
         (pretask (car markedtasks))
         (prename (etask-db-get pretask etask-db-attr-taskname))
         (preend (etask-db-get pretask etask-db-attr-taskend))
         (bizdaysp (etask-state-get etask-stateid-businessdays))
         succtask succbegin succend succduration daysbetwtasks
         linktask linkix)
    (while linklist
      (etask-goto-elementline (car (etask-cat-get-elementindex pretask)))
      (etask-cat-show-elements)
      (setq succtask (car linklist)
            linkix (etask-cat-get-elementindex succtask)
            succbegin (etask-db-get succtask etask-db-attr-taskbegin)
            succend (etask-db-get succtask etask-db-attr-taskend)
            succduration (if bizdaysp
                             (etask-business-days-between succbegin succend)
                           (etask-days-between succbegin succend)))
      (setq daysbetwtasks
            (string-to-number 
             (etask-read
              (concat
               (if bizdaysp
                   (etask-lang-msg 180 etask-language)
                 (etask-lang-msg 181 etask-language))
               (format " '%s' "
                       (etask-shorten-string 
                        prename
                        etask-longer-taskname-len-minibuf))
               (etask-lang-msg 704 etask-language)
               (format " '%s': "
                       (etask-shorten-string 
                        (etask-db-get succtask etask-db-attr-taskname)
                        etask-longer-taskname-len-minibuf)))
              (lambda (x) (string-match 
                           etask-integernumber-regexp x))
              (if (calendar-date-compare (list preend) (list succbegin))
                  (number-to-string
                   (if bizdaysp
                       (etask-business-days-between preend succbegin)
                     (etask-days-between preend succbegin)))
                (number-to-string
                 (if bizdaysp
                     (- 0 (etask-business-days-between preend succbegin))
                   (- 0 (etask-days-between preend succbegin))))))))
      (setq linktask
            (etask-db-set
             (etask-db-set succtask etask-db-attr-taskbegin
                           (if bizdaysp
                               (etask-add-businessdays-to-date
                                preend daysbetwtasks)
                             (etask-add-days-to-date
                              preend daysbetwtasks)))
             etask-db-attr-taskend
             (if bizdaysp
                 (etask-add-businessdays-to-date 
                  preend (+ daysbetwtasks succduration))
               (etask-add-days-to-date
                preend (+ daysbetwtasks succduration)))))
      (etask-cat-store-element linktask linkix)
      (setq pretask linktask)
      (setq prename (etask-db-get linktask etask-db-attr-taskname))
      (setq preend (etask-db-get linktask etask-db-attr-taskend))
      (setq linklist (cdr linklist)))
    (etask-cat-show-elements)))

(defun etask-proj-show-elementstatus(task &optional reportingp)
  "Print task status after `etask-statusheader'.  If REPORTINGP is
non-nil, output goes to the reporting file instead."
  (let* ((taskname (etask-db-get task etask-db-attr-taskname))
         (planned-hours (etask-simplify-number
                         (etask-db-get task etask-db-attr-peffort)))
         (expended-hours (etask-simplify-number
                          (etask-db-get task etask-db-attr-eeffort)))
         (open-hours 
          (if (> (- planned-hours expended-hours) 0)
              (etask-simplify-number
               (- planned-hours expended-hours))
            0))
         (planned-days 
          (if (= etask-workinghours-per-day 0)
              0
            (round (/ (float planned-hours) 
                      etask-workinghours-per-day))))
         (expended-days 
          (if (= etask-workinghours-per-day 0)
              0
            (round (/ (float expended-hours) 
                      etask-workinghours-per-day))))
         (open-days 
          (if (= etask-workinghours-per-day 0)
              0
            (round (/ (float open-hours) 
                      etask-workinghours-per-day))))
         (expendedpercent
          (if (= (+ open-hours expended-hours) 0)
              0
            (round (* 100
                      (/ (float expended-hours)
                         (+ open-hours expended-hours))))))
         (openpercent
          (if (= (+ open-hours expended-hours) 0)
              0
            (round (* 100
                      (/ (float open-hours)
                         (+ open-hours expended-hours))))))
         (percentformatstr
          (concat
           "%"
           (number-to-string (max 
                              (length (number-to-string expendedpercent))
                              (length (number-to-string openpercent))))
           "s"))
         (planned-days-rounded-p
          (if (or (not (natnump planned-hours)) 
                  (/= (% planned-hours etask-workinghours-per-day) 0))
              t
            nil))
         (open-days-rounded-p
          (if (or (not (natnump open-hours)) 
                  (/= (% open-hours etask-workinghours-per-day) 0))
              t
            nil))
         (expended-days-rounded-p
          (if (or (not (natnump expended-hours)) 
                  (/= (% expended-hours etask-workinghours-per-day) 0))
              t
            nil))
         (expendedpercent-rounded-p
          (and (> (+ open-hours expended-hours) 0)
               (not (natnump
                     (etask-simplify-number
                      (* 100
                         (/ (float expended-hours)
                            (+ open-hours expended-hours))))))))
         (openpercent-rounded-p expendedpercent-rounded-p)
         (expendedpercent-round-symbol
          (cond (expendedpercent-rounded-p
                 "~")
                (openpercent-rounded-p
                 " ")
                (t
                 "")))
         (openpercent-round-symbol
          (cond (openpercent-rounded-p
                 "~")
                (expendedpercent-rounded-p
                 " ")
                (t
                 "")))
         (planned-days-round-symbol
          (cond (planned-days-rounded-p
                 "~")
                ((or expended-days-rounded-p open-days-rounded-p)
                 " ")
                (t
                 "")))
         (open-days-round-symbol
          (cond (open-days-rounded-p
                 "~")
                ((or expended-days-rounded-p planned-days-rounded-p)
                 " ")
                (t
                 "")))
         (expended-days-round-symbol
          (cond (expended-days-rounded-p
                 "~")
                ((or open-days-rounded-p planned-days-rounded-p)
                 " ")
                (t
                 "")))
         (labelsformatstr
          (concat
           "%"
           (number-to-string 
            (max (length (etask-lang-msg 760 etask-language))
                 (length (etask-lang-msg 765 etask-language))
                 (length (etask-lang-msg 767 etask-language))))
           "s"))
         (hoursformatstr
          (concat
           "%"
           (number-to-string 
            (max (length (number-to-string planned-hours))
                 (length (number-to-string expended-hours))
                 (length (number-to-string open-hours))))
           "s"))
         (daysformatstr
          (concat
           "%"
           (number-to-string 
            (max (length (number-to-string planned-days))
                 (length (number-to-string expended-days))
                 (length (number-to-string open-days))))
           "s"))

         (hoursalignmentstr (etask-get-alignment-string 761 762))
         (daysalignmentstr (etask-get-alignment-string 763 764))

         (planned-hours-plural-p (etask-num-plural-s planned-hours))
         (planned-hours-align-spc
          (if (and (not planned-hours-plural-p)
                   (or (etask-num-plural-s open-hours) 
                       (etask-num-plural-s expended-hours)))
              hoursalignmentstr))

         (expended-hours-plural-p (etask-num-plural-s expended-hours))
         (expended-hours-align-spc
          (if (and (not expended-hours-plural-p)
                   (or (etask-num-plural-s open-hours) 
                       (etask-num-plural-s planned-hours)))
              hoursalignmentstr))

         (open-hours-plural-p (etask-num-plural-s open-hours))
         (open-hours-align-spc
          (if (and (not open-hours-plural-p)
                   (or (etask-num-plural-s expended-hours) 
                       (etask-num-plural-s planned-hours)))
              hoursalignmentstr))

         (planned-days-plural-p (etask-num-plural-s planned-days))

         (expended-days-plural-p (etask-num-plural-s expended-days))
         (expended-days-align-spc
          (if (and (not expended-days-plural-p)
                   (etask-num-plural-s open-days))
              daysalignmentstr))
               
         (open-days-plural-p (etask-num-plural-s open-days))
         (open-days-align-spc
          (if (and (not open-days-plural-p)
                   (etask-num-plural-s expended-days))
              daysalignmentstr))

         (timeremaining (etask-calculate-timetoplannedend task))
         (tracking (etask-db-get task etask-db-attr-tracking))
         (status)
         (maxtypelen
          (max (length (etask-lang-msg 740 etask-language))
               (length (etask-lang-msg 741 etask-language))
               (length (etask-lang-msg 742 etask-language)))))
    (setq status
          (concat
           (if reportingp
               (concat
                (etask-apply-face 
                 (concat
                  (etask-lang-msg 778 etask-language)
                  " '") 
                 'etask-face-task-on-report)
                (etask-apply-face taskname 'etask-face-task-on-report))
             (progn
               (concat
                (etask-apply-face (concat
                                   "'"
                                   taskname)
                                  'etask-face-statusheader-onscreen))))
           (if reportingp
               (etask-apply-face "'" 'etask-face-task-on-report)
             (etask-apply-face "'" 'etask-face-statusheader-onscreen))
           (if reportingp
               "\n")
           "\n  "
           (cond ((string= (etask-db-get task etask-db-attr-tasktype)
                           etask-normaltask-string)
                  (etask-apply-face     ;normal
                   (concat 
                    (etask-lang-msg 740 etask-language)
                    (make-string 
                     (- (1+ maxtypelen)
                        (length (etask-lang-msg 740 etask-language)))
                     ? ))
                   'etask-face-normaltask))
                 ((string= (etask-db-get task etask-db-attr-tasktype)
                           etask-highrisktask-string)
                  (etask-apply-face     ;high risk
                   (concat 
                    (etask-lang-msg 741 etask-language)
                    (make-string 
                     (- (1+ maxtypelen)
                        (length (etask-lang-msg 741 etask-language)))
                     ? ))
                   'etask-face-highrisktask))
                 ((string= (etask-db-get task etask-db-attr-tasktype)
                           etask-criticaltask-string)
                  (etask-apply-face     ;critical
                   (concat 
                    (etask-lang-msg 742 etask-language)
                    (make-string 
                     (- (1+ maxtypelen)
                        (length (etask-lang-msg 742 etask-language)))
                     ? ))
                   'etask-face-criticaltask))
                 (t
                  (make-string (1+ (length etask-statusheader)) ? )))
           (if (etask-is-milestone-p task)
               (calendar-date-string 
                (etask-db-get task etask-db-attr-taskbegin) t t)
             (concat
              (calendar-date-string 
               (etask-db-get task etask-db-attr-taskbegin) t t)
              " -- "
              (calendar-date-string 
               (etask-db-get task etask-db-attr-taskend) t t)
              "  ("
              (etask-calculate-business-days-string
               (etask-db-get task etask-db-attr-taskbegin)
               (etask-db-get task etask-db-attr-taskend))
              ", "
              (etask-calculate-fte-string task)
              ")"))
           "\n\n"
           (cond ((string= tracking "s-shape-65")
                  (concat
                   (etask-calculate-taskstatus-s-shape-65 task)
                   (if (etask-is-milestone-p task)
                       ""
                     (concat
                      "  "
                      (etask-lang-msg 600 etask-language)))))
                 ((string= tracking "s-shape-70")
                  (concat
                   (etask-calculate-taskstatus-s-shape-70 task)
                   (if (etask-is-milestone-p task)
                       ""
                     (concat
                      "  "
                      (etask-lang-msg 601 etask-language)))))
                 (t
                  (concat
                   (etask-calculate-taskstatus-linear task)
                   (if (etask-is-milestone-p task)
                       ""
                     (concat
                      "  "
                      (etask-lang-msg 602 etask-language))))))
           "\n\n"
           (if (etask-is-milestone-p task)
               ""
             (concat
              (format
               labelsformatstr (etask-lang-msg 760 etask-language))
              ": "
              (format hoursformatstr (number-to-string planned-hours))
              " "
              (if planned-hours-plural-p
                  (etask-lang-msg 762 etask-language)
                (etask-lang-msg 761 etask-language))
              planned-hours-align-spc
              " ("
              planned-days-round-symbol
              (format daysformatstr (number-to-string planned-days))
              " "
              (if planned-days-plural-p
                  (etask-lang-msg 764 etask-language)
                (etask-lang-msg 763 etask-language))
              (when (> planned-days 4)
                (concat
                 ", "
                 (number-to-string
                  (etask-simplify-number
                   (/ (float planned-days)
                      etask-workingdays-per-week)))
                 " weeks"))
              ")"
              "\n"
              (format
               labelsformatstr
               (etask-lang-msg 765 etask-language))
              ": "
              (format hoursformatstr (number-to-string expended-hours))
              " "
              (if expended-hours-plural-p
                  (etask-lang-msg 762 etask-language)
                (etask-lang-msg 761 etask-language))
              expended-hours-align-spc
              " ("
              expended-days-round-symbol
              (format daysformatstr (number-to-string expended-days))
              " "
              (if expended-days-plural-p
                  (etask-lang-msg 764 etask-language)
                (etask-lang-msg 763 etask-language))
              expended-days-align-spc
              " "
              (etask-lang-msg 766 etask-language)
              " "
              expendedpercent-round-symbol
              (format percentformatstr (number-to-string expendedpercent))
              "%)"
              "\n"
              (format
               labelsformatstr
               (etask-lang-msg 767 etask-language))
              ": "
              (format hoursformatstr (number-to-string open-hours))
              " "
              (if open-hours-plural-p
                  (etask-lang-msg 762 etask-language)
                (etask-lang-msg 761 etask-language))
              open-hours-align-spc
              " ("
              open-days-round-symbol
              (format daysformatstr (number-to-string open-days))
              " "
              (if open-days-plural-p
                  (etask-lang-msg 764 etask-language)
                (etask-lang-msg 763 etask-language))
              open-days-align-spc
              " "
              (etask-lang-msg 766 etask-language)
              " "
              openpercent-round-symbol
              (format percentformatstr (number-to-string openpercent))
              "%)\n\n"))
           (cond ((or (> timeremaining 1) 
                      (= timeremaining 0))
                  (concat 
                   (number-to-string timeremaining) 
                   " "
                   (etask-lang-msg 706 etask-language) ;days
                   " ("
                   (etask-calculate-business-days-string 
                    (calendar-current-date)
                    (etask-add-days-to-date
                     (calendar-current-date)
                     timeremaining)
                    'between)
                   ") "
                   (if (etask-is-milestone-p task)
                       (etask-lang-msg 770 etask-language) ;till milestone
                     (etask-lang-msg 768 etask-language)))) ;till deadline
                 ((= timeremaining 1)
                  (concat 
                   (number-to-string timeremaining) 
                   " "
                   (etask-lang-msg 720 etask-language) ;day
                   " ("
                   (etask-calculate-business-days-string
                    (calendar-current-date)
                    (etask-add-days-to-date
                     (calendar-current-date)
                     timeremaining)
                    'between)
                   ") "
                   (if (etask-is-milestone-p task)
                       (etask-lang-msg 770 etask-language) ;till milestone
                     (etask-lang-msg 768 etask-language)))) ;till deadline
                 ((= timeremaining -1)
                  (concat 
                   (number-to-string (abs timeremaining)) 
                   " "
                   (etask-lang-msg 720 etask-language) ;day
                   " ("
                   (etask-calculate-business-days-string
                    (calendar-current-date)
                    (etask-add-days-to-date
                     (calendar-current-date)
                     timeremaining)
                    'between)
                   ") "
                   (if (etask-is-milestone-p task)
                       (etask-lang-msg 771 etask-language) ;after milestone
                     (etask-lang-msg 769 etask-language)))) ;after deadline
                 (t
                  (concat 
                   (number-to-string (abs timeremaining)) 
                   " "
                   (etask-lang-msg 706 etask-language) ;days
                   " ("
                   (etask-calculate-business-days-string
                    (calendar-current-date)
                    (etask-add-days-to-date
                     (calendar-current-date)
                     timeremaining)
                    'between)
                   ") "
                   (if (etask-is-milestone-p task)
                       (etask-lang-msg 771 etask-language) ;after m
                     (etask-lang-msg 769 etask-language))))) ;after d
           (when (and (etask-is-milestone-p task)
                      (not reportingp))
             "\n\n\n\n ")))         ;space needed for etask-fit-window
    status))

(defun etask-proj-set-peffort()
  "Set planned effort for current task or, if there are marked tasks,
for all marked tasks according to values collected via minibuffer."
  (let* ((editlist (etask-get-ixlist-to-be-edited))
         element peffort oldpeffort newpeffort)
    (while editlist
      (setq element (etask-cat-get-element (car editlist)))
      (setq oldpeffort 
            (when (> (etask-db-get element etask-db-attr-peffort)
                     0)
              (etask-db-get element etask-db-attr-peffort)))
      (setq newpeffort 
            (etask-get-planned-effort element oldpeffort))
      (setq element
            (etask-db-set 
             (if (= newpeffort 0)       ;now a milestone, therefore:
                 (etask-db-set
                  (etask-db-set 
                   element
                   etask-db-attr-taskend ;taskend must be taskbegin
                   (etask-db-get 
                    element etask-db-attr-taskbegin))
                  etask-db-attr-eeffort ;peffort now 0 => eeffort 0 too
                  0)
               element)
             etask-db-attr-peffort
             (etask-simplify-number
              newpeffort)))
      (etask-cat-store-element element)
      (setq editlist (cdr editlist)))))

(defun etask-proj-create-tasks-alist()
  "Create associated list of task names used for completion reading."
  (let ((file (concat etask-working-dir etask-tasklist-filename)))
    (when (and (stringp file) (> (length file) 0)
               (file-readable-p file))
      (save-current-buffer
        (let ((l 1))
          (setq etask-task-alist nil)
          (set-buffer (find-file-noselect file))
          (goto-char (point-min))
          (while (not (eobp))
            (forward-line 0)
            (setq line (buffer-substring-no-properties 
                        (point) (re-search-forward "$")))
            (setq etask-task-alist
                  (cons
                   (list (etask-string-trim line) l)
                   etask-task-alist))
            (setq l (1+ l))
            (forward-line 1))))
      (setq etask-task-alist (nreverse etask-task-alist)))))

        
;;; Module Initialization

(setq etask-proj-loaded-p t)
(provide 'etask-proj)


;;; etask-proj.el  end of file