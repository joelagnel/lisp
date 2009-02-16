;;; fsdired.el --- sort files to go to different places in dired

;; Copyright (C) 2006 Joakim Verona

;; Filename: fsdired.el
;; Author: Joakim Verona joakim@verona.se
;; Version: 0.1 
;; Keywords: dired, environment, files, renaming

;; fsdired.el is free software

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

; Mark files and move them to different places with the help of dired.
;
; Use it, for instance, if you have a lot random files in a directory,
; and would like to move them to different places, on some criteria
; that you know, but not your computer. For instance, mark all comic
; book archive files with "1" and all text-files about motherboards
; with "2" and all text-files about dinosaurs with "3". Noodle about
; with this for a while, reviewing your marks, discovering new types
; of files in your disorganized random dir, and finally press "c-c" to
; pop-up a buffer where you decide where the files go. When decided,
; press c-c and the files go there!
;
;More precise insructions:
;
; - use "fsdired-change-to-fsdired-mode" to start, in a dired buffer
; - use numeric keys 0-9 to tag files in different groups
; - use C-c [0-9] to review you marks
;   - use g to restore the view
; - use c-c c-c to end. this will:
;   - switch back the marks buffer to  dired mode
;   - popup a command buffer where you can decide what to do with each group
;     there is a line for each group looking like:
;     "AB C", where A corresponds to a mark, B is a command, and C an argument for the command
;     commands(yeah only one for now):
;     M - move

;;;;;;;;;;;;;;;;;;;;;;;;
;Issues: and ideas 

;- currently nearly no error checking is done!
;
;  - ought to handle invalid format of the command buffer
;  - ought to handle missing command character definitions
;  - ought to report no matching command line in the command buffer for a mark
;  - some convenience functions to build the command buffer:
;    - command to fill with sane defaults(consider though that we might want to reuse command buffers)
;    - facility to enter a directory path conveniently
;- hide-lines.el is used when reviewing marks. would be
;  better to find some std (cvs) emacs facility
;- dont crash when destination exists
;- prompt for directory creation, if desired destionation doesnt exist
;- dont crash when "." is somewhere, mark or destinarion
;- before commiting command buffer, see to it that lines arent hidden
;- possibly hide/show lines automatically in dired buffer when moving in command buffer
;- the keymap set/restore mechanism seem to discard user customizations, why?


;how to hide lines:
(setq fsdired-has-hide-lines (require 'hide-lines nil t) )
;http://www.emacswiki.org/cgi-bin/wiki/HideLines
; - I try to fail gracefully if no hide-lines 



;I tried another method which didnt require an external package:
;  dired-do-kill-lines, set dired-marker-regexp
;  revert-bufer to restore
;this didnt work because revert-buffer doesnt restore marks on hidden lines


(defun fsdired-change-to-fsdired-mode ()
"start sorting files
enter from dired. 
"
  (interactive)
  (use-local-map fsdired-mode-map)
)

(defun fsdired-change-to-dired-mode ()
"quit sorting files"
  (interactive)
  "Change the mode back to dired."
  (use-local-map dired-mode-map)
)


(defun fsdired-mark-char (arg char)
  (let ((dired-marker-char char))
    (dired-mark arg)))


(defun fsdired-mark (arg)
  (interactive "P")
  (let
      ((new-char (string-to-char (this-command-keys))))
    (fsdired-mark-char arg new-char) 
    )
  )

 (defun fsdired-view-marked (arg)
   (interactive "P")
   (let*
       ((filter-char  (substring (this-command-keys) -1))
        (regexp (concat "^[^" (regexp-quote filter-char) "]")))
     (fsdired-revert-buffer nil)
     (if  fsdired-has-hide-lines    (hide-matching-lines regexp)   ) ;from hide-lines
     )
   )



(defun fsdired-revert-buffer (arg)
  (interactive "P")
  (if  fsdired-has-hide-lines    (show-all-invisible)  ) ;from hide-lines
)

;(defvar fsdired-mode-map nil)
;(unless fsdired-mode-map
  (setq fsdired-mode-map (copy-keymap dired-mode-map))
  (define-key fsdired-mode-map "1" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 1") 'fsdired-view-marked)
  (define-key fsdired-mode-map "2" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 2") 'fsdired-view-marked)
  (define-key fsdired-mode-map "3" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 3") 'fsdired-view-marked)
  (define-key fsdired-mode-map "4" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 4") 'fsdired-view-marked)
  (define-key fsdired-mode-map "5" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 5") 'fsdired-view-marked)
  (define-key fsdired-mode-map "6" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 6") 'fsdired-view-marked)
  (define-key fsdired-mode-map "7" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 7") 'fsdired-view-marked)
  (define-key fsdired-mode-map "8" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 8") 'fsdired-view-marked)
  (define-key fsdired-mode-map "9" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 9") 'fsdired-view-marked)
  (define-key fsdired-mode-map "0" 'fsdired-mark)
  (define-key fsdired-mode-map (kbd "C-c 0") 'fsdired-view-marked)

  (define-key fsdired-mode-map (kbd "C-c C-c") 'fsdired-open-process-marks-commandlist-buffer)
  (define-key fsdired-mode-map "g" 'fsdired-revert-buffer)
;)

(setq fsdired-command-buffer-mode-map (make-sparse-keymap))
(define-key fsdired-command-buffer-mode-map (kbd "C-c C-c") 'fsdired-process-marks)
(define-key fsdired-command-buffer-mode-map (kbd "C-c i") 'fsdired-insert-file-name)

(defvar fsdired-last-dired-buffer)

(defun fsdired-open-process-marks-commandlist-buffer (arg)
  "start processing marks, from given command argument buffer"
  (interactive "i")
  (fsdired-change-to-dired-mode) ; switch back to dired mode
  (setq fsdired-last-dired-buffer (current-buffer))
  (switch-to-buffer "*FSDIRED COMMAND BUFFER*")
  (use-local-map fsdired-command-buffer-mode-map)
)

(defun fsdired-process-marks (arg)
  "start processing marks, from given command argument buffer"
  (interactive "i")
  ; parse the command buffer
  ; figure out which dired buffer the marks are in
  ;  maybe fromb buffer local var, or just selecting the buffer
  ; call fsdired-process-marks-commandlist
  
  ; do the parse
  ; currently no error checking
    (save-excursion 
      (beginning-of-buffer)
      (setq fsdired-commandlist '())
      
      (let 
            ((exit-while nil))
        (while (not exit-while) ; this loop turned out kind of ugly
          (let ((beg (point)))
            (forward-line 1)
            (setq exit-while (eobp))
            (let* ((line (buffer-substring-no-properties beg (point)))
                   (mark-char (substring line 0 1))
                   (command-char (substring line 1 2))
                   (argument (substring line 3 -1))
                   )
              (setq  fsdired-commandlist (append fsdired-commandlist (list (list mark-char command-char argument ))))
               )
            )
          )
        )
      (message "commandlist:%s" fsdired-commandlist)
      )
    ;now select the buffer with the marks, and process them
    (save-excursion 
      (let ((bf (read-buffer "Buffer where marks are:" fsdired-last-dired-buffer t)))
        (set-buffer bf)
        (fsdired-process-marks-commandlist fsdired-commandlist)
        )
      )
    )

(defun fsdired-process-marks-commandlist (command-list)
  "this does the actual processing of marks"
  (mapcar (lambda (char-command-pair)
            (let* ((dired-marker-char (string-to-char (first char-command-pair)))
                   (files (dired-get-marked-files));now "files" are all the files marked with the current marker chair in the command-list
                   (command-char (second char-command-pair));this is the command char we are going to apply to the files
                   (argument (third char-command-pair))
                   )
              (fset 'mapcommand2 `(lambda (file) ( ,(intern (concat "fsdired-command-" command-char)) file argument) ))
              (message "commandchar:%s command:%s" command-char (symbol-function 'mapcommand2))
              (mapcar 'mapcommand2 files)
              )
            )
          command-list)
)

(defun fsdired-insert-file-name (arg)
  "ask for a filename and insert at point"
  (interactive "i")
  (insert (read-file-name "directory-name:"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; defuns matching command chars in the command buffer
; currently only "M" does anything useful
; "M" - move
; "C" - copy

(defun fsdired-command-M (file whereto)
  (message "mv '%s' '%s'" file whereto)
  (dired-rename-file file whereto nil)
)

(defun fsdired-command-C (file whereto)
  (message "dummy cp '%s' '%s'" file whereto)
)


