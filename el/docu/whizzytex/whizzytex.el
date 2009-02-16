;; whizzytex.el --- WhizzyTeX, a WYSIWIG environment for LaTeX
;; 
;; Copyright (C) 2001, 2002, 2003, 2004 INRIA.
;; 
;; Author         : Didier Remy <Didier.Remy@inria.fr>
;; Version        : 1.2.2
;; Bug Reports    : whizzytex-bugs@pauillac.inria.fr
;; Web Site       : http://pauillac.inria.fr/whizzytex
;; 
;; WhizzyTeX is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; WhizzyTeX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;; (enclosed in the file GPL).
;; 
;; See the file COPYING enclosed with the distribution.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  File whizzytex.el (Emacs-lisp code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;
;; WhizzyTeX is a minor Emacs mode for previewing LaTeX while your editing
;;
;; To install whizzytex, your also need its two companion files
;;   
;;   whizzytex             (A bash shell-script)
;;   whizzytex.sty         (A LaTeX macro package)
;;
;; The variable whizzy-command-name should be the relative or full path
;; of the executable shell-script, which should itself contain provide the
;; full or relative part of whizzytex.sty in its PACKAGE variable.
;;
;; To install whizzytex, then add to your ~/.emacs file:
;;
;;   (autoload 'whizzytex-mode "whizzytex"
;;        "WhizzyTeX, a minor-mode WYSIWYG environment for LaTeX" t)
;; 
;; To launch WhizzyTeX, tyoe Esc-x whizzytex-mode
;; This applies only to the current buffer.
;;
;; Some user variable control the setting.
;; See the documentation for details by typing (once autoloaded)
;; 
;;    Esc-x describe-function whizzytex-mode
;;


;;; History:
;; 

;;; Code:

(require 'comint)
(require 'timer)

(defconst whizzytex-version "1.2.2"
   "*This tells the version of WhizzyTeX emacs-mode.

It should be the same number as \"whizzytex\" shell script visible from the
unix PATH and \"whizzytex.sty\" visible from the TEXINPUTS path (this later
one is displayed on the welcoming page when launching whizzytex)

It should also be compatible with the version of Active-DVI, especially 
if your using advi gadgets---see the manual for details.")

;;;
(defgroup whizzytex nil
  "Incremental previewing of latex documents."
  :tag "WhizzyTeX"
  :prefix "whizzy"
  :group 'emacs
)

;;; Bindings
(defvar whizzytex-mode-hook 'whizzy-default-bindings
  "*Hook run when `whizzytex-mode' is turned on (before default setting).

By default it is equal to `whizzy-default-bindings' which setup useful 
bindings, including a menu bar.  If you wish to cancel default bindings, 
just add in your Emacs configuration file (usually ~/.emacs.el):

    (setq-default whizzytex-mode-hook nil)

To customize whizzytex, you can

    (setq-default whizzytex-mode-hook
       '(lambda () ... <your settings> ...))

where <your settings> should include `whizzy-default-bindings' if you wish
to keep default bindings, since your value of the hook will be used instead
of default one.  You can use `whizzy-unset-options' to unset most
default options.")

;;; User variables

(defvar whizzy-command-name "/usr//lib/whizzytex/whizzytex"
  "*Short or full name of the WhizzyTeX deamon.")

(defvar whizzy-viewers
  '(
    ("-advi" "advi -html Start-Document") ("-dvi" "xdvi")
    ("-ps" "gv")
    ("-pdf" "xpdf")
   )
  "*Alist defining accepted previewers and their default configuration.

The first element of the list (which should never be empty), defines the 
default previewer. Hence, you can use:

    (setq-default whizzy-viewers \'((\"-dvi\" \"xdvi\") (\"-ps\" \"gv\")))

to make xdvi the default previewer.

Each element of the alist is of the form

  <type> <command> <option list>

where

  <type> can only be \"-advi\", \"-dvi\", \"-ps\", or \"-pdf\"

  <command>

    is a string that defines the default previwer command for that previewer
    type, including its command line options, but the filename will be added
    by whizzytex.
  
  <option list>

    is a list of strings that defines the default options to pass to
    whizzytex for that previewer type. Liktely to be nil, since options
    passed to whizzytex are usually file dependant.
    (see `whizzy-write-configuration' for details)
    
Moreover, <type> and <default-command> should agree in the following way:

  -dvi and -advi

    tell whizzytex to produce DVI files and to use signal SIGUSR1 to tell
    the previewer to reload the file. Examples of commands are xdvi and advi.
    With -advi, the command need to be an ActiveDVI previewer.  See also
    `whizzy-line', and `whizzy-point-visible'.

  -ps

    tells whizzytex to produce Postscript files (using dvips) and to use
    signal SIGHUP to tell the previewer to reload the file. Example is gv.

  -pdf

    tells whizzytex to produce PDF files (using pdflatex) and to use
    xpdf to preview them and xpdf -reload to reload the file. 
    Currently, no other option if left for the previewer.


In addition -advi tell whizzytex to tell latex to dump source line number
and file name information into the DVI file as additional \\special commands
of the form

    line: <line number> <file name>


See also `whizzy-write-configuration'.")

(defvar whizzy-slicing-commands
  (list 'set-mark-command)
  "*List of commands that should force a slice, anyway.
Should remain small for efficiency...")

(defvar whizzy-noslicing-commands
  (list 'handle-switch-frame)
  "*List of commands that should always be ignored for slicing.
Should remain small for efficiency...")

(defvar whizzy-line t
  "*Tell whether DVI should be instrumented with source line numbers.

Only useful with advi type of previwers.

A click in the previewr can then be turn into a possition into the source
file (and a file), to which Emacs will jump.

This enables advi to turn pages automatically so as to follow the cursor, 
by inserting the HTML anchor Start-Document near the cursor.
To benefit from this feature, the previewer must be called with the option
 -html Start-Document.  

However, this makes WhizzyTeX more fragile, because the slice must be
instrumented with additiocal code, which at some position may unfortunaly
make the slice erroneous.

This can be toggled from the menu bar or with `whizzy-toggle-line'.")
(make-variable-buffer-local 'whizzy-line)

(defvar whizzy-point-visible t
  "*Make point visible (and refresh when not too busy). 

When the previewer is advi like, this also enables advi to turn pages
automatically, much as `whizzy-line' but in a most precise way. 

However, this makes WhizzyTeX more fragile, because the slice must be
instrumented with additiocal code, which at some position may unfortunaly
make the slice erroneous.

This can be toggled from the menu bar or with `whizzy-toggle-point'.

The value can also be function, which should then return  the position 
of where the point active char should be inserted or nil if point should 
not be printed  in the current slice.")

(make-variable-buffer-local 'whizzy-point-visible)

(defvar whizzy-paragraph-regexp "\n *\n *\n"
  "*Regexp for paragraph mode.")
(make-variable-buffer-local 'whizzy-paragraph-regexp)

(defvar whizzy-overlays
  (or (and (functionp 'make-overlay)
           (functionp 'delete-overlay)
           (functionp 'overlay-put))
      (and (condition-case nil (require 'overlay) (file-error))
           (functionp 'make-overlay)
           (functionp 'delete-overlay)
           (functionp 'overlay-put))
      )
  "*If true  WhizzyTeX will overlay LaTeX errors. 
In XEmacs, this requires the package 'overlay to be loaded.")

(defvar whizzy-pop-up-windows nil
  "*Local value of `pop-up-windows' for WhizzyTeX.
If set, WhizzyTeX can split windows when visiting new buffers.")

(defvar whizzy-auto-visit 'whizzytex
  "*Determine what WhizzyTeX should do when a slace buffer is visited.

Nil means nothing, 'whizzytex means turn WhizzyTeX mode on, 'ask means ask
using y-or-no before turning on, and t means visit the  buffer, but do
not turn WhizzyTeX mode on.")

(defvar whizzy-save-buffers nil
  "*Determine whether WhizzyTeX should save buffer when starting.
If set to 'ask will prompt the user with y-or-n, otherwise will save or
not according to the value of this variable."
)

(defvar whizzy-auto-raise t
  "*If true WhizzyTeX will raise the frame a WhizzyTeX buffer is visited.")

(defconst whizzy-mode-regexp-prefix
  "\n\\(%WHIZZY\\|\\\\begin{[A-Za-z]*}[ \n]*\\|\\)")

(defvar whizzy-mode-regexp-alist
  (append
   (list
    (cons 'paragraph whizzy-paragraph-regexp)
    (cons 'ocaml "^([*]")
    )
   (mapcar
    '(lambda (a) (cons (car a) (concat whizzy-mode-regexp-prefix (cdr a))))
   '(
     (letter .  "\\\\begin{letter}")
     (slide . "\\\\\\(overlays *{?[0-9*]+}? *{[% \t\n]*\\\\\\)?\\(begin *{slide.*}\\|newslide\\|Slide\\b\\)[^\n]*")
     (subsubsection
      .  "\\\\\\(\\(s\\(ubs\\(ub\\|\\)\\|\\)ection\\)\\|chapter\\|part\\)\\b[^{]*{")
     (subsection
      .  "\\\\\\(\\(s\\(ubs\\|\\)ection\\)\\|chapter\\|part\\)\\b[^{]*{")
     (section
      .  "\\\\\\(section\\|chapter\\|part\\)\\b[^{]*{")
     (chapter
      .  "\\\\\\(chapter\\|part\\)\\b[^{]*{")
     (document
      . "\\\\begin{document}\\(.*\n\\)+")
     (none . nil)
     )
   ))
  "*An alist defining slicing modes.

Each element is a pair (<mode> .  regexp) that defines for the regexp that
separates slices in mode <mode>.

If keyword whizzy-paragraph is defined in the header of the current buffer,
it overrides this regexp locally, even if the slicing mode is not paragraph.")

(defvar whizzy-class-mode-alist
  (list
    (cons "seminar" 'slide)
    (cons "letter"  'letter)
    (cons "article" 'section)
    (cons "book" 'chapter)
    )
  "*Alist mapping latex document class to slicing modes.
\(See also `whizzy-mode-regexp-alist')"
)

(defvar whizzy-select-mode-regexp-alist
  (list
    (cons "^ *\\\\begin *{slide}" 'slide )
    (cons "^ *\\\\begin *{letter}" 'letter )
    (cons "^ *\\\\\\(part\\)" 'chapter)
    (cons "^ *\\\\\\(\\(s\\(ubs\\(ub\\|\\)\\)ection\\)\\|chapter\\)" 'section)
   )
  "*Alist selecting modes from regexp.  Used to find mode automatically."
)

(defvar whizzy-auto-show-output t
  "*Control the display of the process buffer.

When true, the process buffer will be displayed when a latex error is
persistent, and hidden when the error disapears.")

;;; End of user variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Xemacs compatibility

(defun whizzy-sit-for (seconds &optional milliseconds display)
  ;; Emacs on Mac OS 10 does not allow milliseconds greater than 2000!
  (if milliseconds
      (setq seconds (+ seconds (/ milliseconds 1000))
            milliseconds (mod milliseconds 1000)))
  (sit-for seconds milliseconds display))

(defvar whizzy-xemacsp (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")
(cond
 (whizzy-xemacsp
  (require 'overlay)
  (defun window-buffer-height (&optional window)
    (save-selected-window
      (set-buffer (window-buffer window))
      (count-lines (point-min) (point-max))))
  (defun whizzy-sit-for (seconds &optional milliseconds display)
    (sit-for
     (if milliseconds (+ (float seconds) (/ (float milliseconds) 1000))
       seconds)
     display))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; More variables

(defconst whizzy-file-prefix "_whizzy_")
;; (defvar whizzy-speed-string "?")
;; (defvar whizzy-error-string nil)
(defvar whizzytex-mode-line-string
  (list " Whizzy" 
        '(:eval (whizzy-get whizzy-error-string)) 
        "." 
        '(:eval (whizzy-get whizzy-speed-string))))
;; those two should rather be part of the status--- to be fixed XXX
(make-variable-buffer-local 'whizzy-speed-string)
(make-variable-buffer-local 'whizzy-error-string)

;; A vector of whizzytex parameters, shared between all buffers related to the
;; same session. This variable is made buffer local, but its content is
;; shared between all related buffers.

(defvar whizzy-status nil 
  "*Vector of fields describing the status of the whizzytex process.
Each buffer where whizzytex is active contains a non nil value. 
Values of the vector are shared between all buffers contributing to the same 
document.  

These fiels are internal and should never be set the user, nor read, 
unless for debugging purposes. Fields are: 

 `whizzy-running' `whizzy-process' `whizzy-process-window'
 `whizzy-master-buffer' `whizzy-active-buffer' `whizzy-process-buffer'
 `whizzy-dir' `whizzy-input-dir' `whizzy-output-dir' `whizzy-filename'
 `whizzy-basename' `whizzy-slicename' `whizzy-slaves'
 `whizzy-slice-start' `whizzy-slice-time' `whizzy-slice-date'
 `whizzy-slice-fed' `whizzy-slicing-mode' `whizzy-view-mode'
 `whizzy-counters' `whizzy-layers' `whizzy-log-buffer' `whizzy-custom'

Fields can be read with `whizzy-get' (and set with `whizzy-set'). 
")
(make-variable-buffer-local 'whizzy-status)
(put 'whizzy-status 'permanent-local t)

;; fields of the status vector

(defconst whizzy-running 0
  "Field of `whizzy-status' describing the status of the WhizzyTeX process. 
nil means that the process is terminated.
Otherwise, it should be an integer in the set 0 1 2 3, 
no error, an error in the slice, error in the whole document, or an error 
in format.")
(defconst whizzy-master-buffer 1)
(defconst whizzy-active-buffer 2
  "Field of `whizzy-status' pointing to the buffer that wrote the last slice.")
(defconst whizzy-process-buffer 3
  "Field of `whizzy-status' pointing to the process buffer.")
(defconst whizzy-process-window 4
  "Field of `whizzy-status' when set pointing to the process buffer window.")
(defconst whizzy-process 5)

(defconst whizzy-dir 7)
(defconst whizzy-input-dir 8)
(defconst whizzy-output-dir 9)
(defconst whizzy-filename 10)
(defconst whizzy-basename 11)
(defconst whizzy-slicename 12)
(defconst whizzy-slaves 13)

(defconst whizzy-slice-start 14)
(defconst whizzy-slice-time 15)
(defconst whizzy-slice-date 16)
(defconst whizzy-slice-fed 18)

(defconst whizzy-slicing-mode 19)
(defconst whizzy-view-mode 20)

(defconst whizzy-counters 21)
(defconst whizzy-layers 22)
(defconst whizzy-log-buffer 23)
(defconst whizzy-custom 24)
(defconst whizzy-configuration-loaded 26)
(defconst whizzy-debug 25)
(defconst whizzy-initialized 26)
(defconst whizzy-error-string 27)
(defconst whizzy-speed-string 28)

(defconst whizzy-length 30)
(defun whizzy-get (f)
  (if whizzy-status (elt whizzy-status f)
     ;; (error "whizzy-get")
     ))
(defun whizzy-set (f v)
  (if whizzy-status (aset whizzy-status f v)
    (error "Attempting to set field %d when whizzy-status is nil" f)))

(defun whizzy-describe (f)
  (interactive "S")
  (with-output-to-temp-buffer "*Help*"
    (print (whizzy-get f))))
      
;; Tell if buffer is a slave and give its relative name to the master dir

(defvar whizzy-slave nil
  "*Indicates relative filename of the slave from the master-file directory.
A cons means that the file is macro file and the path to the master 
file is its cdr. nil means that the file is a master file.")
(make-variable-buffer-local 'whizzy-slave)
(put 'whizzy-slave 'permanent-local t)

;; BEGIN functions for maping source section headers to LaTeX counters.

;; Make a join between two maps sections-to-lines (computeted by emacs) 
;; and lines-to-counters (computed by latex). 

(defvar whizzy-section-regexp
  "^\\\\\\(subsubsection\\|subsection\\|section\\|chapter\\|part\\)[^\n]*$"
  "*Regexp for sectionning command.
Used to trace section and reset counters at the beginning of a slice.")

(defvar whizzy-section-counters-alist nil
  "Structure mapping source file sections to LaTeX counters.
Its csr is tha alist. Its car is  a pointer to the list used for last update.")
(make-variable-buffer-local 'whizzy-section-counters-alist)

(defvar whizzy-line-section-alist nil
  "Variable to remember line numbers of sections in the source file.
Should be consistent with the file on disk.")
(make-variable-buffer-local 'whizzy-line-section-alist)

(defun whizzy-save-section-lines ()
  (if (or
       ;; this is a macro file
       (consp whizzy-slave) 
       ;; latex will not compute sections for slides
       (equal (whizzy-get whizzy-slicing-mode) 'slide))
      nil
    (save-excursion
      (goto-char (point-min))
      (let ((all) (last (point)) (line 1))
        (while
            (or (re-search-forward whizzy-section-regexp (point-max) t)
                (re-search-forward "^\\\\\\(end\\) *{document}"
                                   (point-max) 'move))
          (setq line
                (+ line (count-lines last (setq last (match-beginning 0)))))
          (setq all (cons (cons line (match-string 0)) all)))
        (setq whizzy-line-section-alist (nreverse all)))
      )))

;; compare-strings not defined in XEmacs
(defun whizzy-relname (dir path)
  "Check that dir is a prefix of path and returns the suffix."
  (let ((ldir (length dir)) (lpath (length path)))
    (and (> lpath ldir)
         (equal dir (substring path 0 ldir))
         (substring path ldir nil))))

(defun whizzy-section-counters (&optional arg)
  "Returns  counters at section ARG.
Uses `whizzy-section-counters-alist' (and recompile it if needed).
Return nil if section is not found unless ARG is t in which case 
it return counters at top of file. 
If ARG is nil, just recompiles it and return t if the 
alist is not empty, and nil otherwise."
  (let ((all-counters (whizzy-get whizzy-counters)))
    (unless (eq (car whizzy-section-counters-alist) all-counters)
      ;; recompilation is needed
      (let* ((top-counters
              (and (consp all-counters)
                   (cdr (assoc (or (whizzy-relname (whizzy-get whizzy-dir)
                                                   buffer-file-name)
                                   (error "Slave if not a subdir of master"))
                               all-counters))))
             (top (car top-counters))
             (counters (cdr top-counters))
             (sections whizzy-line-section-alist)
             (all))
        ;; do the join
        (while (and sections counters)
          (cond
           ((= (caar sections) (caar counters))
            (let ((left (cdar sections)) (right (cdar counters)))
              (if (equal (string-match (car right) left) 1)
                  (setq all (cons (cons left (cdr right)) all))))
            (setq sections (cdr sections))
            (setq counters (cdr counters))
            )
           ((< (caar sections) (caar counters))
            (setq sections (cdr sections))
            )
           ((> (caar sections) (caar counters))
            (setq counters (cdr counters))
            )))
        ;; set the result
        (setq whizzy-section-counters-alist
              (cons all-counters (cons top all))))
        ))
    (let ((section-before (cddr whizzy-section-counters-alist))
          (section-after))
      (if arg
          (if (equal arg t)
              (setq section-after (reverse section-before)
                    section-before nil)
            (while
                (and (consp section-before)
                     (not (string-equal arg (caar section-before))))
              (setq section-after
                    (cons (car section-before) section-after)
                    section-before (cdr section-before)) 
              )))
      (cons (cons section-before section-after)
            ;; note that sections are in reverse order
            (and section-before
                 (concat "\\WhizzySlice " (cddr (car section-before))))
            ))
    )

(defun whizzy-read-sections (&optional arg)
  (if (or (whizzy-get whizzy-counters) arg)
      (save-excursion
        (set-buffer (whizzy-get whizzy-master-buffer))
        (let* ((filename (concat (whizzy-get whizzy-dir)
                                 (whizzy-get whizzy-basename) ".pag"))
               (tmp) (list) (all) (elem) (last))
          (if (file-exists-p filename)
              (save-excursion
                (set-buffer (get-buffer-create "*load*"))
                (insert-file-contents filename)
                (while (re-search-forward
                        "^\\(.*\\):\\([0-9][0-9]*\\):\\(.*\\)\@\\(\\([^.]*\\).*\\)$"
                        (point-max) 'move)
                  (let ((new
                         (cons (match-string 1)
                               (cons (string-to-number (match-string 2))
                                     (cons (match-string 3)
                                           (cons
                                            (string-to-number (match-string 5))
                                            (match-string 4))
                                           )))))
                    (if (or (null last) (equal (car last) (car new))) nil
                      (setq all
                            (cons (cons (car new) (cdr (cddr last))) all)))
                    (setq all (cons (setq last new) all))))
                (erase-buffer))
            (message "File %s not found" filename))
          ;; (setq all (nreverse all))
          (setq all (sort all '(lambda (a b) (string< (car a) (car b)))))
          ;; building two-level association list. 
          (while all
            (setq elem (caar all))
            (while (string-equal (caar all) elem)
              (setq last (cdar all))
              (if (consp last) (setq list (cons last list)))
              (setq all (cdr all)))
            (setq tmp 
                  (cons (cons elem (cons (and (stringp last) last) list))
                        tmp)))
          (whizzy-set whizzy-counters tmp)
          ;; this will invalidate all local values of whizzy-section-counters
          ))))

;; END of functions for maping source section headers to LaTeX counters.

(defun whizzy-start-process (args)
  (let ((buf (concat "*" (buffer-name) "*"))
        (status whizzy-status))
    (if (get-buffer buf)
        (save-excursion
          (set-buffer buf)
          (erase-buffer)
          (setq whizzy-status status)
          (whizzy-set whizzy-initialized nil)
          (cd (whizzy-get whizzy-dir))
          (if (get-buffer-process buf)
              (progn
                (quit-process (get-buffer-process buf))
                (if (get-buffer-process buf)
                    (progn 
                      (message "Did not quit on QUIT. Sending KILL")
                      (kill-process (get-buffer-process buf)))
                  (message "Quitted"))
                ))))
    (setq buf (apply 'make-comint (buffer-name)
                     whizzy-command-name nil args))
    (whizzy-set whizzy-process-buffer buf)
    (whizzy-set whizzy-process (get-buffer-process buf))
    (save-excursion
      (set-buffer buf)
      (setq whizzy-status status)
      (setq comint-output-filter-functions
            (list (function whizzy-filter-output)))
      )
    ))
 
; Neither works correctly, so better use the global value, 
; and test for local-mode. 
; (make-variable-buffer-local 'write-region-annotate-functions)
; (make-local-hook 'write-region-annotate-functions)
(defvar whizzy-write-at-begin-document nil)

(defun whizzy-backward-comment ()
  (let ((here (point)))
    (if (looking-at "%") (forward-char 1))
    (while (re-search-backward "[^\\\\]%[^\n]*\\=" (- here 100) t)
      (or (looking-at "\n%") (forward-char 1)))
    (if (and (looking-at "^\n%") (< (point) here)) (forward-char 1))
    (<  (point) here)))

(defun whizzy-in-comment-p ()
  (save-excursion (whizzy-backward-comment)))

(defun whizzy-backward-space ()
  (let ((here (point)))
    (skip-chars-backward " \t")
    (while (and (looking-at "^[ \t]*\\([^ \t\n]\\|%\\)")
                (> (point) (point-min))
                (progn (backward-char 1) t))
      (whizzy-backward-comment)
      (skip-chars-backward " \t")
      )
    (if (< (point) here)
        (if (and (looking-at "^[ \t]*\n") (<= (match-end 0) here))
            (goto-char (match-end 0))
          t)                        
    (looking-at "[ \t]")
    )))

(defun whizzy-backward-word ()
  (let ((here (point)))
    (or (and (skip-syntax-backward "w.")
             (looking-at "\\(\\sw\\|\\s.\\)"))
        (progn (goto-char here) nil))
    ))

(defun whizzy-backward-command ()
  (let ((here (point)) begin)
    (whizzy-backward-space)
    (re-search-backward "\\\\\\([^a-z@A-Z]?\\|[a-z@A-Z]*\\)\\="
                        ;; "\\\\\\([^a-zA-Z]?\\|[a-zA-Z]*\\)\\="
                        (- (point) 100) t)
    (setq begin (point))
    (skip-chars-backward "\\\\")
    (re-search-forward "\\(\\\\\\\\\\)" begin t)
    (or (and (looking-at "\\\\\\([^a-zA-Z]\\|[a-zA-Z]+\\)")
             (match-end 0))
        (progn (goto-char here) nil)
        )))

(defun whizzy-in-command-p ()
  (save-excursion (whizzy-backward-command)))

(defun whizzy-in-fragile-arg-p ()
  (or
   (save-excursion
     (and (re-search-backward
           "{\\([^{} \t\n]*{[^{} \t\n]*}\\)*[^{} \t\n]*\\=" (- (point) 100) t)
          (looking-at  "{\\([^{} \t\n]*{[^{} \t\n]*}\\)*[^{} \t\n]*}")))
   (save-excursion
     (and (re-search-backward "\\(\\[\\|^ *\\)[^][ \t\n]*\\="
                              (- (point) 100) t)
          (looking-at  "\\(\\[\\|^ *\\)[^][ \t\n]*\\(\\]\\|[^\\\\]%\\)"))
     )
   ))

(defun whizzy-between-args-p ()
  (save-excursion
    (and (looking-at "[ \n\t][ \t]*[[{]")
         (whizzy-backward-space)
         (or (and (< (skip-chars-backward "}]") 0)
                  (looking-at "[]}][ \t]*[ \t\n][ \t]*[[{]"))
             (whizzy-backward-command)))
    ))

(defun whizzy-dimen-p ()
  (looking-at
   "[ \t]*\\(by +\\|to +\\)?[+-]?\\([0-9]*[.,]\\)?[0-9]+ *[epibcmds][xtcnmpdu]"
   ;; em ex pc in mm cm pc pt ...
  ))

(defun whizzy-backward-arg ()
  (let ((here (point)))
    (whizzy-backward-space)
    (or (and (re-search-backward "}\\=" (- (point) 100) t)
             (= (mod (skip-chars-backward "\\\\") 2) 0)
             (and (re-search-backward
                   "[^\\\\]{[^{}]*\\=" (- (point) 400) t)
                  (progn (forward-char 1) t))
             )
        (progn (goto-char here) nil))))

(defun whizzy-backward-script ()
  (let ((here (point)))
    (skip-chars-backward " \t")
    (or (< (skip-chars-backward "_^" (- (point) 1)) 0)
        (progn (goto-char here) nil))
    ))

(defun whizzy-backward-atom ()
  (let ((here (point)) (limit))
    (whizzy-backward-space)
    (setq limit (- (point) 100))
    (cond
     ((re-search-backward "\\\\\[[]()]\\=" limit t) 'math)
     ((re-search-backward "[^\\\\]#[1-9]?\\=" limit t)
      (forward-char 1) 'param)
     ((whizzy-backward-arg) 'arg)
     ((whizzy-backward-script) 'script)
     ((and (whizzy-backward-command) (< (point) here)) 'command)
     ((re-search-backward "[^\\\\$]\\$?\\$\\=" limit t)
      (forward-char 1) 'math)
     ((< (skip-syntax-backward "w." (- (point) 1)) 0) 'letter)
     ((< (skip-syntax-backward "_" (- (point) 1)) 0) 'symbol)
     ((< (skip-chars-backward "[][(){]" (- (point) 1)) 0) 'symbol)
     ((re-search-backward "[^ \t\n][ \t\n]*\n\\=" (- (point) 200) t)
      (forward-char 1) 'par)
     (t 'unknow)
     )))

(defun whizzy-backward-safe ()
  (save-excursion
    (if  (whizzy-dimen-p) nil
      (whizzy-backward-space)
      (let ((here (point))
            (back (whizzy-backward-atom)))
        (cond
         ((equal back 'math) t)
         ((equal back 'par) t)
         ((equal back 'param) nil)
         ((equal back 'script) 'script)
         ((equal back 'symbol) t)
         ((equal back 'letter)
          (skip-syntax-backward "w")
          (not (whizzy-dimen-p)))
         ((equal back 'arg)
          (and (< (point) here) (whizzy-backward-safe)))
         ((equal back 'command)
          (whizzy-backward-script))
         )))))

;; variables for slicing

(defvar whizzy-last-tick 0)
(defvar whizzy-last-saved 0)
(defvar whizzy-last-point 0)
(defvar whizzy-last-slice-begin 0)
(defvar whizzy-last-slice-end 0)
(defvar whizzy-last-layer 0)
(make-variable-buffer-local 'whizzy-last-tick)
(make-variable-buffer-local 'whizzy-last-saved)
(make-variable-buffer-local 'whizzy-last-point)
(make-variable-buffer-local 'whizzy-last-slice-begin)
(make-variable-buffer-local 'whizzy-last-slice-end)
(make-variable-buffer-local 'whizzy-last-layer)

;; replaces whizzy-show-position

(defun whizzy-show-point ()
  (save-excursion
    (if (looking-at "[ \t]\\|^[ \t]*$") (whizzy-backward-space))
    (let ((back))
      (cond
       ;; do not put cursor inside fragile args
       ;; unless at the beginning of document
       ((equal (point) whizzy-last-slice-begin))
       ;; useless if in comment...
       ((whizzy-in-comment-p))
       ;; (that maybe arguments of  \label, \cite, \begin, \end etc.)
       ((whizzy-in-fragile-arg-p))
       ;; do not put cursor between args
       ((whizzy-between-args-p))
       ;; difficult case: commands
       (t
        (if (looking-at "\\(\\\\\\|\\sw\\)") (forward-char 1))
        (setq back (whizzy-backward-atom))
        ;; (message "[%S]" back)
        (cond
         ((equal back 'math) (point))
         ((equal back 'par)
          (skip-chars-forward " \t\n") (point))
         ((equal back 'script)
          (cond
           ((looking-at "[_^][ \t]*\\([A-Za-z0-9]\\|\\\\[A-Za-z]+\\)")
            (list (cons (match-beginning 1) "{")
                  (cons (match-end 1) "}")))
           ((looking-at "[_^][ \t]*{") (match-end 0))
           ))
         ((equal back 'arg))
         ((or (equal back 'command) (equal back 'letter))
          (let ((tmp (whizzy-backward-safe)))
            (cond
             ((and (equal tmp 'script)
                   (looking-at "\\\\[A-Za-z]+\\|[A-Za-z0-9]"))
              (list (cons (match-beginning 0)
                          "{") (cons (match-end 0) "}")))
             (tmp (point))
             )))
         ))
       ))))

(defun whizzy-show-point-safer ()
  (save-excursion
    (let ((here (point)) (tmp) (spaces))
      (setq spaces (skip-syntax-backward "w"))
      (cond
       ;; nil if it could be a command
       ((< (skip-chars-backward "\\\\") 0) nil)
       ;; fine if it starts a paragraph
       ((and (< (setq tmp (skip-chars-backward " \t\n")) -1)
             (looking-at "[ \t]*\n[ \t]*\n"))
        here)
       ;; so far, we have skip letters and all space backward
       ;; file if it end/starts math, could extend to latex versions.
       ((< (setq tmp (skip-chars-backward "$")) -1)
        (and (> tmp -3) (= (skip-chars-backward "\\\\") 0)
             here))
       ;; fine if there is at least another word
       ((and (setq tmp (point))
             (skip-chars-backward ",;:--.!?()")
             (< (skip-syntax-backward "w") 0)
             (skip-chars-backward ",;:--.!?()")
             (< (skip-chars-backward " \t\n") 0)
             (< (+ tmp spaces) -1)
             )
        here)
       ;; Otherwise nil
       ))))

(defun whizzy-test-show ()
  (let ((there
         (if (functionp whizzy-point-visible)
             (funcall whizzy-point-visible)
           (whizzy-show-point))))
    (if (numberp there) (whizzy-overlay-region (- there 1) there)
      (if (consp there)
          (whizzy-overlay-region (car (car there)) (car (cadr there)))
      (whizzy-delete-error-overlay)))))

; slicing

(defvar whizzy-use-write-annotate t
  "*Tells whether `write-region-annotate-functions' can be used.

This is the normal way to append annotations strings during write, 
which is used during slicing, by default.  However, some packages, 
such as x-symbol, do not treat this properly. 

When set to nil, slicing proceeds by inserted the annotations before 
write and deleting then after.

Calls `call-with-transparent-undo' which assumes version 21 or above.")

(defun whizzy-write-slice (from to &optional local word)
  (unless (or (< (point) from) (>= from to) (> (point) to))
    (if local
        (if (or whizzy-use-write-annotate
                (not (functionp 'call-with-transparent-undo)))
            ;; we insert annotations on the fly
            (let ((write-region-annotate-functions
                   (cons 'whizzy-write-region-annotate
                         write-region-annotate-functions))
                  (whizzy-write-at-begin-document word))
              (condition-case nil
                  ;; stange thinks happens at startup: the file is written
                  ;;  but not seen
                  (write-region from to (whizzy-get whizzy-slicename)
                                nil 'ignore)
                (quit 
                 (message "Quit occured during slicing has been ignored"))))
          ;; insert and delete
          (setq whizzy-write-at-begin-document word)
          (let ((insertions (whizzy-write-region-annotate from to))
                (shift 0))
            (save-excursion 
              (call-with-transparent-undo
               (lambda ()
                 (while (consp insertions)
                   (goto-char (+ (caar insertions) shift))
                   (insert (cdar insertions))
                   (setq shift (- (point) (caar insertions)))
                   (setq insertions (cdr insertions)))
                 (write-region from (+ to shift) (whizzy-get whizzy-slicename)
                               nil 'ignore)
                 ))
              )))
      ;; there are no annotatinos
      (write-region from to (whizzy-get whizzy-slicename) nil 'ignore))
    (whizzy-wakeup)))

(defvar whizzytex-mode nil)
(make-variable-buffer-local 'whizzytex-mode)

(let ((mode (assq 'whizzytex-mode minor-mode-alist))
      (value  (list whizzytex-mode-line-string)))
  (if mode (setcdr mode value)
    (setq  minor-mode-alist
          (cons (cons 'whizzytex-mode value) minor-mode-alist))))


;; revert-buffer kills mode and local variables. 
;; it would be nicer to keep the same session running, 
;; but significant rewiring would be needed. 

; (defun whizzy-before-revert () 
;   "Function to be called before reverting a buffer."
;   (if (equal whizzytex-mode t)
;       (progn
;         (whizzy-mode-off)
;         (make-local-hook 'after-revert-hook)
;         (add-hook 'after-revert-hook 'whizzytex-mode)
;         ;; this hook will normally be removed by revert-buffer 
;         ;; info could be passed through a global variable to increase safety
;         )))
(defvar whizzy-local-variables 
  '(whizzy-status
    whizzy-slice-overlay whizzy-error-overlay whizzy-configuration 
    whizzy-begin whizzy-last-tick whizzy-line-section-alist 
    whizzytex-mode whizzy-section-counters-alist whizzy-last-slice-begin 
    whizzy-last-slice-end whizzy-last-point whizzy-last-layer 
    whizzy-last-saved)
  "List of WhizzyTeX buffer local variables."
  )

(defvar whizzy-temp-for-revert-buffer nil)
(defun whizzy-before-revert ()
  (setq whizzy-temp-for-revert-buffer
        (mapcar '(lambda (atom) (cons atom (symbol-value atom)))
                whizzy-local-variables))
  (setq whizzytex-mode nil)
  (setq whizzy-status nil)
  (remove-hook 'before-revert-hook 'whizzy-before-revert t)
  (remove-hook 'kill-buffer-hook 'whizzy-mode-off t)
  (remove-hook 'after-save-hook 'whizzy-after-save t)
  (remove-hook  'post-command-hook 'whizzy-observe-changes t)
  (make-local-hook 'after-revert-hook)
  (add-hook 'after-revert-hook 'whizzy-after-revert t t)
)

(defun whizzy-after-revert ()
  (unless (null whizzy-temp-for-revert-buffer)
    (remove-hook 'after-revert-hook 'whizzy-after-revert t) 
    (mapcar '(lambda (elem) (set (car elem) (cdr elem)))
            whizzy-temp-for-revert-buffer)
    (setq whizzy-temp-for-revert-buffer nil)
    (add-hook 'before-revert-hook 'whizzy-before-revert t t)
    (add-hook 'kill-buffer-hook 'whizzy-mode-off t t)
    (add-hook 'after-save-hook 'whizzy-after-save t t)
    (add-hook  'post-command-hook 'whizzy-observe-changes t t)
    ))



;; Code for slicing

(defun whizzy-customize (arg)
  "Customize the slice with TeX-source code to executed at the beginning

ARG should be a string or nil. 
Interactively prompt for a string, or set it to nil if prefix argument 
numeric value is 0. 

For example, 

        '(whizzy-customize \"\\\\large\")

will preview the document in larger font, which you can later cancel with

        '(whizzy-customize nil)

For permanent customization, you may use a file whizzy.sty in the current
latex path, which is automatically loaded when WhizzyTeX-ing the document. 
(See the WhizzyTeX manual)
"
  (interactive "p")
  (if (not whizzy-status)
      (error "This file has not been setup for WhizzyTeX-ing")
    (whizzy-set 
     whizzy-custom
     (if (stringp arg) arg
       (if (equal arg 0) nil
         (read-string "Custom (TeX source): "
                      (whizzy-get whizzy-custom))
         )))
    (setq whizzy-last-tick 0)
    ))

(defvar whizzy-sync-lines t
  "*When set newlines are inserted so that line numbers of the slice
match those of the file")

(defun whizzy-write-region-annotate (start end)
  ;; check for whizzytex-mode since it appears in a global hook, even though
  ;; it should be removed from the hook immediately
  (if whizzytex-mode
      (let ((empty-lines (count-lines (point-min) start))
            (full-lines (count-lines start (point)))
            (word (and whizzy-point-visible
                       (if (functionp whizzy-point-visible)
                           (funcall whizzy-point-visible)
                         (whizzy-show-point)))
                  ))
        ;; (message "WWABD=%S word="%S"  whizzy-write-at-begin-document word)
        (let ((line
               (if whizzy-line          ; (not whizzy-point-visible)
                   (concat
                    "\\WhizzyLine" (if (consp word) nil "Point") "{"
                    (int-to-string (+ empty-lines full-lines))
                    "}")))
              )
          (append
           (list
            (cons start
                  (concat
                   "\\begin{document}\\WhizzySkip"
                   (if (stringp whizzy-slave)
                       (concat "\\WhizzyMaster{" whizzy-slave "}")
                     (concat "\\SourceFile{" (whizzy-get whizzy-filename) "}"))
                   whizzy-write-at-begin-document
                   (if whizzy-sync-lines (make-string empty-lines 10)
                     (concat "\\Addtolineno " (number-to-string empty-lines)))
                   (whizzy-get whizzy-custom)
                   "\\WhizzyStart{}" line "\\relax ")))
           (cond ((and (numberp word) (< word end)) (list (cons word "")))
                 ((consp word) word)) 
           (list (cons end "\n\\end{document}\n")))
          )
        )))


(defun whizzy-call (command &optional args)
  (if (and whizzytex-mode (whizzy-get whizzy-running))
      (let ((p (whizzy-get whizzy-process)))
        (if (and p (processp p))
            (if (> (process-exit-status p) 0)
                (whizzy-mode-off)
              (continue-process p)
              (process-send-string 
               p (concat
                  (if (equal args 'filename)
                      (concat command " "
                              (cond
                               ((stringp whizzy-slave) whizzy-slave)
                               ((consp whizzy-slave) (cdr whizzy-slave))
                               (t  (whizzy-get whizzy-filename))))
                    command)
                    "\n")))
          ))))



;; regexp beginning a slice
(defvar whizzy-begin nil)
(make-variable-buffer-local 'whizzy-begin)

(defun whizzy-local ()
  (not (equal (whizzy-get whizzy-slicing-mode) 'none)))

(defun whizzy-backward (&optional n)
  (if (re-search-backward whizzy-begin (point-min) t (or n 1))
      (let ((max (match-end 0)))
        (if (re-search-backward whizzy-begin (point-min) 'move)
            (goto-char (match-end 0)))
        (re-search-forward whizzy-begin max t)
        (goto-char (match-beginning 0))
        )))

(defvar whizzy-slice-pages 0
  "*Recommended number of pages per slice.
WhizzyTeX will try to extend the slice before and after the default slice
to get that number of pages without exeeding it. It never reduces the default
slice, which can be changed by `whizzy-change-mode'.  
In particular, a value of zero cancel this facility.

Pages are estimated from the last recompilation of the full document,
so this may not be exact. 

Can be set with `whizzy-slice-adjust' and from entry adjust in menu Slicing."
  )

;; We could cash the slice region with marks.
;; and invalidate the cash when reloading section numbers.
;; (defvar whizzy-slice-region-begin (make-marker))
;; (defvar whizzy-slice-region-end (make-marker))
;; (defvar whizzy-last-slice-word nil)

(defun whizzy-slice (layer)
  (if (> (whizzy-get whizzy-slice-time) 0)
      (whizzy-set whizzy-speed-string
            (int-to-string (/ (whizzy-get whizzy-slice-time) 100))))
  (if (equal (whizzy-get whizzy-slicing-mode) 'none) ; no slice
      (whizzy-write-slice (point-min) (point-max))
;;     (if (and whizzy-last-slice-word
;;              (not layer)
;;              ;; ensures markers are set
;;              (equal (marker-buffer whizzy-slice-region-begin)
;;                     (current-buffer))
;;              (<= whizzy-last-position-begin
;;                  (marker-position whizzy-slice-region-begin))
;;              (<= whizzy-last-position-begin (point))
;;              (<= (point) whizzy-last-position-begin)
;;              (<= whizzy-last-position-begin
;;                  (marker-position whizzy-slice-region-end))
;;              )
;;         nil
    (let ((here (point)) (word))
      (if (or (whizzy-backward)      ; could find the beginning of the slice
              (and
               (re-search-backward "^[ \t]*\\\\begin{document}[ \t\n]*"
                                   (point-min) t)
               (goto-char (match-end 0)))
              (and whizzy-slave
                   (goto-char (point-min))))
          (progn
            (while (looking-at "[ \t]*\\(%[^\n]*\\|\\)\n")
              (goto-char (match-end 0)))
            (if (looking-at "[ \t]*\\\\begin{document}[ \t\n]*")
                (goto-char (match-end 0)))
            (let ((from (point)) (to) 
                  (next (match-end 0))
                  (counters)
                  (line))
              (if (cdr (whizzy-section-counters))
                  (or
                   (and (looking-at whizzy-section-regexp)
                        (setq counters
                              (whizzy-section-counters (match-string 0)))
                        (setq word (cdr counters)))
                   (and (re-search-backward whizzy-section-regexp
                                            (point-min) t)
                        (setq line
                              (cdr (whizzy-section-counters (match-string 0))))
                        (setq word (concat line (match-string 0)
                                           "[...]\\par\\medskip")))
                   (setq word (cdr (whizzy-section-counters t)))
                   )
                (setq word (cdr (whizzy-section-counters t))))
              ;; (message "Word-1 %S" word)
              (goto-char next)
              (if (and
                   (or (re-search-forward whizzy-begin (point-max) t)
                       (re-search-forward "\\\\end\\( *{document}\\|input\\)"
                                          (point-max) t)
                       (and whizzy-slave
                            (goto-char (point-max))
                            (re-search-forward "$")))
                   (goto-char (match-beginning 0)))
                  (let ((before (caar counters))
                        (after (cdar counters)))
                    (setq to (point))
                    ;; Should we enlarge slice to get whizzy-slice-pages?
                    (if (and (> whizzy-slice-pages 0) after before)
                        (let ((page (/ (+ (cadr (car before))
                                          (cadr (car after))
                                          1 whizzy-slice-pages) 2)))
                          (while
                              (and (cdr after)
                                   (< (cadr (car after)) page)
                                   (re-search-forward
                                    (concat "^" (regexp-quote (caar after)))
                                    (point-max) t)
                                   (setq to (match-beginning 0)))
                            (setq after (cdr after)))
                          (setq before (cdr before))
                          (goto-char from)
                          ;; (setq from nil)
                          (setq page (- page whizzy-slice-pages))
                          (while
                              (and before
                                   (< page (cadr (car before)))
                                   (re-search-backward
                                    (concat "^" (regexp-quote (caar before)))
                                    (point-min) t))
                            (setq word
                                  (concat "\\WhizzySlice "
                                          (cddr (car before))))
                            (setq before (cdr before)))
                          ))
                    ;; (message "Word-2 %S" word)

                    
                    ;; end of enlargment code
                    (goto-char here)
                    (setq whizzy-last-point here)
                    (setq whizzy-last-slice-begin from)
                    (setq whizzy-last-slice-end to)
                    (whizzy-move-slice-overlays)
                    (if layer
                        (setq word
                              (concat "\\WhizzySlide[" layer "]{"
                                      (int-to-string
                                       (count-lines (point-min) here))
                                      "}")))
                    (setq whizzy-last-layer layer)
                    (whizzy-write-slice whizzy-last-slice-begin
                                        whizzy-last-slice-end t word)
                    )))))
        (goto-char here)
      )))


(defun whizzy-current-time ()
  (let ((time (current-time)))
     (+ (* (mod (cadr time) 1000) 1000)
                  (/ (cadr (cdr time)) 1000))))

(defun whizzy-set-time (arg)
  (let* ((this (whizzy-current-time))
         (start (whizzy-get whizzy-slice-start))
         )
    ;; (message "set-time: %S (this=%S start=%S)" arg this start)
    (if arg
        (if (= start 0) nil
          (whizzy-set whizzy-slice-start 0)
          (whizzy-set whizzy-slice-time (- this start))
          )
      (if (= start 0)
          (whizzy-set whizzy-slice-start this)
        (message "Inconsistent time"))
      )))

(defvar whizzy-timer nil)
(defun whizzy-watch ()
  (if whizzy-timer (cancel-timer whizzy-timer))
  (setq whizzy-timer 
        (run-with-timer 2 nil 'whizzy-check-consistency (current-buffer)))
)
(defun whizzy-check-consistency (buffer)
  (unless (= whizzy-last-tick (buffer-modified-tick))
    (if (equal buffer (current-buffer))
      (whizzy-observe-changes)
      ;; else we might have changed buffer ---do not reslice
      ;; to be consistent, there should have been a consistency check 
      ;; when exiting the buffer
      )))

(defun whizzy-wakeup ()
  (let* ((p (whizzy-get whizzy-process))
         (s (and p (processp p) (process-status p))))
    (whizzy-set whizzy-slice-date (whizzy-current-time))
    (if (equal s 'stop) (continue-process p))
    (if (or (null p) (equal s 'exit))
        (progn
          (message "*** Mode turned off while slicing. ")
          (whizzy-mode-off))
      (whizzy-set whizzy-slice-fed t)
      (comint-send-string p "\n"))
    ))

(defun whizzy-kill (&optional arg)
  (let* ((p (whizzy-get whizzy-process))
         ;; (buf (whizzy-get whizzy-process-buffer))
         )
    (if arg
        (progn
          (message "Killing running whizzytex")
          (shell-command
             (concat whizzy-command-name " -kill "
                     (whizzy-get whizzy-filename)
                     " 2>/dev/null"))
          ;; (kill-buffer buf)
          )
      (and p (member (process-status p) '(run stop))
           ;; In future version, there might be
           ;; (process-send-signal 'SIGTERM p t)
           ;; Avoid (quit-process p t) which would create a core
           (process-send-eof p)
           (signal-process (process-id p) 'SIGTERM)
           ;; Then try whizzytex -kill
           (whizzy-sit-for 0 500)
           (unless (member (process-status p) '(exit signal)) (whizzy-kill t))
           ;; At last, use quit, which may create a core.
           ;; Maybe deleting the buffer would be better.
           (unless (equal (process-status p) '(exit signal)) (quit-process p))
           )
      )))

(defun whizzy-suspend (&optional arg)
  "Suspend or resume slicing in the current buffer.

Suspend WhizzyTeX if `whizzytex-mode' is t and set it to 'suspended.
Resume WhizzyTeX if `whizzytex-mode' is 'suspended and set it to t.
Otherwise, it raises an error.

Optional ARG, is ignored. 

This only stop slicing and does not kill WhizzyTeX.  It can be useful to do
a sequence of editing while slicing could be distracting or annoying."
  (interactive "P")
  (cond
   ((null whizzytex-mode)
    (error "WhizzyTeX is not on"))
   ((equal whizzytex-mode t)
    (remove-hook  'post-command-hook 'whizzy-observe-changes t)
    (whizzy-set whizzy-speed-string "Z")
    (force-mode-line-update)
    (setq whizzytex-mode 'suspended))
   ((equal whizzytex-mode 'suspended)
    (add-hook 'post-command-hook 'whizzy-observe-changes t t)
    (whizzy-set whizzy-speed-string "?")
    (force-mode-line-update)
    (setq whizzytex-mode t))
   (t
    (error "Unknown whizzytex-mode %S" whizzytex-mode)))
  )

(defvar whizzy-load-factor 0.6
  "Control the interval between slicing as a factor of the slicing REAL time.
Decreasing this will slice slower and let Emacs
or other processus be more responsive.")

(defun whizzy-load-factor (&optional arg)
  "Change the whizzy-load-factor.
Set to `whizzy-load-factor' to ARG, if this is a float.
Otherwise, according to prefix-numeric value of ARG (or value returned
interactively):

 1 (nil) ask the user
 4 (default) set load factor to default value 0.6
 9 (lower) divide load factor by 2
 0 (higher) multiply load factor by 2

The function maintain values in the range of 0.1 - 10.
Other can only be set assigned to `whizzy-load-factor' by hand."
  (interactive "P")
  (let ((p (prefix-numeric-value arg)))
    (cond
     ((floatp arg) (setq whizzy-load-factor arg))
     ((equal p 4)
      (setq whizzy-load-factor 0.6))
     ((equal p 9)
      (setq whizzy-load-factor (max (/ whizzy-load-factor 2) 0.1)))
     ((equal p 0)
      (setq whizzy-load-factor (min (* whizzy-load-factor 2) 10.0)))
     ((or (equal arg nil) (equal p 1))
      (let ((table '(("lower" . 9) ("default" . 4) ("higher" . 0))))
        (whizzy-load-factor
         (or (cdr (assoc (completing-read "load factor: "
                                          table nil t) table))
             whizzy-load-factor))))
     ))
  (message "%.1f" whizzy-load-factor)
  )

(defun whizzy-wait (priority)
  (let* ((now  (whizzy-current-time))
         (date (or (whizzy-get whizzy-slice-date) now))
         (slice (or (whizzy-get whizzy-slice-time) 1))
         (timeout (- (round (/ slice whizzy-load-factor priority))
                     (max 0 (- now date))))
         )
    ;; (message "Wait priority=%S timeout=%S date=%S now=%S slicing=%S load=%S"
    ;;              priority timeout
    ;;              (whizzy-get whizzy-slice-date)
    ;;              (whizzy-current-time)
    ;;              (whizzy-get whizzy-slice-time)
    ;;              whizzy-load-factor)
    (let ((res
           (or (<= timeout 0) (whizzy-sit-for 0 timeout))
           ))
      ;; (message "%s" (if res "exhausted" "aborted"))
      res)
    ))

(defun whizzy-duplex ()
  "Launch another previewer on the whole document."
  (interactive)
  (message "Opening a duplex view on the document %s"
           (buffer-name (whizzy-get whizzy-master-buffer)))
  (whizzy-call "duplex"))

(defun whizzy-toggle-debug ()
  "*Debug mode

Toggle debug mode when whizzytex-mode is on or start WhizzyTeX in debug 
mode, otherwise. This is necessary to keep log files when WhizzyTeX fails
during initialization."
  (interactive)
  (let ((debug (not (whizzy-get whizzy-debug))))
    (if whizzytex-mode
        (whizzy-call (if debug "trace on" "trace off"))
    (whizzytex-mode 16))))

(defun whizzy-auto-recompile (arg)
  "Set shell variable AUTORECOMPILE according to arg.
   If arg numeric value is strictly positive, then reformating
   will also launch recompilation. "
  (interactive "p")
  (whizzy-call "autorecompile" (if (> arg 0) "on" "off")))

(defun whizzy-reslice ()
  "Recompile the slice."
  (interactive)
  (whizzy-call "reslice" 'filename))

(defun whizzy-reformat ()
  "Reformat the document."
  (interactive)
  (whizzy-call "reformat" 'filename))

(defun whizzy-recompile ()
  "Recompiles the whole document."
  (interactive)
  (whizzy-call "whole" 'filename))

(defun whizzy-after-save ()
  "Action to take after saving a file"
  ;; save sections header-lines
  (whizzy-save-section-lines)
  ;; call for recompilation
  (if (stringp whizzy-slave) (whizzy-recompile)
    (whizzy-reformat)))

;;

;; Need to fix the following problem:
;; when some sit-for in filter-output... 
;; is aborted by a new event, the following whizzy-wait 
;; seems to abort as well, so the slice may not be updated after 
;; the last change. It seems that sit-for should not be used in filter-output
;; check waiting-for-user-input-p or find an other way to close the 
;; error window, such as return immediate but leave a flag that will be check 
;; by the main window.
;; could this be the cause of the bug en new versions of emacs?

(defun whizzy-observe-changes (&optional ignore-check force)
  ;; (message "--> %S" last-input-event)
  (if executing-kbd-macro  t
    (if (or (and (equal whizzytex-mode t) (whizzy-get whizzy-running))
            ignore-check)
        (let
            ((tick (buffer-modified-tick))
             (ticks (- (buffer-modified-tick) whizzy-last-tick))
             (pos (point)) (layer) (tmp)
             (error t))
          (unwind-protect
              (progn
                (if (and
                     (not (memq this-command whizzy-noslicing-commands))
                     (or
                      (and
                       (or
                        (if (or (/= ticks 0)
                                (not (equal (whizzy-get whizzy-active-buffer)
                                            (current-buffer))))
                            t
                          (if (or (buffer-modified-p)
                                  (= tick whizzy-last-saved))
                              nil
                            (setq whizzy-last-saved tick)
                            (whizzy-wakeup) ; rerun same slice
                            nil))
                        (and (whizzy-local)
                             (or
                              (< pos whizzy-last-slice-begin)
                              (> pos whizzy-last-slice-end)))
                        (and whizzy-layers
                             (progn
                               (beginning-of-line)
                               (setq tmp (point))
                               (goto-char pos)
                               (if (re-search-backward
                                    "\\\\overlay *{?\\([0-9*]\\)" tmp t)
                                   (setq layer (match-string 1)))
                               (goto-char pos)
                               (not (equal layer whizzy-last-layer))))
                        (or (memq this-command whizzy-slicing-commands))
                        )
                       (or (whizzy-wait
                            (/ (+ ticks
                                  (if (equal (whizzy-get whizzy-running) 0) 3
                                    1))
                               4.0))
                           ;; some input came earlier, but too many changes 
                           (> ticks (/ 10 whizzy-load-factor))
                           (progn (whizzy-watch) nil)
                           ))
                      (and whizzy-point-visible
                           (not (equal (point) whizzy-last-point))
                           (whizzy-wait (/ (+ 1 ticks) 4.0)))
                      ))
                    (progn
                      (whizzy-set whizzy-active-buffer (current-buffer))
                      (whizzy-slice layer)
                      (setq whizzy-last-tick tick)
                      ))
                (setq error nil))
            (if (and error whizzytex-mode)
                (if (whizzy-get whizzy-debug)
                    (progn
                      (message
                       "*** Fatal Error while slicing. Entering debugger")
                      (debug))
                  (message "*** Fatal Error while slicing. Mode turned off.
    (Rerun with WhizzyTeX debug-mode on to produce an Emacs backtrace.)")
                  (whizzy-mode-off)))
            )))))


;;;###autoload
(defun whizzytex-mode (&optional arg)
  "Switch whizzytex mode according to ARG.

If ARG is null, toggle the mode in the current buffer. 

Otherwise, according to the numeric value of ARG it turns mode on (> 0)
or off (< 0) in the current buffer, or turn the mode off in the master buffer 
and all its slaves if (= 0). If the mode is already on while turn on, 
it is first turn off, then on. 

Turning the mode ON assigns (or reassigns) values to WhizzyTeX parameters
searching configuration information in order in the current file 
\(see `whizzy-write-configuration'), in configuration files 
\(see `whizzy-add-configuration'), in global values (see `whizzy-viewers' 
and `whizzy-class-mode-alist'), or prompting the user when essential 
information is missing.

If ARG is 4 or higer, the user is always asked to confirm/change the
default setting, interactively. (See also `whizzy-change-mode')

If ARG is 16, whizzytex is started in debug mode. Then, and only then, 
log files will be kept in case of failure during initialization. 

This command also evaluates `whizzytex-mode-hook' before setting up
file dependent variables. By default is equal to `whizzy-default-bindings'
and used to set up some bindings. File dependent hook defined in
`whizzy-hook-alist' that match the filename are evaluated.

You can also set default values to WhizzyTeX user variables in your eamcs
configuration file (usually ~/.emacs) by inserting lines of the form: 

        (setq-default VARIABLE VALUE)

The following variables can be customized: 

 - Settings:

    `whizzy-command-name'
    `whizzy-slicing-commands'
    `whizzy-viewers'
    `whizzy-class-mode-alist'
    `whizzy-mode-regexp-alist'
    `whizzy-select-mode-regexp-alist'
    `whizzy-paragraph-regexp'
    `whizzy-slice-pages'
    `whizzy-configuration-path'
    `whizzy-configuration-alist'
    `whizzy-hook-alist'
    `whizzytex-mode-hook'

 - Limiting WhizzyTeX features (which may sometimes lead to LaTeX errors):

    `whizzy-line'
    `whizzy-point-visible'
    `whizzy-overlays'

 - Controling recompilation (which may affect speed):

    `whizzy-auto-recompile'

 - User Interaction:

    `whizzy-save-buffers'
    `whizzy-auto-visit'
    `whizzy-auto-raise'
    `whizzy-auto-show-output'
    `whizzy-pop-up-windows'

 - Downgrade performance for compatibility with other packages:

    `whizzy-use-write-annotate'

 - Transparent customization of the latex document under whizzytex.

    `whizzy-customize'

 - Customizing faces: 

    `whizzy-slice-face'
    `whizzy-error-face' 

 - Check `whizzytex-version' for WhizzyTeX version number.
"

  (interactive "P")
  (and buffer-read-only
       (not (y-or-n-p "Are you sure you wish to WhizzyTeX read only buffer? "))
       (error "Aborted"))
  (let* ((prefix-arg (prefix-numeric-value arg))
         (new-mode (if (null arg) (not whizzytex-mode) (>  prefix-arg 0))))
    (if (and (= prefix-arg 0) (whizzy-get whizzy-running))
        (whizzy-mode-off 'master)
      (if (and whizzytex-mode
               (or (not new-mode) (not (null arg))))
          (progn (whizzy-mode-off) (whizzy-sit-for 1)))
      (if (and new-mode (not whizzytex-mode))
          (whizzy-mode-on prefix-arg))))
  whizzytex-mode)

(defvar whizzytex-mode-map nil
  "*Keymap used in `whizzytex-mode'.")

(defvar whizzy-bad-extensions
  '("log" "aux" "bib" "dvi" "ps" "pdf" "fmt" "waux" "wdvi")
    "List of forbidden extension (used internally).")

(defvar whizzy-configuration-alist nil
  "*Alist of mapping file name regexps to configurations. 

A configuration is itself an alist from keywords to strings where 
keywords are either the symbol whizzy or whizzy-master and the string 
represent the configuration string for that keyword 
\(see `whizzy-write-configuration'). 

Configurations can be defined in a WhizzyTeX configuration files
using the command `whizzy-add-configuration'. 

Configurations files of name \"whizzy.el\" that are in the path
`whizzy-configuration-path' or in the master file directory are
automatically loaded (the former are only loader once, while the later
are reloaded at every invocation of whizzytex).

You may look at the default configuration before changing it, since 
you may loose some features by removing certain options. 
")

(defvar whizzy-hook-alist nil
  "*A-list of file dependent hooks. 

Elements are pairs ( REGEXP . HOOK ) meaning that the function HOOK
should be run when whizzytex-mode is turned on on a file whose fullname
matches REGEXP. The hook is run before parameter settings.

These can be defined with `whizzy-add-configuration'. 
")

(defun whizzy-run-file-hooks ()
  (let ((f (cdr (whizzy-assoc-if
                 '(lambda (r) (string-match r (buffer-file-name)))
                 whizzy-hook-alist))))
    (if f (apply f nil))))

(defun whizzy-mode-on (&optional query)
  "See `whizzytex-mode' for meaning of QUERY"
  (let ((mode-set))
    (if whizzytex-mode nil
      (or whizzy-status
          (setq whizzy-status (make-vector whizzy-length nil)))
      (unwind-protect
          (if (not (whizzy-setup  (>= query 4)))
              (progn
                (message "Setup failed. Turning mode off")
                (whizzy-mode-off))
            (if (and (buffer-modified-p)
                     (if (or (equal whizzy-save-buffers 'ask)
                             (not (file-exists-p buffer-file-name)))
                         (y-or-n-p "Save buffer? ")))
                (save-buffer))
            (whizzy-setup-mode (whizzy-get whizzy-slicing-mode))
            (whizzy-create-slice-overlays)
            (unless (consp whizzy-slave) 
              ;; this is not a macro file
              (whizzy-set whizzy-slice-date (whizzy-current-time))
              (whizzy-set whizzy-slice-start 0)
              (whizzy-set whizzy-slice-time 0)
              (setq whizzy-last-tick -1)
              (whizzy-save-section-lines))
            (make-local-hook 'post-command-hook)
            (make-local-hook 'before-revert-hook)
            (make-local-hook 'kill-buffer-hook)
            (make-local-hook 'after-save-hook)
            ;; Sanity check. 
            (if (and whizzy-slave (not (whizzy-get whizzy-running)))
                (error "** WhizzyTeX anomaly: master is not running."))
            (cond
             ((or (consp whizzy-slave) (stringp whizzy-slave))
              (whizzy-set whizzy-slaves
                          (cons (current-buffer) (whizzy-get whizzy-slaves)))
              )
             ((null whizzy-slave)
              (whizzy-set whizzy-master-buffer (current-buffer))
              (let* ((filename
                      (file-name-nondirectory
                       (or buffer-file-name
                           (error "Buffer has no associated file name"))))
                     (dir (concat (file-name-directory buffer-file-name)))
                     (basename (concat whizzy-file-prefix
                                       (file-name-sans-extension filename)))
                     (subdir (concat dir basename "_d/"))
                     (input (concat subdir "input/"))
                     (output (concat subdir "output/"))
                     (slicename (concat input basename ".new"))
                     (logname (concat output "log"))
                     )
                (if (string-match " " filename)
                    (error
                     "File '%s' cannot be WhizzyTeX: its name contain a space."
                     filename)
                  )
                (if (and (string-match "\\.\\([^.]*\\)$" filename)
                         (member (match-string 1 filename)
                                 whizzy-bad-extensions))
                    (error
                     "File name %s has forbidden extension %s"
                     filename (match-string 1 filename))
                  )
                (whizzy-set whizzy-slicename slicename)
                (whizzy-set whizzy-filename filename)
                (whizzy-set whizzy-basename basename)
                (whizzy-set whizzy-dir dir)
                (whizzy-set whizzy-input-dir input)
                (whizzy-set whizzy-output-dir output)
                (whizzy-set whizzy-log-buffer logname)
                (if (let* ((p (whizzy-get whizzy-process))
                           (s (and p (process-status p))))
                      (or (equal s 'run) (equal s 'stop)))
                    (whizzy-kill t))
                (if (file-exists-p filename)
                    (or (file-readable-p filename)
                        (error "File \"%s\" not readable!" filename))
                  (error "File \"%s\" does not exists!" filename))
                ;; file-accessible-directory-p ?
                (or (file-exists-p subdir) (make-directory subdir))
                (or (file-exists-p output) (make-directory output))
                (or (file-exists-p input) (make-directory input))
                ;; do we want this? YYY
                (whizzy-set whizzy-slaves nil)
                (if (= query 16)
                    (whizzy-set whizzy-debug t)
                  (if (whizzy-get whizzy-debug)
                      (if (y-or-n-p "Start WhizzyTeX in debug mode? ") nil
                        (whizzy-set whizzy-debug nil))))
                (let ((args (append (whizzy-get whizzy-view-mode)
                                    (if (whizzy-get whizzy-debug) '("-trace"))
                                    (list filename)
                                    )))
                  (whizzy-start-process args))
                  (whizzy-set whizzy-running 0)
                  )))
            (add-hook 'after-save-hook 'whizzy-after-save t t)
            (add-hook 'kill-buffer-hook 'whizzy-mode-off t t)
            (add-hook 'before-revert-hook 'whizzy-before-revert t t)
            ;; (use-local-map whizzytex-mode-map)
            (setq whizzytex-mode t)
            (setq mode-set t)
            ;; move to first slice?
            (unless
                (or whizzy-slave
                    (save-excursion
                      (end-of-line)
                      (re-search-backward "^[ \t]*\\\\begin{document}"
                                          (point-min) t)))
              (re-search-forward "^[ \t]*\\\\begin{document}[ \n\t]*"
                                 (point-max) 'move
                                 ))
            ;; whizzy-reformat need mode on
            (if (not (consp whizzy-slave))
                ;; This is regular tex file
                (add-hook 'post-command-hook 'whizzy-observe-changes t t)
              ;; This is a macro file
              (whizzy-reformat)
              )
            (sleep-for 1)
            )
        ;; could not turn mode on. Should switch it off (maybe in slaves). 
        (unless whizzytex-mode
          (whizzy-mode-off t))
        ;; did not even turn mode on. whizzy-status could be inconsistent.
        (unless mode-set
          (setq whizzy-status nil))
        (force-mode-line-update)
        ))))

(defun whizzy-mode-off (&optional arg)
  "Turn WhizzyTeX mode off.

If ARG is a buffer turn mode off in that buffer (usually the master buffer). 
If ARG turn mode off even if apparently allready off. 
"
  (interactive "p")
  (if whizzy-status
      (if (and (equal arg 'master) (whizzy-get whizzy-running))
          (let ((master (whizzy-get whizzy-master-buffer)))
            (if (buffer-live-p master)
                (save-excursion (set-buffer master) (whizzy-mode-off))))
        (if (not (or whizzytex-mode arg)) nil
          (remove-hook  'post-command-hook 'whizzy-observe-changes t)
          (remove-hook  'after-save-hook 'whizzy-after-save t)
          (remove-hook  'kill-buffer-hook 'whizzy-mode-off t)
          (remove-hook  'before-revert-hook 'whizzy-before-revert t)
          (whizzy-delete-slice-overlays)
          (whizzy-delete-error-overlay)
          (if whizzy-slave
              (whizzy-set whizzy-slaves
                          (delete (current-buffer) (whizzy-get whizzy-slaves)))
            (whizzy-set whizzy-running nil)
            (whizzy-kill)
            (let ((buffers (whizzy-get whizzy-slaves)))
              (while buffers
                (if (buffer-live-p (car buffers))
                    (save-excursion
                      (set-buffer (car buffers)) (whizzy-mode-off)))
                (setq buffers (cdr buffers)))
              (whizzy-set whizzy-slaves nil)))
          (setq whizzytex-mode nil)
          (force-mode-line-update)
          ))
    ))


(defun whizzy-auto-mode ()
  (save-excursion
    (goto-char (point-min))
    (let ((mode nil)
          (regexp whizzy-select-mode-regexp-alist))
      (if (re-search-forward
           "^ *\\\\documentclass[^{}\n]*{\\([a-zA-Z]+\\)}"
           (point-max) t)
          (setq mode (assoc (match-string 1) whizzy-class-mode-alist)))
      (if (consp mode)
          (cdr mode)
        (while
            (and (not mode) (consp regexp))
          (if (re-search-forward (car (car regexp)) (point-max) t)
              (setq mode (cdr (car regexp)))
            (setq regexp (cdr regexp))))
        (or mode 'document)
        ))))

(defun whizzy-setup-mode (mode)
  "Adjust local variables according to MODE."
  (setq whizzy-begin (cdr (assoc mode whizzy-mode-regexp-alist)))
  (cond
   ((null mode)
    (error "Mode should be a non-nil atom"))

   ((member mode '(subsubsection subsection section chapter document none))
    (whizzy-set whizzy-counters (or (whizzy-get whizzy-counters) t)))

   ((equal mode 'paragraph)
    (whizzy-set whizzy-counters (or (whizzy-get whizzy-counters) t))
    (let ((str (whizzy-read-file-config "whizzy-paragraph")))
      (setq whizzy-begin
            (or (and str  (car (read-from-string str)))
                (cdr (assoc mode whizzy-mode-regexp-alist))
                whizzy-begin)))
    )

   ((equal mode 'slide)
    (whizzy-set whizzy-counters nil)
    (save-excursion
      (goto-char (point-min))
      (if (or (re-search-forward
               "^\\\\documentclass *\\[\\([^]{}\n]*,\\)*semlayer\\(,[^]{}\n]*\\)*\\]"
               (point-max) t)
              (re-search-forward "\\\\overlay *{?[0-9*]" (point-max) t))
          (whizzy-set whizzy-layers t))))
   ((equal mode 'letter)
    (whizzy-set whizzy-counters nil))
   (t
    (error "Ill-formed mode"))
   )
  (if (and (null whizzy-begin) (not (equal mode 'none)))
      (error "Ill-formed slicing mode or incomplete whizzy-mode-regexp-alist"))
  (whizzy-set whizzy-slicing-mode mode)
  )

(defun whizzy-read-mode (&optional default)
  (interactive)
  (let* ((table
          (mapcar (lambda (x) (cons (symbol-name (car x)) (car x)))
                  whizzy-mode-regexp-alist))
         (mode
          (completing-read (format "Mode [%S] : " default) table nil t nil))
         (elem)
         )
    (if (setq elem (and mode (assoc mode table))) (cdr elem) default)))

(defun whizzy-change-mode (&optional arg)
  "If ARG is a symbol, set mode to to ARG.
If ARG is null read mode interactively
Otherwise, if set mode to section unit acording to the prefix value of ARG:
6, 7, and 8 set mode to document, paragraph and none.
0 and 9 behaves as -1 and 1.
Positive (negative) values of ARG widen (narrow) slice by as ARG steps.

See also `whizzy-mode-regexp-alist' for the list of all modes and
`whizzy-slice-pages' for enlarging small slices automatically."
  (interactive "P")
  (if whizzy-status
      (let ((p (prefix-numeric-value arg))
            (m (whizzy-get whizzy-slicing-mode))
            (mode) (modes))
        (if (= p 0) (setq p -1)
          (if (= p 9) (setq p 1)))
        (setq mode
              (cond
               ((null arg) (whizzy-read-mode m))
               ((symbolp arg) arg)
               ((= p 8) 'none)
               ((= p 7) 'paragraph)
               ((= p 6) 'document)
               ((and (> p -5) (< p 5) m)
                (setq modes (nthcdr 3 (mapcar 'car whizzy-mode-regexp-alist)))
                (if (> p 0) (nth p (member m modes))
                  (nth (- 0 p) (member m (reverse modes)))))
               (t (whizzy-read-mode m))
               ))
        (message "Setting mode to: %S" (or mode m))
        (if (not mode) nil
          (if (not (assoc mode whizzy-mode-regexp-alist))
              (error "Ill formed mode")
            (whizzy-setup-mode mode)
            (whizzy-delete-slice-overlays)
            (setq whizzy-last-slice-begin 0)
            (setq whizzy-last-slice-end 0)
            (setq whizzy-last-tick -1)
            )))
    (error "Run whizzytex first (buffer has no whizzytex information)")
    ))

(defun whizzy-read-mode-from-file ()
  (let* ((mode-string (whizzy-read-file-config "whizzy"))
         (mode-begin) (mode-end) (mode))
    (if (and mode-string (string-match "^ *\\([a-z]+\\)" mode-string))
        (progn
          (setq mode-string (match-string 1 mode-string))
          (setq mode-begin (match-beginning 1))
          (setq mode-end (match-end 1))
          )
      (setq mode-string nil))
    (if mode-string
        (if (setq mode
                  (assoc mode-string
                         (mapcar (lambda (x) (cons (symbol-name (car x))
                                                   (car x)))
                                 whizzy-mode-regexp-alist)))
            (setq mode (cdr mode))
          (goto-char mode-begin)
          (set-mark mode-end)
          (error
           (concat
            "Slicing mode ``" mode-string
            "'' is not valid. Use: "
            (mapconcat '(lambda (e) (symbol-name (car e)))
                       whizzy-mode-regexp-alist ", ")))
          )
      (setq mode (whizzy-auto-mode)))
    mode))
    

; (defun whizzy-basename (name &optional ext)
;   (file-name-sans-extension name))
;   (if (or (string-match
;            (concat "\\(.*\\)" (regexp-quote (or ext ".tex"))) name)
;           (string-match
;            (concat "\\(.*\\)" (regexp-quote (or ext ".ltx"))) name))
;         (match-string 1 name)
;       name))

(defun whizzy-unquote (arg)
  (if (stringp arg)
      (progn
        (if (string-match " *\\(.*[^ ]\\) *" arg)
          (setq arg (match-string 1 arg)))
        (if (string-match "\"\\(.*\\)\"" arg)
          (setq arg (match-string 1 arg)))))
  arg)

(defvar whizzy-command-options 
  '(("-ext" . t)
    ("-mkslice" . t) ("-mkfile" . t) ("-makeindex" . t) ("-bibtex" . t)
    ("-initex" . t) ("-latex" . "latex") ("-format" . t) ("-fmt" . t)
    ("-dvicopy" . "dvicopy")
    ("-display" . t)
    ("-duplex") ("-trace"))
  "List of command line options"
  )

(defun whizzy-read-view-from-file ()
  (let ((string (whizzy-read-file-config "whizzy"))
        (start 0)
        (tmp-view))
    (and
     string
     (string-match "\\([a-z]+ +\\)?-" string)
     (progn
       (setq start (- (match-end 0) 1))
       (if (string-match "\\(-a?dvi\\|-ps\\|-pdf\\)\\b *" string start)
           (progn
             (setq tmp-view (cons (match-string 1 string) tmp-view))
             (setq start (match-end 0)))
         (setq tmp-view (cons nil tmp-view)))
       (let ((options whizzy-command-options)
             (option) (argp) (tmp))
         (if (equal (string-match "[^-]" string start) start)
             (setq argp t)
           (setq tmp-view (cons nil tmp-view)))
         (while (consp options)
           (setq option (caar options))
           (setq tmp (and (cdar options) option))
           (if (string-match
                (concat (if argp "\\(\\(.*[^ ]\\) +\\)") option
                        (if tmp " +" " *"))
                string start)
               (progn 
                 (setq tmp-view
                       (cons option
                             (if argp
                                 (if (match-string 2 string) 
                                     (cons (match-string 2 string) tmp-view)
                                   (error "Option %S expect an argument" argp))
                               tmp-view)))
                 (setq argp tmp)
                 (setq start (match-end 0))))
           (setq options (cdr options))
           )
       (cond
        ((and (consp (cdr tmp-view)) (not argp))
         (if (< start (length string))
             (error "Do not know what to do with trailing command argument %s"
                    (substring string start))))
        (t 
         (setq tmp-view
               (cons (if (< start (length string)) (substring string start))
                     tmp-view))))
       )
       (setq tmp-view (reverse (mapcar 'whizzy-unquote tmp-view)))
       tmp-view
       ))))


(defun whizzy-setup (&optional query)
  (run-hooks 'whizzytex-mode-hook)
  (whizzy-load-configuration)
  (whizzy-run-file-hooks)
  (cond
   ;; XXX two first condition have been inverted 23/08/04
   ((or
     (equal (current-buffer) (whizzy-get whizzy-master-buffer))
     (save-excursion
       (goto-char (point-min))
       (or (re-search-forward "^[ \t]*\\\\begin{document}"
                              (point-max) t)
           (and
            (re-search-forward
             "^[ \t]*\\(\\\\begin\\(  *\\){document}\\)" (point-max) t)
            (delete-region (match-beginning 2) (match-end 2))))))
    (whizzy-setup-master query)
    )
   ((or whizzy-slave
        (let ((master
               (whizzy-read-file-config "whizzy *-ma\\(ster\\|cros\\)")))
          (and master
               (not (string-equal
                     (expand-file-name master)
                     buffer-file-name))))
        ;; (and (boundp 'TeX-master) (stringp TeX-master))
        )
    (whizzy-setup-slave))
   (t
    (whizzy-setup-slave)
    )
   ))


(defun whizzy-setup-master (&optional query)
  (if whizzy-slave (setq whizzy-slave nil))
  (let ((slicing (whizzy-get whizzy-slicing-mode))
        (tmp-mode (whizzy-get whizzy-view-mode))
        (view-read (whizzy-read-view-from-file))
        (view-type nil) (view-command nil) (view-options nil)
        (tmp-view nil)
        (tmp))
    ;; Setting view mode
    (setq view-type (car view-read))
    (setq view-command (cadr view-read))
    (setq view-options (cddr view-read))
    (if (and view-type (not query))
        nil
      (if tmp-mode
          (progn 
             (setq view-type (car tmp-mode))
             (setq view-command (cadr tmp-mode))
             (unless view-options (setq view-options (cddr tmp-mode))))
        (setq view-type (caar whizzy-viewers)))
      (if (or query (null view-type))
            (progn
              (setq view-type
                    (completing-read "Viewer type: "
                                     whizzy-viewers nil t view-type))
              (if (equal view-type "")
                  (error "You must specify a viewer type"))
              )))
    (setq tmp (cadr (assoc view-type whizzy-viewers)))
    (or tmp
        (error (concat "viewer type " (car tmp-view) " should be one of "
                       (mapconcat 'car whizzy-viewers ", "))))
    (or view-command (equal view-command "") (setq view-command tmp))
    (or view-options 
        (setq view-options
              (append (cddr (assoc view-type whizzy-viewers))
                      view-options)))
    (if query
        (progn
          (setq view-command (read-string "Viewer command: " view-command))
          (if (equal view-command "")
              (error "You must specify a viewer command"))
          )
      )
    ;; Setting slicing mode
    (or
     ;; do not reset if query or already bound
     (and query slicing)
     ;; reset otherwise
     (setq slicing (whizzy-read-mode-from-file)))
    (if query (setq slicing (whizzy-read-mode slicing)))
    ;; other options
    (if query
        (let ((all) (option))
          (while
              (and (setq option
                         (completing-read
                          "Other options ? [TAB for a list, or RET] "
                          whizzy-command-options))
                   (not (equal option "")))
            (setq option (assoc option whizzy-command-options))
            (if (consp option)
                (progn
                  (setq all (cons (car option) all))
                  (if (cdr option)
                      (setq all
                            (cons
                             (read-string
                              "value: "
                              (and (stringp (cdr option)) (cdr option)))
                             all))
                    ))))
          (setq view-options (append view-options (reverse all)))))
    ;; all options
    (setq tmp-view (cons view-type (cons view-command view-options)))
    (if (and tmp-view slicing)
        (progn
          (whizzy-set whizzy-view-mode tmp-view)
          (whizzy-set whizzy-slicing-mode slicing)
          )
      )
    tmp-view))

(defun whizzy-find-running-master ()
  "Find a master-buffer running whizzytex having the current-buffer for slave."
  (let* ((all (buffer-list)) (filename buffer-file-name)
         (sections) (dir) (found))
    (save-excursion
      (while (and all (not found))
        (set-buffer (car all))
        (unless (or whizzy-slave (not (whizzy-get whizzy-running)))
          (setq dir (whizzy-get whizzy-dir))
          (setq sections (whizzy-get whizzy-counters))
          (while (and (consp sections) (not found))
            (if (string-equal
                 (expand-file-name (concat dir (caar sections)))
                 filename)
                (setq found buffer-file-name))
            (setq sections (cdr sections))))
        (setq all (cdr all)))
      found)))
          
(defun whizzy-default-guess-master ()
  "Default function for variable `whizzy-guess-master'."
  (let*
      ((extension
        (file-name-nondirectory
         (if (string-match "\.sty$" buffer-file-name) buffer-file-name
           (concat (file-name-sans-extension buffer-file-name) ".tex"))))
       (grep
        (concat "grep -q -s -e '[A-Za-z--_/0-9]*" extension "\\( \\|$\\)'"))
       (command
        (concat
         "find . "
         ;; print only log files
         " -type f '(' -name '*.log' -o -name format ')' "
         ;; prune all except along whizzytex hierarchies
         " '(' -path './_whizzy_*_d' -o -path './_whizzy_*_d/output' -o -prune ')' "
         ;; command
         " -exec " grep " {} ';' -print"
         " | xargs ls -t "
         ))
       (filename (car (split-string (shell-command-to-string command))))
       (basename
        (and
         filename
         (or (string-match "\\./_whizzy_\\([^/]*\\)_d/output/format" filename)
             (string-match "\\./_whizzy_\\([^/]*\\)\\.log" filename)
             (string-match "\\./\\([^/]*\\)\\.log" filename))
         (match-string 1 filename)))
       )
    (and
     (or (file-exists-p (setq filename (concat basename ".tex")))
         (file-exists-p (setq filename (concat basename ".ltx"))))
     filename)))

(defvar whizzy-guess-master 'whizzy-default-guess-master
  "*Function to guess the master file name.
This should either be nil (which means do not guess)
or a function with no argument that returns a filename that is the best
candidate to be the master. The function is called from  the LaTeX buffer 
that is looking for its master file. 

nil means do not guess.
")


(defun whizzy-setup-slave (&optional query)
  (save-excursion
    (let* ((this-buffer (current-buffer))
           (master-buffer (whizzy-get whizzy-master-buffer))
           (master-file) (status)
           (auto))
      ;; master buffer might have been killed
      (if (buffer-live-p master-buffer) nil
        (whizzy-set whizzy-master-buffer nil)
        (setq master-buffer nil))
      (if master-buffer nil
        (setq master-file
              (expand-file-name
               (or
                (let ((dir (whizzy-get whizzy-dir))
                      (file (whizzy-get whizzy-filename)))
                  (and dir file (concat dir file)))
                (or (whizzy-unquote
                     (whizzy-read-file-config "whizzy *-ma\\(ster\\|cros\\)"))
                    (and (boundp 'TeX-master)
                         (stringp TeX-master)
                         (file-readable-p TeX-master)
                         TeX-master)
                    (whizzy-find-running-master)
                    (let ((default
                            (and whizzy-guess-master
                                 (funcall whizzy-guess-master))))
                      (read-file-name "Master (main LaTeX file): "
                                      nil default  t default)))
                )))
        (or (setq master-buffer (get-file-buffer master-file))
            (and (or (equal whizzy-auto-visit 'whizzytex)
                     (setq auto 
                           (y-or-n-p
                            (format "WhizzyTeX the master file %s? "
                                    master-file))))
                 (or (file-readable-p master-file)
                     (error "Master file %s does not exist" master-file))
                 (setq master-buffer (find-file-noselect master-file)))
            (error "Run whizzytex on the master %s first" master-file)))
      (if (equal master-buffer this-buffer)
          (error "A slave file should not be mastered by itself."))
      ;; check that master is running whizzytex
      (unwind-protect
          (save-excursion
            ;; to avoid looping, don't leave whizztex-mode nil
            (unless whizzytex-mode (setq whizzytex-mode 'installing))
            (set-buffer master-buffer)
            (or whizzytex-mode
                (and (or whizzy-auto-visit auto
                         (y-or-n-p
                          (format "WhizzyTeX the master file %s? "
                                  master-file)))
                     (message "Running whizzytex on master!")
                     (whizzytex-mode (if query 4 1)))
                whizzytex-mode
                (error "Please, run whizzytex on the master %s first"
                       master-file))
            ;; Now on the master run whizzytex
            (setq status whizzy-status))
        ;; undo the lock
        (if (equal whizzytex-mode 'installing)
            (setq whizzytex-mode nil)))
      ;; Come back to slave
      ;; (set-buffer this-buffer)
      (setq whizzy-status status)
      (unless (equal (whizzy-get whizzy-master-buffer) master-buffer)
          (setq whizzy-status nil)
          (error "Fatal error: Inconsistent master. Maybe equal to itself."))
      ;; we must compute the relative basename of the slave
      (let* ((this-name
             ;; (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name this-buffer)))
             ;;)
             (this-dir
              (file-name-directory (buffer-file-name this-buffer)))
             (master-dir
              (file-name-directory (buffer-file-name master-buffer)))
             )
        (if (string-match (concat (regexp-quote master-dir) "\\(.*\\)")
                          this-dir)
            (setq whizzy-slave
                  (concat (match-string 1 this-dir) this-name))
          (setq whizzy-status nil)
          (error
           "Master file %s is not a parent directory of the slave %s"
           master-dir this-name)))
      (if (and whizzy-slave
               (or (whizzy-read-file-config "whizzy *-macros")
                   (string-match "\.sty$"
                                 (expand-file-name buffer-file-truename))
                   ))
          (setq whizzy-slave (cons 'macros whizzy-slave)))
      whizzy-slave
      )))


;; to read file-dependend configuration parameters

;; assoc-if is not available in old vesions of emaacs
(defun whizzy-assoc-if (pred list)
  "Returns the longest sublist of LIST whose first element satisfied PRED."
  (while (and (consp list) (consp (car list))
              (not (funcall pred (caar list))))
    (setq list (cdr list)))
  (car list))

(defun whizzy-add-configuration (regexp config)
  "*Declare files whose path matches REGEXP to have configuration CONFIG.

The path is absolute if REGEXP starts with a /, otherwise, it is relative 
to the default directory. 

CONFIG may be a function, treated as a hook to run when whizztex-mode 
is activated (see `whizzy-hook-alist'), or a pair of a configuration
keyword and a string (see `whizzy-configuration-alist').

The new configuration is added ahead of the alist. Hence, the order in
which configuration are added is significant. The most selective REGEXP 
must be added last. 
"
  (if (string-match "^/" regexp) nil
    (setq regexp 
          (concat (regexp-quote (expand-file-name default-directory)) 
                  regexp)))
  (let* ((alist (if (functionp config) whizzy-hook-alist
                  whizzy-configuration-alist))
         (tmp (assoc regexp alist)))
    (if (consp tmp)
        (setcdr tmp config)
      (setq alist (cons (cons regexp config) alist))
      (if (functionp config) 
          (setq whizzy-hook-alist alist)
        (setq whizzy-configuration-alist alist)
        ))))

; (defun whizzy-add-configuration (regexp config)
;   "make file matching REGEXP have configuration CONFIG."
;   (if (string-match "/" regexp) nil
;     (setq regexp (concat (regexp-quote default-directory) regexp)))
;   (let
;       ((tmp (assoc regexp whizzy-configuration-alist)))
;     (if (consp tmp)
;         (setcdr tmp config)
;       (setq whizzy-configuration-alist
;             (cons (cons regexp config) whizzy-configuration-alist))
;       )))

(defvar whizzy-configuration nil)
(make-variable-buffer-local 'whizzy-configuration)

(defvar whizzy-configuration-path  (list "/etc/whizzytex" "~/.whizzytex")
  "*Dir(s) where to read configuration from.
This may be a string or a list of strings. 

When  `whizzy-configuration-alist' is not nil, means the dir or list of dirs
to read configuration from. 

Otherwise, a STRING means do not read configuration anymore, while 
a CONS means read configuration in current directory unless already read. 
")

(defun whizzy-load-configuration ()
  (unless (whizzy-get whizzy-configuration-loaded)
    (let ((files 
           (append
            (if whizzy-configuration-alist nil
              (mapcar
               '(lambda (f) (concat f "/whizzy.el"))
               (if (stringp whizzy-configuration-path)
                   (list whizzy-configuration-path)
                 whizzy-configuration-path)))
            (list "whizzy.el"))
           ))
      (while (consp files)
        (and (file-readable-p (car files)) (load-file (car files)))
        (setq files (cdr files))))
    ))

(defun whizzy-read-file-config (regexp)
  (or
   (save-excursion
     (beginning-of-buffer)
     (and (re-search-forward (concat "^%; *" regexp " +") 800 'move)
          (looking-at "\\([^\n]+[^ \n]\\) *\n")
          (match-string 1)))
   (cdr (whizzy-assoc-if
         '(lambda (a) (string-match regexp (symbol-name a)))
         ;; local-variable-p takes two arguments in xemacs
         (if (local-variable-p 'whizzy-configuration (current-buffer))
             (cdr whizzy-configuration)
           ;; (whizzy-load-configuration)
           (cdr (setq whizzy-configuration
                      (whizzy-assoc-if
                       '(lambda (a)
                          (string-match a buffer-file-name))
                       whizzy-configuration-alist)))
           )))
   ))
(defun whizzy-explode-filename (arg)
  (let ((filename
         (or (and (stringp arg) (expand-file-name arg))
             (and (bufferp arg) (buffer-file-name arg)))))
    (if (and (file-exists-p filename) (not (file-directory-p filename)))
        (split-string filename "/")
      (error "Filename %s does not exists or is a directory" filename))
    ))

(defun whizzy-relative-dir (from to)
  (setq from (whizzy-explode-filename from))
  (setq to  (whizzy-explode-filename to))
  (while (and from to (equal (car from) (car to)))
    (setq from (cdr from))
    (setq to (cdr to)))
  (concat (mapconcat '(lambda (x) "../") (cdr from) nil)
          (mapconcat 'identity to "/")))



(defun whizzy-write-file-config (name string &optional regexp)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (concat "^%; *" (or regexp (regexp-quote name)) " +") 800 t)
        (progn (end-of-line) (delete-region (match-end 0) (point)))
      (insert "%; " name  " \n") (backward-char 1))
    (insert string)))

(defun whizzy-write-configuration ()
  "Save the whizzytex buffer configuration in the current file.

This makes the configuration persistent so that it can be restored
next time the file will be visited and WhizzyTeX-ed.

The configuration can also be written by hand in the topmost lines
of the file (first 1000 characters). A configuration line starts with
the regexp \"^%; +\" and is followed by a configuration keyword.
If two configurations lines have the same keyword, only the first one is
considered. There are three keywords, describe below, together with
the interpretation of the rest of the line:

%; whizzy-master <name>

  The is used for files mastered by another file. That is, it is not
  LaTeX-ed directly, but  loaded by another LaTeX file. Conversely, the
  second and last line are used for master files. For WhizzyTeX, a master
  file is one that contains a \documentclass command and \begin{document}
  commands. Other files are treated as mastered.

  <name>
     is the relative or fullname of the master file

%; whizzy-macros <name>

  This is equivalent to whizzy-master <name>, but for a file containing
  macros. Such a file is not sliced while editing, but saving it reformats
  the master. 

%; whizzy [ <slicing> ] [ <viewer> [ <command> ] ] [ -pre <make> ]  ... [ -trace ]

  All arguments are optional, but if present they must be passed in order:

  <slicing>

     describes the way buffer is sliced (a slice is the region of the
     buffer under focus and displayed).

     See `whizzy-mode-regexp-alist' for a list of all slicing modes
     and `whizzy-class-mode-alist' for default mode selection when ommitted.

  <viewer>

     defines the type of previewer used (see `whizzy-viewers').
     This value is both interpretted by emacs and passed to the whizzytex
     script.

  <command>

     is only passed to the whizzytex script (not interpreted by emacs)
     this is the command that whizzytex should call to launch the
     previewer (see the manual). Command include the name of the
     previewer as well as its options. It is does not have to be quoted.
     It extends to the right as must as possible, i.e. ends with the end
     of line or some recognized option.
        
  -ext <suffix>
  
     makes <suffix> the latex file extension, instead of the sufffix of
     the master file taken by default.

  -mkslice <command>

     This tells whizzytex to preprocess the slice with <command> instead 
     of simply renaming the slice. The convention is that WhizzyTeX prepares 
     a slice  BASENAME.new, calls <command> BASENAME.tex, which should
     produce a file BASENAME.tex

     When the option is ommitted,  BASENAME.new is renamed into BASENAME.tex

  -mkfile <command>

     runs <command> <filename> every time a (master or slave) buffer is saved,
     and before reformating or recompiling. The default is to do nothing. 
     <filename> is the path of buffer-file-name relative to the master file
     directory.

  -makeindex <command>

     uses <command> instead of 'makeindex' to build index if needed.
     If command is false, indexes will never be recompiled.

  -initex <command>

     uses <command> instead of the default, usually 'initex', 
     to create the initial format. 

  -latex <command>

     This tells whizzytex to use <command> to compile a slice instead of
     the default (usually latex)

  -format <format>

     uses <format> instead the default, usually 'latex' 
     (the format extension ---see below--- is always added). 
     For instance, hugelatex may be needed for processing large files.

  -fmt <suffix>

     uses the <suffix> extension for format files instead  of fmt. 

  -dvicopy <command>

     uses <command> instead of the default (mv) to copy the DVI files
     (from FILE.dvi to FILE.wdvi). This can be used with command dvicopy,
     so as to expand virtual fonts, which advi does not understand yet.

  -duplex

     also launches a previewer on the whole document.

  -watch

     watches other files than just latex sources.

  -trace

     traces all commands of the script. 

%; whizzy-save <emacs-lisp-function>

     This assigns <emacs-lisp-function> to variable `whizzy-save'.
     (See the documentation.)

%; whizzy-paragraph <regexp>

  <regexp>
     defined overrides the slicing mode to paragraph and locally overrides
     the default paragraph regexp."
  (interactive)
  (let ((master (whizzy-get whizzy-master-buffer)))
    (cond
     ((not (buffer-live-p master))
      (error "Master buffer is undefined or killed"))
     (whizzy-slave
      (whizzy-write-file-config
       "whizzy-master"
       (whizzy-relative-dir (current-buffer) (whizzy-get whizzy-master-buffer))
       "whizzy.*-master"))
     ((equal (current-buffer) master)
      (let* ((slicing (whizzy-get whizzy-slicing-mode))
             (view (whizzy-get whizzy-view-mode))
             (type (car view))
             (command
              (if (equal (cadr view) (cadr (assoc type whizzy-viewers))) nil
                (cadr view)))
             (options (cddr view)))
         (whizzy-write-file-config
          "whizzy"
          (mapconcat
           'identity
           (list (and slicing (symbol-name slicing))
                 (and (or command (not (equal type (caar whizzy-viewers))))
                      type)
                 command
                 options)
           " "))
         ))
     (t (error "Current buffer is neither master nor slave"))
     )
    (if (local-variable-p 'whizzy-paragraph-regexp (current-buffer))
        (whizzy-write-file-config "whizzy-paragraph"
                                  (concat "\"" whizzy-paragraph-regexp "\"")))
    ))

(defun whizzy-string-from-file (keyword count &optional regexp position)
  (save-excursion
    (beginning-of-buffer)
    (or position (setq position 2))
    (setq regexp (or regexp "\\([^\n]*[^ \n]\\)"))
    (if (and
         keyword
         (re-search-forward
          (concat "^%; *" keyword " +" regexp " *$")
          400 'move count))
        (if (match-beginning position)
            (match-string position)
          (match-string 1))
        
      nil
      )))

(defun whizzy-recenter-output-buffer (line)
  "Redisplay buffer of WhizzyTeX job output.

This makes the most recent output visible.  The last line of the buffer
is displayed on line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (whizzy-get whizzy-process-buffer))
        (old-buffer (current-buffer)))
    (if (null tex-shell)
        (message "No WhizzyTeX output buffer")
      (pop-to-buffer tex-shell)
      (bury-buffer tex-shell)
      (goto-char (point-max))
      (recenter (if line
                    (prefix-numeric-value line)
                  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))


;;; errors

(defvar whizzy-shrink-output t
  "*If true (recommended), the ouput buffer shrinks automatically. 

Otherwise, output is kept as long as the window is visible 
 (may be used for debugging in combination with trace).
")

(defun whizzy-shrink-output (pos)
  (if (and whizzy-shrink-output (whizzy-get whizzy-initialized)
           (or (window-live-p (whizzy-get whizzy-process-window))
               (> (point-max) 16000)))
      (delete-region (point-min) pos)
    ))

(defvar whizzy-latex-error-regexp
  "^\\(! Missing \\|! Undefined \\|! LaTeX Error:\\|! Package \\|Runaway argument?\\)")


(defun whizzy-check-errors ()
  (interactive "p")
  (let ((source-buffer (whizzy-get whizzy-active-buffer))
        (shell-buffer (whizzy-get whizzy-process-buffer))
        (error-begin) (error-end) (line-string) (shell-moveto)
        (beg)
        )
    (or (buffer-live-p source-buffer)
        (setq source-buffer (current-buffer)))
    (set-buffer shell-buffer)
    (goto-char (point-max))
    (if (re-search-backward
         "<[^\n]*\\(compilation\\|formating\\) failed>" 
         (point-min) 'move)
        (whizzy-shrink-output (point)))
    (setq beg (point))
    (if (re-search-forward
         "^l\\.\\([1-9][0-9]*\\) \\([^\n]*\\)" (point-max) 'move)
        (progn
          (setq error-begin (string-to-int (match-string 1)))
          (setq shell-moveto (point))
          (setq line-string
                (buffer-substring
                 (max (match-beginning 2) (- (match-end 2) 36))
                 (match-end 2)))
          (and (save-excursion
                 (re-search-backward "^##line[+][=]\\([--+0-9]+\\)" beg t))
               (setq error-begin
                     (+ error-begin (string-to-int (match-string 1))))
               )
          ;; (message "line=%s" error-begin)
          (if (re-search-backward whizzy-latex-error-regexp
                                  (point-min) t)
              (setq shell-moveto (match-beginning 0))
            (setq shell-moveto (point))
            ))
      (setq shell-moveto 0)
      )
    (if shell-moveto
        (unwind-protect (goto-char shell-moveto)))
    (if (not error-begin)
        (whizzy-delete-error-overlay)
      (set-buffer source-buffer)
      (let ((here (point)))
        (unwind-protect
            (progn
              (goto-line error-begin)
              (beginning-of-line)
              (setq error-begin (point))
              (end-of-line)
              (if (and (> (length line-string) 0)
                       (search-backward line-string (- (point) 200) t))
                  (progn
                    (setq error-begin (match-beginning 0))
                    (setq error-end (match-end 0)))
                (setq error-end (point)))
              ;; (message "%S-%S:%S" error-begin error-end line-string)
              (whizzy-overlay-region
               error-begin
               (if (= 0 whizzy-last-slice-end)
                   (max error-end (+ error-begin 1))
                 (min (max error-end (+ error-begin 1))
                      whizzy-last-slice-end)))
              )
          (goto-char here))
        ))))



;; the whizzy-shell-buffer is shared between several processes.
;; it is always set to the buffer that saved the most recent slice.
;; however, occasionally, another concurrent process could report the error
;; to the wrong buffer.
;; this could be fixed by echoing the name of the file in the output buffer,
;; or by using a different buffer for each process.

;; if overlays do not currently work in xemacs.
;; use the unset options hook and ignore compilation messages.

(defvar whizzy-error-overlay nil)
(make-variable-buffer-local 'whizzy-error-overlay)
(put 'whizzy-error-overlay 'permanent-local t)

(defun whizzy-jump-to-error ()
  (interactive)
  (let ((status  (whizzy-get whizzy-running))
        (end (overlay-end whizzy-error-overlay)))
    (if (and status (> status 0))
        (if end (goto-char end)
          (message "Error line not found"))
      (message "No error"))
    ))

(defun whizzy-delete-error-overlay ()
  (if whizzy-error-overlay (delete-overlay  whizzy-error-overlay))
)

(defvar whizzy-error-face 'whizzy-error-face
  "*Face for error overlays.

This is a variable whose value should be a face.

Its default value is the face 'whizzy-error-face', which can be configured
with \\[customize-face]. You can also set this variable to another existing
face \(type \\[list-faces-display] for a list of existing faces).")

(unless (facep whizzy-error-face)
  (defface whizzy-error-face
    '((((class color)) (:background "Khaki")))
    "Face used for marking erros in in WhizzyTeX."
    :group 'whizzytex))

; (unless (facep whizzy-error-face)
;   (make-face 'whizzy-error-face)
;   (set-face-background 'whizzy-error-face "Khaki"))

(defun whizzy-overlay-region (beg end)
  (if (and whizzy-overlays (< beg end) (< end (point-max)))
      (progn
        (if  (not whizzy-error-overlay)
          (setq whizzy-error-overlay (make-overlay 1 1)))
        (overlay-put whizzy-error-overlay 'face whizzy-error-face)
        (move-overlay whizzy-error-overlay beg end (current-buffer))
        )))


(defun whizzy-show-wdiff (arg)
  (if (string-match
       "\\([0-9]+\\)\\([ac]\\).* Word \\([0-9]+\\)\\([ac]\\).*: \\([^\n]*\\)"
       arg)
      (let ((line
             (+ (string-to-int (match-string 1 arg))
                (if (equal (match-string 2 arg) "a") 1 0)))
            (word
             (- (string-to-int (match-string 3 arg))
                (if (equal (match-string 4 arg) "a") 0 1)))
            (words (match-string 5 arg)))

        (save-excursion
          (goto-line line)
          (if (or (<= (point) whizzy-last-slice-begin)
                  (>= (point) whizzy-last-slice-end))
              nil
            (beginning-of-line)
            (if (re-search-forward "[ \n]+" whizzy-last-slice-end t word)
                (progn
                  (skip-chars-backward "[ \n]" whizzy-last-slice-begin)
                  (let ((list-words  (split-string words "[ ]")) (beg (point)))
                    (while
                        (and (consp list-words)
                             (skip-chars-forward "[ \n]")
                             (looking-at (regexp-quote (car list-words))))
                      (goto-char (match-end 0))
                      (setq list-words (cdr list-words)))
                    (skip-chars-forward "[ \n]" whizzy-last-slice-end)
                    (whizzy-overlay-region beg (point))
                    ))
              )))
        )))

(defun whizzy-show-interaction (&optional arg)
  "Display WhizzyTeX process buffer according to ARG.
Toggle if ARG is ommitted."
  (interactive "P")
  (if (and whizzy-status
        (or whizzytex-mode (whizzy-get whizzy-active-buffer))) ;
      (let* ((buf (current-buffer))
             (shell  (whizzy-get whizzy-process-buffer))
             (pos)
             (window (whizzy-get whizzy-process-window))
             (window-alive
              (and window
                   (window-live-p window)
                   (equal (window-buffer window) shell)))
             (hide (if (null arg) window-alive
                     (= (prefix-numeric-value arg) 0)))
             (height (window-height))
             (resize))
        (if (and window-alive
                 (or (equal (minibuffer-window) window)
                     (equal (next-window window) window)))
            (if whizzy-status
                (whizzy-set whizzy-process-window nil))
          (if hide
              (if window-alive
                  (progn
                    (if whizzy-status (whizzy-set whizzy-process-window nil))
                    (delete-window window)
                    (bury-buffer shell)))
            
            (if (equal buf (whizzy-get whizzy-active-buffer))
                (save-selected-window
                  (set-buffer shell)
                  ;; select-window may change the buffer's position
                  (setq pos (point))
                  (if window-alive (select-window window)
                    (setq height (* (/ height 3) 2))
                    (split-window-vertically  height)
                    (select-window (next-window))
                    (switch-to-buffer shell t)
                    )
                  (goto-char pos)
                  (setq window (selected-window))
                  (setq resize
                        (- (window-height)
                           (max window-min-height
                                (min (window-buffer-height window)
                                     (/ (+ (window-height) height) 3)))))
                  (select-window (previous-window))
                  (enlarge-window resize)
                  ))
            (if whizzy-status (whizzy-set whizzy-process-window window)))
          )
        )
    (let ((buf (whizzy-get whizzy-process-buffer)))
      (if buf (display-buffer buf)
        (if (setq buf (get-buffer (concat "*" (buffer-name) "*")))
            (progn 
              (message "Warning! WhizzyTeX did not start successfully.")
              (display-buffer buf))
            (message "Sorry: no WhizzyTeX process buffer %S" buf))))
    ))

;; User timer instead of sit-for
;; sit-for does not work with xemacs and may disturb execution such as 
;; flashing parens, other sit-for, etc. 
(defvar whizzy-error-timer nil)
(defun whizzy-watch-error ()
  (if whizzy-error-timer (cancel-timer whizzy-error-timer))
  (setq whizzy-error-timer 
        (run-with-timer 2 nil 'whizzy-show-error (current-buffer)))
)
(defun whizzy-show-error (buffer)
  (let ((status (whizzy-get whizzy-running)))
    (if (and status (> status 0) (equal buffer (current-buffer)))
        (whizzy-show-interaction t)
      )))

(defun whizzy-auto-show (arg)
  (if whizzy-auto-show-output
      (if (or (= arg 0)
              (window-live-p (whizzy-get whizzy-process-window)))
          (whizzy-show-interaction arg)
        (if (not (whizzy-get whizzy-slice-fed))
            (whizzy-watch-error)))))

; (defun whizzy-auto-show (arg)
;   (if (and whizzy-auto-show-output
;            (or (= arg 0)
;                (or (and whizzy-xemacsp
;                         (= whizzy-last-tick (buffer-modified-tick)))
;                    (and (not (whizzy-get whizzy-slice-fed))
;                         ;; (whizzy-sit-for 2)
;                         )
;                    ))
;            (or (window-live-p (whizzy-get whizzy-process-window))
;                (not (= arg 0)))
;            )
;       (whizzy-show-interaction arg)))

(defun iso-translate-string (string trans-tab)
  "Use the translation table TRANS-TAB to translate STRING."
  (let ((work-tab trans-tab)
        (buffer-read-only nil)
        (case-fold-search nil)
        (argument string))
    (while work-tab
      (let ((trans-this (car work-tab))
            (string argument) (result "") (start 0) beg end)
        (while (string-match (car trans-this) string start)
          (setq beg (match-beginning 0)
                end (match-end 0)
                result (concat result (substring string start beg)
                               (car (cdr trans-this)))
                start end))
        (setq argument (concat result (substring string start)))
        (setq work-tab (cdr work-tab))))
    argument))


;; detex-ification
;; maybe, it should be done 

;; iso-translate is not available in xemacs...
(defun whizzy-tex2iso-string (string)
  "Replace tex accents to iso in STRING."
  (if (and (functionp 'iso-tex2iso)
           (or (boundp 'iso-tex2iso-trans-tab)
               (iso-tex2iso (point-max) (point-max)))
           (boundp 'iso-tex2iso-trans-tab)
           ; (string-match "latin\\|8859"
           ;               (symbol-name buffer-file-coding-system))
           (string-match "\\\\['`^\"c]\\|[!?]`" string))
      (iso-translate-string string iso-tex2iso-trans-tab)
    string))

(defun whizzy-detex (string)
  (let ((detex (whizzy-tex2iso-string string)))
    (if (string-equal string detex)
        (regexp-quote string)
      (concat "\\(" (regexp-quote string) "\\|" (regexp-quote detex) "\\)")
      )))

(defun whizzy-goto-file (file)
  "Find a buffer visiting file FILE and start WhizzyTeX if needed. 
Do nothing if not called from a buffer already running whizzytex. 
Implicitly add extension .tex to FILE if FILE does not exists, and fails if
FILE.tex does not exists either. If no buffer is visiting FILE, then visit
FILEaccording to the value of `whizzy-auto-visit'.  If the buffer associate
to FILE did not exits or was not in whizzytex-mode,  and the value of
`whizzy-auto-visit' is 'whizzytex, then start mode on the buffer.  
"
  (let ((dest-buffer (whizzy-get whizzy-active-buffer))
        (status whizzy-status))
    (if (buffer-live-p dest-buffer)
        (set-buffer dest-buffer)
      (setq dest-buffer nil))
    (and
     file (not (equal  file ""))
     (let* ((longname (concat (whizzy-get whizzy-dir) file))
            (fullname longname))
       (or (file-readable-p longname)
           (file-readable-p (setq fullname (concat longname ".tex")))
           (setq fullname longname))           
       (or (setq dest-buffer (find-buffer-visiting fullname))
           (and (or (and (equal whizzy-auto-visit 'ask)
                         (y-or-n-p (format "Visit file %s? " fullname)))
                    whizzy-auto-visit)
                (if (setq dest-buffer (find-file-noselect fullname))
                    (save-excursion
                      (set-buffer dest-buffer)
                      (setq whizzy-status status)
                      (setq whizzy-slave t)
                      t)
                  (message "File %s does not exits" fullname)
                  nil))))
     (progn
       (unless (minibuffer-window-active-p (minibuffer-window))
         (let ((temp pop-up-windows))
           (setq pop-up-windows whizzy-pop-up-windows)
           (pop-to-buffer dest-buffer)
           (setq pop-up-windows temp)
           (if whizzy-auto-raise (raise-frame))
           ))
       (set-buffer dest-buffer)
       (unless whizzytex-mode
         (or whizzy-status (setq whizzy-status status))
         (or whizzy-slave (setq whizzy-slave t))
         (if (equal whizzy-auto-visit 'whizzytex)
             (whizzytex-mode)
           (message
            (substitute-command-keys
             "Type \\[whizzytex-mode] to whizzytex this file"
             ))))
       ))
    dest-buffer
    )) 

(defun whizzy-goto-line (s)
  (if (string-match
"\#line \\([0-9]*\\), \\([0-9]+\\) \\(<<\\(.*\\)\\)?<<\\(.*\\)>><<\\([^>]*\\)>>\\(\\(.*\\)>>\\)? \\([^ \t\n]*\\)"
       s)
      (let ((line (string-to-int (match-string 1 s)))
            (last (string-to-int (match-string 2 s)))
            (left (match-string 4 s))
            (before (match-string 5 s))
            (after (match-string 6 s))
            (right (match-string 8 s))
            (file (match-string 9 s))
            (bound)
            )
        (cond
         ((or (not (whizzy-goto-file file))
              (and (> line last) (/= last 0)))
          nil)
         ((string-equal right "Next-Slice")
          (whizzy-previous-slice 1)
          (whizzy-observe-changes)
          )
         ((string-equal left "Previous-Slice")
          (whizzy-next-slice 1)
          (whizzy-observe-changes)
          )
         (t
          (setq before (whizzy-detex before))
          (setq after (whizzy-detex  after))
          (if left (setq left (whizzy-detex left)))
          (if right (setq right (whizzy-detex right)))
          (let*
              ((here (point))
               ;; (space "[\t ]*\n?[\t ]*")
               (blank "\\([\t ]\\|[^\\]%[^\n]*\n\\)*")
               (sp (concat blank "\n?" blank))
               (beg "\\(") (sep "[^A-Za-z0-9]") (end "\\)")
               (word (concat beg sep before end after sep))
               (context (concat beg left sp  before end after sp right))
               (left-context (concat beg sep  before end after sp right))
               (right-context (concat beg left sp  before end after sep))
               )
            (cond
             ((> last 0)
              (goto-line line) (beginning-of-line) (setq bound (point))
              (goto-line last) (end-of-line)
              (cond
               ((or (re-search-backward context bound t)
                    (re-search-backward left-context bound t)
                    (re-search-backward right-context bound t)
                    (re-search-backward word bound t))
                (goto-char (match-end 1)))
               ((and
                 (or
                  (re-search-backward word (max 0 (- bound 1000)) t)
                  (and 
                   (re-search-backward (concat "\\(" word "\\|" "\n\n\\)")
                                       (max 0 (- bound 5000)) t)
                   (looking-at word))))
                (goto-char (match-end 1)))
               ((progn (goto-char (point-max))
                       (re-search-backward context (point-min) t))
                (goto-char (match-end 1)))
               (t
                (goto-char here))
               ))
             ((> line 0)
              (if (= last 0) (setq bound (point-max))
                (goto-line last) (end-of-line) (setq bound (point)))
              (goto-line line) (beginning-of-line)
              (if (not (re-search-forward word bound t))
                  (goto-char here)
                (goto-char (match-end 1)))
              )
             (t))
            (whizzy-observe-changes)
            )))
        )))

(defconst whizzy-slice-error 1)
(defconst whizzy-tex-error 2)
(defconst whizzy-format-error 3)
(defconst whizzy-reformat-message 4)
(defconst whizzy-recompile-message 5)
(defvar whizzy-error-name-alist
  `((,whizzy-slice-error . "-SLICE")
    (,whizzy-tex-error . "-LATEX")
    (,whizzy-format-error . "-FORMAT")
    (,whizzy-reformat-message . "-Reformat")
    (,whizzy-recompile-message . "-Recompile")
  "A-list mapping errors to string mode suffix.
Log file name is obtain from suffix by removing leading character."
))

(defun whizzy-running-message (mes)
  (save-excursion
    (if (whizzy-set-active-buffer)
        (let ((string (or (assoc mes whizzy-error-name-alist) nil)))
          (if (equal (whizzy-get whizzy-error-string) string) nil
            (whizzy-set whizzy-error-string string)
            (force-mode-line-update))
          ))))

(defun whizzy-error (error &optional clean)
  (let ((old (whizzy-get whizzy-running)))
    (if old
        (let ((new (if clean (if (= error old) (max 0 (- old 1)) old)
                     (max old error))))
          (whizzy-set whizzy-running new)
          (whizzy-running-message new)
          )))) 

(defun whizzy-set-active-buffer ()
  (and (whizzy-get whizzy-running)
       (let ((buffer (whizzy-get whizzy-active-buffer)))
         (and (buffer-live-p buffer) (set-buffer buffer)))))
          
(defun whizzy-edit-field (name val)
  (save-excursion
    ;; (message "%S %S" name val)
    (and val
         (not (string-equal val "*"))
         (looking-at
          (concat "\\([xywhdXYWHD]\\(=-?[0-9.]*\\)?,\\)*"
                  name "\\(=-?[0-9.]*\\|\\)?[,}]"))
         (progn
           (goto-char (match-beginning 3))
           (delete-region (point) (match-end 3))
           (insert "=" val)
           t))
    ))

(defun whizzy-edit (command name first line  file type dx dy)
  (let ((x) (y) (regexp))
    ;; (message "command=%S name=%S[%S] line=%S file=%S"
    ;;    command name first line file)
    (if (string-equal name "") nil
      (setq name (concat " *\n? *\\[" (regexp-quote name) "\\]")))
    (cond
     ((equal type 'moveto) (setq x "x" y "y"))
     ((equal type 'resizetop) (setq x "w" y "h"))
     ((equal type 'resizebot) (setq x "w" y "d"))
     (t (error "whizzy-edit")))
    (if (string-match "^\\([xywhd]\\)=1$" first)
      (setq first
            (concat "\\(" (regexp-quote first) "\\|"
                    (regexp-quote (match-string 1 first)) "\\)"))
      (setq first (regexp-quote first)))
    (setq regexp
          (concat (regexp-quote command) "\\*?" name
                  " *\n? *{\\([^}]*\\b" first "\\(,[^}]*\\)*\\)}"))
    ;; (message "%S" regexp)
    (save-window-excursion
      (save-excursion
        (and (whizzy-goto-file file)
             (prog1 (goto-line (string-to-int line))
               (end-of-line))
             (or (re-search-backward regexp (point-min) t)
                 (re-search-forward regexp (point-max) t))
             (let ((begin  (match-beginning 1))
                   (modified  (buffer-modified-p))
                   (edited))
               (goto-char begin)
               ;; (message "%S=%S %S=%S" x dx y dy)
               (setq edited (whizzy-edit-field x dx))
               (setq edited (or (whizzy-edit-field y dy) edited))
               (unless (not edited)
                 (if (or modified
                         (equal (whizzy-get whizzy-active-buffer)
                                (current-buffer)))
                     (whizzy-observe-changes) 
                   (save-buffer)
                   (whizzy-reslice)
                   ))
               
               ))
        ))))

(defun whizzy-filter-output (s)
  (let ((commands (split-string s "[\n]")) command)
    (while commands
      (setq command (car commands) commands (cdr commands))
      (save-current-buffer
        (cond
         ((null command) (message "NIL COMMAND"))
         ;; nop, but most frequent command
         ((string-equal "<Waiting>" command))
         ;; cleanup
         ((string-equal "<Compilation succeeded>" command)
          (goto-char (point-max))
          (whizzy-shrink-output (point)) ;; (- (point) (length command)) ? 
          (if (whizzy-set-active-buffer)
              (progn
                (whizzy-delete-error-overlay)
                (whizzy-auto-show 0)
                (whizzy-error whizzy-slice-error t)
                (whizzy-set-time t)
                ))
          )
         ;; those do not 
         ((string-equal "<Continuing>" command))
         ;; (whizzy-check-errors)
         ;; (and (whizzy-set-active-buffer) (whizzy-auto-show 1))
         ;; )
         ((string-equal "<Reformatting failed>" command)
          (whizzy-error whizzy-format-error)
          (whizzy-auto-show 1))
         ((string-equal "<Reformatting succeeded>" command)
          (whizzy-error whizzy-format-error t)
          )
         ((string-equal "<Whole document recompilation failed>" command)
          (whizzy-error whizzy-tex-error)
          )
         ((string-equal "<Whole document updated>" command)
          (whizzy-error whizzy-tex-error t)
          )
         ((string-equal "<Pages and sections updated>" command)
          (whizzy-read-sections t)
          )
         ((string-equal "<Recompiling>" command)
          (whizzy-set whizzy-slice-fed nil)
          (whizzy-set-time nil)
          )
         ((string-equal "<Reformating>" command)
          (whizzy-running-message whizzy-reformat-message)
          )
         ((string-equal "<Recompiling whole document>" command)
          (whizzy-running-message whizzy-recompile-message)
          )
         ((string-equal "<Recompilation failed>" command)
          (whizzy-error whizzy-slice-error)
          (whizzy-set-time t)
          )
         ((string-match "^<Error in Line \\([^\n]*\\) *>" command)
          (beginning-of-line)
          (goto-char (point-max))
          (previous-line 1)
          (if (whizzy-set-active-buffer)
              (progn
                (whizzy-show-wdiff (match-string 1 s))
                (whizzy-auto-show 1)))
          )
         ((string-match "^\#line \\([0-9][0-9]*\\)" command)
          (whizzy-goto-line s)
          )

         ((string-match
           "^<edit \"\\([^ \t\n\"]*\\)\" \"\\([^ \t\n\"]*\\)\"\\[\\([^]]*\\)\\] #\\([0-9]*\\) @\\([^ \t\n]*\\) \\(moveto\\|resizetop\\|resizebot\\) \\(-?[0-9.]*\\|\\*\\),\\(-?[0-9.]*\\|\\*\\)>" s)
          (if (whizzy-set-active-buffer)
              (whizzy-edit
               (match-string 1 s)
               (match-string 2 s)
               (match-string 3 s)
               (match-string 4 s)
               (match-string 5 s)
               (cond
                ((string-equal (match-string 6 s) "resizetop") 'resizetop)
                ((string-equal (match-string 6 s) "resizebot") 'resizebot)
                (t 'moveto))
               (match-string 7 s)
               (match-string 8 s)
               ))
          )
         ((string-equal "<Fatal error>" command)
          (if (whizzy-set-active-buffer)
              (progn
                (whizzy-mode-off)
                (whizzy-show-interaction t)))
          (message "External Fatal error. WhizzyTeX Mode switched off.")
          )
         ((string-equal "<Initialization failed>" command)
          (let ((file (concat (whizzy-get whizzy-output-dir)
                              "initialization")))
            (and (file-writable-p file)
                 (write-region (point-min) (point-max) file)))
          (whizzy-check-errors)
          (and (whizzy-set-active-buffer)
               (whizzy-show-interaction t)
               )
          (whizzy-mode-off 'master)
          (message "Initialization failed")
          )
         ((string-equal "<Initialization succeeded, entering loop>" command)
          (let ((file (concat (whizzy-get whizzy-output-dir)
                              "initialization")))
            (and (file-writable-p file)
                 (write-region (point-min) (point-max) file)))
          (whizzy-set whizzy-initialized t)
          (if (whizzy-get whizzy-debug)
              (progn
                (message
                 "Initialization succeeded, you may turn debug mode off")
                (sit-for 2)
                (where-is 'whizzy-toggle-debug)))
          )
         ((string-equal "<Quitting>" command) 
          ;; (if (whizzy-set-active-buffer) (whizzy-mode-off))
          (whizzy-mode-off 'master)
          (message "Mode switched off externally")
          )
         ((string-match "<Viewlog>\\(.*\\)" command)
          (let (;; (line (match-string 1)) 
                (file
                 (expand-file-name "texlog" (whizzy-get whizzy-output-dir))))
            (unless (not (file-readable-p file))
              (goto-char (point-max))
              (if (search-backward "<Viewlog>")
                  (delete-region (match-beginning 0) (match-end 0))
                ;; (message "NOT FOUND")
                )
              ;; (whizzy-shrink-output (point))
              (insert-file-contents file)
              (goto-char (point-max))
              (whizzy-check-errors)
              (and (whizzy-set-active-buffer) (whizzy-auto-show 1))
              )))
         ;; Watch it
         ;; ((string-equal "<Continuing with the old format>" command))
         ;; (t (message "UNMATCHED: %s" command))
         )
        )
      )))


(defvar whizzy-slice-face 'whizzy-slice-face
  "*Face for mark out-of-slice part of the buffer.
This is a variable whose value should be a face.")

(unless (facep whizzy-slice-face)
  (defface whizzy-slice-face
    '((((class color))
       (:background "LightGray")
       (:foreground "dim gray")
       ))
    "Face used for marking text out of the current slice in WhizzyTeX."
    :group 'whizzytex))

(defvar whizzy-slice-overlay nil)
(make-variable-buffer-local 'whizzy-slice-overlay)
(put 'whizzy-slice-overlay 'permanent-local t)

(defun whizzy-create-slice-overlays ()
  (if (and whizzy-overlays (not whizzy-slice-overlay))
      (let ((beg (make-overlay 1 1)) (end (make-overlay 1 1)))
        (overlay-put beg 'face whizzy-slice-face)
        (overlay-put end 'face whizzy-slice-face)
        (setq whizzy-slice-overlay (cons beg end)))))

(defun whizzy-move-slice-overlays (&optional beg end)
  (if (and whizzy-overlays whizzy-slice-overlay)
      (progn
        (move-overlay (car whizzy-slice-overlay)
                      (point-min) (or beg whizzy-last-slice-begin))
        (move-overlay (cdr whizzy-slice-overlay)
                      (or end whizzy-last-slice-end) (point-max)))))

(defun whizzy-delete-slice-overlays ()
  (if (and whizzy-overlays whizzy-slice-overlay)
      (progn
        (delete-overlay (car whizzy-slice-overlay))
        (delete-overlay (cdr whizzy-slice-overlay)))))

;; to move according to the mode

(defun whizzy-next-slice (n)
  "Move forward N slices (or backward if N is negative)."
  (interactive "p")
  (if whizzy-begin
      (let* ((begin-document "^[ \t]*\\\\begin{document}[ \t\n]*")
             (begin-regexp
              (concat "\\(" begin-document 
                      "\\|" ;; "\\(\\)\\|\\(\\)"
                      whizzy-begin "\\)"))
             (end-regexp
              (concat "\\(^[ \t]*\\\\end *{document}\\|^\\\\endinput\\|"
                      whizzy-begin "\\)"))
             (beg 
              (cond
               ((> n 0)
                (or (and (re-search-forward begin-regexp (point-max) 'move n)
                         (match-beginning 0))
                    (and (re-search-backward begin-regexp (point-min) t)
                         (goto-char (match-end 0))
                         (match-beginning 0))
                    (goto-char (point-min))))
               ((< n 0)
                (or (and (re-search-backward begin-regexp (point-min) t
                                             (- 1 n))
                         (goto-char (match-end 0))
                         (match-beginning 0))
                    (and (re-search-backward begin-regexp (point-min) t)
                         (looking-at begin-document)
                         (goto-char (match-end 0))
                         (match-beginning 0))
                    (goto-char (point-min))))
               (t
                (end-of-line)
                (or (and (re-search-backward begin-regexp (point-min) 'move)
                         (goto-char (match-end 0))
                         (match-beginning 0))
                    (goto-char (point-min))))
               ))
             (here (point))
             (end
              (or (and (re-search-forward end-regexp (point-max) t)
                       (goto-char (match-beginning 0)))
                  (point-max)))
             )
        (goto-char beg)
        (if (looking-at begin-document) (setq beg (match-end 0)))
        (goto-char here)
        (whizzy-move-slice-overlays beg end)
        (recenter 2)
        )))

(defun whizzy-previous-slice (n)
  "Move backward N slices (or forward if N is negative)."
  (interactive "p")
  (whizzy-next-slice (- 0 n))
  )


;;; moving pages in the previewer from the emacs buffer
;; asumes you have a command sendkey (see the efuns ocaml xlib)

(defvar whizzy-sendkey-command "sendKey"
  "The name of the command to send keys to windows.
It should accept the following arguments

   -name <WINDOW-NAME>
   -key  <KEY-NAME>
   <CHARACTERS-STRING>"
)

(defun whizzy-toggle-point ()
  "Toggle `whizzy-point-visible' variable."
  (interactive)
  (setq whizzy-point-visible (not whizzy-point-visible))
  (setq whizzy-last-tick 0)
  (setq whizzy-last-point 0)
  )
(defun whizzy-toggle-line ()
  "Toggle `whizzy-line' variable."
  (interactive)
  (setq whizzy-line (not whizzy-line))
  (setq whizzy-last-tick 0)
  )

(defun whizzy-toggle-auto-show ()
  "Toggle `whizzy-auto-show-output' variable."
  (interactive)
  (setq whizzy-auto-show-output (not whizzy-auto-show-output))
  )

(defun whizzy-toggle-delete-output ()
  "Toggle `whizzy-shrink-output' variable."
  (interactive)
  (setq whizzy-shrink-output (not whizzy-shrink-output)))

(defun whizzy-toggle-sync-lines ()
  "Toggle `whizzy-sync-lines' variable."
  (interactive)
  (setq whizzy-sync-lines (not whizzy-sync-lines)))


;;; menus and bindings

(defun whizzy-help ()
  (interactive)
  (if (and whizzy-xemacsp (functionp 'hyper-describe-function))
      (apply 'hyper-describe-function '(whizzytex-mode))
    (describe-function 'whizzytex-mode)
    ))

(defun whizzy-help-view-slice ()
  (interactive)
  (view-file-other-window (whizzy-get whizzy-slicename)))

(defun whizzy-view-log (&optional file)
  (interactive)
  (unless
      (and file
           (setq file (expand-file-name (whizzy-get whizzy-output-dir)))
           (file-readable-p file))
    (or file
        (and (setq file (cdr (assoc (whizzy-get whizzy-running)
                               whizzy-error-name-alist)))
             (setq file (downcase (substring file 1)))))
    (setq file
          (read-file-name "Log: " (whizzy-get whizzy-output-dir)
                          nil t file)))
  (if (equal (expand-file-name file) buffer-file-name)
      (message "Aborted (no selection)")
    (let ((buf (get-file-buffer file)))
      (and buf
           (not (verify-visited-file-modtime buf))
           (file-exists-p file)
           (not (buffer-modified-p buf))
           (with-current-buffer buf (revert-buffer t t))
           ;; (kill-buffer buf)
           )
      (view-buffer-other-window (find-file-noselect file) nil)
      (goto-char (point-min))
      (re-search-forward "^! " (point-max) t))))


(defun whizzy-slicing-paragraph () (interactive)
  (whizzy-change-mode 'paragraph)
  (setq whizzy-begin whizzy-paragraph-regexp)
  )

(defun whizzy-slicing-wide-paragraph () (interactive)
  (whizzy-change-mode 'paragraph)
  (setq whizzy-begin  "\n *\n *\n *\n")
  )

(defun whizzy-set-paragraph-regexp  () (interactive)
  (setq whizzy-paragraph-regexp
        (read-string "Paragraph regexp: "
                     (cons whizzy-paragraph-regexp 0) nil))
  (whizzy-slicing-paragraph)
  )

(defun whizzy-unmaster () (interactive)
  (whizzy-mode-off) (setq whizzy-slave nil))

(defun whizzy-show-point-never () (interactive)
  (setq whizzy-point-visible nil))
(defun whizzy-show-point-pessimistic () (interactive)
  (setq whizzy-point-visible 'whizzy-show-point-safer))
(defun whizzy-show-point-optimistic () (interactive)
  (setq whizzy-point-visible t))

(defun whizzy-slicing-mode-p (mode)
  (equal (whizzy-get whizzy-slicing-mode) mode))

(defun whizzy-slice-adjust (arg)
  (interactive "NNumber of pages: ")
  (setq whizzy-slice-pages arg))

(defvar whizzy-menu-map
  (if whizzy-xemacsp nil
    (let ((menu-map (make-sparse-keymap "Whizzy")))
      (define-key menu-map [whizzy-help]
        '("Help" . whizzy-help))
      (define-key menu-map [sep3] '("--"))
      (define-key menu-map [whizzy-jump-to-error]
        '("Jump to error" . whizzy-jump-to-error))
      (put 'whizzy-jump-to-error 'menu-enable 'whizzytex-mode)
      (define-key menu-map [whizzy-view-log]
        '("View log..." . whizzy-view-log))
      (put 'whizzy-view-log 'menu-enable
           '(file-exists-p  (whizzy-get whizzy-output-dir)))
      (define-key menu-map [whizzy-show-interaction]
        '("Show interaction" . whizzy-show-interaction))
      (define-key menu-map [sep2] '("--"))
      (define-key menu-map [whizzy-toggle-debug]
        '(menu-item "Debug"  whizzy-toggle-debug
                    :button (:toggle whizzy-get whizzy-debug)))
      (define-key menu-map [whizzy-toggle-sync-lines]
        '(menu-item "Sync lines" whizzy-toggle-sync-lines
                    :button (:toggle and whizzy-sync-lines)))
      (define-key menu-map [whizzy-toggle-delete-output]
        '(menu-item "Auto shrink output"  whizzy-toggle-delete-output
                    :button (:toggle and whizzy-shrink-output)))
      (define-key menu-map [whizzy-toggle-auto-show]
        '(menu-item "Auto interaction"  whizzy-toggle-auto-show
                    :button (:toggle and whizzy-auto-show-output)))
      (define-key menu-map [whizzy-toggle-line]
        '(menu-item "Page to point"  whizzy-toggle-line
                    :button (:toggle and whizzy-line)))
      (let ((map (make-sparse-keymap "Show point")))
        (define-key map [never]
          '(menu-item "Never"  whizzy-show-point-never
                      :button (:radio equal whizzy-point-visible nil)))
        (define-key map [pessimistic]
          '(menu-item "Be pessimistic" whizzy-show-point-pessimistic
                      :button (:radio equal whizzy-point-visible
                                      'whizzy-show-point-safer)))
        (define-key map [optimistic]
          '(menu-item "Be optimistic"  whizzy-show-point-optimistic
                      :button (:radio equal whizzy-point-visible t)))
        (define-key menu-map [show-point]
          (cons "Show point" map)))
      (define-key menu-map [whizzy-customize]
        '("Customize slice" . whizzy-customize ))
      (define-key menu-map [whizzy-load-factor]
        '("Load factor" . whizzy-load-factor ))
      (define-key menu-map [sep1] '("--"))
      (define-key menu-map [whizzy-write-configuration]
        '("Write config" . whizzy-write-configuration))
      (put 'whizzy-write-configuration 'menu-enable 'whizzy-status)
      (define-key menu-map [whizzy-unmaster]
        '("Unmaster file" . whizzy-unmaster))
      (put 'whizzy-unmaster 'menu-enable 'whizzy-slave)
      (define-key menu-map [whizzy-suspend]
        '(menu-item "Suspend/Resume" whizzy-suspend ([ whizzy-suspend ])
                    :button (:toggle and (equal whizzytex-mode 'suspended))))
      (put 'whizzy-suspend 'menu-enable 'whizzytex-mode)
      (define-key menu-map [whizzy-previous-slice]
        '("Previous slice" . whizzy-previous-slice))
      (define-key menu-map [whizzy-next-slice]
        '("Next slice" . whizzy-next-slice))
      (define-key menu-map [whizzy-duplex]
        '("Duplex" . whizzy-duplex))
      (put 'whizzy-duplex 'menu-enable 'whizzytex-mode)
      (let ((map (make-sparse-keymap "Slicing"))
            (modes whizzy-mode-regexp-alist) (mode))
        (define-key map [other]
          '("other" . whizzy-change-mode))
        (define-key map [adjust]
          '("adjust" . whizzy-slice-adjust))
        (define-key map [paragraph-regexp]
          '("paragraph regexp" . whizzy-set-paragraph-regexp))
        (define-key map [sep1] '("--"))
        (while (and (consp modes) (consp (car modes)))
          (setq mode (caar modes))
          (if (equal mode 'none)
              (define-key map [sep2] '("--")))
          (define-key map (vector mode)
            (list 'menu-item
                  (symbol-name mode)
                  (list 'lambda nil '(interactive)
                        (list 'whizzy-change-mode  (list 'quote mode)))
                  ':button
                  (list ':radio 'whizzy-slicing-mode-p
                        (list 'quote mode))
                  ))
          ;; (put mode 'menu-enable (equal whizzytex-mode mode))
          (setq modes (cdr modes))
          (define-key menu-map [slicing]
            (cons "Slicing"  map))
          ; (put 'map 'menu-enable 'whizzytex-mode)
          (put 'slicing 'menu-enable 'whizzytex-mode)
          ))
      (define-key menu-map [whizzy-recompile]
        '("Recompile" . whizzy-recompile))
      (put 'whizzy-recompile 'menu-enable 'whizzytex-mode)
      (define-key menu-map [whizzy-reformat]
        '("Reformat" . whizzy-reformat))
      (put 'whizzy-reformat 'menu-enable 'whizzytex-mode)
      (define-key menu-map [whizzytex-mode]
        '(menu-item "WhizzyTeX"  whizzytex-mode
                    :button (:toggle and whizzytex-mode)))
      menu-map)))

(defvar whizzy-xemacsp-menu
  (if whizzy-xemacsp
      '("Whizzy"
        [ "WhizzyTeX" whizzytex-mode
          :style toggle :selected whizzytex-mode ]
        [ "Reformat" whizzy-reformat :active whizzytex-mode ]
        [ "Recompile" whizzy-recompile :active whizzytex-mode ]
        [ "Duplex" whizzy-duplex :active whizzytex-mode ]
        ("Slicing"
         [ "none" (whizzy-change-mode 'none)
           :style radio :selected (whizzy-slicing-mode-p 'none) ]
         "---"
         [ "document" (whizzy-change-mode 'document)
           :style radio
           :selected (whizzy-slicing-mode-p 'document) ]
         [ "section" (whizzy-change-mode 'section)
           :style radio :selected (whizzy-slicing-mode-p 'section) ]
         [ "subsection" (whizzy-change-mode 'subsection)
           :style radio :selected (whizzy-slicing-mode-p 'subsection) ]
         [ "subsubsection" (whizzy-change-mode 'subsubsection)
           :style radio :selected (whizzy-slicing-mode-p 'subsubsection) ]
         [ "letter" (whizzy-change-mode 'letter)
           :style radio :selected (whizzy-slicing-mode-p 'letter) ]
         [ "slide" (whizzy-change-mode 'slide)
           :style radio :selected (whizzy-slicing-mode-p 'slide) ]
         "---"
         [ "adjust" whizzy-slice-adjust t ]
         [ "other" whizzy-change-mode t ]
         )
        [ "Suspend/Resume" whizzy-suspend :active whizzytex-mode
          :style toggle :selected (equal whizzytex-mode 'suspended) ]
        [ "Next slice"  whizzy-next-slice ]
        [ "Previous slice"  whizzy-previous-slice ]
        [ "Unmaster" whizzy-unmaster :active whizzy-slave ]
        [ "Write config" whizzy-write-configuration
          :active whizzy-status ]
        "---"
        [ "Customize slice" whizzy-customize ]
        [ "Load factor" whizzy-load-factor ]
        [ "Page to point" whizzy-toggle-line
          :style toggle :selected whizzy-line ]
        ( "Show point" 
          [ "Optimistic" whizzy-show-point-optimistic
            :style radio :selected (equal whizzy-point-visible t) ]
          [ "Pessimistic" whizzy-show-point-pessimistic
            :style radio :selected (equal whizzy-point-visible
                                          'whizzy-show-point-safer) ]
          [ "Never" whizzy-show-point-never
            :style radio :selected (equal whizzy-point-visible nil) ]
          )
        [ "Auto interaction" whizzy-toggle-auto-show
          :style toggle :selected whizzy-auto-show-output ]
        [ "Auto shrink output" whizzy-toggle-delete-output
          :style toggle :selected whizzy-shrink-output ]
        [ "Sync lines" whizzy-toggle-sync-lines
          :style toggle :selected whizzy-sync-lines ]
        [ "Debug" whizzy-toggle-debug
          :style toggle :selected (whizzy-get whizzy-debug) ]
        "---"
        [ "Show interaction" whizzy-show-interaction ]
        [ "View log..."  whizzy-view-log :active
          (or whizzytex-mode (whizzy-get whizzy-debug)) ]
        [ "Jump to error"  whizzy-jump-to-error :active whizzytex-mode ]
        "---"
        [ "Help" whizzy-help t ]
        ))
  "Menu to add to the menubar when running Xemacs.")

;; keymap
; (defvar whizzytex-mode-keymap
;   (let ((map (make-sparse-keymap)))
;     (define-key map [?\C-c ?\C-w] 'whizzytex-mode)
;     (define-key map [?\C-c ?\C-s] 'whizzy-change-mode)
;     (define-key map [?\C-c ?\C-z] 'whizzy-suspend)
;     ;; xemacs style bindings are apparently also accepted by emacs
;     (if whizzy-xemacsp nil
;       (define-key map [menu-bar whizzy] (cons "Whizzy" (whizzy-menu-map))))
;     (define-key map [(control next)] 'whizzy-next-slice)
;     (define-key map [(control prior)] 'whizzy-previous-slice)
;     (define-key map [(control return)] 'whizzy-show-interaction)
;     ;; emacs style bindings
;     ;;  (define-key map [C-next] 'whizzy-next-slice)
;     ;;  (define-key map [C-prior] 'whizzy-previous-slice)
;     ;;  (define-key map [C-return] 'whizzy-show-interaction)
;     map)
;    "Keymap for WhizzyTeX minor mode."
; )

; (or (not (boundp 'minor-mode-map-alist))
;     (let ((mode (assq 'whizzytex-mode minor-mode-map-alist)))
;       (if mode (setcdr mode whizzytex-mode-keymap)
;         (setq  minor-mode-map-alist
;           (cons (cons 'whizzytex-mode whizzytex-mode-keymap)
;                 minor-mode-map-alist)))))

;; two useful hooks
(defvar whizzy-common-bindings
  '(( [(control next)] . whizzy-next-slice)
    ( [(control prior)] . whizzy-previous-slice)
    ( [(control return)] . whizzy-show-interaction))
  "Common default bindings")

(defvar whizzy-short-bindings
  `(( [?\C-c ?\C-w] . whizzytex-mode)
    ( [?\C-c ?\C-s] . whizzy-change-mode)
    ( [?\C-c ?\C-r] . whizzy-reformat)
    ( [?\C-c ?\C-z] . whizzy-suspend)
    ( [?\C-c ?\C-d] . whizzy-duplex)
    ( [?\C-c ?\C-=] . whizzy-customize)
    ( [?\C-c ?\C-1] . whizzy-view-log)
    ( [?\C-c ?\C-j] . whizzy-jump-to-error)
    . ,whizzy-common-bindings)
  "Short bindings for WhizzyTeX"
  )

(defvar whizzy-auctex-bindings
  `(( [?\C-c ?w] . whizzytex-mode)
    ( [?\C-c ?s] . whizzy-change-mode)
    ( [?\C-c ?r] . whizzy-reformat)
    ( [?\C-c ?z] . whizzy-suspend)
    ( [?\C-c ?d] . whizzy-duplex)
    ( [?\C-c ?\C-1] . whizzy-view-log)
    ( [?\C-c ?j] . whizzy-jump-to-error)
    . ,whizzy-common-bindings)
  "Bindings for running WhizzyTeX with AuxTeX"
  )

(defvar whizzy-key-bindings 'whizzy-auctex-bindings
  "*Alist of pairs key bindings installed by `whizzy-default-bindings'
This may also be a symbol whose value is an alist. 

Either `whizzy-auctex-bindings' or `whizzy-short-bindings'
could be used as default-value, both of which define bindings for
the following functions:

  Toggle WhizzyTeX mode         \\[whizzytex-mode]
  Move one slice forward        \\[whizzy-next-slice]
  Move one slice backward       \\[whizzy-previous-slice]
  Change slicing mode           \\[whizzy-change-mode]
  Show interaction              \\[whizzy-show-interaction]
  Suspend/Resume                \\[whizzy-suspend]

For instance, you may include the following line in your ~/.emacs:

  (setq-default whizzy-key-bindings 'whizzy-auctex-bindings)
")

(defun whizzy-default-bindings ()
  "*Suggested hook for whizzytex.

Install bindings defined in the variable `whizzy-key-bindings'
as well as a tool bar menu `whizzytex-mode-map' in the current local map
Does nothing if there is no current local map."
  (let ((map (current-local-map)))
    (unless
        ;; do nothing if there is no local map
        (not (keymapp map)) 
      (mapcar '(lambda (b) (if b (define-key map (car b) (cdr b))))
              (eval whizzy-key-bindings))
      (if whizzy-xemacsp
          (progn
            (eval '(if (and (featurep 'menubar) current-menubar)
                       (progn
                         (set-buffer-menubar current-menubar)
                         (add-submenu nil whizzy-xemacsp-menu))))
            )
        ;; (define-key map [menu-bar] (make-sparse-keymap))
        (define-key map [menu-bar whizzy] (cons "Whizzy" whizzy-menu-map))
        )
      ))
  )

(defun whizzy-unset-options ()
  "*Hook to unset some default options (WhizzyTeX may then run safer)."
  (setq whizzy-auto-show-output nil)
  (setq whizzy-point-visible nil)
  (setq whizzy-line nil)
)


(provide 'whizzytex)

;;; whizzytex.el ends here
