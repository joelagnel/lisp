;;;; texihtml.el

;;; Emacs lisp functions to convert Texinfo files to HTML files.

(defvar texihtml-version "0.01 of 17 Jun 1993")

;;; Erik C. Ostrom (eostrom@iicm.tu-graz.ac.at)
;;; but mostly ripped off from texinfmt.el, by
;;; Robert J. Chassell          

;;; Copyright (C) 1985, 1986, 1988,
;;;               1990, 1991, 1992, 1993  Free Software Foundation, Inc.


;;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Variable definitions

(require 'texinfo)          ; So `texinfo-footnote-style' is defined.
(require 'texnfo-upd)       ; So `texinfo-section-types-regexp' is defined.

(defvar texihtml-format-syntax-table nil)

(defvar texihtml-vindex)
(defvar texihtml-findex)
(defvar texihtml-cindex)
(defvar texihtml-pindex)
(defvar texihtml-tindex)
(defvar texihtml-kindex)
(defvar texihtml-last-node)
(defvar texihtml-node-names)
(defvar texihtml-enclosure-list)


;;; Syntax table

(if texihtml-format-syntax-table
    nil
  (setq texihtml-format-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " texihtml-format-syntax-table)
  (modify-syntax-entry ?\\ " " texihtml-format-syntax-table)
  (modify-syntax-entry ?@ "\\" texihtml-format-syntax-table)
  (modify-syntax-entry ?\^q "\\" texihtml-format-syntax-table)
  (modify-syntax-entry ?\[ "." texihtml-format-syntax-table)
  (modify-syntax-entry ?\] "." texihtml-format-syntax-table)
  (modify-syntax-entry ?\( "." texihtml-format-syntax-table)
  (modify-syntax-entry ?\) "." texihtml-format-syntax-table)
  (modify-syntax-entry ?{ "(}" texihtml-format-syntax-table)
  (modify-syntax-entry ?} "){" texihtml-format-syntax-table)
  (modify-syntax-entry ?\' "." texihtml-format-syntax-table))


;;; Top level buffer and region formatting functions

(defun texihtml-format-buffer ()
  "Process the current buffer as texihtml code, into an HTML file.
The HTML file output is generated in a directory named FILE-html, where FILE
is the name indicated with the @setfilename command; within that directory,
each node is stored as a file with the node's name."
  (interactive)
  (let ((lastmessage "Formatting HTML file..."))
    (message lastmessage)
    (texihtml-format-buffer-1)
    (message (concat lastmessage
                     (if (interactive-p) "done.")))))

(defvar texihtml-region-buffer-name "*HTML Region*"
  "*Name of the temporary buffer used by \\[texihtml-format-region].")

(defun texihtml-format-region (region-beginning region-end)
  "Convert the current region of the Texihtml file to HTML format.
This lets you see what that part of the file will look like in HTML.
The command is bound to \\[texihtml-format-region].  The text that is
converted to HTML is stored in a temporary buffer."
  (interactive "r")
  (message "Converting region to HTML format...")
  (let (texihtml-command-start
        texihtml-command-end
        texihtml-command-name
        texihtml-vindex
        texihtml-findex
        texihtml-cindex
        texihtml-pindex
        texihtml-tindex
        texihtml-kindex
        texihtml-stack
        (texihtml-format-filename "")
        texihtml-example-start
        texihtml-last-node-pos
        texihtml-last-node
        texihtml-node-names
        (texihtml-footnote-number 0)
        last-input-buffer
        (fill-column-for-info fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory)
        (header-text "")
        (header-beginning 1)
        (header-end 1))
    
;;; Copy lines between beginning and end of header lines, 
;;;    if any, or else copy the `@setfilename' line, if any.
    (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((search-end (save-excursion (forward-line 100) (point))))
            (if (or
                 ;; Either copy header text.
                 (and 
                  (prog1 
                      (search-forward texihtml-start-of-header search-end t)
                    (forward-line 1)
                    ;; Mark beginning of header.
                    (setq header-beginning (point)))
                  (prog1 
                      (search-forward texihtml-end-of-header nil t)
                    (beginning-of-line)
                    ;; Mark end of header
                    (setq header-end (point))))
                 ;; Or copy @filename line.
                 (prog2
                  (goto-char (point-min))
                  (search-forward "@setfilename" search-end t)
                  (beginning-of-line)
                  (setq header-beginning (point))
                  (forward-line 1)
                  (setq header-end (point))))
                
                ;; Copy header  
                (setq header-text
                      (buffer-substring
                       (min header-beginning region-beginning)
                       header-end))))))

;;; Find a buffer to use.
    (switch-to-buffer (get-buffer-create texihtml-region-buffer-name))
    (erase-buffer)
    ;; Insert the header into the buffer.
    (insert header-text)
    ;; Insert the region into the buffer.
    (insert-buffer-substring
     input-buffer
     (max region-beginning header-end)
     region-end)
    ;; Make sure region ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    
    (goto-char (point-min))
    (texihtml-mode)
    (message "Converting region to Info format...")
    (setq fill-column fill-column-for-info)
    ;; Install a syntax table useful for scanning command operands.
    (set-syntax-table texihtml-format-syntax-table)

    ;; Insert @include files so `texihtml-raise-lower-sections' can
    ;; work on them without losing track of multiple
    ;; @raise/@lowersections commands. 
    (while (re-search-forward "^@include" nil t)
      (setq texihtml-command-end (point))
      (let ((filename (concat input-directory
                              (texihtml-parse-line-arg))))
        (beginning-of-line)
        (delete-region (point) (save-excursion (forward-line 1) (point)))
        (message "Reading included file: %s" filename)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point)
             (+ (point) (car (cdr (insert-file-contents filename)))))
            (goto-char (point-min))
            ;; Remove `@setfilename' line from included file, if any,
            ;; so @setfilename command not duplicated.
            (if (re-search-forward 
                 "^@setfilename" (save-excursion (forward-line 100) (point)) t)
                (progn
                  (beginning-of-line)
                  (delete-region
                   (point) (save-excursion (forward-line 1) (point)))))))))

    ;; Raise or lower level of each section, if necessary.
    (goto-char (point-min))
    (texihtml-raise-lower-sections)
    ;; Append @refill to appropriate paragraphs for filling.
    (goto-char (point-min))
    (texihtml-append-refill)
    ;; If the region includes the effective end of the data,
    ;; discard everything after that.
    (goto-char (point-max))
    (if (re-search-backward "^@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    ;; Don't use a previous value of texihtml-enclosure-list.
    (setq texihtml-enclosure-list nil)

    (goto-char (point-min))
    (if (looking-at "\\\\input[ \t]+texihtml")
        (delete-region (point) (save-excursion (forward-line 1) (point))))

    ;; Insert Info region title text.
    (goto-char (point-min))
    (if (search-forward 
         "@setfilename" (save-excursion (forward-line 100) (point)) t)
        (progn
          (setq texihtml-command-end (point))
          (beginning-of-line)
          (setq texihtml-command-start (point))
          (let ((arg (texihtml-parse-arg-discard)))
            (insert " "
              texihtml-region-buffer-name
              " buffer for:  `") 
            (insert (file-name-nondirectory (expand-file-name arg)))
            (insert "',        -*-Text-*-\n")))
      ;; Else no `@setfilename' line
      (insert " "
              texihtml-region-buffer-name
              " buffer                       -*-Text-*-\n"))
    (insert "produced by `texihtml-format-region'\n"
            "from a region in: "
            (if (buffer-file-name input-buffer)
                  (concat "`"
                          (file-name-sans-versions
                           (file-name-nondirectory
                            (buffer-file-name input-buffer)))
                          "'")
                (concat "buffer `" (buffer-name input-buffer) "'"))
              "\nusing `texinfmt.el' version "
              texihtml-version
              ".\n\n")

    ;; Now convert for real.
    (goto-char (point-min))
    (texihtml-format-scan)
    (goto-char (point-min))
    
    (message "Done.")))


;;; Primary internal formatting function for the whole buffer.

(defun texihtml-format-buffer-1 ()
  (let (texihtml-format-filename
        texihtml-example-start
        texihtml-command-start
        texihtml-command-end
        texihtml-command-name
        texihtml-last-node
        texihtml-vindex
        texihtml-findex
        texihtml-cindex
        texihtml-pindex
        texihtml-tindex
        texihtml-kindex
        texihtml-stack
        texihtml-node-names
        (texihtml-footnote-number 0)
        last-input-buffer
        outdir
        (fill-column-for-info fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory))
    (setq texihtml-enclosure-list nil)
    (save-excursion
      (goto-char (point-min))
      (or (search-forward "@setfilename" nil t)
          (error "Texihtml file needs an `@setfilename FILENAME' line."))
      (setq texihtml-command-end (point))
      (setq outdir (expand-file-name (concat (texihtml-parse-line-arg)
					     "-html"))))
    (if (file-exists-p outdir)
	(if (not (file-directory-p outdir))
	    (error "%s exists and is not a directory" outfile))
      (call-process "mkdir" nil nil nil outdir))
    (set-buffer (get-buffer-create "*texihtml-to-html*"))
    (texinfo-mode)
    (setq fill-column fill-column-for-info)
    (set-syntax-table texihtml-format-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (message "Converting %s to Info format..." (buffer-name input-buffer))
    
    ;; Insert @include files so `texihtml-raise-lower-sections' can
    ;; work on them without losing track of multiple
    ;; @raise/@lowersections commands. 
    (goto-char (point-min))
    (while (re-search-forward "^@include" nil t)
      (setq texihtml-command-end (point))
      (let ((filename (concat input-directory
                              (texihtml-parse-line-arg))))
        (beginning-of-line)
        (delete-region (point) (save-excursion (forward-line 1) (point)))
        (message "Reading included file: %s" filename)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point)
             (+ (point) (car (cdr (insert-file-contents filename)))))
            (goto-char (point-min))
            ;; Remove `@setfilename' line from included file, if any,
            ;; so @setfilename command not duplicated.
            (if (re-search-forward 
                 "^@setfilename"
                 (save-excursion (forward-line 100) (point)) t)
                (progn
                  (beginning-of-line)
                  (delete-region
                   (point) (save-excursion (forward-line 1) (point)))))))))
    ;; Raise or lower level of each section, if necessary.
    (goto-char (point-min))
    (texihtml-raise-lower-sections)
    ;; Append @refill to appropriate paragraphs
    (goto-char (point-min))
    (texihtml-append-refill)
    (goto-char (point-min))
    (search-forward "@setfilename")
    (beginning-of-line)
    (delete-region (point-min) (point))
    ;; Remove @bye at end of file, if it is there.
    (goto-char (point-max))
    (if (search-backward "@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    ;; Scan the whole buffer, converting to Info format.
    (texihtml-format-scan)
    ;; Return data for indices.
    (goto-char (point-min))
    (list outdir
          texihtml-vindex texihtml-findex texihtml-cindex
          texihtml-pindex texihtml-tindex texihtml-kindex)))


;;; Perform non-@-command file conversions: quotes and hyphens

(defun texihtml-format-convert (min max)
;;;  ;; Convert left and right quotes to typewriter font quotes.
;;;  (goto-char min)
;;;  (while (search-forward "``" max t)
;;;    (replace-match "\""))
;;;  (goto-char min)
;;;  (while (search-forward "''" max t)
;;;    (replace-match "\""))
  ;; Convert three hyphens in a row to two.
  (goto-char min)
  (while (re-search-forward "^\\(@@.*\\|[^@\n].*\\|$\\)\n\n+" max t)
    (replace-match "\\1<P>\n" t))
  (goto-char min)
  (while (re-search-forward "\\( \\|\\w\\)\\(---\\)\\( \\|\\w\\)" max t)
    (delete-region (1+ (match-beginning 2)) (+ 2 (match-beginning
    2)))))


;;; Handle paragraph filling

(defvar texihtml-no-refill-regexp
  "^@\\(example\\|smallexample\\|lisp\\|smalllisp\\|display\\|format\\|flushleft\\|flushright\\|menu\\|titlepage\\|iftex\\|tex\\)"
  "Regexp specifying environments in which paragraphs are not filled.")

(defvar texihtml-part-of-para-regexp
  "^@\\(b{\\|bullet{\\|cite{\\|code{\\|emph{\\|equiv{\\|error{\\|expansion{\\|file{\\|i{\\|inforef{\\|kbd{\\|key{\\|lisp{\\|minus{\\|point{\\|print{\\|pxref{\\|r{\\|ref{\\|result{\\|samp{\\|sc{\\|t{\\|TeX{\\|today{\\|var{\\|w{\\|xref{\\)"
  "Regexp specifying @-commands found within paragraphs.")

(defun texihtml-append-refill ()
  "Append @refill at end of each paragraph that should be filled.
Do not append @refill to paragraphs within @example and similar environments.  
Do not append @refill to paragraphs containing @w{TEXT} or @*."

  ;; It is necessary to append @refill before other processing because
  ;; the other processing removes information that tells Texihtml
  ;; whether the text should or should not be filled.
  
  (while (< (point) (point-max))
    (let ((refill-blank-lines "^[ \t\n]*$")
          (case-fold-search nil))       ; Don't confuse @TeX and @tex....
      (beginning-of-line)
      ;; 1. Skip over blank lines;
      ;;    skip over lines beginning with @-commands, 
      ;;    but do not skip over lines
      ;;      that are no-refill environments such as @example or
      ;;      that begin with within-paragraph @-commands such as @code.
      (while (and (looking-at (concat "^@\\|^\\\\\\|" refill-blank-lines))
                  (not (looking-at 
                        (concat
                         "\\(" 
                         texihtml-no-refill-regexp
                         "\\|" 
                         texihtml-part-of-para-regexp
                         "\\)")))
                  (< (point) (point-max)))
        (forward-line 1))
      ;; 2. Skip over @example and similar no-refill environments.
      (if (looking-at texihtml-no-refill-regexp)
          (let ((environment
                 (buffer-substring (match-beginning 1) (match-end 1))))
            (progn (re-search-forward (concat "^@end " environment) nil t)
                   (forward-line 1)))
        ;; 3. Do not refill a paragraph containing @w or @*
        (if  (or
              (>= (point) (point-max))
              (re-search-forward
               "@w{\\|@\\*" (save-excursion (forward-paragraph) (point)) t))
            ;; Go to end of paragraph and do nothing.
            (forward-paragraph) 
          ;; 4. Else go to end of paragraph and insert @refill
          (forward-paragraph)
          (forward-line -1)
          (end-of-line)
          (delete-region
           (point)
           (save-excursion (skip-chars-backward " \t") (point)))
          ;; `looking-at-backward' not available in v. 18.57
          ;; (if (not (looking-at-backward "@refill\\|@bye")) ;)
          (if (not (re-search-backward
                    "@refill\\|@bye"
                    (save-excursion (beginning-of-line) (point))
                    t))
              (insert "@refill"))
          (forward-line 1))))))


;;; Handle `@raisesections' and `@lowersections' commands

;; These commands change the hierarchical level of chapter structuring
;; commands. 
;;    
;; @raisesections changes @subsection to @section,
;;                        @section    to @chapter,
;;                        etc.
;;
;; @lowersections changes @chapter    to @section
;;                        @subsection to @subsubsection,
;;                        etc.
;;
;; An @raisesections/@lowersections command changes only those
;; structuring commands that follow the @raisesections/@lowersections
;; command.
;;
;; Repeated @raisesections/@lowersections continue to raise or lower
;; the heading level.
;; 
;; An @lowersections command cancels an @raisesections command, and
;; vice versa.
;;
;; You cannot raise or lower "beyond" chapters or subsubsections, but
;; trying to do so does not elicit an error---you just get more
;; headings that mean the same thing as you keep raising or lowering
;; (for example, after a single @raisesections, both @chapter and
;; @section produce chapter headings).

(defun texihtml-raise-lower-sections ()
  "Raise or lower the hierarchical level of chapters, sections, etc. 

This function acts according to `@raisesections' and `@lowersections'
commands in the Texihtml file.

For example, an `@lowersections' command is useful if you wish to
include what is written as an outer or standalone Texihtml file in
another Texihtml file as an inner, included file.  The `@lowersections'
command changes chapters to sections, sections to subsections and so
on.

@raisesections changes @subsection to @section,
                       @section    to @chapter,
                       @heading    to @chapheading,
                       etc.

@lowersections changes @chapter    to @section,
                       @subsection to @subsubsection,
                       @heading    to @subheading,
                       etc.

An `@raisesections' or `@lowersections' command changes only those
structuring commands that follow the `@raisesections' or
`@lowersections' command.

An `@lowersections' command cancels an `@raisesections' command, and
vice versa.

Repeated use of the commands continue to raise or lower the hierarchical
level a step at a time.

An attempt to raise above `chapters' reproduces chapter commands; an
attempt to lower below subsubsections reproduces subsubsection
commands."
  
  ;; `texinfo-section-types-regexp' is defined in `texnfo-upd.el';
  ;; it is a regexp matching chapter, section, other headings
  ;; (but not the top node).

  (let (type (level 0))
    (while 
        (re-search-forward
         (concat
          "\\(\\(^@\\(raise\\|lower\\)sections\\)\\|\\("
          texinfo-section-types-regexp
          "\\)\\)")
         nil t)
      (beginning-of-line)
      (save-excursion (setq type (read (current-buffer))))
      (cond 
       
       ;; 1. Increment level
       ((eq type '@raisesections)
        (setq level (1+ level))
        (delete-region
         (point) (save-excursion (forward-line 1) (point))))
       
       ;; 2. Decrement level
       ((eq type '@lowersections)
        (setq level (1- level))
        (delete-region
         (point) (save-excursion (forward-line 1) (point))))
       
       ;; Now handle structuring commands
       ((cond
         
         ;; 3. Raise level when positive
         ((> level 0)
          (let ((count level)
                (new-level type))
            (while (> count 0)
              (setq new-level
                    (cdr (assq new-level texihtml-raisesections-alist)))
              (setq count (1- count)))
            (kill-word 1)
            (insert (symbol-name new-level))))
         
         ;; 4. Do nothing except move point when level is zero
         ((= level 0) (forward-line 1))
         
         ;; 5. Lower level when positive
         ((< level 0)
          (let ((count level)
                (new-level type))
            (while (< count 0)
              (setq new-level
                    (cdr (assq new-level texihtml-lowersections-alist)))
              (setq count (1+ count)))
            (kill-word 1)
            (insert (symbol-name new-level))))))))))

(defvar texihtml-raisesections-alist
  '((@chapter . @chapter)             ; Cannot go higher
    (@unnumbered . @unnumbered)

    (@majorheading . @majorheading)
    (@chapheading . @chapheading)
    (@appendix . @appendix)
    
    (@section . @chapter)
    (@unnumberedsec . @unnumbered)
    (@heading . @chapheading)
    (@appendixsec . @appendix)
    
    (@subsection . @section)
    (@unnumberedsubsec . @unnumberedsec)
    (@subheading . @heading)
    (@appendixsubsec . @appendixsec)
    
    (@subsubsection . @subsection)
    (@unnumberedsubsubsec . @unnumberedsubsec)
    (@subsubheading . @subheading)
    (@appendixsubsubsec . @appendixsubsec))
  "*An alist of next higher levels for chapters, sections. etc.
For example, section to chapter, subsection to section.
Used by `texihtml-raise-lower-sections'.
The keys specify types of section; the values correspond to the next
higher types.")

(defvar texihtml-lowersections-alist
  '((@chapter . @section)  
    (@unnumbered . @unnumberedsec)
    (@majorheading . @heading)
    (@chapheading . @heading)
    (@appendix . @appendixsec)
    
    (@section . @subsection)
    (@unnumberedsec . @unnumberedsubsec)
    (@heading . @subheading)
    (@appendixsec . @appendixsubsec)
    
    (@subsection . @subsubsection)
    (@unnumberedsubsec . @unnumberedsubsubsec)
    (@subheading . @subsubheading)
    (@appendixsubsec . @appendixsubsubsec)
    
    (@subsubsection . @subsubsection) ; Cannot go lower.
    (@unnumberedsubsubsec . @unnumberedsubsubsec)
    (@subsubheading . @subsubheading)
    (@appendixsubsubsec . @appendixsubsubsec))
  "*An alist of next lower levels for chapters, sections. etc.
For example, chapter to section, section to subsection.
Used by `texihtml-raise-lower-sections'.
The keys specify types of section; the values correspond to the next
lower types.")


;;; Perform those texihtml-to-info conversions that apply to the whole input
;;; uniformly.

(defun texihtml-format-scan ()
  (texihtml-format-convert (point-min) (point-max))
  ;; Scan for @-commands.
  (goto-char (point-min))
  (while (search-forward "@" nil 'move)
    (if (looking-at "[@{}'` *]")
        ;; Handle a few special @-followed-by-one-char commands.
        (if (= (following-char) ?*)
            (progn
              ;; remove command
              (delete-region (1- (point)) (1+ (point)))
              ;; insert <P>.  not sure this is the right representation.
	      ;; also insert newline for readability.
	      (insert "<P>\n"))
	  ;; The other characters are simply quoted.  Delete the @.
	  (delete-char -1)
          (forward-char 1))
      ;; @ is followed by a command-word; find the end of the word.
      (setq texihtml-command-start (1- (point)))
      (if (= (char-syntax (following-char)) ?w)
          (forward-word 1)
        (forward-char 1))
      (setq texihtml-command-end (point))
      ;; Call the handler for this command.
      (setq texihtml-command-name
            (intern (buffer-substring
		     (1+ texihtml-command-start) texihtml-command-end)))
      (let ((enclosure-type
             (assoc
              (symbol-name texihtml-command-name)
              texihtml-enclosure-list)))
        (if enclosure-type
            (progn
              (insert
               (car (car (cdr enclosure-type))) 
               (texihtml-parse-arg-discard)
               (car (cdr (car (cdr enclosure-type)))))
              (goto-char texihtml-command-start))
          (let ((cmd (get texihtml-command-name 'texihtml-format)))
            (if cmd (funcall cmd) (texihtml-unsupported)))))))
  
  (if texihtml-last-node
      (write-region (point-min) (point)
		    (concat outdir "/" (texihtml-url-quote texihtml-last-node)
			    ".html")))
  (cond (texihtml-stack
         (goto-char (nth 2 (car texihtml-stack)))
         (error "Unterminated @%s" (car (car texihtml-stack))))))

(put 'begin 'texihtml-format 'texihtml-format-begin)
(defun texihtml-format-begin ()
  (texihtml-format-begin-end 'texihtml-format))

(put 'end 'texihtml-format 'texihtml-format-end)
(defun texihtml-format-end ()
  (texihtml-format-begin-end 'texihtml-end))

(defun texihtml-format-begin-end (prop)
  (setq texihtml-command-name (intern (texihtml-parse-line-arg)))
  (setq cmd (get texihtml-command-name prop))
  (if cmd (funcall cmd)
    (texihtml-unsupported)))

;;; Parsing functions

(defun texihtml-parse-line-arg ()
  (goto-char texihtml-command-end)
  (let ((start (point)))
    (cond ((looking-at " ")
           (skip-chars-forward " ")
           (setq start (point))
           (end-of-line)
           (skip-chars-backward " ")
           (delete-region (point) (progn (end-of-line) (point)))
           (setq texihtml-command-end (1+ (point))))
          ((looking-at "{")
           (setq start (1+ (point)))
           (forward-list 1)
           (setq texihtml-command-end (point))
           (forward-char -1))
          (t
           (error "Invalid texihtml command arg format")))
    (prog1 (buffer-substring start (point))
           (if (eolp) (forward-char 1)))))

(defun texihtml-parse-expanded-arg ()
  (goto-char texihtml-command-end)
  (let ((start (point))
        marker)
    (cond ((looking-at " ")
           (skip-chars-forward " ")
           (setq start (point))
           (end-of-line)
           (setq texihtml-command-end (1+ (point))))
          ((looking-at "{")
           (setq start (1+ (point)))
           (forward-list 1)
           (setq texihtml-command-end (point))
           (forward-char -1))
          (t
           (error "Invalid texihtml command arg format")))
    (setq marker (move-marker (make-marker) texihtml-command-end))
    (texihtml-format-expand-region start (point))
    (setq texihtml-command-end (marker-position marker))
    (move-marker marker nil)
    (prog1 (buffer-substring start (point))
           (if (eolp) (forward-char 1)))))

(defun texihtml-format-expand-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (let (texihtml-command-start
          texihtml-command-end
          texihtml-command-name
          texihtml-stack)
      (texihtml-format-scan))
    (goto-char (point-max))))

(defun texihtml-parse-arg-discard ()
  (prog1 (texihtml-parse-line-arg)
         (texihtml-discard-command)))

(defun texihtml-discard-command ()
  (delete-region texihtml-command-start texihtml-command-end))

(defun texihtml-optional-braces-discard ()
  "Discard braces following command, if any."
  (goto-char texihtml-command-end)
  (let ((start (point)))
    (cond ((looking-at "[ \t]*\n"))     ; do nothing
          ((looking-at "{")             ; remove braces, if any
           (forward-list 1)
           (setq texihtml-command-end (point)))
          (t
           (error
            "Invalid `texihtml-optional-braces-discard' format \(need braces?\)")))
    (delete-region texihtml-command-start texihtml-command-end)))

(defun texihtml-format-parse-line-args ()
  (let ((start (1- (point)))
        next beg end
        args)
    (skip-chars-forward " ")
    (while (not (eolp))
      (setq beg (point))
      (re-search-forward "[\n,]")
      (setq next (point))
      (if (bolp) (setq next (1- next)))
      (forward-char -1)
      (skip-chars-backward " ")
      (setq end (point))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
                       args))
      (goto-char next)
      (skip-chars-forward " "))
    (if (eolp) (forward-char 1))
    (setq texihtml-command-end (point))
    (nreverse args)))

(defun texihtml-format-parse-args ()
  (let ((start (1- (point)))
        next beg end
        args)
    (search-forward "{")
    (save-excursion
      (texihtml-format-expand-region 
       (point)
       (save-excursion (up-list 1) (1- (point)))))
    ;; The following does not handle cross references of the form:
    ;; `@xref{bullet, , @code{@@bullet}@{@}}.' because the
    ;; re-search-forward finds the first right brace after the second
    ;; comma. 
    (while (/= (preceding-char) ?\})
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (re-search-forward "[},]")
      (setq next (point))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (cond ((< beg end)
             (goto-char beg)
             (while (search-forward "\n" end t)
               (replace-match " "))))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
                       args))
      (goto-char next))
    (if (eolp) (forward-char 1))
    (setq texihtml-command-end (point))
    (nreverse args)))

(defun texihtml-format-parse-defun-args ()
  (goto-char texihtml-command-end)
  (let ((start (point)))
    (end-of-line)
    (setq texihtml-command-end (1+ (point)))
    (let ((marker (move-marker (make-marker) texihtml-command-end)))
      (texihtml-format-expand-region start (point))
      (setq texihtml-command-end (marker-position marker))
      (move-marker marker nil))
    (goto-char start)
    (let ((args '())
          beg end)
      (skip-chars-forward " ")
      (while (not (eolp))
        (cond ((looking-at "{")
               (setq beg (1+ (point)))
               (forward-list 1)
               (setq end (1- (point))))
              (t
               (setq beg (point))
               (re-search-forward "[\n ]")
               (forward-char -1)
               (setq end (point))))
        (setq args (cons (buffer-substring beg end) args))
        (skip-chars-forward " "))
      (forward-char 1)
      (nreverse args))))

(defun texihtml-discard-line ()
  (goto-char texihtml-command-end)
  (skip-chars-forward " \t")
  (or (eolp)
      (error "Extraneous text at end of command line."))
  (goto-char texihtml-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line."))
  (delete-region (point) (progn (forward-line 1) (point))))

(defun texihtml-discard-line-with-args ()
  (goto-char texihtml-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))


;;; @setfilename

;; Only `texihtml-format-buffer' handles @setfilename with this
;; definition; `texihtml-format-region' handles @setfilename, if any,
;; specially. 
(put 'setfilename 'texihtml-format 'texihtml-format-setfilename)
(defun texihtml-format-setfilename ()
  (let ((arg (texihtml-parse-arg-discard)))
    (message "Formatting HTML file: %s" arg)
    (setq texihtml-format-filename
          (file-name-nondirectory (expand-file-name arg)))
    (insert "HTML file: "
            texihtml-format-filename ",    -*-Text-*-\n"
            ;; Date string removed so that regression testing is easier.
            ;; "produced on "
            ;; (substring (current-time-string) 8 10) " "
            ;; (substring (current-time-string) 4 7) " "
            ;; (substring (current-time-string) -4)  " "
            "produced by `texihtml-format-buffer'\n"
            "from file"
            (if (buffer-file-name input-buffer)
                (concat " `"
                        (file-name-sans-versions
                         (file-name-nondirectory
                          (buffer-file-name input-buffer)))
                        "'")
              (concat "buffer `" (buffer-name input-buffer) "'"))
            "\nusing `texinfmt.el' version "
            texihtml-version
            ".\n\n")))

;;; @node, @menu

(put 'node 'texihtml-format 'texihtml-format-node)
(put 'nwnode 'texihtml-format 'texihtml-format-node)
(defun texihtml-format-node ()
  (let* ((args (texihtml-format-parse-line-args))
         (name (nth 0 args))
         (next (nth 1 args))
         (prev (nth 2 args))
         (up (nth 3 args)))
    (texihtml-discard-command)
    (cond (texihtml-last-node
	   (write-region (point-min) (point)
			 (concat outdir "/"
				 (texihtml-url-quote texihtml-last-node)
				 ".html"))
	   (delete-region (point-min) (point)))
	  (t
	   (delete-region (point-min) (point))))
    (setq texihtml-last-node name)
    (let ((tem (downcase name)))
      (if (assoc tem texihtml-node-names)
          (error "Duplicate node name: %s" name)
        (setq texihtml-node-names (cons (list tem) texihtml-node-names))))
    (setq texihtml-footnote-number 0)
    (insert "<TITLE>" texihtml-format-filename ": " name "</TITLE>\n")
    (if next
	(insert "Next: " (texihtml-make-local-anchor next) "<P>\n"))
    (if prev
        (insert "Prev: " (texihtml-make-local-anchor prev) "<P>\n"))))

(defun texihtml-make-local-anchor (name &optional text)
  (texihtml-make-anchor (texihtml-url-quote (concat name ".html")) text))

(defun texihtml-make-anchor (url &optional text)
  (concat "<A HREF=\"" url "\">" (or text name) "</A>"))

(defun texihtml-url-quote (name)
  "quick and wrong, for now"
  (let ((result "") (pos 0) match)
    (while (setq match (string-match " " name pos))
      (setq result (concat result (substring name pos match) "_"))
      (setq pos (1+ match)))
    (concat result (substring name pos))))

(put 'menu 'texihtml-format 'texihtml-format-menu)
(defun texihtml-format-menu ()
  (texihtml-discard-line)
  (insert "Menu:\n<DL COMPACT>\n")
  (let ((menu-start (point)))
    (save-excursion
      (if (not (re-search-forward "^@end *menu"))
	  (error "Unmatched @menu")
	(delete-region (save-excursion (beginning-of-line) (point)) (point))
	(insert "</DL>\n")
	(while (re-search-backward "^\\* *" menu-start 'move)
	  (replace-match "")
	  (cond ((looking-at "^\\([^:]*\\):: *")
		 (let ((md (match-data))
		       (insertion (concat "<DT>"
					  (texihtml-make-local-anchor
					   (buffer-substring
					    (match-beginning 1)
					    (match-end 1)))
					  "<DD>")))
		   (store-match-data md)
		   (replace-match insertion t t)))
		((looking-at "^\\([^:]*\\): *\\([^.]*\\)\\. *")
		 (let ((noderef (buffer-substring (match-beginning 2)
						  (match-end 2)))
		       (name (buffer-substring (match-beginning 1)
					       (match-end 1))))
		   (replace-match (concat "<DT>"
					  (texihtml-make-anchor
					   (texihtml-noderef-to-url
					    noderef)
					   name)
					  "<DD>")
				  t t)))))))))

(defun texihtml-noderef-to-url (noderef)
  (if (string-match "^(\\([^()]*\\)\\(.*\\)$" noderef)
      (concat "../"
	      (texihtml-url-quote (substring noderef
					     (match-beginning 1)
					     (match-end 1)))
	      "/"
	      (texihtml-utl-quote (substring noderef
					     (match-beginning 2)
					     (match-end 2)))
	      ".html")
    (concat (texihtml-url-quote noderef) ".html")))
     
(put 'menu 'texihtml-end 'texihtml-discard-command)


;;; Cross references

; @xref {NODE, FNAME, NAME, FILE, DOCUMENT}
; -> *Note FNAME: (FILE)NODE
;   If FILE is missing,
;    *Note FNAME: NODE
;   If FNAME is empty and NAME is present
;    *Note NAME: Node
;   If both NAME and FNAME are missing
;    *Note NODE::
;   texihtml ignores the DOCUMENT argument.
; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;   If FILE is specified, (FILE)NODE is used for xrefs.
;   If fifth argument DOCUMENT is specified, produces
;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;    of DOCUMENT

; @ref             a reference that does not put `See' or `see' in
;                  the hardcopy and is the same as @xref in Info
(put 'ref 'texihtml-format 'texihtml-format-ref)
(defun texihtml-format-ref ()
  (let ((args (texihtml-format-parse-args)))
    (texihtml-discard-command)
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (insert (cond ((null (or fname (nth 3 args)))
		     (texihtml-make-local-anchor (car args)))
		    ((nth 3 args)
		     (texihtml-make-anchor
		      (concat "../" (texihtml-url-quote (nth 3 args))
			      "-html/" (texihtml-url-quote (car args)) ".html")
		      (or fname (car args))))
		    (t
		     (texihtml-make-local-anchor (car args)
						 (or fname (car args)))))))))

(put 'xref 'texihtml-format 'texihtml-format-xref)
(defun texihtml-format-xref ()
  (let ((start (point-marker)))
    (texihtml-format-ref)
    (save-excursion
      (goto-char start)
      (insert "See "))))

(put 'pxref 'texihtml-format 'texihtml-format-pxref)
(defun texihtml-format-pxref ()
  (let ((start (point-marker)))
    (texihtml-format-ref)
    (save-excursion
      (goto-char start)
      (insert "see "))))

;@inforef{NODE, FNAME, FILE}
;Like @xref{NODE, FNAME,,FILE} in texinfo.
;In Tex, generates "See Info file FILE, node NODE"
(put 'inforef 'texihtml-format 'texihtml-format-ref)
(defun texihtml-format-inforef ()
  (let ((args (texihtml-format-parse-args)))
    (texihtml-discard-command)
    (if (nth 1 args)
        (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))
      (insert "*Note " "(" (nth 2 args) ")" (car args) "::"))))


;;; Section headings

(put 'majorheading 'texihtml-format 'texihtml-format-chapter)
(put 'chapheading 'texihtml-format 'texihtml-format-chapter)
(put 'ichapter 'texihtml-format 'texihtml-format-chapter)
(put 'chapter 'texihtml-format 'texihtml-format-chapter)
(put 'iappendix 'texihtml-format 'texihtml-format-chapter)
(put 'appendix 'texihtml-format 'texihtml-format-chapter)
(put 'iunnumbered 'texihtml-format 'texihtml-format-chapter)
(put 'top 'texihtml-format 'texihtml-format-chapter)
(put 'unnumbered 'texihtml-format 'texihtml-format-chapter)
(defun texihtml-format-chapter ()
  (texihtml-format-chapter-1 ?1))

(put 'heading 'texihtml-format 'texihtml-format-section)
(put 'isection 'texihtml-format 'texihtml-format-section)
(put 'section 'texihtml-format 'texihtml-format-section)
(put 'iappendixsection 'texihtml-format 'texihtml-format-section)
(put 'appendixsection 'texihtml-format 'texihtml-format-section)
(put 'iappendixsec 'texihtml-format 'texihtml-format-section)
(put 'appendixsec 'texihtml-format 'texihtml-format-section)
(put 'iunnumberedsec 'texihtml-format 'texihtml-format-section)
(put 'unnumberedsec 'texihtml-format 'texihtml-format-section)
(defun texihtml-format-section ()
  (texihtml-format-chapter-1 ?2))

(put 'subheading 'texihtml-format 'texihtml-format-subsection)
(put 'isubsection 'texihtml-format 'texihtml-format-subsection)
(put 'subsection 'texihtml-format 'texihtml-format-subsection)
(put 'iappendixsubsec 'texihtml-format 'texihtml-format-subsection)
(put 'appendixsubsec 'texihtml-format 'texihtml-format-subsection)
(put 'iunnumberedsubsec 'texihtml-format 'texihtml-format-subsection)
(put 'unnumberedsubsec 'texihtml-format 'texihtml-format-subsection)
(defun texihtml-format-subsection ()
  (texihtml-format-chapter-1 ?3))

(put 'subsubheading 'texihtml-format 'texihtml-format-subsubsection)
(put 'isubsubsection 'texihtml-format 'texihtml-format-subsubsection)
(put 'subsubsection 'texihtml-format 'texihtml-format-subsubsection)
(put 'iappendixsubsubsec 'texihtml-format 'texihtml-format-subsubsection)
(put 'appendixsubsubsec 'texihtml-format 'texihtml-format-subsubsection)
(put 'iunnumberedsubsubsec 'texihtml-format 'texihtml-format-subsubsection)
(put 'unnumberedsubsubsec 'texihtml-format 'texihtml-format-subsubsection)
(defun texihtml-format-subsubsection ()
  (texihtml-format-chapter-1 ?4))

(defun texihtml-format-chapter-1 (levelchar)
  (let ((arg (texihtml-parse-arg-discard)))
    (message "Formatting: %s ... " arg)   ; So we can see where we are.
    (insert ?\n "<H" levelchar ">")
    (let ((start (point)))
      (insert arg "</H" levelchar ">\n")
      (goto-char (point)))))


;;; Space controling commands:  @. and @:   
(put '\. 'texihtml-format 'texihtml-format-\.)
(defun texihtml-format-\. ()
  (texihtml-discard-command)
  (insert "."))

(put '\: 'texihtml-format 'texihtml-format-\:)
(defun texihtml-format-\: ()
  (texihtml-discard-command))


;;; @center, @sp, and @br

(put 'center 'texihtml-format 'texihtml-format-center)
(defun texihtml-format-center ()
  (message "HTML has no @center equivalent...")
  (let ((arg (texihtml-parse-expanded-arg)))
    (texihtml-discard-command)
    (insert "<P>\n" arg "<P>")))

;;;  (let ((arg (texihtml-parse-expanded-arg)))
;;;    (texihtml-discard-command)
;;;    (insert arg)
;;;    (insert ?\n)
;;;    (save-restriction
;;;      (goto-char (1- (point)))
;;;      (let ((indent-tabs-mode nil))
;;;        (center-line)))))

(put 'sp 'texihtml-format 'texihtml-format-sp)
(defun texihtml-format-sp ()
  (let* ((arg (texihtml-parse-arg-discard))
         (num (read arg)))
    (while (> num 0)
      (insert "<P>\n")
      (setq num (1- num)))))

(put 'br 'texihtml-format 'texihtml-format-paragraph-break)
(defun texihtml-format-paragraph-break ()
  "Force a paragraph break.
If used within a line, follow `@br' with braces."
  (texihtml-optional-braces-discard)
  (insert "<P>\n"))


;;; @footnote  and  @footnotestyle

; In Texihtml, footnotes are created with the `@footnote' command.
; This command is followed immediately by a left brace, then by the text of
; the footnote, and then by a terminating right brace.  The
; template for a footnote is:
; 
;      @footnote{TEXT}
;
; Info has two footnote styles:
; 
;    * In the End of node style, all the footnotes for a single node
;      are placed at the end of that node.  The footnotes are
;      separated from the rest of the node by a line of dashes with
;      the word `Footnotes' within it.
; 
;    * In the Separate node style, all the footnotes for a single node
;      are placed in an automatically constructed node of their own.

; Footnote style is specified by the @footnotestyle command, either
;    @footnotestyle separate
; or
;    @footnotestyle end
; 
; The default is  separate

(defvar texinfo-footnote-style "separate" 
  "Footnote style, either separate or end.")

(put 'footnotestyle 'texihtml-format 'texihtml-footnotestyle)
(defun texihtml-footnotestyle ()
  "Specify whether footnotes are at end of node or in separate nodes.
Argument is either end or separate."
  (setq texinfo-footnote-style (texihtml-parse-arg-discard)))

(defvar texihtml-footnote-number)

(put 'footnote 'texihtml-format 'texihtml-format-footnote)
(defun texihtml-format-footnote ()
  "Format a footnote in either end of node or separate node style.
The   texinfo-footnote-style  variable controls which style is used."
  (setq texihtml-footnote-number (1+ texihtml-footnote-number))
  (cond ((string= texinfo-footnote-style "end")
         (texihtml-format-end-node))
        ((string= texinfo-footnote-style "separate")
         (texihtml-format-separate-node))))

(defun texihtml-format-separate-node ()
  "Format footnote in Separate node style, with notes in own node.
The node is constructed automatically."
  (let* (start
         (arg (texihtml-parse-line-arg)))
    (texihtml-discard-command)  ; remove or insert whitespace, as needed
    (delete-region (save-excursion (skip-chars-backward " \t\n") (point))
                   (point))
    (insert " (" (texihtml-make-anchor
		  (texihtml-url-quote (concat texihtml-last-node
					      "-Footnotes.html"))
		  (concat "footnote "
			  (prin1-to-string texihtml-footnote-number))) ")")
    (save-excursion
      (if (and (re-search-forward "^@node " nil 'move)
	       (looking-at (concat (regexp-quote texihtml-last-node)
				   "-Footnotes,")))
	  (progn
	    (re-search-forward "^@node " nil 'move)
	    (beginning-of-line))
	(beginning-of-line)
	(insert "^@node " texihtml-last-node
		"-Footnotes, , " texihtml-last-node "\n\n"))
      (insert (format "(%d)  %s\n\n" texihtml-footnote-number arg)))))

(defun texihtml-format-end-node ()
  "Format footnote in the End of node style, with notes at end of node."
  (let (start
         (arg (texihtml-parse-line-arg)))
    (texihtml-discard-command)  ; remove or insert whitespace, as needed
    (delete-region (save-excursion (skip-chars-backward " \t\n") (point))
                   (point))
    (insert " (" (texihtml-make-anchor
		  (texihtml-url-quote (concat texihtml-last-node
					      "-Footnotes.html"))
		  (concat "footnote "
			  (prin1-to-string texihtml-footnote-number))) ")")
    (save-excursion
      (re-search-forward "^@node " nil 'move)
      (beginning-of-line)
      (if (eq texihtml-footnote-number 1)
	  (insert "Footnotes:\n\n"))
      (insert (format "(%d)  %s\n\n" texihtml-footnote-number arg)))))


;;; @itemize, @enumerate, and similar commands

;; @itemize pushes (itemize "COMMANDS" STARTPOS) on texihtml-stack.
;; "COMMANDS" aren't used in HTML conversion, though.  And I'm not sure
;; whether or not lists can be nested.
;; @enumerate pushes (enumerate 0 STARTPOS).
;; @item dispatches to the texihtml-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the @end is reached.

(defvar texihtml-stack-depth 0
  "Count of number of unpopped texihtml-push-stack calls.
Used by @refill indenting command to avoid indenting within lists, etc.")

(defun texihtml-push-stack (check arg)
  (setq texihtml-stack-depth (1+ texihtml-stack-depth))
  (setq texihtml-stack
        (cons (list check arg texihtml-command-start)
              texihtml-stack)))

(defun texihtml-pop-stack (check)
  (setq texihtml-stack-depth (1- texihtml-stack-depth))
  (if (null texihtml-stack)
      (error "Unmatched @end %s" check))
  (if (not (eq (car (car texihtml-stack)) check))
      (error "@end %s matches @%s"
             check (car (car texihtml-stack))))
  (prog1 (cdr (car texihtml-stack))
         (setq texihtml-stack (cdr texihtml-stack))))

(put 'itemize 'texihtml-format 'texihtml-itemize)
(defun texihtml-itemize ()
  (texihtml-push-stack
   'itemize
   (progn (skip-chars-forward " \t")
          (if (eolp)
              "@bullet"
            (texihtml-parse-line-arg))))
  (texihtml-discard-line-with-args)
  (insert "<UL>\n"))

(put 'itemize 'texihtml-end 'texihtml-end-itemize)
(defun texihtml-end-itemize ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'itemize)
  (insert "</UL>\n"))


(put 'enumerate 'texihtml-format 'texihtml-enumerate)
(defun texihtml-enumerate ()
  (texihtml-push-stack
   'enumerate 
   (progn (skip-chars-forward " \t")
          (if (eolp)
              1
            (read (current-buffer)))))
  (if (and (symbolp (car (cdr (car texihtml-stack))))
           (> 1 (length (symbol-name (car (cdr (car texihtml-stack)))))))
      (error
       "@enumerate: Use a number or letter, eg: 1, A, a, 3, B, or d." ))
  (texihtml-discard-line-with-args)
  (insert "<OL>\n"))

(put 'enumerate 'texihtml-end 'texihtml-end-enumerate)
(defun texihtml-end-enumerate ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'enumerate)
  (insert "</OL>\n"))

;; @alphaenumerate never became a standard part of Texihtml
(put 'alphaenumerate 'texihtml-format 'texihtml-alphaenumerate)
(defun texihtml-alphaenumerate ()
  (texihtml-push-stack 'alphaenumerate (1- ?a))
  (texihtml-discard-line)
  (insert "<OL>\n"))

(put 'alphaenumerate 'texihtml-end 'texihtml-end-alphaenumerate)
(defun texihtml-end-alphaenumerate ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'alphaenumerate)
  (insert "</OL>\n"))

;; @capsenumerate never became a standard part of Texihtml
(put 'capsenumerate 'texihtml-format 'texihtml-capsenumerate)
(defun texihtml-capsenumerate ()
  (texihtml-push-stack 'capsenumerate (1- ?A))
  (texihtml-discard-line)
  (insert "<OL>\n"))

(put 'capsenumerate 'texihtml-end 'texihtml-end-capsenumerate)
(defun texihtml-end-capsenumerate ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'capsenumerate)
  (insert "</OL>\n"))

(put 'item 'texihtml-format 'texihtml-item)
(put 'itemx 'texihtml-format 'texihtml-item)
(defun texihtml-item ()
  (funcall (get (car (car texihtml-stack)) 'texihtml-item)))

(put 'itemize 'texihtml-item 'texihtml-itemize-item)
(defun texihtml-itemize-item ()
  ;; (texihtml-discard-line)   ; Did not handle text on same line as @item.
  (delete-region (1+ (point)) (save-excursion (beginning-of-line) (point)))
  (insert "<LI>"))

(put 'enumerate 'texihtml-item 'texihtml-itemize-item)
(put 'alphaenumerate 'texihtml-item 'texihtml-itemize-item)
(put 'capsenumerate 'texihtml-item 'texihtml-itemize-item)


;;; @table

; The `@table' command produces two-column tables.

(put 'table 'texihtml-format 'texihtml-table)
(defun texihtml-table ()
  (texihtml-push-stack 
   'table 
   (progn (skip-chars-forward " \t")
          (if (eolp)
              "@asis"
            (texihtml-parse-line-arg))))
  (texihtml-discard-line-with-args)
  (insert "<DL>\n"))

(put 'table 'texihtml-item 'texihtml-table-item)
(defun texihtml-table-item ()
  (let ((arg (texihtml-parse-arg-discard))
        (itemfont (car (cdr (car texihtml-stack)))))
    (insert "<DT>" itemfont ?\{ arg "}<DD>"))
  (forward-line -2))

(put 'table 'texihtml-end 'texihtml-end-table)
(defun texihtml-end-table ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'table)
  (insert "</DL>\n"))

;; @description appears to be an undocumented variant on @table that
;; does not require an arg.  It fails in texinfo.tex 2.58 and is not
;; part of makeinfo.c   The command appears to be a relic of the past.
(put 'description 'texihtml-end 'texihtml-end-table)
(put 'description 'texihtml-format 'texihtml-description)
(defun texihtml-description ()
  (texihtml-push-stack 'table "@asis")
  (texihtml-discard-line))


;;; @ftable, @vtable

; The `@ftable' and `@vtable' commands are like the `@table' command
; but they also insert each entry in the first column of the table
; into the function or variable index.

;; Handle the @ftable and @vtable commands:

(put 'ftable 'texihtml-format 'texihtml-ftable)
(put 'vtable 'texihtml-format 'texihtml-vtable)

(defun texihtml-ftable () (texihtml-indextable 'ftable))
(defun texihtml-vtable () (texihtml-indextable 'vtable))

(defun texihtml-indextable (table-type)
  (texihtml-push-stack table-type (texihtml-parse-arg-discard))
  (insert "<DL>\n"))

;; Handle the @item commands within ftable and vtable:

(put 'ftable 'texihtml-item 'texihtml-ftable-item)
(put 'vtable 'texihtml-item 'texihtml-vtable-item)

(defun texihtml-ftable-item () (texihtml-indextable-item 'texihtml-findex))
(defun texihtml-vtable-item () (texihtml-indextable-item 'texihtml-vindex))

(defun texihtml-indextable-item (index-type)
  (let ((item (texihtml-parse-arg-discard))
        (itemfont (car (cdr (car texihtml-stack))))
        (indexvar index-type))
    (insert "<DT>" itemfont ?\{ arg "}<DD>")
    (set indexvar
         (cons
          (list item texihtml-last-node)
          (symbol-value indexvar)))
    (forward-line -2)))

;; Handle @end ftable, @end vtable

(put 'ftable 'texihtml-end 'texihtml-end-ftable)
(put 'vtable 'texihtml-end 'texihtml-end-vtable)

(defun texihtml-end-ftable () (texihtml-end-indextable 'ftable))
(defun texihtml-end-vtable () (texihtml-end-indextable 'vtable))

(defun texihtml-end-indextable (table-type)
  (texihtml-discard-command)
  (texihtml-pop-stack table-type)
  (insert "</DL>\n"))


;;; @ifinfo,  @iftex, @tex

(put 'ifinfo 'texihtml-format 'texihtml-discard-line)
(put 'ifinfo 'texihtml-end 'texihtml-discard-command)
;;; I'm a little dubious, but for the moment it seems better to include
;;; the @ifinfo stuff than not.

(put 'iftex 'texihtml-format 'texihtml-format-iftex)
(defun texihtml-format-iftex ()
  (delete-region texihtml-command-start
                 (progn (re-search-forward "@end iftex[ \t]*\n")
                        (point))))

(put 'tex 'texihtml-format 'texihtml-format-tex)
(defun texihtml-format-tex ()
  (delete-region texihtml-command-start
                 (progn (re-search-forward "@end tex[ \t]*\n")
                        (point))))


;;; @titlepage

(put 'titlepage 'texihtml-format 'texihtml-format-titlepage)
(defun texihtml-format-titlepage ()
  (delete-region texihtml-command-start
                 (progn (re-search-forward "@end titlepage[ \t]*\n")
                        (point))))

(put 'endtitlepage 'texihtml-format 'texihtml-discard-line)

; @titlespec         an alternative titling command; ignored by Info

(put 'titlespec 'texihtml-format 'texihtml-format-titlespec)
(defun texihtml-format-titlespec ()
  (delete-region texihtml-command-start
                 (progn (re-search-forward "@end titlespec[ \t]*\n")
                        (point))))

(put 'endtitlespec 'texihtml-format 'texihtml-discard-line)


;;; @today

(put 'today 'texihtml-format 'texihtml-format-today)

; Produces Day Month Year style of output.  eg `1 Jan 1900'
; The `@today{}' command requires a pair of braces, like `@dots{}'.
(defun texihtml-format-today ()
  (texihtml-parse-arg-discard)
  (insert (format "%s %s %s"
          (substring (current-time-string) 8 10)
          (substring (current-time-string) 4 7)
          (substring (current-time-string) -4))))


;;; @ignore

(put 'ignore 'texihtml-format 'texihtml-format-ignore)
(defun texihtml-format-ignore ()
  (delete-region texihtml-command-start
                 (progn (re-search-forward "@end ignore[ \t]*\n")
                        (point))))

(put 'endignore 'texihtml-format 'texihtml-discard-line)


;;; Define the Info enclosure command: @definfoenclose

; A `@definfoenclose' command may be used to define a highlighting
; command for Info, but not for TeX.  A command defined using
; `@definfoenclose' marks text by enclosing it in strings that precede
; and follow the text.
; 
; Presumably, if you define a command with `@definfoenclose` for Info,
; you will also define the same command in the TeX definitions file,
; `texinfo.tex' in a manner appropriate for typesetting.
; 
; Write a `@definfoenclose' command on a line and follow it with three
; arguments separated by commas (commas are used as separators in an
; `@node' line in the same way).  The first argument to
; `@definfoenclose' is the @-command name \(without the `@'\); the
; second argument is the Info start delimiter string; and the third
; argument is the Info end delimiter string.  The latter two arguments
; enclose the highlighted text in the Info file.  A delimiter string
; may contain spaces.  Neither the start nor end delimiter is
; required.  However, if you do not provide a start delimiter, you
; must follow the command name with two commas in a row; otherwise,
; the Info formatting commands will misinterpret the end delimiter
; string as a start delimiter string.
; 
; An enclosure command defined this way takes one argument in braces.
;
; For example, you can write:
;
;     @ifinfo
;     @definfoenclose phoo, //, \\
;     @end ifinfo
;
; near the beginning of a Texihtml file at the beginning of the lines
; to define `@phoo' as an Info formatting command that inserts `//'
; before and `\\' after the argument to `@phoo'.  You can then write
; `@phoo{bar}' wherever you want `//bar\\' highlighted in Info.
;
; Also, for TeX formatting, you could write 
;
;     @iftex
;     @global@let@phoo=@i
;     @end iftex
;
; to define `@phoo' as a command that causes TeX to typeset
; the argument to `@phoo' in italics.
;
; Note that each definition applies to its own formatter: one for TeX,
; the other for texihtml-format-buffer or texihtml-format-region.
;
; Here is another example: write
;
;     @definfoenclose headword, , :
;
; near the beginning of the file, to define `@headword' as an Info
; formatting command that inserts nothing before and a colon after the
; argument to `@headword'.

(put 'definfoenclose 'texihtml-format 'texihtml-define-info-enclosure)
(defun texihtml-define-info-enclosure ()
  (let* ((args (texihtml-format-parse-line-args))
         (command-name (nth 0 args))
         (beginning-delimiter (or (nth 1 args) ""))
         (end-delimiter (or (nth 2 args) "")))
    (texihtml-discard-command)
    (setq texihtml-enclosure-list
        (cons
         (list command-name
               (list
                beginning-delimiter
                end-delimiter))
         texihtml-enclosure-list))))


;;; @var, @code and the like

(put 'var 'texihtml-format 'texihtml-format-var)
;  @sc  a small caps font for TeX; formatted as `var' in Info
(put 'sc 'texihtml-format 'texihtml-format-var)
(defun texihtml-format-var ()
  (insert "<VAR>" (texihtml-parse-arg-discard) "</VAR>")
  (goto-char texihtml-command-start))

; various noops, but not for texihtml

(put 'b 'texihtml-format 'texihtml-format-bold)
(defun texihtml-format-bold ()
  (insert "<B>" (texihtml-parse-arg-discard) "</B>")
  (goto-char texihtml-command-start))

(put 'i 'texihtml-format 'texihtml-format-italic)
(defun texihtml-format-italic ()
  (insert "<I>" (texihtml-parse-arg-discard) "</I>")
  (goto-char texihtml-command-start))

(put 'r 'texihtml-format 'texihtml-format-noop) ;; grr, no easy way to do this
(put 't 'texihtml-format 'texihtml-format-typewriter)
(defun texihtml-format-typewriter ()
  (insert "<TT>" (texihtml-parse-arg-discard) "</TT>")
  (goto-char texihtml-command-start))

(put 'w 'texihtml-format 'texihtml-format-noop)
(put 'asis 'texihtml-format 'texihtml-format-noop)
(put 'dmn 'texihtml-format 'texihtml-format-noop)
(put 'key 'texihtml-format 'texihtml-format-key)
(defun texihtml-format-key ()
  (insert "<KEY>" (texihtml-parse-arg-discard) "</KEY>")
  (goto-char texihtml-command-start))

(put 'math 'texihtml-format 'texihtml-format-noop) ;; blink blink, what?
(put 'titlefont 'texihtml-format 'texihtml-format-noop)

(defun texihtml-format-noop ()
  (insert (texihtml-parse-arg-discard))
  (goto-char texihtml-command-start))

(put 'cite 'texihtml-format 'texihtml-format-cite)
(defun texihtml-format-cite ()
  (insert "<CITE>" (texihtml-parse-arg-discard) "</CITE>")
  (goto-char texihtml-command-start))

(put 'code 'texihtml-format 'texihtml-format-code)
(defun texihtml-format-code ()
  (insert "<CODE>" (texihtml-parse-arg-discard) "</CODE>")
  (goto-char texihtml-command-start))

(put 'kbd 'texihtml-format 'texihtml-format-kbd)
(defun texihtml-format-kbd ()
  (insert "<KBD>" (texihtml-parse-arg-discard) "</KBD>")
  (goto-char texihtml-command-start))

(put 'file 'texihtml-format 'texihtml-format-samp)
(put 'samp 'texihtml-format 'texihtml-format-samp)
(defun texihtml-format-samp ()
  (insert "<SAMP>" (texihtml-parse-arg-discard) "</SAMP>")
  (goto-char texihtml-command-start))

(put 'emph 'texihtml-format 'texihtml-format-emph)
(defun texihtml-format-emph ()
  (insert "<EM>" (texihtml-parse-arg-discard) "</EM>")
  (goto-char texihtml-command-start))

(put 'strong 'texihtml-format 'texihtml-format-strong)
(defun texihtml-format-strong ()
  (insert "<STRONG>" (texihtml-parse-arg-discard) "</STRONG>")
  (goto-char texihtml-command-start))

(put 'dfn 'texihtml-format 'texihtml-format-defn)
(put 'defn 'texihtml-format 'texihtml-format-defn) ;; ?
(defun texihtml-format-defn ()
  (insert "<DFN>" (texihtml-parse-arg-discard) "</DFN>")
  (goto-char texihtml-command-start))

(put 'bullet 'texihtml-format 'texihtml-format-bullet)
(defun texihtml-format-bullet ()
  "Insert an asterisk.
If used within a line, follow `@bullet' with braces."
  (texihtml-optional-braces-discard)
  (insert "*"))


;;; @example, @lisp, @quotation, @display, @smalllisp, @smallexample

;;; for now, just do @display like @example.  this is wrong, but i don't
;;; think html has what we want.
(put 'display 'texihtml-format 'texihtml-format-example)
(put 'example 'texihtml-format 'texihtml-format-example)
(put 'lisp 'texihtml-format 'texihtml-format-example)
(put 'smallexample 'texihtml-format 'texihtml-format-example)
(put 'smalllisp 'texihtml-format 'texihtml-format-example)
(defun texihtml-format-example ()
  (texihtml-push-stack 'example nil)
  (texihtml-discard-line)
  (insert "<PRE>\n"))

(put 'example 'texihtml-end 'texihtml-end-example)
(put 'display 'texihtml-end 'texihtml-end-example)
(put 'lisp 'texihtml-end 'texihtml-end-example)
(put 'smallexample 'texihtml-end 'texihtml-end-example)
(put 'smalllisp 'texihtml-end 'texihtml-end-example)
(defun texihtml-end-example ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'example)
  (insert "</PRE\n>"))

(put 'quotation 'texihtml-format 'texihtml-format-quotation)
(put 'quotation 'texihtml-end 'texihtml-end-quotation)
(defun texihtml-format-quotation ()
  (texihtml-push-stack 'quotation nil)
  (texihtml-discard-line)
  (insert "<BLOCKQUOTE>\n"))

(defun texihtml-end-quotation ()
  (texihtml-discard-command)
  (texihtml-pop-stack 'quotation)
  (insert "</BLOCKQUOTE\n>"))

(put 'exdent 'texihtml-format 'texihtml-discard-command)
;;; perhaps should automatically do end-<whatever>, insert, <whatever>, but ick

;;; @cartouche 

; The @cartouche command is a noop in Info; in a printed manual,
; it makes a box with rounded corners.

(put 'cartouche 'texihtml-format 'texihtml-discard-line)
(put 'cartouche 'texihtml-end 'texihtml-discard-command)


;;; @flushleft and @format

; The @flushleft command left justifies every line but leaves the
; right end ragged.  As far as Info is concerned, @flushleft is a
; `do-nothing' command
; HTML, too.

; The @format command is similar to @example except that it does not
; indent; this means that in Info, @format is similar to @flushleft.
; Skip that, it's similar to an example in HTML.

(put 'format 'texihtml-format 'texihtml-format-example)
(put 'flushleft 'texihtml-format 'texihtml-format-flushleft)
(defun texihtml-format-flushleft ()
  (texihtml-discard-line))

(put 'format 'texihtml-end 'texihtml-end-example)
(put 'flushleft 'texihtml-end 'texihtml-end-flushleft)
(defun texihtml-end-flushleft ()
  (texihtml-discard-command))


;;; @flushright

; The @flushright command right justifies every line but leaves the
; left end ragged.  Spaces and tabs at the right ends of lines are
; removed so that visible text lines up on the right side.
; We don't do this in HTML, cope.

(put 'flushright 'texihtml-format 'texihtml-format-flushright)
(defun texihtml-format-flushright ()
  (texihtml-discard-line))

(put 'flushright 'texihtml-end 'texihtml-end-flushright)
(defun texihtml-end-flushright ()
  (texihtml-discard-command))


;;; @ctrl, @TeX, @copyright, @minus, @dots

(put 'ctrl 'texihtml-format 'texihtml-format-noop)
;;; no idea what this is supposed to be...
;;; (defun texihtml-format-ctrl ()
;;;   (let ((str (texihtml-parse-arg-discard)))
;;;     (insert (logand 31 (aref str 0)))))

(put 'TeX 'texihtml-format 'texihtml-format-TeX)
(defun texihtml-format-TeX ()
  (texihtml-parse-arg-discard)
  (insert "TeX"))

(put 'copyright 'texihtml-format 'texihtml-format-copyright)
(defun texihtml-format-copyright ()
  (texihtml-parse-arg-discard)
  (insert "(C)"))

(put 'minus 'texihtml-format 'texihtml-format-minus)
(defun texihtml-format-minus ()
  "Insert a minus sign.
If used within a line, follow `@minus' with braces."
  (texihtml-optional-braces-discard)
  (insert "-"))

(put 'dots 'texihtml-format 'texihtml-format-dots)
(defun texihtml-format-dots ()
  (texihtml-parse-arg-discard)
  (insert "..."))


;;; Refilling and indenting:  @refill, @paragraphindent, @noindent

;;; Indent only those paragraphs that are refilled as a result of an
;;; @refill command.  

;    * If the value is `asis', do not change the existing indentation at
;      the starts of paragraphs.

;    * If the value zero, delete any existing indentation.

;    * If the value is greater than zero, indent each paragraph by that
;      number of spaces.

;;; But do not refill paragraphs with an @refill command that are
;;; preceded by @noindent or are part of a table, list, or deffn.

(put 'paragraphindent 'texihtml-format 'texihtml-paragraphindent)

(defun texihtml-paragraphindent ()
  "Specify the number of spaces for @refill to indent a paragraph.
This is irrelevant or impossible, depending on point of view, in HTML."
  (texihtml-parse-arg-discard))

(put 'refill 'texihtml-format 'texihtml-format-refill)
(defun texihtml-format-refill ()
  "Refill paragraph. Also, indent first line as set by @paragraphindent.
Refill schmefill in HTML."
  (texihtml-discard-command)
  (insert "<P>\n"))

(put 'noindent 'texihtml-format 'texihtml-noindent)
(defun texihtml-noindent ()  
  (texihtml-discard-line))


;;; Index generation

(put 'vindex 'texihtml-format 'texihtml-format-vindex)
(defun texihtml-format-vindex ()
  (texihtml-index 'texihtml-vindex))

(put 'cindex 'texihtml-format 'texihtml-format-cindex)
(defun texihtml-format-cindex ()
  (texihtml-index 'texihtml-cindex))

(put 'findex 'texihtml-format 'texihtml-format-findex)
(defun texihtml-format-findex ()
  (texihtml-index 'texihtml-findex))

(put 'pindex 'texihtml-format 'texihtml-format-pindex)
(defun texihtml-format-pindex ()
  (texihtml-index 'texihtml-pindex))

(put 'tindex 'texihtml-format 'texihtml-format-tindex)
(defun texihtml-format-tindex ()
  (texihtml-index 'texihtml-tindex))

(put 'kindex 'texihtml-format 'texihtml-format-kindex)
(defun texihtml-format-kindex ()
  (texihtml-index 'texihtml-kindex))

(defun texihtml-index (indexvar)
  (let ((arg (texihtml-parse-expanded-arg)))
    (texihtml-discard-command)
    (set indexvar
         (cons (list arg
                     texihtml-last-node
                     ;; Region formatting may not provide last node position.
		     (1+ (count-lines (point-min) (point))))
               (symbol-value indexvar)))))

(defconst texihtml-indexvar-alist
  '(("cp" . texihtml-cindex)
    ("fn" . texihtml-findex)
    ("vr" . texihtml-vindex)
    ("tp" . texihtml-tindex)
    ("pg" . texihtml-pindex)
    ("ky" . texihtml-kindex)))


;;; @defindex   @defcodeindex
(put 'defindex 'texihtml-format 'texihtml-format-defindex)
(put 'defcodeindex 'texihtml-format 'texihtml-format-defindex)

(defun texihtml-format-defindex ()
  (let* ((index-name (texihtml-parse-arg-discard)) ; eg: `aa'
         (indexing-command (intern (concat index-name "index")))
         (index-formatting-command      ; eg: `texihtml-format-aaindex'
          (intern (concat "texihtml-format-" index-name "index")))
         (index-alist-name              ; eg: `texihtml-aaindex'
          (intern (concat "texihtml-" index-name "index"))))

    (set index-alist-name nil)

    (put indexing-command               ; eg, aaindex
         'texihtml-format
         index-formatting-command)      ; eg, texihtml-format-aaindex

    ;; eg: "aa" . texihtml-aaindex
    (or (assoc index-name texihtml-indexvar-alist)
        (setq texihtml-indexvar-alist
              (cons
               (cons index-name
                     index-alist-name)
               texihtml-indexvar-alist)))

    (fset index-formatting-command
          (list 'lambda 'nil
                (list 'texihtml-index 
                      (list 'quote index-alist-name))))))


;;; @synindex   @syncodeindex

(put 'synindex 'texihtml-format 'texihtml-format-synindex)
(put 'syncodeindex 'texihtml-format 'texihtml-format-synindex)

(defun texihtml-format-synindex ()
  (let* ((args (texihtml-parse-arg-discard))
         (second (cdr (read-from-string args)))
         (joiner (symbol-name (car (read-from-string args))))
         (joined (symbol-name (car (read-from-string args second)))))

    (if (assoc joiner texihtml-short-index-cmds-alist)
        (put
          (cdr (assoc joiner texihtml-short-index-cmds-alist))
         'texihtml-format
         (or (cdr (assoc joined texihtml-short-index-format-cmds-alist))
             (intern (concat "texihtml-format-" joined "index"))))
      (put
       (intern (concat joiner "index"))
       'texihtml-format
       (or (cdr(assoc joined texihtml-short-index-format-cmds-alist))
           (intern (concat "texihtml-format-" joined "index")))))))

(defconst texihtml-short-index-cmds-alist
  '(("cp" . cindex)
    ("fn" . findex)
    ("vr" . vindex)
    ("tp" . tindex)
    ("pg" . pindex)
    ("ky" . kindex)))

(defconst texihtml-short-index-format-cmds-alist
  '(("cp" . texihtml-format-cindex)
    ("fn" . texihtml-format-findex)
    ("vr" . texihtml-format-vindex)
    ("tp" . texihtml-format-tindex)
    ("pg" . texihtml-format-pindex)
    ("ky" . texihtml-format-kindex)))


;;; Sort and index (for VMS)

;; Sort an index which is in the current buffer between START and END.
;; Used on VMS, where the `sort' utility is not available.
(defun texihtml-sort-region (start end)
  (require 'sort)
  (save-restriction
    (narrow-to-region start end)
    (sort-subr nil 'forward-line 'end-of-line 'texihtml-sort-startkeyfun)))

;; Subroutine for sorting an index.
;; At start of a line, return a string to sort the line under.
(defun texihtml-sort-startkeyfun ()
  (let ((line
         (buffer-substring (point) (save-excursion (end-of-line) (point)))))
    ;; Canonicalize whitespace and eliminate funny chars.
    (while (string-match "[ \t][ \t]+\\|[^a-z0-9 ]+" line)
      (setq line (concat (substring line 0 (match-beginning 0))
                         " "
                         (substring line (match-end 0) (length line)))))
    line))


;;; @printindex

(put 'printindex 'texihtml-format 'texihtml-format-printindex)

(defun texihtml-format-printindex ()
  (let ((indexelts (symbol-value
                    (cdr (assoc (texihtml-parse-arg-discard)
                                texihtml-indexvar-alist))))
        opoint)
    (insert "\n<DIR>\n\n")
    (setq opoint (point))
    (texihtml-print-index nil indexelts)

    (if (eq system-type 'vax-vms)
        (texihtml-sort-region opoint (point))
      (shell-command-on-region opoint (point) "sort -fd" 1))))

(defun texihtml-print-index (file indexelts)
  (while indexelts
    (if (stringp (car (car indexelts)))
          (insert "<LI>" (texihtml-make-anchor
			  (if file
			      (concat "../"
				      (texihtml-url-quote file) "/"
				      (texihtml-url-quote
				       (nth 1 (car indexelts)))
				      ".html")
			    (concat
			     (texihtml-url-quote (nth 1 (car indexelts)))
			     ".html"))
			  (car (car indexelts)))
		  (if (nth 2 (car indexelts))
		      (format " (%d)" (nth 2 (car indexelts)))
		    "")
		  "\n")
      ;; index entries from @include'd file
      (texihtml-print-index (nth 1 (car indexelts))
                           (nth 2 (car indexelts))))
    (setq indexelts (cdr indexelts))))


;;; Glyphs: @equiv, @error, etc

;; @equiv           to show that two expressions are equivalent
;; @error           to show an error message
;; @expansion       to show what a macro expands to
;; @point           to show the location of point in an example
;; @print           to show what an evaluated expression prints
;; @result          to indicate the value returned by an expression

(put 'equiv 'texihtml-format 'texihtml-format-equiv)
(defun texihtml-format-equiv ()
  (texihtml-parse-arg-discard)
  (insert "=="))

(put 'error 'texihtml-format 'texihtml-format-error)
(defun texihtml-format-error ()
  (texihtml-parse-arg-discard)
  (insert "error-->"))

(put 'expansion 'texihtml-format 'texihtml-format-expansion)
(defun texihtml-format-expansion ()
  (texihtml-parse-arg-discard)
  (insert "==>"))

(put 'point 'texihtml-format 'texihtml-format-point)
(defun texihtml-format-point ()
  (texihtml-parse-arg-discard)
  (insert "-!-"))

(put 'print 'texihtml-format 'texihtml-format-print)
(defun texihtml-format-print ()
  (texihtml-parse-arg-discard)
  (insert "-|"))

(put 'result 'texihtml-format 'texihtml-format-result)
(defun texihtml-format-result ()
  (texihtml-parse-arg-discard)
  (insert "=>"))


;;; Definition formatting: @deffn, @defun, etc

;; What definition formatting produces:
;;
;; @deffn category name args...
;;     In Info, `Category: name ARGS'
;;     In index: name:  node. line#.
;;
;; @defvr category name 
;;     In Info, `Category: name'
;;     In index: name:  node. line#.
;;
;; @deftp category name attributes...
;; `category name attributes...'       Note: @deftp args in lower case.
;;     In index: name:  node. line#.
;;
;; Specialized function-like or variable-like entity:
;;
;; @defun, @defmac, @defspec, @defvar, @defopt
;;
;; @defun name args           In Info, `Function: name ARGS'
;; @defmac name args          In Info, `Macro: name ARGS'
;; @defvar name               In Info, `Variable: name'
;; etc.
;;     In index: name:  node. line#.
;;
;; Generalized typed-function-like or typed-variable-like entity:
;; @deftypefn category data-type name args...
;;     In Info, `Category:  data-type name args...'
;; @deftypevr category data-type name 
;;     In Info, `Category:  data-type name'
;;     In index: name:  node. line#.
;;
;; Specialized typed-function-like or typed-variable-like entity:
;; @deftypefun data-type name args...
;;     In Info, `Function:  data-type name ARGS'
;;     In index: name:  node. line#.   
;;
;; @deftypevar data-type name 
;;     In Info, `Variable:  data-type name'
;;     In index: name:  node. line#.   but include args after name!?
;;
;; Generalized object oriented entity: 
;; @defop category class name args...
;;     In Info, `Category on class: name ARG'
;;     In index: name on class: node. line#.
;;
;; @defcv category class name         
;;     In Info, `Category of class: name'
;;     In index: name of class: node. line#.
;;
;; Specialized object oriented entity:
;; @defmethod class name args... 
;;     In Info, `Method on class: name ARGS'
;;     In index: name on class: node. line#.
;;
;; @defivar class name
;;     In Info, `Instance variable of class: name'
;;     In index: name of class: node. line#.


;;; The definition formatting functions

(defun texihtml-format-defun ()
  (texihtml-push-stack 'defun nil)
  (setq fill-column (- fill-column 5))
  (texihtml-format-defun-1 t))

(defun texihtml-end-defun ()
  (setq fill-column (+ fill-column 5))
  (texihtml-discard-command)
  (let ((start (nth 1 (texihtml-pop-stack 'defun))))
    (texihtml-do-itemize start)
    ;; Delete extra newline inserted after header.
    (save-excursion
      (goto-char start)
      (delete-char -1))))

(defun texihtml-format-defunx ()
  (texihtml-format-defun-1 nil))

(defun texihtml-format-defun-1 (first-p)
  (let ((parse-args (texihtml-format-parse-defun-args))
        (command-type (get texihtml-command-name 'texihtml-defun-type)))
    (texihtml-discard-command)
    ;; Delete extra newline inserted after previous header line.
    (if (not first-p)
        (delete-char -1))
    (funcall
     (get texihtml-command-name 'texihtml-deffn-formatting-property) parse-args)
    ;; Insert extra newline so that paragraph filling does not mess
    ;; with header line.
    (insert "\n\n")
    (rplaca (cdr (cdr (car texihtml-stack))) (point))
    (funcall
     (get texihtml-command-name 'texihtml-defun-indexing-property) parse-args)))

;;; Formatting the first line of a definition

;; @deffn, @defvr, @deftp
(put 'deffn 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(put 'deffnx 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(put 'defvr 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(put 'defvrx 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(put 'deftp 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(put 'deftpx 'texihtml-deffn-formatting-property 'texihtml-format-deffn)
(defun texihtml-format-deffn (parsed-args)
  ;; Generalized function-like, variable-like, or generic data-type entity:
  ;; @deffn category name args...
  ;;     In Info, `Category: name ARGS'
  ;; @deftp category name attributes...
  ;; `category name attributes...'       Note: @deftp args in lower case.
  (let ((category (car parsed-args))
        (name (car (cdr parsed-args)))
        (args (cdr (cdr parsed-args))))
    (insert " -- " category ": " name)
    (while args
      (insert " "
              (if (or (= ?& (aref (car args) 0))
                      (eq (eval (car command-type)) 'deftp-type))
                  (car args)
                (upcase (car args))))
      (setq args (cdr args)))))

;; @defun, @defmac, @defspec, @defvar, @defopt: Specialized, simple
(put 'defun 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defunx 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defmac 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defmacx 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defspec 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defspecx 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defvar 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defvarx 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defopt 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(put 'defoptx 'texihtml-deffn-formatting-property
     'texihtml-format-specialized-defun)
(defun texihtml-format-specialized-defun (parsed-args)
  ;; Specialized function-like or variable-like entity:
  ;; @defun name args           In Info, `Function: Name ARGS'
  ;; @defmac name args          In Info, `Macro: Name ARGS'
  ;; @defvar name               In Info, `Variable: Name'
  ;; Use cdr of command-type to determine category:
  (let ((category (car (cdr command-type)))
        (name (car parsed-args))
        (args (cdr parsed-args)))
    (insert " -- " category ": " name)
    (while args
      (insert " "
              (if (= ?& (aref (car args) 0))
                  (car args)
                (upcase (car args))))
      (setq args (cdr args)))))

;; @deftypefn, @deftypevr: Generalized typed
(put 'deftypefn 'texihtml-deffn-formatting-property 'texihtml-format-deftypefn)
(put 'deftypefnx 'texihtml-deffn-formatting-property 'texihtml-format-deftypefn)
(put 'deftypevr 'texihtml-deffn-formatting-property 'texihtml-format-deftypefn)
(put 'deftypevrx 'texihtml-deffn-formatting-property 'texihtml-format-deftypefn)
(defun texihtml-format-deftypefn (parsed-args)
  ;; Generalized typed-function-like or typed-variable-like entity:
  ;; @deftypefn category data-type name args...
  ;;     In Info, `Category:  data-type name args...'
  ;; @deftypevr category data-type name 
  ;;     In Info, `Category:  data-type name'
  ;; Note: args in lower case, unless modified in command line.
  (let ((category (car parsed-args))
        (data-type (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category ": " data-type " " name)
    (while args
      (insert " " (car args))
      (setq args (cdr args)))))

;; @deftypefun, @deftypevar: Specialized typed
(put 'deftypefun 'texihtml-deffn-formatting-property 'texihtml-format-deftypefun)
(put 'deftypefunx 'texihtml-deffn-formatting-property
     'texihtml-format-deftypefun)
(put 'deftypevar 'texihtml-deffn-formatting-property 'texihtml-format-deftypefun)
(put 'deftypevarx 'texihtml-deffn-formatting-property
     'texihtml-format-deftypefun)
(defun texihtml-format-deftypefun (parsed-args)
  ;; Specialized typed-function-like or typed-variable-like entity:
  ;; @deftypefun data-type name args...
  ;;     In Info, `Function:  data-type name ARGS'
  ;; @deftypevar data-type name 
  ;;     In Info, `Variable:  data-type name'
  ;; Note: args in lower case, unless modified in command line.
  ;; Use cdr of command-type to determine category:
  (let ((category (car (cdr command-type)))
        (data-type (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr (cdr parsed-args))))
    (insert " -- " category ": " data-type " " name)
    (while args
      (insert " " (car args))
      (setq args (cdr args)))))

;; @defop: Generalized object-oriented
(put 'defop 'texihtml-deffn-formatting-property 'texihtml-format-defop)
(put 'defopx 'texihtml-deffn-formatting-property 'texihtml-format-defop)
(defun texihtml-format-defop (parsed-args)
  ;; Generalized object oriented entity: 
  ;; @defop category class name args...
  ;;     In Info, `Category on class: name ARG'
  ;; Note: args in upper case; use of `on'
  (let ((category (car parsed-args))
        (class (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category " on " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defcv: Generalized object-oriented
(put 'defcv 'texihtml-deffn-formatting-property 'texihtml-format-defcv)
(put 'defcvx 'texihtml-deffn-formatting-property 'texihtml-format-defcv)
(defun texihtml-format-defcv (parsed-args)
  ;; Generalized object oriented entity: 
  ;; @defcv category class name         
  ;;     In Info, `Category of class: name'
  ;; Note: args in upper case; use of `of'
  (let ((category (car parsed-args))
        (class (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category " of " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defmethod: Specialized object-oriented
(put 'defmethod 'texihtml-deffn-formatting-property 'texihtml-format-defmethod)
(put 'defmethodx 'texihtml-deffn-formatting-property 'texihtml-format-defmethod)
(defun texihtml-format-defmethod (parsed-args)
  ;; Specialized object oriented entity:
  ;; @defmethod class name args... 
  ;;     In Info, `Method on class: name ARGS'
  ;; Note: args in upper case; use of `on'
  ;; Use cdr of command-type to determine category:
  (let ((category (car (cdr command-type)))
        (class (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr  (cdr parsed-args))))
    (insert " -- " category " on " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defivar: Specialized object-oriented
(put 'defivar 'texihtml-deffn-formatting-property 'texihtml-format-defivar)
(put 'defivarx 'texihtml-deffn-formatting-property 'texihtml-format-defivar)
(defun texihtml-format-defivar (parsed-args)
  ;; Specialized object oriented entity:
  ;; @defivar class name
  ;;     In Info, `Instance variable of class: name'
  ;; Note: args in upper case; use of `of'
  ;; Use cdr of command-type to determine category:
  (let ((category (car (cdr command-type)))
        (class (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr  (cdr parsed-args))))
    (insert " -- " category " of " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))


;;; Indexing for definitions

;; An index entry has three parts: the `entry proper', the node name, and the
;; line number.  Depending on the which command is used, the entry is
;; formatted differently:
;;
;; @defun, 
;; @defmac, 
;; @defspec, 
;; @defvar, 
;; @defopt          all use their 1st argument as the entry-proper 
;;
;; @deffn, 
;; @defvr, 
;; @deftp 
;; @deftypefun
;; @deftypevar      all use their 2nd argument as the entry-proper
;;
;; @deftypefn, 
;; @deftypevr       both use their 3rd argument as the entry-proper  
;;
;; @defmethod       uses its 2nd and 1st arguments as an entry-proper 
;;                    formatted: NAME on CLASS

;; @defop           uses its 3rd and 2nd arguments as an entry-proper 
;;                    formatted: NAME on CLASS
;;        
;; @defivar         uses its 2nd and 1st arguments as an entry-proper
;;                    formatted: NAME of CLASS
;;
;; @defcv           uses its 3rd and 2nd argument as an entry-proper
;;                    formatted: NAME of CLASS

(put 'defun 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defunx 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defmac 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defmacx 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defspec 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defspecx 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defvar 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defvarx 'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defopt  'texihtml-defun-indexing-property 'texihtml-index-defun)
(put 'defoptx  'texihtml-defun-indexing-property 'texihtml-index-defun)
(defun texihtml-index-defun (parsed-args)
  ;; use 1st parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car parsed-args)
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'deffn 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deffnx 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'defvr 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'defvrx 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftp 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftpx 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftypefun 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftypefunx 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftypevar 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(put 'deftypevarx 'texihtml-defun-indexing-property 'texihtml-index-deffn)
(defun texihtml-index-deffn (parsed-args) 
 ;; use 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car (cdr parsed-args))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'deftypefn 'texihtml-defun-indexing-property 'texihtml-index-deftypefn)
(put 'deftypefnx 'texihtml-defun-indexing-property 'texihtml-index-deftypefn)
(put 'deftypevr 'texihtml-defun-indexing-property 'texihtml-index-deftypefn)
(put 'deftypevrx 'texihtml-defun-indexing-property 'texihtml-index-deftypefn)
(defun texihtml-index-deftypefn (parsed-args)
  ;; use 3rd parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car (cdr (cdr parsed-args)))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defmethod 'texihtml-defun-indexing-property 'texihtml-index-defmethod)
(put 'defmethodx 'texihtml-defun-indexing-property 'texihtml-index-defmethod)
(defun texihtml-index-defmethod (parsed-args)
  ;; use 2nd on 1st parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s on %s"            
                   (car (cdr parsed-args))
                   (car parsed-args))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defop 'texihtml-defun-indexing-property 'texihtml-index-defop)
(put 'defopx 'texihtml-defun-indexing-property 'texihtml-index-defop)
(defun texihtml-index-defop (parsed-args)
  ;; use 3rd on 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s on %s"            
                   (car (cdr (cdr parsed-args)))
                   (car (cdr parsed-args)))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defivar 'texihtml-defun-indexing-property 'texihtml-index-defivar)
(put 'defivarx 'texihtml-defun-indexing-property 'texihtml-index-defivar)
(defun texihtml-index-defivar (parsed-args)
  ;; use 2nd of 1st parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s of %s"            
                   (car (cdr parsed-args))
                   (car parsed-args))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defcv 'texihtml-defun-indexing-property 'texihtml-index-defcv)
(put 'defcvx 'texihtml-defun-indexing-property 'texihtml-index-defcv)
(defun texihtml-index-defcv (parsed-args)
  ;; use 3rd of 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texihtml-findex or the like
  (let ((index-list (get texihtml-command-name 'texihtml-defun-index)))
    (set index-list
         (cons 
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s of %s"            
                   (car (cdr (cdr parsed-args)))
                   (car (cdr parsed-args)))
           texihtml-last-node
           ;; Region formatting may not provide last node position.
           (if texihtml-last-node-pos
               (1+ (count-lines texihtml-last-node-pos (point)))
             1))
          (symbol-value index-list)))))


;;; Properties for definitions

;; Each definition command has six properties:
;;
;; 1. texihtml-deffn-formatting-property      to format definition line
;; 2. texihtml-defun-indexing-property        to create index entry
;; 3. texihtml-format                         formatting command
;; 4. texihtml-end                            end formatting command
;; 5. texihtml-defun-type                     type of deffn to format
;; 6. texihtml-defun-index                    type of index to use
;;
;; The `x' forms of each definition command are used for the second
;; and subsequent header lines.

;; The texihtml-deffn-formatting-property and texihtml-defun-indexing-property
;; are listed just before the appropriate formatting and indexing commands.

(put 'deffn 'texihtml-format 'texihtml-format-defun)
(put 'deffnx 'texihtml-format 'texihtml-format-defunx)
(put 'deffn 'texihtml-end 'texihtml-end-defun)
(put 'deffn 'texihtml-defun-type '('deffn-type nil))
(put 'deffnx 'texihtml-defun-type '('deffn-type nil))
(put 'deffn 'texihtml-defun-index 'texihtml-findex)
(put 'deffnx 'texihtml-defun-index 'texihtml-findex)

(put 'defun 'texihtml-format 'texihtml-format-defun)
(put 'defunx 'texihtml-format 'texihtml-format-defunx)
(put 'defun 'texihtml-end 'texihtml-end-defun)
(put 'defun 'texihtml-defun-type '('defun-type "Function"))
(put 'defunx 'texihtml-defun-type '('defun-type "Function"))
(put 'defun 'texihtml-defun-index 'texihtml-findex)
(put 'defunx 'texihtml-defun-index 'texihtml-findex)

(put 'defmac 'texihtml-format 'texihtml-format-defun)
(put 'defmacx 'texihtml-format 'texihtml-format-defunx)
(put 'defmac 'texihtml-end 'texihtml-end-defun)
(put 'defmac 'texihtml-defun-type '('defun-type "Macro"))
(put 'defmacx 'texihtml-defun-type '('defun-type "Macro"))
(put 'defmac 'texihtml-defun-index 'texihtml-findex)
(put 'defmacx 'texihtml-defun-index 'texihtml-findex)

(put 'defspec 'texihtml-format 'texihtml-format-defun)
(put 'defspecx 'texihtml-format 'texihtml-format-defunx)
(put 'defspec 'texihtml-end 'texihtml-end-defun)
(put 'defspec 'texihtml-defun-type '('defun-type "Special form"))
(put 'defspecx 'texihtml-defun-type '('defun-type "Special form"))
(put 'defspec 'texihtml-defun-index 'texihtml-findex)
(put 'defspecx 'texihtml-defun-index 'texihtml-findex)

(put 'defvr 'texihtml-format 'texihtml-format-defun)
(put 'defvrx 'texihtml-format 'texihtml-format-defunx)
(put 'defvr 'texihtml-end 'texihtml-end-defun)
(put 'defvr 'texihtml-defun-type '('deffn-type nil))
(put 'defvrx 'texihtml-defun-type '('deffn-type nil))
(put 'defvr 'texihtml-defun-index 'texihtml-vindex)
(put 'defvrx 'texihtml-defun-index 'texihtml-vindex)

(put 'defvar 'texihtml-format 'texihtml-format-defun)
(put 'defvarx 'texihtml-format 'texihtml-format-defunx)
(put 'defvar 'texihtml-end 'texihtml-end-defun)
(put 'defvar 'texihtml-defun-type '('defun-type "Variable"))
(put 'defvarx 'texihtml-defun-type '('defun-type "Variable"))
(put 'defvar 'texihtml-defun-index 'texihtml-vindex)
(put 'defvarx 'texihtml-defun-index 'texihtml-vindex)

(put 'defconst 'texihtml-format 'texihtml-format-defun)
(put 'defconstx 'texihtml-format 'texihtml-format-defunx)
(put 'defconst 'texihtml-end 'texihtml-end-defun)
(put 'defconst 'texihtml-defun-type '('defun-type "Constant"))
(put 'defconstx 'texihtml-defun-type '('defun-type "Constant"))
(put 'defconst 'texihtml-defun-index 'texihtml-vindex)
(put 'defconstx 'texihtml-defun-index 'texihtml-vindex)

(put 'defcmd 'texihtml-format 'texihtml-format-defun)
(put 'defcmdx 'texihtml-format 'texihtml-format-defunx)
(put 'defcmd 'texihtml-end 'texihtml-end-defun)
(put 'defcmd 'texihtml-defun-type '('defun-type "Command"))
(put 'defcmdx 'texihtml-defun-type '('defun-type "Command"))
(put 'defcmd 'texihtml-defun-index 'texihtml-findex)
(put 'defcmdx 'texihtml-defun-index 'texihtml-findex)

(put 'defopt 'texihtml-format 'texihtml-format-defun)
(put 'defoptx 'texihtml-format 'texihtml-format-defunx)
(put 'defopt 'texihtml-end 'texihtml-end-defun)
(put 'defopt 'texihtml-defun-type '('defun-type "User Option"))
(put 'defoptx 'texihtml-defun-type '('defun-type "User Option"))
(put 'defopt 'texihtml-defun-index 'texihtml-vindex)
(put 'defoptx 'texihtml-defun-index 'texihtml-vindex)

(put 'deftp 'texihtml-format 'texihtml-format-defun)
(put 'deftpx 'texihtml-format 'texihtml-format-defunx)
(put 'deftp 'texihtml-end 'texihtml-end-defun)
(put 'deftp 'texihtml-defun-type '('deftp-type nil))
(put 'deftpx 'texihtml-defun-type '('deftp-type nil))
(put 'deftp 'texihtml-defun-index 'texihtml-tindex)
(put 'deftpx 'texihtml-defun-index 'texihtml-tindex)

;;; Object-oriented stuff is a little hairier.

(put 'defop 'texihtml-format 'texihtml-format-defun)
(put 'defopx 'texihtml-format 'texihtml-format-defunx)
(put 'defop 'texihtml-end 'texihtml-end-defun)
(put 'defop 'texihtml-defun-type '('defop-type nil))
(put 'defopx 'texihtml-defun-type '('defop-type nil))
(put 'defop 'texihtml-defun-index 'texihtml-findex)
(put 'defopx 'texihtml-defun-index 'texihtml-findex)

(put 'defmethod 'texihtml-format 'texihtml-format-defun)
(put 'defmethodx 'texihtml-format 'texihtml-format-defunx)
(put 'defmethod 'texihtml-end 'texihtml-end-defun)
(put 'defmethod 'texihtml-defun-type '('defmethod-type "Method"))
(put 'defmethodx 'texihtml-defun-type '('defmethod-type "Method"))
(put 'defmethod 'texihtml-defun-index 'texihtml-findex)
(put 'defmethodx 'texihtml-defun-index 'texihtml-findex)

(put 'defcv 'texihtml-format 'texihtml-format-defun)
(put 'defcvx 'texihtml-format 'texihtml-format-defunx)
(put 'defcv 'texihtml-end 'texihtml-end-defun)
(put 'defcv 'texihtml-defun-type '('defop-type nil))
(put 'defcvx 'texihtml-defun-type '('defop-type nil))
(put 'defcv 'texihtml-defun-index 'texihtml-vindex)
(put 'defcvx 'texihtml-defun-index 'texihtml-vindex)

(put 'defivar 'texihtml-format 'texihtml-format-defun)
(put 'defivarx 'texihtml-format 'texihtml-format-defunx)
(put 'defivar 'texihtml-end 'texihtml-end-defun)
(put 'defivar 'texihtml-defun-type '('defmethod-type "Instance variable"))
(put 'defivarx 'texihtml-defun-type '('defmethod-type "Instance variable"))
(put 'defivar 'texihtml-defun-index 'texihtml-vindex)
(put 'defivarx 'texihtml-defun-index 'texihtml-vindex)

;;; Typed functions and variables

(put 'deftypefn 'texihtml-format 'texihtml-format-defun)
(put 'deftypefnx 'texihtml-format 'texihtml-format-defunx)
(put 'deftypefn 'texihtml-end 'texihtml-end-defun)
(put 'deftypefn 'texihtml-defun-type '('deftypefn-type nil))
(put 'deftypefnx 'texihtml-defun-type '('deftypefn-type nil))
(put 'deftypefn 'texihtml-defun-index 'texihtml-findex)
(put 'deftypefnx 'texihtml-defun-index 'texihtml-findex)

(put 'deftypefun 'texihtml-format 'texihtml-format-defun)
(put 'deftypefunx 'texihtml-format 'texihtml-format-defunx)
(put 'deftypefun 'texihtml-end 'texihtml-end-defun)
(put 'deftypefun 'texihtml-defun-type '('deftypefun-type "Function"))
(put 'deftypefunx 'texihtml-defun-type '('deftypefun-type "Function"))
(put 'deftypefun 'texihtml-defun-index 'texihtml-findex)
(put 'deftypefunx 'texihtml-defun-index 'texihtml-findex)

(put 'deftypevr 'texihtml-format 'texihtml-format-defun)
(put 'deftypevrx 'texihtml-format 'texihtml-format-defunx)
(put 'deftypevr 'texihtml-end 'texihtml-end-defun)
(put 'deftypevr 'texihtml-defun-type '('deftypefn-type nil))
(put 'deftypevrx 'texihtml-defun-type '('deftypefn-type nil))
(put 'deftypevr 'texihtml-defun-index 'texihtml-vindex)
(put 'deftypevrx 'texihtml-defun-index 'texihtml-vindex)

(put 'deftypevar 'texihtml-format 'texihtml-format-defun)
(put 'deftypevarx 'texihtml-format 'texihtml-format-defunx)
(put 'deftypevar 'texihtml-end 'texihtml-end-defun)
(put 'deftypevar 'texihtml-defun-type '('deftypevar-type "Variable"))
(put 'deftypevarx 'texihtml-defun-type '('deftypevar-type "Variable"))
(put 'deftypevar 'texihtml-defun-index 'texihtml-vindex)
(put 'deftypevarx 'texihtml-defun-index 'texihtml-vindex)


;;; @set, @clear, @ifset, @ifclear

;; If a flag is set with @set FLAG, then text between @ifset and @end
;; ifset is formatted normally, but if the flag is is cleared with
;; @clear FLAG, then the text is not formatted; it is ignored.

;; If a flag is cleared with @clear FLAG, then text between @ifclear
;; and @end ifclear is formatted normally, but if the flag is is set with
;; @set FLAG, then the text is not formatted; it is ignored.  @ifclear
;; is the opposite of @ifset.

;; If a flag is set to a string with @set FLAG, 
;; replace  @value{FLAG} with the string.
;; If a flag with a value is cleared, 
;; @value{FLAG} is invalid, 
;; as if there had never been any @set FLAG previously.

(put 'clear 'texihtml-format 'texihtml-clear)
(defun texihtml-clear ()
  "Clear the value of the flag."
  (let* ((arg (texihtml-parse-arg-discard))
         (flag (car (read-from-string arg)))
         (value (substring arg (cdr (read-from-string arg)))))
    (put flag 'texihtml-whether-setp 'flag-cleared)
    (put flag 'texihtml-set-value "")))

(put 'set 'texihtml-format 'texihtml-set)
(defun texihtml-set ()
  "Set the value of the flag, optionally to a string.
The command  `@set foo This is a string.'
sets flag foo to the value: `This is a string.'
The command  `@value{foo}'  expands to the value."
  (let* ((arg (texihtml-parse-arg-discard))
         (flag (car (read-from-string arg)))
         (value (substring arg (cdr (read-from-string arg)))))
    (put flag 'texihtml-whether-setp 'flag-set)
    (put flag 'texihtml-set-value value)))

(put 'value 'texihtml-format 'texihtml-value)
(defun texihtml-value ()
  "Insert the string to which the flag is set.
The command  `@set foo This is a string.'
sets flag foo to the value: `This is a string.'
The command  `@value{foo}'  expands to the value."
  (let ((arg (texihtml-parse-arg-discard)))
    (cond ((and
            (eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
                'flag-set)
            (get (car (read-from-string arg)) 'texihtml-set-value))
           (insert (get (car (read-from-string arg)) 'texihtml-set-value)))
          ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp) 
               'flag-cleared)
           (insert (format "{No value for \"%s\"}"  arg)))
          ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp) nil)
           (insert (format "{No value for \"%s\"}"  arg))))))

(put 'ifset 'texihtml-end 'texihtml-discard-command)
(put 'ifset 'texihtml-format 'texihtml-if-set)
(defun texihtml-if-set ()
  "If set, continue formatting; else do not format region up to @end ifset"
  (let ((arg (texihtml-parse-arg-discard)))
    (cond
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          'flag-set)
      ;; Format the text (i.e., do not remove it); do nothing here.
      ())
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          'flag-cleared)
      ;; Clear region (i.e., cause the text to be ignored).
      (delete-region texihtml-command-start
                       (progn (re-search-forward "@end ifset[ \t]*\n")
                              (point))))
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          nil)
      (error  "@ifset flag `%s' is not defined by @set or @clear." arg)))))

(put 'ifclear 'texihtml-end 'texihtml-discard-command)
(put 'ifclear 'texihtml-format 'texihtml-if-clear)
(defun texihtml-if-clear ()
  "If clear, continue formatting; if set, do not format up to @end ifset"
  (let ((arg (texihtml-parse-arg-discard)))
    (cond
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          'flag-set)
      ;; Clear region (i.e., cause the text to be ignored).
      (delete-region texihtml-command-start
                       (progn (re-search-forward "@end ifclear[ \t]*\n")
                              (point))))
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          'flag-cleared)
      ;; Format the text (i.e., do not remove it); do nothing here.
      ())
     ((eq (get (car (read-from-string arg)) 'texihtml-whether-setp)
          nil)
      (error  "@ifclear flag `%s' is not defined by @clear or @set." arg)))))


;;; Process included files:  `@include' command

;; Updated 19 October 1990
;; In the original version, include files were ignored by Info but
;; incorporated in to the printed manual.  To make references to the
;; included file, the Texihtml source file has to refer to the included
;; files using the `(filename)nodename' format for refering to other
;; Info files.  Also, the included files had to be formatted on their
;; own.  It was just like they were another file.

;; Currently, include files are inserted into the buffer that is
;; formatted for Info.  If large, the resulting info file is split and
;; tagified.  For current include files to work, the master menu must
;; refer to all the nodes, and the highest level nodes in the include
;; files must have the correct next, prev, and up pointers.

;; The included file may have an @setfilename and even an @settitle,
;; but not an `\input texihtml' line.

;; Updated 24 March 1993
;; In order for @raisesections and @lowersections to work, included
;; files must be inserted into the buffer holding the outer file
;; before other Info formatting takes place.  So @include is no longer
;; is treated like other @-commands.
(put 'include 'texihtml-format  'texihtml-format-noop)

; Original definition:
; (defun texihtml-format-include ()
;   (let ((filename (texihtml-parse-arg-discard))
;       (default-directory input-directory)
;       subindex)
;     (setq subindex
;         (save-excursion
;           (progn (find-file
;                   (cond ((file-readable-p (concat filename ".texihtml"))
;                          (concat filename ".texihtml"))
;                         ((file-readable-p (concat filename ".texi"))
;                          (concat filename ".texi"))
;                         ((file-readable-p (concat filename ".tex"))
;                          (concat filename ".tex"))
;                         ((file-readable-p filename)
;                          filename)
;                         (t (error "@include'd file %s not found"
;                                   filename))))
;                  (texihtml-format-buffer-1))))
;     (texihtml-subindex 'texihtml-vindex (car subindex) (nth 1 subindex))
;     (texihtml-subindex 'texihtml-findex (car subindex) (nth 2 subindex))
;     (texihtml-subindex 'texihtml-cindex (car subindex) (nth 3 subindex))
;     (texihtml-subindex 'texihtml-pindex (car subindex) (nth 4 subindex))
;     (texihtml-subindex 'texihtml-tindex (car subindex) (nth 5 subindex))
;     (texihtml-subindex 'texihtml-kindex (car subindex) (nth 6 subindex))))
;
;(defun texihtml-subindex (indexvar file content)
;  (set indexvar (cons (list 'recurse file content)
;                      (symbol-value indexvar))))

; Second definition:
; (put 'include 'texihtml-format 'texihtml-format-include)
; (defun texihtml-format-include ()
;   (let ((filename (concat input-directory
;                           (texihtml-parse-arg-discard)))
;         (default-directory input-directory))
;     (message "Reading: %s" filename)
;     (save-excursion
;       (save-restriction
;         (narrow-to-region
;          (point)
;          (+ (point) (car (cdr (insert-file-contents filename)))))
;         (goto-char (point-min))
;         (texihtml-append-refill)
;         (texihtml-format-convert (point-min) (point-max))))
;     (setq last-input-buffer input-buffer)  ; to bypass setfilename
;     ))


;;; Numerous commands do nothing in Texihtml

;; These commands are defined in texinfo.tex for printed output.
;; maybe we should do something with these for html, idunno

(put 'bye 'texihtml-format 'texihtml-discard-line)
(put 'c 'texihtml-format 'texihtml-discard-line-with-args)
(put 'comment 'texihtml-format 'texihtml-discard-line-with-args)
(put 'contents 'texihtml-format 'texihtml-discard-line-with-args)
(put 'finalout 'texihtml-format 'texihtml-discard-line)
(put 'group 'texihtml-end 'texihtml-discard-line-with-args)
(put 'group 'texihtml-format 'texihtml-discard-line-with-args)
(put 'headings 'texihtml-format 'texihtml-discard-line-with-args)
(put 'hsize 'texihtml-format 'texihtml-discard-line-with-args)
(put 'itemindent 'texihtml-format 'texihtml-discard-line-with-args)
(put 'lispnarrowing 'texihtml-format 'texihtml-discard-line-with-args)
(put 'need 'texihtml-format 'texihtml-discard-line-with-args)
(put 'nopara 'texihtml-format 'texihtml-discard-line-with-args)
(put 'page 'texihtml-format 'texihtml-discard-line-with-args)
(put 'parindent 'texihtml-format 'texihtml-discard-line-with-args)
(put 'setchapternewpage 'texihtml-format 'texihtml-discard-line-with-args)
(put 'setq 'texihtml-format 'texihtml-discard-line-with-args)
(put 'settitle 'texihtml-format 'texihtml-discard-line-with-args)
(put 'setx 'texihtml-format 'texihtml-discard-line-with-args)
(put 'shortcontents 'texihtml-format 'texihtml-discard-line-with-args)
(put 'smallbook 'texihtml-format 'texihtml-discard-line)
(put 'summarycontents 'texihtml-format 'texihtml-discard-line-with-args)


;;; Some commands cannot be handled

(defun texihtml-unsupported ()
  (error "%s is not handled by texihtml"
         (buffer-substring texihtml-command-start texihtml-command-end)))

;;; Batch formatting

(defun batch-texihtml-format ()
  "Runs  texihtml-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texihtml-format $docs/ ~/*.texihtml\"."
  (if (not noninteractive)
      (error "batch-texihtml-format may only be used -batch."))
  (let ((version-control t)
        (auto-save-default nil)
        (find-file-run-dired nil)
        (kept-old-versions 259259)
        (kept-new-versions 259259))
    (let ((error 0)
          file
          (files ()))
      (while command-line-args-left
        (setq file (expand-file-name (car command-line-args-left)))
        (cond ((not (file-exists-p file))
               (message ">> %s does not exist!" file)
               (setq error 1
                     command-line-args-left (cdr command-line-args-left)))
              ((file-directory-p file)
               (setq command-line-args-left
                     (nconc (directory-files file)
                            (cdr command-line-args-left))))
              (t
               (setq files (cons file files)
                     command-line-args-left (cdr command-line-args-left)))))
      (while files
        (setq file (car files)
              files (cdr files))
        (condition-case err
            (progn
              (if buffer-file-name (kill-buffer (current-buffer)))
              (find-file file)
              (buffer-flush-undo (current-buffer))
              (set-buffer-modified-p nil)
              (texihtml-mode)
              (message "texihtml formatting %s..." file)
              (texihtml-format-buffer nil)
              (if (buffer-modified-p)
                  (progn (message "Saving modified %s" (buffer-file-name))
                         (save-buffer))))
          (error
           (message ">> Error: %s" (prin1-to-string err))
           (message ">>  point at")
           (let ((s (buffer-substring (point)
                                      (min (+ (point) 100)
                                           (point-max))))
                 (tem 0))
             (while (setq tem (string-match "\n+" s tem))
               (setq s (concat (substring s 0 (match-beginning 0))
                               "\n>>  "
                               (substring s (match-end 0)))
                     tem (1+ tem)))
             (message ">>  %s" s))
           (setq error 1))))
      (kill-emacs error))))


;;; Place `provide' at end of file.
(provide 'texinfmt)
;;;;;;;;;;;;;;;; end texinfmt.el ;;;;;;;;;;;;;;;;
