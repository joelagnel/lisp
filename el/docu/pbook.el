;;; Saved through ges-version 0.3.3dev at 2004-11-20 17:34
;;; From: Luke Gorrie <luke@bluetail.com>
;;; Subject: pbook.el
;;; Newsgroups: gmane.emacs.sources
;;; Date: Mon, 17 May 2004 03:11:14 +0200

;;; pbook.el -- Format a program listing for LaTeX.
;;; Written by Luke Gorrie <luke@member.fsf.org> in May of 2004.
;;; $Id: pbook.el,v 1.3 2004/05/17 01:09:01 luke Exp luke $
;;;
;;; You can find a pretty PDF version of this program here:
;;;   http://www.bluetail.com/~luke/misc/emacs/pbook.pdf
;;;
;;;# Introduction
;;;
;;; Have you ever printed out a program and read it on paper?
;;;
;;; It is an interesting exercise to try with one of your own
;;; programs, one that you think is well-written. The first few times
;;; you will probably find that it's torture to try and read in a
;;; straight line. What seemed so nice in Emacs is riddled with
;;; glaring problems on paper.
;;;
;;; How a program reads on paper may not be very important in itself,
;;; but there is wonderful upside to this. If you go through the
;;; program with a red pen and fix all the mind-bendingly obvious
;;; problems you see, what happens is that the program greatly
;;; improves -- not just on paper, but also in Emacs!
;;;
;;; This is a marvellously effective way to make programs
;;; better.
;;;
;;; Let's explore the idea some more!
;;;
;;;# `pbook'
;;;
;;; This program, `pbook', is a tool for making readable programs by
;;; generating LaTeX'ified program listings. Its purpose is to help
;;; you improve your programs by making them read well on paper. It
;;; serves this end by generating pretty-looking PDF output for you to
;;; print out and attack with a red pen, and perhaps use the medium to
;;; trick your mind into seeking the clarity of a technical paper and
;;; bringing your prose-editing skills to bear on your source code.
;;;
;;; `pbook' is aware of three things: headings, top-level comments,
;;; and code. Headings become LaTeX sections, and have entries in a
;;; table of contents. Top-level comments become plain text in a nice
;;; variable-width font. Other source code is listed as-is in a
;;; fixed-width font.
;;;
;;; These different elements are distinguished in the source using
;;; maximally unobtrusive markup, which you can see at work in the
;;; `pbook.el' source code.
;;;
;;; Read on to see the program and how it works.
;;;
;;;# Prelude
;;;
;;; I have successfully tested this program with GNU Emacs versions
;;; 20.7 and 21.3, and with XEmacs version 21.5.
;;;
;;; For some tiny luxuries and portability help we use the Common Lisp
;;; compatibility library:
(require 'cl)

;;;# Emacs commands
;;;
;;; A handful of Emacs commands make up the pbook user-interface. The
;;; most fundamental is to render a pbook-formatted Emacs buffer as
;;; LaTeX.

(defun pbook-buffer ()
  "Generate LaTeX from the current (pbook-formatted) buffer.
The resulting source is displayed in a buffer called *pbook*."
  (interactive)
  (pbook-process-buffer))

;;; A very handy utility is to display a summary of the buffer's
;;; structure and use it to jump to an appropriate section. I've
;;; always enjoyed being able to do this in texinfo-mode. Happily,
;;; pbook gets this for free using the `occur' function, which lists
;;; all lines in the buffer that match some regular expression.

(defun pbook-show-structure ()
  "Display the pbook heading structure of the current buffer."
  (interactive)
  (occur pbook-heading-regexp))

;;; To avoid a lot of mucking about in the shell there is also a
;;; command to generate and display a PDF file. This function is a
;;; quick hack to make experimentation easy.

(defun pbook-buffer-view-pdf ()
  "Generate and display PDF from the current buffer.
The intermediate files are created in the standard temporary
directory."
  (interactive)
  (save-window-excursion
    (pbook-buffer))
  (with-current-buffer "*pbook*"
    (let ((texfile (pbook-tmpfile "pbook" "tex"))
          (pdffile (pbook-tmpfile "pbook" "pdf")))
      (write-region (point-min) (point-max) texfile)
      ;; Possibly there is a better way to ensure that LaTeX generates
      ;; the table of contents correctly than to run it more than
      ;; once, but I don't know one.
      (shell-command (format "\
  cd /tmp; latex %s && pdflatex %s && acroread %s &"
                             texfile texfile pdffile)))))

(defun pbook-tmpfile (name extension)
  "Return the full path to a temporary file called NAME and with EXTENSION.
An appropriate directory is chosen and the PID of Emacs is
inserted before the extension."
  (format "%s%s-%S.%s"
          (if (boundp 'temporary-file-directory)
              temporary-file-directory
            ;; XEmacs does it this way instead:
            (temp-directory))
          name (emacs-pid) extension))

;;;# Configurable variables
;;;
;;; These are variables that can be customized to affect pbook's
;;; behaviour. The default regular expressions assume Lisp-style
;;; comment characters, but they can be overridden with buffer-local
;;; bindings from hooks for other programming modes. The other
;;; variables that control formatting are best configured with Emacs's
;;; magic "file variables" (see down the very bottom for an example).

(defvar pbook-commentary-regexp "^;;;\\($\\|[^#]\\)"
  "Regular expression matching lines of high-level commentary.")

(defvar pbook-heading-regexp "^;;;\\(#+\\)"
  "Regular expression matching heading lines of chapters/sections/headings.")

(defvar pbook-heading-level-subexp 1
  "The subexpression of `pbook-heading-regexp' whose length indicates nesting.")

(defvar pbook-include-toc t
  "When true include a table of contents.")

(defvar pbook-style 'article
  "Style of output. Either article (small) or book (large).")

(defvar pbook-author (user-full-name)
  "The name to use in the \author LaTeX command.")

;;;# Top-level logic
;;;
;;; Here we have the top level of the program. Setting up, calling the
;;; formatting engine, piecing things together, and putting on the
;;; finishing touches.
;;;
;;; The real work is done in a new buffer called *pbook*. First the
;;; source is copied into this buffer and from there it is massaged
;;; into shape.
;;;
;;; Most of this is mundane, but there is one tricky part: the source
;;; buffer may have buffer-local values for some pbook settings, and
;;; we have to be careful or we'd lose them when switching into the
;;; *pbook* buffer. This is taken care of by moving the correct values
;;; of all the relevant customizable settings into new dynamic
;;; bindings.

(defun pbook-process-buffer ()
  "Generate pbook output for the current buffer
The output is put in the buffer *pbook* and displayed."
  (interactive)
  (let ((buffer    (current-buffer))
        (beginning (pbook-tex-beginning))
        (ending    (pbook-tex-ending))
        (text      (buffer-string)))
    (with-current-buffer (get-buffer-create "*pbook*")
      ;; Setup,
      (pbook-inherit-buffer-locals buffer
                                   '(pbook-commentary-regexp
                                     pbook-heading-regexp
                                     pbook-style))
      (erase-buffer)
      (insert text)
      ;; Reformat as LaTeX,
      (pbook-preprocess)
      (pbook-format-buffer)
      ;; Insert header & footer.
      (goto-char (point-min))
      (insert beginning)
      (goto-char (point-max))
      (insert ending)
      (display-buffer (current-buffer)))))

(defun pbook-inherit-buffer-locals (buffer variables)
  "Make buffer-local bindings of VARIABLES using the values in BUFFER."
  (dolist (v variables)
    (set (make-local-variable v)
         (with-current-buffer buffer (symbol-value v)))))

(defun pbook-preprocess ()
  "Cleanup the buffer to prepare for formatting."
  (goto-char (point-min))
  ;; FIXME: Currently we just zap all pagebreak characters.
  (save-excursion
    (while (re-search-forward "\C-l" nil t)
      (replace-match "")))
  (unless (re-search-forward pbook-heading-regexp nil t)
    (error "File must have at least one heading."))
  (beginning-of-line)
  ;; Delete everything before the first heading.
  (delete-region (point-min) (point)))

(defun pbook-tex-beginning ()
  "Return the beginning prelude for the LaTeX output."
  (format "\
\\documentclass[notitlepage,a4paper]{%s}
\\title{%s}
\\author{%s}
\\begin{document}
\\maketitle
%s\n"
          (symbol-name pbook-style)
          (pbook-latex-escape-string (buffer-name))
          (pbook-latex-escape-string pbook-author)
          (if pbook-include-toc "\\tableofcontents" "")))

(defun pbook-tex-ending ()
  "Return the ending of the LaTeX output."
  "\\end{document}\n")

;;;# Escaping special characters
;;;
;;; We have to escape characters that LaTeX treats specially. This is
;;; done based on the rules in the `Special Characters' node of the
;;; LaTeX2e info manual.

(defun pbook-latex-escape-string (string)
  (with-temp-buffer
    (insert string)
    (pbook-latex-escape (point-min) (point-max))
    (buffer-string)))

(defun pbook-latex-escape (start end)
  "LaTeX-escape special characters in the region from START to END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward "\\\\" nil t)
          (replace-match "$\\backslash$" nil t)))
      (save-excursion
        (while (re-search-forward "\\([#%&~$_^{}]\\)" nil t)
          ;; Don't re-escape our escaped backslashes.
          (if (and (equal (char-before) ?\$)
                   (looking-at (regexp-quote "\\backslash$")))
              (goto-char (match-end 0))
            (replace-match "\\\\\\1")))))))

;;;# Processing engine
;;;
;;; The main loop scans through the source buffer piece by piece and
;;; converts each one to LaTeX as it goes. There are three sorts of
;;; pieces: headings, top-level commentary, and code.
;;;
;;; This loop recognises what type of piece is at the point and then
;;; calls the appropriate subroutine. The subroutines are responsible
;;; for determining where their piece finishes and for advancing the
;;; point beyond the region they have formatted.

(defun pbook-format-buffer ()
  (while (not (eobp))
    (if (looking-at "^\\s *$")
        ;; Skip blank lines.
        (forward-line)
      (cond ((looking-at pbook-heading-regexp)
             (pbook-do-heading))
            ((looking-at pbook-commentary-regexp)
             (pbook-do-commentary))
            (t
             (pbook-do-code))))))

;;;## Heading formatting
;;;
;;; Each heading line is converted to a LaTeX sectioning command. The
;;; heading text is escaped.

(defun pbook-do-heading ()
  ;; NB: `looking-at' sets the Emacs match data (for match-string, etc)
  (assert (looking-at pbook-heading-regexp))
  (let ((depth (length (match-string-no-properties pbook-heading-level-subexp))))
    ;; Strip off the comment characters and whitespace.
    (replace-match "")
    (when (looking-at "\\s +")
      (replace-match ""))
    (pbook-latex-escape (line-beginning-position) (line-end-position))
    (wrap-line (format "\\%s{" (pbook-nth-sectioning-command depth))
               "}"))
  (forward-line))

(defun wrap-line (prefix suffix)
  "Insert PREFIX at the start of the current line and SUFFIX at the end."
  (save-excursion
    (goto-char (line-beginning-position))
    (insert prefix)
    (goto-char (line-end-position))
    (insert suffix)))

;;; LaTeX has different sectioning commands for articles and books, so
;;; we have to choose from the right set. These variables define the
;;; sets in order of nesting -- the first element is top-level, etc.

(defconst pbook-article-sectioning-commands
  '("section" "subsection" "subsubsection")
  "LaTeX commands for sectioning articles.")

(defconst pbook-book-sectioning-commands
  (cons "chapter" pbook-article-sectioning-commands)
  "LaTeX commands for sectioning books.")

(defun pbook-nth-sectioning-command (n)
  "Return the sectioning command for nesting level N (top-level is 1)."
  (let ((commands (ecase pbook-style
                    (article pbook-article-sectioning-commands)
                    (book    pbook-book-sectioning-commands))))
    (nth (min (1- n) (1- (length commands))) commands)))

;;;## Commentary formatting
;;;
;;; Top-level commentary is stripped of its comment characters and we
;;; escape all characters that LaTeX treats specially.

(defun pbook-do-commentary ()
  "Format one or more lines of commentary into LaTeX."
  (assert (looking-at pbook-commentary-regexp))
  (let ((start (point)))
    ;; Strip off comment characters line-by-line until end of section.
    (while (or (looking-at pbook-commentary-regexp)
               (and (looking-at "^\\s *$")
                    (not (eobp))))
      (replace-match "")
      (delete-horizontal-space)
      (forward-line))
    (save-excursion
      (pbook-latex-escape start (point))
      (pbook-pretty-commentary start (point)))))

;;; These functions define a simple Wiki-like markup language for
;;; basic formatting.

(defun pbook-pretty-commentary (start end)
  "Make commentary prettier."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (save-excursion (pbook-pretty-tt))
    (save-excursion (pbook-pretty-doublequotes))))

(defun pbook-pretty-tt ()
  "Format `single quoted' text with a typewriter font."
  (while (re-search-forward "`\\([^`']*\\)'" nil t)
    (replace-match "{\\\\tt \\1}" t)))

(defun pbook-pretty-doublequotes ()
  "Format \"double quoted\" text with ``double single quotes''."
  (while (re-search-forward "\"\\([^\"]*\\)\"" nil t)
    (replace-match "``\\1''")))

;;;## Source code formatting
;;;
;;; Source text is rendered in the `verbatim' environment.

(defun pbook-do-code ()
  (assert (and (not (looking-at pbook-commentary-regexp))
               (not (looking-at pbook-heading-regexp))))
  (let ((start (point)))
    (insert "\\begin{verbatim}\n")
    (pbook-goto-end-of-code)
    (pbook-escape-code start (point))
    (pbook-convert-tabs-to-spaces start (point))
    ;; Delete trailing newlines and spaces.
    (while (or (equal (char-syntax (char-before)) " ")
               (bolp))
      (delete-char -1))
    (insert "\n\\end{verbatim}\n")))

(defun pbook-goto-end-of-code ()
  "Goto the end of the current section of code."
  (if (re-search-forward (format "\\(%s\\)\\|\\(%s\\)"
                                 pbook-heading-regexp
                                 pbook-commentary-regexp)
                         nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun pbook-convert-tabs-to-spaces (start end)
  "Replace tab characters with spaces."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\t" nil t)
        (replace-match (make-string tab-width ?\ ))))))

;;; The escaping rules for verbatim environments are unclear to me. It
;;; looks like the only thing that needs escaping is `\end{verbatim}',
;;; but I don't know of an escape mechanism that works (`\' is taken
;;; literally). Since most programs (apart from this one..) won't
;;; contain that string I have made a kludge to fudge it by inserting
;;; an underscore.
;;;
(defun pbook-escape-code (start end)
  "Escape verbatim source code for LaTeX."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\\\end{verbatim}" nil t)
        (replace-match "\\_end{verbatim}" nil t)))))

;;;# Prologue and file variables

(provide 'pbook)

;;; ;;; We use Emacs's magic `file variables' to make sure pbook is
;;; ;;; formatted how it should be:

;;; ;; Local Variables:
;;; ;; pbook-author:  "Luke Gorrie"
;;; ;; pbook-use-toc: t
;;; ;; pbook-style:   article
;;; ;; End:

