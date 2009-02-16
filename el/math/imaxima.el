;;;; imaxima.el --- Maxima mode with images

;; Copyright (C) 2001, 2002, 2003, 2004 Jesper Harder

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 14 Nov 2001
;; Version: 0.9
;; Location: <http://purl.org/harder/imaxima.html>
;; Keywords: maxima

;; $Id: imaxima.el,v 1.4 2005/08/31 17:11:29 yasube Exp yasube $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;; This file (and imaxima.lisp) provides image support for interacting
;; with the computer algebra system Maxima
;; <http://maxima.sourceforge.net/>
;;
;; The command `imaxima' (M-x imaxima) provides a simple comint
;; derived CLI mode.
;;
;; To use imaxima with the Maxima mode from the Maxima distribution
;; set `imaxima-use-maxima-mode-flag' to `t'.
;;
;; To turn off images, evaluate "display2d:true" in Maxima.  To turn
;; them on again, evaluate "display2d:imaxima".
;;
;; The command `imaxima-latex' prepares a LaTeX version of the Maxima
;; buffer.
;;
;; The package requires Emacs 21 with image support (the ms-windows
;; port doesn't support images yet).
;;
;; A fairly recent version of Ghostscript is recommended (at least
;; newer than v. 5.5).  If your version is too old, you can either set
;; `imaxima-image-type' to 'ps or remove the options
;; "-dTextAlphaBits=4" and "-dGraphicsAlphaBits=4" from
;; `imaxima-gs-options'.  The images won't look nearly as attractive,
;; though -- the text looks ragged because it isn't anti aliased.
;;
;; The file "imaxima.lisp" is a slightly modified version of
;; "texmacs.lisp" in the TeXmacs distribution.  Several of the image
;; routines are borrowed from David Kastrup's preview-latex.el.
;;
;;
;; Installation:
;;
;; * Execute
;;
;;     ./configure
;;     make
;;     make install
;;
;;   to byte-compile and install `imaxima.el', `imaxima.lisp' and the
;;   documentation.
;;
;; * Get the LaTeX package `breqn' from <ftp://ftp.ams.org/pub/tex/>.
;;   Install the package where TeX can find it -- e.g. install in your
;;   texmf or local.texmf tree and run texhash.
;;
;;   (With teTeX a suitable place for the *.sty and *.sym files could
;;   be "/usr/share/texmf/tex/latex/breqn/" and
;;   "/usr/share/texmf/doc/latex/breqn/" for the documentation.)
;;
;; * Put (autoload 'imaxima "imaxima" "Image support for Maxima." t)
;;   in your .emacs file.


;;; Code:

;; modified to remove eval-when-compile form. Surrounding
;; the eval-when-compile prevernts imaxima from running
;; properly in the xemacs on cygwin environment.
(require 'advice)

(require 'comint)
(require 'cl)

;; XEmacs stuff

(defalias 'imaxima-image-type-available-p
  (if (fboundp 'image-type-available-p)
      'image-type-available-p
    'featurep))

(defalias 'imaxima-display-pixel-width
  (if (fboundp 'display-pixel-width)
      'display-pixel-width
    'device-pixel-width))

(defalias 'imaxima-display-pixel-height
  (if (fboundp 'display-pixel-height)
      'display-pixel-height
    'device-pixel-height))

(defalias 'imaxima-display-mm-width
  (if (fboundp 'display-mm-width)
      'display-mm-width
    'device-mm-width))

(defalias 'imaxima-display-mm-height
  (if (fboundp 'display-mm-height)
      'display-mm-height
    'device-mm-height))

(defalias 'imaxima-get-window-width
  (if (featurep 'xemacs)
      'imaxima-get-window-width-xemacs
    'imaxima-get-window-width-emacs))

(defalias 'imaxima-color-values
  (if (fboundp 'color-values)
      'color-values
    '(lambda (color) (color-rgb-components
		      (if (stringp color)
			  (make-color-specifier color)
			color)))))

(defun imaxima-get-bg-color ()
  (if (featurep 'xemacs)
      (face-property 'default 'background)
    (frame-parameter nil 'background-color)))

(defun imaxima-get-fg-color ()
  (if (featurep 'xemacs)
      (face-property 'default 'foreground)
    (frame-parameter nil 'foreground-color)))

;; XEmacs doesn't have subst-char-in-string (sigh!).

(defun imaxima-subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))


(defconst imaxima-mouse2 (if (featurep 'xemacs)
			   [button2]
			 [mouse-2]))

(defconst imaxima-mouse3 (if (featurep 'xemacs)
			   [button3]
			 [mouse-3]))

;; Options

(defgroup imaxima nil
  "Image support for Maxima."
  :version "21.1"
  :link '(url-link "http://purl.org/harder/imaxima.html")
  :link '(custom-manual "(imaxima)")
  :prefix "imaxima-"
  :group 'maxima)

(defvar process-connection-type-flag
  (if (eql system-type 'darwin) t nil))

(defvar imaxima-image-types '(png postscript jpeg tiff))

(defcustom imaxima-image-type 'png
  "Image type to used in Maxima buffer."
  :group 'imaxima
  :type (cons 'choice
	      (mapcar (lambda (type) (list 'const type))
		      (remove-if-not 'imaxima-image-type-available-p
				     imaxima-image-types))))

(defcustom imaxima-pt-size 11
  "*Point size used in LaTeX."
  :group 'imaxima
  :type '(choice (const 9)
		 (const 10)
		 (const 11)
		 (const 12)))

(defcustom imaxima-fnt-size "normalsize"
  "*Default size of font."
  :group 'imaxima
  :type '(choice (const "small")
		 (const "normalsize")
		 (const "large")
		 (const "Large")
		 (const "LARGE")
		 (const "huge")
		 (const "Huge")))

(defcustom imaxima-scale-factor 1.0
  "*All images are scaled by this factor."
  :group 'imaxima
  :type 'number)

(defcustom imaxima-label-color "red"
  "*Color used in output labels."
  :group 'imaxima
  :type '(color))

(defcustom imaxima-equation-color (imaxima-get-fg-color)
  "*Color used for equations."
  :group 'imaxima
  :type '(color))

(defcustom imaxima-bg-color nil
    "Background color of imaxima buffer."
    :group 'imaxima
    :type '(choice (color)
		   (const :tag "None" nil)))

(defcustom imaxima-fg-color nil
    "Foreground color of imaxima buffer."
    :group 'imaxima
    :type '(choice (color)
		   (const :tag "None" nil)))

(defcustom imaxima-latex-preamble ""
  "*String inserted at the start of the document preamble.
This can be used to change, say, the document font.
E.g. `\\usepackage{concrete}' will use the Euler math fonts."
  :group 'imaxima
  :type '(string))

(defcustom imaxima-max-scale 0.85
  "Maximum amount of scaling allowed to fit wide equations in the buffer.
nil means no scaling at all, t allows any scaling."
  :group 'imaxima
  :type 'number)

(defcustom imaxima-linearize-flag t
  "Non-nil means that equations too wide to fit in the buffer are linearized."
  :type '(boolean)
  :group 'imaxima)

(defcustom imaxima-use-maxima-mode-flag nil
  "Non-nil means that the major mode from `maxima.el' is used."
  :type '(boolean)
  :group 'imaxima)

(defcustom imaxima-maxima-program "maxima"
  "Maxima executable."
  :group 'imaxima
  :type '(string))


(defcustom imaxima-initex-option "-ini"
  "Option passed to TeX to start initex."
  :group 'imaxima
  :type '(string))

(defcustom imaxima-tex-program "latex"
  "TeX executable."
  :group 'imaxima
  :type '(string))

(defcustom imaxima-gs-program "gs"
  "Ghostscript executable."
  :group 'imaxima
  :type '(string))

(defcustom imaxima-gs-options '("-q" "-dNOPAUSE"
				"-dSAFER"
				"-dDELAYSAFER"
				"-DNOPLATFONTS" "-dTextAlphaBits=4"
				"-dGraphicsAlphaBits=4")
  "Options passed to gs for conversion from EPS."
  :group 'imaxima
  :type '(repeat string))

(defcustom imaxima-dvips-program "dvips"
  "Dvips executable."
  :group 'imaxima
  :type '(string))

(defcustom imaxima-dvips-options '("-E" "-R")
  "Options passed to dvips for conversion from DVI to EPS."
  :group 'imaxima
  :type '(repeat string))

(defcustom imaxima-tmp-dir
  (cond ((featurep 'xemacs)
	 (temp-directory))
	((eql system-type 'cygwin)
	 "/tmp/")
	(t temporary-file-directory))
  "*Directory used for temporary TeX and image files."
  :type '(directory)
  :group 'imaxima)

(defcustom imaxima-startup-hook nil
  "A hook called at startup.
This hook is called after imaxima has started Maxima."
  :group 'imaxima
  :type 'hook)

(defcustom imaxima-exit-hook nil
  "Hook called when exiting imaxima."
  :group 'imaxima
  :type 'hook)

(defvar imaxima-tmp-subdir ""
  "Subdirectory for temporary files.")

(defcustom imaxima-lisp-file 
  (if (eq system-type 'windows-nt)
      (imaxima-subst-char-in-string ?\\ ?/ (locate-library "imaxima.lisp"))
    (locate-library " /home/ike/repository/lisp/el/math/imaxima.lisp"))
  "Location of `imaxima.lisp'."
  :group 'imaxima
  :type '(file))

(defcustom imaxima-lisp-file 
    "/home/ike/repository/lisp/el/math/imaxima.lisp"
  "Location of `imaxima.lisp'."
  :group 'imaxima
  :type '(file))


(defcustom imaxima-maxima-options
  (if (eq system-type 'windows-nt)
      "-eval (user::run)"
    (format "--preload-lisp=%s" imaxima-lisp-file))
  "Arguments passed to Maxima."
  :group 'imaxima
  :type '(string))

(defface imaxima-latex-error-face
  '((t (:foreground "Blue" :underline t)))
  "Face used for LaTeX errors."
  :group 'imaxima)

(defvar imaxima-image-creators
  '((postscript nil)
    (png ("-sDEVICE=png16m"))
    (jpeg ("-sDEVICE=jpeg"))
    (tiff ("-sDEVICE=tiffpack")))
  "Define functions for generating images.
Argument list is passed to gs.")

(defvar imaxima-resolution nil
  "Screen resolution where rendering started.
Cons-cell of x and y resolution, given in
dots per inch.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'imaxima-resolution)

(defvar imaxima-output ""
  "Accumulator for `imaxima-filter'.")

(defvar imaxima-gs-output ""
  "Accumulator for `imaxima-gs-filter'.")

(defvar imaxima-process nil)
(defvar imaxima-gs-process nil)
(defvar imaxima-gs-computing-p nil)
(defvar imaxima-gs-7.05-is-broken nil)

(defvar imaxima-error-map (make-sparse-keymap)
  "Keymap for mouse clicks on LaTeX errors.")

(defvar imaxima-old-bg-color nil
  "Old background color.")

(defvar imaxima-old-fg-color nil
  "Old foreground color.")

(defvar imaxima-file-counter 0
  "Counter used for naming temp files.")

;; This piece of TeX is `mylatex.ltx' by David Carlisle.  The license is:
;;
;; "There are no restrictions on the distribution or modification of
;; this file, except that other people should not attempt to alter
;; the master copy on the ctan archives."

(defconst imaxima-mylatex
"\\makeatletter\\let\\MYLATEXdocument\\document
\\let\\MYLATEXopenout\\openout\\def\\document{\\endgroup
{\\setbox\\z@\\hbox{\\normalfont% normal
{\\ifx\\large\\@undefined\\else\\large\\fi
\\ifx\\footnotesize\\@undefined\\else\\footnotesize\\fi}%
{\\bfseries\\itshape}% bold and bold italic
{\\itshape}\\ttfamily\\sffamily}}%
\\let\\document\\MYLATEXdocument\\let\\openout\\MYLATEXopenout
\\makeatother\\everyjob\\expandafter{\\the\\everyjob
\\begingroup\\listfiles\\expandafter\\MYLATEXcustomised\\@dofilelist
\\endgroup}\\@addtofilelist{.}\\catcode`\\\\=13\\relax
\\catcode`\\#=12\\relax\\catcode`\\ =9\\relax\\dump}
\\def\\openout#1 {\\g@addto@macro\\MYLATEXopens{\\immediate\\openout#1 }}
\\let\\MYLATEXopens\\@empty\\def\\MYLATEXbegin{\\begin{document}}
\\def\\MYLATEXcomment{mylatex}\\def\\MYLATEXcustomised#1#2#3\\typeout#4{%
\\typeout{CUSTOMISED FORMAT. Preloaded files:^^J\\@spaces\\@spaces.}#3}
{\\catcode`\\^^M=\\active\\catcode`\\/=0 %
/catcode`\\\\=13 /gdef\\{/catcode`/\\=0 /catcode`/^^M=13   /catcode`/%=9 ^^M}%
/long/gdef^^M#1^^M{/def/MYLATEXline{#1}%
/ifx/MYLATEXline/MYLATEXcomment/let/MYLATEXbegin/relax%
/let/MYLATEXline/relax/fi/ifx/MYLATEXline/MYLATEXbegin%
/catcode`/^^M=5/relax/let^^M/par/catcode`/#=6/relax%
/catcode`/%=14/relax/catcode`/ =10/relax%
/expandafter/MYLATEXopens/expandafter/MYLATEXbegin%
/else/expandafter^^M/fi}}\\expandafter\\input\\endinput%"
  "TeX code for dumping a format file.")

;;
;; Geometry
;;

(defun imaxima-get-geometry (buffer)
  "Transfer display geometry parameters from current display.
Those are put in local variable `imaxima-resolution'.  Calculation is done
in source buffer specified by BUFF."
  (let (res)
    (with-current-buffer buffer
      (setq res (cons (/ (* 25.4 (imaxima-display-pixel-width))
			 (imaxima-display-mm-width))
		      (/ (* 25.4 (imaxima-display-pixel-height))
			 (imaxima-display-mm-height)))))
    (setq imaxima-resolution res)))

(defun imaxima-get-window-width-xemacs ()
  "Return window width in mm.
XEmacs verson."
  (/ (* (window-text-area-pixel-width) (imaxima-display-mm-width))
     (imaxima-display-pixel-width)))

(defun imaxima-get-window-width-emacs ()
  "Return window width in mm.
Emacs version."
  (/ (* (- (window-width) 1) (frame-char-width))
     (/ (float (imaxima-display-pixel-width))
	(imaxima-display-mm-width))))

(defun imaxima-bp-to-mm (bp)
  "Convert PostScript big points to mm.  BP is size in big points."
  (* bp 0.352778))

(defun imaxima-color-to-rgb (str)
  "Convert color name STR to rgb values understood by TeX."
  (mapcar '(lambda (x) (/ x 65535.0)) (imaxima-color-values str)))

(defmacro imaxima-with-temp-dir (dir &rest body)
  "Change to DIR temporarily and execute BODY."
  (let ((wd (make-symbol "wd")))
    `(let ((,wd  default-directory))
       (cd ,dir)
       (unwind-protect
	   (progn
	     ,@body)
	 (cd ,wd)))))

;;
;; Gs stuff
;;

(defun imaxima-gs-filter (process str)
  "Set `imaxima-gs-computing-p' to t when gs is done."
  (setq imaxima-gs-output (concat imaxima-gs-output str))
  (when (string-match "GS\\(<[0-9+]\\)?>" imaxima-gs-output)
    (setq imaxima-gs-computing-p nil)
    (setq imaxima-gs-output "")))

(defun imaxima-gs-wait ()
  "Wait for gs to finish."
  (while (and imaxima-gs-computing-p
	      (eq (process-status imaxima-gs-process) 'run))
    (accept-process-output imaxima-gs-process 1)
))

(defun imaxima-start-gs ()
  "Start Ghostscript as an asynchronyous process."
  ;; Are we using the broken GNU Ghostscript 7.05?
  (setq imaxima-gs-7.05-is-broken
	(string-match "\\(GNU\\|ESP\\) Ghostscript 7.05"
		      (shell-command-to-string
		       (concat imaxima-gs-program " --help"))))
  (let* (output
	 (type (cadr (assq imaxima-image-type imaxima-image-creators)))
	 (gs-args (append imaxima-gs-options
			  type
			  (list (format "-r%gx%g" (car imaxima-resolution)
					(cdr imaxima-resolution))))))
    (when (processp imaxima-gs-process)
      (delete-process imaxima-gs-process))
    (setq imaxima-gs-computing-p t)
    (condition-case nil
	(setq imaxima-gs-process (apply 'start-process "imaxima-gs"
					" *imaxima gs output*"
					imaxima-gs-program gs-args))
      (error (error
	      "Sorry, Ghostscript could not be started.  Please check
that you have gs in your path or customize the value of
`imaxima-gs-program' (current values is \"%s\").
%s"
	      imaxima-gs-program
	      (if (imaxima-image-type-available-p 'postscript)
		  "If Ghostscript isn't installed you can set `imaxima-image-type' to `ps'."
		;; don't offer this advice in XEmacs, which doesn't support ps.
		""))))
    (set-process-filter imaxima-gs-process 'imaxima-gs-filter)
    (imaxima-gs-wait)
    (process-kill-without-query imaxima-gs-process nil)
    (unless (eq (process-status imaxima-gs-process) 'run)
      (setq output (shell-command-to-string (concat imaxima-gs-program " -h")))
      (cond
       ((null (string-match (car type) output))
	(error
	 "Your version Ghostscript does not appear to support the image type %s.
The command \"gs -h\" lists the available devices.
You can change the image type in `imaxima-image-type' or the device name
associated with an image type in `imaxma-image-creators'" (car type)))
       (t (error
	   "Some of the options passed to Ghostscript are probably not supported
by your version.  In particular \"-dTextAlphaBits=4\" and \"-dGraphicsAlphaBits=4\"
are not supported by gs 5.5 or earlier.  Please edit `imaxima-gs-options'"))))))

(defun imaxima-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME.
Returns a list of bounding box, width, and height."
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 1024 t)
    (goto-char (point-min))
    (when (search-forward-regexp "%%BoundingBox:\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)" nil t)
      (let ((bb
	    (vector
	     (floor (string-to-number (match-string 1)))
	     (floor (string-to-number (match-string 2)))
	     (ceiling (string-to-number (match-string 3)))
	     (ceiling (string-to-number (match-string 4))))))
      (list bb
	    (- (aref bb 2) (aref bb 0))
	    (- (aref bb 3) (aref bb 1)))))))

(defun imaxima-eps-scale (file bb scale)
  "Scale the eps image in FILE with factor SCALE.
BB is the bounding box of the image.  Returns a list of new bounding
box, width, and height."
  (multiple-value-bind (llx lly urx ury) (append bb nil)
    (let ((x (round (* (- urx llx) scale)))
          (y (round (* (- ury lly) scale)))
	  (buff (find-file-noselect file)))
      (unwind-protect
	  (with-current-buffer buff
	    (goto-char (point-min))
	    (search-forward "%%BoundingBox")
	    (delete-region (line-beginning-position) (line-end-position))
	    (insert (format "%%%%BoundingBox: 0 0 %d %d\n" x y))
	    (search-forward "%%EndComments")
	    (forward-line)
	    (insert "%%BeginProcSet: imaxima 1 0\ngsave\n")
	    (insert (format "%f %f translate\n"
			    (- (* llx scale))
			    (- (* lly scale))))
	    (insert (format "%f %f scale\n" scale scale))
	    (insert "%%EndProcSet\n")
	    (goto-char (point-max))
	    (insert "\ngrestore")
	    (save-buffer))
	(kill-buffer buff))
      (list (vector 0 0 x y) x y))))

(defun imaxima-latex ()
  "Convert Maxima buffer to LaTeX.
Thic command does not work in XEmacs."
  (interactive)
  (let (pos2 label (pos (make-marker))
	     (buf (generate-new-buffer "*imaxima-latex*"))
	     (oldbuf (current-buffer)))
    (set-buffer buf)
    (insert-buffer oldbuf)
    ;; Remove copyright notice
    (goto-char (point-min))
    (search-forward "(C1)" nil t 2)
    (search-backward "(C1)" nil t)
    (delete-region (point-min) (point))
    (goto-char (point-min))
    (insert "\\documentclass[leqno]{article}
\\usepackage{verbatim}
\\usepackage[cmbase]{flexisym}
\\usepackage{breqn}
\\setkeys{breqn}{compact}
\\newcommand{\\ifrac}[2]{\\frac{#1}{#2}}
\\newcommand{\\ifracd}[2]{\\frac{#1}{#2}}
\\newcommand{\\ifracn}[2]{\\frac{#1}{#2}}
\\newcommand{\\isubscript}[2]{{#1}_{#2}}
\\newcommand{\\iexpt}[2]{{#1}^{#2}}
\\newcommand{\\isqrt}[1]{\\sqrt{#1}}
\\begin{document}\n")
;;     (while (and (not (eobp))
;; 		(setq pos (next-single-property-change (point) 'display)))
;;       (goto-char pos)
;;       (insert "\\end{verbatim}\n\n")
;;       (setq pos (copy-marker (next-single-property-change (point) 'display)))
;;       (remove-text-properties (point) pos '(display nil))
;;       (setq pos2 (point))
;;       (re-search-forward "(\\([^)]*\\))")
;;       (setq label (match-string 1))
;;       (delete-region pos2 (point))
;;       (insert (format "\\begin{dmath}[number={%s}]\n" label))
;;       (goto-char pos)
;;       (insert "\\end{dmath}\n\n\\begin{verbatim}"))
;;     (goto-char (point-max))
;;     (insert "\n\\end{verbatim}\n\\end{document}")
    (while (not (eobp))
      (let* ((region-start (copy-marker (point)))
	     (region-end (copy-marker (next-single-property-change (point) 'display nil (point-max))))
	     (text-prop (get-text-property region-start 'display)))
	(if text-prop
	    (progn
	      (goto-char region-start)
	      (remove-text-properties region-start region-end '(display nil))
	      (goto-char region-end)
	      (goto-char region-start)
	      (re-search-forward "(\\([^)]*\\))")
	      (setq label (match-string 1))
	      (delete-region region-start (point))
	      (goto-char region-start)
	      (insert (format "\\begin{dmath}[number={%s}]\n" label))
	      (goto-char region-end)
	      (insert "\\end{dmath}\n\n"))
	  (progn
	    (goto-char region-start)
	    (insert "\\begin{verbatim}")
	    (goto-char region-end)
	    (insert "\\end{verbatim}\n\n")))))
    (insert "\n\\end{document}")
    (switch-to-buffer-other-window buf)
    (latex-mode)))

(defun imaxima-process-sentinel (process event)
  "Process sentinel for Maxima process."
  (message "Process %s %s" process event)
  (unless (eq (process-status process) 'run)
    (imaxima-clean-up)))

(defun imaxima-ps-to-image (psfilename filename bb width height)
  "Convert eps file PSFILENAME to a bitmap image file FILENAME.
BB is the bounding box for eps image.  WIDTH and HEIGHT are the
dimensions of the image."
  (setq imaxima-gs-computing-p t)
  (when (eq system-type 'windows-nt)
    (setq psfilename (imaxima-subst-char-in-string ?\\ ?/ psfilename))
    (setq filename (imaxima-subst-char-in-string ?\\ ?/ filename)))
  (process-send-string imaxima-gs-process
		       (format
			(if imaxima-gs-7.05-is-broken
			    "clear /imaxima-state save def \
<< /PageSize [%d %d] /PageOffset [%d %d] /OutputFile (%s) >> \
setpagedevice (%s) run imaxima-state restore\n"
			  "clear \
<< \
/PageSize [%d %d] /PageOffset [%d %d] /OutputFile (%s) \
>> setpagedevice [save] (%s) (r) file cvx \
systemdict /.runandhide known revision 700 ge and {.setsafe {.runandhide}} if \
stopped {handleerror quit} if count 1 ne {quit} if \
cleardictstack 0 get restore\n")
			width
			height
			(- (aref bb 0))
			(aref bb 1)
			filename
			psfilename))
  (imaxima-gs-wait))

(defun imaxima-make-image (str)
  "Make image from STR."
  (let* ((filename (expand-file-name
		    (number-to-string (incf imaxima-file-counter))
		    imaxima-tmp-subdir))
	 (psfilename (concat filename ".ps"))
	 (label "*"))
    (when (string-match "\\(\\([^]*\\)\\)" str)
      (setq label (match-string 2 str))
      (setq str (replace-match "" t t str 1)))
    (imaxima-tex-to-dvi str label (concat filename ".tex"))
    (imaxima-dvi-to-ps filename)
    (if (not (file-exists-p psfilename))
	(imaxima-latex-error str filename)
      (multiple-value-bind (bb  width height)
	    (imaxima-extract-bb psfilename)
	  (let ((ratio (/  (imaxima-get-window-width)
			   (imaxima-bp-to-mm width))))
	    (when (< ratio 1.0)
	      ;; image is wider than the buffer
	      (if (and imaxima-max-scale
			 (or (eq imaxima-max-scale t)
			     (> ratio imaxima-max-scale)))
		  ;; scale image
		  (multiple-value-setq (bb width height)
		    (imaxima-eps-scale psfilename bb ratio))
		(when imaxima-linearize-flag
		  ;; linearize image
		  (imaxima-tex-to-dvi str label (concat filename ".tex") t)
		  (imaxima-dvi-to-ps filename)
		  (multiple-value-setq (bb width height)
		    (imaxima-extract-bb psfilename))))))
	  (unless (eq imaxima-image-type 'postscript)
	    (imaxima-ps-to-image psfilename filename bb width height))
	  (cond ((featurep 'xemacs)
		 (when (eq system-type 'windows-nt)
		   ;;(setq filename (imaxima-subst-char-in-string ?\\ ?/ filename))
		   ;; FIXME:
		   ;; Ghostscript on Windows doesn't flush the image to the file.
		   ;; So we have to kill the process and restart.  What a kludge!
		   (kill-process imaxima-gs-process)
		   (imaxima-start-gs))
		 (setq str (concat " " str))
		 (set-text-properties 0 (length str)
				      `(begin-glyph
					,(make-glyph (vector imaxima-image-type
							     :file filename))) str)
		 (add-text-properties 1 (length str) '(invisible t) str)
		 str)
		(t
		 (propertize (concat "(" label ") " str) 'display
			     (if (eq imaxima-image-type 'postscript)
				 (create-image psfilename
					       'postscript nil
					       :pt-width width :pt-height height
					       :bounding-box bb :ascent 'center
					       :mask '(heuristic (color-values imaxima-bg-color)))
			       (create-image filename
					     imaxima-image-type nil
					     :ascent 'center
					     :mask '(heuristic
						     (color-values imaxima-bg-color)))))))))))


(defun imaxima-latex-error (str filename)
  "Make clickable error message.
STR is offending LaTeX expression.  FILENAME is name of the LaTeX file."
  (let* ((msg "LaTex error in: ")
	(delim (if (featurep 'xemacs)
		   "; " "\n"))
	imaxima-error-2
	imaxima-error-3
	(error-text (concat "mouse-2: view LaTeX error log" delim
					     "mouse-3: view LaTeX source")))
    (fset 'imaxima-error-2
	  `(lambda ()
	     (interactive)
	     (view-file-other-window (concat ,filename ".log"))))
    (fset 'imaxima-error-3
	  `(lambda ()
	     (interactive)
	     (view-file-other-window (concat ,filename ".tex"))))
    (define-key imaxima-error-map imaxima-mouse2 'imaxima-error-2)
    (define-key imaxima-error-map [(return)] 'imaxima-error-2)
    (define-key imaxima-error-map imaxima-mouse3 'imaxima-error-3)
    (define-key imaxima-error-map [(meta return)] 'imaxima-error-3)
    (set-text-properties 0 14 `(face imaxima-latex-error-face
				     mouse-face highlight
				     help-echo ,error-text
				     keymap ,imaxima-error-map)
			 msg)
    (concat msg str)))


(defun imaxima-dump-tex ()
  "Dump a TeX format file preloaded with the required packages."
  (with-temp-file (expand-file-name "mylatex.ltx" imaxima-tmp-subdir)
    (insert imaxima-mylatex))
  (with-temp-file (expand-file-name "format.tex" imaxima-tmp-subdir)
    (insert
     ;;"\\batchmode\n"
     (format "\\documentclass[%dpt,leqno]{article}\n" imaxima-pt-size)
     imaxima-latex-preamble
     "\\usepackage{color}\n"
     "\\usepackage{exscale}\n"
     "\\usepackage[cmbase]{flexisym}\n"
     "\\usepackage{breqn}\n"
     "\\setkeys{breqn}{compact}\n"
     "\\setlength{\\textheight}{200cm}\n"
     ;; define \boxed from amsmath.sty
     "\\makeatletter
      \\newcommand{\\boxed}[1]{\\fbox{\\m@th$\\displaystyle#1$}}
\\newcommand{\\operatorname}[1]{%
\\mathop{\\relax\\kern\\z@\\operator@font{#1}}}
      \\makeatother
      \\newcommand{\\ifrac}[2]{\\frac{#1}{#2}}
      \\newcommand{\\ifracd}[2]{\\frac{#1}{#2}}
      \\newcommand{\\ifracn}[2]{\\frac{#1}{#2}}
      \\newcommand{\\isubscript}[2]{{#1}_{#2}}
      \\newcommand{\\iexpt}[2]{{#1}^{#2}}
      \\newcommand{\\isqrt}[1]{\\sqrt{#1}}\n
      \\nofiles
      \\begin{document}
      \\end{document}"))
  (imaxima-with-temp-dir
   imaxima-tmp-subdir
   (apply 'call-process imaxima-tex-program nil nil nil
	  (list imaxima-initex-option "&latex" "mylatex.ltx"
		(format "\\input{%s}" "format.tex")))))

(defun imaxima-tex-to-dvi (str label filename &optional linear)
"Run LaTeX on STR.
Argument LABEL is used as equation label.  FILENAME is used for
temporary files.  Use linearized form if LINEAR is non-nil."
  (with-temp-file filename
    (insert
     ;;"\\batchmode\n"
     (format "\\documentclass[%dpt,leqno]{article}\n" imaxima-pt-size)
     "\n% mylatex\n"
     (format "\\setlength{\\textwidth}{%dmm}\n"
	     (round (/ (imaxima-get-window-width)
		       imaxima-scale-factor)))
     (if linear
	 (concat
	  ;; braces in both denominator and numerator
	  "\\renewcommand{\\ifrac}[2]{\\left(#1\\right)/\\left(#2\\right)}"
	  ;; only braces denominator
	  "\\renewcommand{\\ifracd}[2]{#1/\\left(#2\\right)}"
	  ;; only braces in numerator
          "\\renewcommand{\\ifracn}[2]{\\left(#1\\right)/#2}"
          "\\renewcommand{\\isubscript}[2]{\\mathrm{subscript}\\left(#1,#2\\right)}"
          "\\renewcommand{\\iexpt}[2]{\\mathrm{expt}\\left(#1,#2\\right)}"
	  "\\renewcommand{\\isqrt}[1]{\\left(#1\\right)^{1/2}}\n")
       "")
     "\\begin{document}\n"
     (apply 'format "\\pagecolor[rgb]{%f,%f,%f}"
	    (imaxima-color-to-rgb (imaxima-get-bg-color)))
     "\\pagestyle{empty}\n"
     (format "\\begin{%s}\n" imaxima-fnt-size)
     (apply 'format "\\color[rgb]{%f,%f,%f}"
	    (imaxima-color-to-rgb imaxima-label-color))
     "\\tt"
     (format "\\begin{dmath}[number={%s}]\n" label)
     (apply 'format "\\color[rgb]{%f,%f,%f}"
	    (imaxima-color-to-rgb imaxima-equation-color))
     str "\\end{dmath}"
     (format "\\end{%s}" imaxima-fnt-size)
     "\\end{document}"))
    (imaxima-with-temp-dir imaxima-tmp-subdir
      (apply 'call-process imaxima-tex-program nil nil nil
	     (list "&mylatex" filename))))

(defun imaxima-dvi-to-ps (filename)
  "Convert dvi file FILENAME to PostScript."
  (let ((dvips-args (append
		     imaxima-dvips-options
		     (list "-x" (format "%s" (* imaxima-scale-factor 1000))
			   "-y" (format "%s" (* imaxima-scale-factor 1000))
			   (concat filename ".dvi") "-o"))))
    (imaxima-with-temp-dir imaxima-tmp-subdir
      (apply 'call-process imaxima-dvips-program nil nil nil dvips-args))))

(defun imaxima-clean-up ()
  "Kill gs process, delete temporary files and restore colors if applicable."
  (interactive)
  (ignore-errors
    (kill-process imaxima-gs-process))
  (mapc 'delete-file (directory-files imaxima-tmp-subdir t "^[^.].*"))
  (delete-directory imaxima-tmp-subdir)
  (if (featurep 'xemacs)
      (ad-deactivate 'comint-output-filter)
    ;; restore frame colors in Emacs
    (when imaxima-fg-color
      (modify-frame-parameters
       nil (list (cons 'foreground-color imaxima-old-fg-color))))
    (when imaxima-bg-color
      (modify-frame-parameters
       nil (list (cons 'background-color imaxima-old-bg-color)))))
  (run-hooks 'imaxima-exit-hook))

(defun imaxima-filter (str)
  "Parse output from Maxima and make image from TeX parts.
Argument STR contains output received from Maxima."
;;; Uncomment to debug:
;;;  (with-current-buffer (get-buffer-create "*imaxima-work*")
;;;   (insert str))
  (let* ((len (length str)))
    (if (zerop len)
	""
      (setq imaxima-output (concat imaxima-output str))
      (let ((lastchar (aref str (1- len))))
	(when (and (char-equal lastchar ?\n) (> len 1))
	  (setq lastchar (aref str (- len 2))))
	(cond
	 ;; Plain text
	 ((string-match "\\`[^]+\\'" imaxima-output)
	  (prog1 imaxima-output
	    (setq imaxima-output "")))
	 ((char-equal lastchar ?)
	  (string-match "\\([^]*\\)\\([^]*\\)$" imaxima-output)
	  (let ((prompt (concat "" (match-string 2 imaxima-output)))
		(output "")
		(rest (match-string 1 imaxima-output))
		text match)
	    (message "Processing Maxima output...")
	    (while (string-match "\\(\\([^]*\\)\\([^]*\\)\\)"
				 rest)
	      (setq text (match-string 2 rest))
	      (setq match (match-string 3 rest))
	      (setq rest (replace-match "" t t rest 1))
	      (setq output (concat output text (imaxima-make-image match))))
	    (setq imaxima-output "")
	    (message "Processing Maxima output...done")
	    (concat output rest prompt)))
	 ;; Special prompt, question.
	 ((char-equal lastchar ?)
	  (string-match "\\([^]*\\)" imaxima-output)
	  (prog1 (imaxima-make-image (match-string 1 imaxima-output))
	    (setq imaxima-output "")))
	 (t ""))))))

(eval-when-compile
  (ignore-errors
    (require 'maxima)))

(defun imaxima-setup-preoutput-filter ()
  "Set up `comint-preoutput-filter-functions' or the equivalent."
  (cond ((featurep 'xemacs)
	 ;; XEmacs does not have comint-preoutput-filter-functions, so
	 ;; we have to advice comint-output-filter instead
	 (defadvice comint-output-filter (before preoutput-filter)
	   "Run comint-preoutput-filter-functions."
	   (ad-set-arg 1 (imaxima-filter (ad-get-arg 1))))
	 (ad-activate 'comint-output-filter))
	(t
	 (make-local-variable 'comint-preoutput-filter-functions)
	 ;; This doesn't work due to a bug in comint.el
	 ;; (add-hook 'comint-preoutput-filter-functions 'imaxima-filter nil t)
	 (add-hook 'comint-preoutput-filter-functions 'imaxima-filter t))))

(defun imaxima-change-color (buf)
  "Change background and foreground color if applicable.
BUF is imaxima buffer."
  (cond
   ((featurep 'xemacs)
    (when imaxima-bg-color
      (set-face-background 'default imaxima-bg-color buf))
    (when imaxima-fg-color
      (set-face-foreground 'default imaxima-fg-color buf)))
   (t
    (when imaxima-bg-color
      (setq imaxima-old-bg-color (frame-parameter nil 'background-color))
      (modify-frame-parameters
       nil (list (cons 'background-color imaxima-bg-color))))
    (when imaxima-fg-color
      (setq imaxima-old-fg-color (frame-parameter nil 'foreground-color))
      (modify-frame-parameters
       nil (list (cons 'foreground-color imaxima-fg-color)))))))

(defun imaxima-setup ()
  "Image support for maxima.el."
  (let ((mbuf (process-buffer inferior-maxima-process)))
    (with-current-buffer mbuf
      (imaxima-change-color mbuf)
      (imaxima-get-geometry mbuf)
      (imaxima-dump-tex)
      (unless (eq imaxima-image-type 'postscript)
	(imaxima-start-gs))
      (add-hook 'kill-buffer-hook 'imaxima-clean-up t t)
      (imaxima-setup-preoutput-filter)
      (maxima-single-string
       (format "block(load(\"%s\"), linenum:0)$\n" imaxima-lisp-file))
      (goto-char (point-max)))))

(defun imaxima ()
  "Image support for Maxima.
\"display2d:true\" in Maxima turns images off, \"display2d:imaxima\"
turns them on.  Set `imaxima-use-maxima-mode-flag' to t to use
`maxima.el'."
  (interactive)
  (unless (imaxima-image-type-available-p imaxima-image-type)
    (error "Your version of Emacs does not support the image type %s"
	   imaxima-image-type))
  (unless imaxima-lisp-file
    (error "The file imaxima.lisp could not be found.
Please customize the option `imaxima-lisp-file'."))
  (setq imaxima-file-counter 0)
  (make-directory
   (setq imaxima-tmp-subdir
	 ;; For some reason TeX doesn't grok underscores in file names
	 (imaxima-subst-char-in-string ?_ ?=
	    (make-temp-name (expand-file-name "imaxima" imaxima-tmp-dir)))))
  (set-file-modes imaxima-tmp-subdir 448) ; 700 in octal
  (let ((process-connection-type process-connection-type-flag))
    (if imaxima-use-maxima-mode-flag
	(progn
	  (add-hook 'inferior-maxima-mode-hook 'imaxima-setup t)
	  (maxima)
	  (remove-hook 'inferior-maxima-mode-hook 'imaxima-setup))
      (setq imaxima-output "")
      (let ((mbuf
	     (apply 'make-comint
	      "imaxima"
	      imaxima-maxima-program
	      nil
	      (split-string
	       imaxima-maxima-options))))
	(save-excursion
	  (set-buffer mbuf)
	  (setq imaxima-process (get-buffer-process mbuf))
	  (imaxima-get-geometry mbuf)
	  (imaxima-change-color mbuf)
	  (imaxima-dump-tex)
	  (set-process-sentinel imaxima-process 'imaxima-process-sentinel)
	  (imaxima-setup-preoutput-filter)
	  (unless (eq imaxima-image-type 'postscript)
	    (imaxima-start-gs)))
	(when (eq system-type 'windows-nt)
	  (comint-send-string
	   mbuf
	   (format "block(load(\"%s\"), linenum:0)$\n" imaxima-lisp-file)))
      (switch-to-buffer mbuf))))
  (run-hooks 'imaxima-startup-hook))

(provide 'imaxima)

;;; imaxima.el ends here
