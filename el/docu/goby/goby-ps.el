; Goby: goby-ps.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug 29, 2003

;;; Commentary:

;; Home page: http://www.mew.org/~kazu/proj/goby/

;;; Code:

(require 'goby)
(require 'goby-view)

(defvar goby-ps-eps-position nil)
(defvar goby-ps-have-eps nil)

(mapcar 'make-variable-buffer-local
	(list 'goby-ps-eps-position
	      'goby-ps-have-eps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands
;;;

;;;###autoload
(defun goby-make-ps ()
  "Create a PhostScript file."
  (interactive)
  (let* ((file (read-file-name "PS file: "))
	 (goby-buf (current-buffer))
	 (name (buffer-name goby-buf))
	 (i 1) ps-buf doit jp)
    (if (file-exists-p file)
	(if (file-directory-p file)
	    (message "%s is a directory" file)
	  (if (y-or-n-p (format "%s exists. Overwrite? " file))
	      (setq doit t)))
      (setq doit t))
    (when doit
      (setq jp (member 'japanese-jisx0208
		       (find-charset-region (point-min) (point-max))))
      (save-excursion
	(with-temp-buffer
	  (setq ps-buf (current-buffer))
	  (goby-ps-header name)
	  (goby-ps-def)
	  (if jp (goby-ps-defj))
	  (setq goby-ps-eps-position (point))
	  (set-buffer goby-buf)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (goby-narrow-to-page)
	    (goto-char (point-min))
	    (goby-ps-page ps-buf i)
	    (widen)
	    (forward-line)
	    (setq i (1+ i)))
	  (set-buffer ps-buf)
	  (goby-ps-tailer)
	  (when goby-ps-have-eps
	    (goto-char goby-ps-eps-position)
	    (goby-ps-eps-def))
	  (write-region (point-min) (point-max) file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header and Tailer
;;;

(defun goby-ps-header (name)
  (insert
   "%!PS-Adobe-" goby-ps-version "\n"
   "%%Title: " name "\n"
   "%%Creator: Goby\n"
   "%%CreationDate: " (format-time-string "%a, %d %b %Y %T %z (%Z)" (current-time)) "\n"
   "%%Pages: (atend)\n"
   "%%PageOrder: Ascend\n"
   "%%DocumentPaperSizes: A4\n"
   "%%Orientation: Landscape\n"
   "%%EndComments\n"))

(defun goby-ps-def ()
  (insert
   "\n%Definitions\n"
   "/inch {72 mul} def\n"
   "/A4-width   {8.27 inch} def\n"
   "/A4-height {11.69 inch} def\n"
   "/window-width  {8 inch} def\n"
   "/window-height {6 inch} def\n"
   "/screen-pixel-width "  (format "%d" (display-pixel-width))  " def\n"
   "/screen-pixel-height " (format "%d" (display-pixel-height)) " def\n"
   "/default-pixel-width "  (format "%d" goby-default-face-pixel-width) " def\n" 
   "/default-pixel-height " (format "%d" goby-default-face-pixel-height) " def\n"
   "/left-fringe-pixel " (format "%d" goby-ps-left-fringe-pixel-magic-number) " def\n" 
   "/default-width {\n"
   "    window-width default-pixel-width mul screen-pixel-width div\n"
   "} def\n"
   "/default-height {\n"
   "    window-height default-pixel-height mul screen-pixel-height div\n"
   "} def\n"
   "/left-fringe {\n"
   "    window-width left-fringe-pixel mul screen-pixel-width div\n"
   "} def\n"
   "/PERCENT {window-height mul 100 div} def\n"
   "/default-font {/Courier findfont default-height scalefont setfont} def\n"
   "/FRAME { newpath\n"
   "    0 0 moveto\n"
   "    0 window-height neg rlineto\n"
   "    window-width 0 rlineto\n"
   "    0 window-height rlineto\n"
   "    closepath\n"
   "    " (format "%d" goby-ps-frame) " setlinewidth\n"
   "    stroke\n"
   "} def\n"
   "/NEWPAGE {\n"
   "    A4-width  window-height sub 2 div\n"
   "    A4-height window-width  sub 2 div translate\n"
   "    90 rotate\n"
   "    FRAME\n"
   "    left-fringe 0 translate\n"
   "    0 0 moveto\n"
   "} def\n"
   "/NL {\n"
   "    currentpoint exch pop 0 exch moveto\n"
   "    0 exch neg " (format "%d" goby-ps-gap-magic-number) " sub rmoveto\n"
   "} def\n"
   "/NL2 {\n"
   "    currentpoint exch pop 0 exch moveto\n"
   "    0 exch neg rmoveto\n"
   "} def\n"
   "/SHOWFONT {\n"
   "    findfont exch PERCENT scalefont setfont show\n"
   "} def\n"
   "/CENTER {\n"
   "    default-width exch mul 0 rmoveto\n"
   "} def\n"
   "/BAR {\n"
   "    gsave currentpoint newpath\n"
   "    exch default-width 4 mul add\n"
   "    exch default-height 2 div add moveto\n"
   "    window-width left-fringe 2 mul sub default-width 8 mul sub 0 rlineto\n"
   "    " (format "%f" goby-ps-bar-gray-scale) " setgray "
   "    " (format "%d" goby-ps-bar-height) " setlinewidth\n"
   "    stroke grestore\n"
   "} def\n"
   "/ITEM {\n"
   "    gsave currentpoint newpath moveto\n"
   "    PERCENT\n"
   "    dup " (format "%d" goby-ps-item-base-ratio) " mul 100 div\n"
   "    dup rmoveto\n"
   "    dup " (format "%d" goby-ps-item-ratio) " mul 100 div\n"
   "    dup 0 exch rlineto\n"
   "    dup 0 rlineto\n"
   "    0 exch neg rlineto\n"
   "    closepath\n"
   "    " (format "%f" goby-ps-item-gray-scale) " setgray fill\n"
   "    stroke grestore\n"
   "   "  (format "%d" goby-ps-item-width-ratio) " mul 100 div 0 rmoveto\n"
   "} def\n"
   "/RAISE { PERCENT mul currentpoint exch pop exch 0 exch rmoveto } def\n"
   "/BAK { currentpoint pop exch moveto } def\n")
  (if goby-ps-use-bold
      (insert
       "/FH { /Helvetica-Bold SHOWFONT } def\n"
       "/FT { /Times-Bold SHOWFONT } def\n"
       "/FI { /Times-BoldItalic SHOWFONT } def\n"
       "/FC { /Courier-Bold SHOWFONT } def\n")
    (insert
     "/FH { /Helvetica SHOWFONT } def\n"
     "/FT { /Times-Roman SHOWFONT } def\n"
     "/FI { /Times-Italic SHOWFONT } def\n"
     "/FC { /Courier SHOWFONT } def\n")))

(defun goby-ps-eps-def ()
  (insert
   "/BeginEPSF {\n"
   "    /b4_Inc_state save def\n"
   "    /dict_count countdictstack def\n"
   "    /op_count count 1 sub def\n"
   "    userdict begin\n"
   "    /showpage {} def\n"
   "    0 setgray 0 setlinecap\n"
   "    1 setlinewidth 0 setlinejoin\n"
   "    10 setmiterlimit [ ] 0 setdash\n"
   "    currentpoint newpath translate\n"
   "    /languagelevel where\n"
   "    {pop languagelevel\n"
   "	     1 ne\n"
   "        {false setstrokeadjust\n"
   "         false setoverprint\n"
   "        }if\n"
   "    }if\n"
   "}bind def\n"
   "/EndEPSF {\n"
   "    count op_count sub\n"
   "    {pop} repeat\n"
   "    countdictstack dict_count sub\n"
   "    {end} repeat\n"
   "    b4_Inc_state restore\n"
   "}bind def\n"))


(defun goby-ps-defj ()
  (insert
   "/FG { /GothicBBB-Medium-H SHOWFONT } def\n"
   "/FM { /Ryumin-Light-H SHOWFONT } def\n"))

(defun goby-ps-tailer ()
  (insert "%%Trailer\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Page
;;;

(defun goby-ps-page (ps-buf i)
  (let ((goby-buf (current-buffer)))
    (set-buffer ps-buf)
    (insert
     "\n%%Page: 1 "
     (format "%d\n" i)
     "NEWPAGE\n")
    (set-buffer goby-buf)
    (while (not (eobp))
      (goby-ps-line ps-buf)
      (forward-line))
    (set-buffer ps-buf)
    (insert "showpage\n")
    (set-buffer goby-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line
;;;

(defun goby-ps-line (ps-buf)
  (let (lim next face prop max)
    (goby-ps-line-feed ps-buf)
    (end-of-line)
    (setq lim (point))
    (beginning-of-line)
    (while (and (not (eolp))
		(setq next (next-property-change (point) nil lim)))
      (cond
       ((setq prop (get-text-property (point) 'goby))
	(cond
	 ((eq prop 'bar)
	  (goby-ps-draw-bar ps-buf))
	 ((eq prop 'item)
	  (goby-ps-draw-item ps-buf (get-text-property (point) 'goby-item)))))
       ((setq prop (goby-get-extent (point)))
	(cond
	 ((goby-extent-space-p (point))
	  (goby-ps-center-line ps-buf prop))
	 ((goby-extent-image-p (point))
	  (setq max (goby-ps-insert-image ps-buf max)))
	 (t
	  (setq face (get-text-property (point) 'face))
	  (cond
	   ((eq face 'default) ;; xxx
	    (goby-ps-insert-default-text ps-buf next))
	   (t
	    (setq max (goby-ps-insert-text ps-buf next face max prop)))))))
       ((setq face (get-text-property (point) 'face))
	(cond
	 ((eq face 'default) ;; xxx checking tab only?
	  (goby-ps-insert-default-text ps-buf next))
	 (t
	  (setq max (goby-ps-insert-text ps-buf next face max))))))
      (goto-char next))
    (goby-ps-line-feed-fix ps-buf max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line functions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line feed
;;;

(defconst goby-ps-line-feed-magic-word "[[YYY]]")

(defun goby-ps-line-feed (ps-buf)
  (let ((goby-buf (current-buffer)))
    (set-buffer ps-buf)
    (insert goby-ps-line-feed-magic-word "\n")
    (set-buffer goby-buf)))

(defun goby-ps-line-feed-fix (ps-buf max)
  (let ((goby-buf (current-buffer)))
    (set-buffer ps-buf)
    (save-excursion
      (when (search-backward goby-ps-line-feed-magic-word)
	(delete-region (match-beginning 0) (match-end 0))
	(if max
	    (insert (format "%d PERCENT NL" max))
	  (insert "default-height NL2"))))
    (set-buffer goby-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;

(defun goby-ps-insert-default-text (ps-buf next)
  (let ((goby-buf (current-buffer))
	(str (buffer-substring-no-properties (point) next)))
    (setq str (make-string (* (length str) goby-ps-tab-width) 32))
    (set-buffer ps-buf)
    (insert "default-font (" str ") show\n")
    (set-buffer goby-buf)))

(defun goby-ps-get-hexstr (str offset)
  (let* ((ustr (string-as-unibyte str))
	 (len (length ustr))
	 (ret (list "< "))
	 (i 0))
    (while (< i len)
      (setq ret (cons (format "%02x " (- (aref ustr i) offset)) ret))
      (setq i (1+ i)))
    (setq ret (cons ">" ret))
    (apply 'concat (nreverse ret))))

(defun goby-ps-insert-text (ps-buf next face max &optional raise)
  (let ((goby-buf (current-buffer))
	(name (symbol-name face))
	family ratio ent psfont str italic)
    (when (string-match goby-face-regex name)
      (setq family (goby-get-family (goby-get-face-family name)))
      (setq ratio (goby-get-face-ratio name))
      (setq italic (goby-get-face-italic name))
      (if italic (setq family goby-math))
      (setq ent (assoc family goby-ps-font-alist))
      (setq psfont (nth 1 ent))
      (if (null psfont) (error "no psfont"))
      (setq str (buffer-substring-no-properties (point) next))
      (if (or (null max) (> ratio max)) (setq max ratio))
      (cond
       ((looking-at goby-ascii-regex)
	(if (string-match "[][()<>/%]" str)
	    (setq str (goby-ps-get-hexstr str 0))
	  (setq str (concat "(" str ")"))))
       ((memq 'latin-iso8859-1 (find-charset-string str))
	(setq str nil)) ;; xxx
       (t
	(setq str (goby-ps-get-hexstr
		   (encode-coding-string str 'euc-jp) 128))))
      (when str
	(set-buffer ps-buf)
	(if raise (insert (format "%d %f RAISE " ratio (nth 1 raise))))
	(insert (format "%s %d %s\n" str ratio psfont))
	(if raise (insert "BAK\n"))))
    (set-buffer goby-buf)
    max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image
;;;

(defun goby-ps-get-image-ratio ()
  (let* ((image (goby-get-extent (point)))
	 (xy-pixel (image-size image 'pixels)))
    (list
     (/ (* (truncate (car xy-pixel)) 100) (display-pixel-width))
     (/ (* (truncate (cdr xy-pixel)) 100) (display-pixel-height)))))

(defun goby-get-x-ratio (xy) (nth 0 xy))
(defun goby-get-y-ratio (xy) (nth 1 xy))

(defun goby-ps-insert-image (ps-buf max)
  (let ((goby-buf (current-buffer))
	file scale pixel-width 
	xy-ratio x-ratio y-ratio epsfile epsi)
    (when (looking-at goby-image-regex)
      (setq epsi nil)
      (setq file (goby-image-get-file))
      (setq scale (goby-image-get-scale))
      (setq epsfile (concat (file-name-sans-extension file)
			    goby-ps-epsfile-suffix))
      (setq xy-ratio (goby-ps-get-image-ratio))
      (setq x-ratio (goby-get-x-ratio xy-ratio))
      (setq y-ratio (goby-get-y-ratio xy-ratio))
      (set-buffer ps-buf)
      (cond
       ((file-readable-p epsfile)
	(setq epsi (goby-ps-load-eps epsfile x-ratio y-ratio)))
       ((null scale)
	(setq epsi (goby-ps-create-eps file x-ratio y-ratio)))
       (t
	(setq pixel-width (goby-scale-pixel-width scale))
	(setq epsi (goby-ps-create-eps file x-ratio y-ratio pixel-width))))
      (when epsi
	(setq goby-ps-have-eps t)
	(insert epsi)))
    (set-buffer goby-buf)
    (if (or (null max) (> y-ratio max)) y-ratio max)))

(defun goby-ps-create-eps (file x-ratio y-ratio &optional pixel-width)
  (let ((topnm (goby-get-topnm file)))
    (when (and (file-readable-p file)
	       topnm
	       (goby-which-exec topnm)
	       (goby-which-exec goby-prog-ppmtopgm)
	       (goby-which-exec goby-prog-pnmscale)
	       (goby-which-exec goby-prog-pnmtops))
      (with-temp-buffer
	(goby-image-safe
	 (set-buffer-multibyte nil)
	 (call-process topnm file '(t nil))
	 (call-process-region (point-min) (point-max) goby-prog-ppmtopgm
			      t '(t nil) nil)
	 (when pixel-width
	   (call-process-region (point-min) (point-max) goby-prog-pnmscale
				t '(t nil) nil
				"-xsize" pixel-width))
	 (call-process-region (point-min) (point-max) goby-prog-pnmtops
			      t '(t nil) nil
			      "-nocenter" "-noturn" "-nosetpage")
	 (goto-char (point-min))
	 (goby-ps-arrange-eps file x-ratio y-ratio)
	 (buffer-string))))))

(defun goby-ps-load-eps (epsfile x-ratio y-ratio)
  (with-temp-buffer
    (insert-file-contents epsfile)
    (goby-ps-arrange-eps epsfile x-ratio y-ratio)
    (buffer-string)))

(defun goby-ps-arrange-eps (file x-ratio y-ratio)
  (let (eps-width eps-height llx lly urx ury beg)
    (when (re-search-forward "BoundingBox: \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
      (setq llx (string-to-number (goby-match-string 1)))
      (setq lly (string-to-number (goby-match-string 2)))
      (setq urx (string-to-number (goby-match-string 3)))
      (setq ury (string-to-number (goby-match-string 4)))
      (setq eps-width  (- urx llx))
      (setq eps-height (- ury lly))
      (goto-char (point-min))
      (while (search-forward "%%PageBoundingBox:" nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	(delete-region beg (point)))
      (goto-char (point-min))
      (insert
       "BeginEPSF\n"
       (format "window-width  %d mul 100 div %s div " x-ratio eps-width)
       (format "window-height %d mul 100 div %s div " y-ratio eps-height)
       "scale\n"
       (format "-%d -%d translate\n" llx lly)
       "%%BeginDocument: " (file-name-nondirectory file) "\n")
      (goto-char (point-max))
      (if (bolp) (forward-line -1))
      (beginning-of-line)
      (unless (looking-at "%%EOF")
	(forward-line)
	(insert "%%EOF\n"))
      (goto-char (point-max))
      (when (search-backward "%%Trailer" nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	(delete-region beg (point)))
      (goto-char (point-max))
      (insert
       "%%EndDocument\n"
       "EndEPSF\n"
       (format
	"window-width %d mul 100 div 0 rmoveto\n" x-ratio)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun goby-ps-center-line (ps-buf prop)
  (let ((goby-buf (current-buffer))
	(width (nth 1 (member ':width prop))))
    (set-buffer ps-buf)
    (insert (format "%d CENTER\n" width))
    (set-buffer goby-buf)))

(defun goby-ps-draw-bar (ps-buf)
  (let ((goby-buf (current-buffer)))
    (set-buffer ps-buf)
    (insert "BAR\n")
    (set-buffer goby-buf)))

(defun goby-ps-draw-item (ps-buf num)
  (let* ((goby-buf (current-buffer))
	 (spec (goby-get-tab-spec num))
	 (ratio (goby-get-tab-ratio spec)))
    (set-buffer ps-buf)
    (insert (format "%d ITEM\n" ratio))
    (set-buffer goby-buf)))

(provide 'goby-ps)

;;; Copyright Notice:

;; Copyright (C) 2003 Kazu Yamamoto
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; goby-ps.el ends here
