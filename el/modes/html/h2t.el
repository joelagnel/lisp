;;; h2t.el --- Convert html to text

;; Copyright (C) 2001 Sean MacLennan
;; Revision:   1.3
;; XEmacs/Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; NOTES:
;; This program attempts to convert html to some form of readable text.
;; It is *heavily* oriented towards the output from the lxr program.
;; Also used for doxygen, slashdot, and my playlist.cgi script.
;; The h2t routines assume case-fold-search is t since html tags are
;; case insensitive.


(defvar h2t-keymap (make-sparse-keymap "h2t"))

;; SAM Should this be here?
(if (boundp 'running-xemacs)
    (progn
      (define-key h2t-keymap 'button1 'h2t-mousable)
      (define-key h2t-keymap 'button2 'h2t-mousable))
  (define-key h2t-keymap [mouse1] 'h2t-mousable)
  (define-key h2t-keymap [mouse2] 'h2t-mousable))
(define-key h2t-keymap "g" 'h2t-mousable)

;; SAM ditto
(unless (facep 'blue)
  (make-face 'blue)
  (set-face-foreground 'blue "blue"))
(unless (facep 'highlight)
  (make-face 'highlight)
  (set-face-foreground 'highlight "darkseagreen2"))

;;;###autoload
(defun html-to-text (buffer &optional keymap)
  "Attempt to convert an html file to text.
Ok, ok, I admit that the optional keymap arg is hokey. If the keymap is
specified, all anchors are surrounded by an extent. The anchor is stored
in the extent as an `anchor' property."
  (interactive "*bBuffer to convert: \nP")
  (save-excursion
    (set-buffer buffer)
    (when (and (buffer-modified-p) (y-or-n-p "Save changes to buffer? "))
      (save-buffer))
    (let ((case-fold-search t))
      (h2t-strip-forms)
      (h2t-strip-divs)
      (if keymap
	  (if (keymapp keymap)
	      (h2t-anchors-to-extents keymap)
	    (h2t-anchors-to-extents h2t-keymap))
	(h2t-anchors-to-extents))
       (h2t-handle-lists)
       (h2t-strip-tags)
       (h2t-handle-headers)
       (h2t-strip-special)
       )))


;; Forms almost always look wrong!
(defun h2t-strip-forms ()
  (let (start)
    (goto-char (point-min))
    (while (re-search-forward "<form" nil t)
      (setq start (match-beginning 0))
      (re-search-forward "</form>") ;; fail if not found
      (if (eolp) (forward-char))
      (delete-region start (point)))))


;; For lxr, we need to remove the <div> sections since they look
;; terrible.
(defun h2t-strip-divs ()
  (let (start)
    (goto-char (point-min))
    (while (re-search-forward "<div" nil t)
      (setq start (match-beginning 0))
      (re-search-forward "</div>") ;; fail if not found
      (if (eolp) (forward-char))
      (delete-region start (point)))))


;; A very simple list handler. We handle the following tags:
;;    <ul>|<ol>   increment indentation level
;;    <li>        replace with indentation
;;    </ul>|</ol> decrement indentation level
(defun h2t-handle-lists ()
  (let ((indent ""))
    (goto-char (point-min))
    (while (re-search-forward "<ul>\\|<ol>\\|<li>\\|</ul>\\|</ol>" nil t)
      (goto-char (match-beginning 0))
      (cond ((looking-at "<ul>\\|<ol>")
	     (replace-match "")
	     (setq indent (concat indent "\t")))
	    ((looking-at "</ul>\\|</ol>")
	     (replace-match "")
	     (setq indent (substring indent 1)))
	    ((looking-at "<li>")
	     (replace-match indent))))))


;; Strip all tags *except* headers and titles.
;; This is extremely brain dead to start.
;; If the entire line was a tag, delete the NL.
(defun h2t-strip-tags ()
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (goto-char (match-beginning 0))
    (cond ((looking-at "</?h[1-6]")
	   (goto-char (match-end 0)))
	  ;; convert titles to headers for later
	  ((looking-at "\\(</?\\)title>")
	   (replace-match (concat (match-string 1) "h1>")))
	  (t
	   (replace-match "")
	   (and (eolp) (bolp) (not (eobp)) (delete-char 1))))))


;; We add a nice extent around headers.
;; We do this *after* all tags stripped so we can guarantee some minimum
;; spacing after headers.
(defun h2t-handle-headers ()
  (let (start extent)
    (goto-char (point-min))
    (while (re-search-forward "<h[^>]+>" nil t)
      (replace-match "")
      (setq start (point))
      (re-search-forward "</h[1-6]>" nil t)
      (replace-match "")
      (setq extent (make-extent start (point)))
      (set-extent-face extent 'bold)
      (end-of-line)
      (unless (looking-at "\n[ \t]*\n") (insert "\n"))
      )))

(defun h2t-make-extent (start end anchor &optional keymap buff)
  (let ((extent (make-extent start end buff)))
    (if keymap
	(progn
	  (set-extent-face extent 'blue)
	  (set-extent-mouse-face extent 'highlight)
	  (set-extent-keymap extent keymap))
      (set-extent-face extent 'red))
    (set-extent-property extent 'anchor anchor)
    extent))

;; We add a nice extent around anchors
;; We do this *after* all tags stripped so we can guarantee some minimum
;; spacing after headers.
;; This version handles tags in anchors.
(defun h2t-anchors-to-extents (&optional keymap)
  (let (start end anchor)
    (goto-char (point-min))
    (while (re-search-forward "<a [^>]+>" nil t)
      (setq anchor (match-string 0))
      (setq start (match-end 0))
      (unless (re-search-forward "</a>" nil t)
	(error "Malformed anchor. No </a> tag."))
      (setq end (match-beginning 0))
      (when (string-match "href=\"\\([^\"]+\\)\"" anchor)
	(h2t-make-extent start end (match-string 1 anchor) keymap)))))

;; Remove nbsps for doxygen
(defun h2t-strip-special ()
  (goto-char (point-min))
  (while (re-search-forward "&[a-z]+;" nil t)
      (if (string= (match-string 0) "&copy;")
	  (replace-match "(c)")
	(replace-match " "))))

(defun h2t-mousable (event)
  "This is called on a mouse click if the user did not specify a keymap.
This version can only handle urls."
  (interactive "e")
  (let* ((extent (extent-at (event-point event)
			    (event-buffer event)))
	 (anchor (extent-property extent 'anchor)))
    (message "%s" anchor)
    (browse-url anchor)))

(provide 'h2t)
