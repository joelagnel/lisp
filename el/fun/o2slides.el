;; o2slides.el -- Snarf gobblebits and grind bones to magic dust

;; Author: Daniel Lundin <daniel@codefactory.se>
;; FTP: ftp://ftp.codefactory.se/pub/people/daniel/o2slides.el

;; Copyright 2001 Seven relatively short dudes and a pale chick
;;
;; This software is released under Disgruntled Catholic License v6.66
;; Redistribution prohibited without authorization from the pope.
;; Under special circumstances under which should such authorization 
;; request be denied or otherwise unable to achieve, redistribution may be
;; authorized by the reader herself.

;;; Commentary:

;; Horrendous Hacks & Terrible Kludges Inc presents yet another blockbuster

;; THE FANTASTIC OUTLINE TO W3SLIDES MUNCHER!!!

;; Usage: o2slides in a properly formatted outline buffer:

;; * Chapter
;;   [Some comment]
  
;;   - Slide
;;     = Item
;;     = Item
;;     [Some comment]

;;   - Slide Two
;;   [Some comment]
;;     = Item
;;     [Some comment]
;;       + Subitem One
;;       + Subitem Two
;;       [Some comment]

;;; Code:

(require 'time-stamp)

;; Variables

(defconst o2slides-version "1.0")

(defvar o2slides-file-name-prefix "slide"
  "Filename prefix for created slides")

(defvar o2slides-default-file-dir-name "/tmp"
  "*Directory where to create slides")

(defvar o2slides-file-dir-name nil
  "Directory where to create slides")


(defvar o2slidesslide-buffers-stay-open t
  "*Buffers should remain open in emacs after creation ")


;; Functions

(defun o2slides-item-end (marker) 
  "Return point at end of current item."
  (let ((result (min (or (save-excursion (search-forward-regexp
					  (concat "^[ ]*" 
						  (regexp-quote marker) " .*") 
					  nil t))
			 (point-max))
		     (or (save-excursion (search-forward-regexp 
					  (concat "^[ ]*\\* .*") nil t))
			 (point-max)))))
    (- result (if (eq result (point-max)) 0 2))))


(defun o2slides-snarf-commentary (marker)
  "Google after [foo] commentary."
  (and 
   (setq beg (search-forward "[" (point-at-eol) t))
   (setq after (skip-chars-forward "^]" (and marker 
					     (o2slides-item-end marker))))
   (buffer-substring-no-properties beg (+ beg after))))


(defun o2slides-snarf-one (marker-string lim)
  "Snarf an item using MARKER-REGEXP."
  (and marker-string
       (let ((item (and (search-forward-regexp (concat "^[ ]*" (regexp-quote
								marker-string)
						       " \\(.*\\)\n")
					       (and lim (o2slides-item-end 
							 lim)) 
					       t)
			(match-string-no-properties 1)))
	     (commentary (o2slides-snarf-commentary lim)))
	 (if (or item commentary)
	     (list (cons item commentary))
	   nil))))


(defun o2slides-snarf-sub-sub-item ()
  "Snarf sub sub item"
  (let ((the-sub-sub-item (o2slides-snarf-one "=" "-"))
      subsubsubitems subsubsubitem)
    (if (not the-sub-sub-item) nil
	(while (setq subsubsubitem (o2slides-snarf-one "+" "="))
	  (setq subsubsubitems (if subsubsubitems
				   (append subsubsubitems (list subsubsubitem))
				 (list subsubsubitem))))
	(reverse (cons subsubsubitems the-sub-sub-item)))))


(defun o2slides-snarf-sub-item ()
  "Snarf sub item"
  (let ((the-sub-item (o2slides-snarf-one "-" "*"))
	subsubitems subsubitem)
    (if (not the-sub-item) nil
      (while (setq subsubitem (o2slides-snarf-sub-sub-item))
	(setq subsubitems (if subsubitems
			      (append subsubitems (list subsubitem))
			    (list subsubitem))))
      (reverse (cons subsubitems the-sub-item)))))


(defun o2slides-snarf-item ()
  "Snarf item"
  (let ((the-item (o2slides-snarf-one "*" nil))
	subitems subitem)
    (if (not the-item) nil
      (while (setq subitem (o2slides-snarf-sub-item))
	(setq subitems (append subitems (list subitem))))
      (reverse (cons subitems the-item)))))

(defun o2slides-snarf ()
  "Snarf it"
  (interactive)
; (save-excursion
    (beginning-of-buffer)
    (let ((items nil))
;      (while (setq the-item (o2slides-snarf-item))
;	(setq items (append items the-item)))
      (setq items (append items (list (o2slides-snarf-item))))
      (setq foo-item items)))



;; Output functions

(defun o2slides-new-buffer (name prev next title subtitle)
  "Create a new buffer for slide"
  (let ((file-name (concat o2slides-file-dir-name "/" name))
	slide-buffer)
    (if (and (file-exists-p file-name)	    
	     (not (y-or-n-p (concat "File '" file-name 
				    "' exists, overwrite? "))))
	(error "User aborted")
      (setq slide-buffer (find-file file-name))
      (erase-buffer)
      (insert
       "<?xml version=\"1.0\"?>\n"
       "<!-- " (time-stamp-string) "\n"
       "Created by " user-full-name " <" user-mail-address ">\n"
       "using o2slides.el " o2slides-version 
       ", written by Daniel Lundin <daniel@codefactory.se>\n"
       "-->"
       "\n"
       "<slide>\n"
       " <head>\n"
       (concat 
	(and title (concat "   <title>" title "</title>\n"))
	(and subtitle (concat "   <subtitle>" subtitle "</subtitle>\n"))
	(and prev (concat "   <prev>" prev "</prev>\n"))
	(and next (concat "   <next>" next "</next>\n")))
       " </head>\n\n"))
  slide-buffer))


(defun o2slides-slide-name (id)
  "Construct a slide filename"
  (concat o2slides-file-name-prefix (format "%03i" id) ".xml"))

(defun o2slides-xml-comment (str)
  "Xmlize a comment"
  (concat (and str (concat "<!-- " (make-string 60 ?-) "\n"
			   str "\n" (make-string 61 ?-) " -->\n"))))


(defun o2slides-make-chapter (item slide-count)
  "Make a chapter cover page"
  (let ((cur-chapter (caar item)))
    (o2slides-new-buffer (o2slides-slide-name slide-count)
			 (o2slides-slide-name (1- slide-count))
			 (o2slides-slide-name (1+ slide-count))
			 cur-chapter nil)
    (insert
     (o2slides-xml-comment (cdar item))
     "<center>\n"
     "<biggest>" cur-chapter "</biggest>\n"
     "</center>\n"
     "</slide>")
    (save-buffer)
    (or o2slides-slide-buffers-stay-open
	(kill-buffer (current-buffer)))
    cur-chapter))

(defun o2slides-make-slide (subitem chapter-title slide-count)
  "Make a slide"
  (let ((cur-slide (caar subitem)))
    (o2slides-new-buffer (o2slides-slide-name slide-count)
			 (o2slides-slide-name slide-count)
			 (o2slides-slide-name slide-count)
			 chapter-title cur-slide)
    (insert
     (o2slides-xml-comment (cdar subitem))
     "<center>\n"
     "<biggest>" cur-slide "</biggest>\n")

    (if (< 0 (length (cadr subitem)))
	(progn
	  (insert "<list>\n")
	  (mapcar '(lambda (subsubitem)
		     (insert "  <li><bigger>" 
			     (caar subsubitem)
			     "</bigger>\n"
			     (o2slides-xml-comment (cdar subsubitem)))

		     (if (< 0 (length (cadr subsubitem)))
			 (progn
			   (insert "      <list>\n")
			   (mapcar '(lambda (subsubsubitem)
				      (insert "         <li>" 
					      (caar subsubsubitem)
					      "</li>\n"))
				   (cadr subsubitem))
			   (insert "      </list>\n")))
		     (insert "   </li>\n"))
		  (cadr subitem))
	  (insert "</list>\n")))
    (insert "</center>\n"
	    "</slide>")
    (save-buffer)
    (or o2slides-slide-buffers-stay-open
	(kill-buffer (current-buffer)))
    cur-slide))


;; Mogrifier

(defun o2slides-mogrify-items (item-list)
  "Mogrify a whole list of doodads"
  (let ((chapter-count 0)
	(slide-count 0)
	chapter-title)
    (setq o2slides-buffer-list nil)
    (mapcar '(lambda (item)
	       (setq slide-count (1+ slide-count))
	       (setq chapter-title (o2slides-make-chapter item slide-count))
	       (mapcar '(lambda (subitem)
		       (setq slide-count (1+ slide-count))
		       (o2slides-make-slide subitem chapter-title slide-count))
		       (cadr item))
	       ) item-list)))


(defun o2slides (thedir)
  "Convert an outline into a w3slides skeleton presentation"
  (interactive "DCreate slides in directory: ")
  (setq o2slides-file-dir-name thedir)
  (o2slides-mogrify-items (o2slides-snarf)))


(provide 'o2slides)
;;;o2slies.el ends here