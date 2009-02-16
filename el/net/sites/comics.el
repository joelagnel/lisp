;;; comics.el --- Get comics from the net

;; Copyright (C) 1998-2002 Sean MacLennan
;; $Revision: 1.13 $ $Date: 2002/07/21 18:17:01 $

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

(defvar comics-dir (expand-file-name "~/comics/")
  "*Base directory to store the files in.")

;; This is now comics.com
(defvar united-media-list
  '("monty" "dilbert" "drabble" "committed" "warped" "reality"
    "randolphitch")
  "*List of comics to retrieve from the united media site.")

(defvar creators-list
  '("speedbump" "naturalselection")
  "*List of comics to retrieve from the united media creators site.")

;; Now ucomics.com
(defvar uexpress-list '("dp" "bz" "fw" "tmqui" "cl")
  "*List of comics to retrieve from the uexpress site.
Note: Use the two character directory names.")

(defvar uexpress-url-prefix
  "http://images.ucomics.com/comics")
  ;; old "http://a1964.g.akamai.net/7/1964/1392/1dcf6501c8f9fa/images.ucomics.com")

(defvar comics-proc-list nil)

(defvar comics-errors 0)
(defvar comics-error-list nil)

(defvar comics-remove t
  "*Remove all old comics before loading new ones.")

(defvar comics-rm-command "rm -f ")

(defun comics-init ()
  "This is really for debugging."
  (interactive)
  (setq comics-proc-list nil) ;; clean up old processii
  (setq comics-error-list nil)
  (setq comics-errors 0)
  (when comics-remove
    (call-process shell-file-name nil nil nil shell-command-switch
		  (concat comics-rm-command
			  comics-dir "*.gif "
			  comics-dir "*.jpg "
			  comics-dir "*.png "))))


;; Yesterday's comics
;;(comics-get (time-sub (current-time) '(1 20864 0)))
;; Two days ago
;;(comics-get (time-sub (current-time) '(2 41728 0)))

;;;###autoload
(defun comics-get (&optional time)
  (interactive)

  ;; See if any processes still running
  (let ((proc-list (process-list)))
    (dolist (comic comics-proc-list)
      (if (member (car comic) proc-list)
	  (error "Already running"))))

  ;; Quick check for `comics-dir'
  (unless (file-exists-p comics-dir)
    (if (yes-or-no-p (concat comics-dir " does not exist. Create? "))
	  (make-directory comics-dir)
      (error "Comics directory does not exist.")))

  (comics-init)

  (setq time (decode-time time))

  ;; These comics will not work except for the current day
  (comics-united-media time)
  (comics-frank-and-ernest time)
  (comics-creators time)
  (comics-aislin time)
  (comics-userfriendly time)
  (comics-freefall time)
  (comics-clango time)

  ;; These comics will work at any time
  (comics-uexpress time)
  ;;(comics-shermans-lagoon time)
  (comics-help-desk time)
  (comics-kevin-kell time)
  (comics-angst-tech time)

  (when running-windoze (sit-for 10)) ;; SAM HACK

  (comics-do-ymd time)
  )

;;;###autoload
(defun comics-build-html ()
  (interactive)
  (save-excursion
    (set-buffer (create-file-buffer (concat comics-dir "index.html")))
    (setq buffer-file-name (concat comics-dir "index.html"))
    (erase-buffer)
    (insert "<html><head><title>Comics</title></head><body><ul>\n")
    (let ((files
	   (directory-files comics-dir nil
			    ".*\.gif\\|.*\.jpg\\|.*\.png"
			    nil)))
      (dolist (file files)
	(insert (format "<li>%s<br><img src=\"%s\">\n" file file))))
    (insert "</ul></body></html>\n")
    (save-buffer)
    (kill-buffer (current-buffer))))
;; ------------------------------------------------------------------
;; Hamburger helper
;; ------------------------------------------------------------------

(defun comics-helper (name url &optional no-error)
  (string-match "\.[a-z]+$" url)
  (let ((ext (match-string 0 url)))
    (push (http-save-page
	   url
	   (concat comics-dir name ext)
	   (format "*%s-status*" name)
	   (if no-error
	       'get-sentinel-no-error
	     'get-sentinel))
	  comics-proc-list)))

;; ------------------------------------------------------------------
;; Individual comics
;; ------------------------------------------------------------------

;; This is now comics.com
;; http://comics.com/comics/monty/archive/images/monty2001073089216.gif
(defun comics-united-media (time)
  ;; united-media
  (push (list
	 (http-get-page "http://www.comics.com/comics/monty/"
			"*monty*"
			'comics-sentinel)
	 (list
	  "/comics/monty/archive/images/monty\\([0-9]+\\.gif\\)"
	  'united-media-page-url
	  united-media-list))
	comics-proc-list))

(defun united-media-page-url (comic gif)
  (concat "http://www.comics.com/comics/"
	  comic "/archive/images/" comic gif))

;; Frank and Ernest is a jpg except on sunday
;; http://comics.com/comics/franknernest/archive/images/franknernestX.gif
(defun comics-frank-and-ernest (time)
  (push (list
	 (http-get-page "http://www.comics.com/comics/franknernest/"
			"*franknernest*"
			'comics-sentinel)
	 (list
	  (concat "/comics/franknernest/archive/images/"
		  "franknernest\\([0-9]+\\.\\(gif\\|jpg\\)\\)")
	  'frank-and-ernest-page-url
	  '("franknernest")))
	comics-proc-list))

(defun frank-and-ernest-page-url (comic gif)
  (concat "http://www.comics.com/comics/"
	  comic "/archive/images/" comic gif))

(defun comics-creators (time)
  ;; united-media creators
  (push (list
	 (http-get-page "http://www.unitedmedia.com/creators/speedbump/"
			"*speedbump*"
			'comics-sentinel)
	 (list
	  "/creators/speedbump/archive/images/speedbump\\([0-9]+\\.gif\\)"
	  'creators-page-url
	  creators-list))
	comics-proc-list))

(defun creators-page-url (comic gif)
  (concat "http://www.unitedmedia.com/creators/"
	  comic "/archive/images/" comic gif))

(defun comics-uexpress (time)
  (let ((year (mod (nth 5 time) 100))
	(list uexpress-list))
    ;; Fifth wave is only on sundays
    (unless (eq (nth 6 time) 0) (setq list (delete "fw" list)))
    (dolist (comic uexpress-list)
      (comics-helper comic
		     (format "%s/comics/%s/%d/%s%02d%02d%02d.gif"
			     uexpress-url-prefix comic (nth 5 time) comic
			     year (nth 4 time) (nth 3 time))))))

(defun comics-userfriendly (time)
  (push (list
	 (http-get-page "http://www.userfriendly.org/static/index.html"
			"*user-friendly*"
			'comics-sentinel)
	 (list
	  (concat "\\(http://www.userfriendly.org/cartoons/archives/[^/]+/"
		  "xuf[0-9]+\\.gif\\)")
	  'userfriendly-page-url
	  '("userfriendly")))
	comics-proc-list))

(defun userfriendly-page-url (comic gif) gif)

;; M W F
(defun comics-freefall (time)
  (let ((dow (nth 6 time)))
    (when (or (eq dow 1) (eq dow 3) (eq dow 5))
      (push (list
	     (http-get-page "http://www.purrsia.com/freefall/"
			    "*free-fall*"
			    'comics-sentinel)
	     (list
	      "img src=\"\\(/freefall/ff[0-9]+/fv[0-9]+.gif\\)\""
	      'freefall-page-url
	      '("freefall")))
	    comics-proc-list))))

(defun freefall-page-url (comic gif)
  (concat "http://www.purrsia.com" gif))

(defun comics-clango (time)
  (push (list
	 (http-get-page "http://clango.org/"
			"*clango*"
			'comics-sentinel)
	 (list
	  "\\(http://images.clango.org/strips/[^.]+.png\\)"
	  'clango-page-url
	  '("clango")))
	comics-proc-list))

(defun clango-page-url (comic gif) gif)

;; Aislin - does not produce a cartoon every day
;; http://www.canada.com/montreal/montrealgazette/specials/aislin/Aislin.0208.jpg
;; sometimes a gif
(defvar aislin-dir
  "http://www.canada.com/montreal/montrealgazette/specials/aislin/")

(defun aislin-page-url (comic gif) (concat aislin-dir gif))

(defun comics-aislin (time)
  (push (list
	 (http-get-page
	  (concat aislin-dir "index.html")
	  "*aislin*"
	  'comics-sentinel)
	 (list
	  "\\(Aislin\\.[0-9]+\\.\\(gif\\|jpg\\)\\)"
	  'aislin-page-url
	  '("aislin")))
	comics-proc-list))

;; Help desk has 3 sometimes 4 images (aceg) on weekdays
(defun comics-help-desk (time)
  (let ((dow   (nth 6 time))
	(year  (nth 5 time))
	(month (nth 4 time))
	(day   (nth 3 time)))
    (unless (or (eq dow 0) (eq dow 6))
      (dolist (comic '("a" "c" "e" "g"))
	(comics-helper (concat "hd" comic)
		       (format "http://ubersoft.net/comics/hd%d%02d%02d%s.png"
			       year month day comic)
		       (not (equal comic "a")))))))

;; -------------------- simple -------------------

;; Sherman's Lagoon
;; http://www.slagoon.com/dailies/SL99.08.08.gif
(defun comics-shermans-lagoon (time)
  (comics-helper "shermans"
		 (format "http://www.slagoon.com/dailies/SL%02d.%02d.%02d.gif"
			 (mod (nth 5 time) 100) (nth 4 time) (nth 3 time))))

;; http://www.kevinandkell.com/2001/strips/kk20010213.gif
(defun comics-kevin-kell (time)
  (comics-helper "kevin-kell"
		 (format "http://www.kevinandkell.com/%d/strips/kk%d%02d%02d.gif"
			 (nth 5 time) (nth 5 time) (nth 4 time) (nth 3 time))))

;; http://www.inktank.com/images/AT/cartoons/07-01-01.gif
(defun comics-angst-tech (time)
  (comics-helper
   "angst"
   (format "http://www.inktank.com/images/AT/cartoons/%02d-%02d-%02d.gif"
	   (nth 4 time) (nth 3 time) (mod (nth 5 time) 100))))

;; ---------------------- ymd ----------------------

;; Note: We downcase the extensions when saving
(defvar comics-ymd-list
  '(("gpf" "http://www.gpf-comics.com/comics/gpf%d%02d%02d.gif"
     "Xmtwtfs") ;; not sunday
    ("crfh" "http://www.crfh.net/comics/crfh%d%02d%02d.jpg")
    ("sinfest" "http://www.sinfest.net/comics/sf%d%02d%02d.gif")
    ("walky" "http://www.itswalky.com/comics/%d%02d%02da.gif")
    ;; Ended ("greytown" "http://www.livingingreytown.com/comics/grey%d%02d%02d.jpg")
    ("ffarm" "http://www.funnyfarmcomics.com/comics/%d%02d%02da.gif")
    ;; Ended ("pentasmal" "http://www.pentasmal.com/comics/%d%02d%02d.png"
    ;; "XmtwtfX") ;; week days
    ;; Ended ("lab" "http://www.baysid.com/comics/LAB%d%02d%02d.png"
    ;; "XmtwtfX") ;; week days
    ("landf" "http://www.lostandfoundcomic.com/comics/%d%02d%02d.gif"
     "smXwXfX")
    ;;("avalon" "http://www.avalonhigh.com/comics/avalon%d%02d%02d.gif")
    ("soap" "http://www.soaprope.com/comics/soar%d%02d%02d.gif"
     "XmtwtXX") ;; mon-thur
    ;; Ended ("coolcat" "http://www.coolcatstudio.com/comics/ccs%d%02d%02d.gif"
    ;;  "XXtXtXs")
    ;; Ended ("bobbins" "http://www.bobbins.org/comics/%d%02d%02d.png")
    ;;("joe" "http://www.joeaverage.org/comics/%d%02d%02d.png"
    ;; "XmtwtfX")
    ("pvp" "http://www.pvponline.com/archive/2002/pvp%d%02d%02d.gif")
    ;;("antisoc" "http://antisocial.meldstar.net/comics/%d%02d%02d.jpg"
    ;; "XmtwtfX")
    ;;("framed" "http://framed.keenspace.com/comics/%d%02d%02d.gif")
    ;;("fj" "http://fatjesus.keenspace.com/comics/fatjesus%d%02d%02d.png")
    ;;("spork" "http://www.sporkman.com/comics/%d%02d%02d.png")
    ;; ended? ("fletchers" "http://fletcherscave.keenspace.com/comics/%d%02d%02d.GIF"
    ;; "XmXwXfX")
    ;; gifs during the week, jpg on sunday
    ("ozy" "http://www.ozyandmillie.org/comics/om%d%02d%02d.gif"
     "Xmtwtfs")
    ;; ("superosity" "http://www.superosity.com/comics/sup%d%02d%02d.gif")
    ;; ended ("wigu" "http://www.whenigrowup.net/comics/wigu%d%02d%02d.png"
    ;; "XmtwtfX")
    ;;("blotto" "http://blottostreet.keenspace.com/comics/blotto%d%02d%02d.jpg")
    ("sillyconev" "http://sillyconev.keenspace.com/comics/%d%02d%02d.gif")
    )
  "*List of comics that use year/month/day format.
The first element is the comics name. This is used for the image name and
for the status buffer.
The second element is the URL with exactly three arguments for year/month/day.
The optional third element is a date mask (smtwtfs), that starts on Sunday. If
any day in the mask is and X, that day is skipped.")

(defun comics-do-ymd (time)
  (let ((dow   (nth 6 time))
	(year  (nth 5 time))
	(month (nth 4 time))
	(day   (nth 3 time))
	days)
    (dolist (comic comics-ymd-list)
      (setq days (nth 2 comic))
      (unless (and days (string= (substring days dow (1+ dow)) "X"))
	(comics-helper (car comic)
		       (format (nth 1 comic) year month day))))))

;; ------------------------------------------------------------------
;; Utility functions
;; ------------------------------------------------------------------

(unless (fboundp 'remassoc)
  ;; This is probably not optimal
  (defun remassoc (key alist)
    (let (new)
      (dolist (elmt alist)
	(unless (and (consp elmt) (equal (car elmt) key))
	  (setq new (cons elmt new))))
      (nreverse new)))
  )

;; '(proc . '(image-regexp page-url-func comics-list))
(defun comics-sentinel (proc str)
  "Called when the page has been read. Parses the page and finds the
current image. Then reads the image to file."
  (save-excursion
    ;; Lookup info list for this page
    (let ((info (car (cdr (assoc proc comics-proc-list)))))
      ;; safe to delete - cannot use delq here
      (setq comics-proc-list (remassoc proc comics-proc-list))

      ;; Check http status
      (set-buffer (process-buffer proc))
      (goto-char (point-min))
      (unless (looking-at "HTTP/.\\.. 200")
	(setq comics-errors (+ 1 comics-errors))
	(error "http request failed: %s"
	       (buffer-substring (point-min) (progn (end-of-line) (point)))))

      ;; Check that list info exists
      (unless info
	(setq comics-errors (+ 1 comics-errors))
	(error "proc %S not in comics-proc-list: %S" proc comics-proc-list))

      ;; Look for image
      (unless (search-forward-regexp (nth 0 info) nil t)
	(setq comics-errors (+ 1 comics-errors))
	(error "%s gif not found" (process-buffer proc)))

      ;; Once we have the gif image, we can get all the pages
      (let ((list (nth 2 info))
	    (gif (match-string 1))
	    ext comic)
	(if (string-match "\\.[a-zA-Z]+$" gif)
	    (setq ext (match-string 0 gif)))
	(while list
	  (setq comic (car list))
	  (push (http-save-page
		 (apply (nth 1 info) (list comic gif))
		 (concat comics-dir comic (downcase ext))
		 (concat "*" comic "-status*")
		 'get-sentinel)
	      comics-proc-list)
	(setq list (cdr list))))
      ))
  (safe-kill-buffer (process-buffer proc)))


;; This sentinel is called after reading an image, either 2-stage or direct
(defun get-sentinel (proc str)
  (if (string= str "finished\n")
      (safe-kill-buffer (process-buffer proc))
    (push (buffer-name (process-buffer proc)) comics-error-list)
    (setq comics-errors (+ 1 comics-errors)))
  (setq comics-proc-list (delq proc comics-proc-list))
  (unless comics-proc-list
    (if (eq comics-errors 0)
	(message "All comics loaded.")
      (error "%d comics failed (%S)" comics-errors comics-error-list))))

;; This sentinel is called after reading an image
(defun get-sentinel-no-error (proc str)
  (safe-kill-buffer (process-buffer proc))
  (setq comics-proc-list (delq proc comics-proc-list))
  (unless comics-proc-list
    (if (eq comics-errors 0)
	(message "All comics loaded.")
      (error "%d comics failed (%S)" comics-errors comics-error-list))))

(defun safe-kill-buffer (buffer)
  "Do not kill a nil buffer."
  (if buffer
      (kill-buffer buffer)))

(defun time-sub (t1 t2)
  "Subtact two lisp time values.
The time values are stored as a list of three 16-bit numbers.
We ignore the 3rd number."
  (let ((t1-lo (cadr t1))
	(t1-hi (car t1))
	(t2-lo (cadr t2))
	(t2-hi (car t2)))
    (when (> t2-lo t1-lo)
	(setq t1-hi (- t1-hi 1))
	(setq t1-lo (+ t1-lo 65536)))
    (list (- t1-hi t2-hi) (- t1-lo t2-lo))))

(require 'http)
(provide 'comics)
