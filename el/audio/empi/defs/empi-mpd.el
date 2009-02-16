;;; EMPI-MPD.EL --- Elisp backend to MPD for EMPI

(require 'empi-defutils)
(require 'empi-elisp)
(require 'libmpdee-utils)

(defgroup empi-mpd-backend nil
  "Elisp backend to MPD for EMPI."
  :prefix "empi-mpd-" :group 'empi :group 'mpd)

(defcustom empi-mpd-connection-parameters nil
  "Parameters for the mpd connection to be used by empi-mpd backend."
  :type 'mpd-connection :group 'empi-mpd-backend)

(defvar empi-mpd 'empi-elisp-command)
(defvar empi-mpd-conn
  (apply 'mpd-conn-new `(,@(mpd-connection-tidy empi-mpd-connection-parameters) nil)))

(defcustom empi-mpd-title-flexi
  '(flexi-print-backend-format
    "%{title}%(!{title}%{file}%)%({artist} | %{artist}%)%({album} | %{album}%)\
%({track} | Track #%{track}%)")
  "Format to be used for titles by the elisp MPD backend for EMPI."
  :type 'flexi-print-format :group 'empi-mpd-backend)

(defcustom empi-mpd-plentry-flexi
  '(flexi-print-backend-format
    "[%({time}%{time}%)%(!{time}-:--%)] %{title}%(!{title}%{file}%)%({artist} - \
%{artist}%)%({album} - %{album}%)%({track} - Track #%{track}%)")
  "Format to be used for playlist entries by the elisp MPD backend for EMPI."
  :type 'flexi-print-format :group 'empi-mpd-backend)

(defun empi-mpd-status-to-output-list (status)
  (let (slist)
    (setq slist
	  (list :qvolume (make-list 2 (aref status 0)) :qrepeat (aref status 1)
		:qshuffle (aref status 2) :qplid (aref status 3)
		:qpllength (aref status 4) :qcrossfade (aref status 7)
		:playingp (numpred (eq (aref status 8) 'play))
		:pausedp (numpred (mpd-convert-compat-pausedp status))))
    (setq slist (append slist (and (aref status 6) (list :qplpos (aref status 6)))))
    (when (aref status 9)
      (setq status
	    (list :qbitrate (aref status 5)
		  :qtime (* (aref status 9) 1000)
		  :qsonglength (* (aref status 10) 1000)
		  :qfrequency (aref status 11)
		  :qbits-per-sample (aref status 12)))
      (setq slist (nconc slist status))) slist))

(defmacro empi-mpd-with-status (cmd func &rest args)
  `(lambda (conn ,@args)
     (let ((status (empi-mpd-status-to-output-list (,func conn ,@args))))
       (and status (nconc status (list ,cmd t))))))

(defun empi-mpd-status (conn)
  (let ((status (mpd-get-status conn)))
    (if status (empi-mpd-status-to-output-list status))))

(defun empi-mpd-get-title (conn)
  (let ((status (mpd-get-status conn)))
    (when status
      (nconc
       (if (aref status 6)
	   (list :qtitle (mpd-format-title empi-mpd-conn
					   empi-mpd-title-flexi
					   (aref status 6))))
       (empi-mpd-status-to-output-list status)))))

(setplist 'empi-mpd
`(:prefix ,(list empi-mpd-conn) :defhandler empi-mpd-status
  :jumpitemnum mpd-seek :voladd mpd-adjust-volume :volsub mpd-decrease-volume
  :play ,(empi-mpd-with-status :play mpd-compat-play) :pause mpd-pause
  :stop ,(empi-mpd-with-status :stop mpd-compat-stop) :repeat mpd-toggle-repeat
  :shuffle mpd-toggle-random :plback mpd-prev :plnext mpd-next
  :enqueue mpd-fs-enqueue :plclear mpd-clear-playlist
  :qplfiles (lambda (conn) (list :qplfiles (mpd-get-playlist conn)))
  :jumptime ,(empi-mpd-with-status :jumptime mpd-jump-to-time-msec time)
  :qtitle empi-mpd-get-title :qvolume nil :qrepeat nil :qshuffle nil :qplid nil
  :qpllength nil :qbitrate nil :qplpos nil :qcrossfade nil :playingp nil
  :pausedp nil :qtime nil :qsonglength nil :qfrequency nil :qbits-per-sample nil
  :qchannels nil :pldel mpd-delete :plmove mpd-move :plswap mpd-swap
  :version (lambda (conn)
	     (concat "MPD "
		     (mapconcat 'number-to-string (mpd-get-version conn) ".")))
  :qpltitles (lambda (conn foreach)
 	       (mpd-get-playlist-entry
 		conn nil
 		`(lambda (song)
 		   (funcall ,foreach
 			    (mpd-format-title empi-mpd-conn
					      empi-mpd-plentry-flexi song)))) t)
  :misc (lambda (conn str)
	  (setq str (list :misc (cdr (mpd-execute-command conn str t))))
	  (mpd-get-last-error conn)
	  str)))

(provide 'empi-mpd)

;;; EMPI-MPD.EL ends here
