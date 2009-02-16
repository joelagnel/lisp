; GPL'ed as under the GNU license..
; -- Deepak Goel (deego@glue.umd.edu) 12/15/00
; Version:
(setq newmail-version "1.0.alpha")



;;;====================================================

;;; Availability: This file, along with accompanying files, and
;;; many sound-files can be gotten from
;;; http://www.glue.umd.edu/~deego/emacspub/newmail

;;;====================================================

;;; Contributions: are *most* welcome, and will lead to invitations for
;;; co-authorship, provided you please agree to GNU-freeness of this
;;; software.

;;;====================================================

;;; QUICK INSTALLING for newbies: Drop this somewhere in your load-path, and
;;; INSERT (load "newmail.el") in your .emacs.

;;;====================================================
;;; RUNNING: Either use (newmail), and give it all options you like,
;;; or use the following sample-functions: (newmail-checkmail), and
;;; (newmail-soundmail). 
;;;
;;;  From then on,  you don't need unix's newmail. 
;;;  Instead, if u like, add the following to your .aliases:
;;;  alias newmail 'emacs -batch -l ~/emacs/newmail.el -f newmail'
;;;  alias soundmail 'emacs -batch -l ~/emacs/soundmail.el -f \\
;;;   newmail-soundmail'
;;;  And use the latter! newmail-soundmail for a full-featured soundmail..
;;;====================================================

;;; Commentary: Like unix's newmail, but with tons of additional
;;; features.. like: complete customizability of the display,
;;; possibility of hourly cheeps etc., possibility of reading out
;;; mail-subject etc. etc. (if u have festival.el installed)..

;;; newmail can create appropriate sounds for you when you get new
;;; mail, and can even read them out for you.. if u have things like
;;; festival..  But on a smaller scale, it is like unix's newmail,
;;; except that you can configure exactly how much of the mail-subject
;;; etc. gets printed out for every new-mail.

;;; CUSTOMIZATION: don't like defaults? Before calling newmail.el,
;;; setq the variables to what you like. In fact, you cam even defun
;;; most of the functions to what you like.. since newmail will defun
;;; those functions only if they have not been already defunn'ed..

;;;====================================================


(defmacro newmail-def (a &rest default)
  "A function is defunn'ed only if not already defined. "
  (list 'if (list 'not (list 'functionp (list 'quote a)))
	(cons 'defun (cons a default)))
  )

(require 'cl)

;;;(defvar newmail-ansi-term-program "/bin/tcsh")

(defvar newmail-sounds-dir "~/sounds/"
  "This is where your sounds .au files are stored..
Don't worry about this if you don't care for a audio-notification of
newmail.. Else, setq this to wherever you store your sounds.."
)


(defvar newmail-newmail-shell-command "/usr/local/bin/newmail &"
  "Works on unix.
This needs to be set by the users on windows systems correctly, to
whatever their respective newmail-shell-commands may me")

(defvar newmail-checkmail-shell-command "checkmail"
  "Works on unix.
This needs to be set by the users on windows systems correctly, to
whatever their respective checkmail-shell-commands may be")

(defvar newmail-play-command "play -v "
  "Works on .au-enabled unix.
If not, or if you are on windoze, please setq this appropriately.")

(defvar newmail-incoming-mail-audio 
  '("doorbell.au" )
  "List of .au files to play for incoming mail.. "
)

(defvar newmail-message-waiting-audio
  '("MessageForYouSir.au")
  "List of files to play if message waiting when newmail is first
started."
)

(defvar newmail-sorry-no-mail-audio
  '("NoMail.au")
  "Files to play  if no message waiting upon starting newmail()."
)

(defvar newmail-hour-audio
  '("CuckooClock.au" "CuckooClock.au")
  "Files to play every hour."
)

(defvar newmail-half-hour-audio
  '("CuckooClock.au")
  "Files to play every half hour."
)

;;;###autoload
(defun newmail-soundmail-old ()
  "Sample full-featured newmail."
  (interactive)
  (newmail t t)
)

;;;###autoload
(defun newmail-old (&optional sound-mailp cheep check-interval)
  "Makes appropriate voices if mails..
Should be run as a batch file from fosters.umd.edu. Gives an initial
assessment when first run, thereafter, barks/gongs uppon new mail and
displays the subject of mail, appropriately abbreviated. By default, uses
play -v 40 NoMail.au and play -v 30 doorbell.au. Upon once playing gong,
plays it again only when there's more new mail. 

Makes mail-associated sounds only if sound-enabled is t.  Makes
hourly-cheeps only if cheep is t. 

Else just displays mail-subjects as they come in. 
"
  (interactive)
  (newmail-print "RUNNING NEWMAIL")
  (if (null check-interval) (setq check-interval 15))
  (shell)
  (sit-for 1)
  (set-buffer "*shell*")
  (let ((last-size (count-lines (point-min) (point-max)))
	(current-size 0)
	(last-hour (caddr (decode-time)))
	(last-minute (cadr (decode-time)))
	(last-half-hour (caddr (decode-time))))
    (if (< last-minute 30) (setq last-half-hour (% (1- last-half-hour) 24)))
    (goto-char (point-max))
    (newmail-wait 2)
    (insert newmail-checkmail-shell-command)
    (comint-send-input)
    (newmail-wait 2)
    (sit-for 2)
    (setq current-size (count-lines (point-min) (point-max)))
    (if (> current-size (+ last-size 1))
	(progn
	  (if sound-mailp (newmail-sound-new-message))
	  (newmail-show-mail-waiting
	   (buffer-substring
	    (progn
	      (goto-line last-size)
	      (point))
	    (point-max)))
	   )
      (if sound-mailp (newmail-sound-sorry)))
    (newmail-wait 2)
    (goto-char (point-max))
    (insert newmail-newmail-shell-command)
    (comint-send-input)
    (newmail-wait 2)
    (setq last-size (count-lines (point-min) (point-max)))  
    (newmail-wait 2)
    (goto-char (point-max))
    (sit-for check-interval)
    (while (> 1 0)
      (setq last-size (count-lines (point-min) (point-max)))  
      (sit-for check-interval)
      (newmail-wait 2)
      (setq current-size (count-lines (point-min) (point-max)))
      (if (> current-size last-size)
	  (progn
	    (if sound-mailp (newmail-sound-new-bell))
	    (newmail-show-new-message
	     (buffer-substring
	      (progn
		(goto-line last-size)
		(point))
	      (point-max)))
	    )
	(if sound-mailp (newmail-sound-nothing))
	)
      (if  cheep (setq last-half-hour (newmail-sound-half-hour
				       last-half-hour)))
      (if cheep (setq last-hour (newmail-sound-hour last-hour)))))
  
)


;;;###autoload
(defun newmail (&optional sound-mailp cheep check-interval)
  "Makes appropriate voices if mails..
Should be run as a batch file from fosters.umd.edu. Gives an initial
assessment when first run, thereafter, barks/gongs uppon new mail and
displays the subject of mail, appropriately abbreviated. By default, uses
play -v 40 NoMail.au and play -v 30 doorbell.au. Upon once playing gong,
plays it again only when there's more new mail. 

Makes mail-associated sounds only if sound-enabled is t.  Makes
hourly-cheeps only if cheep is t. 

Else just displays mail-subjects as they come in. 
"
  (interactive)
  (newmail-print "RUNNING NEWMAIL")
  (if (null check-interval) (setq check-interval 15))
  (shell)
  (sit-for 1)
  (set-buffer "*shell*")
  (let ((last-size (point-max))
	(current-size 0)
	(last-hour (caddr (decode-time)))
	(last-minute (cadr (decode-time)))
	(last-half-hour (caddr (decode-time))))
    (if (< last-minute 30) (setq last-half-hour (% (1- last-half-hour) 24)))
    (goto-char (point-max))
    (newmail-wait 2)
    (comint-send-input)
    (setq current-size last-size)
    (setq last-size current-size)
    (goto-char (point-max))
    (insert newmail-checkmail-shell-command)
    (comint-send-input)
    (newmail-wait 2)
    (sit-for 2)
    (setq current-size (count-lines (point-min) (point-max)))
    (if (> current-size (+ last-size 1))
	(progn
	  (if sound-mailp (newmail-sound-new-message))
	  (newmail-show-mail-waiting
	   (buffer-substring
	    (progn
	      (goto-line last-size)
	      (point))
	    (point-max)))
	   )
      (if sound-mailp (newmail-sound-sorry)))
    (newmail-wait 2)
    (goto-char (point-max))
    (insert newmail-newmail-shell-command)
    (comint-send-input)
    (newmail-wait 2)
    (setq last-size (count-lines (point-min) (point-max)))  
    (newmail-wait 2)
    (goto-char (point-max))
    (sit-for check-interval)
    (while (> 1 0)
      (setq last-size (count-lines (point-min) (point-max)))  
      (sit-for check-interval)
      (newmail-wait 2)
      (setq current-size (count-lines (point-min) (point-max)))
      (if (> current-size last-size)
	  (progn
	    (if sound-mailp (newmail-sound-new-bell))
	    (newmail-show-new-message
	     (buffer-substring
	      (progn
		(goto-line last-size)
		(point))
	      (point-max)))
	    )
	(if sound-mailp (newmail-sound-nothing))
	)
      (if  cheep (setq last-half-hour (newmail-sound-half-hour
				       last-half-hour)))
      (if cheep (setq last-hour (newmail-sound-hour last-hour)))))
  
)



(newmail-def newmail-sound-half-hour (oldhour)
	       " "
  (let ((newhour (caddr (decode-time))) (newminute (cadr
						    (decode-time))))
    (if (and (not (= newhour oldhour)) (>= newminute 30))
	(progn
	  (newmail-sound newmail-half-hour-audio 40)
	  newhour)
      oldhour))
)


(newmail-def newmail-sound-hour (oldhour)
		 " "
 (let ((newhour (caddr (decode-time))) (newminute (cadr (decode-time))))
   (if (not (= newhour oldhour))
       (progn
	 (newmail-sound newmail-hour-audio 40)
	 newhour)
     oldhour))
)


(newmail-def newmail-sound (list volume)
  "where list is a list of sounds, plays them is rapid succession."
  (let ((netstring
	 (apply 'concat
		(mapcar (lambda (arg) (concat " " newmail-sounds-dir arg 
					      " "))
			list))))
    (shell-command (concat newmail-play-command (format "%i" volume)
			   netstring) 
		   "*newmail-shell-command*"))
)

(newmail-def newmail-sound-single (name volume)
  "Plays note.."
  (shell-command (concat newmail-play-command (format "%i" volume) " "
			 newmail-sounds-dir name
			 )
		 "*newmail-shell-command*")
)



(newmail-def newmail-sound-new-bell ()
  "Sounds a gong if current-buffer *shell* exists
and there's a command-line available. Used by checkmail"
;;  (newmail-sound-single  "gong.au" 25)
;;   (newmail-sound-single "sounds1/MessageForYouSir.au" 8)
  (newmail-sound newmail-incoming-mail-audio 30)
)

(newmail-def newmail-sound-new-message ()
  "Sounds a gong if current-buffer *shell* exists
and there's a command-line available. Used by checkmail"
;;  (newmail-sound-single  "gong.au" 25)
   (newmail-sound newmail-message-waiting-audio 8)
;;  (newmail-sound-single "sounds2/doorbell.au" 30)
)

(newmail-def newmail-sound-sorry ()
  "Sounds a message if current-buffer *shell* exists
and there's a command-line available. Used by checkmail"
  (newmail-sound newmail-sorry-no-mail-audio 30)
)

(newmail-def newmail-sound-nothing ()
	     "By default, this sound is played every 15 seconds.
Unless there is new mail."
	     nil
)



(newmail-def newmail-wait (&optional arg)
  "Waits for arg secnds, default 8? seconds.
Helps me during login. sit-for doesn't work here since that can be
interrupted by pressing any key. May not work within emacs due to
parallelization.. Use only for batch, else use carefully."
  (if (or (null arg) (< arg 1)) (setq arg 8))
  (let* ((initial-time (cadr (current-time))) (this-time initial-time))
    (while (or (= this-time initial-time)
	       (not (zerop (% (- this-time initial-time) arg))))
      (setq this-time (cadr (current-time)))))
)

(newmail-def newmail-show-new-message (string)
	     "Parses string and displays some of it."
	     (newmail-print string)
)

(newmail-def newmail-show-mail-waiting (string)
	     "Parses string and displays some of it."
	     (newmail-print
	      (with-temp-buffer
		 (insert string)
		 (buffer-substring (point-min) 3)
		 ))
)


(defun newmail-print (&rest args)
  "Prints to shell as well as minibuffer, and insert in *scratch* as well."
;   (shell-command (apply 'concat "echo " 
; 			(mapcar 
; 			 (lambda (arg)			    
; 			   (format "%S" (concat 
; 					 (if (stringp arg)
; 					     arg
; 					   (format "%S" arg)) " ")))
;			 args)))
  (mapcar 'newmail-shell-print args)
  (set-buffer "*scratch*")
  (goto-char (point-max))
  (insert "\n")
  (mapcar
   (lambda (arg)
     (insert " ")
     (insert (format "%S" arg)))
   args)
  (mapcar 'princ args))

(defun newmail-shell-print (arg)
  (let ((str
    (progn
      (with-temp-buffer 
	(insert arg)
	(replace-string "\n" "\\\n")
	(buffer-substring (point-min) (point-max))))))
    (shell-command (concat "echo " str)))
)