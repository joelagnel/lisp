;;; bubble.el, by Michelangelo Grigni <mic@cs.ucsd.edu>
;;
;; Commands bubble-buffer and bubble-buffer-back implement an idea
;; suggested in the appended Usenet article by Jacob Dreyer.  Several
;; solutions were posted, this is mine.  Note this uses last-command
;; rather than a timeout (I think this is acceptable to Jacob given
;; that it is possible to backup).  Actually I prefer buffer-menu, but
;; I think this simple interface is better for some users.
;;
;; Setup:
;;
;; (load "bubble" nil t)                ; or autoload the two commands
;; (global-set-key [f20] 'bubble-buffer) ; or some such pair of keys
;; (global-set-key [f19] 'bubble-buffer-back)

(defvar bubble-skip-regexp "^ "		; or "^ \\*Minibuf-"
  "Matches buffer names skipped by bubble-buffer.")
(defvar bubble-buffers nil
  "List of buffers bubbling through bubble-buffer, or nil.")
(defun bubble-buffer (&optional back)
  "Cycle through buffers not matching `bubble-skip-regexp'.
Optional prefix BACK means to bubble backwards."
  (interactive "P")
  (or (and (memq last-command '(bubble-buffer bubble-buffer-back))
	   bubble-buffers)
      (setq bubble-buffers
	    (cons
	     (current-buffer)		; included unconditionally
	     (apply
	      'nconc
	      (mapcar
	       (lambda (buf)
		 (if (string-match bubble-skip-regexp (buffer-name buf))
		     nil (list buf)))
	       (cdr (buffer-list)))))))
  ;; Todo: remove buffers visible in other windows?
  (let ((cb (current-buffer)) (below bubble-buffers) above buf)
    (while (and below (not (eq (car below) cb)))
      (setq above (cons (car below) above) below (cdr below)))
    (if below
	(setq below (cdr below))
      (error "current-buffer not found in bubble-buffers!"))
    ;; Now: (bubble-buffers...) ==  (...above cb below...)
    (cond
     ((and back above)			; backward, not at top
      (setq buf (car above) above (cdr above) below (cons cb below)))
     (back				; backward, at top
      (setq above (reverse bubble-buffers) buf (car above)
	    above (cdr above) below nil))
     (below				; forward, not at bottom
      (setq buf (car below) below (cdr below) above (cons cb above)))
     (t					; forward, at bottom
      (setq below bubble-buffers buf (car below)
	    below (cdr below) above nil)))
    ;; Now: (bubble-buffers...) ==  (...above buf below...)
    (mapcar 'switch-to-buffer above)
    (switch-to-buffer buf)
    (message "bubbling%s: %d above, %d below%s"
	     (if back " back" "")
	     (length above) (length below)
	     (if (not (if back below above)) " (wrapped)" ""))))

(defun bubble-buffer-back (for)
  "Do bubble-buffer backwards."
  (interactive "P") (bubble-buffer (not for)))

;; From: jdreyer@houston.geoquest.slb.com (Jacob Dreyer)
;; Newsgroups: gnu.emacs.help
;; Subject: NEW EXCITING FEATURE: bubble-buffer
;; Message-ID: <3u0u0e$630@sndsu1.sedalia.sinet.slb.com>
;; Date: 12 Jul 95 16:41:50 GMT
;; Organization: Schlumberger SINet, Sedalia, CO
;; NNTP-Posting-Host: picard.se.houston.geoquest.slb.com
;; Mime-Version: 1.0
;; Content-Type: text/plain; charset=us-ascii
;; Content-Transfer-Encoding: 7bit
;; X-Mailer: Mozilla 1.1N (X11; I; SunOS 4.1.3 sun4c)
;; X-URL: news:gnu.emacs.help
;;
;; NEW FUNCTION:
;;   bubble-buffer
;;
;; MOTIVATION:
;;   Besides buffer editing, switching between buffers is one of the most
;;   important tasks for Emacs users. Using find-file is way to time
;;   consuming in terms of # key-strokes, using the buffer list so too.
;;   Problem with these is that you have to *think* about the switching
;;   process, and therefore need to take your attention away from the
;;   task you are working at. switch-to-buffer is better, but will only
;;   be efficient if used to switch back and forth between two top buffers.
;;
;; FEATURES:
;;   The bubble-buffer function is very very simple. No silly questions
;;   to answer, no mouse handling, just very simple and intelligent buffer
;;   switching. For full effect, the function should be bound to a specific
;;   key.
;;   The function causes the most recent looked-at buffers to bubble up to
;;   the top of the buffer stack. The clue is that if a buffer during the
;;   switching is NOT looked at for a specific period of time (say 1 second),
;;   it is NOT considered to be of interest and will therefore NOT bubble
;;   up the stack.
;;
;; ALGORITHM:
;;   Pseudo C algorithm:
;;
;;      #define    LIMIT  1000  (milliseconds)
;;      static int time = 0;
;;
;;      void bubble-buffer()
;;      {
;;         if (time_now - time > LIMIT || all buffers marked)
;;           unmark all buffers;
;;         mark current buffer;
;;         scan down bufferlist and switch current with first unmarked;
;;         time = time_now;
;;      }
;;
;; EXAMPLE ON USE:
;;   Say you have five buffers, named "1" - "5". Topmost is the active buffer
;;   (marked with []). "*" means the buffer is internally 'marked'.
;;
;;     [1]    I am editing a c-file called "1". Suddenly I need the name
;;      2     of a struct field from a header file I now is down there
;;      3     somewhere...
;;      4
;;      5
;;
;;     [2]    ...I hit the bubble-buffer key, buffer "1" is marked and switched
;;      1*    with buffer 2. This is not the one I'am looking for, so I
;;      3     immediately bubble again...
;;      4
;;      5
;;
;;     [3]    ...nope, still not the one. Should be close now though, so
;;      1*    I bubble again...
;;      2*
;;      4
;;      5
;;
;;     [4]    ...there we are. Let me see: what was this name I was looking
;;      1*    for, ehhhhhhhhhhh: TIMEOUT!! Next time I bubble, the marks will
;;      2*    be gone, and I will be back to buffer "1" where I started!!
;;      3*
;;      5
;;
;; LISP CODE:
;;   Well, that's where I need you folks! I am not a great lisp hacker, and
;;   some of you will hopefully see the value of this simple function and help
;;   me out. I guess the challenges will be to do the millisecond timing, and
;;   to find a way to represent the internal marking. The LIMIT value will
;;   probably need some customizing when we see how this all work out in
;;   practice. Hope I've thought about everything; really difficult to get a
;;   good feel for a function like this before it is implemented, but I've
;;   tought through several ideas for something like this for quite a while,
;;   and I think this timing approch is the best and simplest one.
;;
;;
;; Jacob Dreyer
