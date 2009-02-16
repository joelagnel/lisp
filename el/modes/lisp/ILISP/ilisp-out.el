;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-out.el --
;;; ILISP output, including a popper replacement.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-out.el,v 1.15 2003/03/26 03:22:47 rgrjr Exp $

;;; Old history log.
;;;
;;; 2000-03-02: Martin Atzmueller: general rewrite to support
;;; a general interface for multiple different output-frames.


(defvar ilisp-*icon-file* "/pictures/ilisp-icon.bmp")

(defun ilisp-find-ilisp-icon ()
  (if (and ilisp-*icon-file*
	   (file-exists-p (concat ilisp-*directory* ilisp-*icon-file*)))
      (concat ilisp-*directory* ilisp-*icon-file*)
    t))

(defun ilisp-make-output-frame (name)
  (when (and window-system ilisp-*use-frame-for-output*)
    (let ((new-frame
	   (make-frame `((name . ,name)
			 (minibuffer . nil)
			 (visibility . nil)
			 (unsplittable . t)
			 (menu-bar-lines . 0)
			 ;; Use of icon-type is currently disabled due to a bug
			 ;; in at least Emacs 21.1 running on Windows.
			 ;; (icon-type . ,(ilisp-find-ilisp-icon))
			 )))
	   )
      (when (eq +ilisp-emacs-version-id+ 'xemacs)
	(set-frame-properties new-frame '(default-toolbar-visible-p nil
					  default-gutter-visible-p nil
					  menubar-visible-p nil
					  has-modeline-p t))
	)
      new-frame)))


(defvar ilisp-display-output-function 'ilisp-display-output-default
  "The name of a function to display all ilisp output.
The function gets a single argument, a string.")


;;; ilisp-output-sink --
;;; Datastructure for a output sink that points to its 
;;; output-{buffers|frames|windows}

(defstruct ilisp-output-sink
  (buffer nil)
  (frame nil)
  (frame-name nil)
  (mode nil)
  (modeline nil)
  (set-modeline-p nil)
  (major-mode-def nil)
  (window-min-height nil)
  (window-max-height nil)
  (frame-min-height nil)
  (frame-min-width nil))


;;; general ilisp-output

(defvar ilisp-output-mode nil
  "If T, then we are in the ilisp-output minor mode.")

;; Minor mode (just to get a pretty mode line).

(defvar ilisp-output-mode-line nil)


(make-variable-buffer-local 'ilisp-output-mode)

(or (assq 'ilisp-output-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(ilisp-output-mode ilisp-output-mode-line) minor-mode-alist)))


;;; ilisp-output
;;; ilisp-output is the default for all commands

(defvar ilisp-output nil "Output for general ILISP-output")

(setq ilisp-output
      (make-ilisp-output-sink
       :buffer          " *Output*"
       :major-mode-def      'lisp-mode  ; The major mode for the
					; typeout window.
       :frame nil
       ;; Cached frame for ILISP output. If no window system is
       ;; 'running' then the value of this is nil.
       :frame-name "ILISP Output"
       :modeline 'ilisp-output-mode-line
       :set-modeline-p t
       :mode 'ilisp-output-mode
       :window-min-height 2		; The minimum height of the
					; typeout window
       :window-max-height 25		; The maximum height of the
					; typeout window
       :frame-min-height 5		; Rows
       :frame-min-width 70		; Columns
       ))


;;; arglist-output

(defvar ilisp-arglist-output nil "Output sink for Arglist messages.")

(if ilisp-*use-frame-for-arglist-output-p*
    (progn
      ;; if seperate output for arglist enabled, then use it!
      (setq ilisp-arglist-output
            (make-ilisp-output-sink
             :buffer " *Arglist-Output*"
             :major-mode-def 'lisp-mode ; The major mode for the
					; typeout window.
             :frame nil
             ;; Cached frame for ILISP output. If no window system is
	     ;; 'running' then the value of this is nil.
             :frame-name "ILISP Arglist Output"
             :modeline nil
             :set-modeline-p nil
             :mode 'ilisp-output-mode
             :window-min-height 2	; The minimum height of the
					; typeout window
             :window-max-height 25	; The maximum height of the
					; typeout window
             :frame-min-height 5	; Rows
             :frame-min-width 70	; Columns
             ))
      (defvar ilisp-arglist-output-mode nil
        "If T, then we are in the ilisp-arglist-output minor mode.")
      
      (make-variable-buffer-local 'ilisp-arglist-output-mode)
      
      (or (assq 'ilisp-arglist-output-mode minor-mode-alist)
          (setq minor-mode-alist
                (cons '(ilisp-arglist-output-mode
			ilisp-arglist-output-mode-line)
		       minor-mode-alist))))
  ;; Otherwise use default
  (setq ilisp-arglist-output ilisp-output))


;;; ilisp-*command-to-ilisp-output-sink-table* --
;;; Actually implemented as an a-list.

(defvar ilisp-*command-to-ilisp-output-sink-table* ()
  "An association table between 'commands and 'output sinks.
It is used to determine where the output of a 'command' should go.")

;;; Output sink history.
(defvar ilisp-*output-sink-history* nil
  "List of output sinks (i.e. buffers) that may currently be in use,
most recent first [internal state variable].  This grows when
ilisp-output-buffer pushes a new sink onto it, and the topmost sink is
used by 'ilisp-scroll-output' and 'ilisp-bury-output' (which pops it).
Use the 'ilisp-last-active-output-sink' function to return the most
recent sink that is still active, where 'active' means 'currently
displayed on the screen'.")

(defun ilisp-last-active-output-sink ()
  "Return the topmost active output since in ilisp-*output-sink-history*,
after popping any inactive ones."
  (let ((result (car ilisp-*output-sink-history*)))
    (while (and result
		(let ((buffer (ilisp-output-sink-buffer result)))
		  (or (null buffer)
		      (null (get-buffer-window buffer t)))))
      ;; not active; pop it, and try the next.
      (setq ilisp-*output-sink-history* (cdr ilisp-*output-sink-history*))
      (setq result (car ilisp-*output-sink-history*)))
    result))

;;; Accessor functions for
;;; 'ilisp-*command-to-ilisp-output-sink-table*'.

(defun* ilisp-get-sink-for-command (command &optional (default ilisp-output))
  (let ((result (assoc* command ilisp-*command-to-ilisp-output-sink-table*
		       :test #'eq)))
    (if result
	(rest result)
      default)))


(defun* ilisp-set-sink-for-command (command output-sink)
  (setf ilisp-*command-to-ilisp-output-sink-table*
	(acons command output-sink 
	       ilisp-*command-to-ilisp-output-sink-table*)))


(ilisp-set-sink-for-command 'arglist-lisp
			    ilisp-arglist-output)

(ilisp-set-sink-for-command 'ilisp-arglist-message-lisp-space
			    ilisp-arglist-output)


;;; Output buffer functions.

(defun ilisp-output-buffer (ilisp-output-sink &optional create-p)
  "Displays the sink's buffer.
Sets the corresponding modeline if the 'set-modeline-p' slot is T for
sink."
  (let ((buffer
	 (if create-p
	     (get-buffer-create (ilisp-output-sink-buffer ilisp-output-sink))
	   (get-buffer (ilisp-output-sink-buffer ilisp-output-sink))))
	(modeline (ilisp-output-sink-modeline ilisp-output-sink))
	(set-modeline-p (ilisp-output-sink-set-modeline-p ilisp-output-sink))
	)
    ;; save ilisp-output-sink for scrolling and burying.  first, clean up old
    ;; output sinks by calling ilisp-last-active-output-sink for side effect.
    ;; then, move this sink to the top of the stack.
    (ilisp-last-active-output-sink)
    (or (eq ilisp-output-sink (car ilisp-*output-sink-history*))
	(setq ilisp-*output-sink-history*
	      (cons ilisp-output-sink
		    (delete ilisp-output-sink ilisp-*output-sink-history*))))
    (unless (and (boundp modeline) (symbol-value modeline))
      (when set-modeline-p
	(setf (symbol-value modeline)
	      (list (format " %s bury, %s scroll" 
			    (ilisp-where-is 'ilisp-bury-output)
			    (ilisp-where-is 'ilisp-scroll-output))))))
    buffer))
  
(defun ilisp-output-window (ilisp-output-sink)
  "Gets the Output-Window for sink's buffer."
  (let ((buffer (get-buffer (ilisp-output-sink-buffer ilisp-output-sink))))
    (when buffer
      (get-buffer-window buffer t))))


;;; Popper replacement

;;; ilisp-bury-output --
;;;
;;; 19991220 Marco Antoniotti
;;; Changed the function to take care of the output frame.

(defun ilisp-bury-output-internal (ilisp-output-sink)
  ;; given an active output sink, make it go away.
  (let* ((buffer (ilisp-output-sink-buffer ilisp-output-sink))
	 (window (and buffer (get-buffer-window buffer t)))
	 (frame (ilisp-output-sink-frame ilisp-output-sink)))    
    (with-current-buffer buffer
      (erase-buffer))
    (bury-buffer buffer)
    (if frame
	(unless (eql this-command 'ilisp-arglist-message-lisp-space)
	  (ilisp-delete-message-frame ilisp-output-sink)
          (if (eq ilisp-output-sink (car ilisp-*output-sink-history*))
            (setq ilisp-*output-sink-history*
                    (cdr ilisp-*output-sink-history*))))
	(when window
	  (condition-case error
	      (ilisp-delete-window window)
	    (error t))))))

(defun ilisp-bury-output (&optional bury-all-p)
  "Delete the topmost typeout window, with sink's buffer, if any.
If given a numeric argument, deletes all typeout windows."
  (interactive "P")
  (let ((ilisp-output-sink (ilisp-last-active-output-sink))
	(buried-one-p nil))
    (while (and ilisp-*output-sink-history*
		(or bury-all-p
		    (not buried-one-p)))
      (ilisp-bury-output-internal ilisp-output-sink)
      (setq buried-one-p t)
      (setq ilisp-output-sink (ilisp-last-active-output-sink)))
    (if (not buried-one-p)
	(message "No more output to bury."))))

(defun ilisp-delete-window (window)
  "Delete a window with minimal redisplay."
  (let ((height (window-height window))
	(lower-window (ilisp-find-lower-window window)))
    (delete-window window)
    (when (and lower-window
	       (not (eq lower-window window)))
      (let ((old-window (selected-window)))
	(save-excursion
	  (select-window lower-window)
	  (set-buffer (window-buffer))
	  (goto-char (window-start))
	  (vertical-motion (- height))
	  (set-window-start lower-window (point)))
	(select-window old-window)))))

(defun ilisp-scroll-output (&optional lines)
  "Scroll the typeout-window, if any."
  (interactive "P")
  (let* ((ilisp-output-sink (or (ilisp-last-active-output-sink)
				(error "No output to scroll.")))
	 (buffer (ilisp-output-buffer ilisp-output-sink))
	 (window (and buffer (get-buffer-window buffer t)))
	 (old-window (selected-window)))
    (unwind-protect
	 (progn
	   (select-window window)
	   (set-buffer buffer)	;; [maybe redundant?]
	   ;; 19990806 Martin Atzmueller
	   ;; (scroll-up lines)
	   (let ((scroll-in-place nil))
	     (scroll-up lines)))
      (select-window old-window))))

(defun ilisp-grow-output (&optional n)
  "Grow the typeout window by ARG (default 1) lines."
  (interactive "p")
  (let* ((buffer (ilisp-output-buffer ilisp-output))
         (window (and buffer (get-buffer-window buffer t)))
	 (old-window (selected-window)))
    (when window
      (unwind-protect
	  (progn
	    (select-window window)
	    (enlarge-window n))
	(when (ilisp-window-live-p old-window)
	  (select-window old-window))))))


(defun ilisp-trim-blank-lines ()
  ;; Delete leading blank lines
  (goto-char (point-min))
  (when (looking-at "\n+") (replace-match ""))
  ;; Delete trailing blank lines
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (when (< (point) (point-max))
    (delete-region (1+ (point)) (point-max))))

(defun ilisp-write-string-to-buffer (ilisp-output-sink string)
  (let ((buffer (ilisp-output-buffer ilisp-output-sink t)))
    (save-excursion
      (set-buffer buffer)
      (let ((buffer-read-only nil))
      ;; Maybe an option to keep the old output?
        (erase-buffer))
      ;; New: select mode for the output buffer.
      (unless (eq major-mode
		  (ilisp-output-sink-major-mode-def ilisp-output-sink))
	(funcall (ilisp-output-sink-major-mode-def ilisp-output-sink)))
      (setf (symbol-value (ilisp-output-sink-mode ilisp-output-sink)) t)
      (let ((buffer-read-only nil))
      (princ string buffer)
      (ilisp-trim-blank-lines)
        (goto-char (point-min))))))


(defun ilisp-desired-height (ilisp-output-sink windowp)
  (let ((height
	 (cond ((not windowp)
		(ilisp-needed-buffer-height
		 (ilisp-output-sink-buffer ilisp-output-sink)))
	       (windowp
		(ilisp-needed-window-height
                 (get-buffer-window
		  (ilisp-output-sink-buffer ilisp-output-sink) t))))))
    (max window-min-height
	 (min (ilisp-output-sink-window-max-height ilisp-output-sink)
	      (max (ilisp-output-sink-window-min-height ilisp-output-sink)
		   height)))))


;; A first guess at the height needed to display this buffer.
(defun ilisp-needed-buffer-height (buffer)
  (save-excursion
    (set-buffer buffer)
    (1+ (count-lines (point-min) (point-max)))))


;; The height this window must be to display its entire buffer.
(defun ilisp-needed-window-height (window)
  (save-window-excursion
    (select-window window)
    (save-excursion
      (set-buffer (window-buffer))
      ;; 19990806 Marti Atzmueller
      ;; Changed 2 to 3 just below.
      (+ 3 (save-excursion 
	     (goto-char (point-min))
	     ;; Any upper bound on the height of an emacs window will
	     ;; do here.  How about 1000.
	     (vertical-motion 1000))))))


(defun ilisp-shrink-wrap-window (window ilisp-output-sink)
  (let ((previously-selected-window (selected-window))
	(buffer (window-buffer window)))
    
    (select-window window)
    (let* ((current-height (window-height window))
	   (desired-height (ilisp-desired-height ilisp-output-sink t))
	   (delta (- desired-height current-height)))
      (enlarge-window delta)
      (set-buffer buffer)
      (goto-char (point-min))

      ;; Now repair damage to the window below us, if it still exists.
      (let ((lower-window (ilisp-find-lower-window window)))
	(when lower-window
	  (select-window lower-window)
	  (let ((old-point (point)))
	    (goto-char (window-start))
	    (vertical-motion delta)
	    (set-window-start lower-window (point))
	    (goto-char old-point)
	    (when (not (pos-visible-in-window-p old-point))
	      (recenter 0)))))
      ;; If there was no lower window, then we ought to preserve
      ;; the start of the window above us, if any.

      (when (ilisp-window-live-p previously-selected-window)
	(select-window previously-selected-window)))))


;;; ilisp-shrink-wrap-window-and-frame --
;;; I need this one to change the also the frame size.

(defun ilisp-shrink-wrap-window-and-frame (window ilisp-output-sink)
  (let ((previously-selected-window (selected-window))
	(buffer (window-buffer window))
	(frame (window-frame window)))
    (select-window window)
    (let* ((current-height (window-height window))
	   (desired-height (ilisp-desired-height ilisp-output-sink t))
	   (delta (- desired-height current-height))
	   (frame-desired-height
	    (max (ilisp-output-sink-frame-min-height ilisp-output-sink)
		 desired-height)))
      (set-buffer buffer)
      (goto-char (point-min))

      ;; Now repair damage to the window below us, if it still exists.
      ;;
      ;; 19991220 Marco Antoniotti
      ;; This is probably useless, since the '*ilisp-message-frame*'
      ;; only has a single window.
      (let ((lower-window (ilisp-find-lower-window window)))
	(when lower-window
	  (select-window lower-window)
	  (let ((old-point (point)))
	    (goto-char (window-start))
	    (vertical-motion delta)
	    (set-window-start lower-window (point))
	    (goto-char old-point)
	    (unless (pos-visible-in-window-p old-point)
	      (recenter 0)))))

      ;; If there was no lower window, then we ought to preserve
      ;; the start of the window above us, if any.

      (when (ilisp-window-live-p previously-selected-window)
	(select-window previously-selected-window))

      ;; Finally shrink the frame.
      (set-frame-size frame
                      (ilisp-output-sink-frame-min-width ilisp-output-sink)
		      frame-desired-height))))


(defun ilisp-window-live-p (window)
  (window-live-p window))

;;; This old implementation ignores windows in other frames, 
;;; which makes a lot of trouble if the ILISP buffer is shown in 
;;; a single dedicated window in a frame.

;;(defun ilisp-window-live-p (window)
;;  (let* ((initial-window (selected-window))
;;       (win initial-window)
;;       (found nil))
;;    (while win
;;      (cond ((eq window win)
;;           (setq found t
;;                 win nil))
;;          (t
;;           (setq win (next-window win 'no))
;;           (if (eq win initial-window)
;;               (setq win nil)))))
;;    found))


;; XEmacs change -- window-edges is gone in 19.12+ so use
;; next-vertical-window instead.

(defun ilisp-find-lower-window (window)
  "Find the window directly below us, if any.
This is probably the window from which enlarge-window would steal lines."
  (if (or (not (string-match "XEmacs" emacs-version))
	  (and (= emacs-major-version 19)
	       (< emacs-minor-version 12)))
      (let* ((bottom (nth 3 (window-edges window)))
	     (window* nil)
	     (win window))
	(while (not (eq (setq win (next-window win 'no))
			window))
	  (if (and (= (nth 1 (window-edges win))
		      bottom)
		   (null window*))
	      (setq window* win)))
	window*)
    (next-vertical-window window)))


;;; ilisp-find-top-left-most-window --
;;;
;;; Notes:
;;; 19980101
;;; XEmacs change -- There is now a primitive to do this.
;;;
;;; 19991219 Marco Antoniotti
;;; It would seem that also Emacs 20.xx has a built in function for
;;; doing this (either frame-first-window or frame-top-window at least
;;; as of 20.4).  However, I leave it as it is just for safety and
;;; history.  The only changes I make are to make it a DEFUN* with an
;;; optional parameter and to change the way the local variable
;;; 'window*' is bound.

(defun* ilisp-find-top-left-most-window (&optional (frame (selected-frame)))
  "Return the leftmost topmost window on the current screen."
  (if (or (not (string-match "XEmacs" emacs-version))
	  (and (= emacs-major-version 19)
	       (< emacs-minor-version 12)))
	  
      (frame-first-window frame)
      (frame-highest-window frame 0)))


; (defun* ilisp-find-top-left-most-window (&optional (frame (selected-frame)))
;   "Return the leftmost topmost window on the current screen."
;   (if (or (not (string-match "XEmacs" emacs-version))
; 	  (and (= emacs-major-version 19)
; 	       (< emacs-minor-version 12)))
	  
;       (let* ((window* (frame-selected-window frame))
; 	     ;; (window* (selected-window))
; 	     (edges* (window-edges window*))
; 	     (win nil)
; 	     (edges nil)
; 	     (start-window window*))
; 	(message ">>> window* %s %s %s." window* start-window frame)
; 	(while (not (eq (setq win (next-window win 'no))
; 			start-window))
; 	  (message ">>>>>> win %s." win)
; 	  (setq edges (window-edges win))
; 	  (if (or (< (car (cdr edges)) (car (cdr edges*))) ; top
; 		  (and (= (car (cdr edges)) (car (cdr edges*)))
; 		       (< (car edges) (car edges*)))) ; left
; 	      (setq window* win
; 		    edges* edges)))
; 	(message ">>> about to return window*.")
; 	window*)
;       (frame-highest-window frame 0)))


;; This causes the typeout window to be created by splitting or using the
;; top-left-most window on the current screen.  That is different behavior
;; from the popper, which always split the current window.
(defun ilisp-window-to-use-for-typeout ()
  (let ((window (ilisp-find-top-left-most-window)))
    (while (window-dedicated-p window)
      (setq window (next-window window nil 'visible)))
    window))


(defun ilisp-display-buffer-in-typeout-window (ilisp-output-sink)
  "Display buffer in a window at the top of the screen."
  (let* ((buffer (ilisp-output-sink-buffer ilisp-output-sink))
         (window (get-buffer-window buffer t)))

    ;; If buffer already has a window, keep it.
    (if (null window)
	;; Otherwise, find a window to split.
	(let* ((top-window (ilisp-window-to-use-for-typeout))
	       (new-window nil)
	       (previously-selected-window (selected-window))
	       (desired-height (ilisp-desired-height ilisp-output-sink nil)))

	  ;; The new window is always the lower one.
	  (select-window top-window)

	  ;; Always minimize redisplay (except in emacs 18).
	  (let ((split-window-keep-point nil))
	    ;; If the top window is not big enough to split, commandeer it
	    ;; entirely.
	    (if (> desired-height (- (window-height) window-min-height))
		(setq new-window top-window)
	      (setq new-window (split-window-vertically desired-height))))

	  (set-window-buffer top-window buffer)
	  ;; The height is already correct, unless there was line wrapping.
	  ;; Account for that here.
	  (ilisp-shrink-wrap-window top-window ilisp-output-sink)

	  ;; Restore selected window.
	  (if (eq previously-selected-window top-window)
	      (select-window new-window)
	      (select-window previously-selected-window)))

      ;; Simply shrink-wrap an existing window.
      (ilisp-shrink-wrap-window window ilisp-output-sink))))


;;; ilisp-get-message-frame --
;;;
;;; 19991219 Marco Antoniotti
;;; Utility function.  If we get the error it is because the function
;;; has been called in an improper context.
;;; This should not really happen, since this function is called only
;;; within 'ilisp-display-buffer-in-typeout-frame', which is called
;;; only when a window system is running.

(defun ilisp-get-message-frame (ilisp-output-sink)
  (let* ((frame (ilisp-output-sink-frame ilisp-output-sink))
         (f (or (and frame (frame-live-p frame) frame)
                (setf (ilisp-output-sink-frame ilisp-output-sink)
                      (ilisp-make-output-frame
		       (ilisp-output-sink-frame-name ilisp-output-sink))))))
    (if f
	f
      (error "ILISP: cannot build the ILISP output frame."))))

(defun ilisp-delete-message-frame (ilisp-output-sink)
  (let ((frame (ilisp-output-sink-frame ilisp-output-sink)))
    (when frame
      (when (frame-live-p frame)
        (delete-frame frame))
      (setf (ilisp-output-sink-frame ilisp-output-sink) nil))))

(defun ilisp-display-buffer-in-typeout-area (ilisp-output-sink)
  (let ((buffer (ilisp-output-sink-buffer ilisp-output-sink)))
    (cond ((and window-system ilisp-*use-frame-for-output*)
           (message "See ILISP Message Frame.")
           (ilisp-display-buffer-in-typeout-frame ilisp-output-sink))
          (t
           (message "See above.")
           (ilisp-display-buffer-in-typeout-window ilisp-output-sink)))))


(defun ilisp-display-buffer-in-typeout-frame (ilisp-output-sink)
  "Display buffer in a special ILISP frame."
  (let* ((output-frame (ilisp-get-message-frame ilisp-output-sink))
         (buffer (ilisp-output-sink-buffer ilisp-output-sink))
         (buffer-window (get-buffer-window buffer))
         (previous-output-window (selected-window)))
    (if (null buffer-window)
	;; No window is associated to the buffer.
	(unwind-protect
	    (let* ((output-frame-window
		    (ilisp-find-top-left-most-window output-frame))
		   (desired-height
		    (ilisp-desired-height ilisp-output-sink nil)))
	      (select-window output-frame-window)
	      (set-window-buffer output-frame-window buffer)
	      (ilisp-shrink-wrap-window-and-frame output-frame-window
                                                  ilisp-output-sink)
	      (unless (frame-visible-p output-frame)
		(make-frame-visible output-frame))
              (raise-frame output-frame))
	  (progn
	    (select-window previous-output-window)
	    (select-frame (window-frame (selected-window)))))
      ;; else
      (progn
	(ilisp-shrink-wrap-window-and-frame buffer-window ilisp-output-sink)
	;; Let's try to display the buffer window in the output frame.
	(unless (and (eq (window-frame buffer-window)
			 output-frame)
		     (not (frame-visible-p output-frame)))
	  (make-frame-visible output-frame))
        (raise-frame output-frame)))))



;;; lisp-display-output - general output function

(defun lisp-display-output (output)
  "Displays OUTPUT in the appropriate place.
This calls the function given by the value of ILISP-DISPLAY-OUTPUT-FUNCTION in
order to do the real work."
  (when output
    ;; Bugcheck
    (unless (stringp output)
      (error "ILISP: not a string in lisp-display-output"))
    
    (when (ilisp-value 'comint-errorp t)
      (setq output (funcall (ilisp-value 'ilisp-error-filter) output)))
    (let ((ilisp-output-sink
	   (ilisp-get-sink-for-command this-command ilisp-output)))
      (funcall ilisp-display-output-function output ilisp-output-sink))))


;;; Various functions to which to bind ilisp-display-output-function.

;;; ilisp-display-output-default --
;;; This function does what ilisp used to do, except that we use the
;;; new "popper".
;;;
;;; Notes:
;;; 2000-01-22 Martin Atzmueller: force prompt in inferior-lisp-buffer
;;; after an error.

;;; 19990806 Martin Atzmueller
;;; Added check for COMINT-ERRORP.

(defun ilisp-force-output-after-error (ilisp-output-sink)
  "Force the ilisp buffer to display the prompt."
  "Display output in the ilisp buffer"
  (let ((buffer (current-buffer))
	(window (selected-window)))
    ;; we want _exactly_ one prompt
    ;; this means this function has only to be performed once.
    ;; so check for ilisp-last-message!
    (if ilisp-last-message
        (unwind-protect
	    (progn
	      (lisp-pop-to-buffer (ilisp-buffer) ilisp-output-sink)
	      (if (not (eq (current-buffer) buffer))
		  (setq ilisp-last-buffer buffer))
	      (comint-insert 
	       (concat 
		(if ilisp-last-message
		    (concat ";;; " ilisp-last-message "\n"))
		"\n"
		ilisp-last-prompt))
	      (setq ilisp-last-message nil))
          (if (window-point window)
              (progn (select-window window)
                     (set-buffer buffer)))))))


(defun ilisp-display-output-default (output ilisp-output-sink)
  "Displays 'output' depending on the value of 'lisp-no-popper'.
Dispatch on the value of 'lisp-no-popper':
 'lisp-no-popper' = nil:  displays 'output' in a typeout window.
 'lisp-no-popper' = t:    displays 'output' in the ilisp buffer
 otherwise:               displays one-line 'output' in the echo area,
                          multiline output in the ilisp buffer."
  (cond ((null lisp-no-popper)
	 (ilisp-display-output-in-typeout-window output ilisp-output-sink))

	((eq lisp-no-popper t)
	 (ilisp-display-output-in-lisp-listener output ilisp-output-sink))

	(t
	 (ilisp-display-output-adaptively output ilisp-output-sink)))
  
  (when (or (ilisp-value 'comint-errorp t)
	    (string-match (ilisp-value 'ilisp-error-regexp t) output))

    ;; display error-msg too, if not already in lisp-listener
    (unless (eq lisp-no-popper t)
      (ilisp-display-output-in-lisp-listener output ilisp-output-sink))
    ;; force output, e.g. for <prompt>
    (ilisp-force-output-after-error ilisp-output-sink)))


;; This is the display function I like to use.

;; Another trick which might be useful is to dispatch on the value
;; this-command here, to make output from different ilisp commands
;; go to different places.

(defun ilisp-display-output-adaptively (output ilisp-output-sink)
  "Display one-liners in the echo area, others in the typeout window"
  (cond ((or (string-match "\n" output)
	     (> (length output) (window-width (minibuffer-window))))
	 (ilisp-display-output-in-typeout-window output ilisp-output-sink))
	(t
	 (ilisp-display-output-in-echo-area output ilisp-output-sink))))


(defun ilisp-display-output-in-typeout-window (output ilisp-output-sink)
  "Display output in a shrink-wrapped window at the top of the screen."
  (let ((old-buffer (current-buffer))
	(old-window (selected-window))
	(buffer (ilisp-output-buffer ilisp-output-sink t)))
    (ilisp-write-string-to-buffer ilisp-output-sink output)
    (ilisp-display-buffer-in-typeout-area ilisp-output-sink)
    
    ;; Martin Atzmueller 2000-01-27
    ;; this-command trick:
    ;; if this-command is ilisp-message-lisp-space, switch back!
    (if (and (eql this-command 'ilisp-arglist-message-lisp-space)
             ilisp-*arglist-message-switch-back-p*
             (not (member (buffer-name old-buffer) special-display-buffer-names)))
	(progn
	  (raise-frame (window-frame old-window))
	  (switch-to-buffer old-buffer)))))

(defun ilisp-display-output-in-echo-area (output ilisp-output-sink)
  "Display output as a message in the echo area."
  ;; First clear any existing typeout so as to not confuse the user.
  (if (ilisp-output-buffer ilisp-output-sink)
      (let ((buffer (get-buffer (ilisp-output-buffer ilisp-output-sink))))
	(if (and buffer
		 (not (eq (selected-window)
			  (get-buffer-window buffer t))))
	    (ilisp-bury-output-internal ilisp-output-sink))))
  
  ;; v5.7: Patch suggested by hunter@work.nlm.nih.gov (Larry Hunter)
  ;; If output contains '%', 'message' loses.
  ;; (message (ilisp-quote-%s output))
  ;; An alternative here could be '(princ output)', as suggested by
  ;; Christopher Hoover <ch@lks.csi.com>
  ;; (princ output)

  ;; v5.7b: Patch suggested by fujieda@jaist.ac.jp (Kazuhiro Fujieda)
  ;; Best one for FSF Emacs 19.2[89].
  (message "%s" output))


;;; ilisp-quote-%s --
;;; Patch suggested by hunter@work.nlm.nih.gov (Larry Hunter)

(defun ilisp-quote-%s (string)
  "Quote all the occurences of ?% in STRING in an ELisp fashion."
  (mapconcat '(lambda (char)
		(if (char-equal char ?%)
		    "%%"
		  (char-to-string char)))
	     string ""))


(defun ilisp-display-output-in-temp-buffer (output)
  (with-output-to-temp-buffer ilisp-output-buffer
    (princ output)))


(defun ilisp-display-output-in-lisp-listener (output ilisp-output-sink)
  "Display output in the ilisp buffer"
  (let ((buffer (current-buffer))
	(window (selected-window)))
    (unwind-protect
	(progn
	  (lisp-pop-to-buffer (ilisp-buffer) ilisp-output-sink)
	  (unless (eq (current-buffer) buffer)
	    (setq ilisp-last-buffer buffer))
	  (comint-insert 
	   (concat 
	    (when ilisp-last-message
              (concat ";;; " ilisp-last-message "\n"))
	    (comint-remove-whitespace output)
	    "\n"
	    ilisp-last-prompt))
	  (setq ilisp-last-message nil))
      (when (window-point window)
	(select-window window)
	(set-buffer buffer)))))


(defun lisp-pop-to-buffer (pbuffer &optional ilisp-output-sink set-input-focus-p)
  "Like pop-to-buffer, but select a screen that buffer was shown in.
ilisp-output-sink is the last ilisp-output-sink visited/active or nil
if this is not relevant."
  (let* ((buffer (or pbuffer
		     (when ilisp-output-sink
		       (ilisp-output-sink-buffer ilisp-output-sink))))
         (window (if ilisp-epoch-running
                     (epoch::get-buffer-window buffer)
                   (get-buffer-window buffer t)))
         (frame  (when window (window-frame window))))
    (cond ((not window)
           (when (and ilisp-output-sink
		      (get-buffer (ilisp-output-sink-buffer ilisp-output-sink)))
	     ;; is this necessary?
             (ilisp-bury-output-internal ilisp-output-sink))
           (pop-to-buffer buffer))
          (set-input-focus-p
           (if (fboundp 'select-frame-set-input-focus)
               (select-frame-set-input-focus frame)
             (raise-frame frame)
             (select-frame frame)
             (focus-frame frame))
           (select-window window))
          (t (when (or (memq (frame-visible-p frame) '(nil icon))
                       (when (fboundp 'frame-iconified-p)
                         (frame-iconified-p frame)))
               (raise-frame frame)
               (raise-frame (selected-frame)))
             (select-frame frame)
             (select-window window)))
    (set-buffer buffer)))


(defun switch-to-lisp (eob-p &optional ilisp-only)
  "If in an ILISP buffer, switch to the last non-ILISP buffer visited.
Otherwise, switch to the current ILISP buffer.  With argument,
positions cursor at end of buffer.  If you don't want to split
windows, set pop-up-windows to NIL."
  (interactive "P")
  (if (and (not ilisp-only) ilisp-last-buffer 
	   (memq major-mode ilisp-modes))
      (lisp-pop-to-buffer ilisp-last-buffer nil t)
    (unless (memq major-mode ilisp-modes)
      (setq ilisp-last-buffer (current-buffer)))
    (lisp-pop-to-buffer (ilisp-buffer) nil t)
    (when eob-p
      (goto-char (point-max)))))

;;; end of file -- ilisp-out.el --
