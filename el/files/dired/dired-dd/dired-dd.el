;;; dired-dd.el --- Drag and Drop interface to dired/dired-x package

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998,1999,2000,2001 Seiichi Namba <sn@asahi-net.email.ne.jp>
;;

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;
;;  Version 0.9   ---- means still alpha (almost bata ?)
;;
;; 0.0.0.0: Sun Nov 30 19:49:28 1997
;; 0.0.0.1: Wed Dec 10 14:53:39 1997
;; 0.0.0.2: Thu Dec 11 14:25:10 1997 Dialog interface thrown away
;;                                   (harmful for close button)
;; 0.0.0.3: Wed Dec 17 21:58:21 1997 Supported C,M,S modifier at drop timing
;; 0.0.0.4: Sat Dec 20 23:17:05 1997 Nonofficial sound support, shell cmds
;; 0.0.0.5: Mon Dec 22 14:51:59 1997 Inter-frame drag & drop
;; 0.0.0.6: Thu Dec 25 16:32:28 1997 Split dired-dd-open-or-execute into five
;; 0.0.0.7: Mon Dec 29 02:05:33 1997 Support Open/Execute by button2
;; 0.0.0.8: Sat Jan  3 23:32:23 1998 Dired-DD implemented as minor mode.
;;	    Mon Jan  5 11:21:25 1998 Public release as 0.0.0.8
;; 0.9 <= 0.0.0.9 Fri Jan  9 14:30:44 1998
;;               first "0.0." of ver # removed for RCS + vc (version control) 
;; 0.9.1.2,3: Wed Jan 14 23:36:33 1998 copy-recursive, async shell-command
;; 0.9.1.3: Tue Jan 20 19:27:36 1998 Made public very shortly on 1/22/98
;; 0.9.1.4: Thu Jan 22 11:40:15 1998 Minor fix, provide/run-hooks order.
;; 0.9.1.5: Fri Jan 23 23:51:05 1998 started: just to freeze published 0.9.1.4
;;          Sun Jan 25 14:29:44 1998 Fixed interframe BUG (mark was not retained).
;; 0.9.1.6: Thu Jan 29 12:35:52 1998 Now dired-dd is under GPL.
;; 0.9.1.7: Sat Jan 31 00:32:44 1998 Minor fix. Root window drop supported.
;; 0.9.1.8: Sat Feb  7 13:56:43 1998 New branch.
;; 0.9.1.9: Sat Feb  7 13:56:43 1998 Fix on root window drop.
;; 0.9.1.10: Fri Jun 19 03:21:16 1998 Now double-mouse-1 is for exec-or-open.
;; 0.9.1.11: Wed Dec  9 17:37:41 1998 Non-dired target buffer (window) 
;;                                    handler support.
;;                                    New var and func for that:
;;                                    var: dired-dd-non-dired-drop-handlers
;;                                    fun: dired-dd-exec-non-dired-drop-handlers
;; 0.9.1.12: Tue Dec 13 21:45:28 1998 Plays audio on a successful
;;                                    dired-dd-exec-non-dired-drop-handlers call.
;;                                    More generic dired-dd-move-to-xy-in-window.
;; 0.9.1.13: Sat Dec 19 17:42:32 1998 Almost the same as previous except
;;                                    dired-dd-insert-fname.el.
;; 0.9.1.14: Sun Dec 20 02:35:31 1998 Almost the same as previous except
;;                                    dired-dd-b3-menu.el, which was fixed
;;                                    according suggestion by
;;                                    Yuji Yamano <yyamano@kt.rim.or.jp>
;;                                    Slight cleanup of comments.
;; 0.9.1.15: Tue Dec 29 23:52:41 1998 dired-dd-drop-on-root-window fix concerning
;;                                    coding style consistency.
;;                                    dired-dd-exec-command-directly(-20) fixed
;;                                    for filenames including space or shell
;;                                    meta-characters (added double-quoting).
;;                                    Similar fixes on dired-dd-b3-menu.el.
;; 0.9.1.16: Tue Mar  2 19:05:18 1999 Fixes,fixes,fixes...
;; 0.9.1.17: Tue Apr  4 19:31:10 2000 Further small fixes.
;; 0.9.1.18: Tue Apr  4 19:31:10 2000 Added item to call w3-open-local B3 menu.
;; 0.9.1.19 - 0.9.1.20:               A lot of changes. See NEWS/ChangeLog.
;; 0.9.1.21: Sun Jun 10 01:17:03 2001 Workarounds for emacs-21, and fvwm2 (CVS version).
;; 0.9.1.23: Tue Aug 21 14:10:23 2001 Fix on dired-dd-exec-async-shell-command().
;;                                    Support emacs-w3m (in b3 menu).
;;                                    Recursive deletion/copy for ftp dir available.
;; 0.9.1.24: 2002-01-01               A new non-dired-drop-handler for emacs-w3m.
;; 0.9.1.25: Thu Jan 10 15:19:44 2002 emacs-w3m non-dired-drop-handler enhancement.
;; 0.9.1.26,27: 2002-04-12            Fixes on root-window-drop.
;; 0.9.1.28:			      Cleanup buffer on recursive rm.

(defconst dired-dd-version "0.9.1.28")

;;
;; Major Mouse button binding provided with this package:
;;
;; [mouse-1] :
;;      
;;   Drag:
;;      Drag region to toggle mark files with (*). 
;;      Move horizontally a bit to mark/unmark single file/directory.
;;      Dragged region is copied in the kill ring as a side effect.
;;      Secondary selection handling with M-*-mouse-1 is disabled at all.
;;	
;;      Modifier keys are not supported in dragging yet.
;;
;;   Double click:
;;      Double-click supported experimentally to open, or execute
;;      shell commands on file/directory, invoking same commands bound
;;      to mouse-2 click.  C-, M-, S- modifier keys works similarly in
;;      case of button 2 (see below), except that you can't pass
;;      prefix arg to the file open/execute commands.
;;      
;;      This Windose-Mac flavor may be removed in future release, but I am
;;      curious how many users like or dislike this feature.  I do not
;;      recommend to rely on it, or you may become double-click
;;      everywhere (eg. Gnus group buffer...) in Emacs.
;;
;; [mouse-2] :
;;
;;   Drag:
;;      Drag marked (or on point) files/dir and drop somewhere.  You
;;      can drop the object into another dired buffer, into directory
;;      line in the same dired buffer.  See what happens even dropping
;;      into non-dired buffer, or just drag horizontally.
;;
;;      On drag and drop into a directory object (dired buffer or
;;      directory line in the same dired buffer), a menu is popped up
;;      to ask file operation method (copy, move, etc.).
;;      The menu includes basic (minimum number of) file operations.
;;
;;      In case of drag and drop, the selected job with menu is
;;      immediately executed (copy, move, link etc.), while minibuffer
;;      prompt is raised if target directory can not be determined.
;;
;;      The same menu is popped upon horizontal drag.  Thes means that
;;      you made drag & drop into the same directory.  Of course,
;;      the destination of copy/move/link etc. is asked also in this case.
;;
;;      Create a directory or two to test drag and drop operations.
;;
;;      Drag and drop into non-directory object (other non-dired
;;      buffer), the file/dir object is just opend using the window to
;;      which they are dropped in.
;;
;;      Dragging with Ctrl/Meta/Shift is experimentally suppoerted.
;;      A Windows/Mac flavoured operation.
;;
;;      These modifiers are detected at very timing of mouse button
;;      release (as in XV's Visual Schunauzer), so you may start drag
;;      with [down-mouse-2], then press Ctrl/Meta/Shift before release
;;      button to select file operation method even if you rebind
;;      C-/M-/S-mouse-2 to other functions.
;;
;;      Drag and drop into another frame is now supported after
;;      version 0.0.0.5.  I don't think my coding for interframe drop
;;      is just the thing, but I could not find other alternative.
;;      But, we can't use events generated out of track-mouse, right ?
;;      
;;   Click (Single):
;;      Clicking mouse-2 opens, or executes shell commands on 
;;	file/directory.  A mere click opens file using currently
;;      selected window.
;;
;;      Several modifiers are now supported upon click:
;;
;;         C-S-mouse-2:   Opens file/directory using other window
;;         M-mouse-2:     Executes command related the file extension
;;         C-M-S-mouse-2: Execute command with minibuffer prompt
;;
;;      C-h v dired-guess-shell-alist-user RET shows you how to
;;      determine the command for certain file extension ("*.jpeg" =>
;;      execute xv, etc).  This feature relies on  dired-x.
;;
;;      Rough and ready interactive command dired-dd-register-shell-alist
;;      is provided to add entry in dired-guess-shell-alist-user.
;;
;; [mouse-3] :
;;
;;      Raise a full menu (not so `full' as dired menu bar, but good
;;      enough for daily use, I suppose) to ask handling for marked
;;      (or on point) files delete/copy/move... etc.  S-mouse-3 (Shift
;;      + button3) raises original dired menu-bar menu as popup menu,
;;      which is full-featured one in Emacs-19.xx.  You can use
;;      C-mouse-3 menu as it is in emacs-20.
;;
;; See attached documents for more detailed informations.
;;

;;
;; Installation:
;;
;;  This package uses dired-x package extensively.
;;  To load dired-dd.el, include `add-hook' codes below in you ~/.emacs.
;;
;;  (add-hook
;;   'dired-load-hook
;;   (function
;;    (lambda ()
;;      (load "dired-x")
;;      ;; Set dired-x variables here.
;;      ;; To and flo...
;;      (if window-system (require 'dired-dd)))))
;;
;;  Dired-x should be included in normal emacs distribution (you don't
;;  have to ftp it).  Read info of dired-x for datail.
;;

;;
;; Platforms:
;;
;;  dired-dd.el was written and tested on mule-2.3 (emacs-19.28 based)
;;  and emacs-20.2.  Works fine on both, but some behaviour (eg. menu
;;  appearance) may not exactly the same.  No support for emacs-18
;;  (Nemacs).  I have not slightest idea that dired-dd works or not in
;;  XEmacs.  I have just tested one or two version in spring to summer
;;  in 1997, at that time this program did not exist at all.
;;

;;
;; Customize:
;;  Refer to attached documents (info, or html version of it).
;;  Also see document strings of each variables having dired-dd prefix.
;;

;;
;; While 0.9.1.3 updating, all of a sudden, this eval-and-compile
;; became necessary.  Without this, -batch-byte-compile'ed file (by make)
;; barfs at dired-map-over-marks in dired-dd-get-marked-files.  I know
;; this is a macro in dired.el, but why didn't it happen in 0.0.0.8 or in
;; plain 0.9 ?
;;
(eval-and-compile
  (require 'dired) ;; probably only this is necessary to make working elc.
  (require 'dired-aux)
  (require 'dired-x)
  (require 'dired-dd-b3-menu)
  (require 'cl))

;; 
;; Dired-dd minor mode definition.
;; Minor mode skelton was derived from $lisp/view.el.
;; Sat Jan  3 22:09:06 1998
;;
(defvar dired-dd-mode nil "Non-nil if dired-dd mode is enabled.")
;;;###autoload
(make-variable-buffer-local 'dired-dd-mode)

(or (assq 'dired-dd-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(dired-dd-mode " DragDrop") minor-mode-alist)))

(defvar dired-dd-mode-map
  (let ((dired-dd-mode-map (make-sparse-keymap)))

    (define-key
      dired-dd-mode-map [C-M-down-mouse-1] 'dired-dd-copy-filename-as-kill)
    (define-key
      dired-dd-mode-map [C-M-down-mouse-3] 'dired-dd-unmark-all-marks)
    ;; Sat Nov  4 17:44:06 2000 
    ;; to bind C-M-down-mouse-2 to dired-dd-popup-shell-alist-menu()
    ;; via dired-dd-drag-drop().
    ;;
    ;; Convention conflicts C-M-down-mouse-1, and C-M-down-mouse-3,
    ;; Sashite-mondai-janai-wa..., she says.
    (define-key
      dired-dd-mode-map [C-M-mouse-2] 'dired-dd-copy-filename-as-kill-full)
    (define-key
      dired-dd-mode-map [C-M-down-mouse-2] 'dired-dd-popup-shell-alist-menu)
    ;; Was in dired-ddpopup-shell-alist.el, merged.
    (define-key
      dired-dd-mode-map [S-down-mouse-3]   'dired-dd-popup-shell-alist-menu)

    (define-key dired-dd-mode-map [C-return] 'revert-buffer)
    (define-key dired-dd-mode-map [M-down-mouse-1] 'dired-dd-drag-region) ;;;
    ;; Not yet (for dired-dd-unmark-in-region).
    ;; (define-key dired-dd-mode-map [M-C-down-mouse-1] 'dired-dd-drag-region)
    (define-key dired-dd-mode-map [C-S-down-mouse-1] 'dired-dd-nop)
    (define-key dired-dd-mode-map [C-M-S-down-mouse-1] 'dired-dd-nop)
    (define-key
      dired-dd-mode-map [C-M-S-double-mouse-1] 'dired-dd-exec-command-query)
    (define-key
      dired-dd-mode-map [M-double-mouse-1] 'dired-dd-exec-command-directly)
    (define-key
      dired-dd-mode-map [C-S-double-mouse-1] 'dired-dd-find-file-other-window)
    (define-key dired-dd-mode-map [down-mouse-1] 'dired-dd-drag-region)
    (define-key dired-dd-mode-map [C-down-mouse-2] 'dired-dd-drag-drop)
    (define-key dired-dd-mode-map [M-down-mouse-2] 'dired-dd-drag-drop)
    (define-key dired-dd-mode-map [S-down-mouse-2] 'dired-dd-drag-drop)
    (define-key dired-dd-mode-map [down-mouse-2] 'dired-dd-drag-drop)

    ;; From 0.9.1.10, double-mouse-1 is for generic exec-or-open.
    (define-key dired-dd-mode-map [mouse-2] 'dired-dd-find-file) ; Same old
    (define-key
      dired-dd-mode-map [M-mouse-2] 'dired-dd-exec-command-directly) ; Same old
    (define-key
      dired-dd-mode-map [double-mouse-1] 'dired-dd-find-file-or-exec) ; New

    (define-key dired-dd-mode-map [C-M-S-mouse-2] 'dired-dd-exec-command-query)
    ;; From 0.9.16, more rational find-other-win
    (define-key dired-dd-mode-map [S-mouse-2] 'dired-dd-find-file-other-window)
    (define-key dired-dd-mode-map [C-mouse-2] 'dired-dd-view-file)

    (define-key
      dired-dd-mode-map [C-S-mouse-2] 'dired-dd-find-file-other-window)
    (define-key dired-dd-mode-map [C-S-down-mouse-2] 'dired-dd-drag-drop)
    (define-key dired-dd-mode-map [C-M-S-down-mouse-2] 'dired-dd-drag-drop)
    (define-key dired-dd-mode-map [mouse-3] 'dired-dd-no-destination-handling)
    dired-dd-mode-map))

(or (assq 'dired-dd-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'dired-dd-mode dired-dd-mode-map) minor-mode-map-alist)))

(defvar dired-dd-mode-hook nil
  "Normal hook run when starting to dired-dd mode.")

(defun dired-dd-mode (&optional arg)
  "\

Enables Drag & Drop interface for dired buffer.

Keybindings:

\\{dired-dd-mode-map}"
  (interactive "P")
  (cond
   ((and arg
	 (if (> (prefix-numeric-value arg) 0)
	     dired-dd-mode (not dired-dd-mode)))
    ())					; Do nothing if already OK.
   (dired-dd-mode (dired-dd-mode-disable))
   (t (dired-dd-mode-enable))))

(defvar dired-dd-awakening nil
  "*Signify a road to awakening everytime you open dired buffer.")
;; May be done by hooking
;;(add-hook 'dired-after-readin-hook 'dired-dd-the-ball-of-fire t)

(defun dired-dd-mode-enable ()
  "Turn on dired-dd mode."
  (if (<= (string-to-int emacs-version) 19)
      (add-hook 'change-major-mode-hook 'dired-dd-mode-disable nil)
    (make-local-hook 'change-major-mode-hook)
    (add-hook 'change-major-mode-hook 'dired-dd-mode-disable nil t))
  (setq dired-dd-mode t)
  (force-mode-line-update)
  (if dired-dd-awakening
      (dired-dd-the-ball-of-fire t))
  (run-hooks 'dired-dd-mode-hook))

(defun dired-dd-mode-disable ()
  "Turn off dired-dd mode."
  (if (> (string-to-int emacs-version) 19)
      (remove-hook 'change-major-mode-hook 'dired-dd-mode-disable t)
    (remove-hook 'change-major-mode-hook 'dired-dd-mode-disable))
  (setq dired-dd-mode nil))

(defvar dired-dd-mode-as-default t
  "*Determines whether to use dired-dd as default minor mode.
Set to non-nil if you want to enable dired-dd mode when you opened a
dired buffer. If you don't, set to nil before you load dired-dd.el.

Default: t")
;; Enable dired-dd-mode within dired-mode
(if dired-dd-mode-as-default
    (add-hook 'dired-mode-hook 'dired-dd-mode-enable))

;;
;; Version print, jingle, or sing and dance.
;; Thu Jan  8 02:23:52 1998 profiling for good animation added in 0.0.0.9
;;
(defun dired-dd-version-print ()
  (interactive)
  (message "dired-dd version: %s" dired-dd-version))

(defvar dired-dd-no-fancy-stuff nil
  "*Try to set this to non-nil if your OS is disabled, \
or something weird is happening.
This disables more facilities than `dired-dd-inhibit-bogo-profile' does.")

(defvar dired-dd-inhibit-bogo-profile dired-dd-no-fancy-stuff
  "*If non-nil, X server profiling is never executed.
`dired-dd-set-mouse-color-usec' and `dired-dd-version-circle-resolution'
are set to really bogus values and all the fancy stuffs are disabled
after all. On loading `dired-dd', `dired-dd-no-fancy-stuff'
value is inherited as default.")

;; Two profiling functions to determine X-pointer animation speed.
(defun dired-dd-bogo-profile (sexp count)
  "Eval SEXP COUNT times and returns laptime in \(secdelta . usecdelta\) \
format."
  (let (st en secdelta tem)
    (setq st (current-time))
    (while (>= (setq count (1- count)) 0) (eval sexp))
    (setq en (current-time))
    (cons (setq secdelta (- (nth 1 en) (nth 1 st)))
	  ;; usecdelta part
	  ;; Took care of overflow in usec part.
	  ;; Sun Oct 29 17:44:44 2000
	  (cond
	   ((= secdelta 0)
	    (- (nth 2 en) (nth 2 st)))
	   (t
	    (setq tem
		  (+ (nth 2 en) (- 1000000 (nth 2 st))))
	    (if (< tem 1000000)
		tem
	      ;; otherwise, usec part overflowed.
	      (setq secdelta (1+ secdelta))
	      (- tem 1000000))))
;;;	  (if (= secdelta 0)
;;;	      (- (nth 2 en) (nth 2 st))
;;;	    (+ (nth 2 en)
;;;	       (- 1000000 (nth 2 st))))
	  )))

(defun dired-dd-bogo-perf (sexp count)
  "Calls dired-dd-bogo-profile COUNT times passing SEXP and COUNT, \
and averaged timing list '\(sec . usec\) returned.
The same COUNT is handed to dired-dd-bogo-profile.  Thus:

\(dired-dd-bogo-perf '\(bogofunc\) 10\) means calling dired-dd-bogo-profile
10 times, and each dired-dd-bogo-profile executes bogofunc x10 times, 
and 10 outputs of dired-dd-bogo-profile is averaged."
  (garbage-collect)
  (let ((sectotal 0) (usectotal 0) result)
    (mapcar '(lambda (dummy)
	       (setq result (dired-dd-bogo-profile sexp count)
		     sectotal (+ sectotal (car result))
		     usectotal (+ usectotal (cdr result))))
	    (make-list count 'x))
    (cons (/ sectotal count) (/ usectotal count))))

;; More generic version. overflow danger reduced, but lot of /'s,
;; so diviation might be large (?). The one above seems to be accurate
;; (should take care of overflow)
;;(defun dired-dd-bogo-perf-generic (sexp count)
;;  (let ((secavg 0) (usecavg 0))
;;    (mapcar '(lambda (result)
;;	       (setq secavg (+ secavg (/ (car result) count))
;;		     usecavg (+ usecavg (/ (cdr result) count))))
;;	    (mapcar '(lambda (dummy)
;;		       (dired-dd-bogo-profile sexp count))
;;		    (make-list count 'x)))
;;    (cons secavg usecavg)))

;; Host miles: (millenium 4MB, 16bpp, 1600x1200, P5 166MHz, 1170rpm-Xengine)
;; On this miles's local display, (0 . 32018) to (0 . 37280) resulted from
;;        (dired-dd-bogo-xperf '(set-mouse-color nil) 10)
;; So, if (dired-dd-circle-resolution = 1degree) shows nice dired-dd-version 
;; on miles's local display, approx 35000 uSec (dired-dd-set-mouse-color-usec)
;; compares to that 1degree.

;; Mmmmmmmm...  But,
;;        (setq dired-dd-bogo-xperf-count 10)
;;        (dired-dd-bogo-perf '(eval-current-buffer) 1) => (1 . 369603)
;;        (dired-dd-bogo-perf '(load "dired-dd.elc") 1) => (1 . 419227)
;; these result sounds too slow.  What's wrong ?  Seems like no that slow
;; actually, leave 30000 ~ 37000 and count=10 as it is for a little while.

;; Experiment:
;; dired-dd-bogo-xperf-count reduced to 10 => 5 to shorten loading time.
;; dired-dd-set-mouse-color-usec-per-degree-reference =>  35000/2 
;;        (dired-dd-bogo-perf '(eval-current-buffer) 1) => (0 . 340287)
;;        (dired-dd-bogo-perf '(load "dired-dd.elc") 1) => (0 . 136217)
;; These sounds too fast.  load count = 1 is the problem ?
;; I don't know.  Sounds like working in anyway.

(defvar dired-dd-circle-resolution 20
  "*Delta of arc in degrees while x-pointer is traveling \
along the Boll Of Fire.
Increase if your machine is slow.  Default is 20.")

(fset 'dired-dd-bogo-xperf 'dired-dd-bogo-perf)

;;
;; These const's are initialized every evaluation time.
;;
;; dired-dd-set-mouse-color-usec-per-degree-reference /
;; dired-dd-bogo-xperf-count => reduced to 1/2 each (from 36000/10),
;; for dired-dd-version-constants-calibration takes 3-4 sec if
;; we are driving emacs through 10Base ethernet, which is irritating.
;; What if we are over isdn or modem (Forget about it, all the other stuff
;; are slowed down) ?
(defconst dired-dd-set-mouse-color-usec-per-degree-reference 18000
  "A magic number.  Unit: microsecond.
Compares to reference machine's laptime of n \(set-mouse-color nil \) calls,
wherein n = dired-dd-bogo-xperf-count.  You must change this
if dired-dd-bogo-xperf-count is modified.")
(defconst dired-dd-bogo-xperf-count 5
  "If this count is changed, you should \
modify dired-dd-set-mouse-color-usec-per-degree-reference as well.")

;; Func calling two defconst (setq with doc string). unusual.
(defun dired-dd-version-constants-calibration ()
  (defconst dired-dd-set-mouse-color-usec
    (dired-dd-bogo-xperf '(set-mouse-color nil) dired-dd-bogo-xperf-count)
    "Average laptime in microsecond of dired-dd-bogo-xperf-count times of \
set-mouse-color call.
Actually derived from
\(dired-dd-bogo-xperf '\(set-mouse-color nil\) dired-dd-bogo-xperf-count\)")
  (defconst dired-dd-version-circle-resolution
    (if (fboundp 'atan)
	;; Guess float number is accepted most of current emacsen.
	(if (= 0 (car dired-dd-set-mouse-color-usec))
	    (/ (float (cdr dired-dd-set-mouse-color-usec))
	       dired-dd-set-mouse-color-usec-per-degree-reference)
	  dired-dd-circle-resolution)	; = 20 (see above)
      ;; else no floating point in this emacs: unused anyway.
      nil)
    "Special value for dired-dd-circle-resolution while executing \
function dired-dd-version."))

;; Do the first calibration here:
;; Please be patient if your network is slow and running emacs over it.
;;
;; With non-nil dired-dd-inhibit-bogo-profile, calibration won't run.
;; Tue Mar 14 12:52:52 2000
(if (null dired-dd-inhibit-bogo-profile)
    (dired-dd-version-constants-calibration)
  (setq dired-dd-set-mouse-color-usec (cons 10 999999)
	dired-dd-version-circle-resolution 'bogus))

;; (dired-dd-circle-resolution dired-dd-version-circle-resolution) part is new.
(defun dired-dd-version (&optional arg)
  "Print version, and play a little while if allowed."
  (interactive "P")
  (garbage-collect)
  (let ((dired-dd-no-x-pointer-animation nil)
	(dired-dd-circle-delay 0)
	;;(dired-dd-play-audio nil) ;uncomment if force silent totally here.
	(dired-dd-circle-resolution dired-dd-version-circle-resolution))
    (dired-dd-audio-play-by-method 'start)
    (dired-dd-the-ball-of-fire arg 1.02 t) ; adaptive: t
    ;;(dired-dd-audio-play-by-method 'move)
    (mapcar '(lambda (blink)
	       (dired-dd-set-drag-pointer) (sit-for (car blink))
	       (dired-dd-restore-drag-pointer) (sit-for (cdr blink))
	       ) '((0.2 . 0.1) (0.2 . 0.1) (0.2 . 0.1) (0.4 . 0)))
    ;;(dired-dd-version-print)
    ;; Recalibration always may be annoying.
    ;;(dired-dd-version-constants-calibration) ; also a part of animation (?)
    (message "dired-dd version: %s \(dired-dd-set-mouse-color-usec: %s\)"
	     dired-dd-version (cdr dired-dd-set-mouse-color-usec))
    (sit-for 1)))

;;
;; The wheel of fire
;;
;; 0.9.1.7: Manage to draw even in out-of-frame situation (even if caused by
;; bug of emacs or not ?) but it may not succeed always.  Probably on dropping
;; directory on root window, the wheel won't draw at all: I don't care.
(defun dired-dd-the-ball-of-fire (&optional arg rotate adaptive)
  "Dancing and Jingling.
If optional ARG is t circle center is adjusted to current mouse position.
Optional 2nd arg ROTATE specifies number of rotation.
If with optional 3rd arg ADAPTIVE ball size is determined adaptively."
  (interactive "P\nP")
  (let* ((h (frame-pixel-height)) (w (frame-pixel-width))
	 ;;(r 55)
	 (r (if adaptive (/ (if (>= h w) w h) 4.2) 55))	; diameter
	 (x0 (round (/ w 2)))		; default center (center of frame)
	 (y0 (round (/ h 2)))
	 (rot -90)
	 (rotdelta dired-dd-circle-resolution)
	 (delay dired-dd-circle-delay)
	 (rotmin (- rot (round (* 360 (or rotate 1.02)))))
	 (pos (mouse-pixel-position))
	 (fr (car pos)) (x00 (car (cdr pos))) (y00 (cdr (cdr pos))))
    ;;(or (and x00 y00)
    ;;    (setq x00 x0 y00 y0))
    (if arg (setq x0 x00 y0 y00))
;;; This occurs in mule-2.3 (sometimes, rarely).  Sounds buggy.
;;;Signalling: (wrong-type-argument integerp nil)
;;;  set-mouse-pixel-position(#<frame mule[1]: *Minibuf-0* 0x81f4e00> nil nil)
;;;  dired-dd-the-ball-of-fire(t)
    (if (or (null x0) (null y0))
	(setq x0 30 y0 30))
    (set-mouse-pixel-position fr x0 y0) (sit-for delay)
    ;;(sit-for 0.3)
    (dired-dd-set-drag-pointer)
    (while (> rot rotmin)		; counter-clockwise
      (set-mouse-pixel-position
       fr
       (+ x0 (round (* r (cos (degrees-to-radians rot)))))
       (+ y0 (round (* r (sin (degrees-to-radians rot))))))
      ;;(message "%s" rot)
      ;;(sit-for 0.000015)
      (sit-for delay)
      (dired-dd-cycle-x-pointer)
      (setq rot (- rot rotdelta)))
    (sit-for 0.22)
    (dired-dd-restore-drag-pointer)
    ;; On root drop, nil for x00 and y00 may occur...
    (and x00 y00
	 (if arg
	     (set-mouse-pixel-position fr x00 y00)))))

;; Redefinition things 
;; if this emacs running via slow X display, or on non-float machines.
;; Almost NOP, and no pointer animations.
(if (and (fboundp 'atan)
	 (not dired-dd-no-fancy-stuff)
	 ;;(string-match "^:" x-display-name)
	 (<= (car dired-dd-set-mouse-color-usec) 1))
    (defvar dired-dd-circle-delay 0.0001
      "Frame delay while x-pointer is circling.  Unit: seconds")
  (setq dired-dd-no-x-pointer-animation t) ; defvar hereafter, but force t here.
  (defconst dired-dd-circle-delay 0)
  (defun dired-dd-version (&optional arg)
    (interactive "P") (dired-dd-version-print))
  (defun dired-dd-the-ball-of-fire (&optional arg rotate adaptive)
    (interactive "P\nP") ()))

;;
;; Copy-Recursively handlers.
;;
;; This part was meant to be dired-xxx.el, but was not.
;; Copy file/directory recursively.
;; I don't know if these are working safely. Sun Jan 11 20:14:01 1998
;;
;; Support remote (ange-ftp'ed) directory, Sun Nov 25 00:29:10 2001
;;

(defvar dired-dd-keep-marker-copy-recursive ?R
  "See variable `dired-keep-marker-copy'.")

;; Fri Apr  6 21:38:52 2001
;; Format changed. Option should be setup separately.
(defvar dired-dd-copy-recursive-command "cp"
  "*Unix command to copy file or directory recursively.
The command option should be setup in `dired-dd-copy-recursive-options'.")

(defvar dired-dd-copy-recursive-options '("-a")
  "*Suitable options `dired-dd-copy-recursive-command'.
Should be a list.")

(defvar dired-dd-remote-copy-recursive-command "rcp"
  "*Unix command to remote-copy file or directory recursively.
The command option should be setup in `dired-dd-copy-recursive-options'.")

(defvar dired-dd-remote-copy-recursive-options '("-rp")
  "*Suitable options `dired-dd-copy-recursive-command'.
Should be a list.")

(defun dired-dd-convert-fname-ange-2-rcp (path)
  "Convert emacs-formatted ftp path-name PATH to rcp\(1\) format.
PATH should contain \":\" and start with \"/\" character, otherwise
PATH is returned as it is."
  (save-match-data
    (cond ((and (string-match ":" path)
		(string-match "^/+\\(.*\\)$" path))
	   (substring path (match-beginning 1) (match-end 1)))
	  (t path))))

(defun dired-dd-copy-recursive (file dest-path &optional ok-if-already-exists)
  "Copies file with executing shell command defined by \"cp -a\"

Called from `dired-dd-do-copy-recursive' one by one for each of marked files.
`dired-dd-copy-recursive-command' and `dired-dd-copy-recursive-options' are
referred. They default respectively \"cp\" and \"-a\".

Of course works only for local files.
Target file is presumably a directory \(tree\).
\(So, \) no backup is supported.

Three arguments: FILE DEST-PATH &optional OK-IF-ALREADY-EXISTS,
but OK-IF-ALREADY-EXISTS is currently always ignored."
  (interactive "FCopy recursive: \nFTo %s: \nP")
  (let (command options via-rcp)
    (cond ((or (eq (find-file-name-handler file 'shell-command)
		   'ange-ftp-hook-function)
	       (eq (find-file-name-handler dest-path 'shell-command)
		   'ange-ftp-hook-function))
	   (setq command dired-dd-remote-copy-recursive-command
		 options dired-dd-remote-copy-recursive-options
		 via-rcp t))
	  (t (setq command dired-dd-copy-recursive-command
		   options dired-dd-copy-recursive-options)))
    (cond
     ;; In this case, the cp -a creates new directory named
     ;; (file-name-nondirectory file) under `destpath' and that is not what
     ;; we want. Tue Apr  4 19:09:36 2000
     ((and (file-directory-p dest-path)
	   (string= (file-name-nondirectory file)
		    (file-name-nondirectory dest-path)))
      (setq dest-path (file-name-directory dest-path)) ))

    ;; ** `dest-path' must be directory in `via-rcp' case here after the
    ;; check, for finalized pathname of `file' has been passed in dired(-dd)).
    (when via-rcp
      (setq file (dired-dd-convert-fname-ange-2-rcp file)
	    dest-path (file-name-directory  ;; **
		       (dired-dd-convert-fname-ange-2-rcp dest-path))))

    ;; Mon Oct 23 20:20:16 2000
    ;; Fixed to use dired-shell-stuff-it() to quote shell meta-characters.
    ;; Fri Apr  6 21:33:26 2001
    ;; `dired-shell-stuff-it' unused anymore here (`file-name-coding-system'
    ;; implied).
    (condition-case nil
	;; Tue Oct 24 09:30:07 2000 revert-buffer() added to manage to
	;; show `to' window's current status (not so perfectly,
	;; especially when "cp -a"ing huge file(s)).  Now this
	;; call-process() async-executes "cp -a" with `nil 0 nil'
	;; flags.  I don't know why `nil nil nil' won't work as I
	;; intended.
	(prog1
	    (apply 'call-process command 
		   nil 0 nil (append options (list file dest-path)))
	  ;; Surely here we are in the `from' buffer (see the `fake'
	  ;; in dired-dd-dropfrom-to()), and I don't think this
	  ;; revert-buffer() take effect (really ?).
	  ;; (revert-buffer)
	  ;; Perhaps this sit-for() slightly improves the update in
	  ;; the target dired buffer: guess "/bin/ls" can detect the
	  ;; new file.
	  (sit-for 0.1))
      (error
       (error "Command failed: %s"
	      (concat command " " (mapconcat 'identity options " ")))))))

(defun dired-dd-do-copy-recursive (&optional arg)
   "Copy recursively all marked (or next ARG) files into a directory."
  (interactive "P")
  (dired-do-create-files 'copyrecursive (function dired-dd-copy-recursive)
                           "CopyRecursive"
			   arg dired-dd-keep-marker-copy-recursive)
  ;; Tue Oct 24 10:29:02 2000 added. See also dired-dd-drop-from-to(), and
  ;; dired-dd-copy-recursive(). (Not always makes sense: we can't tell which
  ;; is the target dired buffer in which revert-buffer() should be called).
  (message "Hit `g' if target dired buffer doesn't look right."))

;; I don't know if this is working good. s.n.
(defun dired-dd-do-copy-recursive-regexp
  (regexp dest-path &optional arg whole-path)
  "Copy recursively marked files containing REGEXP to DEST-PATH.
See functions `dired-dd-do-copy-recursive' and `dired-dd-copy-recursive'
for more info."
  (interactive (dired-mark-read-regexp "CopyRecursive"))
  (dired-do-create-files-regexp
   (function dired-dd-copy-recursive)
   "CopyRecursive" nil regexp dest-path whole-path
   dired-dd-keep-marker-copy-recursive))

;; Sat Apr  7 16:13:16 2001
;; `dired-dd-shell-rm-R-command' and `dired-dd-shell-rm-R' are deprecated
;; in dired-dd-b3-menu.el.

(defvar dired-dd-delete-recursive-command "rm"
  "*Unix command to delete file or directory recursively.
The command option should be setup in `dired-dd-delete-recursive-options'.")

(defvar dired-dd-delete-recursive-options '("-rf")
"*Suitable options `dired-dd-delete-recursive-command'.
Should be a list.")

(defun dired-dd-delete-recursive (fn-list)
  (when (yes-or-no-p
	 (format
	  "Delete these files or directories recursively ?\n %s" fn-list))
    (cond
     ;; Sat Nov 24 13:55:57 2001
     ;; Must do it via dired-do-shell-command (via ange-ftp)
     ((eq 'ange-ftp-hook-function
	  (find-file-name-handler (car fn-list) 'shell-command))
      (dired-do-shell-command
       (concat dired-dd-delete-recursive-command " "
	       (mapconcat 'identity dired-dd-delete-recursive-options " "))
       current-prefix-arg
       (mapcar 'file-name-nondirectory fn-list) ))
     ;; On local files (directories).
     (t
      ;; Sometimes process is left `run' in emacs-20.2. Why? Use 'sync
      ;; (dired-dd-shell-command dired-dd-shell-rm-R-command fn-list 'sync)

      (condition-case nil
	;; Tue Oct 24 09:30:07 2000 revert-buffer() added to manage to
	  ;; show `to' window's current status (not so perfectly,
	  ;; especially when "cp -a"ing huge file(s)).  Now this
	  ;; call-process() async-executes "cp -a" with `nil 0 nil'
	  ;; flags.  I don't know why `nil nil nil' won't work as I
	  ;; intended.
	  (prog1
	      (apply 'call-process dired-dd-delete-recursive-command
		     nil 0 nil (append
				dired-dd-delete-recursive-options fn-list))
	    ;; Like `dired-internal-do-deletions'
	    (mapc (lambda (f) (dired-clean-up-after-deletion f)) fn-list)
	    (sit-for 0.1))
	(error
	 (error "Command failed: %s"
		(concat dired-dd-delete-recursive-command " "
			(mapconcat 'identity
				   dired-dd-delete-recursive-options " ")))))
      (message "Hit `g' if target dired buffer doesn't look right.")))
    (revert-buffer)))

;;
;; Seems like dired-do-relsymlink-regexp of dired-x.el in emacs-20.2
;; has `wrong number' of arguments ? (`arg' missed ?)
;;(defun dired-do-relsymlink-regexp (regexp newname &optional arg whole-path)
;;  "RelSymlink all marked files containing REGEXP to NEWNAME.
;;See functions `dired-do-rename-regexp' and `dired-do-relsymlink'
;;for more info."
;;  (interactive (dired-mark-read-regexp "RelSymLink"))
;;  (dired-do-create-files-regexp
;;   (function dired-make-relative-symlink)
;;   "RelSymLink" nil regexp newname whole-path dired-keep-marker-relsymlink))
;;

;; Can't find good key bindings.
;; All of these may confuse sort of key semantics in dired-mode
;;(define-key dired-dd-mode-map "\eC" 'dired-dd-do-copy-recursive)
;;(define-key dired-dd-mode-map "%c" 'dired-dd-do-copy-recursive-regexp)
;;(define-key dired-dd-mode-map "%R" 'dired-dd-do-copy-recursive-regexp)

;;(provide 'dired-xxx)


;;
;; Key, mouse bindings: This section turned into all comments.
;;

;; Someone might like these (I hope).
;; (define-key dired-dd-mode-map " " 'dired-dd-toggle-mark)

;; I don't think it's better to modify even Meta-mouse map...
;; (define-key dired-dd-mode-map [M-mouse-3] 'dired-dd-revert-buffer)

;; I think this may be helpful for some user.
;; Hit RET to go into Mac-Win-like file search (isearch-forward, actually).
;; RET turns ON and OFF isearh.  You can isearch-backward too.
;; Return seriese is not bound in original dired.  TAB is another possibility
;; to extend shortcuts, but you may have to wipe out the curses/spell of
;; emacs indent system first.
;; (define-key dired-dd-mode-map [return] 'isearch-forward)

;; Guess both of these are useful while browsing thru directories. 

;;
;; Macros for hilit19 and mouse pointer handling.
;;
;; for hilit19, no harm for font-lock-mode user.
;; I don't use font-lock-mode daily. 
;; Repaint is not required for font-lock-mode.

(defsubst dired-dd-repaint (st en)
  (if (featurep 'hilit19)
      ;;(hilit-repaint-command nil)
      (hilit-rehighlight-region st en t)))

(defsubst dired-dd-repaint-line ()
  (dired-dd-repaint
      (save-excursion (beginning-of-line nil) (point))
      (save-excursion (end-of-line nil) (point))
      ))

;; X-Pointer handling var/macro for to change pointer while dragging
;;(defvar dired-dd-x-pointer-shape x-pointer-diamond-cross)
(if ;; dired-dd-ms-os-p
    dired-dd-no-fancy-stuff		; to support the disabled OS.
    ;; Dummy, unused, just for suppress error in loading.
    (setq x-pointer-shape 0
	  x-pointer-iron-cross 1
	  x-pointer-leftbutton 2
	  x-pointer-middlebutton 3
	  x-pointer-rightbutton 4))

(defvar dired-dd-x-pointer-shape x-pointer-iron-cross
  "*Pointer shape while drag if dired-dd-no-x-pointer-animation is non-nil.")
(defvar dired-dd-old-x-pointer-shape x-pointer-shape
  "Dired-dd internal use only.")

;; Seems like these two should be defsubst. Pointer won't change with defmacro.
(defsubst dired-dd-restore-drag-pointer ()
  (setq x-pointer-shape (eval dired-dd-old-x-pointer-shape))
  (set-mouse-color nil))
(defsubst dired-dd-set-drag-pointer ()
  (setq dired-dd-old-x-pointer-shape x-pointer-shape
	x-pointer-shape (eval dired-dd-x-pointer-shape))
  (set-mouse-color nil))

;; X-Pointer ainimation while draggin with button2.
(defvar dired-dd-no-x-pointer-animation nil
  "*If you do not want x-pointer animation while dragging, set this t.")
(defvar dired-dd-x-pointer-list '(x-pointer-leftbutton
				  x-pointer-middlebutton
				  x-pointer-rightbutton
				  )
  "*A list of x-pointer composing animation frames.")
(defvar dired-dd-x-pointer-counter 0 "Dired-dd internal use only.")

(defsubst dired-dd-cycle-x-pointer ()
  ;; no arg, only incremental direction.
  (and (>= (setq dired-dd-x-pointer-counter (1+ dired-dd-x-pointer-counter))
	  (length dired-dd-x-pointer-list))
      (setq dired-dd-x-pointer-counter 0))
  (setq x-pointer-shape
	(eval (nth dired-dd-x-pointer-counter dired-dd-x-pointer-list)))
  ;;(message "%d:%s" dired-dd-x-pointer-counter 
  ;;	   (nth dired-dd-x-pointer-counter dired-dd-x-pointer-list))
  (set-mouse-color nil))

(if ;; dired-dd-ms-os-p
    dired-dd-no-fancy-stuff
    (progn
      (defsubst dired-dd-restore-drag-pointer () ())
      (defsubst dired-dd-set-drag-pointer () ())
      (defsubst dired-dd-cycle-x-pointer () ())))

;; Oops.  Emacs is always under construction.
;; Here comes the return of a son of a renamed function all over again.
;; dired-unmark-all-files-no-query in 19.28
;; seems to have renamed as dired-unmark-all-marks in 20.20.
(or (fboundp 'dired-unmark-all-marks)
    (fset 'dired-unmark-all-marks 'dired-unmark-all-files-no-query))

;;
;; Trivia.
;; You may not want to use these.  Currently
;; most of these are not bound to any mouse or key event.
;;
;;(defun dired-dd-reposition (event)
;; unused, because compatible to mouse-set-point (see $lisp/mouse.el),
;; but one example of mouse handling
;;  (set-buffer (window-buffer (posn-window (event-end event))))
;;  (goto-char (posn-point (event-end event))))

;; All mouse command should set point to dired buffer.  E.g., it is dangerous
;; to bind (original dired's) dired-unmark-all-marks directly to mouse.
(defun dired-dd-toggle-mark-mouse (event)
  (interactive "e")
  ;(dired-dd-reposition event)
  (mouse-set-point event) (dired-dd-toggle-mark nil) (dired-dd-repaint-line))
(defun dired-dd-do-copy (event)
  (interactive "e")
  (mouse-set-point event) (dired-do-copy nil))
(defun dired-dd-do-relsymlink (event)
  (interactive "e")
  (mouse-set-point event) (dired-do-relsymlink nil))
(defun dired-dd-do-rename (event)
  (interactive "e")
  (mouse-set-point event) (dired-do-rename nil))
;;; These are bound to [C-M-down-mouse-*]
;;; So, (read-event) is required to throw away button up event.
;;; The binding [C-M-down-mouse-*] is related to my bookmark-popmenu.el.
(defun dired-dd-copy-filename-as-kill (event &optional arg)
  (interactive "e\nP")
  (mouse-set-point event) (read-event) (dired-copy-filename-as-kill arg))
(defun dired-dd-revert-buffer (event)
  (interactive "e")
  (mouse-set-point event) (revert-buffer nil))
;; Sat Nov  4 17:44:06 2000 read-event removed, to bind this to C-M-mouse-2,
;; and bind C-M-down-mouse-2 to dired-dd-popup-shell-alist-menu().
(defun dired-dd-copy-filename-as-kill-full (&optional event arg)
  (interactive "e\nP")
  (mouse-set-point event) ;;;(read-event)
  (dired-copy-filename-as-kill 0))
(defun dired-dd-unmark-all-marks (event)
  (interactive "e")
  (mouse-set-point event) (read-event) (dired-unmark-all-marks)
  (dired-dd-repaint (point-min) (point-max)))
(defun dired-dd-quit (event)
  (interactive "e")
  (mouse-set-point event) (dired-quit))

;; Bound to [C-mouse-2]
(defun dired-dd-view-file (event &optional arg)
  (interactive "e")
  (mouse-set-point event)
  (dired-view-file))

;;
;; Mouse-1:
;; Double click to open, or drag to mark files.
;;

;;
;; Based on 19.28's mouse-drag-region, but added flavor of emacs-20.2's
;;
(defun dired-dd-drag-region (start-event &optional arg)
  "Toggles mark '*' on file or directory lines in a dired buffer.

In the file open by double click, the only clicked file is opened.
If you want to open all the marked file, use \\[dired-dd-drag-drop].

Optional ARG handed to the function called upon double click, otherwise
has no effect.  Probably this ARG handling makes sense only in Mule-2.3
based on emacs-19.28."

;; `Copy-region-as-kill' DEPRECATED Sun May 14 13:40:09 2000
;; Also the dragged region is selected by copy-region-as-kill by side effect
;; as in mouse-drag-region.  Region handling is exactly the same as that of
;; mouse-drag-region.

  (interactive "e\nP")
  (mouse-minibuffer-check start-event)
  (let* ((start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (start-frame (window-frame start-window))
	 (bounds (window-edges start-window))
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (click-count (1- (event-click-count start-event)))
 	 (modifiers))
    (setq mouse-selection-click-count click-count)
    (setq mouse-selection-click-count-buffer (current-buffer))
    (mouse-set-point start-event)

    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
	(goto-char start-point))
    (setq start-point (point))

    ;; Do not support double, and triple click boundary control here.
    ;;(let ((range (mouse-start-end start-point start-point click-count)))
    ;; (move-overlay mouse-drag-overlay (car range) (nth 1 range)
    ;;	      (window-buffer start-window)))
    (move-overlay mouse-drag-overlay (point) (point)
		  (window-buffer start-window))
    (deactivate-mark)
    ;; end-of-range is used only in the single-click case.
    ;; It is the place where the drag has reached so far
    ;; (but not outside the window where the drag started).
    (let (event end end-point)
      (track-mouse
	(while (progn
		 (setq event (read-event)
		       modifiers (event-modifiers event))
		 ;;(message "pos:%s:%s"
		 ;;	  click-count
		 ;;	  (posn-x-y (event-start event)))
		 (or (mouse-movement-p event)
		     (eq (car-safe event) 'switch-frame)))
	  (if (eq (car-safe event) 'switch-frame)
	      nil
	    (setq end (event-end event)
		  end-point (posn-point end))

	    (cond
	     ;; Are we moving within the original window?
	     ((and (eq (posn-window end) start-window)
		   (integer-or-marker-p end-point))
	      (goto-char start-point)
	      (goto-char end-point)
	      (let ((range (mouse-start-end start-point (point) click-count)))
		(move-overlay mouse-drag-overlay (car range) (nth 1 range))))

	     (t
	      (let ((mouse-row (cdr (cdr (mouse-position)))))
		(cond
		 ((null mouse-row))
		 ((< mouse-row top)
		  (mouse-scroll-subr start-window (- mouse-row top)
				     mouse-drag-overlay start-point))
		 ((>= mouse-row bottom)
		  (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
				     mouse-drag-overlay start-point)))))))))
      (if (consp event)
	  (let ((fun (key-binding (vector (car event)))))
	    ;;(if (fboundp fun) (message "%s" fun))
	    (if (not (= (overlay-start mouse-drag-overlay)
			(overlay-end mouse-drag-overlay)))
		;; Dired-part Sun Nov 30 20:34:32 1997
		;;  (point) > (mark t) always ?
		(let (last-command this-command)
		  ;; Do original thing as side effect, part1.
		  ;;(message "do mark! fun:%s" fun)
		  (push-mark (overlay-start mouse-drag-overlay) t t)
		  (goto-char (overlay-end mouse-drag-overlay))
		  ;; Do original thing as side effect, part1.
		  (let (st en)
		    (setq st (save-excursion
			       (goto-char (mark t))
			       (beginning-of-line nil)
			       (point))
			  en		; Same as (point)
			  (save-excursion (end-of-line nil) (point)))
		    ;; Support ?D marking by meta-drag-mouse-1
		    (cond
		     ;; Not yet (for dired-dd-unmark-in-region).
		     ;; ((and (member 'shift modifiers)
		     ;;	   (member 'control modifiers))
		     ;;  (dired-dd-unmark-in-region st en))
		     ((car (member 'meta modifiers))
		      (dired-dd-toggle-mark-in-region st en dired-del-marker))
		     (t (dired-dd-toggle-mark-in-region st en)))
		    (dired-dd-repaint st en))
		  ;; DEPRECATED Sun May 14 13:40:09 2000
		  ;; Do original thing as side effect, part2.
		  ;; (copy-region-as-kill (point) (mark t))
		  )
	      ;; Dired-part Sun Nov 30 20:34:32 1997
	      ;;(message "No mark! fun:%s" fun)
	      ;;(goto-char (overlay-end mouse-drag-overlay))
	      ;;(delete-overlay mouse-drag-overlay)
	      (if (fboundp fun)
		  (setq unread-command-events
			(cons event unread-command-events))
		;;(funcall fun event arg) ; Otherwise we can't pass prefix arg ??
		)		
	      (setq this-command 'mouse-set-point)
	      )))
      (delete-overlay mouse-drag-overlay)) ))

(defconst dired-dd-non-mark "[ CHLSR]"
  "Regex of markers to be regarded as non-marker.")

(defun dired-dd-toggle-mark (arg)
  "Toggle mark of current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (cond
   ((save-excursion
      (forward-line 0)
      (looking-at "\\( +/.*:$\\|^$\\|^ +total\\)")) ())
   (t
    (let ((dired-marker-char
	   ;; ?R for dired-dd-keep-marker-copy-recursive, but
	   ;; sounds like I screwed up something ???
	   (save-excursion
	     (forward-line 0)
	     (if (looking-at dired-dd-non-mark)
		 dired-marker-char
	       ;; The file is marked: Clear it only when it's identical
	       ;; to CURRENT `dired-marker-char'.
	       (if (looking-at (char-to-string dired-marker-char))
		   ?\040 dired-marker-char)))))
      (if (and (dired-mark arg)
	       (not (save-excursion (forward-line 0) (bobp))))
	  (dired-previous-line 1) )))))

(defsubst dired-dd-toggle-mark-in-region (st en &optional mark)
  "Toggle mark in region BEG END.
If optional 3rd arg MARK is provided, use it as dired-marker-char
and then dired-dd-toggle-mark is called."
  (interactive "r")
  (save-excursion
    (goto-char st)
    (while (< (point) en)
      (if mark 
	  (let ((dired-marker-char mark))
	    (dired-dd-toggle-mark nil))
	(dired-dd-toggle-mark nil))
      ;(dired-next-line 1)
      (forward-line 1)
      )))

(defsubst dired-dd-unmark-in-region (st en)
  "Umark in region BEG END."
  (interactive "r")
  (save-excursion
    (goto-char st)
    (while (< (point) en)
      (dired-unmark 1))))

;; Should be accomodated in the `No command for ...' error part.
;; Not yet did so because good saving prompt, or automatic save (use
;; emacs-20's customize ?) is not conceived yet.  I do not want force user
;; complicated sequence.  Should I  raise tcl/tk menu (like Windows) ?
;; Seems like there is no `customize' for dired-guess-shell-alist-user. 
;; Of course you can use this standalone.  Not well Tested.
(defun dired-dd-register-shell-alist (extension command &optional arg)
  "Read file EXTENSION string and COMMAND string, and register them  as alist \
in dired-guess-shell-alist-user.  No duplicate check for COMMAND definition."
  (interactive "sExtension: \nsCommand: \nP")
  (or (string-equal "" extension)
      (string-equal "" command)
      ;; we search thru this `ext' by entry, so concat "$" is not required.
      (let ((ext (if (string-match "^\\." extension)
		     extension (concat "\\." extension)))
	    (cmd (if (string-match "\\*" command)
		     command (concat command " *")))
	    (found-inserted nil))
	(setq dired-guess-shell-alist-user
	      (mapcar (lambda (entry)
			(if (not (string-match (car entry) ext))
			    entry
			  (setq found-inserted t)
			  (cons (car entry) (cons cmd (cdr entry)))))
		      dired-guess-shell-alist-user))
	(if found-inserted
	    (message
	     "command \"%s\" was inserted at the top for extension \\%s$."
	     cmd ext)
	  ;; Here we concat "$" to `ext' here (not extension).
	  (setq ext (if (string-match "\\$$" ext)
			ext (concat ext "$")))
	  (setq dired-guess-shell-alist-user
		(cons (list ext cmd) dired-guess-shell-alist-user)
		found-inserted t)
	  (message "(\"\\\%s\" \"%s\") was inserted at the top." ext cmd)))))

;;
;; Process, find-file functions.
;; Called by double click of mouse-1, click of mouse-2
;;
;; Unsupport several normal *-down-mouse-1 bindings.  Especially
;; mouse-drag-secondary and relating functions handling secondary
;; selection eat up double clicks as a side effect, which makes our
;; intended double click command unusable.  Remember that they are
;; globally bound.  As for down-mouse-1, double-click handling is ok.
;;
(defun dired-dd-mouse-set-point (event &optional arg)
  "Dummy, almost nop, but it executes mouse-set-point as a side effect.
Normally bound to *-mouse-down-* event, and reads a mouse up event.
If read event is bound to a valid function, pushes the up event into
unread-command-events cue."
  (interactive "e\nP") 
  (mouse-minibuffer-check event)
  ;; This func triggers the four func below.
  (mouse-set-point event)
  (setq event (read-event))
  ;;(let (last-command this-command) ())
  (let ((fun (key-binding (vector (car event)))))
    (if (fboundp fun)
	;; We don't allow non-dired-dd commands here. 
	;; eg. M-mouse-1.
	(if (string-match "^dired-dd" (format "%s" fun))
	    (setq unread-command-events
		  (cons event unread-command-events))
	  )))
  (setq this-command 'mouse-set-point))

(fset 'dired-dd-nop 'dired-dd-mouse-set-point)

;; Seems like that arg is not handed to double-click-bound function.
;; So, always marked files are processed ?
;; Arg handling codes in the four functions below is useless. Damn.
(defun dired-dd-exec-command-directly-20 (event &optional arg guessed-cmd)
  "Executes shell command asynchronously on marked files (or directory) \
with no query.
Shell command is determined automatically according to the file extension.
For example `xv' is executed for files having .jpg extension.
Correspondence of the file extension and command to be executed should have
been defined in dired-x variable
`dired-guess-shell-alist-default' and `dired-guess-shell-alist-user'.

Command output is collected in buffer `*dired-dd-proc*'.
All the process invoked by dired-dd-exec-command-directly shares
this buffer.  Until all the process sharing the buffer,
string `run' is shown in mode line of the buffer `*dired-dd-proc*'.
After all the process are exit, the mode line will show `exit'.

Optional ARG is recognized as `do-it-for-this-file' flag.
2nd optional arg GUESSED-CMD is used as pre-determined shell command list."
  (interactive "e\nP")
  ;; In case called from dired-dd-drop-within-single-dir (with event=nill)
  (or (null event)
      (progn (mouse-minibuffer-check event)
	     ;;  (mouse-set-point event)
	     (if (not (windowp (posn-window (event-end event))))
		 (error "Cursor not in text area of window"))))
  ;; This duplicated mouse-set-point is something to do with
  ;; (not (eq major-mode 'dired-mode)) check at the top of dired-dd-drag-drop.
  ;; At the first call (no proc is in "*dired-dd-proc*" buffer) in emacs-19.28,
  ;; it seems like that mouse-set-point is not called in dired-dd-drag-drop
  ;; Really weird. Sat Jan 17 14:17:33 1998 s.n.
  (if (= (string-to-int emacs-version) 19)
      (mouse-set-point event))
  (let* ((fn-list (dired-get-marked-files t arg))
	 (guessed-cmd (or guessed-cmd (dired-guess-default fn-list)))
	 command-line)
    (if (listp guessed-cmd)
	(setq guessed-cmd (car guessed-cmd)))
    (if (null guessed-cmd)
	(error "No command for: %s" fn-list)
      (dired-dd-exec-async-shell-command
       (setq command-line
;;; Fix for 0.9.1.15 Sun Jan  3 01:38:54 1999
;;; More dired-compatible way. `on-each' (3rd arg) is nil.
	     (dired-shell-stuff-it guessed-cmd fn-list nil arg) ))
      (message "Asynchronously launched %s" command-line)
      (run-hooks 'dired-dd-exec-command-directly-hook))))

(defvar dired-dd-dont-display-async-command-output nil
  "*Normally you don't want to set this to non-nil.")

(defvar dired-dd-process-list nil "dired-dd internal use only.")

;; Sat Aug 11 15:30:39 2001
(defsubst dired-dd-purge-async-symbol (cmd)
 "Purge ampersand at the end of string CMD, and return it."
 (cond ((string-match "\&[\040\t]*$" cmd)
	(substring cmd 0 (match-beginning 0)))
       (t cmd)))

(defun dired-dd-exec-async-shell-command (&rest cmd)
  (let* ((procname "dired-dd-proc")
	 (buffer (format "*%s*" procname))
	 ;; Support non-dired-buffer too. Just for future expansion.
	 ;; Sat Apr 25 02:45:26 1998, just use default dir.
	 ;; cf., In -lR recursive listing, (dired-current-directory)
	 ;; may return different value from that of default-directory.
	 ;; On the other hand, (dired-get-marked-files) returns relative
	 ;; path from default-directory.
	 (cwd default-directory)
	 ;;(cwd (if dired-subdir-alist
	 ;;	 ;;(eq major-mode 'dired-mode)
	 ;;	 (dired-current-directory)
	 ;;       default-directory))
	 (cmd (list (dired-dd-purge-async-symbol (car cmd))))
	 (proc))
    ;; do not kill to assemble echoing of commands
    ;;(if (get-buffer buffer) (kill-buffer buffer))
    (setq buffer (get-buffer-create buffer))
    (or dired-dd-dont-display-async-command-output
	(display-buffer buffer))
    (save-excursion
      (set-buffer buffer)
      ;;(buffer-flush-undo (current-buffer))
      
      (setq default-directory cwd)

      (goto-char (point-max))
      (set-process-buffer
       (setq proc (apply 'start-process-shell-command procname buffer cmd))
       buffer)
      (setq dired-dd-process-list (cons proc dired-dd-process-list))
      (setq mode-line-process
	    (format ": %s" (process-status proc)))
      (set-process-sentinel proc 'dired-dd-exec-async-shell-command-sentinel) )))

(defun dired-dd-exec-async-shell-command-sentinel (proc mes)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; the buffer killed
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
	 (unwind-protect
	     (progn
	       (set-buffer (process-buffer proc))
	       ;; setq is needed inside unwind-protect ?
	       (setq dired-dd-process-list (delq proc dired-dd-process-list))
	       ;; Also clean up other accidentally killed processes
	       ;; Sounds like not working well yet (?). How can we deal with
	       ;; killed *dired-dd-proc* buffer ?
	       (let ((tem (reverse dired-dd-process-list)))
		 (setq dired-dd-process-list ())
		 (mapcar '(lambda (other)
			    (or (memq (process-status other) '(signal exit))
				(setq dired-dd-process-list
				      (cons other dired-dd-process-list))))
			 tem))
;;; Suspicious. Been written with the comment above.
;;;	       (mapcar '(lambda (other)
;;;			  (if (memq (process-status other) '(signal exit))
;;;			      (setq dired-dd-process-list
;;;				    (delq other dired-dd-process-list))))
;;;		       dired-dd-process-list)
	       (goto-char (point-max))
	       (message "%s" (substring 
			 (format "Process %s %s %s"
				 proc (process-command proc) mes) 0 -1))
	       (or dired-dd-process-list
		   (setq mode-line-process
			 (format ": %s" (process-status proc)))))
	   ;;(set-buffer-modified-p (buffer-modified-p))
	   (progn (goto-char (point-max))
		  (force-mode-line-update))))))

;;(setq special-display-regexps (cons "^\\*dired-dd" special-display-regexps))

;;
;; Emacs-19 bug (? or my async process code is wrong ?) workaround.
;; Above dired-dd-exec-command-directly-20 does not work well in Emacs-19.xx.
;; Seems like some process-sentinel handling is not correct, and
;; dired-dd-drag-drop's working buffer is moved to async command buffer,
;; and it's dired-current-directory raises spurious error.
;;
;;(defvar dired-dd-ignore-spurious-error nil
;;  "Set to non-nil to ignore spurious cosmetic error in verson 19.xx emacs.
;;The error is raised upon dired-dd-exec-command-directly execution.")
;;
;; Uses dired-do-shell-command interface with "&" concatenation.
;;
;; Currently Unused.
(defun dired-dd-exec-command-directly-19 (event &optional arg)
  "Executes shell command asynchronously on marked files (or directory) \
with no query.
Shell command is determined automatically according to the file extension.
For example `xv' is executed for files having .jpg extension.
Correspondence of the file extension and command to be executed should have
been defined in `dired-x' variable
dired-guess-shell-alist-default and dired-guess-shell-alist-user."
  (interactive "e\nP")
  ;; In case called from dired-dd-drop-within-single-dir (with event=nill)
  (or (null event)
      (progn (mouse-minibuffer-check event)
	     ;;  (mouse-set-point event)
	     (if (not (windowp (posn-window (event-end event))))
		 (error "Cursor not in text area of window"))))
  (let* ((fn-list (dired-get-marked-files t arg))
	 (guessed-cmd (dired-guess-default fn-list)))
    (if (listp guessed-cmd)
	(setq guessed-cmd (car guessed-cmd)))
    (if (null guessed-cmd)
	(error "No command for: %s" fn-list)
      (setq guessed-cmd (format "%s &" guessed-cmd))
      (message "Asynchronously launched [arg: %s]: \"%s\""  arg guessed-cmd)
      (if (<= (string-to-int emacs-version) 19)
	  (dired-do-shell-command guessed-cmd arg)
	(dired-do-shell-command
	 guessed-cmd arg fn-list))      )))

;; Use dired-dd-exec-command-directly-20
;; See also major-mode check at the top of dired-dd-drag-drop
(fset 'dired-dd-exec-command-directly 'dired-dd-exec-command-directly-20)

(defun dired-dd-exec-command-query (event &optional arg)
  "Executes shell command on marked files or directory.
Related command to the file extension is prompted in minibuffer.
The default command set should have been defined in dired-x variable
  dired-guess-shell-alist-default
  and
  dired-guess-shell-alist-user
Refer to the document string of these variables to customize
shell command set."
  (interactive "e\nP")
  (mouse-minibuffer-check event)
;;  (mouse-set-point event)
  (let ((posn (event-end event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
      (eval ;;you should learn dired-dd-b3-keymap structure:
       (lookup-key dired-dd-b3-keymap [shellcmd]))))

;; New from 0.9.1.10 (Still Beta).
;; (define-key dired-dd-mode-map [mouse-2] 'dired-dd-find-file-or-exec) ; test
(defvar dired-dd-find-multiple-file nil
  "*If non-nil, dired-dd-find-file-or-exec opens tries to open all marked files.
The marked files opened using multiple windows.  The opening-all-marked-files
may be annoying, e.g., in case you're marking files to
be deleted or moved and you have to examine a file content.

If dired-dd-find-multiple-file is nil \(dired-dd's default\), only
clicked file or directry is opened even if any file or directory is
marked in the dired buffer.

This variable only affects the behaviour of dired-dd-find-file-or-exec, and
has no effect on an external command execution:
the marked files are always opened using command(s)
related to the file extension, according to the definition of
dired-guess-shell-alist-default or dired-guess-shell-alist-user.

Further, dired-dd-find-multiple-file does not affect
dropping onto root or other window window via
dired-dd-drag-drop (drag-mouse-2), dired-find-file-other-frame, and
dired-find-file-other-window etc.  The first two operations always try to
open all the marked files, while dired-find-file-other-window opens
just one clicked file.")

(defun dired-dd-find-file-or-exec (event &optional arg)
  "Opens file or directory with emacs, or external command suitable for marked file's extension.

Function dired-dd-find-file is used to file-file, or
dired-dd-exec-command-directly to execute command related to the marked file's
extension with `dired-x' variable dired-guess-shell-alist-default and
dired-guess-shell-alist-user.  Also refer C-h f dired-dd-exec-command-directly,
and `dired-x' info."
  (interactive "e\nP")
  ;; No mouse handling here.
  ;; dired-dd-find-file, or dired-dd-exec-command-directly do that.
  (let* ((fn-list (dired-get-marked-files t arg))
	 (guessed-cmd (dired-guess-default fn-list)))
    (if guessed-cmd
	(dired-dd-exec-command-directly event arg)
;;      (dired-dd-find-file event arg)   ; old one file version.
      (if dired-dd-find-multiple-file
	  (dired-dd-find-marked-files event arg)
	(dired-dd-find-file event arg) ))))

;; New from 0.9.1.10 (Beta, rough ?).
;; Used by dired-dd-find-file-or-exec, or can be called directly through
;; this bindings (for example):
;; (define-key dired-dd-mode-map [M-mouse-2] 'dired-dd-find-marked-files)
(defun dired-dd-find-marked-files (event &optional arg)
  "Opens file or directory using currently selected window."
  (interactive "e\nP")
  (mouse-minibuffer-check event)
;;  (mouse-set-point event)
  (let ((posn (event-end event))
	(modifiers (event-modifiers event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
;;      (if (and (<= (string-to-int emacs-version) 19) (boundp 'MULE))
    (funcall 'dired-do-find-marked-files arg)
;;	(funcall dired-dd-find-file-method))
      (message "")))

;; Old. For just one file (even if multiple files marked)
(defun dired-dd-find-file (event &optional arg)
  "Opens file or directory using currently selected window."
  (interactive "e\nP")
  (mouse-minibuffer-check event)
;;  (mouse-set-point event)
  (let ((posn (event-end event))
	(modifiers (event-modifiers event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
      (if (and (<= (string-to-int emacs-version) 19) (boundp 'MULE))
	  (funcall dired-dd-find-file-method arg)
	(funcall dired-dd-find-file-method))
      (message "")))

(defun dired-dd-find-file-other-window (event &optional arg)
  "Opens file or directory using `other' window."
  (interactive "e\nP")
  (mouse-minibuffer-check event)
;;  (mouse-set-point event)
  (let ((posn (event-end event))
	(modifiers (event-modifiers event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
    (if (and (<= (string-to-int emacs-version) 19) (boundp 'MULE))
	(funcall 'dired-find-file-other-window arg)
      (funcall 'dired-find-file-other-window))
    (message "")))

;; Fri Jun 19 14:41:35 1998, 0.9.1.10
(defun dired-dd-find-file-other-frame (event &optional arg)
  "Opens file or directory using `other' window."
  (interactive "e\nP")
  (mouse-minibuffer-check event)
;;  (mouse-set-point event)
  (let ((posn (event-end event))
	(modifiers (event-modifiers event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
    (if (and (<= (string-to-int emacs-version) 19) (boundp 'MULE))
	;; See next function/fset for dired-find-file-other-frame
	(dired-find-file-other-frame arg)
      (dired-find-file-other-frame))
    (message "")))

;; Fri Jun 19 14:41:35 1998, 0.9.1.10
;; dired-find-file-other-window'look-alike.
;; In Hope that this can be used with such a binding:
;;   (define-key dired-mode-map "\e\C-o" 'dired-find-file-other-frame)
;;   (autoload 'dired-find-file-other-frame "dired-dd" nil t)
;; Actually, dired-find-file-other-frame is not his, but mine.
(fset 'dired-find-file-other-frame 'dired-dd-find-file-other-frame-1)
(defun dired-dd-find-file-other-frame-1 (&optional arg)
  "Almost compatible to dired-find-file-other-window, but use `other-frame'."
  (interactive "P")
  (let ((fn-list (dired-get-marked-files t arg)))
	(mapcar
	 (lambda (fn)
	   (if (and (<= (string-to-int emacs-version) 19) (boundp 'MULE))
	       (find-file-other-frame fn arg)
	     (find-file-other-frame fn)) )
	   fn-list) ))

(defvar dired-dd-find-file-method 'dired-advertised-find-file
  "*Find file method for double click.
'dired-find-file, or 'dired-find-file-other-window is other alternatives.
'dired-find-file-other-window almost emulates the mouse-2 of original dired.")

;;
;; Mouse-2:
;; Drag marked (or on point) file(s) around, and drop to somewhere.
;;

(defun dired-dd-drag-drop (start-event &optional arg)
  "Drag marked file into other window (in the same or different frame).
Optional ARG specifies the number of files to be processed if
as in normal dired operations (dired-do-copy etc.)."
  (interactive "e\nP")
  (mouse-minibuffer-check start-event)
  (mouse-set-point start-event)

  ;; Major mode check required to make workaround emacs-19 bug.
  ;; This harms nothing (of course) in normal dired-dd opearation.
  (if (not (eq major-mode 'dired-mode))
      ;; This happens in emacs-19.xx: dired-dd-drag-drop is called as if
      ;; it is in *dired-dd-proc* (async process buffer).
      ;; Might be my faulty async process coding, or sentinel handling bug
      ;; in emacs-19.xx (observed in mule-2.3).  Something to do with
      ;; that emacs-19.xx can't detect double-drag correctly ?
      ()
    ;; `modifiers' local is set within track-mouse loop
    ;; to support C-, M-, S-, C-M, S-C, C-M-S- modifier key to suppress Menu.
    (let* ((start-posn (event-start start-event))
	   (start-point (posn-point start-posn))
	   (start-window (posn-window start-posn))
	   ;; (start-frame (window-frame start-window))
	   (click-count (1- (event-click-count start-event)))
	   modifiers			; Has list form.
	   shortnames
	   curdir)
      (setq mouse-selection-click-count click-count)
      (setq curdir (dired-current-directory)
	    ;; `No file on line' should *NOT* raise error here.
	    shortnames (dired-dd-get-marked-files t arg t))
      (let (event last end end-point)
	(dired-dd-set-drag-pointer)
	(track-mouse
	  (while
	      (progn
		(setq event (read-event)
		      modifiers (event-modifiers event))
		;; pointer animation.
		(or dired-dd-no-x-pointer-animation
		    (dired-dd-cycle-x-pointer))
		(message "%s" ;; without this, fname with "%s" raise error.
		 (if dired-dd-debug
		     ;; modifiers: nil, while move.
		     (format "%s:%s:%s" arg modifiers event)
		   (format "%s:%s %s"
			   arg
			   (dired-dd-trunc-fn-list "Dragging %s" shortnames 60)
			   (posn-x-y (event-start event)) )))
		(or (mouse-movement-p event)
		    (eq (car-safe event) 'switch-frame)))
	    (if (eq (car-safe event) 'switch-frame)
		;;@@
		(OOooooooops this should never be called, if dired-dd-drag-drop
				is bound to down-, or drag-mouse-*)
	      (setq end (event-end event)
		    end-point (posn-point end)))))
	(dired-dd-restore-drag-pointer)

	(let ((fun (key-binding (vector (car event)))))
	  (if (fboundp fun)
	      ;;(setq unread-command-events
	      ;;  (cons event unread-command-events))
	      ;; *WAS NOT* drag. Do it right here.  Do not push event to
	      ;; unread-command-events, which can't handle prefix arg.
	      ;;(message "%s:%s:%s" event arg fun)
	      (funcall fun event arg)
	    ;; Or Do the drag drop.
	    (let* ((end-window (posn-window end))
		   (end-point (posn-point end))
		   (start-buffer (window-buffer start-window))
		   end-buffer)
	      ;;(message "X:%s" event)
	      (cond
	       ((framep end-window)
		(mouse-minibuffer-check event)
		;; Version 0.0.0.5 draft, support inter-frame drag & drop.
		;; end/end-event is garbage if dired-dd-drag-drop is bound to
		;; down-, or drag-mouse-*.  Use Current mouse-xy-position.
		(let* ((last-xy-pos (mouse-pixel-position))
		       (last-cur-pos (mouse-position))
		       cx cy rcxy)
		  ;;(message "%s:%s" last-xy-pos)
		  (if (null (car-safe (cdr-safe last-xy-pos)))
		      (dired-dd-drop-on-root-window arg)
		    (progn
		      ;; Switch to the frame on which mouse is
		      (dired-dd-switch-frame
		       (car last-xy-pos)
		       ;;(list (car (cdr last-xy-pos)) (cdr (cdr last-xy-pos)))
		       ;; now dired-dd-switch-frame supports (X . Y) format
		       (cdr last-xy-pos))

		      ;; mouse-position (character position)
		      (setq cx (car (cdr last-cur-pos))
			    cy (cdr (cdr last-cur-pos)))
		      ;; window-at : returns a window containing
		      ;; the absolute xy (char) position in the frame.
		      (select-window (window-at cx cy))

		      ;; Probably this won't fail
		      (setq rcxy
			    ;; Sun Jun 10 00:46:04 2001
			    ;; `coordinates-in-window-p' in emacs-21 seems to
			    ;; return cons of float in some occasions.
			    ;; Workaround is in `dired-dd-move-to-xy-in-window'.
			    (coordinates-in-window-p
			     (cons cx cy) (selected-window)))
		      ;; Move to the buffer position.
		      ;; corresponding to rcxy in th window
		      (dired-dd-move-to-xy-in-window rcxy)

		      ;; Now we are in newly switched frame.
		      ;; Use selected window/buffer.
		      (dired-dd-drop-from-to
		       start-window (selected-window) start-point (point) curdir
		       modifiers arg)))
		  ))
	       ((windowp end-window)
		(setq end-buffer (window-buffer end-window) )
		;; win to win compare not required here.
		;; dired-dd-drop-from-to now works (almost) good
		;; even if start-buffer and end-buffer are identical.
		(dired-dd-drop-from-to
		 start-window end-window start-point end-point curdir
		 modifiers arg))
	       (t (error "Unknown error, sorry..."))
	       ))))))
    ))

(defvar dired-dd-debug nil "Turns on debug mode if non-nil.")

;;
;; Support root window drop
;; New from 0.9.1.7
;; 
(defvar dired-dd-iconify-control-on-root-drop nil
  "*Setting to non-nil enables iconify-or-deiconify control upon object is \
dropped on root window in emacs-20.xx.

Default is nil, and setting this to t may not work nicely yet.

In emacs-19.xx, dired-dd-iconify-control-on-root-drop is always regarded
as being nil.")

(defun dired-dd-drop-on-root-window (&optional arg)
  "Handler for root window drop."
  ;; Wed Apr 22 14:53:24 1998.
  ;;   dired-dd-drag-drop does not creates fn-list (full path version),
  ;;   so we have to expand filenames, or we fail in dropping multiple marked
  ;;   directories. I do not want to modify dired-dd-drag-drop to have fn-list.
  ;; Tue Dec 29 23:37:18 1998 (0.9.1.15)
  ;;   For a solution to the issue above, changed to
  ;;   call `dired-dd-get-marked-files'.  Optional arg works for next ARG files.
  ;;   Operation is compatible to 0.9.1.14.
  (let ((fn-list (dired-dd-get-marked-files nil arg t))
	(handler 'find-file-other-frame)
	(files-in-frame
	 (mapcar
	  (lambda (w) (buffer-file-name (window-buffer w))) (window-list))))
    (mapc
     (lambda (fn)
       (cond
	(fn
	 (cond ((member fn files-in-frame)
		(dired-dd-switch-frame (make-frame))
		(find-file fn))
	       (t (funcall handler fn)))
	 (if (and dired-dd-iconify-control-on-root-drop
		  (>= (string-to-int emacs-version) 20))
	     (iconify-or-deiconify-frame))
	 (message "")
	 (dired-dd-audio-play-by-method handler))
	;; Same error message as in dired-get-filename
	(t (error "No file to open.  Unknown error."))))
     fn-list)))

;;
;; Switching frame, goto XY pos in a window in the frame.
;; Combining these frame and window oriented code is not such a good idea
;; for readability.
;;
;;(require 'dired-dd-drag-drop2.draft1)  ; once the code was in this.
;;

;; Borrowed code from windows-2.10.tar.gz (win:select-frame).
(defun dired-dd-switch-frame (goal &optional xy)
  "Switch to FRAME, and optional 2nd arg XY is a list of mouse position.
XY should have form '(x  y) or '(x . y).  If no XY specified (0 -5) is used."
  ;; xy: list of xy position
  (if (null xy) (setq xy '(0 -5)))
  ;; You can specify xy as '(X . Y)
  (if (listp (cdr-safe xy))
      ()
    (setq xy (list (car-safe xy) (cdr-safe xy))))
  (if (null (frame-live-p goal))
      ()
    (if (eq (cdr (assq 'visibility (frame-parameters goal))) 'icon)
	(make-frame-visible goal))	;to de-iconify(if iconified)
    (while (null (member goal (visible-frame-list))) (sit-for 0))  
    (raise-frame goal)
    (select-frame goal)
    (if (not (eq (selected-frame) goal))
	nil
      ;; Workaround for fvwm2-2.3.3x (with mouse focus policy).
      ;; Seems like we should be off focus, then focus to the `xy'.
      (apply 'set-mouse-pixel-position goal '(-5 -5))
      (apply 'set-mouse-pixel-position goal xy)
      (unfocus-frame)			; This is ignored in emacs-20/21
      goal
      ;;      (mouse-select-window (read-event))
      ;;(frame-first-window goal)
      ;;(current-buffer)
      ;;(window-buffer (selected-window))
      )))

;; `dired-dd-move-to-xy-in-window'
;; Before 0.9.1.12, this was dired buffer specific. Now it's generic.
;; 0.9.1.12: Sun Dec 13 21:03:11 1998
;;   Added non-dired mode handling, to move to drop point accurately
;;   considering dired-dd-insert-fname.el and dired-dd-insert-file.el.
;; Old generic (hairy) code is in
;; dired-dd-drag-drop2.draft1.el (developer's draft)
;;
;; Sun Jun 10 00:46:04 2001 Workaround for emacs-21 (round() for goal[xy]).

(defun dired-dd-move-to-xy-in-window (cor &optional win)
  "Move to (X . Y) position in WINDOW. (X . Y) is relative coordinate in WINDOW.
If WINDOW is not supplied, selected window is assumed."
  (if win				; else use selected-window
      (select-window win))
  (let* ((goalx (round (car cor)))
	 (goaly (round (cdr cor))))
    (move-to-window-line goaly)		; move to goaly, then
    (if (eq 'dired-mode major-mode)
	(dired-move-to-filename nil nil) ;  good enough for dired-dd.
      ;;(vertical-motion 0) ; Unused. move-to-window-line does this.
      (move-to-column (+ (current-column) goalx)))))

;; See version 0.0.0.1 for Dialog box.
;; Dialog interface is thrown away, but keep this for my memory.
;;(defvar dired-dd-prompting-interface 'menu
;;  "Defines the way of prompting inter-window drag'n'drop (menu or dialog).
;;Valid value is one 'menu, or 'dialog.")

(defun dired-dd-drop-from-to
  (from to start-point end-point src-path modifiers &optional arg)
  ;; `from' and `to' should be window.

  ;; Changed to use WINDOW (not BUFFER) for `from' and `to' in version 0.0.0.5.
  ;; These may be buffer if the two buffer's are different each other, etc.
  ;; In inter-frame d&d, there is a situation where two buffers are
  ;; the same but are displayed through different windows each having
  ;; different point position.  Especially observed a problem when a
  ;; directory tree is displayed recursively with -alR flag, and the
  ;; same dired buffer is displayed in two frames each having different
  ;; windows to display the dired buffer.  Unused buffer handling code
  ;; is still left here and there...

  ;; start-point is location in `from' buffer,
  ;; end-point is location in `to' buffer (`from' and `to' may be same),
  ;; and win is window which should be drop to.
  (let (;;src-path
	dest-path target-diredp fn-list (samedir nil))
    (save-window-excursion
      ;;(set-buffer from)
      (select-window from)
      ;;(setq src-path (dired-current-directory))
      (setq ;; We'd better raise NO error on `No file on line' to
	    ;; allow create dir etc, because there are some dired
	    ;; buffers having only directories.  Copy/Move raises error
	    ;; anyway via lowlevel dired subroutine.
	    fn-list (dired-dd-get-marked-files nil arg t)
	    shortnames (dired-dd-get-marked-files t arg t)))
    (save-window-excursion
      ;;(set-buffer to)
      (select-window to)
      (if (setq target-diredp (eq 'dired-mode major-mode))
	  (save-excursion
	    (setq dest-path
		  (let (pointed-dest)
		    (goto-char end-point)
		    (setq pointed-dest
			  (dired-get-filename nil t))
		    (if (or
			 (null pointed-dest)
			 (not (file-directory-p pointed-dest)))
			(dired-current-directory)
		      ;; else directory line pointed:
		      pointed-dest))))
	(setq dest-path "")))		; or `samedir' raise error

    ;; expand-file-name does not take care of "/" automatically, so,
    (or (string-match "/$" src-path)
	(setq src-path (concat src-path "/")))
    (or (string-match "/$" dest-path)
	(setq dest-path (concat dest-path "/")))
    (setq samedir
	  (string-equal			; Within single directory...
	   (expand-file-name src-path) (expand-file-name dest-path)))

    ;; do copy etc.
    (if target-diredp
	;; Inter-directory file Handling...
	(let (handler)
	  ;; Quick D&D with Meta(Move)/Control(Copy), or raise menu.
	  ;; I don't know delq 'drag is needed or not.
	  (cond
	   ((and (car (member 'meta modifiers))
		 (null samedir))
	    ;;(message "Meta+ drag => Move.")
	    (setq handler (dired-dd-get-b2-menu-method 'move)))
	   ((and (car (member 'control modifiers))
		 (null samedir))
	    ;;(message "Control+drag => Copy.")
	    (setq handler (dired-dd-get-b2-menu-method 'copy)))
	   ((and (car (member 'shift modifiers))
		 (null samedir))
	    ;;(message "Shift+drag => Link (default Link method).")
	    (setq handler
		  (dired-dd-get-b2-menu-method
		   dired-dd-default-link-method)))
	   ;; Else raise popup menu here.
	   (t
	    (message
	     (format "Target dir: %s" (expand-file-name dest-path)))
	    (setq handler (dired-dd-popup-handling-method-menu shortnames))
	    ))

	  ;; handler is ready here.
	  (cond
	   ((or (null handler)		; NOP, (off-the-menu input).
		(eq handler 'cancel))
	    (message "Aborted")
	    (dired-dd-audio-play-by-method 'cancel))
	       ;;; find-file check should preceed others.

	   ;; Sat Nov  4 18:21:45 2000 s.n.
	   ;; Support new item "Select Related CMD" in B2-menu.
	   ((eq handler 'dired-dd-popup-shell-alist-menu)
	    ;;(message "POPUP-SHELL-ALIST: %s %s" arg fn-list)
	    (funcall handler event arg))

	   ((or (eq handler 'find-file)
		(eq handler 'find-file-other-window)
		(eq handler 'find-file-other-frame))
	    ;; Should switch to cbuf in case of find-file-other-window ???
	    (mapcar (lambda (fn)
		      (if fn
			  (progn (funcall handler fn)
				 (dired-dd-audio-play-by-method handler))
			;; Same error message as in dired-get-filename
			(error "No file on this line")))
		    fn-list))

	   ((eq handler 'dired-do-delete)
	    (if (or
		 ;; Sat Nov  4 17:09:20 2000
		 ;; Checking `samedir' does not making sense, because user
		 ;; may make horizontal drag to raise the b2-menu to delete.
		 ;; samedir ;; THE OLD CODE.
		 ;; Changed to compare window objects:
		 (eq from to)
		 (yes-or-no-p
		  (format 
;;;		   "You've moved into another directory (%s),
		   "You've moved into another window,
but delete %s anyway ?"
;;;		   dest-path
		   fn-list)))
		(progn
		  ;; Returned value of the handler means nothing for this purpose
		  ;;(if (funcall handler arg) 
		  ;;  (dired-dd-audio-play-by-method 'cancel)
		  ;;  (dired-dd-audio-play-by-method handler))
		  (set-buffer-modified-p nil) ; clear flag to detect, because,
		  ;; this may be cancelled further yes-or-no-p. All the
		  ;; other minibuffer interface is only by C-g...
		  (funcall handler arg)	; always done in `from' buffer.
		  (if (buffer-modified-p) ; Dirty, if deletion is done.
		      (dired-dd-audio-play-by-method handler)
		    (dired-dd-audio-play-by-method 'cancel)))
	      (dired-dd-audio-play-by-method 'cancel)))

	   ;; Sat Nov  4 17:09:20 2000, s.n. New
	   ;; Sat Apr  7 16:36:48 2001, `dired-dd-shell-rm-R' deprecated.
	   ((eq handler 'dired-dd-delete-recursive)
	    ;;(eq handler 'dired-dd-shell-rm-R)
	    (if (or			; Compare window objects as above
		 (eq from to)
		 (yes-or-no-p
		  (format 
		   "You've moved into another window,
but delete %s anyway ?"
		   fn-list)))
		;; fn-list is already built according to arg:
		;; Just one file when arg is non-nil.
		(funcall handler fn-list)))

	   ((eq handler 'exec-direct)
	    (if (or
		 ;; samedir
		 (eq from to)
		 (yes-or-no-p
		  (format
;;;		   "You've moved into another directory \(%s\),
		   "You've moved into another window,
but execute command \(%s\)
on %s anyway ?"
;;;		   dest-path
		   (car-safe (dired-guess-default fn-list))
		   fn-list)))
		(dired-dd-exec-command-directly nil arg) ; no sound here.
	      (dired-dd-audio-play-by-method 'cancel)))

	   ((eq handler 'dired-view-file)
	    ;; dired-view-file ain't support multiple files originally.
	    (funcall handler)
	    (dired-dd-audio-play-by-method handler))

	   ;; find-file check should preceed others.
	   (samedir
	    (dired-dd-drop-within-single-dir handler arg))

	   ;; Not samedir already here, so, inhibit newdir,
	   ;; even if user may required.
	   ((eq (car handler) 'newdir)
	    (error
	     "You can create new directory only in same dired buffer.") )

	   (t 
	    ;; Fake as if we are still in the `from' window.
	    (save-window-excursion
	      (select-window from)
	      ;; Dired package does not raise Yes-No dialog.
	      ;; So overwrite query is still lousy here...  But,
	      ;; this interfacing with menu resource is very lousy too.
	      ;; See dired-dd-popup-no-destination-handling-method-dialog
	      ;; below for smart interface.
	      ;; However we must supply destination
	      ;; directory here anyhow, to emulate dired's operation.
	      (dired-create-files
	       (nth 1 handler) ;(function dired-copy-file)
	       (nth 2 handler) ;(if dired-copy-preserve-time "Copy [-p]" "Copy")
	       fn-list
	       (function
		(lambda (frm)
		  (expand-file-name (file-name-nondirectory frm) dest-path)))
	       (nth 4 handler)))	;dired-keep-marker-copy

	    ;; Wed Oct 25 02:03:15 2000
	    ;; Fiddle for the new async dired-dd-copy-recursive(). Ain't 
	    ;; work good for copying big file(s), but better than nothing.
	    (cond
	     ;; Do revert-buffer() only when "Copy Recursive":
	     ;; It's annoying for other handlers.
	     ((eq (nth 1 handler) 'dired-dd-copy-recursive)
	      (save-window-excursion
		(select-window to)
		(revert-buffer))
	      (message "Hit `g' if target dired buffer doesn't look right.")))

	    (dired-dd-audio-play-by-method (car handler))
	     )))

      ;; Else file Open by Drop (onto other non-dired window)
      ;;(message "Drop open files %s" fn-list)
      ;; Arg `win' is used here. 
      (if (eq (window-frame from) (window-frame to))
	  ;; Within the same frame, keep `from' (==dired) window displayed,
	  ;; All (marked) files are opened in one `to' window.
	  ;; (Last Marked file is on top of bufferlist).
	  (or
	   (and dired-dd-non-dired-drop-handlers
		(progn
		  (select-window to)
		  (dired-dd-exec-non-dired-drop-handlers
		   (list fn-list to modifiers end-point)))
		;; No sound if exec-handler fails:
		(progn (dired-dd-audio-play-by-method 'find-file) t))
	   ;; only if dired-dd-exec-non-dired-drop-handlers failed, do this:
	   (mapcar (lambda (fn)
		     ;;(switch-to-buffer from)
		     ;;(find-file-other-window (car fn-list))
		     (if (null fn)
			 (error "No file on this line")
		       (select-window to)
		       (find-file fn)
		       (dired-dd-audio-play-by-method 'find-file)))
		   fn-list))

	;; else inter-frame drop, surely we can keep `from' window displayed,
	;; so, I should (?) manage to show opened file as many as possible
	;; using multiple windows.
	(let ((buf (save-window-excursion
		     (select-window from)
		     (window-buffer))))
	  (or
	   (and dired-dd-non-dired-drop-handlers
		(progn (select-window to)
		       (dired-dd-exec-non-dired-drop-handlers
			(list fn-list to modifiers end-point)))
		;; No sound if exec-handler fails:
		(progn (dired-dd-audio-play-by-method 'find-file-other-frame) t))
	   ;; only if dired-dd-exec-non-dired-drop-handlers failed, do this:
	   (progn
	     (switch-to-buffer buf)
	     (dired-do-find-marked-files)))
	  (dired-dd-audio-play-by-method 'find-file-other-frame)))
      )))

;;
;; DragDrop into non-`dired' buffers
;;
;; New from 0.9.1.11.
;; An interface to customize drag-and-drop into non-dired target buffer (window).
;; Handler functions should be registered in dired-dd-non-dired-drop-handlers
;; in a list form.  The functions registered are called through
;; function dired-dd-exec-non-dired-drop-handlers, in the order of the list
;; until one of them succeeds.
;;

(defvar dired-dd-non-dired-drop-handlers nil
  "A list of handler function for non-dired target buffers.
The handler function to be registered in this list must accept one arg

ARGLIST, whose format is:

  \(FILENAME-LIST WINDOW MODIFIERS POINT\)

wherein

  FILENAME-LIST: a list of filenames \(in full path description\)
  WINDOW:        target window \(probably useless\)
  MODIFIERS:     modifier key list at the relevant drag-and-drop operation
  POINT:         buffer position corresponding to point where file is dropped.

Not enough ?  Don't forget that we are lisp programmers.  You can dare to
use variables such as `modifiers', `event', `start-window' etc.,
which are defined in upper functions.  Refer to the upper functions
dired-dd-drag-drop, and  dired-dd-drop-from-to in dired-dd.el,
for those parameters.

The handler function can assume that current window/buffer
is already switched to target buffer when it is called. 
It is responsibility of the handler function to
examine whether the current window/buffer is suitable for its job.
The examination should be strict as possible, because it is not predictable
that the user registers what handler function in what order.

The handler function must return non-nil if its operation was successful,
otherwise nil.

A function add-hook can be used to register a handler.")

;;
;; The Handler of dired-dd-non-dired-drop-handlers above.
;; Current arglist format: (fn-list to modifiers end-point)
;;   fn-list:   filename list valid within major `let' in dired-dd-drop-from-to
;;   to:        target window
;;   modifiers: modifier key list at the relevant drag-and-drop operation
;;   end-point: dropped point
;; Handlers are called one by one until one of them succeeds. Returns non-nil
;; (generated by handler) on a successful handler execution, otherwise nil
;; (in case all failed)
;;

(defun dired-dd-exec-non-dired-drop-handlers (arglist)
  "Calls handler functions registered in dired-dd-non-dired-drop-handlers \
in order passing arg ARGLIST until some of them succeeds.
If one handler was successful (non-nil returned), the loop is finished
there and the result (non-nil) is returned.
Returns nil if all the handler are failed.

Refer to docstring of variable dired-dd-non-dired-drop-handlers for
detailed info on the arg ARGLIST."
  ;; emacs-20.x has compatible stuff to this
  ;; (looks like C primitive. Not well tested this method yet).
  (if (fboundp 'run-hook-with-args-until-success)
      (run-hook-with-args-until-success 
       'dired-dd-non-dired-drop-handlers arglist)
    ;; emacs-19.x has not run-hook-with-args-until-success
    (let ((result nil)
	  (handlers dired-dd-non-dired-drop-handlers)
	  fun)
      (while (and handlers (null result))
	(setq fun (car handlers))
	(if (fboundp fun)
	    (setq result (funcall fun arglist)))
	(setq handlers (cdr handlers)) )
      result)))

;;
;; `non-dired-**drop**' via KEYBOARD (!?!?)
;;
;; Mon Dec 18 12:18:57 2000
;;

;; What's good with the `drag-drop' via KEYBOARD ?
;;
;;     Most of other dired-dd command/functions can be executed via
;;     equivalent keyboard command, but `non-dired' drop was a few of
;;     exceptions in older `dired-dd' versions: there was no such
;;     keyboard command to drop files into MH/Mew/TiMidity/Monk/Gnus
;;     buffers.

(defsubst dired-dd-exec-non-dired-drop-kbd-subr (modifiers)
  "Subr. Drop marked files with MODIFIERS.
`dired-dd-exec-non-dired-drop-handlers' is used to do the drop, and
`other-window-for-scrolling' is called to determine the drop target."
  (let ((files (dired-dd-get-marked-files)) ; Get all marked in full-path
	;; Hey, `dired-dwim-target-directory' or `dired-dwim-target' can't
	;; be used because this is non-dired drop.
	;; (win (let ((dired-dwim-target t)) (dired-dwim-target-directory)))
	(win (other-window-for-scrolling)) ; or just `next-window' ?
	(modifiers modifiers))
    (save-window-excursion
      (select-window win)
      (dired-dd-exec-non-dired-drop-handlers
       (list files win modifiers (point))))))

;; 
;; How do we deal with ARG ?
;; I kinda annoyed by minibuffer interface (such as "Which win/buf ?") here.
;;

;; Key: |
(defun dired-dd-exec-non-dired-drop-kbd (arg)
  "DragDrop via keyboard. Drop marked files with plain \(drag\) modifiers.

Drop target is determind by means probably 
`next' window.  See `dired-dd-exec-non-dired-drop-kbd-subr'
which determines the target window."
  (interactive "P")
  (dired-dd-exec-non-dired-drop-kbd-subr (quote (drag))))

;; Key: C-|
(defun dired-dd-exec-non-dired-drop-kbd-C (arg)
  "DragDrop via keyboard. Drop marked files with \(control drag\) modifiers.
See `dired-dd-exec-non-dired-drop-kbd' for more detailed info."
  (interactive "P")
  (dired-dd-exec-non-dired-drop-kbd-subr (quote (control drag))))

;; Key: C-M-|
(defun dired-dd-exec-non-dired-drop-kbd-C-M (arg)
  "DragDrop via keyboard. Drop marked files with \(control meta drag\) \
modifiers.
See `dired-dd-exec-non-dired-drop-kbd' for more detailed info."
  (interactive "P")
  (dired-dd-exec-non-dired-drop-kbd-subr (quote (control meta drag))))

;; Keybindings:
;; Guess some of "|" key-system is unbound in `dired' mode.
;; The [?|] sort of notations seem to byte-compiles into incompatible
;; form depending upon emacs versions --- means that `dired-dd' is not
;; `binary-compatible' anymore... Thu Dec 28 12:20:49 2000
(cond (dired-dd-mode-map		; cond() not required maybe but
       (define-key
	 dired-dd-mode-map [?|] 'dired-dd-exec-non-dired-drop-kbd)
       (define-key
	 dired-dd-mode-map [?\C-|] 'dired-dd-exec-non-dired-drop-kbd-C)
       ;; M-| (shell-command-on-region) exists. Inadequate.
       ;; (define-key
       ;;   dired-dd-mode-map [?\C-|] 'dired-dd-exec-non-dired-drop-kbd)
       (define-key
	 dired-dd-mode-map [?\C-\M-|] 'dired-dd-exec-non-dired-drop-kbd-C-M)))

;;
;; Menus for dd via [mouse-2]
;;

;; Within a same directory, menu entry triggers minibuffer interface.
(defun dired-dd-drop-within-single-dir (handler &optional arg)
  (let ((op-code (car handler)))
    ;; These are cancelled only by C-g.
    (cond ((eq 'copy op-code) (dired-do-copy arg))
	  ((eq 'copyrecursive op-code)
	   ;; this happens user selected 'copyrecursive for non-directory obj.
	   (dired-dd-do-copy-recursive arg))
	  ((eq 'move op-code) (dired-do-rename arg))
	  ((eq 'symlink op-code) (dired-do-symlink arg))
	  ((eq 'hardlink op-code) (dired-do-hardlink arg))
	  ((eq 'relsymlink op-code) (dired-do-relsymlink arg))
	  ((eq 'newdir op-code)
	   (dired-create-directory
	    (read-file-name "Create directory: "
			    (dired-current-directory)))
	   (dired-dd-audio-play-by-method 'newdir))
	  ((eq 'cancel op-code) 
	   (message "Aborted")
	   (dired-dd-audio-play-by-method 'cancel))
	  )))

;; Menu style interface.
;; The arg `fnames' should be shortnames (without dir part).
(defun dired-dd-popup-handling-method-menu (fnames)
  "Popups dialog to ask file handler for dired-dd-drop-from-to."
  ;;(ding)
  (let ((flist-truncated
	 (dired-dd-trunc-fn-list "Do what on %s ?" fnames)) ret)
    ;; Current version does not message fnames for mouse-2.
    ;; (message "%s" flist-truncated)
    (setq ret (x-popup-menu t dired-dd-b2-menu))
    (message "")			; Clean up echo area
    ret))

;; Non-quoted elements in dired-dd-b2-menu are eval'ed at the time of
;; evaluating this defconst.  So dired-dd-keep-marker-copy-recursive
;; etc should be defined before the evaluation of this defconst
(defconst dired-dd-b2-menu
  (list
   ;; This title won't appear in mule-2.3 (emacs-19.28)
   ;; flist-truncated  ; Including fnames in menu sounds noisy.
   "Dired-dd Drag Drop"
   (list "Handlers";; what is this title for ?
	 ;; See dired-do-copy in dired-aux.el for these list structures.
	 (cons "Select Related CMD" 'dired-dd-popup-shell-alist-menu)
	 (cons "" nil)
	 (cons "Copy" (list 'copy (function dired-copy-file)
			    (if dired-copy-preserve-time
				"Copy [-p]" "Copy")
			    'arg 'dired-keep-marker-copy))
	 (cons "Copy Recursive" (list 'copyrecursive
				      (function dired-dd-copy-recursive)
			    "CopyRecursive" 'arg
			    'dired-dd-keep-marker-copy-recursive))
	 (cons "Move" (list 'move (function dired-rename-file)
			    "Move" 'arg 'dired-keep-marker-rename "Rename"))
	 (cons "" nil)
	 (cons "Symlink" (list 'symlink (function make-symbolic-link)
			       "Symlink" 'arg 'dired-keep-marker-symlink))
	 (cons "Hardlink" (list 'hardlink (function add-name-to-file)
				"Hardlink"
				'arg 'dired-keep-marker-hardlink))
	 ;; You need dired-x to use this.
	 (cons "Relative Symlink"
	       (list 'relsymlink
		     (function dired-make-relative-symlink)
		     "RelSymLink" 'arg 'dired-keep-marker-relsymlink))
	 (cons "" nil)
	 (cons "Delete" 'dired-do-delete)
;;;	 (cons "Delete Recursive" 'dired-dd-shell-rm-R) ; in dired-dd-b3-menu
	 (cons "Delete Recursive" 'dired-dd-delete-recursive) ; New
	 (cons "" nil)
	 ;; This should be allowed in `samedir' status only.
	 (cons "New Directory" (list 'newdir nil))
	 (cons "" nil)
	 (cons "Exec Related CMD" 'exec-direct)
	 (cons "" nil)
	 (cons "Open" 'find-file)
	 (cons "View" 'dired-view-file)
	 (cons "Open Other Window" 'find-file-other-window)
	 (cons "Open Other Frame" 'find-file-other-frame)
	 (cons "" nil)
	 (cons "Cancel" 'cancel)
	 ))
  "Menu raised by drag-mouse-2. For programmer's only.
To modify this variable, you must read functions
dired-dd-popup-handling-method-menu and dired-dd-drop-from-to in dired-dd.el.")

(defvar dired-dd-default-link-method 'relsymlink
  "*Set preferred default symbolic link method.
Valid value is one of 'symlink, 'hardlink, or 'relsymlink.
Dired-dd's default is 'relsymlink.")

;; Search opcode in dired-dd-b2-menu and return corresponding
;; hander (cdr of one menu entry list = return value of x-popup-menu).
(defun dired-dd-get-b2-menu-method (opcode)
  (let (handler)
    (mapcar 
     '(lambda (tem)
	(if (eq (car-safe (cdr-safe tem)) opcode)
	    (setq handler (cdr-safe tem))) )
     (reverse (cdr (car (cdr dired-dd-b2-menu)))))
    handler))

(fset 'dired-dd-fname-threshold 'window-width)
(defun dired-dd-trunc-fn-list (prompt fn-list &optional thres-given)
  (let* ((fn-str (format prompt fn-list))
	 (tailer " ...)")
	 (thres (if thres-given thres-given (dired-dd-fname-threshold))))
     (if (> (length fn-str) thres)
	 (concat (substring fn-str
			    0 
			    (- thres (length tailer)))
		 tailer)
       fn-str)))

(defun dired-dd-get-marked-files (&optional localp arg noerror)
  "Return the marked files' names as list of strings.
Same as original dired-get-marked-files, except it accepts
3rd arg NOERROR-IF-NOT-FILE.
If original dired-get-filename is changed this barfs."
  (save-excursion
    (nreverse (dired-map-over-marks (dired-get-filename localp noerror) arg))))

;; === Once dialog style function was here, but deprecated. ===

;;; Was: popup-shell-alist.el --- Get the file's handler, and show as menu.

;; Fri Oct 20 19:31:10 2000
;; Popup a menu of handlers associated with the file's extention in
;; `dired-guess-shell-alist-default' and `dired-guess-shell-alist-user'.

;;(eval-and-compile
;;  (require 'dired-dd))

;; ARG handling is implemented as in dired-dd-exec-command-directly-20().

;; Key binding -- added in the previous section.
;;(define-key dired-dd-mode-map
;;  [S-down-mouse-3] 'dired-dd-popup-shell-alist-menu)

(defun dired-dd-popup-shell-alist-menu (&optional event arg)
  "Popups a menu for shell-alist associated to the marked file.
Bound to \\[dired-dd-popup-shell-alist-menu]."
  (interactive "e\nP")
  (progn (mouse-minibuffer-check event)
	 (mouse-set-point event)
	 (if (not (windowp (posn-window (event-end event))))
	     (error "Cursor not in text area of window")))
  (let (flist guess command (width (window-width)))
    ;; (dired-dd-get-marked-files nil arg t)
    ;; as in dired-dd-exec-command-directly-20(), use theirs.
    (condition-case nil
	(and (setq flist 
		   ;; fn-list may be bound in dired-dd-(drag-drop|drag-from-to)
		   (or (and (boundp 'fn-list) fn-list)
		       (dired-get-marked-files t arg))
		   )
	     (or (setq guess (dired-guess-default flist))
		 ;; This command does not use track-mouse, so we have to take
		 ;; a bit cheaper messaging method, to show warning while
		 ;; mouse button is pressed.
		 (progn
		  (message "%s"		; Pathname may contain "%s"
			   (dired-dd-trunc-fn-list
			    "Can't guess shell command for %s" flist width))
		  (ding) (sit-for 3) (error)))
	     (message "%s"
		      (dired-dd-trunc-fn-list
		       "Select handler for %s" flist width))
	     ;; I don't know why selecting no item won't drops into
	     ;; [S-mouse-3] (plain click) binding, while 
	     ;; dired-get-marked-files() above drops into it ?
	     (setq command
		   (prog1
		       (or
			(dired-dd-x-popup-menu-out-of-keymap
			 t (dired-dd-build-shell-alist-keymap arg flist guess))
			(error))	; jumps into 'error tab below
		     (message "")
		     ))
	     (eval command))
      ;; If some error occurs, does nothing and returns nil
      ;; gracefully, to ensure the event is handed to subsequent key
      ;; binding like [S-mouse-3] (bound to save-buffer in my emacs).
      (error  (message "") nil))
    ))

;;
;; The keymap builder
;;

;; Now dired-dd-exec-command-directly-20() is modified to accept `guessed-cmd'
;; (from "0.9.1.19").
;; Use dired-dd-x-popup-menu-out-of-keymap(), or x-popup-menu() to test this.

(defun dired-dd-build-shell-alist-keymap (arg flist guess)
  "Build shell-alist menu for FLIST as keymap."
  (let ((ent 0))
    (or (listp guess)
	(setq guess (list guess)))
    (append
     (cons
      'keymap
      (mapcar
       (lambda (x)
;;; Works only in emacs-20.x and 19.34
;;;	 `( ,(intern (format "ddshal%d" (setq ent (1+ ent))))
;;;	   ,x
;;;	   (nil)
;;;	   dired-dd-exec-command-directly event arg (list ,x))
	 (` ( (, (intern (format "ddshal%d" (setq ent (1+ ent)))))
	      (, x)
	      (nil)
	      dired-dd-exec-command-directly event arg (list (, x))))
	 )
       guess))
     (list "Select a handler"))))

;;
;; Mouse-3:
;; Unary file operation.
;;

;; delete method works with `eval' method.
(defun dired-dd-no-destination-handling (e &optional arg)
  (interactive "e\nP")
  (or arg (setq arg nil));; required ?
  (mouse-set-point e)
  ;; `No file on line' should *NOT* raise error here
  (let* ((fn-list (dired-dd-get-marked-files nil arg t))
	 (shortnames (dired-dd-get-marked-files t arg t))
	 (cbuf (current-buffer))
	 (handler
	  ;; Dialog style  won't work anymore because of
	  ;;  too many number of items.
	  (dired-dd-popup-no-destination-handling-method-menu shortnames)
	    ))
    (if (symbolp handler)
	;; `open' kind operations. find-file* etc.
	(cond ((or (null handler)	; NOP, (off-the-menu input).
		   (eq handler 'cancel))
	       (message "Aborted")
	       ;; we do not play sound in b3 menu.
	       ;;(dired-dd-audio-play-by-method 'cancel)
	       )
	      (t (mapcar (lambda (fn)
			   ;; other-window type should switch-to-buffer ???
			   ;;(if (eq handler 'find-file-other-window)
			   ;;    (switch-to-buffer cbuf))
			   (funcall handler fn)) fn-list)
		 ))
      ;; else '(dired-...) type (eval'able object).
      (eval handler)
      ;(funcall handler arg)
      )))

;;
;; B-3 Menu handlers.
;;

;; This returns definition:
;;(dired-dd-x-popup-menu-out-of-keymap t dired-dd-b3-keymap)
;; This returns key:
;;(x-popup-menu t dired-dd-b3-keymap)
;;(x-popup-menu t dired-dd-b2-menu)
(defun dired-dd-x-popup-menu-out-of-keymap (event map)
  "On EVENT, raise a menu out of KEYMAP and returns selected definition.
X-popup-menu just returns a key when a keymap is the definition, 
but this returns final definition."
  (let ((val (x-popup-menu event map)))
    (if val (lookup-key map (apply 'vector val)) nil)))

;; Replacement of dired-dd-popup-no-destination-handling-method-menu
(defun dired-dd-popup-no-destination-handling-method-menu (fnames)
  (let ((flist-truncated
	 (dired-dd-trunc-fn-list "Do what on %s" fnames)) ret)
    ;; Needs this for mouse-3 menu. 
    (message "%s" flist-truncated) ;; "%s" required (flist-truc... may have "%").
    (setq ret
	  (dired-dd-x-popup-menu-out-of-keymap t dired-dd-b3-keymap))
    (message "")
    ret))

;; The entity of
;; Mouse button 3 menu (dired-dd-popup-no-destination-handling-method-menu)
;; is patched by this.
;;(require 'dired-dd-b3-menu)	; Now this is eval-and-compile'd at the top.
;; Make cache of dired-dd-b3-keymap
(dired-dd-x-popup-menu-out-of-keymap nil dired-dd-b3-keymap)

;; Audio module, do nothing at default.
;; 
;; dired-dd-audio.el: Sat Dec 20 18:27:52 1997
;; Probably built into dired-dd.el
;; Most of the code was stolen from gnus-audio.el.
;;

;; Probably XEmacs stuff (I don't know these).
(defvar dired-dd-audio-inline-sound
  (and (fboundp 'device-sound-enabled-p)
       (device-sound-enabled-p))
  "When t, we will not spawn a subprocess to play sounds.")

(defvar dired-dd-play-audio nil "*Play sound or not on drag drop")

;; Typical linux path. change to 
(defvar dired-dd-audio-directory
  (expand-file-name "~/sound/") "*A directory containing audio files.")
(defvar dired-dd-audio-wav-player "/usr/local/bin/vplay"
  "*Unix command to play .wav/.voc")
(defvar dired-dd-audio-au-player "/usr/bin/showaudio"
  "*Unix command to play .au")

(defun dired-dd-audio-play (file)
  "Play an .au/.wav/.voc FILE."
  (interactive)
  (let ((sound-file (if (file-exists-p file)
			file
		      (concat dired-dd-audio-directory file))))
    (if (file-exists-p sound-file)
	(if dired-dd-audio-inline-sound
	    ;; Probably XEmacs stuff.
	    (play-sound-file sound-file)
	  (cond ((or (string-match "\\.wav$" sound-file)
		     (string-match "\\.voc$" sound-file))
		 (call-process dired-dd-audio-wav-player
			       sound-file 0 nil sound-file))
		((string-match "\\.au$" sound-file)
		 (call-process dired-dd-audio-au-player
			       sound-file 0 nil sound-file)))))))

;; Example: tkdesk audio file.  Most of the files can be found as possibly
;; sounds.tar.gz somewhere under http://people.mainz.netsurf.de/~bolik/tkdesk/

(defvar dired-dd-audio-file-alist
 '((copy                    . "laser.voc")
   (move                    . "door_shut.voc")
   (find-file               . "door_shut.voc")
   (find-file-other-window  . "door_shut.voc")
   (find-file-other-frame   . "door_shut.voc")
   (dired-view-file         . "door_shut.voc")
   (copyrecursive           . "door_open.voc")
   (delete                  . "fire.voc")
   (dired-do-delete         . "fire.voc")
   (symlink                 . "uplift.voc")
   (newdir                  . "jackhammer-short.au")
   (hardlink                . "computer.voc")
   (relsymlink              . "uplift.voc")
   (cancel                  . "metal.voc")
   (start                   . "start.voc")
   ) "*Alist connecting drag drop action, and sound to be played.")

(defsubst dired-dd-audio-play-by-method (opcode)
  "Play an .au/.wav/.voc file corresponding METHOD"
  (if dired-dd-play-audio
      (let ((audiofile (cdr (assoc opcode dired-dd-audio-file-alist))))
	(if audiofile
	    (dired-dd-audio-play audiofile)))))

;;(provide 'dired-dd-audio)

;;
;; Appendix A.
;; S-mouse-3:
;; Further abuse of dired menu bar.
;; Probably useless if you are using emacs-20, which has
;; [C-down-mouse-3] menu already).
;;

;; Insert overall prompt string "Dired Original Menu", or menu won't work
;; in case fset --> define-key binding below.
;; Use inheritance, 
;; but sounds like keymap turns wrong in fset->define-key method in mule-2.3.

;; Some 19.xx (eg. mule-2.3(19.34)) has 20.x compatible major-mode menu,
;; so comparing major version # is not consistent.
(if (or (fboundp 'mouse-major-mode-menu) ; Fbound in emacs-20 (After 19.29 ?).
	dired-dd-no-fancy-stuff)
    ()					; NOP
  (setq dired-dd-original-menu
	(cons 'keymap
	      (cons "Dired Original Menu"
		    (lookup-key dired-mode-map [menu-bar]))))
  ;; Straight-forward version.  This works too.
  ;;(setq dired-dd-original-menu
  ;;      (let ((tem (lookup-key dired-dd-mode-map [menu-bar])))
  ;;	(cons (car tem) (cons "Dired Original Menu" (cdr tem)))))

  ;; The method found in Elisp Info.  Works, but it does not set point...
  ;;(fset 'dired-dd-popmenu-map dired-dd-original-menu)
  ;;(define-key dired-dd-mode-map [S-mouse-3] 'dired-dd-popmenu-map)
  ;;(x-popup-menu nil dired-dd-original-menu)

  ;;
  ;; I wanted to set point by mouse, then do menu entry.
  ;;
  (defun dired-dd-popmenu-map-func (ev &optional arg)
    "Steal original dired menu bar and pop it up."
    (interactive "e\nP")
    (mouse-set-point ev)
    (let* ((val (x-popup-menu ev dired-dd-original-menu))
	   (key (vector (car val) (car (cdr val)))) 
	   (fun (lookup-key dired-dd-original-menu key)))
      (if (and (symbolp fun)
	       (fboundp fun))
	  (call-interactively fun))))

  (define-key dired-dd-mode-map [S-mouse-3] 'dired-dd-popmenu-map-func)

  (x-popup-menu nil dired-dd-original-menu) ; make cache
  )					; (> (string-to-int emacs-version) 19)

;;(x-popup-menu t dired-dd-original-menu) ; for test

;;
;; Finally, my suggested mouse binding.
;; 
(defvar dired-dd-use-modeline-binding-global nil
  "*Determines if \[mode-line C-mouse-3\], \[mode-line S-mouse-3\],\
and \[mode-line C-M-mouse-3\] bindings should be used globally.
If this variable is set to non-nil, you can use these

  \[mode-line C-mouse-3\]   \(mouse-kill-this-buffer\)
  \[mode-line S-mouse-3\]   \(mouse-bury-buffer\)
  \[mode-line C-M-mouse-3\] \(mouse-delete-frame\)

commands for every buffer.  My recomendation is to set this to t
\(guess goes well with normal [mode-line mouse-3] \(mouse-delete-window\)\),
but default is nil.

If you are already mapping \[mode-line C-mouse-3\], \[mode-line S-mouse-3\]
and \[mode-line C-M-mouse-3\] to other function, leave this variable
as being nil.  If you like these mapping in dired-dd but you are
autoloading dired/dired-dd, you had better bind these mouse bindings
using global-set-key etc. in your ~/.emacs.")

(provide 'dired-dd)			; must preceed run-hooks
(run-hooks 'dired-dd-load-hook)		; cf. dired-x's info

;; Should be done after (run-hooks 'dired-dd-load-hook):
(if dired-dd-use-modeline-binding-global
    (progn
      (define-key global-map [mode-line C-mouse-3] 'mouse-kill-this-buffer)
      (define-key global-map [mode-line S-mouse-3] 'mouse-bury-buffer)
      (define-key global-map [mode-line C-M-mouse-3] 'mouse-delete-frame))
  (define-key dired-dd-mode-map [mode-line C-mouse-3] 'mouse-kill-this-buffer)
  (define-key dired-dd-mode-map [mode-line S-mouse-3] 'mouse-bury-buffer)
  (define-key dired-dd-mode-map [mode-line C-M-mouse-3] 'mouse-delete-frame))

(defun mouse-kill-this-buffer (e)
  "Compatible to kill-this-buffer.  Normally bound to mouse."
  (interactive "@e")
  (kill-this-buffer))
(defun mouse-bury-buffer (e)
  "Compatible to bury-buffer.  Normally bound to mouse."
  (interactive "@e")
  (bury-buffer))
(defun mouse-delete-frame (e &optional arg)
  "Compatible to delete-frame.  Normally bound to mouse.
Optional ARG means force delete even if it is the last frame left
Deleting the last frame means forceing kill-emacs (without any save ?)."
  (interactive "@e\nP")
  (delete-frame (selected-frame) arg))
