;; timidity-mode-menu.el --- A menu/mouse interface for timidity-mode

;;
;; Version 1.7
;;
(defconst timidity-mode-menu-version "1.7")

;; Copyright (C) 1999 Seiichi.Namba <sn@asahi-net.email.ne.jp>
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
;; Fri Jan 15 15:08:01 1999 Written and tested mule-2.3(19.28) & emacs-20.2
;; Sun Jan 17 19:48:57 1999 v1.0: filelist menu added.
;; Mon Jan 18 11:17:31 1999 v1.1: emacs-19 and -20 compatibility cleaned up.
;; Tue Jan 19 16:04:14 1999 v1.2: Speeding code.
;; Thu Jan 21 14:16:14 1999 v1.3: Speeding codes cleaned up.
;; Thu Jan 21 14:16:14 1999 v1.4: More Speeding. Rehash is done while playing.
;; Fri Jan 22 18:57:00 1999 v1.5: File list syncs with {next,prev}-files.
;; Wed Jan 27 02:12:04 1999 v1.6: lambdas in baremenu changed to real function.
;; Wed Jan 27 02:12:04 1999 v1.7: Comment change only:  down-mouse-1 disabling
;;

;;
;; With this program,
;;
;;     [down-mouse-3] popups a menu (including a file list submenu) interface.
;;     Separator hairline in the file list menu indicates current file.
;;     Selecting any file in the file list menu plays the file.
;; 
;;     `Clear' menu command clears the file list menu (internally, this means
;;     clearing timidity-next-files and timidity-prev-files).
;;     
;;     New keyboard commands N (timidity-Next-file) and P (timidity-Prev-file)
;;     is provided.  Compatible to n (timidity-next-file) and 
;;     p (timidity-prev-file) except that these new commands accept numeric
;;     argument as number of files. Menu items `Next' and `Previous' are
;;     respectively bound to the new commands.
;;
;;     Most of the other menu commands are not big surprise, and are
;;     compatible to original timidity-mode keyboard commands. Most of prefix
;;     argument input before raising menu is propergated to commands (if
;;     a relevant original timidity-mode command supports one).
;;
;; You should get TiMidity++ from http://www.goice.co.jp/member/mo/timidity/
;; and install timidity.el to use this program.
;;
;; If you want to play MIDI file(s) by drag-and-drop from dired buffer,
;; get dired-dd from http://www.asahi-net.or.jp/~pi9s-nnb/dired-dd-home.html
;; and install dired-dd-timidity.el.
;;
;; To load, inlude this sort of codes in your ~/.emacs:
;; 
;; (add-hook 'timidity-mode-hook
;;	  '(lambda () (if window-system
;;			  (require timidity-mode-menu))))
;;
;; A setup code (author's) example:
;;
;; (autoload 'timidity "timidity" nil t)
;; ;; It's a shame that timidity.el has no (run-hooks 'timidity-load-hook)
;; (add-hook 'timidity-mode-hook
;;           '(lambda ()
;;              (if window-system
;;                  (require 'timidity-mode-menu))))
;; (add-hook 'timidity-mode-menu-load-hook ;; or eval-after-load "timidity" ?
;;           '(lambda ()
;;              ;; Want to use mouse-1 to select window (S-mouse-1 has conflict).
;;              (define-key timidity-mode-map [M-mouse-2] 'timidity-Prev-file)
;;              (define-key timidity-mode-map [mouse-2] 'timidity-Next-file)))
;; (setq timidity-default-options '("-U"))
;; (setq timidity-prog-path "/usr/local/bin/timidity4414")
;;

;; 
;; As a default, [down-mouse-3] is bound to timidity-mode-menu-map-func,
;; which updatedes the current position (hairline) in the file list menu
;; synchronized with the file list variables timidity-next-files and
;; timidity-prev-files.  Alternatively [down-mouse-3] (or any other prefered
;; mouse event) can be bound to a command timidity-mode-menu-map-func-no-sync,
;; which is maybe faster and simple (or more bug-free ?). You should use
;; timidity-mode-menu-load-hook to customize the mouse binding (or any other
;; feature of this program) such as:
;;
;; (add-hook 'timidity-mode-menu-load-hook
;;           '(lambda ()
;;              (define-key timidity-mode-map [down-mouse-3]
;;               'timidity-mode-menu-map-func-no-sync)))
;;

(provide 'timidity-mode-menu) ; Provide on top is safest way (that I believe).
(eval-and-compile
  (require 'timidity) ; Unnecessary in installation above, but for insurance.
  ;; for caar, cadr, cdar etc.
  (if (< (string-to-int emacs-version) 20) (require 'cl)))

;;;
;;; Keymap definitions
;;;

;; Firstly, keyboard mapping preceding menu keymap to make
;; key suggestions (N),(P) etc. in menu (in emacs19).

;; I believe timidity-mode-map is not void as variable here.
(progn
  ;; Default (initial) key binding for our menu handler. Or, perhaps
  ;; you might like [mouse-3] (may not be nice since emacs may too busy):
  (define-key timidity-mode-map [down-mouse-3] 'timidity-mode-menu-map-func)

  ;; Some other mouse bindings (you may not prefer these)
  ;; (define-key timidity-mode-map [mouse-1] 'timidity-prev-file)
  ;; (define-key timidity-mode-map [mouse-2] 'timidity-next-file)

  (define-key timidity-mode-map "N" 'timidity-Next-file)
  (define-key timidity-mode-map "P" 'timidity-Prev-file)

  ;; Or you might want use these (use timidity-mode-menu-load-hook):
  ;; (define-key timidity-mode-map "n" 'timidity-Next-file)
  ;; (define-key timidity-mode-map "p" 'timidity-Prev-file)
  )

;; Just for docstring right here.
(defvar timidity-mode-menu-work-map nil
  "Internal use only.  Do not edit this variable's definition.
This map is dynamically defined. If you want to customize the menu design,
edit the menu-skelton variable timidity-mode-menu-bare-map.")

;;;(defvar timidity-sync-flist-menu t
;;;  "If non-nil the file list menu is synchronized with play list.")

;; This variable can be customizable.
;;(defconst timidity-mode-menu-bare-map ; while debuggin' this. 
(defvar timidity-mode-menu-bare-map
  (let ((map (make-sparse-keymap
	      "TiMidiy"	; This menu title is annoying in emacs-20.
	      ;; ""  ; might be like this.
	      )))
    ;; Define in reverse order.
    (define-key map [quit]
      (cons "Quit" 'timidity-quit))
    (define-key map [replay]
      (cons "Replay" 'timidity-replay-cmd))
    (define-key map [tglpause]
      (cons "Pause" 'timidity-menu-toggle-pause))
    (define-key map [sep100] '("--" undefined))

    (define-key map [spectrogram]
      (cons "Spectrogram" 'timidity-menu-spectrogram))

    (define-key map [sep99] '("--" undefined))

    (define-key map [drumch]
      (cons "Drum Channel" 'timidity-select-drumchannel))
    (define-key map [vocdcr]
      (cons "Voice decrement" 'timidity-menu-voice-decrement))
    (define-key map [vocinc]
      (cons "Voice increment" 'timidity-menu-voice-increment))

    (define-key map [sep97] '("--" undefined))

    ;; I don't know if these two reverb command menu is working good or not...
    (define-key map [revtime] 
      (cons "Reverb Time"
	    '(keymap "Reverb Time"
		     (rtime1 "time4" (nil) . timidity-menu-reverb-time-4)
		     (rtime2 "time5" (nil) . timidity-menu-reverb-time-5)
		     (rtime3 "time6" (nil) . timidity-menu-reverb-time-6))))
    (define-key map [revffect] 
      (cons "Reverb Effect"
	    '(keymap "Reverb Effect"
		     (effect1 "effect1" (nil) . timidity-menu-reverb-effect-1)
		     (effect2 "effect2" (nil) . timidity-menu-reverb-effect-2)
		     (effect3 "effect3" (nil) . timidity-menu-reverb-effect-3))))

    (define-key map [sep94] '("--" undefined))

    (define-key map [keydown]
      (cons "Key down" 'timidity-menu-key-down))
    (define-key map [keyup]
      (cons "Key up" 'timidity-menu-key-up))

    (define-key map [sep94a] '("--" undefined))

    (define-key map [softer]
      (cons "Softer" 'timidity-menu-softer))
    (define-key map [louder]
      (cons "Louder" 'timidity-menu-louder))

    (define-key map [sep3] '("--" undefined))

    (define-key map [slower]
      (cons "Slower" 'timidity-menu-slower))
    (define-key map [faster]
      (cons "Faster" 'timidity-menu-faster))

    (define-key map [sep2] '("--" undefined))

    (define-key map [backward]
      (cons "Backward" 'timidity-menu-backward))
    (define-key map [forward]
      (cons "Forward" 'timidity-menu-forward))

    (define-key map [sep1] '("--" undefined))

;;;    ;; Should be updated dynamically
;;;    (define-key timidity-mode-menu-work-map [tglsync] 
;;;      (cons (if timidity-sync-flist-menu "Unsync file list" "Sync file list")
;;;	    'timidity-toggle-sync-flist))
    (define-key map [load]
      (cons "Load file list" 'timidity-load-file))
    (define-key map [loadlist]
      (cons "Load file" 'timidity-load-file-list))
    ;; Having [clearflist] in this section is not way cool.
    ;; Defining [clearflist] in file list submenu looks good, but,
    ;; user may want to clear file list even when he made too tall one.
    ;; Putting [clearflist] here enables such clearing job.
    (define-key map [clearflist]
      (cons "Clear file list" 'timidity-menu-clear-flist))

    (define-key map [sep1a] '("--" undefined))

    (define-key map [previous]
      (cons "Previous" 'timidity-Prev-file))
    (define-key map [next]
      (cons "Next" 'timidity-Next-file))
    map) 
"A bare menu keymap skelton for timidity-mode \(without file list menu\).
User customizable.  If you modified this keymap, eval next expression
to force update working copy of the keymap.

\(timidity-mode-menu-init t\)")

;; Rehash above map here.
(x-popup-menu nil timidity-mode-menu-bare-map)

;;;
;;; Some utils (some are by defsubst)
;;;

(defsubst timidity-init-work-map ()
  "Create bare timidity-mode-menu-work-map \(without file list menu\) out of \
timidity-mode-menu-bare-map."
  ;; Eventually the same code as the (defvar timidity-mode-menu-bare-map..
  ;; was here.  Guess this is faster than that.
  ;; The bare map is already rehashed.
  (setq timidity-mode-menu-work-map (copy-keymap timidity-mode-menu-bare-map)))

(defun timidity-mode-menu-init (&optional force)
  "Init menu keymaps for timidity-mode-menu.
For timidity-mode-hook, or "
  ;; Update some menu title.
  (if (or force (null timidity-mode-menu-work-map))
      (progn
	;; Order significant
	(timidity-init-work-map)
;;;	(define-key timidity-mode-menu-work-map [tglsync] 
;;;	  (cons (if timidity-sync-flist-menu "Unsync file list" "Sync file list")
;;;		'timidity-toggle-sync-flist))
	))
  (if (or force (or timidity-next-files timidity-prev-files))
      (timidity-build-flist-menu 'force)))

(defsubst timidity-x-popup-menu (ev map)
  "On EVENT, raise a menu out of KEYMAP and returns cons of \
selected key and definition.
X-popup-menu just returns a key when its input is a keymap,
but this returns final definition always, even from nested menu."
  (let ((val (x-popup-menu ev map)))
    ;; returns (cons val def) for future extension.
    (if val (cons val (lookup-key map (apply 'vector val))) nil) ))

;;; 
;;; Maintainers for the file list menu.
;;; 

;; The rehashing speed of x-popup-menu is supposed to be dependent on how many
;; keymap items are modified (that is how may files are added
;; (drag-and-dropped) or moved from timidity-next-files to timidity-prev-files
;; etc.).  The profiling was done by profile functions dired-dd-bogo-perf and
;; dired-dd-bogo-profile, which can be found dired-dd.el (dired-dd can be
;; retrieved from the URL notice at the top of this program).

(defvar timidity-old-flist nil "Internal use only.")

(defun timidity-build-flist-menu (&optional force)
  "Setups a file list menu in the main menu-keymap timidity-mode-menu-work-map.
Joins timidity-next-files and timidity-prev-files to create a file list.

BUG: can't split to submenus when the menu is too tall.
Perhaps sorting items is necessary for the split menu, and it is too
heavy job because emacs is so busy while timidity is working."
  (let ((fl (append
	     (reverse timidity-next-files)
	     timidity-prev-files))
	(flkeymap))
    (if (null fl)			; No file list at all, then
	;; Make (clear) brand new menu whole map (ready-rehashed, maybe).
	(timidity-init-work-map)
      ;; This query's overhead is quite smaller than x-popup-menu.
      (if (and (null force)
	       (equal timidity-old-flist fl))
	  ()
	(setq timidity-old-flist fl	; Save current as old for next call.
	      flkeymap (timidity-make-flist-keymap fl))
	;; (x-popup-menu nil flkeymap)  ; Not here, but at last.
	(define-key timidity-mode-menu-work-map [sep999]
	  (cons "--" 'undefined))
	(define-key timidity-mode-menu-work-map [filelist]
	  (cons "Select File" flkeymap))
	(x-popup-menu nil timidity-mode-menu-work-map)
	))))

;; Tester
;;(progn (timidity-build-flist-menu 'force)
;;(x-popup-menu t timidity-mode-menu-work-map))

;; defsubst (or defun maybe) just for tuning purpose (in future ?)
(defsubst timidity-update-menu-item (fmenu key def)
  (define-key fmenu key def))

(defun timidity-make-flist-keymap (fl)
  "Make, and return a menu keymap out of a filename list FILELIST."
  (let ((iindex 0)			; item index in whole map
	(findex)			; file index in next/prev filelist
	(file)				; file name
	(fmenu (make-sparse-keymap "Select File"))) ; Always create new one.
    ;; At first, start indexing timidity-next-files in reverse order
    (setq findex (1- (length timidity-next-files)))
    (mapcar '(lambda (unused)
	       (setq file (car fl))
	       (timidity-update-menu-item
		fmenu (vector iindex)
		(cons file (cons 'next (cons findex file))))	       
	       (setq
		fl (cdr fl)
		findex (1- findex)	; doing 'next
		iindex (1+ iindex)))
	    timidity-next-files)	; Just for length info
    ;; Insert separator always (even if on bottom).
    (timidity-update-menu-item fmenu (vector iindex) (cons "--" 'undefined))
    (setq iindex (1+ iindex))
    ;; Then, indexing timidity-prev-files in normal order.
    (setq findex 0)
    (mapcar '(lambda (unused)
	       (setq file (car fl))
	       (timidity-update-menu-item
		fmenu (vector iindex)
		(cons file (cons 'prev (cons findex file))))
	       (setq
		fl (cdr fl)
		findex (1+ findex)	; doing 'prev
		iindex (1+ iindex)))
	    timidity-prev-files)	; Just for length info
    fmenu))

;;;
;;; Synchronizing flist menu and timidity-{next,prev}-files
;;; From 1.5. 
;;; Rather hairy. Includes not only integer index method but also
;;; equal method, which is not necessary for current keydef contains index.
;;;

(defun timidity-find-file-in-list (pred lis &optional rev)
  "Find ELEMENT, or N'TH-ELEMENT (when 1st arg is integer) in LIST.
Result is returned as a list of found part \(`member' compatible\) and
unfound part. If the search failed, the whole list is copied as unfound part.
If optional arg REVERSE is non-nil, LIST is reversed before find process."
  (let ((found nil) (count 0) (found-list) (unfound-list))
    (if rev (setq lis (reverse lis)))
    (mapcar
     '(lambda (e)
	(if (null found)    ; don't test anymore if already non-nil
	    (setq found (if (integerp pred) (= count pred) (equal pred e))))
	(setq count (1+ count))
	(if found
	    (setq found-list (append found-list (list e)))
	  (setq unfound-list (append unfound-list (list e))) ))
     lis)
    (list found-list unfound-list)))

(defun timidity-find-file-in-previous (f)
  "Find ELEMENT, or N'TH-ELEMENT in (when arg is integer) timidity-prev-files.
Returns list of maybe-new-timidity-prev-files, and
maybe-new-top-of-timidity-next-files, or nil if the search failed."
  (let ((ffl (timidity-find-file-in-list f timidity-prev-files nil)))
    (if (car ffl)
	(list (car ffl) (reverse (cadr ffl)))
      nil)))

(defun timidity-find-file-in-next (f)
  "Find ELEMENT, or N'TH-ELEMENT in (when arg is integer) timidity-next-files.
Returns list of maybe-new-timidity-next-files, and
maybe-new-top-of-timidity-prev-files, or nil if the search failed."
  (let ((ffl (timidity-find-file-in-list f timidity-next-files nil)))
    (if (car ffl)
	(list (cdar ffl) (cons (caar ffl) (reverse (cadr ffl))))
      nil)))

;;;
;;; 1st initialization
;;;

;; Build the first keymap for the menu and rehash here.
;; Tester: (x-popup-menu t timidity-mode-menu-work-map)
;; If user repeatedly loaded this file, do not destroy file list menu (maybe)
(timidity-mode-menu-init) ; (Maybe) at load (require) time only.

;; For restart (add-hook takes care of duplication on reloading)
(add-hook 'timidity-mode-hook 'timidity-mode-menu-init 'append)

;; This first rehash takes 260000 usec (0.26 s) or so
;; on my P5 166MHz+emacs-20.2 (v1.4 or older).  emacs-19.28 runs a bit faster.
(x-popup-menu nil timidity-mode-menu-work-map)

;;
;; A direct keymapping method example (Uses fset'ed alias).
;; This method is not applicable to this program, since the
;; current [filelist] keymap format can not be supported.
;;
;; A defect of function method in current version is that describe-key
;; command can not be used to get docstring for each menu item.
;;
;; (fset 'Ftimidity-mode-menu-map timidity-mode-menu-work-map)
;; (define-key timidity-mode-map [mouse-3] 'Ftimidity-mode-menu-map)
;;

;;;
;;; An entry point (don't use a direct keymap binding)
;;;

;;(defun timidity-mode-menu-map-func (ev &optional arg)
;;  (interactive "e\nP")
;;  (if timidity-sync-flist-menu
;;      (timidity-mode-menu-map-func-sync ev arg)
;;    (timidity-mode-menu-map-func-no-sync ev arg)))

;; [filelist] menu synchronizes with timidity-{next,prev}-files update.
;;(defun timidity-mode-menu-map-func-sync (ev &optional arg)
(defun timidity-mode-menu-map-func (ev &optional arg)
  "On EVENT, pop up timidity-mode-menu.
Selecting a file from menu synchronizes menu and timidity-{next,prev}-files."
  (interactive "e\nP")
  ;; (mouse-set-point ev) ;; perhaps just selecting window is good enough. 
  ;; (select-window (posn-window (event-start ev))) ;; No, not required too.
  (timidity-build-flist-menu 'force)	; Must support update by keyboard.
  (let ((def (cdr (timidity-x-popup-menu ev timidity-mode-menu-work-map))))
    (if def
	(cond
	 ((commandp def)
	  (call-interactively def))
	 ((consp def)
	  ;; New in 1.5
	  (timidity-update-flist-by-keydef def))
	 (t (error (format "BUG. Invalid Menu entry: %s" def)))))))

;; In 1.5, timidity-x-popup-menu returns stuffs such as:
;; (setq v (timidity-x-popup-menu t timidity-mode-menu-work-map))
;;   => ((filelist 8) prev 1 . "~/Bach/new.bach/jsb1080a.mid.gz")
;; (car v)              ;; key
;;   => (filelist 8)
;; (setq def (cdr v))   ;; definition
;;   => (prev 1 . "~/Bach/new.bach/jsb1080a.mid.gz")
;; (car def)            ;; filelist spec
;;   => prev
;; (cadr def)           ;; index in the list
;;   => 1
;; (cddr (cdr v)) ;; file name
;;   => "~/Bach/new.bach/jsb1080a.mid.gz"

;; Called only from timidity-mode-menu-map-func
;; So, (timidity-build-flist-menu 'force) is not necessary.
(defun timidity-update-flist-by-keydef (def)
  "Update timidity-next-files and timidity-prev-files by KEYDEF.
KEYDEF must be in the format in [filelist] map in timidity-mode-menu-work-map."
  (let ((file) (spec))
    (setq spec (car-safe def))
    (cond
     ((eq 'prev spec)
      (raw-timidity-load-file (cddr def))
      (let* ((findex (cadr def))
	     (result (timidity-find-file-in-previous findex))
	     (newprev (car result))
	     (newnext (append (cadr result) timidity-next-files)))
	(if (null result)
	    (error (format "BUG ! find-file-in-previous barfed %s" def))
	  (setq timidity-prev-files newprev
		timidity-next-files newnext))))
     ((eq 'next spec)
      (raw-timidity-load-file (cddr def))
      (let* ((findex (cadr def))
	     (result (timidity-find-file-in-next findex))
	     (newnext (car result))
	     (newprev (append (cadr result) timidity-prev-files)))
	(if (null result)
	    (error (format "BUG ! find-file-in-next barfed %s" def))
	  (setq timidity-prev-files newprev
		timidity-next-files newnext))))
     (t (error (format "Unknown Menu entry: %s" def))))))

;; [filelist] menu's indicator does not synchronize with
;; timidity-{next,prev}-files's update (but new file-list
;; addition/clear/drag-and-drop is reflected to the menu).
(defun timidity-mode-menu-map-func-no-sync (ev &optional arg)
  "On EVENT, pop up timidity-mode-menu.
Probably faster than timidity-mode-menu-map-func.
File list menu's indicator does not synchronize with
timidity-{next,prev}-files's update, but new file-list
addition/clear/drag-and-drop is reflected to the menu."
  (interactive "e\nP")
  ;; (mouse-set-point ev) ;; perhaps just selecting window is good enough. 
  ;; (select-window (posn-window (event-start ev))) ;; No, not required too.
  (timidity-build-flist-menu nil)	; Faster (maybe).
  (let ((def (cdr (timidity-x-popup-menu ev timidity-mode-menu-work-map)))
	(file) (spec))
    (if def
	(cond
	 ((commandp def)
	  (call-interactively def))
	 ;; In 1.5 
	 ((consp def)
	  (setq spec (car-safe def))
	  (cond
	   ((or (eq 'prev spec) (eq 'next spec))
	    ;; Do not sync with flist (see timidity-mode-menu-map-func), and
	    ;; neither (timidity-build-flist-menu 'force) required because
	    ;; selecting a file in flist does not affects {prev,next}-files.
	    (raw-timidity-load-file (cddr def)))
	   (t (error (format "Unknown Menu entry: %s" def)))))
	 (t (error (format "BUG. Invalid Menu entry: %s" def)))))))

;;;
;;; Some TiMidity functions to be registered in the menu
;;; Most of '(lamda) in timidity-mode-menu-bare-map should be move to here
;;; in future version.
;;;

;; 
;; In version 1.5 or older versions, most of the functions below are directly
;; defined in timidity-mode-menu-bare-map. From version 1.6 all the lambda's
;; are changed to real functions.
;; 
;; The very first
;;   (dired-dd-bogo-perf '(x-popup-menu nil timidity-mode-menu-bare-map) 1)
;; marked 219690 usec (v1.5) and 207821 usec (v1.6) respectively on my 
;; P5 166MHz+emacs-20.2.
;;   (- 219690 207821) => 11869 usec (0.01 sec) earned.
;;   (* 100 (/ 11869.0 219690.0)) => 5.4 % speeding.
;; Not so fast as I have expected...
;;

(defun timidity-menu-toggle-pause (e &optional arg)
  "Pause or restart playing midi file."
  (interactive "e\np")
  (timidity-simple-cmd arg ? ))

(defun timidity-menu-spectrogram (e &optional arg)
  "Displays or stop displaying spectrogram."
  (interactive "e\np")
  (timidity-simple-cmd arg ?g))

(defun timidity-menu-voice-decrement (e &optional arg)
  "Decrease number of voice."
  (interactive "e\np")
  (timidity-simple-cmd arg ?o))

(defun timidity-menu-voice-increment (e &optional arg)
  "Increase number of voice."
  (interactive "e\np")
  (timidity-simple-cmd arg ?O))

(defun timidity-menu-reverb-time-4 (e &optional arg)
  "Reverb time spec 4."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?4))

(defun timidity-menu-reverb-time-5 (e &optional arg)
  "Reverb time spec 5."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?5))

(defun timidity-menu-reverb-time-6 (e &optional arg)
  "Reverb time spec 6."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?6))

(defun timidity-menu-reverb-effect-1 (e &optional arg)
  "Reverb effect spec 1."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?1))

(defun timidity-menu-reverb-effect-2 (e &optional arg)
  "Reverb effect spec 2."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?2))

(defun timidity-menu-reverb-effect-3 (e &optional arg)
  "Reverb effect spec 3."
  (interactive "e\np")
  (timidity-simple-cmd 1 ?3))

(defun timidity-menu-key-down (e &optional arg)
  "Key of the tune goes down."
  (interactive "e\np")
  (timidity-simple-cmd arg ?-))

(defun timidity-menu-key-up (e &optional arg)
  "Key of the tune goes up."
  (interactive "e\np")
  (timidity-simple-cmd arg ?+))

(defun timidity-menu-softer (e &optional arg)
  "Plays softer. 4 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 4 arg) ?v))

(defun timidity-menu-louder (e &optional arg)
  "Plays louder. 4 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 4 arg) ?V))

(defun timidity-menu-slower (e &optional arg)
  "Plays slower.  5 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 5 arg) ?<))

(defun timidity-menu-faster (e &optional arg)
  "Plays faster.  5 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 5 arg) ?>))

(defun timidity-menu-backward (e &optional arg)
  "Rewind backward. 4 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 4 arg) ?b))

(defun timidity-menu-forward (e &optional arg)
  "Wind forward. 4 times of ARG is propergated to timidity-simple-cmd."
  (interactive "e\np")
  (timidity-simple-cmd (* 4 arg) ?f))

(defun timidity-menu-clear-flist (e &optional arg)
  "Clears play file list."
  (interactive "e\np")
  (setq timidity-next-files nil
	timidity-prev-files nil
	;; should clear this (user may drop the same files after clear).
	timidity-old-flist nil)
  ;; Calling (timidity-build-flist-menu 'force) might generate some noise,
  ;; or maybe this is overkill here (in `sync' method).
  (timidity-build-flist-menu 'force))

;; Unused
;;(defun timidity-toggle-sync-flist ()
;;  "Toggle"
;;  (interactive)
;;  (setq timidity-sync-flist-menu (if timidity-sync-flist-menu nil t))
;;  (timidity-mode-menu-init 'force))

;; Unused
;;(defun timidity-sync-flist ()
;;  "Switch to sync mode in which file list sync's with play list."
;;  (interactive)
;;  (setq timidity-sync-flist-menu t)
;;  (timidity-mode-menu-init 'force))

;; Unused
;;(defun timidity-unsync-flist ()
;;  (interactive)
;;  "Switch to unsync mode in which file list unsync's with play list."
;;  (interactive)
;;  (setq timidity-sync-flist-menu nil)
;;  (timidity-mode-menu-init 'force))

;; Unused
;;(defun timidity-menu-next-file (e &optional arg)
;;  "Compatible to command timidity-next-file, but make rehash menu keymap"
;;  (interactive "e\np")
;;  ;; Use Mine. ARG not supported in original.
;;  (call-interactively 'timidity-Next-file)
;;  (timidity-build-flist-menu 'force))

;; Unused
;;(defun timidity-menu-prev-file (e &optional arg)
;;  "Compatible to command timidity-prev-file but make rehash menu keymap"
;;  (interactive "e\np")
;;  ;; Use Mine. ARG not supported in original.
;;  (call-interactively 'timidity-Next-file)
;;  (timidity-build-flist-menu 'force))

;;
;; New keyboard commands.
;;

(defun timidity-Next-file (&optional arg)
  "Compatible to timidity-next-file, except ARGth file is played."
  (interactive "p")
  (let ((f))
    (while (and timidity-next-files (> arg 0))
      (setq f (car timidity-next-files)
	    timidity-next-files (cdr timidity-next-files)
	    timidity-prev-files (cons f timidity-prev-files)
	    arg (1- arg)))
    (if f (timidity-load-file f))))

(defun timidity-Prev-file (&optional arg)
  "Compatible to timidity-prev-file, except ARGth file is played."
  (interactive "p")
  (if timidity-playing-flag
      (if (cdr timidity-prev-files)
	  (let ((f))
	    (while (and timidity-prev-files (> arg 0))
	      (setq f (car timidity-prev-files)
		    timidity-prev-files (cdr timidity-prev-files)
		    timidity-next-files (cons f timidity-next-files)
		    arg (1- arg)))
	    (timidity-load-file (car timidity-prev-files)))
	(timidity-load-file (car-safe timidity-prev-files)))
    (timidity-load-file (car-safe timidity-prev-files))))

;;
;; Yet another miscellaneous utils
;;

;; Selecting a file in the file list menu calls raw-timidity-load-file(),
;; which is almost compatible with timidity-load-file() and does not affect
;; variables timidity-next-files or timidity-prev-files (at least in this
;; version).

;; Maybe defined in my dired-dd-timidity.el.
;; emacs_c.c and timidity.el sounds like 
;; timidity-next-files and timidity-prev-files is updated automatically.
(or (fboundp 'raw-timidity-load-file)
    ;; emacs_c.c and timidity.el sounds like 
    ;; timidity-next-files and timidity-prev-files is updated
    ;; automatically by this:
    (defun raw-timidity-load-file (fname)
      ;; (interactive "fMIDI file: ")
      (if fname
	  (progn
	    (setq timidity-playing-flag t)
	    (timidity-run)
	    (setq fname (timidity-expand-file-name fname))
	    (garbage-collect)
	    (process-send-string timidity-cmd-process
				 (format "L\nPLAY %s\n" fname))
	    (message "MIDI File: %s" fname))
	(setq timidity-playing-flag nil))))

;; Run load-hook
(run-hooks 'timidity-mode-menu-load-hook)

;; From TiMidity++-1.0.1/interface/emacs_c.c
;; Perhaps most of these commands are available from the menu.
;;
;;    switch(cmd[0])
;;    {
;;      case 'L':
;;	return RC_LOAD_FILE;
;;      case 'V':
;;	*valp = 10 * n;
;;	return RC_CHANGE_VOLUME;
;;      case 'v':
;;	*valp = -10 * n;
;;	return RC_CHANGE_VOLUME;
;;      case '1':
;;      case '2':
;;      case '3':
;;	*valp = cmd[0] - '2';
;;	return RC_CHANGE_REV_EFFB;
;;      case '4':
;;      case '5':
;;      case '6':
;;	*valp = cmd[0] - '5';
;;	return RC_CHANGE_REV_TIME;
;;      case 'Q':
;;	return RC_QUIT;
;;      case 'r':
;;	return RC_RESTART;
;;      case 'f':
;;	*valp = play_mode->rate * n;
;;	return RC_FORWARD;
;;      case 'b':
;;	*valp = play_mode->rate * n;
;;	return RC_BACK;
;;      case ' ':
;;	return RC_TOGGLE_PAUSE;
;;      case '+':
;;	*valp = n;
;;	return RC_KEYUP;
;;      case '-':
;;	*valp = -n;
;;	return RC_KEYDOWN;
;;      case '>':
;;	*valp = n;
;;	return RC_SPEEDUP;
;;      case '<':
;;	*valp = n;
;;	return RC_SPEEDDOWN;
;;      case 'O':
;;	*valp = n;
;;	return RC_VOICEINCR;
;;      case 'o':
;;	*valp = n;
;;	return RC_VOICEDECR;
;;      case 'd':
;;	*valp = n;
;;	return RC_TOGGLE_DURMCHAN;
;;      case 'g':
;;	return RC_TOGGLE_SNDSPEC;
;;    }
