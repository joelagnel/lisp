;;; volume.el --- tweak your sound card volume from Emacs
;; Copyright (C) 2005  Daniel Brockman
;; Copyright (C) 1998, 2000, 2001, 2002, 2003, 2004, 2005
;;   Free Software Foundation, Inc.

;; Version: 0.5
;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/volume-el/
;; Created: The late night of 9th September, 2005

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Installation:

;; To use this program, put this file in your `load-path',
;; and put the following autoload in your ~/.emacs:

;;   (autoload 'volume "volume"
;;             "Tweak your sound card volume." t)

;; Then type M-x volume RET to run the program.  Of course,
;; use M-x customize-group RET volume RET to customize it.

;;; Commentary:

;; Tweaking the volume of the music used to be one of the
;; few things I constantly went outside of Emacs to do.
;; I just decided I've had enough of that, and so I wrote
;; this simple mixer frontend.

;; It comes with backend glue for aumix and amixer, but the
;; latter is pretty slow, so I have to recommend the former.
;; If you can't use either, writing your own glue should be
;; straightforward.  And if you do, please consider sending
;; the code to me, so I can integrate it into this file.

;;; Code:

(defgroup volume nil
  "Tweak your sound card volume."
  :group 'multimedia)

(defcustom volume-backend
  (cond ((executable-find "aumix") 'volume-aumix-backend)
        ((executable-find "amixer") 'volume-amixer-backend))
  "The set of primitives used by Volume to do real work.
Value is an alist containing entries `get', `set' and `nudge',
or the name of a variable containing such an alist."
  :type '(radio (const :tag "aumix" volume-aumix-backend)
                (const :tag "amixer" volume-amixer-backend)
                (const :tag "None" nil)
                (radio :tag "Custom" variable
                       (list :tag "Function alist" :greedy t
                             (cons :format "%v"
                                   (const :format "" get)
                                   (function :tag "Get"))
                             (cons :format "%v"
                                   (const :format "" set)
                                   (function :tag "Set"))
                             (cons :format "%v"
                                   (const :format "" nudge)
                                   (function :tag "Nudge")))))
  :group 'volume)

(defcustom volume-electric-mode t
  "Run Volume electrically, in the echo area.
Electric mode saves some space, but uses its own command loop."
  :type 'boolean
  :group 'volume)

(defface volume-bar
  '((t (:inverse-video t :weight bold)))
  "Face used for the indicator bar in Volume mode."
  :group 'volume)

(defun volume-backend-call (primitive &rest arguments)
  "Call PRIMITIVE from the current backend with ARGUMENTS.
See the variable `volume-backend'."
  (let ((backend (symbol-value (indirect-variable volume-backend))))
    (when (null backend)
      (error "No backend (see `volume-backend')"))
    (apply (cdr (assoc primitive backend)) arguments)))

(defvar volume-buffer nil
  "The current Volume buffer, or nil.")

(defun volume-call-process (program &rest arguments)
  "Like `shell-command-to-string', but doesn't have to use the shell."
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (eq 0 (apply 'call-process program nil t nil arguments))
        (error "Process `%s' exited abnormally"
               (mapconcat 'identity (cons program arguments) " "))))))


;;;; The aumix backend

(defvar volume-aumix-backend
  '((get . volume-aumix-get)
    (set . volume-aumix-set)
    (nudge . volume-aumix-nudge)))

(defgroup volume-aumix nil
  "The aumix backend."
  :group 'volume)

(defcustom volume-aumix-program "aumix"
  "The name of the aumix program."
  :type 'string
  :group 'volume-aumix)

(defcustom volume-aumix-device nil
  "The name of the mixer device, or nil for the default.
This corresponds to the `-d' option of aumix."
  :type '(choice (const :tag "/dev/mixer" nil)
                 (const "/dev/mixer1")
                 (const "/dev/mixer2")
                 (const "/dev/mixer3")
                 (const "/dev/mixer4")
                 file)
  :group 'volume-aumix)

(defcustom volume-aumix-extra-arguments nil
  "Extra arguments to pass to the aumix program."
  :type '(repeat string)
  :group 'volume-aumix)

(defun volume-aumix-call (&rest arguments)
  "Call aumix with ARGUMENTS and return the output."
  (apply 'volume-call-process volume-aumix-program
         (append (when volume-aumix-device
                   (list "-d" volume-aumix-device))
                 volume-aumix-extra-arguments
                 arguments)))

(defun volume-aumix-parse-output (string)
  "Parse the output of an aumix volume query.
Return the volume percentage as a floating-point number.
If STRING cannot be parsed, raise an error."
  (if (string-match "vol \\([0-9]+\\)" string)
      (float (string-to-number (match-string 1 string)))
    (error "Failed to parse aumix output")))

(defun volume-aumix-get ()
  "Return the current volume in percent, using aumix to get it."
  (volume-aumix-parse-output (volume-aumix-call "-vq")))

(defun volume-aumix-set (n)
  "Use aumix to set the current volume to N percent.
Return the new volume in percent."
  (volume-aumix-parse-output
   (volume-aumix-call (format "-v%d" n) "-vq")))

(defun volume-aumix-nudge (n)
  "Use aumix to change the volume by N percentage units.
Return the new volume in percent."
  (let ((sign (if (>= n 0) "+" "-")))
    (volume-aumix-parse-output
     (volume-aumix-call (format "-v%s%d" sign (abs n)) "-vq"))))


;;;; The amixer backend

(defvar volume-amixer-backend
  '((get . volume-amixer-get)
    (set . volume-amixer-set)
    (nudge . volume-amixer-nudge)))

(defgroup volume-amixer nil
  "The amixer backend."
  :group 'volume)

(defcustom volume-amixer-program "amixer"
  "The name of the amixer program."
  :type 'string
  :group 'volume-amixer)

(defcustom volume-amixer-card nil
  "The ALSA sound card number to use, or nil for the default.
This corresponds to the `-c' option of amixer."
  :type '(choice integer (const :tag "Default" nil))
  :group 'volume-amixer)

(defcustom volume-amixer-device nil
  "The ALSA device name to use, or nil for the default.
This corresponds to the `-D' option of amixer."
  :type '(choice string (const :tag "Default" nil))
  :group 'volume-amixer)

(defcustom volume-amixer-extra-arguments nil
  "Extra arguments to pass to the amixer program."
  :type '(repeat string)
  :group 'volume-amixer)

(defun volume-amixer-call (&rest arguments)
  "Call amixer with ARGUMENTS and return the output."
  (apply 'volume-call-process volume-amixer-program
         (append (when volume-amixer-card
                   (list "-c" (number-to-string volume-amixer-card)))
                 (when volume-amixer-device
                   (list "-D" volume-amixer-device))
                 volume-amixer-extra-arguments
                 arguments)))

(defun volume-amixer-control-has-volume-p (control)
  "Return non-nil if CONTROL uses the concept of a volume."
  (condition-case nil
      (string-match "Capabilities: .*volume"
                    (volume-amixer-call "get" control))
    (error nil)))

(defcustom volume-amixer-control
  (or (when (executable-find volume-amixer-program)
        (cond
         ((volume-amixer-control-has-volume-p "Master") "Master")
         ((volume-amixer-control-has-volume-p "PCM") "PCM")))
      "Master")
  "The name of the ALSA mixer control to manipulate."
  :type '(radio (const "Master") (const "PCM") string)
  :group 'volume-amixer)

(defun volume-amixer-parse-output (output)
  "Parse the OUTPUT of an amixer control dump.
Return the volume percentage as a floating-point number.
If OUTPUT cannot be parsed, raise an error."
  (if (string-match "\\[\\([0-9]+\\)%\\]" output)
      (float (string-to-number (match-string 1 output)))
    (error "Failed to parse amixer output")))

(defun volume-amixer-get ()
  "Return the current volume, using amixer to get it."
  (volume-amixer-parse-output
   (volume-amixer-call "get" volume-amixer-control)))

(defun volume-amixer-set (n)
  "Use amixer to set the current volume to N percent."
  (volume-amixer-parse-output
   (volume-amixer-call "set" volume-amixer-control
                       (format "%d%%" n))))

(defun volume-amixer-nudge (amount)
  "Use amixer to change the volume by N percentage units."
  (let ((sign (if (>= n 0) "+" "-")))
    (volume-amixer-parse-output
     (volume-amixer-call "set" volume-amixer-control
                         (format "%d%s" (abs n) sign)))))


;;;; User interface

(defun volume-get ()
  "Return the current volume in percent."
  (volume-backend-call 'get))

(defun volume-set (n)
  "Set the volume to N percent."
  (volume-backend-call 'set n))

(defun volume-nudge (n)
  "Change the volume by N percentage units.
Return either the new volume or nil, depending on the backend."
  (volume-backend-call 'nudge n))

(defun volume-show (&optional volume)
  "Display the current volume in the minibuffer.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (message "Volume: %d%%" (or volume (round (volume-get)))))

(defun volume-redisplay (&optional volume)
  "Update the Volume buffer to reflect the current volume.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (when (null volume)
    (setq volume (volume-get)))
  (let ((inhibit-read-only t))
    (set-buffer volume-buffer)
    (delete-region (point-min) (point-max))
    (insert "Volume: ")
    (let* ((bar-start (point))
           (available-width (- (window-width) bar-start))
           (bar-width (round (* (/ volume 100.0) available-width)))
           (label (format " %d%% " volume))
           (label-width (length label)))
      (insert-char ?\  available-width)
      (goto-char
       (+ bar-start
          (if (< bar-width label-width) (1+ bar-width)
            (/ (1+ (- bar-width label-width)) 2))))
      (delete-char label-width) (insert label)
      (put-text-property bar-start (+ bar-start bar-width)
                         'face 'volume-bar)
      (goto-char (+ bar-start bar-width)))))

(defun volume-update (&optional volume)
  "Maybe call `volume-show' or `volume-redisplay'; return VOLUME.
This function should be called by UI commands that change the volume."
  (prog1 volume
    (if volume-buffer
        ;; The electric command loop will trigger a redisplay
        ;; after each command anyway, so avoid doing it twice.
        (unless volume-electric-mode
          (volume-redisplay volume))
      (volume-show volume))))

(defun volume-error (string)
  "Either signal a real error, or manually beep and display STRING.
Real errors cannot be used in electric mode."
  (if (not volume-electric-mode)
      (error string)
    (beep)
    (with-current-buffer volume-buffer
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert string)
        (sit-for 2)))
    (volume-redisplay)))

(defun volume-assign (n)
  "Set the volume to N percent.
If N is negative, call `volume-raise' instead."
  (interactive "P")
  (if (integerp n)
      (if (< n 0) (volume-raise n)
        (volume-update (volume-set n)))
    (volume-error "Need integer argument")))

(defun volume-lower (n)
  "Lower the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (- (or n 1)))))

(defun volume-raise (n)
  "Raise the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (or n 1))))

(defun volume-minimize ()
  "Lower the volume as much as possible."
  (interactive)
  (volume-set 0))

(defun volume-maximize ()
  "Raise the volume as much as possible."
  (interactive)
  (volume-set 100))

(defun volume-assign-and-quit (&optional n)
  "Set the volume to N percent and then quit Volume mode.
If N is nil, just quit Volume mode."
  (interactive "P")
  (when (integerp n)
    (volume-redisplay (volume-assign n))
    (sit-for 1))
  (volume-quit))

(defun volume-quit ()
  "Quit Volume mode."
  (interactive)
  (if volume-electric-mode
      (throw 'volume-done nil)
    (condition-case nil
        (while (get-buffer-window volume-buffer)
          (delete-window (get-buffer-window volume-buffer)))
      (error nil))
    (kill-buffer volume-buffer)
    (setq volume-buffer nil)))

(defun volume-mode ()
  "Major mode for twiddling your audio volume.

\\{volume-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'volume-mode)
  (setq mode-name "Volume")
  (use-local-map volume-mode-map)
  (volume-update)
  (run-mode-hooks 'volume-mode-hook))

(defvar volume-mode-map
  (let ((map (make-sparse-keymap))
        (lower-more (lambda (n)
                      (interactive "p")
                      (volume-lower (* n 10))))
        (raise-more (lambda (n)
                      (interactive "p")
                      (volume-raise (* n 10)))))
    (suppress-keymap map)
    (define-key map "b" 'volume-lower)
    (define-key map "f" 'volume-raise)
    (define-key map "B" lower-more)
    (define-key map "F" raise-more)
    (define-key map [left] 'volume-lower)
    (define-key map [right] 'volume-raise)
    (define-key map [(shift left)] lower-more)
    (define-key map [(shift right)] raise-more)
    (define-key map "s" 'volume-assign)
    (define-key map "a" 'volume-minimize)
    (define-key map "e" 'volume-maximize)
    (define-key map "g" 'volume-redisplay)
    (define-key map "\C-m" 'volume-assign-and-quit)
    (define-key map "q" 'volume-quit)
    (define-key map [escape escape] 'volume-quit)
    map)
  "Keymap for Volume mode.")

;; This function was based on the function `calculator' from
;; calculator.el, which is copyrighted by the FSF.
;;;###autoload
(defun volume ()
  "Tweak your sound card volume."
  (interactive)
  (setq volume-buffer (get-buffer-create "*volume*"))
  (if volume-electric-mode
      (unwind-protect
          (save-window-excursion
            (require 'electric) (message nil)
            (let ((echo-keystrokes 0)
                  (garbage-collection-messages nil))
              (set-window-buffer (minibuffer-window) volume-buffer)
              (select-window (minibuffer-window))
              (let ((old-local-map (current-local-map))
                    (old-global-map (current-global-map)))
                (use-local-map nil)
                (use-global-map volume-mode-map)
                (unwind-protect
                    (progn
                      (volume-redisplay)
                      (run-hooks 'volume-mode-hook)
                      (catch 'volume-done
                        (Electric-command-loop
                         'volume-done
                         ;; Avoid `noprompt' due to
                         ;; a bug in electric.el.
                         '(lambda () 'noprompt)
                         nil
                         (lambda (x y) (volume-redisplay)))))
                  (use-local-map old-local-map)
                  (use-global-map old-global-map)))))
        (when volume-buffer
          (kill-buffer volume-buffer)
          (setq volume-buffer nil)))
    (cond
     ((null (get-buffer-window volume-buffer))
      (let ((window-min-height 2)
            (split-window-keep-point nil))
        (select-window
         (split-window-vertically
          (if (and (fboundp 'face-attr-construct)
                   (plist-get (face-attr-construct 'modeline) :box))
              -3 -2)))
        (switch-to-buffer volume-buffer)))
     ((not (eq (current-buffer) volume-buffer))
      (select-window (get-buffer-window volume-buffer))))
    (volume-mode)
    (setq buffer-read-only t)))

(provide 'volume)
;;; volume.el ends here.
