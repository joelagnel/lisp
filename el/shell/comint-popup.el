;;; comint-popup.el --- maybe show comint process output windows

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions, processes, comint
;; Status: Works in Emacs 19
;; Created: 1995-02-27
;; Public domain.

;; LCD Archive Entry:
;; comint-popup|Noah Friedman|friedman@splode.com|
;; maybe show comint process output windows|
;; $Date: 1999/10/08 11:15:21 $|$Revision: 1.2 $|~/misc/comint-popup.el.gz|

;; $Id: comint-popup.el,v 1.2 1999/10/08 11:15:21 friedman Exp $

;;; Commentary:

;; This program is enabled by putting something like the
;; following in your .emacs:
;;
;;     (eval-after-load "comint-popup"
;;       '(progn
;;          (defun comint-popup-add-popup-hook ()
;;            (add-hook 'comint-output-filter-functions
;;                      'comint-popup-buffer))
;;          (let ((h '(rlogin-mode-hook shell-mode-hook telnet-mode-hook)))
;;            (while h
;;              (add-hook (car h) 'comint-popup-add-popup-hook)
;;              (setq h (cdr h))))))
;;
;; This is somewhat complicated to avoid adding the hook to every inferior
;; comint process.  For example, you probably never want ange-ftp process
;; buffers to become visible.  (You could just disable pb-popup-mode
;; locally in that buffer and install these hooks globally, but you would
;; still be creating extra overhead in buffers where it wasn't necessary.)
;;
;; After all that, the variables below and `pb-popup-mode' (part of another
;; package on which this program depends) control how and when, e.g. a
;; shell buffer's output should cause a new window to pop up showing that
;; output, if it is not already in a display elsewhere.
;;
;; The general case is that if output appears in a buffer that's been idle
;; for some time (as opposed to continuously spewing), or the spewage
;; finally ends and the prompt becomes visible, then display the buffer.
;; It's also possible to list (via regexps) output that's considered
;; "urgent", such as shutdown broadcasts, talk requests, etc.

;;; Code:

(require 'pb-popup)

(defvar comint-popup-idle-threshold 30
  "*Minimum length of time considered for process to be idle.
If no output from a process has been received in this many seconds, then
make sure to pop up a window showing the recent output if it is not already
visible.

Setting this variable to -1 means that emacs will always try to display a
window with the recent output, regardless of idle time.

This variable may be made buffer-local.")

(defvar comint-popup-at-prompt-p t
  "*If non-nil, make process buffer visible whenever prompt appears.
Prompt in this case means that the most recent output ends with the prompt
defined by `comint-prompt-regexp'.

This variable may be made buffer-local.")

(defvar comint-popup-output-urgent-regexp-list nil
  "*Output from process considered urgent.
If the output from a process matches one of the regular expressions in this
list, make sure the process buffer is displayed regardless of idle time
limits or apparent end-of-process-output.

This variable may be made buffer-local.")


;; for internal use only.
(defvar comint-popup-last-event '(0 0))
(make-variable-buffer-local 'comint-popup-last-event)


(defsubst comint-popup-prompt-visible-p (beg end)
  (and comint-popup-at-prompt-p
       (save-match-data
         (let ((p (point)))
           (goto-char end)
           (prog1
               (and (re-search-backward comint-prompt-regexp beg t)
                    (= (match-end 0) end))
             (goto-char p))))))

(defsubst comint-popup-output-urgent-p (beg end)
  (let ((re comint-popup-output-urgent-regexp-list)
        (foundp nil)
        (p (point)))
    (while re
      (goto-char beg)
      (if (re-search-forward (car re) end t)
          (progn
            (setq re nil)
            (setq foundp t))
        (setq re (cdr re))))
    (goto-char p)
    foundp))

;; Compute the difference, in seconds, between a and b, two structures
;; similar to those returned by `current-time'.
;; Use addition rather than logand since that is more robust; the low 16
;; bits of the seconds might have been incremented, making it more than 16
;; bits wide.
(defsubst comint-popup-elapsed (a b)
  (+ (lsh (- (car b) (car a)) 16)
     (- (car (cdr b)) (car (cdr a)))))


;;;###autoload
(defun comint-popup-buffer (string)
  (cond
   ((string= string ""))
   (t
    (let* ((tm (current-time))
           (elapsed (comint-popup-elapsed comint-popup-last-event tm))
           (end (process-mark (get-buffer-process (current-buffer))))
           (beg (or (and (boundp 'comint-last-output-start)
                         comint-last-output-start)
                    (- end (length string)))))
      (setq comint-popup-last-event tm)

      (and (or (> elapsed comint-popup-idle-threshold)
               (comint-popup-prompt-visible-p beg end)
               (comint-popup-output-urgent-p beg end))
           (pb-popup (current-buffer)))))))

(provide 'comint-popup)

;; comint-popup.el ends here
