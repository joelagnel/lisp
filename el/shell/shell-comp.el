;From utkcs2!emory!swrinde!ucsd!tut.cis.ohio-state.edu!sgtp.apple.juice.or.jp!shin Thu Jun 21 08:48:16 EDT 1990
;Article 2122 of gnu.emacs.bug:
;Path: utkcs2!emory!swrinde!ucsd!tut.cis.ohio-state.edu!sgtp.apple.juice.or.jp!shin
;>From: shin@sgtp.apple.juice.or.jp (Shinichirou Sugou)
;Newsgroups: gnu.emacs.bug
;Subject: file completion in shell-mode (final version)
;Message-ID: <9006200854.AA13102@sgtp.apple.juice.or.jp>
;Date: 20 Jun 90 08:54:41 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 152
;
;It seems that posting to gnu.emacs from my site fail.  I try to post to
;another one, gnu.emacs.bug.  I'll give up if this trial fails.
;
;
;Hi.  I posted file completion program a week ago, and this is the final
;version.
;
;What has changed is,
;
;(1) Fixed a bug (sorry).  In the previous program, the message
;
;        "[Complete, but not unique]"
;
;    has never appeared.
;
;(2) The messages now appeares not in the minibuffer but in the position where
;    the cursor lies.  Don't worry!  That message is automatically removed and
;    will NOT be sent to the shell process.
;
;    For example, in the following situation (assume '#' shows the cursor),
;
;        foo% ls -l ~/Makefile# [Complete, but not unique]
;
;    If you are satisfied with 'Makefile', please ignore "[Complete, but..."
;    message and type merely <CR>.  The correct contents
;
;        "ls -l ~/Makefile"
;
;    will be sent to the shell process.
;
;If you define the following 'setq' in your '.emacs' file,
;
;(setq shell-mode-hook
;      '(lambda ()
;         (define-key shell-mode-map "\C-c\C-i" 'my-shell-complete)
;         (define-key shell-mode-map "\C-c\?" 'my-shell-completion-help)))
;
;^c^i works just like ^i in the minibuffer.
;
;^c?  works just like ? in the minibuffer.
;
;Enjoy.

;; Revised 90/6/14 final version
;; File-completion-in-shell-mode by Shinichirou Sugou 90/6/8
;;        shin%sgtp.apple.juice.or.jp@uunet.uu.net
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defun my-shell-complete ()
  (interactive)
  (let* ((beg  (save-excursion
                 (re-search-backward "\\s ")
                 (1+ (point))))
         (end (point))
         (file (file-name-nondirectory (buffer-substring beg end)))
         (dir (or (file-name-directory (buffer-substring beg end)) ""))
         (lpc (file-name-completion file dir))
         (akin (file-name-all-completions file dir))
         (echo-keystrokes 0))           ; inhibit prefix key echo
    (cond ((eq lpc t)
           (my-momentary-string-display " [Sole completion]" (point) ?\0 ""))
          ((eq lpc nil)
           (ding t)
           (my-momentary-string-display " [No match]" (point) ?\0 ""))
          ((and (string= lpc file) (my-member lpc akin 'equal))
           (my-momentary-string-display " [Complete, but not unique]" (point) ?\0 ""))
          ((string= lpc file)
           (my-shell-completion-help akin))
          (t
           (delete-region beg end)
           (insert dir lpc)))))

(defun my-member (item list &optional testf)
  "Compare using TESTF predicate, or use 'eql' if TESTF is nil."
  (setq testf (or testf 'eql))
  (catch 'bye
    (while (not (null list))
      (if (funcall testf item (car list))
          (throw 'bye list))
      (setq list (cdr list)))
    nil))
(defun my-shell-completion-help (&optional akin)
  (interactive)
  (if (null akin)
      (let* ((beg  (save-excursion
                     (re-search-backward "\\s ")
                     (1+ (point))))
             (end (point))
             (file (file-name-nondirectory (buffer-substring beg end)))
             (dir (or (file-name-directory (buffer-substring beg end)) "")))
        (message "Making completion list...")
        (setq akin (file-name-all-completions file dir))))
  (if akin
      (with-output-to-temp-buffer " *Completions*"
        (display-completion-list (sort akin 'string-lessp)))
    (ding t)
    (let ((echo-keystrokes 0))
      (my-momentary-string-display " [No completion]" (point) ?\0 ""))))

(defun my-momentary-string-display (string pos &optional exit-char message) 
  "Emacs original momentary-string-display but the cursor positions at the
beginning of the STRING."
  (or exit-char (setq exit-char ?\ ))
  (let ((buffer-read-only nil)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end
        cur-pos)
    (unwind-protect
	(progn
          (goto-char pos)
          ;; defeat file locking... don't try this at home, kids!
          (setq buffer-file-name nil)
          (setq cur-pos (point))
          (insert-before-markers string)
          (setq insert-end (point))
          (goto-char cur-pos)
	  (message (or message "Type %s to continue editing.")
		   (single-key-description exit-char))
	  (let ((char (read-char)))
	    (or (eq char exit-char)
		(setq unread-command-char char))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

;CAUTION:
;  (1) Reply-command of your mail system may NOT generate my address correctly.
;      Please use the following address instead.
;
;        shin%sgtp.apple.juice.or.jp@uunet.uu.net
;
;  (2) I have no relation to Apple Computer Inc. :-)
;
;-----
;  Shin'ichirou Sugou   shin%sgtp.apple.juice.or.jp@uunet.uu.net


