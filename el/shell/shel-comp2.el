;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!mintaka!mintaka.lcs.mit.edu!sra Mon Mar 19 12:41:53 1990
;Article 825 of gnu.emacs.bug:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!mintaka!mintaka.lcs.mit.edu!sra
;>From sra@lcs.mit.edu (Rob Austein)
;Newsgroups: gnu.emacs.bug
;Subject: Completing filenames in shell-mode
;Message-ID: <1990Mar16.054548.26107@mintaka.lcs.mit.edu>
;Date: 16 Mar 90 05:45:48 GMT
;Sender: news@mintaka.lcs.mit.edu
;Distribution: gnu
;Organization: ITS Preservation Society
;Lines: 47
;
;Here's a feature I've wanted for a while.  The following hack allows
;filename completion in a shell-mode buffer.  The behavior is basicly
;like tcsh's completion.  It doesn't do pop-up windows because there's
;no good way to clean them up automaticly except from the minibuffer.

;;; allspice.lcs.mit.edu:/u/sra/shell-file-complete.el, 15-Mar-1990 21:50, sra

(defvar shell-filename-chars
  "/~a-zA-Z0-9---_#=+.,"
  "*Characters that shell-filename-complete considers legitimate.")

(defun shell-filename-complete ()
  (interactive)
  (let (start name directory file completion)
    (save-excursion
      (beginning-of-line)
      (setq start (point)))
    (save-excursion
      (skip-chars-backward shell-filename-chars start)
      (setq start (point)))
    (setq name (buffer-substring start (point))
	  directory (file-name-directory name)
	  file (file-name-nondirectory name))
    (cond
     ((string= name "")
      (beep))
     ((and directory (not (file-directory-p directory)))
      (beep))
     ((not (setq completion
		 (file-name-completion file (or directory default-directory))))
      (beep))
     ((eq t completion)
      (message "[Sole completion]"))
     ((string= completion file)
      (message
       (mapconcat 'identity
		  (file-name-all-completions
		   file (or directory default-directory)) " ")))
     (t
      (insert (substring completion (length file)))))))

(setq shell-mode-hook
      (function
       (lambda ()
	 (define-key shell-mode-map "\C-c\C-i" 'shell-filename-complete))))

;--Rob Austein


