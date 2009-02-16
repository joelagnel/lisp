;;; Saved through ges-version 0.3.3dev at 2004-05-14 13:15
;;; ;;; From:  Erik Arneson <g.e.sources@erik.aarg.net>
;;; ;;; Subject: gnus-anon 0.3 -- Type II and III Anonymous mail interface for Gnus.
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 19 Mar 2004 22:45:11 -0800
;;; ;;; Organization: AARG! Net

;;; [[PGP Signed Part:Undecided]]
;;; [1. text/plain]

;;; (This post was automatically generated using ges-post.el, version 0.4)

;;; A few changes in this version, including some new features and API
;;; alterations.

;;; 2004-03-19 Version 0.3
;;;    * Support for inserting and using SURBs with Type III messages.
;;;    * Replaced `mixminion-send-and-exit' and
;;;      `mixmaster-send-and-exit' with single wrapper function,
;;;      `anonymous-send-and-exit'.  Should be more seamless and
;;;      intuitive.
;;;    * Added new variables and customizations to support the same.
;;; 2004-03-19 Patch from Steve Youngs <sryoungs@bigpond.net.au>
;;;    * Fixes a bunch of customization variables so they're a bit nicer.

;;; I have tested the SURB stuff, and it works great.  The next version of
;;; Mixminion should have some new features which will greatly aid the
;;; creation of a mixminion-read-mode of some sort, so keep your anonymous
;;; eyeballs peeled.

;;; -- 
;;; ;; Erik Arneson <spam@erik.aarg.net> AARG Net <http://www.aarg.net/> ;;
;;; ;; GPG Key ID: 2048R/8B4CBC9C             <http://erik.arneson.org/> ;;
;;; ;;  "Civilization is only savagery silver-gilt." - H. Rider Haggard  ;;

;;; [2. gnus-anon.el --- application/emacs-lisp]

;;; gnus-anon.el --- Type II and III Anonymous mail interface for Gnus.

;; Copyright (C) 2004 Erik Arneson

;; Author:        Erik Arneson <erik@aarg.net>
;; Maintainer:    erik@aarg.net
;; Created:       2004-03-18
;; Version:       0.3
;; Keywords:      mail, mixmaster, mixminion, remailer, anonymous

(defvar gnus-anon-version "0.3"
  "Version of gnus-anon (anonymous-minor-mode)")

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; $Id: gnus-anon.el,v 1.9 2004/03/20 04:21:53 erik Exp $

;;{{{ Commentary:

;; This is an interface for Gnus to the Type II and Type III anonymous
;; remailer networks.  It has been tested with Mixmaster 2.9.1 and
;; Mixminion 0.0.7b1.
;;
;; This interface requires one or both of the above pieces of
;; software.  You can find Mixmaster at
;; <URL:http://mixmaster.sourceforge.net/> and Mixminion at
;; <URL:http://www.mixminion.net/>.

;; The Type II interface also allows posting news anonymously, but
;; this has not been extensively tested, so should be used with
;; caution.

;; Note that when sending mail anonymously, there are a few things
;; which need to be watched carefully in order to protect the users
;; privacy.  First, copies should not be kept of the user's anonymous
;; messages.  Gcc and Fcc headers should be ignored.  Second, logging
;; is a bad idea -- this interface tries to keep around just enough
;; information that the user could debug the latest message, but
;; recipients and detailed information are obscured.  Finally, as the
;; message is intended to be private and anonymous, old message
;; buffers should have an extremely short lifespan.  No provisions are
;; made for that here, but the user is encouraged to set
;; `message-max-buffers' to a nice low value.  3 is a good number for
;; anonymity.

;; That last sentence was a joke.  But really, keep the number low so
;; your old buffers get nuked quickly.

;; To use, add these lines to your init.el:
;;
;; (autoload 'anonymous-minor-mode "gnus-anon" nil t)
;; (autoload 'anonymous-send-and-exit "gnus-anon" "Send mail anonymously." t)
;; (autoload 'mixmaster-send "gnus-anon" nil t)
;; (autoload 'mixminion-send "gnus-anon" nil t)
;;
;; Also, make sure that anonymous-minor-mode is added to
;; message-setup-hook, like this:
;;
;; (add-hook 'message-setup-hook 'anonymous-minor-mode)
;;
;; You can then use `C-h m' to view the keybindings for this mode.
;;
;; The interface is customizable via `M-x customize-group RET
;; anonymous'.  Please browse through these settings and make sure
;; everything is configured properly for your system and needs.

;;}}}
;;{{{ History:

;; 2004-03-19 Version 0.3
;;    * Support for inserting and using SURBs with Type III messages.
;;    * Replaced `mixminion-send-and-exit' and
;;      `mixmaster-send-and-exit' with single wrapper function,
;;      `anonymous-send-and-exit'.  Should be more seamless and
;;      intuitive.
;;    * Added new variables and customizations to support the same.
;; 2004-03-19 Patch from Steve Youngs <sryoungs@bigpond.net.au>
;;    * Fixes a bunch of customization variables so they're a bit nicer.
;; 2004-03-18 Adapted from mixmaster.el

;;}}}
;;{{{ Todo:

;; 1. Add VM, RMAIL, and MH-E support.  I broke all of these when I
;;    switched this over to Gnus.  That's why it's named gnus-anon.el
;;    now.
;; 2. Check Type II message sizes agains remailer capabilities.
;; 3. Multiple recipients?
;; 4. Add a mixminion-read mode of some sort, to support decoding and
;;    decrypting of received SURB replies, along with decryption and
;;    other goodies.

;;}}}
;;{{{ Requirements

(require 'message)
(require 'mail-utils)
(require 'sendmail)

;; And what can I say?  I'm a Common Lisper at heart.
(require 'cl)

;;}}}
;;{{{ Customization

(defgroup anonymous nil
  "Anonymous mail and news."
  :group 'gnus)

(defcustom anonymous-default-method 'mixminion
  "*Default remailer type"
  :type '(choice (const :tag "Type II (Mixmaster)" mixmaster)
                 (const :tag "Type III (Mixminion)" mixminion))
  :group 'anonymous)

(defgroup mixmaster nil
  "Mixmaster mail and news interface"
  :group 'anonymous)

(defcustom mixmaster-directory
  (file-name-as-directory
   (expand-file-name "Mix" (getenv "HOME")))
  "*Mixmaster installation directory."
  :type '(directory :must-match t)
  :group 'mixmaster)

(defcustom mixmaster-path 
  (or (executable-find "mixmaster")
      (executable-find "mix")
      (expand-file-name "mix" mixmaster-directory))
  "*Path to the Mixmaster binary."
  :type '(file :must-match t)
  :group 'mixmaster)

(defcustom mixmaster-min-reliable nil
  "Minimum reliability of listed remailers.
A nil value means to list all remailers, regardless of reliability."
  :type 'integer
  :group 'mixmaster)

(defcustom mixmaster-max-latency nil
  "Maximum latency of listed remailers, in minutes.
A nil value means to list all remailers, regardless of latency."
  :type 'integer
  :group 'mixmaster)

(defgroup mixminion nil
  "Mixminion (type III anonymous mail) interface"
  :group 'anonymous)

(defcustom mixminion-path (executable-find "mixminion")
  "*Path to mixminion executable."
  :type 'file
  :group 'mixminion)

(defcustom mixminion-conf (expand-file-name ".mixminionrc"
					    (getenv "HOME"))
  "*Mixminion configuration file."
  :type 'file
  :group 'mixminion)

(defcustom mixminion-hops 6
  "*Default number of estimated hops."
  :type 'integer
  :group 'mixminion)

(defcustom mixminion-send-args '()
  "*Additional arguments for the \"mixminion send\" command."
  :type '(repeat string)
  :group 'mixminion)

(defcustom mixminion-reply-block-dir
  (file-name-as-directory
   (expand-file-name ".reply-blocks" (getenv "HOME")))
  "*Directory used for storing reply blocks."
  :type '(directory :must-match t)
  :group 'mixminion)

;;}}}
;;{{{ Minor Mode setup

;; Set up the keymap.
(defvar anonymous-minor-mode-map nil
  "Keymap for `anonymous-minor-mode'")

(unless anonymous-minor-mode-map
  (setq anonymous-minor-mode-map (make-sparse-keymap))

  ;; Universal interface
  (define-key anonymous-minor-mode-map [(control c) a m] 'anonymous-send-and-exit)
  
  ;; Type II interface
  (define-key anonymous-minor-mode-map [(control c) a a] 'mixmaster-attach-file)
  (define-key anonymous-minor-mode-map [(control c) a l] 'mixmaster-set-latency)
  (define-key anonymous-minor-mode-map [(control c) a c] 'mixmaster-set-copies)

  ;; Type III interface
  (define-key anonymous-minor-mode-map [(control c) a i] 'mixminion-insert-surb))

;; Set up the minor mode.

(defvar anonymous-minor-mode-string " Anon"
  "*String to pu in the mode line when `anonymous-minor-mode' is active.")

;; Install our minor mode.
(unless (assq 'anonymous-minor-mode minor-mode-alist)
  (push (cons 'anonymous-minor-mode 'anonymous-minor-mode-string) minor-mode-alist))
(unless (assq 'anonymous-minor-mode minor-mode-map-alist)
  (push (cons 'anonymous-minor-mode anonymous-minor-mode-map)
        minor-mode-map-alist))

(or (assq 'anonymous-minor-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(anonymous-minor-mode anonymous-minor-mode-string)
                minor-mode-alist)))

(defvar anonymous-minor-mode nil
  "If t, Mixmaster minor mode is active")

(make-variable-buffer-local 'anonymous-minor-mode)

;;;###autoload
(defun anonymous-minor-mode (&optional arg)
  "\nMinor mode for sending mail and news through the Type II and Type III
anonymous remailer networks.
\\<anonymous-minor-mode-map>

Universal anonymous mail interface:
\\[anonymous-send-and-exit]\t\tSend message anonymously

Type II anonymous mail:
\\[mixmaster-set-copies]\t\tSet the desired number of copies
\\[mixmaster-attach-file]\t\tAttach a file via Mixmaster
\\[mixmaster-set-latency]\t\tRequest the desired remailer latency

Type III anonymous mail:
\\[mixminion-insert-surb]\t\tInsert Type III SURB\n"
  (interactive)
  (setq anonymous-minor-mode
        (if (null arg) (not anonymous-minor-mode)
          (> (prefix-numeric-value arg) 0))))

;;}}}
;;{{{ Universal Interface

;;;###autoload
(defun anonymous-send-and-exit (&optional arg)
  "Send message using `mixmaster-send' or `mixminion-send', then, if no errors,
exit from mail buffer."
  (interactive "p")
  (let ((buf (current-buffer))
        (type (completing-read "Type of anonymous sending: "
                               '(("mixminion" . t)
                                 ("mixmaster" . t)) nil t
                                 (symbol-name anonymous-default-method)))
        func)
    (setq func (cond ((string= type "mixminion")
                      #'mixminion-send)
                     ((string= type "mixmaster")
                      #'mixmaster-send)))
    (when (and (funcall func arg)
	       (buffer-name buf))
      (if message-kill-buffer-on-exit
	  (kill-buffer buf)
	(bury-buffer buf)
	(when (eq buf (current-buffer))
	  (message-bury buf)))
      t)))

;;}}}
;;{{{ Type II Anonymous Mail (Mixmaster)

;;;###autoload
(defun mixmaster-send (&optional arg)
  "Send a Type II anonymous message via Mixmaster."
  (interactive "P")

  ;; Common message-mode setup stuff as we prepare to send.  We need
  ;; to be able to undo, and we need to be able to write to the whole
  ;; buffer if need be.
  (undo-boundary)
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (point-max) 'read-only nil))
  
  (let ((mailbuf (current-buffer))
        (errbuf (get-buffer-create " mixmaster errors"))
        (available-remailers (mixmaster-full-remailer-list))
        mix-args mail-to mail-subject mail-body remailer-list)
    (loop for remailer = (completing-read
                          (format "Choose a remailer or press RET when finished (%s):"
                                  (cond ((listp remailer-list)
                                         (join-list remailer-list ","))
                                        (t
                                         "")))
                          available-remailers
                          'mixmaster-remailer-filter)
      while (> (length remailer) 0)
      do (setq remailer-list (append remailer-list (list remailer))))
    (save-excursion
      (message "Preparing mail buffer for Mixmaster ...")
      (set-buffer mailbuf)
      
      (setq mail-subject (message-fetch-field "Subject")
            mail-body (buffer-substring-no-properties
                       (mail-text)
                       (point-max)
                       mailbuf))

      ;; Set up our mixmaster args
      (message "Calling Mixmaster ...")
      (setq mix-args
            (append (mixmaster-make-args mailbuf)
                    (list (format "--subject=%s" mail-subject)
                          (format "--chain=%s" (join-list remailer-list ",")))))

      ;; Is it a mail message, or a news post?
      (cond ((message-fetch-field "To")
             (setq mix-args (append mix-args
                                    (list "--mail"
                                          (format "--to=%s"
                                                  (message-fetch-field "To"))))))
            ((message-fetch-field "Newsgroups")
             (setq mix-args (append mix-args
                                    (list "--post"
                                          (format "--post-to=%s"
                                                  (message-fetch-field "Newsgroups")))))))

      ;; Before calling mixmaster, we want to make sure our output
      ;; buffer is empty.  It's a risk to leave that information
      ;; sitting around for long periods of time.  And besides, if you
      ;; are remailing a lot, it fills up!
      (save-excursion
        (set-buffer errbuf)
        (erase-buffer))
      
      ;; Call the mixmaster process.  Need to use `apply' here because
      ;; call-process doesn't like taking a list as the final argument.
      (save-excursion
        (set-buffer mailbuf)
        (message-goto-body)
        (unless (= 0 (apply 'call-process-region (point) (point-max)
                             mixmaster-path nil errbuf nil
                             mix-args))
          (error "Mixmaster call failed!")))

      ;; Now that sending is succeeded, we perform some of the cleanup
      ;; that `message-send' would normally do.
      (message "Sending anonymously via %s ... done." mixmaster-path)
      ;; Mark the buffer undeleted and remove any autosave stuff.
      (set-buffer-modified-p nil)
      (delete-auto-save-file-if-necessary t)
      (message-disassociate-draft)
      ;; Delete mail buffers!
      (message-do-send-housekeeping)
      t)))

(defun mixmaster-make-args (&optional buf)
  (let ((mix-args (list "-M" "--send" "--verbose")))
    (if (null buf)
        (setq buf (current-buffer)))
    (save-excursion
      (set-buffer buf)
      ;; Now let's build our command line.  These various headers are
      ;; still in the beta stages, but I think something like this
      ;; should work pretty well.
      (if (message-fetch-field "X-Nym")
          (push (format "--nym=%s" (message-fetch-field "X-Nym"))
                mix-args))
        
      (if (message-fetch-field "X-Attach")
          (let ((filename (expand-file-name (message-fetch-field "X-Attach"))))
            (if (file-exists-p filename)
                (push (format "--attachment=%s" filename) mix-args))
              (message (format "Mixmaster warning: %s not found!" filename))))
      (if (message-fetch-field "X-Latency")
          (push (format "--latency=%s" (message-fetch-field "X-Latency"))
                mix-args))
      (if (message-fetch-field "X-Copies")
          (push (format "--copies=%s" (message-fetch-field "X-Copies"))
                mix-args)))
    mix-args))

;;}}}
;;{{{ Calling Mixminion

;;;###autoload
(defun mixminion-send (&optional arg)
  "Send the current message anonymously using Mixminion.
If the \"To\" header is empty, the user will be prompted for the location
of a SURB file to use for the recipient.

For more details on Type III anonymous remailers, see
<URL:http://www.mixminion.net/>."
  (interactive "p")

  ;; In case we need to undo.
  (undo-boundary)
  
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (point-max) 'read-only nil))
  
  (let ((mailbuf (current-buffer))
        (errbuf (get-buffer-create " mixminion errors"))
        (mail-to          (message-fetch-field "To"))
        (mail-subject     (message-fetch-field "Subject"))
        (mail-in-reply-to (message-fetch-field "In-Reply-To"))
        (mail-references  (message-fetch-field "References"))
        (send-args (nconc (list (format "--path=~%d" (if (> 1 arg)
                                                         arg
                                                       mixminion-hops))
                                (format "--config=%s" (expand-file-name mixminion-conf)))
                          mixminion-send-args)))

    ;; Build our recipients.
    (if mail-to
        (progn
          (unless (= 1 (length (rfc822-addresses mail-to)))
            (error "Too many recipients specified!")) 
          (setq send-args (cons (format "--to=%s" (car (rfc822-addresses mail-to)))
                                send-args)))
      (setq send-args (cons (format "--reply-block=%s"
                                    (expand-file-name
                                     (read-file-name "Recipient SURB file: "
                                                     mixminion-reply-block-dir)))
                            send-args)))
      
    ;; Build our argument list.
    (if mail-subject
        (push (format "--subject=%s" mail-subject) send-args))
    (if mail-in-reply-to
        (push (format "--in-reply-to=%s" mail-in-reply-to) send-args))
    (if mail-references
        (push (format "--references=%s" mail-references) send-args))

    ;; Before calling anything, we clear out our old error messages.
    (save-excursion
      (set-buffer errbuf)
      (erase-buffer))

    (message "Sending anonymously via %s ..." mixminion-path)
    (save-excursion
      (message-goto-body)
      (unless (= 0 (apply 'call-process-region (point)
                          (point-max)
                          mixminion-path nil errbuf nil
                          (cons "send" send-args)))
        (error "Error calling %s!  Aborting send." mixminion-path)))

    ;; We've sent and succeeded.  We don't run hooks, but we need to
    ;; make sure we do a good job cleaning up the associated buffer.
    (message "Sending anonymously via %s ... done." mixminion-path)
    ;; Mark the buffer as unmodified and delete auto-save.
    (set-buffer-modified-p nil)
    (delete-auto-save-file-if-necessary t)
    (message-disassociate-draft)
    ;; Delete other mail buffers and stuff.
    (message-do-send-housekeeping)
    ;; Return success.
    t))

(defun mixminion-insert-surb (&optional arg)
  "Generate a single-use reply block (SURB) and insert it at point.
Takes an optional ARG, which if passed should be the number of
approximate hops in the reply block."
  (interactive "p")
  (undo-boundary)
  (let* ((arg 1)
         (dest (read-string "SURB destination: " user-mail-address))
         (output (generate-new-buffer "*mixminion surb*"))
         (args (list "--passphrase-fd=0"
                     "--verbose"
                     (format "--path=~%d" (if (< 1 arg)
                                              arg
                                            mixminion-hops))
                     (format "--to=%s" dest)))
         (password (read-passwd "Mixminion keyring password: "))
         proc status rv block)

    ;; Start our mixminion process.
    (setq proc (apply 'start-process-shell-command
                      "*Mixminion*" output mixminion-path
                      (cons "generate-surb"
                            args)))

    ;; Send the passphrase.
    (process-send-string proc (concat password "\n"))
    (process-send-eof proc)

    ;; Wait for it to finish.
    (while (eq 'run (process-status proc))
      (accept-process-output proc 5))

    (setq status (process-status proc))
    (setq rv (process-exit-status proc))

    ;; Check our return value.
    (unless (= rv 0)
      (error "%s exited abnormally: %d" mixminion-path rv))

    ;; Fetch our reply block.
    (save-excursion
      (let (begin end)
        (set-buffer output)
        (goto-char (point-min))
        (unless (re-search-forward "^-----BEGIN TYPE III REPLY BLOCK-----" nil t)
          (error "Reply block not found"))
        (setq begin (match-beginning 0))
        (unless (re-search-forward "^-----END TYPE III REPLY BLOCK-----" nil t)
          (error "Reply block not terminated properly"))
        (setq end (match-end 0))
        (setq block (buffer-substring begin (+ end 1)))))

    (save-excursion
      (insert-string block)
      (kill-buffer output))
    t))

;;}}}
;;{{{ Header creation

(defun mixmaster-attach-file (filename)
  "Use Mixmaster to attach a file to the current mail message."
  (interactive "fAttach File: ")
  (mixmaster-set-header "X-Attach" (expand-file-name filename)))

(defun mixmaster-set-latency (hours)
  "Request of Mixmaster that the message have a specified latency."
  (interactive "nRequested latency, in hours: ")
  (mixmaster-set-header "X-Latency" (int-to-string hours)))

(defun mixmaster-set-copies (copies)
  "Ask Mixmaster to send a specified number of hours.
This is only useful if you are using random remailers."
  (interactive "nNumber of copies to be sent: ")
  (mixmaster-set-header "X-Copies" (int-to-string copies)))

(defun mixmaster-set-header (header value)
  (save-excursion
    (if (message-fetch-field header)
        (let (start end)
          (mail-position-on-field header)
          (setq end (point))
          (re-search-backward ": ")
          (setq start (+ (point) 2))
          (delete-region start end)))
    (mail-position-on-field header)
    (insert value)))

;;}}}
;;{{{ Info and Support

(defun mixmaster-remailer-list (&optional type2-list)
  (unless type2-list
    (setq type2-list (expand-file-name "type2.list" mixmaster-directory)))
  (let ((buf (find-file-noselect type2-list))
        list)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (setq list (loop while (search-forward-regexp "^\\([a-z0-9]+\\) \\([^ ]+\\) .*$" nil t)
                   collect (cons (match-string 1) (match-string 2))))
      (kill-buffer buf))
    list))

(defun mixmaster-full-remailer-list (&optional mlist-txt)
  "Parse an mlist2.txt remailer statistics file and return an alist with members
like this:

   (REMAILER (OPTIONS) RELIABILITY LATENCY)

Where REMAILER is a string with the name of the remailer, OPTIONS is a
list of symbols describing various settings for this particular
remailer, RELIABILITY is a floating point number representing the
reliability of the remailer (with 100.0 being the best), and LATENCY
is the number of minutes on average a message takes to make it through
this remailer."
  (if (null mlist-txt)
      (setq mlist-txt (expand-file-name "mlist2.txt" mixmaster-directory)))
  (if (not (file-readable-p mlist-txt))
      (error (format "Cannot read mlist2.txt from %s" mlist-txt)))
  (let ((mybuf (find-file-noselect mlist-txt))
        alist)
    (save-excursion
      (set-buffer mybuf)
      (goto-char (point-min))

      ;; Find the end of the mlist.txt header.
      (re-search-forward "^--------------------------------------------$" nil t)
      (forward-line 1)
      (beginning-of-line)

      ;; Collect statistics.
      (setq alist (loop while (re-search-forward
                               (concat
                                "^\\([a-z0-9]+\\) +"        ; remailer name
                                "[\\?0-9]+ +"               ; latent-hist
                                "\\([0-9]*:[0-9][0-9]\\) +" ; latency
                                "[+0-9\\?]+ +"              ; uptime-hist
                                "\\([0-9]+\\.[0-9]+\\)%  "  ; reliability
                                "\\([ a-zA-Z0-9]+\\)$")     ; options
                               nil t)
                    collect (list (match-string 1)
                                  (mixmaster-option-list  (match-string 4))
                                  (string-to-number (match-string 3))
                                  (mixmaster-calc-latency (match-string 2)))))
      (kill-buffer mybuf))
    alist))

(defun mixmaster-calc-latency (latstr)
  (let (elements)
    (cond ((stringp latstr)
           (setq elements (split-string latstr ":"))
           (if (>= (length elements) 2)
               (+ (* (string-to-int (nth 0 elements)) 60)
                  (string-to-int (nth 1 elements)))
             0))
          (t
           0))))

(defun mixmaster-option-list (option-string)
  "Parse an option string and return a capability list."
  (loop for char in (string-to-char-list option-string)
    with column = 0
    do (incf column)
    when (not (= char ?\ ))
    collect (cond ((= char ?D) "middle")
                  ((= char ?P) "post")
                  ((= char ?M) "mix")
                  ((= char ?R) "remix")
                  ((= char ?H) "hybrid")
                  ((= char ?G) "repgp")
                  ((= char ?O) "pgponly")
                  ((= char ?X) "ext")
                  ((= char ?A) "max")
                  ((= char ?T) "test")
                  ((= char ?L) "latent")
                  ((= char ?e) "ek")
                  ((= char ?E) "ekx")
                  ((= char ?U) "esub")
                  ((= char ?I) "inflt")
                  ((= char ?N) "rhop")
                  ((= column 15)
                   (format "klen%c" char))
                  ((= char ?2)
                   (if (= column 3)
                       "remix2"
                     "repgp2"))
                  (t
                   (char-to-string char)))))

(defun mixmaster-remailer-filter (item)
  (not (or (if (and mixmaster-min-reliable
                    (< (caddr item) mixmaster-min-reliable))
               t)
           (if (and mixmaster-max-latency
                    (> (cadddr item) mixmaster-max-latency))
               t)
           nil)))

;;}}}
;;{{{ Utility functions

(defun join-list (jlist &optional sep)
  "Join a list of strings together into a single string, delimited by SEP."
  (mapconcat #'identity
             jlist (or sep " ")))

;;}}}
;;{{{ Provides

(provide 'gnus-anon)

;;; ;;}}}

;;; ;; Local variables:
;;; ;; folded-file: t
;;; ;; folding-internal-margins: nil
;;; ;; end:
;;; [3. application/pgp-signature]

;;; [[End of PGP Signed Part]]

