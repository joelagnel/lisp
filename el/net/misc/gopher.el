;;; gopher.el --- an emacs gopher client

;; Copyright (C) 1992 scott snyder

;; Author: scott snyder <snyder@fnald0.fnal.gov>
;; Created: 29 Jun 1992
;; Version: 1.03
;; Keywords: gopher

;; LCD Archive Entry:
;; gopher|scott snyder|snyder@fnald0.fnal.gov|
;; An emacs gopher client.|
;; 20-Apr-1993|1.02|~/interfaces/gopher.el.Z|

;; This file is not part of GNU Emacs, but is distributed under the
;; same conditions.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;;
;; An emacs gopher client.  Currently supports directory, text, CSO,
;; index, image, and telnet objects.
;; Requires forms.el and background.el.
;; 
;; Written by scott snyder <snyder@fnald0.fnal.gov>
;; Some code borrowed from GNUS (by Masanobu UMEDA).
;; Some code (bookmarks, xterms, error handling) contributed
;; by Stewart Clamen <clamen@cs.cmu.edu>.

;;; Commentary:
;; OPERATING INSTRUCTIONS
;;
;; To use, `M-x gopher'.  To specify a different root server, use
;; `C-u M-x gopher'.  If you want to use bookmarks, set the variable
;; gopher-support-bookmarks appropriately.
;; The command `M-x gopher-atpoint' will attempt to interpret the text
;; around point as a gopher bookmark specification and will retrieve
;; that item.
;;
;; Sample .emacs configuration:
;;  (autoload 'gopher "gopher")
;;  (autoload 'gopher-atpoint "gopher")
;;  (setq gopher-support-bookmarks t)
;;
;; In directory mode:
;;   Space, return, `f', or `e' selects the line point is on.
;;   With a numeric prefix argument, select that object.
;;   `q', `l', or `u' will return to the previous node.
;;   `n' and `p' to the next and previous lines.
;;   `a' will add an object to your bookmark list.
;;   `v' will display your bookmark list.
;;   `=' gives detailed information about an object.
;;  In the bookmark list, all of the above (except `a'), plus:
;;   `C-k' will delete an object from the bookmark list.
;;   `C-y' will yank the most recently deleted bookmark object back into
;;         the bookmark buffer.
;;   `s' will save the bookmark list.
;;   `Q' will quit gopher entirely, killing all gopher buffers.
;;
;;   All commands which operate on a specific object can take an optional
;;   numeric prefix argument giving the index of the object on which
;;   to operate.
;;
;; In document mode:
;;   Space pages forward.
;;   Delete pages backward.
;;   `q', `l' or `u' returns to the last node.
;;
;; In the CSO entry form:
;;    `C-c RET' performs a look-up, based on the field contents
;;      you've filed in.
;;    `C-c l' returns to the previous node.
;;
;; Telnets:
;;    If you have an X server set, gopher will try to create an xterm
;;    running telnet.  If not, the emacs-lisp telnet mode will be used.
;;    From the emacs-lisp telnet mode, use `C-c l' to kill the session
;;    and return to the previous node.
;;    See also the variable gopher-telnet-command.
;;
;; Images:
;;    Images are displayed using the command gopher-image-display-command.
;;    The default setting for this variable uses xv.
;;
;; Note:
;;   If gopher consistently hangs while trying to retrieve an object,
;;   try turning on gopher-buggy-accept (which see).
;;
;; VMS notes:
;;   To use this on VMS, you'll need my emacs subprocess patches (recently
;;   posted on gnu.emacs.sources; if you can't find them, send me mail).
;;   To be able to run telnet in a separate decterm, you'll also need
;;   to (setq shell-file-name "docmd") and create the file
;;   emacs_library:[etc]docmd.com containing the following:
;;     $ if p1 .eqs. "-C" then p1 = ""
;;     $ deass sys$input
;;     $ 'p1 'p2 'p3 'p4 'p5 'p6 'p7 'p8
;;     $ eoj

;;; Change Log:
;;
;; Version 1.03    21-JUL-2002
;;  * Added support for html
;;
;; Version 1.02    20-APR-1993 
;;  * Avoid using replace-regexp in gopher-clean-text.  (Suggested by
;;    sbyrnes@rice.edu (Steven Byrnes)).
;;  * Added gopher-port-aliases.
;;  * Print ports as strings in gopher-directory-show-object.
;;  * Don't (ding) when the net stream closes unexpectedly.
;;  * Added image display (based on code from beldar@MicroUnity.com
;;    (Gardner Cohen)).
;;  * Attempt to improve error reporting.
;;  * Reworked gopher-parse-bookmark to handle out-of-order fields.
;;  * Added gopher-atpoint.
;;  * Moved bookmark init stuff to gopher-read-bookmarks.
;;  * Added gopher-quit (based on code from Thomas L|fgren
;;    <tde9104@abacus.hgs.se>).
;;  * Change usage of background for lemacs.
;;    (Patch from "William M. Perry" <wmperry@raisin.ucs.indiana.edu>).
;;  * Added a (provide 'gopher) at end.
;;  * Added `f' and `e' bindings in directory mode.
;;    
;; Version 1.01
;;  * Added patch suggested by Humberto Ortiz-Zuazaga
;;    <zuazaga@ucunix.san.uc.EDU> to allow null Path= items in .gopherrc.
;;
;; Version 1.00    29-AUG-1992
;;  * Added gopher-buggy-accept.
;;  * Reworked telnet stuff to use an arbitrary command string to start up
;;    the telnet process.  This can be used to start the telnet in a
;;    separate terminal window.
;;    Based on code from Stewart Clamen.
;;  * Stewart Clamen <clamen@cs.cmu.edu> added bookmarks.
;;  * Added 's key binding to save bookmarks.
;;  * Added a prefix argument to gopher-directory-buffer and
;;    gopher-add-bookmark.
;;  * Added standard emacs-lisp header.
;;  * Stewart Clamen <clamen@cs.cmu.edu> added some error trapping and
;;    recovery (gopher-retrieve-document-cleanly).
;;  * Appended node description to the node's buffer's name.
;;  * Reformat bookmark buffers when returning to them via gopher-last-node
;;    or when an item is deleted.
;;  * Added gopher-yank-bookmark.
;;  * Added gopher-bookmark-modified-tick to prevent reformatting bookmark
;;    buffers needlessly.
;;
;; Version 0.92    27-JUL-1992
;;  * Added gopher-hostname-aliases.
;;
;; Version 0.91    30-JUN-1992
;;  * Deal with servers which send stuff after the CR.
;;  * Prevent gopher-directory-show-object from clearing the read-only flag.
;;  * Allow specification of port number in `C-u M-x gopher'.
;;
;; Version 0.9     29-JUN-1992
;;  * Initial release.

;;; Code:

(require 'electric)
(require 'forms)

;; background has the same name as an epoch function. 
;; Rename it to gopher-background...
;; also, the version i got from the archive didn't have a provide...
(cond ((and (string-lessp "19" emacs-version)
	    (not (boundp 'epoch::version)))
       ;; background is obsolete in emacs19: just add a & to shell-command.
       (defun gopher-background (command)
	 (shell-command (concat command "&"))))
      (t
       ;; background has the same name as an epoch function. 
       ;; Rename it to gopher-background...
       ;; also, the version i got from the archive didn't have a provide...
       (if (not (fboundp 'gopher-background))
	   (if (fboundp 'background)
	       (let ((old-background (symbol-function 'background)))
		 (load-library "background")
		 (fset 'gopher-background (symbol-function 'background))
		 (fset 'background old-background))
	     (load-library "background")
	     (fset 'gopher-background (symbol-function 'background))
	     ))
       ))

(defvar gopher-root-node (vector ?1 "root" "" "gopher.micro.umn.edu" 70)
  "The root gopher server, as a gopher object.")

(defvar gopher-directory-mode-hook nil
  "*Invoked when entering a new gopher directory.")
(defvar gopher-directory-mode-map (make-keymap)
  "Keymap for gopher-directory-mode.")

(defvar gopher-document-mode-hook nil
  "*Invoked when showing gopher document.")
(defvar gopher-document-mode-map (make-keymap)
  "Keymap for gopher-document-mode.")

(defvar gopher-form-mode-hooks nil
  "*Invoked with entering a gopher form (i.e., for CSO).")
(defvar gopher-form-mode-map (make-keymap)
  "Keymap for gopher-form-mode.")

(defvar gopher-tmp-buf nil
  "Buffer used to receive output from gopher.")

(defvar gopher-debug-read t
  "*If non-nil, show the current status about reading the gopher server output.")

;; On some systems (such as SGI Iris), accept-process-output doesn't seem
;; to return for the last packet received on a connection.  Turn this on
;; to work around the problem, but does anyone know what causes this?
(defvar gopher-buggy-accept nil
  "*If non-nil, use sit-for instead of accept-process-output.
If gopher consistently hangs while fetching an object, try turning this on.")

(defvar gopher-hostname-aliases
  '(("128.230.33.31" . "oliver.syr.edu"))
  "Emacs can't deal with raw IP addresses used as a hostname.
Use this to work around...")

(defvar gopher-port-aliases
  '(("whois_port" . 43))
  "Some losing hosts send a port name instead of a number.
Use this table to convert...")


(defvar gopher-support-bookmarks nil
  "*If nil, do not implement bookmarks. 
If 'unix or t, read and write bookmarks to ~/.gopherrc. 
If a filename, read and save vector from there directly (not implemented yet).
If a vector, treat as a built-in directory.")

(defconst gopher-bookmarks nil "Internal bookmark directory.")
(defconst gopher-bookmarks-modified nil "Do bookmarks need to be saved?")
(defconst gopher-killed-bookmark nil "The last bookmark object to be killed")
(defconst gopher-bookmark-directory-p nil
  "Is this buffer a bookmark directory?  A buffer-local variable.")

(defvar gopher-bookmark-modified-tick 0
  "Counts each time the bookmark vector is modified.")


(defvar gopher-telnet-command
  (cond ((eq system-type 'vax-vms)
         (if (getenv "DECW$DISPLAY")
             "create/terminal/wait/window=(title=\"telnet\") telnet"))
        (t
         (if (getenv "DISPLAY")
             "xterm -e telnet"))
        )
  "*Command to use to start a telnet session.
If this is nil, the emacs-lisp telnet package will be used.
The default setting is to create a terminal window running telnet
if you've specified an X server, and to use the emacs-lisp telnet otherwise.")


(defvar gopher-image-display-command "xv -geometry +200+200"
  "*The command used to try to display an image object.")

(defvar gopher-browse-url-function browse-url-browser-function
  "* The command used to try to display an html object.")

(defvar gopher-browser-groks-gopher t
  "* Set to true if the `gopher-browse-url-function' browser supports
gopher natively.")

(defvar gopher-object-type-alist
  '(( ?0   ""          gopher-document-object)
    ( ?1   "/"         gopher-directory-object)
    ( ?2   " <CSO>"    gopher-cso-object)
    ( ?3   " <error>"  gopher-unimplemented-object)
    ( ?4   " <binhex>" gopher-binary-object)
    ( ?5   " <DOS>"    gopher-binary-object)
    ( ?6   " <UU>"     gopher-binary-object)
    ( ?7   " <?>"      gopher-index-object)
    ( ?8   " <TEL>"    gopher-telnet-object)
    ( ?9   " <bin>"    gopher-binary-object)
    ( ?T   " <T>"      gopher-unimplemented-object)
    ( ?s   " <)"       gopher-binary-object)
    ( ?M   " <MIME>"   gopher-unimplemented-object)
    ( ?h   " <html>"   gopher-html-object)
    ( ?I   " <image>"  gopher-image-object)
    ( ?c   " <cal>"    gopher-unimplemented-object)
    ( ?g   " <GIF>"    gopher-image-object)
    ;; For floodgap.com
    ( ?i   ""          gopher-info-object)
    )
  "*Alist describing the types of gopher objects this client know about.
The keys are the gopher type characters.
The second element in each list is the string to tag onto the end
of an object's description, to identify it to the user.
The third element is the function to use to retrieve the object.
It is called with two arguments: the gopher object to retrieve and
the buffer which should be returned to when the user is done
with this object.")


;;;
;;; The data structure describing a gopher object is a vector of five elements:
;;;    [ TYPE DESCR SELECTOR HOST PORT ]
;;;
;;;  TYPE is the type character.
;;;  DESCR is the human-readable description of the object.
;;;  SELECTOR is the opaque selector to be sent to HOST to retrieve the obj.
;;;  HOST is the name of the Internet host on which the object resides.
;;;  PORT is the TCP/IP port on which the host is listening.
;;;
;;;  The following macros set and fetch elements of this structure.
;;;  

(defconst gopher-object-length 5)

(defmacro gopher-object-type (object)
  "Return the gopher type of OBJECT."
  (` (aref (, object) 0)))

(defmacro gopher-object-descr (object)
  "Return the gopher description of OBJECT."
  (` (aref (, object) 1)))

(defmacro gopher-object-selector (object)
  "Return the gopher selector string for OBJECT."
  (` (aref (, object) 2)))

(defmacro gopher-object-host (object)
  "Return the gopher hostname for OBJECT."
  (` (aref (, object) 3)))

(defmacro gopher-object-port (object)
  "Return the gopher TCP port number for OBJECT."
  (` (aref (, object) 4)))


(defmacro gopher-set-object-type (object type)
  "Set the gopher type of OBJECT to TYPE."
  (` (aset (, object) 0 (, type))))

(defmacro gopher-set-object-descr (object descr)
  "Set the gopher description of OBJECT to DESCR."
  (` (aset (, object) 1 (, descr))))

(defmacro gopher-set-object-selector (object selector)
  "Set the gopher selector string for OBJECT to SELECTOR."
  (` (aset (, object) 2 (, selector))))

(defmacro gopher-set-object-host (object host)
  "Set the gopher hostname for OBJECT to HOST."
  (` (aset (, object) 3 (, host))))

(defmacro gopher-set-object-port (object port)
  "Set the gopher TCP port number for OBJECT to PORT."
  (` (aset (, object) 4 (, port))))


(defmacro gopher-retrieve-document-cleanly (args handle &rest body)
  "Call gopher-retrieve-document with condition-case wrapped around, 
applying HANDLE if appropriate."
  (` (condition-case nil
         (progn
           (gopher-retrieve-document (,@ args))
           (,@ body))
       (error (, handle)))))


;;
;; buffer-local variables.
;; declared here to prevent warnings from the new byte-compiler.
;;

(defvar gopher-dir nil)
(defvar gopher-last nil)
(defvar gopher-obj nil)
(defvar gopher-telnet-process-name nil)
(defvar gopher-bookmark-buffer-tick nil)
(defvar forms-accept-action nil)        


;;;;--------------------------------------------------------------------------
;;;; main dispatching logic.
;;;;

(defun gopher (&optional askserv)
  "Start a gopher session.  With C-u, prompt for a gopher server."
  (interactive "P")
  (if askserv
      (progn
        (gopher-set-object-host
         gopher-root-node
         (read-string "Gopher server: "
                      (gopher-object-host gopher-root-node)))

        (let (portstr port)
          (while (not (numberp port))
            (setq portstr
                  (read-string "Port: "
                               (int-to-string
                                (gopher-object-port gopher-root-node))))

            (setq port (condition-case nil
                           (car (read-from-string portstr))
                         (error nil)))

            (if (not (numberp port))
                (progn
                  (ding)
                  (message "Port must be numeric")
                  (sit-for 1)))
            )

          (gopher-set-object-port gopher-root-node port))))

  (gopher-read-bookmarks)

  (gopher-dispatch-object gopher-root-node nil))
  


(defun gopher-atpoint nil
  "Try to interpret the text around point as a gopher bookmark, and dispatch
to that object."
  (interactive)

  (let (bkmk)
    (save-excursion
      (re-search-backward "^#[ \t]*$\\|^[ \t]*$\\|\\`")
      (skip-chars-forward " \t\n")
      (setq bkmk (gopher-parse-bookmark)))
    (if bkmk
	(progn
	  (gopher-read-bookmarks)
	  (gopher-dispatch-object bkmk nil))
      (error "Illformed bookmark"))))

  
(defun gopher-dispatch-object (obj lastbuf)
  "Dispatch a gopher object depending on its type."
  (let ((typedesc (assq (gopher-object-type obj) gopher-object-type-alist)))

    (if typedesc
        (funcall (nth 2 typedesc) obj lastbuf)
      (gopher-unimplemented-object obj lastbuf))))


(defun gopher-unimplemented-object (obj lastbuf)
  (error "unimplemented object type"))


;;;;--------------------------------------------------------------------------
;;;; utilities
;;;;

(defun gopher-next-field nil
  "Returns as a string all chars between point and the next tab or newline.
Point is advanced to after the tab (or to the end-of-line)."

  (let ((beg (point)) s)
    (skip-chars-forward "^\t\n")
    (setq s (buffer-substring beg (point)))
    (if (eq (following-char) ?\t)
        (forward-char))
    s))


;; from GNUS
(defun gopher-make-local-vars (&rest pairs)
  ;; Take VARIABLE-VALUE pairs and makes local variables initialized to the
  ;; value.
  (while pairs
    (make-local-variable (car pairs))
    (set (car pairs) (car (cdr pairs)))
    (setq pairs (cdr (cdr pairs)))))


(defun gopher-get-tmp-buf nil
  "Get a temporary buffer in which to receive gopher output."
  (or (bufferp gopher-tmp-buf)
      (progn
        (setq gopher-tmp-buf (get-buffer-create " *gopher-tmp*"))
        (buffer-flush-undo gopher-tmp-buf)))
  gopher-tmp-buf)


(defun gopher-get-dir-buf (descr)
  "Get a new buffer suitable for a gopher directory or document."
  (let ((buf (generate-new-buffer (concat "*gopher*" descr))))
    (buffer-flush-undo buf)
    buf))

(fset 'gopher-get-doc-buf (symbol-function 'gopher-get-dir-buf))


(defun gopher-trim-blanks (str)
  "Remove leading and trailing blanks from STR."
  (string-match "\\`[ \t\n]*" str)
  (substring str
             (match-end 0)
             (string-match "[ \t\n]*\\'" str (match-end 0))))


;;;;--------------------------------------------------------------------------
;;;; directory handling
;;;;


(defun gopher-directory-object (obj oldbuf)
  "Retrieve and display a gopher directory."

  (let ((tmpbuf (gopher-get-tmp-buf))
        (dirbuf (gopher-get-dir-buf (gopher-object-descr obj))))

    ;; Get the directory...
    (gopher-retrieve-document-cleanly (tmpbuf
                                       (gopher-object-selector obj)
                                       (gopher-object-host     obj)
                                       (gopher-object-port     obj))
        
        (progn
          (kill-buffer dirbuf)
          (error "Problems retrieving directory."))

      ;; Parse it and store our internal representation in gopher-dir.
      (switch-to-buffer dirbuf)
      (gopher-make-local-vars
       'gopher-dir (gopher-parse-directory tmpbuf)
       'gopher-last oldbuf)

      ;; Format it for your viewing pleasure.
      (gopher-format-directory gopher-dir dirbuf)
      (goto-char (point-min))
      (if (> (- (point-max) (point)) 7) (forward-char 7))

      ;; Turn on directory mode and put the description in the mode line.
      (gopher-directory-mode)
      (setq mode-line-buffer-identification (concat "Gopher: "
                                                    (gopher-object-descr obj)))
      )))


(defun gopher-parse-directory (buf)
  "Parse the gopher directory in buffer BUF into our internal representation.
Returns a vector of gopher objects."

  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))

    (let* ((len (count-lines (point-min) (point-max)))
           (dir (make-vector len nil))
           (i 0))

      (while (not (eobp))
        (aset dir i (gopher-parse-directory-line))
        (setq i (1+ i))
        (forward-line 1))

      dir)))


(defun gopher-parse-directory-line nil
  "Parse the line containing point as a gopher directory entry.
Returns the corresponding gopher object."

  (let (type descr selector host port)
    (beginning-of-line)
    (setq type (following-char))
    (forward-char)
    (setq descr (gopher-next-field))
    (setq selector (gopher-next-field))
    (setq host (gopher-next-field))
    (setq port (gopher-next-field))

    (if (string-match "^[0-9]+$" port)
        (setq port (string-to-int port)))

    (vector type descr selector host port)))


(defun gopher-format-directory (dir buf)
  "Print the directory vector DIR into buffer BUF."

  (save-excursion
    (set-buffer buf)
    (erase-buffer)
    (let ((i 0)
          (len (length dir)))
      (while (< i len)
        (gopher-format-directory-line (aref dir i) (1+ i))
        (setq i (1+ i)))

      )))


(defun gopher-format-directory-line (obj ndx)
  "Insert a line describing the gopher object OBJ into the current buffer.
NDX is a numeric index to display to the left of the object description."

  (let ((ndx-str (int-to-string ndx))
        (typedesc (assq (gopher-object-type obj) gopher-object-type-alist)))

    (if (and typedesc (eq ?i (car typedesc)))
	(insert "       ")
      ;; display the index number.  use 5 digits, right-justified.
      (if (< (length ndx-str) 5)
	  (insert (make-string (- 5 (length ndx-str)) ? )))
      (insert ndx-str)
      (insert ". "))

    ;; add the object description.
    (insert (gopher-object-descr obj))

    ;; add a tag indicating the gopher object type.
    (insert (if typedesc
                (nth 1 typedesc)
              (concat " ???" (char-to-string (gopher-object-type obj)))))

    (insert "\n")))


(defun gopher-directory-mode nil
  "Gopher directory mode.

\\{gopher-directory-mode-map}
"
  (use-local-map gopher-directory-mode-map)
  (setq major-mode 'gopher-directory-mode)
  (setq mode-name "gopher dir")
  (run-hooks 'gopher-directory-mode-hook)
  (setq buffer-read-only t))


;;; keymap for directory mode
(suppress-keymap gopher-directory-mode-map)
(define-key gopher-directory-mode-map "\r" 'gopher-directory-choose)
(define-key gopher-directory-mode-map " " 'gopher-directory-choose)
(define-key gopher-directory-mode-map "l"  'gopher-last-node)
(define-key gopher-directory-mode-map "q"  'gopher-last-node)
(define-key gopher-directory-mode-map "u"  'gopher-last-node)
(define-key gopher-directory-mode-map "="  'gopher-directory-show-object)
(define-key gopher-directory-mode-map "Q"  'gopher-quit)
(define-key gopher-directory-mode-map "f" 'gopher-directory-choose)
(define-key gopher-directory-mode-map "e" 'gopher-directory-choose)

; Virginia Peck <vapeck@cs>  Mon Aug 10 1992
(define-key gopher-directory-mode-map "n"  'next-line)
(define-key gopher-directory-mode-map "p"  'previous-line)
;;(define-key gopher-directory-mode-map "\C-xk"  'gopher-last-node)

; Stewart Clamen <clamen@cs.cmu.edu>  Mon Aug 17 1992
(define-key gopher-directory-mode-map "v"  'gopher-display-bookmarks)
(define-key gopher-directory-mode-map "a"  'gopher-add-bookmark)
(define-key gopher-directory-mode-map "\C-k"  'gopher-delete-bookmark)
(define-key gopher-directory-mode-map "s" 'gopher-directory-save-bookmarks)
(define-key gopher-directory-mode-map "\C-y" 'gopher-yank-bookmark)


(defun gopher-directory-nth-obj (n)
  "Returns the Nth object (starting at 1) in a gopher directory buffer."
  (if (or (<= n 0) (> n (length gopher-dir)))
      (error "Out of range."))
  (aref gopher-dir (1- n)))


(defun gopher-directory-n (arg)
  "Return the index of the object specified by ARG (starting at 1).
If ARG is nil, this is the index of the current line.
Otherwise, it is the value of ARG (as a prefix argument)."
  (if arg
      (prefix-numeric-value arg)
    (if (eq (point) (point-max))
	(1+ (count-lines (point-min) (point-max)))
      (count-lines (point-min) (1+ (point))))))


(defun gopher-directory-obj (arg)
  "Return the gopher object given by prefix arg ARG.
If it is nil, return the object given by the line point is on.
Otherwise, ARG is the index of the object."
  (gopher-directory-nth-obj (gopher-directory-n arg)))


(defun gopher-directory-choose (arg)
  "Choose an item from the directory, and do whatever is appropriate
based on the object's type.  Default is to choose the object given by the
line the cursor is on.  With numeric prefix argument N, choose object N."
  (interactive "P")
  (gopher-dispatch-object (gopher-directory-obj arg) (current-buffer)))


(defun gopher-directory-show-object (arg)
  "Dump the internal information in a gopher object.
With numeric prefix argument N, show information about the Nth object."
  (interactive "P")
  (let* ((obj (gopher-directory-obj arg))
         (type (gopher-object-type obj))
         (typespec (assq type gopher-object-type-alist))
         (typetag (if typespec (nth 1 typespec) "?"))
         (typeproc (if typespec (nth 2 typespec) "?")))
    (with-output-to-temp-buffer "*Gopher object*"
      (princ (format "Type        : %c   `%s'   %s\n" type typetag typeproc))
      (princ (format "Description : %s\n" (gopher-object-descr    obj)))
      (princ (format "Selector    : %s\n" (gopher-object-selector obj)))
      (princ (format "Host        : %s\n" (gopher-object-host     obj)))
      (princ (format "Port        : %s\n" (gopher-object-port     obj)))
      (current-buffer)
      ))
  (shrink-window-if-larger-than-buffer (get-buffer-window "*Gopher object*"))

  ;; shrink-window-if-larger-than-buffer screws these up...
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))


(defun gopher-last-node nil
  "Return to the previous gopher node.
By convention, a gopher buffer has the local variable gopher-last which
contains the buffer to which we should return."
  (interactive)
  (let ((oldbuf (current-buffer)))
    (if gopher-last
        (progn
          (switch-to-buffer gopher-last)
          (kill-buffer oldbuf)
	  (and (gopher-bookmark-directory-p)
	       (> gopher-bookmark-modified-tick gopher-bookmark-buffer-tick)
	       (let ((ppos (1- (gopher-directory-n nil))))
		 (gopher-format-bookmarks)
		 (forward-line ppos)
		 (if (> (- (point-max) (point)) 7) (forward-char 7)))))
      (if (and gopher-support-bookmarks
               gopher-bookmarks-modified
               (y-or-n-p
                "Changes have been made to the Bookmark directory.  Save? "))
          (gopher-save-bookmarks))
      (kill-buffer oldbuf))))


(defun gopher-directory-save-bookmarks ()
  "Save the bookmark list."
  (interactive)

  (if (not (gopher-bookmark-directory-p))
      (error "This isn't the bookmark directory."))

  (gopher-save-bookmarks))



;;; Gopher clean-up and quit.
;;; Originally from Thomas L|fgren <tde9104@abacus.hgs.se>

(defun gopher-quit nil
  "Quit gopher, and kill all gopher buffers.
If there are unsaved changes to your bookmark directory, you will be
asked if you want to save them"
  (interactive)
  (if (y-or-n-p "Do you really want to kill all gopher buffers? ") 
      (progn
	(if (and gopher-support-bookmarks
		 gopher-bookmarks-modified
		 (y-or-n-p
		  "Changes have been made to the Bookmark directory.  Save? "))
	    (gopher-save-bookmarks))
	(let ((buflist (buffer-list))
	      (case-fold-search t))
	  (while buflist
	    (if (eq (string-match "\\*gopher" (buffer-name (car buflist))) 0)
		(kill-buffer (car buflist)))
	    (setq buflist (cdr buflist)))))))


;;;;--------------------------------------------------------------------------
;;;; bookmarks (Implemented originally by clamen@cs.cmu.edu)
;;;;


(defun gopher-read-bookmarks ()
  (cond ((null gopher-support-bookmarks))
        ((or (equal gopher-support-bookmarks 'unix)
             (equal gopher-support-bookmarks t))
         (setq gopher-bookmarks 
               (gopher-read-unix-bookmarks)))
        ((stringp gopher-support-bookmarks)
         (gopher-read-lisp-bookmarks gopher-support-bookmarks))
        ((vectorp gopher-support-bookmarks)
         (setq gopher-bookmarks gopher-support-bookmarks))
        (t
         (message "Illformed gopher-bookmarks, assuming none"))))


(defun gopher-read-unix-bookmarks ()
  "Read bookmarks out of ~/.gopherrc file."
  (let ((rcfile "~/.gopherrc"))
    (if (file-exists-p rcfile)
        (let* ((rcbuf (find-file-noselect rcfile))
               (bkmks (gopher-parse-bookmark-buffer rcbuf)))
          (kill-buffer rcbuf)
          (setq gopher-bookmarks-modified nil)
	  (setq gopher-bookmark-modified-tick
		(1+ gopher-bookmark-modified-tick))
          bkmks)
      (message "No %s exists." rcfile)
      nil)))

(defun gopher-parse-bookmark-buffer (buf)
  "Read buffer containing bookmarks, formatted like ~.gopherrc 
in UNIX gopher client."
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (if (re-search-forward "^bookmarks:\n" (point-max) t)
        (let (bkmk bkmks)
          (while (setq bkmk (gopher-parse-bookmark))
            (setq bkmks (cons bkmk bkmks)))
          (apply 'vector (reverse bkmks))))))

(defun gopher-parse-bookmark-line (regexp end setf bkmk)
  (save-excursion
    (if (re-search-forward regexp end t)
	(eval (list setf bkmk
		    (buffer-substring (match-beginning 1) (match-end 1))))
      )))


(defun gopher-parse-bookmark ()
  "Read next bookmark.  Return a directory object."
  (if (looking-at "^#$")
      (forward-line))
  (if (not (eobp))
      (let ((end (save-excursion
		   (forward-line 5)
		   (point)))
	    (bkmk (make-vector 5 nil)))
	(prog1
	    (and (gopher-parse-bookmark-line "^Type *= *\\(.+\\) *$" end
					     'gopher-set-object-type bkmk)
		 (gopher-parse-bookmark-line "^Name *= *\\(.*\\) *$" end
					     'gopher-set-object-descr bkmk)
		 (gopher-parse-bookmark-line "^Path *= *\\(.*\\) *$" end
					     'gopher-set-object-selector bkmk)
		 (gopher-parse-bookmark-line "^Host *= *\\(.+\\) *$" end
					     'gopher-set-object-host bkmk)
		 (gopher-parse-bookmark-line "^Port *= *\\(.+\\) *$" end
					     'gopher-set-object-port bkmk)
		 (progn
		   (gopher-set-object-type
		    bkmk (string-to-char (gopher-object-type bkmk)))
		   (gopher-set-object-port
		    bkmk (string-to-int (gopher-object-port bkmk)))
		   bkmk))
	  (goto-char end))
	)))

(defun gopher-format-bookmarks ()
  "Make the current buffer (which is assumed to be a bookmark buffer)
contain an up-to-date listing of the bookmark list."

  (let ((buffer-read-only nil))
    (erase-buffer)
    (setq gopher-dir gopher-bookmarks)

    ;; Format it for your viewing pleasure.
    (gopher-format-directory gopher-dir (current-buffer))
    (goto-char (point-min))
    (if (> (- (point-max) (point)) 7) (forward-char 7))
    (setq gopher-bookmark-buffer-tick gopher-bookmark-modified-tick)))

(defun gopher-display-bookmarks ()
  "Retrieve and display the gopher bookmark directory."
  (interactive)

  (if (> (length gopher-bookmarks) 0)
      (let ((oldbuf (current-buffer))
            (dirbuf (gopher-get-dir-buf "*Gopher Bookmarks*")))

        ;; Store our internal representation in gopher-dir.
        (switch-to-buffer dirbuf)
        (gopher-make-local-vars
         'gopher-dir gopher-bookmarks
	 'gopher-bookmark-directory-p t
	 'gopher-bookmark-buffer-tick gopher-bookmark-modified-tick
         'gopher-last oldbuf)

	(gopher-format-bookmarks)

        ;; Turn on directory mode and put the description in the mode line.
        (gopher-directory-mode)
        (setq mode-line-buffer-identification (concat "Gopher: *Bookmarks*"))
        )
    (error "No bookmarks supported.")))


(defun gopher-save-bookmarks ()
  "Save bookmarks."
  (cond 
   ((or (equal gopher-support-bookmarks 'unix)
        (equal gopher-support-bookmarks t))
    (gopher-save-unix-bookmarks))
   ((stringp gopher-support-bookmarks)
    (gopher-save-lisp-bookmarks gopher-support-bookmarks))
   (t
    (message "Illformed gopher-support-bookmarks, assuming none")))

  (setq gopher-bookmarks-modified nil))


(defun gopher-save-unix-bookmarks ()
  "Save bookmarks out to ~/.gopherrc file."
  (save-excursion
    (let* ((rcfile "~/.gopherrc")
           (new-file-p (not (file-exists-p rcfile)))
           (rcbuf (find-file-noselect rcfile)))
      (set-buffer rcbuf)
      (if new-file-p
          (insert "bookmarks:\n")
        (goto-char (point-min))
        (if (re-search-forward "^bookmarks:\n" nil t)
            (delete-region (point) (point-max))
          (goto-char (point-max))
          (insert "bookmarks:\n")))

      ;; Now, insert defined bookmarks into file
  
      (let ((obj-count 0))
        (while (< obj-count (length gopher-bookmarks))
          (let ((obj (aref gopher-bookmarks obj-count)))
            (insert "#"
                    "\nType=" (gopher-object-type obj)
                    "\nName=" (gopher-object-descr obj)
                    "\nPath=" (gopher-object-selector obj)
                    "\nHost=" (gopher-object-host obj)
                    "\nPort=" (int-to-string (gopher-object-port obj))
                    "\n")
            (setq obj-count (1+ obj-count)))))

      (write-file rcfile))))


(defun gopher-add-bookmark (arg)
  "Add current object to menu of bookmarks.
With numeric prefix argument N, add Nth object."
  (interactive "P")
  (if (gopher-bookmark-directory-p)
      (error "That item is already a bookmark!")
    (let ((existing-bookmarks gopher-bookmarks)
          (new-bookmarks (make-vector (1+ (length gopher-bookmarks)) nil))
          (obj (copy-sequence (gopher-directory-obj arg)))
          (l (length gopher-bookmarks)))
      (gopher-set-object-descr
       obj
       (read-from-minibuffer "Node Name: "
                             (gopher-object-descr obj)))
      (aset new-bookmarks l obj)
      (while (> l 0)
        (progn (setq l (1- l))
               (aset new-bookmarks l (aref existing-bookmarks l))))
      (setq gopher-bookmarks new-bookmarks
            gopher-bookmarks-modified t
	    gopher-bookmark-modified-tick (1+ gopher-bookmark-modified-tick))
      )))


(defun gopher-delete-bookmark (arg)
  "Delete current bookmark.
With numeric prefix argument N, delete Nth bookmark."
  (interactive "P")
  (if (not (gopher-bookmark-directory-p))
      (error "Can only delete object in Bookmark directory.")
    (let ((new-bookmarks (make-vector (1- (length gopher-bookmarks)) nil))
          (pos (1- (gopher-directory-n arg)))
          (l (length gopher-bookmarks))
          (i 0))
      (while (< i pos)
        (progn (aset new-bookmarks i (aref gopher-bookmarks i))
               (setq i (1+ i))))
      (while (< i (1- l))
        (progn (aset new-bookmarks i (aref gopher-bookmarks (1+ i)))
               (setq i (1+ i))))
      (setq gopher-killed-bookmark (aref gopher-bookmarks pos)
	    gopher-bookmarks new-bookmarks
            gopher-dir new-bookmarks
            gopher-bookmarks-modified t
	    gopher-bookmark-modified-tick (1+ gopher-bookmark-modified-tick))
      (let ((ppos (1- (gopher-directory-n nil))))
	(if (< pos ppos)
	    (setq ppos (1- ppos)))
	(gopher-format-bookmarks)
	(goto-char (point-min))
	(forward-line ppos)
	(forward-char 7)))
    (if (= (point) (point-max)) (previous-line 1))
;    (let ((buffer-read-only nil))
;      (beginning-of-line 1)
;      (kill-line 1)
;      (if (= (point) (point-max)) (previous-line 1)))
    (if (zerop (length gopher-bookmarks))
        (gopher-last-node))))


(defun gopher-yank-bookmark (arg)
  "Yank the most recently killed bookmark at the current position.
With numeric prefix argument N, yank into position N."
  (interactive "P")
  (cond ((not (gopher-bookmark-directory-p))
	 (error "Can only yank bookmark objects into bookmark directory."))
	((null gopher-killed-bookmark)
	 (error "No killed bookmark object"))
	(t
	 (let* ((len (length gopher-bookmarks))
		(new-bookmarks (make-vector (1+ len) nil))
		(pos (1- (gopher-directory-n arg)))
		i)

	   (if (or (< pos 0) (> pos (length gopher-bookmarks)))
	       (error "Out of range."))

	   (setq i (1- pos))
	   (while (>= i 0)
	     (aset new-bookmarks i (aref gopher-bookmarks i))
	     (setq i (1- i)))

	   (aset new-bookmarks pos gopher-killed-bookmark)

	   (setq i pos)
	   (while (< i len)
	     (aset new-bookmarks (1+ i) (aref gopher-bookmarks i))
	     (setq i (1+ i)))

	   (setq gopher-bookmarks new-bookmarks
		 gopher-bookmarks-modified t
		 gopher-killed-bookmark nil
		 gopher-bookmark-modified-tick
		   (1+ gopher-bookmark-modified-tick))

	   (let ((ppos (1- (gopher-directory-n nil))))
	     (if (<= pos ppos)
		 (setq ppos (1+ ppos)))
	     (gopher-format-bookmarks)
	     (goto-char (point-min))
	     (forward-line ppos)
	     (forward-char 7))
	   ))))
	   

(defun gopher-bookmark-directory-p ()
  "Return T if currently displaying Bookmark directory."
  gopher-bookmark-directory-p)
;  (equal gopher-dir gopher-bookmarks))


(defun gopher-read-lisp-bookmarks (fn)
  "currently unsupported"
  (error "gopher-read-lisp-bookmark is not yet supported.  Sorry."))

(defun gopher-save-lisp-bookmarks (fn)
  "currently unsupported"
  (error "gopher-save-lisp-bookmark is not yet supported.  Sorry."))



;;;;--------------------------------------------------------------------------
;;;; gopher documents
;;;;


(defun gopher-document-object (obj oldbuf &optional end-regexp)
  "Retrieve and display a gopher document.
Optional argument END-REGEXP is used if the data will not be ended by `.'."

  (let ((docbuf (gopher-get-doc-buf (gopher-object-descr obj))))

    ;; Snarf the data into the buffer.
    (gopher-retrieve-document-cleanly (docbuf
                                       (gopher-object-selector obj)
                                       (gopher-object-host     obj)
                                       (gopher-object-port     obj)
                                       end-regexp)

        (progn
          (kill-buffer docbuf)
          (error "Problems retrieving document."))
      
      ;; Turn on document mode and put the description in the mode line.
      (switch-to-buffer docbuf)
      (gopher-make-local-vars
       'gopher-last oldbuf)
      (goto-char (point-min))
      (gopher-document-mode)
      (setq mode-line-buffer-identification (concat "Gopher: "
                                                    (gopher-object-descr obj)))
      )))


;; keymap for document mode
(suppress-keymap gopher-document-mode-map)

;Virginia Peck <vapeck@cs>  Mon Aug 10 21:44:35 1992
;;(define-key gopher-document-mode-map "\C-xk"  'gopher-last-node)

(define-key gopher-document-mode-map "l"  'gopher-last-node)
(define-key gopher-document-mode-map "q"  'gopher-last-node)
(define-key gopher-document-mode-map "u"  'gopher-last-node)
(define-key gopher-document-mode-map " "  'scroll-up)
(define-key gopher-document-mode-map "\C-?"  'scroll-down)
(define-key gopher-document-mode-map "\r"  'gopher-scroll-one-line-up)


(defun gopher-document-mode nil
  "Gopher document mode.

\\{gopher-document-mode-map}
"
  (use-local-map gopher-document-mode-map)
  (setq major-mode 'gopher-document-mode)
  (setq mode-name "gopher doc")
  (run-hooks 'gopher-document-mode-hook)
  (setq buffer-read-only t))


;; from gosmacs.el
(defun gopher-scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))


;;;;--------------------------------------------------------------------------
;;;; CSO handling.
;;;;
;;;; uses a subset of forms mode to handle data entry.
;;;;

(defun gopher-cso-object (obj oldbuf)
  "Display a CSO lookup form."

  ;; The following will create a buffer displaying the form described
  ;; by the list in the last argument (cf. forms-mode).  When the user
  ;; accepts the data in the form (by pressing `C-c RET'), the function
  ;; gopher-do-cso will be called with the data the user supplied.
  (gopher-form (gopher-object-descr obj)
               'gopher-do-cso
               4
               '("====== phone directory lookup ======"
                 "\n Press `C-c RET' to lookup, `C-c l' to return to the last gopher object."
                 "\n (you must fill in at least one of the first three fields)"
                 "\n"
                 "Name    : "   1
                 "\n"
                 "Phone   : "   2
                 "\n"
                 "E-Mail  : "   3
                 "\n"
                 "Address : "   4
                 ))

  ;; Record gopher-last so gopher-last-node knows where to go.
  ;; Record gopher-obj so gopher-do-cso knows what server to query.
  (gopher-make-local-vars
   'gopher-last oldbuf
   'gopher-obj  obj))


(defconst gopher-cso-fields '("name" "phone" "email" "address")
  "Field names to use in CSO queries.")

(defun gopher-do-cso (vals)
  "Make a CSO query.  VALS is the data the user entered in the form,
as a list of strings."

  ;; Check that the required data was provided.
  (if (zerop (+ (length (nth 0 vals))
                (length (nth 1 vals))
                (length (nth 2 vals))))
      (error "Must specify name, phone, or email."))

  (let ((query "query")
        (fields gopher-cso-fields)
        (obj gopher-obj))

    ;; Form the query string
    (while vals

      (if (not (zerop (length (car vals))))
          (setq query (concat query " " (car fields) "=" (car vals))))

      (setq vals (cdr vals))
      (setq fields (cdr fields)))

    ;; Use this string as the object selector.
    (gopher-set-object-selector gopher-obj query)

    ;; Retrieve the data from the server.  Unlike gopher, the CSO data
    ;; does not use `.' as a terminator.
    (gopher-document-object gopher-obj (current-buffer) "^[2-9]")

    ;; Strip CSO control information from the buffer.
    (gopher-clean-cso-buffer obj)))


(defun gopher-clean-cso-buffer (obj)
  "Strip CSO control information from the current buffer."

  (let ((req "")
        (buffer-read-only nil)
        beg nreq)
    (goto-char (point-min))
    (insert "\n")
    (while (not (eobp))
      (cond ((and (>= (following-char) ?3) (<= (following-char) ?9))
             (delete-char 4)
             (insert (concat (gopher-object-selector obj) "\n")))
            
            ((eq (following-char) ?-)
             (delete-char 5)
             (setq beg (point))
             (skip-chars-forward "^:")
             (setq nreq (buffer-substring beg (point)))
             (goto-char beg)
             (or (string= req nreq)
                 (insert (concat "--------------------------"
                                 "-----------------------------\n")))
             (setq req nreq)
             (setq beg (point))
             (skip-chars-forward "^:")
             (forward-char)
             (delete-region beg (point)))

            (t
             (setq beg (point))
             (forward-line 1)
             (delete-region beg (point))
             (forward-line -1))
            )
      (forward-line 1))

    (goto-char (point-min))
    (delete-char 1)))


;;;;--------------------------------------------------------------------------
;;;; indices.
;;;;
;;;; To query an index, the search string is appended to the selector.
;;;; The index returns a gopher directory.
;;;;


(defun gopher-index-object (obj oldbuf)
  "Query a gopher directory object."

  ;; Get the search string from the user.
  (let ((str (read-from-minibuffer "Key: "))
        (newobj (copy-sequence obj)))

    ;; Append it to the selector and retrieve the modified object
    ;; like a directory.
    (setq str (gopher-trim-blanks str))
    (if (> (length str) 0)
        (progn
          (gopher-set-object-selector newobj
                                      (concat (gopher-object-selector obj) "\t"
                                              str))
          (gopher-directory-object newobj (current-buffer)))
      )))



;;;;--------------------------------------------------------------------------
;;;; telneting.
;;;;

(defun gopher-telnet-object (obj oldbuf)
  "Start a telnet session to a gopher object.
If gopher-telnet-command is nonnil, then that is a command to start
a telnet session in a subprocess.  Otherwise, the emacs-lisp telnet
package is used."
  
  ;; make the telnet argument string
  (let ((arg (gopher-object-host obj))
        (port (gopher-object-port obj)))
    (if (not (zerop port))
        (setq arg (concat arg 
                          (if (eq system-type 'vax-vms)
                              "/port="
                            " ")
                          port)))

    (if gopher-telnet-command

        ;; start up telnet as a separate process
        (save-window-excursion
          (gopher-background
           (concat gopher-telnet-command " " arg)))

      ;; use telnet-mode
      (telnet arg)
      ;; set things up so we can get back to the last node.
      (gopher-make-local-vars
       'gopher-last oldbuf
       'gopher-telnet-process-name (concat arg "-telnet"))
      (local-set-key "\C-cl" 'gopher-telnet-quit)
      (local-set-key "\C-xk" 'gopher-telnet-quit)
      )

    ;; show the login info to the user
    (if (not (zerop (length (gopher-object-selector obj))))
        (progn
          (beep)
          (message (concat 
                    "Login as: "
                    (gopher-object-selector obj)
                    ))
          ))
    ))


(defun gopher-telnet-quit nil
  "Clean up a telnet session and return to the previous gopher node."
  (interactive)
  (condition-case nil
      (delete-process gopher-telnet-process-name)
    (error t))
  (gopher-last-node))



;;;;--------------------------------------------------------------------------
;;;; Images/sounds.
;;;;

(defun gopher-image-object (obj oldbuf)
  "Retrieve what we hope is an image and show it."
  (let (
 	(showit (y-or-n-p "Display this item? "))
 	(fname)
 	(buf (gopher-get-doc-buf (gopher-object-descr obj))))
    (if showit
 	(setq fname (make-temp-name "/tmp/gopherimg"))
      (setq fname(read-file-name "File to save in: ")))
    (gopher-retrieve-document-cleanly (buf
				       (gopher-object-selector obj)
				       (gopher-object-host     obj)
				       (gopher-object-port     obj)
				       'none)
	(progn
	  (error "Problems retrieving object.")
	  (kill-buffer buf))
 
      (save-excursion
	(set-buffer buf)
	(write-file fname))
      (kill-buffer buf)
      (if (and showit gopher-image-display-command)

	  ;; Spawn a process to display the image.
	  ;; But modify its sentinel so that the file we wrote
	  ;; will get deleted when the process exits.
	  (save-window-excursion
	    (let ((p (gopher-background
		      (concat gopher-image-display-command " " fname))))
	      (set-process-sentinel p
		    (` (lambda (process msg) 
			 ((, (process-sentinel p)) process msg)
			 (if (not (eq (process-status process) 'run))
			     (delete-file (, fname)))
			 )))
	      ))
	))))

 

;;;;--------------------------------------------------------------------------
;;;; Various opaque objects.  Just save them in a file for now.
;;;;

(defun gopher-binary-object (obj oldbuf)
  "Retrieve a gopher object and save it to a file,
without trying to interpret it in any way."
  (let ((fname (read-file-name "File to save in: "))
        (buf (gopher-get-doc-buf (gopher-object-descr obj))))

    (gopher-retrieve-document-cleanly (buf
                                       (gopher-object-selector obj)
                                       (gopher-object-host     obj)
                                       (gopher-object-port     obj)
                                       'none)

        (progn
          (error "Problems retrieving object.")
          (kill-buffer buf))

      (save-excursion
        (set-buffer buf)
        (write-file fname))
      (kill-buffer buf)
      )))


;;;;--------------------------------------------------------------------------
;;;; HTML object
;;;;

(defun gopher-html-object (obj oldbuf)
  "Retrieve what we hope is an html file and show it."
  (if gopher-browser-groks-gopher
      ;; Send url directly to browser
      (let ((url (format "gopher://%s:%d/%c%s"
			 (gopher-object-host     obj)
			 (gopher-object-port     obj)
			 (gopher-object-type     obj) ;; should be ?h
			 (gopher-object-selector obj))))
	(apply gopher-browse-url-function url nil))

    ;; Save in temp file and send temp file to browser
    (let ((fname (concat (make-temp-name "/tmp/gopherimg") ".html"))
	  (buf (gopher-get-doc-buf (gopher-object-descr obj))))

      (gopher-retrieve-document-cleanly (buf
					 (gopher-object-selector obj)
					 (gopher-object-host     obj)
					 (gopher-object-port     obj)
					 'none)
	  (progn
	    (error "Problems retrieving object.")
	    (kill-buffer buf))

	(save-excursion
	  (set-buffer buf)
	  (write-file fname))
	(kill-buffer buf)

	(when (functionp gopher-browse-url-function)
	  (apply gopher-browse-url-function fname nil)
	  ;; SAM delete temp file here?
	  )
	))))


;;;;--------------------------------------------------------------------------
;;;; Informational object
;;;;

(defun gopher-info-object (obj oldbuf)
  "Informational only. You cannot select it."
  (ding))


;;;;--------------------------------------------------------------------------
;;;; forms stuff
;;;;
;;;; Uses some of the internal routines from forms.el to present
;;;; a form which is not associated with a file.
;;;;

(defun gopher-form (form-name accept-action number-of-fields format-list)
  "Display a buffer containing a form for the user to enter data.
The form is described by NUMBER-OF-FIELDS and FORMAT-LIST (cf. forms-mode).
FORM-NAME is a string to put in the modeline.
When the user accepts the data in the form by pressing `C-c RET', the
function ACCEPT-ACTION is called with a list of the strings which
the user entered."

  (switch-to-buffer (generate-new-buffer "*gopher form*"))

  (gopher-make-local-vars
   'forms-format-list   format-list
   'forms-number-of-fields number-of-fields
   'forms-field-sep        "\t"
   'forms-read-only        nil
   'forms-multi-line       nil
   'forms--number-of-markers nil
   'forms--markers           nil
   'forms--format            nil
   'forms--parser            nil
   'forms--dynamic-text      nil
   'forms-fields             nil
   'forms-the-record-list    nil
   'forms-accept-action      accept-action
   )

  (forms--process-format-list)
  (forms--make-format)
  (forms--make-parser)

  (erase-buffer)

  ;; make local variables
  (make-local-variable 'forms--file-buffer)
  (make-local-variable 'forms--total-records)
  (make-local-variable 'forms--current-record)
  (make-local-variable 'forms--the-record-list)
  (make-local-variable 'forms--search-rexexp)

  ;; set the major mode indicator
  (setq major-mode 'gopher-form-mode)
  (setq mode-name "gopher form")

  (set-buffer-modified-p nil)

  (use-local-map gopher-form-mode-map)

  (forms--show-record (make-string (1- number-of-fields) ?\t))

  (run-hooks 'gopher-form-mode-hooks))


(defun gopher-form-accept nil
  (interactive)

  (funcall forms-accept-action (forms--parse-form)))

(define-key gopher-form-mode-map "\C-c\r"  'gopher-form-accept)
(define-key gopher-form-mode-map "\C-cl"   'gopher-last-node)


;;;;--------------------------------------------------------------------------
;;;; low-level communications routines
;;;;


(defun gopher-retrieve-document (buf sel host port &optional end-regexp)
  "Retrieve a gopher object into BUF.
The object is identified by a SEL HOST PORT triple.
Optional argument END-REGEXP is used for data which is not `.'-terminated.
If END-REGEXP is non-nil and not a string, then it is assumed that
the data is binary, and reading will continue until the sender disconnects.
Returns NIL if an error occured during the attempt to retrieve the
document, otherwise T.
"

  ;; Default is single period termination.
  (or end-regexp (setq end-regexp "^\\.\r$"))

  (save-excursion
    (set-buffer buf)
    (erase-buffer)

    (let ((h (assoc host gopher-hostname-aliases)))
      (if h (setq host (cdr h))))

    ;; Open the connection to the server.
    ;; If we get an unknown service error, try looking the port up in
    ;; gopher-port-aliases.  If we find it there, try the connect again
    ;; with that translation.
    (let (wait
	  (gopher-server-process
	   (let (p (try-again t))
	     (while try-again
	       (setq try-again nil)
	       (condition-case errinfo
		   (setq p (open-network-stream "gopher" (current-buffer)
						host port))
		 (error (if (and (string-match "^Unknown service .*$"
					       (nth 1 errinfo))
				 (setq port (cdr (assoc port
							gopher-port-aliases))))
			    (setq try-again t)
			  (ding)
			  (message (format "%s: %s"
					   (nth 0 errinfo)
					   (nth 1 errinfo)))
			  ))))
	     p)))

      (cond (gopher-server-process
             
             ;; keep the emacs end-of-process status line out of the buffer
             (set-process-sentinel gopher-server-process 'gopher-sentinel)

             ;; send the selector to the server
             (process-send-string gopher-server-process (concat sel "\r\n"))

             ;; receive the response from the server
             ;; based on nntp.el from GNUS
             (setq wait t)
             (while wait
               (if (stringp end-regexp)
                   (progn
                     (goto-char (point-max))
                     (forward-line -1)))
               (if (and (stringp end-regexp)
                        (looking-at end-regexp))
                   (setq wait nil)
                 (if (not (memq (process-status gopher-server-process)
				'(open run)))
                     (progn
                       (message "gopher: connection closed")
                       (setq wait nil))
                   (if gopher-debug-read
                       (message "gopher: Reading..."))
                   (cond (gopher-buggy-accept
                          (sit-for 1))
                         ((and (boundp 'epoch::version) epoch::version)
                          (accept-process-output gopher-server-process 2))
                         (t
                          (accept-process-output gopher-server-process))
                         )
                   (if gopher-debug-read
                       (message " ")))
                 ))

             ;; be sure the net connection has gone away...
             (condition-case nil
                 (delete-process gopher-server-process)
               (error t))

             ;; clean up the text buffer
             (if (stringp end-regexp)
                 (gopher-clean-text))

             t)

            (t nil))
      )))


;;; adapted from GNUS
(defun gopher-clean-text ()
  "Decode text transmitted by gopher.
0. Delete status line.
1. Delete `^M' at end of line.
2. Delete `.' at end of buffer (end of text mark).
3. Delete `.' at beginning of line.   (does gopher want this?)"

  ;; Insert newline at end of buffer.
  (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  ;; Delete `^M' at end of line.
  (goto-char (point-min))
  (while (re-search-forward "\r[^\n]*$" nil t)
    (replace-match ""))
;  (goto-char (point-min))
;  (while (not (eobp))
;    (end-of-line)
;    (if (= (preceding-char) ?\r)
;       (delete-char -1))
;    (forward-line 1)
;    )
  ;; Delete `.' at end of buffer (end of text mark).
  (goto-char (point-max))
  (forward-line -1)                     ;(beginning-of-line)
  (while (looking-at "^\\.$")
    (delete-region (point) (progn (forward-line 1) (point)))
    (forward-line -1))
  ;; Replace `..' at beginning of line with `.'.
  (goto-char (point-min))
  ;; (replace-regexp "^\\.\\." ".")
  (while (search-forward "\n.." nil t)
    (delete-char -1))
  )


(defun gopher-sentinel (proc status)
  nil)

(provide 'gopher)

;;; gopher.el ends here

;;;(gopher.el) Local Variables:
;;;(gopher.el) eval: (put 'gopher-retrieve-document-cleanly 'lisp-indent-hook 2)
;;;(gopher.el) End:

