;;; $Id: todo.el,v 1.13 1998/03/18 15:54:16 queinnec Exp $
;;; Copyright (C) 1996 by C.Queinnec (Universite Paris 6 & INRIA-Rocquencourt)

;;; LCD Archive Entry:
;;; fold|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; A minor mode for TODO files|
;;; $Date: 1998/03/18 15:54:16 $|$Revision: 1.13 $|
;;; ~/misc/todo|

;;; This file is not part of GNU Emacs.

;;; {{{ Documentation

;;; A TODO file is a text file that contains text, mails, shell or
;;; EmacsLisp sentences.  All of them have to be handled, sent,
;;; executed soon or later. This package analyzes such a file and
;;; proposes to perform the relevant actions. I edit my TODO file when
;;; diconnected at home with the myriad of things I should do when
;;; connected at my office.  Of course, this mode will be obsolete
;;; when every place of the earth will be continuously connected!

;;; A TODO file has several parts wrapped between lines consisting of
;;; a single repeated character (a line of colons or stars or whatever
;;; (but repeated at least five times)). The text that follows a part
;;; separator tells the type of the part.  A mail starts with "To:", a
;;; shell script starts with "#!" (and may, as usual on the rest of
;;; the line, tell which shell to use), an EmacsLisp Sexpression
;;; starts with an open parenthesis, etc.

;;; To take benefit of the TODO mode, compile this file, add the following
;;; line to your .emacs and visit a TODO file.
;;;     (setq auto-mode-alist
;;;           (cons (cons "[Tt][Oo][Dd][Oo]$" 'todo-mode) 
;;;                 auto-mode-alist ) )
;;;     (autoload 'todo-mode "todo")

;;; This package is known to work with Emacs 19.34.

;;; {{{ Repository
;;; Bugs, remarks etc should be sent to
;;;     Christian.Queinnec@inria.fr
;;; Newer versions will be sent to the LCD Archive but may appear earlier on:
;;;     ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/todo.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     ftp://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html
;;; }}}

;;; }}}

;;; {{{ Global variables

(defvar todo-mode nil
  "This buffer specific variable tells if the TODO mode is active." )
(make-variable-buffer-local 'todo-mode)

(defvar todo-keymap (make-sparse-keymap "ToDo")
  "This is the minor keymap of the TODO mode." )

(defvar todo-mode-hook nil
  "Hooks run after installing TODO mode." )

(defvar todo-face (and window-system 'highlight)
  "Face to highlight parts that are handled." )

(defvar todo-overlays nil
  "The overlays used by TODO mode." )

(defvar todo-buffers nil
  "The buffers used by TODO mode." )

(defvar todo-emitmail-hook nil
  "The hooks that are run after a message is prepared by TODO." )

(defvar todo-separator 
  "^\\([^ \t]\\)\\1\\1\\1+ *\n"
  "Regexp to separate two independent parts in a TODO file.
By default, it corresponds to a line of a single character repeated at 
least five times. Trailing spaces are ignored. " )

(defvar todo-actions
  (list (cons "^\\(To\\|Subject\\|Cc\\):" 
              'todo-emitmail-analyze)
        (cons "^#! *\\(.*\\)"             
              'todo-execute-shell-script)
        (cons "^\\(\\(http\\|ftp\\|mailto\\)://.*\\) *"
              'todo-load-url)
        (cons "[ \t\n]*("                         
              'todo-evaluate-lisp)
        )
  "List of (REGEXP . FUNCTION) to identify what to do with a part.
The buffer is narrowed to the identified part then FUNCTION is invoked. " )

;;; }}}
;;; {{{ Code

(defun todo-mode ()
  "Enter/Exit TODO mode.
A TODO file is separated into a variety of parts whose beginning tells
the nature (mail, shell, EmacsLisp...). A separator is any line composed 
with a single character repeated at least five times. A mail part starts 
with \"To:\", a shell script starts with \"#!\", an URL starts with http,
an EmacsLisp Sexpression starts with an opening parenthesis. This scheme 
may of course be extended (see the todo-actions variable).

The main command is \\[todo-handle] to handle a part ie send a mail,
execute a shell script, an EmacsLisp Sexpression, etc. 

\\[todo-clean] removes all internal resources needed by TODO (buffers, 
overlays) and removes as well the parts already handled. "
  (interactive)
  (setq todo-mode (not todo-mode))
  (if todo-mode
      (progn 
        (make-variable-buffer-local 'minor-mode-map-alist)
        (or (assq 'todo minor-mode-map-alist)
            (setq minor-mode-map-alist
                  (cons (cons 'todo-mode todo-keymap)
                        minor-mode-map-alist ) ) )
        (make-variable-buffer-local 'minor-mode-alist)
        (or (assq 'todo-mode minor-mode-alist)
            (setq minor-mode-alist
                  (cons (list 'todo-mode " ToDo")
                        minor-mode-alist ) ) )
        (force-mode-line-update)
        (define-key todo-keymap "\M-\C-x"  'todo-handle)
        (define-key todo-keymap "\C-c\C-l" 'todo-clean)
        (run-hooks 'todo-mode-hook) )
    ;; Exit from TODO mode
    (todo-clean nil) ) )

(defun todo-clean (removep)
  "Free all resources (overlays, buffers) needed by TODO mode. 
Remove also handled parts if REMOVEP is true (which is the default). "
  (interactive (list t))
  (while (consp todo-overlays)
    (let ((ov (car todo-overlays)))
      (if removep (delete-region (overlay-start ov) (overlay-end ov)))
      (delete-overlay ov) )
    (setq todo-overlays (cdr todo-overlays)) )
  (while (consp todo-buffers)
    (kill-buffer (car todo-buffers))
    (setq todo-buffers (cdr todo-buffers)) ) )

(defun todo-handle ()
  "Identify the current part and handle it according to its type. "
  (interactive)
  (if (not (search-backward-regexp todo-separator (point-min) t))
      (message "Cannot find the head of the current TODO part")
    (let ((start (match-end 0)))
      (goto-char start)
      (if (not (search-forward-regexp todo-separator (point-max) t))
          (message "Cannot find the tail of the current TODO part")
        (let ((end (match-beginning 0)))
          (save-excursion
            (todo-hilite start end)
            (todo-handle-part start end) ) ) ) ) ) )

(defun todo-hilite (start end)
  "Highlight a part specified as [START...END]. This allows to know the
parts that you already attempt to handle. It does not ensure that the
part was successfully handled. 
You may remove already handled parts with \\[todo-clean]. "
  (let ((ov (make-overlay start end)))
    (setq todo-overlays (cons ov todo-overlays))
    (overlay-put ov 'face todo-face) ) )

(defun todo-handle-part (start end)
  "Narrow the buffer to the part [START...END], identify its nature then
invoke the right action on that part (ie send a mail, execute a shell 
script, ...). Mark the part as handled when done. 
You may remove already handled parts with \\[todo-clean]. "
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (let ((actions todo-actions)
          (fun nil) )
      (while (consp actions)
        (if (looking-at (car (car actions)))
            (progn 
              (setq fun (cdr (car actions)))
              (setq actions nil) )
          (setq actions (cdr actions)) ) )
      (if fun (funcall fun)
        (message "No possible action on that TODO part") ) ) ) )

;;; }}}
;;; {{{ Shell

;;; A shell expression starts with #! and may mention the name of the
;;; shell to start. By default, this may be ESHELL, SHELL or /bin/sh.

(defun todo-execute-shell-script ()
  "This function evaluates the script in the narrowed buffer. 
The name of the program to run is already matched. "
  (let* ((program (buffer-substring (match-beginning 1) (match-end 1)))
         (script-start (match-end 0))
         (name    "TODO:shell")
         (buffer  (get-buffer-create (concat "*" name "*"))) )
    (setq todo-buffers (cons buffer todo-buffers))
    (if (equal program "")
        (setq program (or (and (boundp 'explicit-shell-file-name)
                               explicit-shell-file-name )
                          (getenv "ESHELL")
                          (getenv "SHELL")
                          "/bin/sh" )) )
    (shell-command-on-region script-start (point-max) program buffer nil)
    (switch-to-buffer-other-window buffer)
    (rename-uniquely) ) )

;;; }}}
;;; {{{ Mail

;;; Extract mails from a file and prepare them to be sent.  The mail
;;; should start with To: or Subject: The body of the mail is
;;; separated from its headers by at least one blank line. The body
;;; may contains #include or #send lines.

(defun todo-emitmail-analyze ()
  "Analyze the buffer (narrowed to the mail) and prepare a message.
Identify To, Subject and other mail fields. Handle the body to #include 
files or #send files. "
  (let (to cc subject body)
    (setq body (todo-emitmail-extract-body))
    ;; point is left at the beginning of the body.
    (save-restriction
      ;; mail-fetch-field requires narrrowing to mail header.
      (narrow-to-region (point-min) (point))
      (setq to (mail-fetch-field "To"))
      (setq cc (mail-fetch-field "cc"))
      (setq subject (mail-fetch-field "Subject")) )
    (todo-emitmail-prepare to cc subject body) ) )

(defun todo-emitmail-prepare (to cc subject body)
  "Prepare a mail to be ready to be sent. "
  (let ((bufnam " *TODO:EmitMail* "))
    (with-output-to-temp-buffer bufnam
      (switch-to-buffer bufnam)
      (rename-uniquely)
      (erase-buffer)
      (buffer-enable-undo)
      (mail-mode)
      (mail-setup to subject nil cc nil nil)
      (save-excursion
        (insert body) )
      (save-excursion
        (todo-handle-includes)
        (todo-handle-sends) )
      (run-hooks 'todo-emitmail-hook) ) ) )

(defun todo-emitmail-extract-body ()
  "Extract the body of the message. 
It is supposed to start after the first blank line that follow the To, 
Cc and Subject lines. Leave point to the beginning of the body. "
  (goto-char (point-min))
  (if (search-forward-regexp "^\\([ \t]*\\)$" (point-max) nil)
      (buffer-substring (point) (point-max))
    (message "No content for that mail!") ) )

(defun todo-handle-includes ()
  "Expand #include directives inside the body of a mail."
  (while (search-forward-regexp "^#include[ \t]+.*$" (point-max) t)
    (let ((line (buffer-substring (match-beginning 0) (match-end 0)))
          filename )
      (beginning-of-line)
      (if (looking-at "^#include[ \t]+\"\\(.+\\)\"[ \t]*$")
          'ok
        (if (looking-at "^#include[ \t]+\\(.+\\)[ \t]*$")
            'ok
          (error "Cannot process %s" line) ) )
      (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "" t t)
      (insert-file-contents filename) ) ) )

(defun todo-handle-sends ()
  "Expand #send directives inside the body of a mail."
  (while (search-forward-regexp "^#send[ \t]+.*$" (point-max) t)
    (let ((line (buffer-substring (match-beginning 0) (match-end 0)))
          filename )
      (beginning-of-line)
      (if (looking-at "^#send[ \t]+\"\\(.+\\)\"[ \t]*$")
          'ok
        (if (looking-at "^#send[ \t]+\\(.+\\)[ \t]*$")
            'ok
          (error "Cannot process %s" line) ) )
      (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
      (replace-match "" t t)
      (require 'fileinmail)
      (fim-mail-send-encoded-file filename) ) ) )

;;; }}}
;;; {{{ EmacsLisp

;;; An EmacsLisp part starts with an opening parenthesis.

(defun todo-evaluate-lisp ()
  "Evaluate some EmacsLisp expressions. "
  (require 'eval-reg)
  (elisp-eval-region (point-min) (point-max) t) )

;;; }}}
;;; {{{ URL

;;; An URL part contains an unique URL. This relies on the presence of 
;;; the browse-url package.

(defun todo-load-url ()
  "Transmit an URL to an http client."
  (require 'browse-url)
  (browse-url-at-point) )
  
;;; }}}

(provide 'todo)

;;; end of todo.el
