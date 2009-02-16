;;; gpg.el --- Interface to GNU Privacy Guard

;; Copyright (C) 2000 RUS-CERT, University Of Stuttgart

;; Author: Florian Weimer <Florian.Weimer@RUS.Uni-Stuttgart.DE>
;; Maintainer: Florian Weimer <Florian.Weimer@RUS.Uni-Stuttgart.DE>
;; Keywords: crypto
;; Created: 2000-04-15

;; This file is NOT (yet?) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA
;; ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA
;;
;; This code is not well-tested.  BE CAREFUL!
;; 
;; ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA
;; ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA ALPHA

;; Implemented features which can be tested:
;;
;; * Customization for all flavors of PGP is possible.
;; * The main operations (verify, decrypt, sign, encrypt, sign &
;;   encrypt) are implemented.
;; * Optionally, Gero Treuner's gpg-2comp script is supported, 
;;   to generate data which is compatible with PGP 2.6.3.

;; Customizing external programs 
;; =============================

;; The customization are very similar to those of others programs,
;; only the C-ish "%" constructs have been replaced by more Lisp-like
;; syntax.
;;
;; First, you have to adjust the default executable paths
;; (`gpg-command-default-alist', customization group `gpg-options',
;; "Controlling GnuPG invocation.").  After that, you should
;; change the configuration options which control how specific
;; command line flags are built (`gpg-command-flag-sign-with-key',
;; (`gpg-command-flag-recipient').  The elements of these lists are
;; concatenated without spaces, and a new argument is only started
;; where indicated.  The `gpg-command-flag-recipient' list is special:
;; it consists of two parts, the first one remains at the beginning
;; of the argument, the second one is repeated for each recipient.
;; Finally, `gpg-command-passphrase-env' has to be changed if there's
;; no command line flag to force the external program to read the data
;; from standard input before the message.
;;
;; In customization group `gpg-commands', "Controlling GnuPG
;; invocation.", you have to supply the actual syntax for external
;; program calls.  Each variable consists of a pair of a program
;; specification (if a Lisp symbol is given here, it is translated
;; via `gpg-command-default-alist') and a list of program arguments
;; with placeholders.  Please read the documentation of each variable
;; before making your adjustments and try to match the given
;; requirements as closely as possible!
;;
;; The `gpg-commands-key' group, "GnuPG Key Management Commands.",
;; specifies key management commands.  The syntax of these variables
;; is like those in the `gpg-commands' group.  Note that the output
;; format of some of these external programs has to match very close
;; that of GnuPG.  Additional tools (Thomas Roessler's "pgpring.c")
;; are available if your favorite implementation of OpenPGP cannot
;; output the this format.

;; Security considerations 
;; =======================

;; On a typical multiuser UNIX system, the memory image of the
;; Emacs process is not locked, therefore it can be swapped to disk
;; at any time.  As a result, the passphrase might show up in the
;; swap space (even if you don't use the passphrase cache, i.e. if
;; `gpg-passphrase-timeout' is 0).  If someone is able to run `gdb' or
;; another debugger on your Emacs process, he might be able to recover
;; the passphrase as well.  Unfortunately, nothing can be done in
;; order to prevent this at the moment.
;;
;; BE CAREFUL: If you use the passphrase cache feature, the passphrase
;; is stored in the variable `gpg-passphrase' -- and it is NOT
;; encrypted in any way.  (This is a conceptual problem because the
;; nature of the passphrase cache requires that Emacs is able to
;; decrypt automatically, so only a very weak protection could be
;; applied anyway.)
;;
;; In addition, if you use an unpatched Emacs 20 (and earlier
;; versions), passwords show up in the output of the `view-lossage'
;; function (bound to `C-h l' by default).


;;; Code:

(if (featurep 'xemacs)
    (require 'timer-funcs)
  (require 'timer))
(eval-when-compile (require 'cl))

(eval-and-compile 
  (defalias 'gpg-point-at-eol
    (if (fboundp 'point-at-eol)
	'point-at-eol
      'line-end-position)))

;; itimer/timer compatibility
(eval-and-compile
  (if (featurep 'xemacs)
      (progn
	(defalias 'gpg-cancel-timer 'delete-itimer)
	(defalias 'gpg-timer-activate 'activate-itimer)
	(defalias 'gpg-timer-create 'make-itimer)
	(defalias 'gpg-timer-set-function 'set-itimer-function)
	(defalias 'gpg-timer-set-time 'set-itimer-value))
    (defalias 'gpg-cancel-timer 'cancel-timer)
    (defalias 'gpg-timer-activate 'timer-activate)
    (defalias 'gpg-timer-create 'timer-create)
    (defalias 'gpg-timer-set-function 'timer-set-function)
    (defalias 'gpg-timer-set-time 'timer-set-time)))

;;;; Customization:

;;; Customization: Groups:

(defgroup gpg nil
  "GNU Privacy Guard interface."
  :tag "GnuPG"
  :group 'processes)

(defgroup gpg-options nil
  "Controlling GnuPG invocation."
  :tag "GnuPG Options"
  :group 'gpg)

(defgroup gpg-commands nil
  "Primary GnuPG Operations."
  :tag "GnuPG Commands"
  :group 'gpg)

(defgroup gpg-commands-key nil
  "Commands for GnuPG key management."
  :tag "GnuPG Key Commands"
  :group 'gpg-commands)

;;; Customization: Widgets:

(if (get 'alist 'widget-type)
    (define-widget 'gpg-command-alist 'alist
      "An association list for GnuPG command names."
      :key-type '(symbol :tag   "Abbreviation")
      :value-type '(string :tag "Program name")
      :convert-widget 'widget-alist-convert-widget
      :tag "Alist")
    (define-widget 'gpg-command-alist 'repeat
      "An association list for GnuPG command names."
      :args '((cons :format "%v"
		    (symbol :tag   "Abbreviation")
		    (string :tag "Program name")))
      :tag "Alist"))

(define-widget 'gpg-command-program 'choice
  "Widget for entering the name of a program (mostly the GnuPG binary)."
  :tag "Program"
  :args '((const :tag "Default GnuPG program."
		 :value gpg)
	  (const :tag "GnuPG compatibility wrapper."
		 :value gpg-2comp)
	  (const :tag "Disabled"
		 :value nil)
	  (string :tag "Custom program" :format "%v")))

(define-widget 'gpg-command-sign-options 'cons
  "Widget for entering signing options."
  :args '(gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert armor option here if necessary."
		   :value armor)
	    (const :tag "Insert text mode option here if necessary."
		   :value textmode)
	    (const :tag "Insert the sign with key option here if necessary."
		   :value sign-with-key)
	    (string :format "%v")))))

(define-widget 'gpg-command-key-options 'cons
  "Widget for entering key command options."
  :args '(gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert key ID here." 
		   :value key-id)
	    (string :format "%v")))))

;;; Customization: Variables:

;;; Customization: Variables: Paths and Flags:

(defcustom gpg-passphrase-timeout
  0
  "Timeout (in seconds) for the passphrase cache.
The passphrase cache is cleared after is hasn't been used for this
many seconds.  The values 0 means that the passphrase is not cached at
all."
  :tag "Passphrase Timeout"
  :type 'number
  :group 'gpg-options)

(defcustom gpg-default-key-id
  nil
  "Default key/user ID used for signatures."
  :tag "Default Key ID"
  :type '(choice
	  (const :tag "Use GnuPG default." :value nil)
	  (string))
  :group 'gpg-options)

(defcustom gpg-temp-directory 
  (expand-file-name "~/tmp")
  "Directory for temporary files.
If you are running Emacs 20, this directory must have mode 0700."
  :tag "Temp directory"
  :type 'string
  :group 'gpg-options)

(defcustom gpg-command-default-alist 
  '((gpg . "gpg")
    (gpg-2comp . "gpg"))
  "Default paths for some GnuPG-related programs.
Modify this variable if you have to change the paths to the
executables required by the GnuPG interface.  You can enter \"gpg-2comp\"
for `gpg-2comp' if you have obtained this script, in order to gain
PGP 2.6.x compatibility."
  :tag "GnuPG programs"
  :type 'gpg-command-alist
  :group 'gpg-options)

(defcustom gpg-command-all-arglist
  nil
  "List of arguments to add to all GPG commands."
  :tag "All command args"
  :group 'gpg-options)

(defcustom gpg-command-flag-textmode "--textmode"
  "The flag to indicate canonical text mode to GnuPG."
  :tag "Text mode flag"
  :type 'string
  :group 'gpg-options)

(defcustom gpg-command-flag-armor "--armor"
  "The flag to request ASCII-armoring output from GnuPG."
  :tag "Armor flag"
  :type 'string
  :group 'gpg-options)

(defcustom gpg-command-flag-sign-with-key '("--local-user=" sign-with-key)
  "String to include to specify the signing key ID.
The elements are concatenated (without spaces) to form a command line
option."
  :tag "Sign with key flag"
  :type '(repeat :tag "Argument parts"
	  (choice :format "%[Type%] %v"
	   (const :tag "Start next argument." :value next-argument)
	   (const :tag "Insert signing key ID here." :value sign-with-key)
	   (string)))
  :group 'gpg-options)

(defcustom gpg-command-flag-recipient
  '(nil . ("-r" next-argument recipient next-argument))
  "Format of a recipient specification.
The elements are concatenated (without spaces) to form a command line
option.  The second part is repeated for each recipient."
  :tag "Recipients Flag"
  :type '(cons
	  (repeat :tag "Common prefix"
	   (choice :format "%[Type%] %v"
	    (const :tag "Start next argument." :value next-argument)
	    (string)))
	  (repeat :tag "For each recipient"
	   (choice :format "%[Type%] %v"
	    (const :tag "Start next argument." :value next-argument)
	    (const :tag "Insert recipient key ID here." :value recipient)
	    (string))))
  :group 'gpg-options)

(defcustom gpg-command-passphrase-env
  nil
  "Environment variable to set when a passphrase is required, or nil.
If an operation is invoked which requires a passphrase, this
environment variable is set before calling the external program to
indicate that it should read the passphrase from standard input."
  :tag "Passphrase environment"
  :type '(choice
	  (const :tag "Disabled" :value nil)
	  (cons
	   (string :tag "Variable")
	   (string :tag "Value")))
  :group 'gpg-options)

;;; Customization: Variables: GnuPG Commands:

(defcustom gpg-command-verify
  '(gpg . ("--status-fd" "1" "--batch" "--verbose" "--verify" signature-file message-file))
  "Command to verify a detached signature.
The invoked program has to read the signed message and the signature
from the given files.  It should write human-readable information to
standard output and/or standard error.  The program shall not convert
charsets or line endings; the input data shall be treated as binary."
  :tag "Verify Command"
  :type '(cons 
	  gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert name of file containing the message here." 
		   :value message-file)
	    (const :tag "Insert name of file containing the signature here."
		   :value signature-file)
	    (string :format "%v"))))
  :group 'gpg-commands)

(defcustom gpg-command-verify-cleartext
  '(gpg . ("--status-fd" "1" "--batch" "--verbose" "--verify" message-file))
  "Command to verify a message.
The invoked program has to read the signed message from the given
file.  It should write human-readable information to standard output
and/or standard error.  The program shall not convert charsets or line
endings; the input data shall be treated as binary."
  :tag "Cleartext Verify Command"
  :type '(cons 
	  gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert name of file containing the message here." 
		   :value message-file)
	    (string :format "%v"))))
  :group 'gpg-commands)

(defcustom gpg-command-decrypt
  '(gpg . ("--status-fd" "2" "--decrypt" "--batch" "--passphrase-fd=0"))
  "Command to decrypt a message.
The invoked program has to read the passphrase from standard
input, followed by the encrypted message.  It writes the decrypted
message to standard output, and human-readable diagnostic messages to
standard error."
  :tag "Decrypt Command"
  :type '(cons
	  gpg-command-program
	  (repeat
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert name of file containing the message here." 
		   :value message-file)
	    (string :format "%v"))))
  :group 'gpg-commands)

(defcustom gpg-command-sign-cleartext
  '(gpg-2comp . ("--batch" "--passphrase-fd=0" "--output=-"
		 armor textmode  "--clearsign"
		 sign-with-key))
  "Command to create a \"clearsign\" text file.  
The invoked program has to read the passphrase from standard input,
followed by the message to sign.  It should write the ASCII-amored
signed text message to standard output, and diagnostic messages to
standard error."
  :tag "Clearsign Command"
  :type 'gpg-command-sign-options
  :group 'gpg-commands)

(defcustom gpg-command-sign-detached
  '(gpg-2comp . ("--batch" "--passphrase-fd=0" "--output=-"
		 armor textmode "--detach-sign" 
		 sign-with-key))
  "Command to create a detached signature. 
The invoked program has to read the passphrase from standard input,
followed by the message to sign.  It should write the ASCII-amored
detached signature to standard output, and diagnostic messages to
standard error.  The program shall not convert charsets or line
endings; the input data shall be treated as binary."
  :tag "Sign Detached Command"
  :type 'gpg-command-sign-options
  :group 'gpg-commands)

(defcustom gpg-command-sign-encrypt
  '(gpg-2comp . ("--batch" "--passphrase-fd=0" "--output=-"
		 armor textmode  "--always-trust" sign-with-key recipients
		  "--sign" "--encrypt" plaintext-file))
  "Command to sign and encrypt a file.
The invoked program has to read the passphrase from standard input,
followed by the message to sign and encrypt if there is no
`plaintext-file' placeholder.  It should write the ASCII-amored
encrypted message to standard output, and diagnostic messages to
standard error."
  :tag "Sign And Encrypt Command"
  :type '(cons 
	  gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert the `sign with key' option here if necessary."
		   :value sign-with-key)
	    (const :tag "Insert list of recipients here."
		   :value recipients)
	    (const :tag "Insert here name of file with plaintext."
		   :value plaintext-file)
	    (string :format "%v"))))
  :group 'gpg-commands)

(defcustom gpg-command-encrypt
  '(gpg-2comp . ("--batch" "--output=-" armor textmode "--always-trust" 
		 "--encrypt" recipients plaintext-file))
  "Command to encrypt a file.  
The invoked program has to read the message to encrypt from standard
input or from the plaintext file (if the `plaintext-file' placeholder
is present).  It should write the ASCII-amored encrypted message to
standard output, and diagnostic messages to standard error."
  :type '(cons 
	  gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert list of recipients here."
		   :value recipients)
	    (const :tag "Insert here name of file with plaintext."
		   :value plaintext-file)
	    (string :format "%v"))))
  :group 'gpg-commands)

;;; Customization: Variables: Key Management Commands:

(defcustom gpg-command-key-import
  '(gpg . ("--import" "--verbose" message-file))
  "Command to import a public key from a file."
  :tag "Import Command"
  :type '(cons 
	  gpg-command-program
	  (repeat 
	   :tag "Arguments"
	   (choice 
	    :format "%[Type%] %v"
	    (const :tag "Insert name of file containing the key here." 
		   :value message-file)
	    (string :format "%v"))))
  :group 'gpg-commands-key)

(defcustom gpg-command-key-export
  '(gpg . ("--no-verbose" "--armor" "--export" key-id))
  "Command to export a public key from the key ring.
The key should be written to standard output using ASCII armor."
  :tag "Export Command"
  :type 'gpg-command-key-options
  :group 'gpg-commands-key)

(defcustom gpg-command-key-verify
  '(gpg . ("--no-verbose" "--batch" "--fingerprint" "--check-sigs" key-id))
  "Command to verify a public key."
  :tag "Verification Command"
  :type 'gpg-command-key-options
  :group 'gpg-commands-key)

(defcustom gpg-command-key-public-ring
  '(gpg . ("--no-verbose" "--batch" "--with-colons" "--list-keys" key-id))
  "Command to list the contents of the public key ring."
  :tag "List Public Key Ring Command"
  :type 'gpg-command-key-options
  :group 'gpg-commands-key)

(defcustom gpg-command-key-secret-ring
  '(gpg . ("--no-verbose" "--batch" "--with-colons" 
	   "--list-secret-keys" key-id))
  "Command to list the contents of the secret key ring."
  :tag "List Secret Key Ring Command"
  :type 'gpg-command-key-options
  :group 'gpg-commands-key)

(defcustom gpg-command-key-retrieve 
  '(gpg . ("--batch" "--recv-keys" key-id))
  "Command to retrieve public keys."
  :tag "Retrieve Keys Command"
  :type 'gpg-command-key-options
  :group 'gpg-commands-key)


;;;; Helper functions for GnuPG invocation:

;;; Build the GnuPG command line:

(defun gpg-build-argument (template substitutions &optional pass-start)
  "Build command line argument(s) by substituting placeholders.
TEMPLATE is a list of strings and symbols.  The placeholder symbols in
it are replaced by SUBSTITUTIONS, the elements between
`next-argument' symbols are concatenated without spaces and are
returned in a list.

SUBSTITIONS is a list of (SYMBOL . SEXP) pairs, where SEXP is either
a string (which is inserted literally), a list of strings (which are
inserted as well), or nil, which means to insert nothing.

If PASS-START is t, `next-argument' is also inserted into the result,
and symbols without a proper substitution are retained in the output,
otherwise, an untranslated symbol results in an error.

This function does not handle empty arguments reliably."
  (let ((current-arg "")
	(arglist nil))
    (while template
      (let* ((templ (pop template))
	     (repl (assoc templ substitutions))
	     (new (if repl (cdr repl) templ)))
	(cond
	 ((eq templ 'next-argument)
	  ;; If the current argument is not empty, start a new one.
	  (unless (equal current-arg "")
	    (setq arglist (nconc arglist 
				 (if pass-start
				     (list current-arg 'next-argument)
				   (list current-arg))))
	    (setq current-arg "")))
	 ((null new) nil)		; Drop it.
	 ((and (not (stringp templ)) (null repl))
	  ;; Retain an untranslated symbol in the output if
	  ;; `pass-start' is true.
	  (unless pass-start
	    (error "No replacement for `%s'" templ))
	  (setq arglist (nconc arglist (list current-arg templ)))
	  (setq current-arg ""))
	 (t
	  (unless (listp new)
	    (setq new (list new)))
	  (setq current-arg (concat current-arg 
				    (apply 'concat new)))))))
    (unless (equal current-arg "")
      (setq arglist (nconc arglist (list current-arg))))
    arglist))

(defun gpg-build-arg-list (template substitutions)
  "Build command line by substituting placeholders.
TEMPLATE is a list of strings and symbols.  The placeholder symbols in
it are replaced by SUBSTITUTIONS.

SUBSTITIONS is a list of (SYMBOL . SEXP) pairs, where SEXP is either a
string (which is inserted literally), a list of strings (which are
inserted as well), or nil, which means to insert nothing."
  (let ((arglist (copy-sequence gpg-command-all-arglist)))
    (while template
      (let* ((templ (pop template))
	     (repl (assoc templ substitutions))
	     (new (if repl (cdr repl) templ)))
	(cond
	 ((and (symbolp templ) (null repl))
	  (error "No replacement for `%s'" templ))
	 ((null new) nil)		; Drop it.
	 (t
	  (unless (listp new)
	    (setq new (list new)))
	  (setq arglist (nconc arglist new))))))
    arglist))

(defun gpg-build-flag-recipients-one (recipient)
  "Build argument for one RECIPIENT."
  (gpg-build-argument (cdr gpg-command-flag-recipient)
		      `((recipient . ,recipient)) t))

(defun gpg-build-flag-recipients (recipients)
  "Build list of RECIPIENTS using `gpg-command-flag-recipient'."
  (gpg-build-argument
   (apply 'append (car gpg-command-flag-recipient)
	          (mapcar 'gpg-build-flag-recipients-one
			  recipients))
   nil))

(defun gpg-read-recipients ()
  "Query the user for several recipients."
  (let ((go t) 
	recipients r)
    (while go
      (setq r (read-string "Enter recipient ID [RET when no more]: "))
      (if (equal r "")
	  (setq go nil)
	(setq recipients (nconc recipients (list r)))))
    recipients))
    
(defun gpg-build-flag-sign-with-key (key)
  "Build sign with key flag using `gpg-command-flag-sign-with-key'."
  (let ((k (if key key 
	     (if gpg-default-key-id gpg-default-key-id
	       nil))))
    (if k
	(gpg-build-argument gpg-command-flag-sign-with-key
			    (list (cons 'sign-with-key k)))
      nil)))

(defmacro gpg-with-passphrase-env (&rest body)
  "Adjust the process environment and evaluate BODY.
During the evaluation of the body forms, the process environment is
adjust according to `gpg-command-passphrase-env'."
  (let ((env-value (make-symbol "env-value")))
    `(let ((,env-value))
       (unwind-protect
	   (progn
	     (when gpg-command-passphrase-env
	       (setq ,env-value (getenv (car gpg-command-passphrase-env)))
	       (setenv (car gpg-command-passphrase-env) 
		       (cdr gpg-command-passphrase-env)))
	     ,@body)
	 (when gpg-command-passphrase-env
	   ;; This will clear the variable if it wasn't set before.
	   (setenv (car gpg-command-passphrase-env) ,env-value))))))
(put 'gpg-with-passphrase-env 'lisp-indent-function 0)
(put 'gpg-with-passphrase-env 'edebug-form-spec '(body))

;;; Temporary files:

(defun gpg-make-temp-file ()
  "Create a temporary file in a safe way"
  (let ((name  ;; User may use "~/"
	 (expand-file-name "gnupg" gpg-temp-directory)))
    (if (fboundp 'make-temp-file)
	;; If we've got make-temp-file, we are on the save side.
	(make-temp-file name)
      ;; make-temp-name doesn't create the file, and an ordinary
      ;; write-file operation is prone to nasty symlink attacks if the
      ;; temporary file resides in a world-writable directory.
      (unless (or (memq system-type '(windows-nt cygwin32 win32 w32 mswindows))
		  (eq (file-modes gpg-temp-directory) 448)) ; mode 0700
	(error "Directory for temporary files (%s) must have mode 0700" gpg-temp-directory))
      (setq name (make-temp-name name))
      (let ((mode (default-file-modes)))
	(unwind-protect
	    (progn
	      (set-default-file-modes 384) ; mode 0600
	      (with-temp-file name))
	  (set-default-file-modes mode)))
      name)))

(defvar gpg-temp-files nil
  "List of temporary files used by the GnuPG interface.
Do not set this variable.  Call `gpg-with-temp-files' if you need
temporary files.")

(defun gpg-with-temp-files-create (count)
  "Do not call this function.  Used internally by `gpg-with-temp-files'."
  (while (> count 0)
    (setq gpg-temp-files (cons (gpg-make-temp-file) gpg-temp-files))
    (setq count (1- count))))

(defun gpg-with-temp-files-delete ()
  "Do not call this function.  Used internally by `gpg-with-temp-files'."
  (while gpg-temp-files
    (let ((file (pop gpg-temp-files)))
      (condition-case nil
	  (delete-file file)
	(error nil)))))

(defmacro gpg-with-temp-files (count &rest body)
  "Create COUNT temporary files, USE them, and delete them.
The function USE is called with the names of all temporary files as
arguments."
  `(let ((gpg-temp-files))
      (unwind-protect
	  (progn
	    ;; Create the temporary files.
	    (gpg-with-temp-files-create ,count)
	    ,@body)
	(gpg-with-temp-files-delete))))
(put 'gpg-with-temp-files 'lisp-indent-function 1)
(put 'gpg-with-temp-files 'edebug-form-spec '(body))

;;;  Making subprocesses:

(defun gpg-exec-path (option)
  "Return the program name for OPTION.
OPTION is of the form (PROGRAM . ARGLIST).  This functions returns
PROGRAM, but takes default values into account."
  (let* ((prg (car option))
	 (path (assq prg gpg-command-default-alist)))
    (cond
     (path (if (null (cdr path))
	       (error "Command `%s' is not available" prg)
	     (cdr path)))
     ((null prg) (error "Command is disabled"))
     (t prg))))

(defun gpg-call-process (cmd args stdin stdout stderr &optional passphrase)
  "Invoke external program CMD with ARGS on buffer STDIN.
Standard output is insert before point in STDOUT, standard error in
STDERR.  If PASSPHRASE is given, send it before STDIN.  PASSPHRASE
should not end with a line feed (\"\\n\").

If `stdin-file' is present in ARGS, it is replaced by the name of a
temporary file.  Before invoking CMD, the contents of STDIN is written
to this file."
  (gpg-with-temp-files 2
   (let* ((coding-system-for-read 'no-conversion)
	  (coding-system-for-write 'no-conversion)
	  (have-stdin-file (memq 'stdin-file args))
	  (stdin-file (nth 0 gpg-temp-files))
	  (stderr-file (nth 1 gpg-temp-files))
	  (cpr-args `(,cmd 
		      nil		; don't delete
		      (,stdout ,stderr-file)
		      nil		; don't display
		      ;; Replace `stdin-file'.
		      ,@(gpg-build-arg-list 
			  args (list (cons 'stdin-file stdin-file)))))
	  res)
     (when have-stdin-file
       (with-temp-file stdin-file
	 (buffer-disable-undo)
	 (insert-buffer-substring stdin)))
     (setq res
	   (if passphrase
	       (with-temp-buffer
		 (buffer-disable-undo)
		 (insert passphrase "\n")
		 (unless have-stdin-file
		   (apply 'insert-buffer-substring 
			  (if (listp stdin) stdin (list stdin))))
		 (apply 'call-process-region (point-min) (point-max) cpr-args)
		 ;; Wipe out passphrase.
		 (goto-char (point-min))
		 (translate-region (point) (gpg-point-at-eol)
				   (make-string 256 ? )))
	     (if (listp stdin)
		 (with-current-buffer (car stdin)
		   (apply 'call-process-region 
			  (cadr stdin)
			  (if have-stdin-file (cadr stdin) (caddr stdin))
			  cpr-args))
	       (with-current-buffer stdin
		 (apply 'call-process-region 
			(point-min) 
			(if have-stdin-file (point-min) (point-max))
			cpr-args)))))
     (with-current-buffer stderr
       (insert-file-contents-literally stderr-file))
     (if (or (stringp res) (> res 0))
	 ;; Signal or abnormal exit.
	 (with-current-buffer stderr
	   (goto-char (point-max))
	   (insert (format "\nCommand exit status: %s\n" res))
	   nil)
       t))))

(defvar gpg-result-buffer nil
  "The result of a GnuPG operation is stored in this buffer.
Never set this variable directly, use `gpg-show-result' instead.")

(defun gpg-show-result-buffer (always-show result)
  "Called by `gpg-show-results' to actually show the buffer."
  (with-current-buffer gpg-result-buffer
    ;; Only proceed if the buffer is non-empty.
    (when (and (/= (point-min) (point-max))
	       (or always-show (not result)))
      (save-window-excursion
	(display-buffer (current-buffer))
	(unless (y-or-n-p "Continue? ")
	  (error "GnuPG operation aborted"))))))

(defmacro gpg-show-result (always-show &rest body)
  "Show GnuPG result to user for confirmation.
This macro binds `gpg-result-buffer' to a temporary buffer and
evaluates BODY, like `progn'.  If BODY evaluates to `nil' (or
`always-show' is not nil), the user is asked for confirmation."
  `(let ((gpg-result-buffer (get-buffer-create 
			 (generate-new-buffer-name "*GnuPG Output*"))))
     (unwind-protect
	 (gpg-show-result-buffer ,always-show (progn ,@body))
       (kill-buffer gpg-result-buffer))))
(put 'gpg-show-result 'lisp-indent-function 1)
(put 'gpg-show-result 'edebug-form-spec '(body))

;;; Passphrase handling:

(defvar gpg-passphrase-timer
  (gpg-timer-create)
  "This timer will clear the passphrase cache periodically.")

(defvar gpg-passphrase
  nil
  "The (unencrypted) passphrase cache.")

(defun gpg-passphrase-clear-string (str)
  "Erases STR by overwriting all characters."
  (let ((pos 0)
	(len (length str)))
    (while (< pos len)
      (aset str pos ? )
      (incf pos))))

;;;###autoload
(defun gpg-passphrase-forget ()
  "Forget stored passphrase."
  (interactive)
  (when gpg-passphrase
    (gpg-cancel-timer gpg-passphrase-timer)
    (setq gpg-passphrase-timer nil)
    (gpg-passphrase-clear-string gpg-passphrase)
    (setq gpg-passphrase nil)))

(defun gpg-passphrase-store (passphrase)
  "Store PASSPHRASE in cache.
Updates the timeout for clearing the cache to `gpg-passphrase-timeout'."
  (unless (equal gpg-passphrase-timeout 0)
    (if (null gpg-passphrase-timer)
	(setq gpg-passphrase-timer (gpg-timer-create)))
    (gpg-timer-set-time gpg-passphrase-timer 
			(timer-relative-time (current-time) 
					     gpg-passphrase-timeout))
    (gpg-timer-set-function gpg-passphrase-timer 'gpg-passphrase-forget)
    (unless (and (fboundp 'itimer-live-p)
		 (itimer-live-p gpg-passphrase-timer))
      (gpg-timer-activate gpg-passphrase-timer))
    (setq gpg-passphrase passphrase))
  passphrase)

(defun gpg-passphrase-read ()
  "Read a passphrase and remember it for some time."
  (interactive)
  (if gpg-passphrase
      ;; This reinitializes the timer.
      (gpg-passphrase-store gpg-passphrase)
    (let ((pp (read-passwd "Enter passphrase: ")))
      (gpg-passphrase-store pp))))


;;;; Main operations:

;;;###autoload
(defun gpg-verify (message signature result)
  "Verify buffer MESSAGE against detached SIGNATURE buffer.
Returns t if everything worked out well, nil otherwise.  Consult
buffer RESULT for details."
  (interactive "bBuffer containing message: \nbBuffer containing signature: \nbBuffor for result: ")
  (gpg-with-temp-files 2
    (let* ((sig-file    (nth 0 gpg-temp-files))
	   (msg-file    (nth 1 gpg-temp-files))
	   (cmd (gpg-exec-path gpg-command-verify))
	   (args (gpg-build-arg-list (cdr gpg-command-verify)
				     `((signature-file . ,sig-file)
				       (message-file . ,msg-file))))
	   res)
      (with-temp-file sig-file 
	(buffer-disable-undo)
	(apply 'insert-buffer-substring (if (listp signature)
					    signature
					  (list signature))))
      (with-temp-file msg-file 
	(buffer-disable-undo)
	(apply 'insert-buffer-substring (if (listp message)
					    message
					  (list message))))
      (setq res (apply 'call-process-region 
		       (point-min) (point-min) ; no data
		       cmd
		       nil		; don't delete
		       result
		       nil		; don't display
		       args))
      (if (or (stringp res) (> res 0))
	  ;; Signal or abnormal exit.
	  (with-current-buffer result
	    (insert (format "\nCommand exit status: %s\n" res))
	    nil)
	t))))

;;;###autoload
(defun gpg-verify-cleartext (message result)
  "Verify message in buffer MESSAGE.
Returns t if everything worked out well, nil otherwise.  Consult
buffer RESULT for details.

NOTE: Use of this function is deprecated."
  (interactive "bBuffer containing message: \nbBuffor for result: ")
  (gpg-with-temp-files 1
    (let* ((msg-file    (nth 0 gpg-temp-files))
	   (cmd (gpg-exec-path gpg-command-verify-cleartext))
	   (args (gpg-build-arg-list (cdr gpg-command-verify-cleartext)
				     `((message-file . ,msg-file))))
	   res)
      (with-temp-file msg-file 
	(buffer-disable-undo)
	(apply 'insert-buffer-substring (if (listp message)
					    message
					  (list message))))
      (setq res (apply 'call-process-region
		       (point-min) (point-min) ; no data
		       cmd
		       nil		; don't delete
		       result
		       nil		; don't display
		       args))
      (if (or (stringp res) (> res 0))
	  ;; Signal or abnormal exit.
	  (with-current-buffer result
	    (insert (format "\nCommand exit status: %s\n" res))
	    nil)
	t))))

;;;###autoload
(defun gpg-decrypt (ciphertext plaintext result &optional passphrase)
  "Decrypt buffer CIPHERTEXT to buffer PLAINTEXT.
Returns t if everything worked out well, nil otherwise.  Consult
buffer RESULT for details.  Reads a missing PASSPHRASE using
`gpg-passphrase-read'."
  (interactive "bBuffer containing ciphertext: \nbBuffer for plaintext: \nbBuffor for decryption status: ")
  (gpg-call-process (gpg-exec-path gpg-command-decrypt)
		    (gpg-build-arg-list (cdr gpg-command-decrypt) nil)
		    ciphertext plaintext result
		    (if passphrase passphrase (gpg-passphrase-read)))
  (when passphrase
    (gpg-passphrase-clear-string passphrase)))

;;;###autoload
(defun gpg-sign-cleartext
  (plaintext signed-text result &optional passphrase sign-with-key)
  "Sign buffer PLAINTEXT, and store PLAINTEXT with signature in
SIGNED-TEXT.
Reads a missing PASSPHRASE using `gpg-passphrase-read'.  Uses key ID
SIGN-WITH-KEY if given, otherwise the default key ID.  Returns t if
everything worked out well, nil otherwise.  Consult buffer RESULT for
details.

NOTE: Use of this function is deprecated."
  (interactive "bBuffer containing plaintext: \nbBuffer for text with signature: \nbBuffer for status information: ")
  (let ((subst (list (cons 'sign-with-key 
			   (gpg-build-flag-sign-with-key sign-with-key))
		     (cons 'armor gpg-command-flag-armor)
		     (cons 'textmode gpg-command-flag-textmode))))
    (gpg-call-process (gpg-exec-path gpg-command-sign-cleartext)
		      (gpg-build-arg-list (cdr gpg-command-sign-cleartext) 
					  subst)
		      plaintext signed-text result
		      (if passphrase passphrase (gpg-passphrase-read))))
  (when passphrase
    (gpg-passphrase-clear-string passphrase)))

;;;###autoload
(defun gpg-sign-detached
  (plaintext signature result &optional passphrase sign-with-key
   armor textmode)
  "Sign buffer PLAINTEXT, and store SIGNATURE in that buffer.
Reads a missing PASSPHRASE using `gpg-passphrase-read'.  Uses key ID
SIGN-WITH-KEY if given, otherwise the default key ID.  Returns t if
everything worked out well, nil otherwise.  Consult buffer RESULT for
details.  ARMOR the result and activate canonical TEXTMODE if
requested."
  (interactive "bBuffer containing plaintext: \nbBuffer for text with signature: \nbBuffer for status information: ")
  (let ((subst (list (cons 'sign-with-key 
			   (gpg-build-flag-sign-with-key sign-with-key))
		     (cons 'armor (if armor gpg-command-flag-armor))
		     (cons 'textmode (if armor gpg-command-flag-textmode)))))
    (gpg-call-process (gpg-exec-path gpg-command-sign-detached)
		      (gpg-build-arg-list (cdr gpg-command-sign-detached)
					  subst)
		      plaintext signature result
		      (if passphrase passphrase (gpg-passphrase-read))))
  (when passphrase
    (gpg-passphrase-clear-string passphrase)))


;;;###autoload
(defun gpg-sign-encrypt
  (plaintext ciphertext result recipients &optional passphrase sign-with-key
   armor textmode)
  "Sign buffer PLAINTEXT, and store SIGNATURE in that buffer.
RECIPIENTS is a list of key IDs used for encryption.  This function
reads a missing PASSPHRASE using `gpg-passphrase-read', and uses key
ID SIGN-WITH-KEY for the signature if given, otherwise the default key
ID.  Returns t if everything worked out well, nil otherwise.  Consult
buffer RESULT for details.  ARMOR the result and activate canonical
TEXTMODE if requested."
  (interactive (list
		(read-buffer "Buffer containing plaintext: " nil t)
		(read-buffer "Buffer for ciphertext: " nil t)
		(read-buffer "Buffer for status informationt: " nil t)
		(gpg-read-recipients)))
    (let ((subst `((sign-with-key . ,(gpg-build-flag-sign-with-key 
				      sign-with-key))
		   (plaintext-file . stdin-file)
		   (recipients . ,(gpg-build-flag-recipients recipients))
		   (armor ,(if armor gpg-command-flag-armor))
		   (textmode ,(if armor gpg-command-flag-textmode)))))
      (gpg-call-process (gpg-exec-path gpg-command-sign-encrypt)
			(gpg-build-arg-list (cdr gpg-command-sign-encrypt) 
					    subst)
			plaintext ciphertext result
			(if passphrase passphrase (gpg-passphrase-read))))
  (when passphrase
    (gpg-passphrase-clear-string passphrase)))


;;;###autoload
(defun gpg-encrypt
  (plaintext ciphertext result recipients &optional passphrase armor textmode)
  "Encrypt buffer PLAINTEXT, and store CIPHERTEXT in that buffer.
RECIPIENTS is a list of key IDs used for encryption.  Returns t if
everything worked out well, nil otherwise.  Consult buffer RESULT for
details.  ARMOR the result and activate canonical
TEXTMODE if requested."
  (interactive (list
		(read-buffer "Buffer containing plaintext: " nil t)
		(read-buffer "Buffer for ciphertext: " nil t)
		(read-buffer "Buffer for status informationt: " nil t)
		(gpg-read-recipients)))
  (let ((subst `((plaintext-file . stdin-file)
		 (recipients . ,(gpg-build-flag-recipients recipients))
		 (armor ,(if armor gpg-command-flag-armor))
		 (textmode ,(if armor gpg-command-flag-textmode)))))
    (gpg-call-process (gpg-exec-path gpg-command-encrypt)
		      (gpg-build-arg-list (cdr gpg-command-encrypt) subst)
		      plaintext ciphertext result nil))
  (when passphrase
    (gpg-passphrase-clear-string passphrase)))


;;;; Key management

;;; ADT: OpenPGP Key

(defun gpg-key-make (user-id key-id unique-id length algorithm
		     creation-date expire-date validity trust)
  "Create a new key object (for internal use only)."
  (vector 
	;;  0   1      2         3      4        
	user-id key-id unique-id length algorithm
	;; 5          6           7        8
	creation-date expire-date validity trust))


(defun gpg-key-p (key)
  "Return t if KEY is a key specification."
  (and (arrayp key) (equal (length key) 9) key))

(defmacro gpg-key-primary-user-id (key)
  "The primary user ID for KEY (human-readable).
DO NOT USE this ID for selecting recipients.  It is probably not
unique."
  (list 'car (list 'aref key 0)))

(defmacro gpg-key-user-ids (key)
  "A list of additional user IDs for KEY (human-readable).
DO NOT USE these IDs for selecting recipients.  They are probably not
unique."
  (list 'cdr (list 'aref key 0)))

(defmacro gpg-key-id (key)
  "The key ID of KEY.
DO NOT USE this ID for selecting recipients.  It is not guaranteed to
be unique."
  (list 'aref key 1))

(defun gpg-short-key-id (key)
  "The short key ID of KEY."
  (let* ((id (gpg-key-id key))
	 (len (length id)))
    (if (> len 8)
	(substring id (- len 8))
      id)))

(defmacro gpg-key-unique-id (key)
  "A non-standard ID of KEY which is only valid locally.
This ID can be used to specify recipients in a safe manner.  Note,
even this ID might not be unique unless GnuPG is used."
  (list 'aref key 2))

(defmacro gpg-key-unique-id-list (key-list)
  "Like `gpg-key-unique-id', but operate on a list."
  `(mapcar (lambda (key) (gpg-key-unique-id key)) 
	   ,key-list))

(defmacro gpg-key-length (key)
  "Returns the key length."
  (list 'aref key 3))

(defmacro gpg-key-algorithm (key)
  "The encryption algorithm used by KEY.
One of the symbols `rsa', `rsa-encrypt', `rsa-sign', `elgamal',
`elgamal-encrypt', `dsa'."
  (list 'aref key 4))

(defmacro gpg-key-creation-date (key)
  "A string with the creation date of KEY in ISO format."
  (list 'aref key 5))

(defmacro gpg-key-expire-date (key)
  "A string with the expiration date of KEY in ISO format."
  (list 'aref key 6))

(defmacro gpg-key-validity (key)
  "The calculated validity of KEY.  
One of the symbols `not-known', `disabled', `revoked', `expired',
`undefined', `trust-none', `trust-marginal', `trust-full',
`trust-ultimate' (see the GnuPG documentation for details)."
 (list 'aref key 7))

(defmacro gpg-key-trust (key)
  "The assigned trust for KEY.  
One of the symbols `not-known', `undefined', `trust-none',
`trust-marginal', `trust-full' (see the GnuPG
documentation for details)."
  (list 'aref key 8))

(defun gpg-key-lessp (a b)
  "Returns t if primary user ID of A is less than B."
  (string-lessp (gpg-key-primary-user-id a) (gpg-key-primary-user-id b) ))

;;; Accessing the key database:

;; Internal functions:

(defmacro gpg-key-list-keys-skip-field ()
  '(search-forward ":" eol 'move))

(defmacro gpg-key-list-keys-get-field ()
  '(buffer-substring (point) (if (gpg-key-list-keys-skip-field) 
				 (1- (point)) 
			       eol)))
(defmacro gpg-key-list-keys-string-field ()
  '(gpg-key-list-keys-get-field))

(defmacro gpg-key-list-keys-read-field ()
  (let ((field (make-symbol "field")))
    `(let ((,field (gpg-key-list-keys-get-field)))
       (if (equal (length ,field) 0)
	   nil
	 (read ,field)))))

(defun gpg-key-list-keys-parse-line ()
  "Parse the line in the current buffer and return a vector of fields."
  (let* ((eol (gpg-point-at-eol))
	 (v (if (eolp)
		nil
	      (vector
	       (gpg-key-list-keys-read-field) ; type
	       (gpg-key-list-keys-get-field) ; trust
	       (gpg-key-list-keys-read-field) ; key length
	       (gpg-key-list-keys-read-field) ; algorithm
	       (gpg-key-list-keys-get-field) ; key ID
	       (gpg-key-list-keys-get-field) ; creation data
	       (gpg-key-list-keys-get-field) ; expire
	       (gpg-key-list-keys-get-field) ; unique (local) ID
	       (gpg-key-list-keys-get-field) ; ownertrust
	       (gpg-key-list-keys-string-field) ; user ID
	       ))))
    (if (eolp)
	(when v
	  (forward-char 1))
      (error "Too many fields in GnuPG key database"))
    v))

(defconst gpg-pubkey-algo-alist
  '((1 . rsa)
    (2 . rsa-encrypt-only)
    (3 . rsa-sign-only)
    (16 . elgamal-encrypt-only)
    (17 . dsa)
    (20 . elgamal))
  "Alist mapping OpenPGP public key algorithm numbers to symbols.")

(defconst gpg-trust-alist
  '((?- . not-known)
    (?o . not-known)
    (?d . disabled)
    (?r . revoked)
    (?e . expired)
    (?q . trust-undefined)
    (?n . trust-none)
    (?m . trust-marginal)
    (?f . trust-full)
    (?u . trust-ultimate))
  "Alist mapping GnuPG trust value short forms to long symbols.")

(defconst gpg-unabbrev-trust-alist
  '(("TRUST_UNDEFINED" . trust-undefined)
    ("TRUST_NEVER"     . trust-none)
    ("TRUST_MARGINAL"  . trust-marginal)
    ("TRUST_FULLY"     . trust-full)
    ("TRUST_ULTIMATE"  . trust-ultimate))
  "Alist mapping capitalized GnuPG trust values to long symbols.")

(defmacro gpg-key-list-keys-in-buffer-store ()
  '(when primary-user-id
     (sort user-id 'string-lessp)
     (push (gpg-key-make (cons primary-user-id  user-id)
			 key-id unique-id key-length
			 algorithm creation-date 
			 expire-date validity trust)
	   key-list)))

(defun gpg-key-list-keys-in-buffer (&optional buffer)
  "Return a list of keys for BUFFER.
If BUFFER is omitted, use current buffer."
  (with-current-buffer (if buffer buffer (current-buffer))
    (goto-char (point-min))
    ;; Skip key ring filename written by GnuPG.
    (search-forward "\n---------------------------\n" nil t)
    ;; Loop over all lines in buffer and analyze them.
    (let (primary-user-id user-id key-id unique-id ; current key components
          key-length algorithm creation-date expire-date validity trust
	  line				; fields in current line
	  key-list)			; keys gather so far
    
      (while (setq line (gpg-key-list-keys-parse-line))
	(cond
	 ;; Public or secret key.
	 ((memq (aref line 0) '(pub sec))
	  ;; Store previous key, if any.
	  (gpg-key-list-keys-in-buffer-store)
	  ;; Record field values.
	  (setq primary-user-id (aref line 9))
	  (setq user-id nil)
	  (setq key-id (aref line 4)) 
	  ;; We use the key ID if no unique ID is available.
	  (setq unique-id (if (> (length (aref line 7)) 0)
			      (concat "#" (aref line 7))
			    (concat "0x" key-id)))
	  (setq key-length (aref line 2))
	  (setq algorithm (assq (aref line 3) gpg-pubkey-algo-alist))
	  (if algorithm
	      (setq algorithm (cdr algorithm))
	    (error "Unknown algorithm %s" (aref line 3)))
	  (setq creation-date (if (> (length (aref line 5)) 0)
				  (aref line 5)))
	  (setq expire-date (if (> (length (aref line 6)) 0)
				(aref line 6)))
	  (setq validity (assq (aref (aref line 1) 0) gpg-trust-alist))
	  (if validity
	      (setq validity (cdr validity))
	    (error "Unknown validity specification %S" (aref line 1)))
	  (setq trust (assq (aref (aref line 8) 0) gpg-trust-alist))
	  (if trust
	      (setq trust (cdr trust))
	    (error "Unknown trust specification %S" (aref line 8))))
	
	 ;; Additional user ID
	 ((eq 'uid (aref line 0))
	  (setq user-id (cons (aref line 9) user-id)))
	 
	 ;; Subkeys are ignored for now.
	 ((memq (aref line 0) '(sub ssb))
	  t)
	 (t (error "Unknown record type %S" (aref line 0)))))

      ;; Store the key retrieved last.
      (gpg-key-list-keys-in-buffer-store)
      ;; Sort the keys according to the primary user ID.
      (sort key-list 'gpg-key-lessp))))

(defun gpg-key-list-keyspec (command &optional keyspec stderr ignore-error)
  "Insert the output of COMMAND before point in current buffer."
  (let* ((cmd (gpg-exec-path command))
	 (key (if (equal keyspec "") nil keyspec))
	 (args (gpg-build-arg-list (cdr command) `((key-id . ,key))))
	 exit-status)
    (setq exit-status 
	  (apply 'call-process-region 
		 (point-min) (point-min) ; no data
		 cmd
		 nil			; don't delete
		 (if stderr t '(t nil))
		 nil			; don't display
		 args))
    (unless (or ignore-error (equal exit-status 0))
      (error "GnuPG command exited unsuccessfully"))))
  
  
(defun gpg-key-list-keyspec-parse (command &optional keyspec)
  "Return a list of keys matching KEYSPEC.
COMMAND is used to obtain the key list.  The usual substring search
for keys is performed."
  (with-temp-buffer 
    (buffer-disable-undo)
    (gpg-key-list-keyspec command keyspec)
    (gpg-key-list-keys-in-buffer)))

;;;###autoload
(defun gpg-key-list-keys (&optional keyspec)
  "A list of public keys matching KEYSPEC.
The usual substring search for keys is performed."
  (gpg-key-list-keyspec-parse gpg-command-key-public-ring keyspec))

;;;###autoload
(defun gpg-key-list-secret-keys (&optional keyspec)
  "A list of secret keys matching KEYSPEC.
The usual substring search for keys is performed."
  (gpg-key-list-keyspec-parse gpg-command-key-secret-ring keyspec))

;;;###autoload
(defun gpg-key-insert-public-key (key)
  "Inserts the public key(s) matching KEYSPEC.
The ASCII-armored key is inserted before point into current buffer."
  (gpg-key-list-keyspec gpg-command-key-export key))

;;;###autoload
(defun gpg-key-insert-information (key)
  "Insert human-readable information (including fingerprint) on KEY.
Insertion takes place in current buffer before point."
  (gpg-key-list-keyspec gpg-command-key-verify key))

;;;###autoload
(defun gpg-key-retrieve (key)
  "Fetch KEY from default key server.
KEY is a key ID or a list of key IDs.  Status information about this
operation is inserted into the current buffer before point."
  (gpg-key-list-keyspec gpg-command-key-retrieve key t t))

;;;###autoload
(defun gpg-key-add-to-ring (key result)
  "Adds key in buffer KEY to the GnuPG key ring.
Human-readable information on the RESULT is stored in buffer RESULT
before point.")

(provide 'gpg)

;;; arch-tag: c972455d-9bc5-4de1-9dc7-4f494d63053b
;;; gpg.el ends here
