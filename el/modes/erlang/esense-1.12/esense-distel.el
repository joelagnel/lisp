;; esense-distel.el - Distel support lib for ESense
;;
;; Copyright (C) 2006  Tamas Patrovics
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;; Commentary:
;;
;; This library enables ESense to talk to an Erlang node directly via
;; Distel instead of starting a new interpreter for every operation.
;;

;; load distel if available
(if (featurep 'xemacs)
    (condition-case nil
        (require 'distel)
      ((file-error error) nil))
  (require 'distel nil t))
 
;----------------------------------------------------------------------
;
; User configuration
;

(defcustom esense-distel-node nil
  "Name of an erlang node (atom) to be used. If nil then Distel is not
used. Note that the name must be an Emacs Lisp symbol (e.g. 'distel@myhost),
not a string.

It is the responsibility of the user to ensure the node is actually
running and it has all the beams for Distel and ESense (including
Yaws) loaded. The simplest way to achieve it is to put the relevant
paths into ~/.erlang:

    code:add_pathz(\"/export/localhome/locpata/local/share/distel/ebin\").

Note that the node has to speak the R9 Erlang protocol, so start the
node with the option +R9 if using OTP R10B or later."
      :type 'symbol
      :group 'esense)


(defcustom esense-check-syntax-when-idle nil
  "The number of idle seconds after which the syntax of the current
buffer is checked for errors. This option needs to be set before
ESense is loaded, otherwise the idle timer will not be started."
      :type '(choice integer 
                     (const nil))
      :group 'esense)


(defface esense-error-header-face
  '((((class color)) (:foreground "black" :background "yellow"))
    (t (:reverse-video t)))
  "Face for the header line if there are errors in the current file."
  :group 'esense)

;----------------------------------------------------------------------

(defvar esense-syntax-errors nil
  "List of syntax errors in the current buffer.")

(make-variable-buffer-local 'esense-syntax-errors)

(defvar esense-current-syntax-error nil
  "Index of the current syntax error in vector `esense-syntax-errors'.")

(defvar esense-syntax-last-buffer-modification nil
  "Value of buffer-modified-tick at the last syntax check.")

(defvar esense-syntax-check-timer nil
   "The value of the timer for checking the syntax of the current
buffer.")


(defun esense-distel-available-p ()
  "Return t if Distel is installed and configured."
  (and (featurep 'distel)
       esense-distel-node))


(defun esense-distel-rpc (module function arguments)
  "Calls the MFA on the remote node and returns the result
synchronously."
  (interactive)
  (let ((node-status nil)
        result)
    (erl-rpc (lambda (r) 
               (setq result r)
               (setq node-status t)
               (push 'esense-signal unread-command-events))
             nil
             esense-distel-node
             module function arguments)

    ;; this will do for know, but it should be more polished
    (let ((timeout 5))
      (while (and (not node-status)
                  (> timeout 0))
        (sit-for 1)
        (decf timeout)))

    (if (eq (car unread-command-events) 'esense-signal)
        (pop unread-command-events))

    (unless node-status
      (error "No answer from node %s" esense-distel-node))

    result))


(defun esense-distel-parse-file (content)
  "Pars the file CONTENT refers to and put the result into CONTENT or
throw an error if something goes wrong."
  (let* ((file (esense-erlang-source content))
         (result (esense-distel-rpc 'esense 
                                    (let ((ext (file-name-extension file)))
                                      (cond ((equal ext "html") 'parse_html_file)
                                            ((equal ext "erl") 'parse_module_file)
                                            ((equal ext "hrl") 'parse_header_file)
                                            (t (assert nil))))
                                    (list file))))
    (if (eq (elt result 0) 'content)
        (esense-distel-convert-to-elisp result content)
      (error "Cannot read index from file %s" file))))
        
  
(defun esense-distel-convert-to-elisp (content result)
  "Convert index information in Erlang format to Elisp."
  (dolist (erl-function (elt content 2))
    (let ((function (make-esense-function)))        
      (assert (eq (elt erl-function 0) 'function))
      (push function (esense-erlang-functions result))

      (setq erl-function (esense-distel-convert-undefined erl-function))

      (setf (esense-function-name function) (elt erl-function 1))
      (setf (esense-function-arity function) (elt erl-function 2))
      (setf (esense-function-spec function) (elt erl-function 3))
      (setf (esense-function-docref function) (elt erl-function 4))
      (setf (esense-function-line function) (elt erl-function 5))
      (setf (esense-function-doc function) (elt erl-function 6))

      (dolist (param (elt erl-function 7))
        (push param (esense-function-params function)))

      (setf (esense-function-exported function) (elt erl-function 8))))

  (dolist (erl-record (elt content 3))
    (let ((record (make-esense-record)))
      (assert (eq (elt erl-record 0) 'record))
      (push record (esense-erlang-records result))

      (setq erl-record (esense-distel-convert-undefined erl-record))

      (setf (esense-record-name record) (elt erl-record 1))
      (setf (esense-record-line record) (elt erl-record 2))
      (setf (esense-record-doc record) (elt erl-record 3))

      (dolist (erl-field (elt erl-record 4))
        (let ((field (make-esense-record-field)))
          (assert (eq (elt erl-field 0) 'field))
          (push field (esense-record-fields record))
            
          (setq erl-field (esense-distel-convert-undefined erl-field))

          (setf (esense-record-field-name field) (elt erl-field 1))
          (setf (esense-record-field-doc field) (elt erl-field 2))))))
            
  (dolist (erl-macro (elt content 4))
    (let ((macro (make-esense-macro)))
      (assert (eq (elt erl-macro 0) 'macro))
      (push macro (esense-erlang-macros result))
            
      (setq erl-macro (esense-distel-convert-undefined erl-macro))

      (setf (esense-macro-name macro) (elt erl-macro 1))
      (setf (esense-macro-value macro) (elt erl-macro 2))))
            
  (dolist (include (elt content 5))
    (push (cons 'include include) (esense-erlang-includes result)))

  (dolist (include_lib (elt content 6))
    (push (cons 'include_lib include_lib) (esense-erlang-includes result)))
            
  (dolist (erl-import (elt content 7))
    (let ((import (make-esense-import)))
      (assert (eq (elt erl-import 0) 'import))
      (push import (esense-erlang-imports result))
            
      (setq erl-import (esense-distel-convert-undefined erl-import))

      (setf (esense-import-module import) (elt erl-import 1))

      (dolist (erl-imported-function (elt erl-import 2))
        (let ((imported-function (make-esense-imported-function)))
          (assert (eq (elt erl-imported-function 0) 'imported_function))
          (push imported-function (esense-import-functions import))
            
          (setq erl-imported-function 
                (esense-distel-convert-undefined erl-imported-function))

          (setf (esense-imported-function-name imported-function)
                (elt erl-imported-function 1))
          (setf (esense-imported-function-arity imported-function)
                (elt erl-imported-function 2)))))))


(defun esense-distel-convert-undefined (seq)
  "Erlang atom `undefined's in seq are converted to nil."
  (nsubstitute nil 'undefined seq))


(defun esense-distel-get-errors ()
  "Get list of errors for the current buffer."
  (let ((file (make-temp-file "esense")))
    (write-region (point-min) (point-max) file)
    ;; clear the wrote message from write-region
    (message "")
    (unwind-protect
        (let ((errors (esense-distel-rpc 'esense 'get_errors (list file))))
          (unless (listp errors)
            (error "Unable to retrieve problems for current buffer."))
          errors)
      (delete-file file))))


(defun esense-check-current-buffer-syntax ()
  "Check the syntax of the current buffer and display problems if any."
  (when (and (eq major-mode 'erlang-mode)
             ;; no completion in progress
             (not esense-completion-list)
             (not (equal esense-syntax-last-buffer-modification
                         (buffer-modified-tick))))

    (setq esense-syntax-last-buffer-modification (buffer-modified-tick))

    (let ((errors (esense-distel-get-errors)))
      (if errors
          (progn
            (setq esense-syntax-errors (coerce errors 'vector))
            (setq esense-current-syntax-error 0)
            (setq header-line-format
                  (propertize 
                   (concat
                    (let ((numerrors (length esense-syntax-errors)))                      
                      (concat " There "
                              (if (= numerrors 1)
                                  "is an error"
                                (format "are %d errors" numerrors))
                              " in this buffer. "))
                    "Use "
                    (substitute-command-keys "\\[esense-distel-previous-error]")
                    "/"
                    (substitute-command-keys "\\[esense-distel-next-error]")
                    " to jump to the previous/next error."
                    (make-string (frame-parameter nil 'width) ? ))
                   'face 'esense-error-header-face)))

        (setq esense-syntax-errors nil)
        (setq header-line-format nil)))

    (force-mode-line-update)))


(defun esense-distel-next-error ()
  "Go to the next syntax error."
  (interactive)
  (if (not esense-syntax-errors)
      (message "No errors.")

    (let* ((error-info (aref esense-syntax-errors
                             esense-current-syntax-error))
           (line (aref error-info 0))
           (msg (aref error-info 1)))
      (goto-line line)
      (message msg))

    (incf esense-current-syntax-error))
    (if (= esense-current-syntax-error (length esense-syntax-errors))
        (setq esense-current-syntax-error 0)))


(defun esense-distel-previous-error ()
  "Go to the previous syntax error."
  (interactive)
  (if (not esense-syntax-errors)
      (message "No errors.")

    (let* ((error-info (aref esense-syntax-errors
                             esense-current-syntax-error))
           (line (aref error-info 0))
           (msg (aref error-info 1)))
      (goto-line line)
      (message msg))

    (decf esense-current-syntax-error)
    (if (< esense-current-syntax-error 0)
        (setq esense-current-syntax-error
              (1- (length esense-syntax-errors))))))


(provide 'esense-distel)
;;; esense-distel.el ends here
