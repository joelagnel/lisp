;;; esense-trace.el --- Support mode for erlang trace analysis

;; Copyright (C) 2006  Tamas Patrovics

;; Author: Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;;
;;; Turn it on in a trace buffer with M-x esense-trace-mode.
;;;
;;; Press h for the available commands.
;;;
;;; It is also available as a minor mode (esense-trace-minor-mode)
;;; which is useful when tracing from Shell buffers, because the trace
;;; output does not need to be saved into a separate file in order to
;;; use the trace analysis functions.
;;;
;;; The minor mode has the same key bindings as the major mode, only
;;; the keys need to be prefixed with C-c C-c.
;;;

;;; Code:

(require 'esense)

;;;--------------------------------------------------------------------
;;;
;;;  User configuration
;;;

(defcustom esense-trace-show-function-in-new-window nil
  "It controls whether to show the function in a new window when
jumping to a function definition from a trace."
  :type 'boolean
  :group 'esense)

;----------------------------------------------------------------------

(defstruct esense-trace-format
  "Trace file format descriptor."
  ;; regular expression for matching a function call
  regexp
  ;; function for retrieving the module name from a match
  module-getter
  ;; function for retrieving the function name from a match
  function-getter
  ;; function for retrieving the beginning position of the arity
  ;; or the argument list
  argpos-getter)


(defvar esense-trace-formats 
  (list 
   (make-esense-trace-format
    :regexp 
    (concat 
     "^##### [^{]+{?{trace_ts,[^,]+,"
     "\\(call,{\\([^,]+\\),\\s-*\\([^,]+\\),\\s-*\\(\\[\\)"
     "\\|return_[a-z]+{\\([^:]+\\):\\([^/]+\\)/\\([0-9]+\\)}\\)")
    :module-getter (lambda ()
                     (or (match-string 2)
                         (match-string 5)))
    :function-getter (lambda ()
                       (or (match-string 3)
                           (match-string 6)))
    :argpos-getter (lambda ()
                     (or (match-beginning 4)
                         (match-beginning 7))))

   (make-esense-trace-format
    :regexp (concat "(<.*>) \\(call\\|returned from\\|returning to\\) "
                    "\\([^:]+\\):\\([^/(]+\\)\\(\\((\\)\\|/\\([0-9]\\)\\)")
    :module-getter (lambda ()
                     (match-string 2))
    :function-getter (lambda ()
                       (match-string 3))
    :argpos-getter (lambda ()
                     (or (match-beginning 5)
                         (match-beginning 6))))

   (make-esense-trace-format
    :regexp (concat "\\(call\\|return_from\\) {[^}]*} {?{"
                    "\\([^,]+\\),\\([^,]+\\),\\([^}]+\\)}")
    :module-getter (lambda ()
                     (match-string 2))
    :function-getter (lambda ()
                       (match-string 3))
    :argpos-getter (lambda ()
                     (match-beginning 4)))

   (make-esense-trace-format
    :regexp (concat "error_reason: {function_clause,\\[{\\([^,]+\\),"
                    "\\(\\s \\|\012\\)+\\([^,]+\\),"
                    "\\(\\s \\|\012\\)+\\(\\[\\)")
    :module-getter (lambda ()
                     (match-string 1))
    :function-getter (lambda ()
                       (match-string 3))
    :argpos-getter (lambda ()
                     (match-beginning 5))))

"List of supported formats.")


(defvar esense-trace-format nil
  "The appropriate item from the list `esense-trace-formats' 
for processing the current buffer contents.")

(make-variable-buffer-local 'esense-trace-format)

     
(defvar esense-trace-mode-map nil
  "Keymap for Esense trace mode.")
     
(if esense-trace-mode-map
    ()             ; Do not change the keymap if it is already set up.
  (setq esense-trace-mode-map (make-keymap))
  (suppress-keymap esense-trace-mode-map)
  (define-key esense-trace-mode-map "c" 'esense-trace-convert-function)
  (define-key esense-trace-mode-map "p" 'esense-trace-previous-function)
  (define-key esense-trace-mode-map "n" 'esense-trace-next-function)
  (define-key esense-trace-mode-map "q" 'esense-trace-quit-buffer)
  (define-key esense-trace-mode-map "h" 'esense-trace-show-help)
  (define-key esense-trace-mode-map "o" 'esense-trace-show-headings)
  (define-key esense-trace-mode-map "g" 'esense-trace-go-to-function-definition))


(defvar esense-trace-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") esense-trace-mode-map)
    map)
  "Keymap for Esense trace minor mode.")


(defun esense-trace-mode ()
  "Major mode for analysing Erlang trace files."
  (interactive)
  ;; first check if it's a buffer which trace mode understands
  (let ((format (esense-trace-get-buffer-format)))
    (if (not format)
        (message (concat "Buffer format is not recognized. "
                         "ESense trace mode is not activated."))
        
      (kill-all-local-variables)
      (setq esense-trace-format format)
      (use-local-map esense-trace-mode-map)
      (setq mode-name "ETrace")
      (setq major-mode 'esense-trace-mode)
      (run-hooks 'esense-trace-mode-hook)
      (esense-maybe-initialize)
      (message "Press h for help."))))


(easy-mmode-define-minor-mode
  esense-trace-minor-mode
  "Erlang trace minor mode."
  nil
  " ETrace"
  esense-trace-minor-mode-map

  (setq esense-trace-format (esense-trace-get-buffer-format))
  (if esense-trace-format
      (esense-maybe-initialize)
    (message (concat "Buffer format is not recognized. "
                     "ESense trace mode is activated, but it won't work."))))
 

(defun esense-trace-get-buffer-format ()
  "Return the trace format of the buffer if recognized."
  (save-excursion
    (goto-char (point-min))
    (some (lambda (entry)
            (if (re-search-forward 
                 (esense-trace-format-regexp entry)
                 nil t)
                entry))
          esense-trace-formats)))
  

(defun esense-trace-next-function ()
  "Jump to the next function in the trace."
  (interactive)
  (let ((begin (point-at-bol)))
    (if (and (re-search-forward 
              (esense-trace-format-regexp esense-trace-format)
              nil t)
             (or (save-excursion
                   (goto-char (match-beginning 0))
                   (/= (point-at-bol) begin))
                 ;; we're in the same function, so search again
                 (re-search-forward 
                  (esense-trace-format-regexp esense-trace-format) 
                  nil t)))
        (goto-char (match-beginning 0))
      (message "No more functions."))))


(defun esense-trace-previous-function ()
  "Jump to the previous function in the trace."
  (interactive)
  (unless (re-search-backward 
           (esense-trace-format-regexp esense-trace-format)
           nil t)
    (message "No more functions.")))


(defun esense-trace-quit-buffer ()
  "Quit from the trace buffer."
  (interactive)
  (kill-buffer nil))


(defun esense-trace-show-help ()
  "Show help for commands in the trace buffer."
  (interactive)
  (esense-show-tooltip-for-point
   (substitute-command-keys
    (concat "Available commands:\n"
            "\n"
            "\\[esense-trace-convert-function] - convert function to a more readable format (resolve records)\n"
            "\\[esense-trace-go-to-function-definition] - go to function definition\n"
            "\\[esense-trace-show-help] - show help\n"
            "\\[esense-trace-next-function] - next function\n"
            "\\[esense-trace-previous-function] - previous function\n"
            "\\[esense-trace-show-headings] - show function headings in other window\n"
            "\\[esense-trace-quit-buffer] - quit buffer\n"))))


(defun esense-trace-go-to-function-definition ()
  "Go to the definition of the current function."
  (interactive)
  (let ((info (esense-trace-get-current-function-info)))
    (if info
        (destructuring-bind 
            (module function arity args) info

          (condition-case error-data
              (let ((esense-find-function-matching-invocation-pattern t))
                (esense-go-to-function-definition module function arity args))
            (esense-error (error (cdr error-data)))))

      (message "You're not standing in a traced function call."))))


(defun esense-trace-get-current-function-info ()
  "Return a list of (MODULE FUNCTION ARITY ARGUMENTS) for current
function around point or nil if no function is found."
  (if (save-excursion
        ;; find the next one first to avoid skipping over the
        ;; current entry when searching backwards
        (or (and (progn 
                   (end-of-line)
                   (re-search-forward
                    (esense-trace-format-regexp esense-trace-format) nil t))
                 (goto-char (match-beginning 0)))
            (goto-char (point-max)))

        (re-search-backward
         (esense-trace-format-regexp esense-trace-format) nil t))

      (let ((module (funcall (esense-trace-format-module-getter 
                              esense-trace-format)))
            (function (funcall (esense-trace-format-function-getter 
                                esense-trace-format)))
            arity args)

        (save-excursion
          (goto-char (funcall (esense-trace-format-argpos-getter
                               esense-trace-format)))
          (case (char-after)
            (?\[
             (setq args (esense-get-function-invocation ?[ ?]))
             (setq arity (esense-get-arity-from-string args)))

            (?\(
             (setq args (esense-get-function-invocation))
             (setq arity (esense-get-arity-from-string args)))

            ;; let's say it's an arity descriptor
            (t
             (looking-at "\\([0-9]+\\)")
             (setq arity (string-to-int (match-string-no-properties 1))))))

        (list module function arity args))))


(defun esense-trace-convert-function ()
  "Convert a function in a trace buffer to a more readable format
 (record field names added, etc.)."
  (interactive)

  (let ((info (esense-trace-get-current-function-info)))
    (if (not info)
        (message "You're not standing in a traced function call.")

      (destructuring-bind 
          (module function arity args) info

        (unless args
          (error "No argument list found for %s:%s." module function))

        (let* ((module-info (condition-case error-data
                                (esense-lookup-module module)
                              (esense-error (error (cdr error-data)))))
               (module-buffer (get-file-buffer 
                               (esense-erlang-source module-info)))
               kill
               (level 0)
               record-stack       ; contains information about records
				  ; ecountered
               position-stack     ; indicates the index of the current
				  ; fields in the records ecountered
               resolved-records         ; association list of resolved
					; records (including those
					; which could not be resolved)
               include-candidates)      ; list of candidate include
					; files for certain prefixes

          (unless module-buffer
            (setq module-buffer 
                  (find-file-noselect
                   (esense-erlang-source module-info) t))
            (setq kill t))

          (pop-to-buffer (get-buffer-create "*esense-converted-trace*"))
          (erase-buffer)
          (erlang-mode)
          (insert args)
          (goto-char (point-min))
          (insert module ":" function)

          (loop do
                (skip-syntax-forward " >")

                (if (eobp)
                    (error "End of buffer is reached during parsing."))

                (cond
                 ((= (char-syntax (char-after)) ?\()            
                  (incf level)
                  (if (eq (char-after) ?{)
                      (push 0 position-stack)
                    (push nil position-stack))
                  (push nil record-stack)
                  (forward-char))
           
                 ((= (char-syntax (char-after)) ?\))
                  (decf level)
                  (pop position-stack)
                  (pop record-stack)
                  (forward-char))

                 ((= (char-after) ?\")
                  (forward-char)
                  (parse-partial-sexp (point) (point-max) nil nil nil 'syntax-table))

                 ((= (char-after) ?,)
                  (if (car position-stack)
                      (incf (car position-stack)))
                  (forward-char)

                  (skip-syntax-forward " >")

                  (if (and (car record-stack)
                           (> (car position-stack) 0))
                      (let ((field (nth (1- (car position-stack))
                                        (esense-record-fields (car record-stack)))))
                        (insert (if field
                                    (esense-record-field-name field)
                                  "??")
                                " = "))))

                 ((= (char-after) ?<)
                  (with-syntax-table esense-arity-syntax-table
                    (forward-sexp)))

                 ;; references, funs
                 ((= (char-after) ?#)
                  (forward-char)
                  (skip-syntax-forward "w")
                  (assert (= (char-after) ?<)
                          nil
                          "Next character should be a '<'")
                  (with-syntax-table esense-arity-syntax-table
                    (forward-sexp)))

                 ;; numbers
                 ((and (>= (char-after) ?0)
                       (<= (char-after) ?9))
                  (while (or (when (and (>= (char-after) ?0)
                                        (<= (char-after) ?9))
                               (skip-syntax-forward "w")
                               t)
                               
                             (when (= (char-after) ?.)
                               (forward-char)
                               t))
                    nil))

                 ;; atoms
                 ((looking-at erlang-atom-regexp)

                  (goto-char (match-end 0))

                  ;; if the atom is the first element in a tuple
                  (when (and (car position-stack)
                             (= (car position-stack) 0))
                    (let ((record-name 
                           (match-string-no-properties erlang-atom-regexp-matches)))
                      (if (eq (aref record-name 0) ?')
                          (setq record-name (substring record-name 1 -1)))

                      ;; try to resolve it as a record
                      (message "Trying to resolve record %s..." record-name)

                      (let ((record-info (assoc record-name resolved-records)))
                        (if record-info
                            (setq record-info (cdr record-info))

                          (setq 
                           record-info
                           (or
                            (with-current-buffer module-buffer
                              (let ((esense-ignore-unknown-includes t))
                                (condition-case nil
                                    (caar (esense-get-record-data record-name))
                                  (esense-error nil))))

                            ;; check possible prefix of record name
                            (let ((prefix (let ((case-fold-search nil))
                                            (if (string-match 
                                                 "^\\([a-z]+\\)[A-Z]" 
                                                 record-name)
                                                (match-string 1 record-name))))
                                  include-candidates-for-prefix)
                              (when prefix
                                (setq include-candidates-for-prefix
                                      (assoc prefix include-candidates))

                                (if include-candidates-for-prefix
                                    ;; we encountered this prefix earlier
                                    (setq include-candidates-for-prefix
                                          (cdr include-candidates-for-prefix))
                                    
                                  ;; find matching include files
                                  (let ((prefix-length (length prefix)))
                                    (setq include-candidates-for-prefix
                                          (remove-if-not 
                                           (lambda (include)
                                             (eq (compare-strings
                                                  prefix 0 prefix-length
                                                  (car include) 0 prefix-length)
                                                 t))
                                           esense-include-files))
                                    (push (cons prefix 
                                                include-candidates-for-prefix)
                                          include-candidates)))

                                ;; check candidate include files
                                (some 
                                 (lambda (include)
                                   (let ((include-data 
                                          (esense-read-include-file include)))
                                     (some (lambda (record)
                                             (if (equal (esense-record-name record)
                                                        record-name)
                                                 record))
                                           (esense-erlang-records include-data))))
                                 include-candidates-for-prefix)))))

                          (push (cons record-name record-info) 
                                resolved-records))

                        (setf (car record-stack) record-info)))))
                      
                 ;; something else
                 (t
                  (error "Unknown character after point: '%c'" (char-after))))

                until (= level 0))

          (if kill
              (kill-buffer module-buffer))))

      (goto-char (point-min))
      (message "Done."))))


(defun esense-trace-show-headings ()
  "Show function headings in an Occur window."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (occur (esense-trace-format-regexp esense-trace-format))))

  
(provide 'esense-trace)
;;; esense-trace.el ends here
