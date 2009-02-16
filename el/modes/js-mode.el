;;; js-mode.el --- minor mode for interacting with Mozilla
;;
;; Copyright (C) 2004  Helmut Eller
;;
;; You can redistribute this file under the terms of the GNU General
;; Public License.
;;

;;; Commentary:
;;
;; This file implements some commands for interacting with Mozilla
;; from Emacs.  Emacs uses a TCP connection to communicate with
;; Mozilla.  The available commands are:
;;   
;;    C-c :     Sends a piece of Javascript code to Mozilla for evaluation
;;              and prints the result.  
;;  	        
;;    C-M-x     Evaluates the current function defintion in Mozilla.
;;    C-c C-e   Evaluates the current line.
;;    C-c C-r   Evaluates the current region
;;   	        
;;    C-c C-l   Loads a javascript file.
;;	        
;;    M-.       Finds the source location of function definitions.
;;    M-,       Returns to the origin of the last M-. command.
;;
;;    C-c C-d   Can be used to inspect the Javascript objects.
;;
;; This file also implements a simple debugger.  The debugger is
;; invoked when an error occurs.  The debugger can show the backtrace,
;; the values of local variables in frames and the source location
;; corresponding to frames.  It is also possible to invoke the
;; debugger exlicitly by inserting the "debugger" keyword in javacript
;; code.  Furthermore, the debugger can be invoked on throw
;; statements.  Use C-c C-b t to enable debugging on throw.
;; 
;; Installation:
;; 
;; - First you have to load the server in Mozilla.  You can add the
;; file emacsslave.js to your rc file or make the neccessary
;; modifications to conkeror itself.  The server can be started with
;; M-x start-server in Conkeror.
;;
;; - Put this file into your Emacs load path add something like this
;; to your .emacs:
;;    
;;    (add-hook 'javascript-mode-hook 'js-mode)
;;    (autoload 'js-mode "js-mode" nil t)
;;      
;; - Then open a javascript file and make sure js-mode is enabled.  To
;; connect to Mozilla type M-x js-connect.
;;
;; - Then you can try the commands from above.
;;
;;; 
;;
;; The code should work in Emacs 20.7 and Emacs 21.  XEmacs
;; compatibility is a bit problematic at the moment.

;;; Code:

(eval-and-compile
  (require 'cl)
  (unless (fboundp 'define-minor-mode)
    (require 'easy-mmode)
    (defalias 'define-minor-mode 'easy-mmode-define-minor-mode)))

(require 'pp)

(defvar js-mode-map (make-sparse-keymap))

(define-minor-mode js-mode 
  "Minor mode for interacting with Mozilla.

Use \\[js-connect] to establish a connection with Mozilla.
Some other commands are:
\\{js-mode-map}"
  nil nil ())

(add-to-list 'minor-mode-alist 
	     '(js-mode (js-mozilla-connection " [js]" " js")))

(define-key js-mode-map (kbd "M-C-x") 'js-eval-defun)
(define-key js-mode-map (kbd "C-c :") 'js-interactive-eval)
(define-key js-mode-map (kbd "C-c C-e") 'js-eval-current-line)
(define-key js-mode-map (kbd "C-c C-r") 'js-eval-region)
(define-key js-mode-map (kbd "C-c C-l") 'js-load-file)
(define-key js-mode-map (kbd "M-.") 'js-find-definitions)
(define-key js-mode-map (kbd "M-,") 'js-pop-find-definition-stack)
(define-key js-mode-map (kbd "C-c C-d")   'js-inspect)
(define-key js-mode-map (kbd "C-c C-b t")   'js-toggle-break-on-throw)
(define-key js-mode-map (kbd "C-c C-b C-t") 'js-toggle-break-on-throw)


;;; Generally useful stuff

(defun js-symbol-at-point ()
  "Return the symbol at point as string, or nil."
  (save-restriction
    (save-excursion
      (let ((string (thing-at-point 'symbol)))
        (and string (not (equal string ""))
             (substring-no-properties string))))))

(defun js-read-symbol (prompt &optional query)
  "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
  (cond ((or current-prefix-arg query (not (js-symbol-at-point)))
         (read-from-minibuffer prompt (js-symbol-at-point)))
        (t (js-symbol-at-point))))

(make-variable-buffer-local
 (defvar js-old-window-config nil
   "The window configuration before when before temp-buffer was displayed.
Buffer local in temp-buffers."))



;;; Networking

(defvar js-mozilla-connection nil)

(defun js-connect (port)
  "Connect to Mozilla on PORT."
  (interactive (list (read-from-minibuffer "Port: " "4007" nil t)))
  (let ((socket (js-net-connect port)))
    (setq js-mozilla-connection socket)
    (js-eval `(mozilla_info) (lambda (info) 
			       (message "Connected to: %s" (car info))))))

(defun js-disconnect ()
  "Close the connection to Mozilla."
  (interactive)
  (delete-process js-mozilla-connection)
  (setq js-mozilla-connection nil))

(defun js-connection ()
  (when (not js-mozilla-connection)
    (error "Not connected"))
  js-mozilla-connection)

(defun js-make-net-buffer (name)
  "Make a buffer suitable for a network process."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (when (fboundp 'set-buffer-multibyte)
	(set-buffer-multibyte nil))
      (buffer-disable-undo))
    buffer))

(defun js-net-connect (port)
  (let* ((socket (open-network-stream "Mozilla" nil "localhost" port))
	 (buffer (js-make-net-buffer "*mozilla*")))
    (set-process-buffer socket buffer)
    (set-process-filter socket 'js-net-filter)
    (set-process-sentinel socket 'js-net-sentinel)
    (when (fboundp 'set-process-coding-system)
      (set-process-coding-system socket 'no-conversion 'no-conversion))
    socket))

(defun js-net-send (string connection)
  (let ((string (concat (js-net-encode-length (length string)) string)))
    (process-send-string connection (string-make-unibyte string))))

(defun js-net-encode-length (n)
  (format "%06x" n))

(defun js-net-filter (process string)
  "Accept output from the socket and input all complete messages."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))
    (goto-char (point-min))
    (js-process-available-input)))

(defun js-process-available-input ()
  "Process all complete messages that have arrived from Mozilla."
  (unwind-protect
      (when (js-connection)
	(with-current-buffer (process-buffer (js-connection))
	  (while (js-net-have-input-p)
	    (let ((event (condition-case error (js-net-read)
			   (error (js-net-panic error)))))
	      (save-current-buffer
		(js-dispatch-event event))))))
    (when (js-connection)
      (with-current-buffer (process-buffer (js-connection))
	(when (js-net-have-input-p)
	  (run-at-time 0 nil 'js-process-available-input))))))

(defun js-net-panic (error)
  (message "net-read error: %S" error)
  (let ((string (buffer-string)))
    (ignore-errors
      (js-disconnect)
      (kill-buffer (current-buffer)))
    (ding)
    (sleep-for 2)
    (with-current-buffer (generate-new-buffer "*saved-connecton-buffer*")
      (insert string)
      (error "PANIC!" error))))

(defun js-net-have-input-p ()
  "Return true if a complete message is available."
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (js-net-read-length))))

(defun js-net-read-length ()
  (string-to-number (buffer-substring (point) (+ (point) 6)) 16))

(defun js-net-read ()
  (let* ((length (js-net-read-length))
	 (start (+ 6 (point)))
         (end (+ start length)))
    (let ((string (buffer-substring start end)))
      (prog1 (read string)
        (delete-region (point-min) end)))))

(defun js-net-sentinel (process message)
  (message "Mozilla connection closed unexpectedly: %s" message)
  (when (eq process js-mozilla-connection)
    (setq js-mozilla-connection nil))
  (kill-buffer (process-buffer process)))

(defun js-send (term)
  (js-log-event (list 'send term))
  (js-net-send (js-term-to-string term) (js-connection)))

(defun js-term-to-string (term)
  (etypecase term
    (symbol (concat "'" (symbol-name term) "'"))
    (string (with-temp-buffer
	      (let ((print-escape-nonascii t)
		    (print-escape-newlines t))
		(prin1 term (current-buffer))
		(buffer-string))))
    (number (number-to-string term))
    (cons (concat "[" (mapconcat 'js-term-to-string term ", ") "]"))))

;;; Event logging

(defvar js-log-events t
  "*Log protocol events to the *js-events* buffer.")
 
(defvar js-log-buffer-name "*js-events*"
  "The name of the js event buffer.")

(defun js-log-event (event)
  "Record the fact that EVENT occurred."
  (when js-log-events
    (with-current-buffer (js-events-buffer)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (js-pprint-event event (current-buffer)))
      (goto-char (point-max)))))

(defun js-pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun js-events-buffer ()
  (get-buffer-create js-log-buffer-name))

;;; RPCing

(defvar js-continuations ()
  "An alist of (ID . FUNCTION) functions waiting for results.")

(defvar js-continuation-counter 0
  "Counter to generate serial number for continuations.")

(defun js-dispatch-event (event)
  (js-log-event event)
  (apply (car event) (cdr event)))

(defun js-eval (term cont)
  "Evaluate term in Mozilla and call the function CONT with the result."
  (let ((id (incf js-continuation-counter))
	(cont (lexical-let ((cont cont) (buffer (current-buffer)))
		(lambda (status value)
		  (with-current-buffer (js-buffer-for-eval buffer)
		    (ecase status
		      (ok (funcall cont value))
		      (error (message "Evaluation aborted: %s" value))))))))
    (push (cons id cont) js-continuations)
    (js-send `(eval_for_emacs ,term ,id))))

(defun js-buffer-for-eval (saved-buffer)
  (let ((alive (buffer-name saved-buffer)))
    (cond (alive saved-buffer)
	  (t (generate-new-buffer (format "*killed %s*" saved-buffer))))))
					 
(defun js-return (id status value)
  (let ((rec (assq id js-continuations)))
    (cond (rec (setq js-continuations (delete rec js-continuations))
	       (funcall (cdr rec) status value))
	  (t
	   (error "Unexpected reply: %S %S" id value)))))

(defvar js-wait-tags ())

(defun js-eval-wait (term)
  "Evaluate TERM in Mozilla and wait until the result is available."
  (let* ((id (incf js-continuation-counter))
	 (tag (gensym))
	 (unwind (lexical-let ((tag tag))
		   (lambda (status value)
		     (unless (memq tag js-wait-tags)
		       (error "Wait-tag not active: %S." tag))
		     (throw tag (list status value))))))
    (push (cons id unwind) js-continuations)
    (js-send `(eval_for_emacs ,term ,id))
    (destructuring-bind (status value) (js-wait tag)
      (ecase status
	(ok value)
	(error (error "Syncronous evaluation aborted: %s" value))))))

(defun js-wait (wait-tag)
  (let ((js-wait-tags (cons wait-tag js-wait-tags))
	(debug-on-quit t)
	(inhibit-quit nil))
    (catch wait-tag
      (while t (accept-process-output nil 0 10000)))))

(defun js-show-result (result)
  (message "%S" result))
    
;;; Evaluation Commands

(defun js-beginning-of-defun ()
  "Move to the beginning of the current function.
Point is placed before foo in the folling example:

foo = function (bar)
{
 ...
}"
  (interactive)
  (c-beginning-of-defun 1)
  (beginning-of-line)
  (when (looking-at "[ \t]*{")
    (forward-line -1)))

(defconst js-identifier
  "[a-zA-Z_\\$][a-zA-Z0-9_\\$]*"
  "Expression for matching Javascript identifiers.")

(defun js-hack-defun (string)
  "If STRING looks like function <name> { convert it to <name> = function {"
  (cond ((string-match (concat "^function\\s +\\(" js-identifier "\\)\\s *(")
		       string)
	 (format "%s = function %s" (match-string 1 string)
		 (substring string (position ?\( string))))
	(t string)))
		  
(defun js-eval-defun ()
  "Evaluate the current function."
  (interactive)
  (save-excursion
    (c-end-of-defun)
    (let ((end (point)))
      (js-beginning-of-defun)
      (let ((string (buffer-substring-no-properties (point) end)))
	(js-eval `(interactive_eval ,(js-hack-defun string))
		 (lambda (result) (message "%s" result)))))))

(defun js-interactive-eval (string &optional insertp)
  (interactive "Mjavascript eval: \nP")
  (cond (insertp 
	 (beginning-of-line 2)
	 (js-eval `(interactive_eval ,string) #'insert))
	(t 
	 (js-eval `(interactive_eval ,string) 
		  (lambda (result) (message "%s" result))))))

(defun js-eval-region (start end &optional insertp)
  "Eval region in Mozilla.  
Insert the result in the current buffer when called with a prefix argument."
  (interactive "r\nP")
  (js-interactive-eval (buffer-substring-no-properties start end) insertp))

(defun js-eval-current-line (&optional insertp)
  "Eval the current line.  See `js-eval-region'."
  (interactive "P")
  (js-eval-region (line-beginning-position) (line-end-position) insertp))

(defun js-load-file (filename)
  "Load the javascript file FILENAME in Mozilla."
  (interactive (list (read-file-name "Load file: " nil nil
				     nil (file-name-nondirectory 
					  (buffer-file-name)))))
  (let ((url (concat "file:" (expand-file-name filename))))
    (js-eval `(load_file_for_emacs ,url)
	     (lambda (message)
	       (message "Loaded: %s" message)))))


;;;; Find definitions

(defvar js-find-definition-history-ring (make-ring 20)
  "History ring recording the definition-finding \"stack\".")

(defun js-push-definition-stack ()
  "Add MARKER to the edit-definition history stack.
If MARKER is nil, use the point."
  (ring-insert-at-beginning js-find-definition-history-ring (point-marker)))

(defun js-pop-find-definition-stack ()
  "Pop the edit-definition stack and goto the location."
  (interactive)
  (unless (ring-empty-p js-find-definition-history-ring)
    (let* ((marker (ring-remove js-find-definition-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
        ;; If this buffer was deleted, recurse to try the next one
        (js-pop-find-definition-stack)))))

(defun js-find-definitions (name)
  "Lookup the definition of the symbol at point."
  (interactive (list (js-read-symbol "Name: ")))
  (js-eval `(find_definitions ,name)
	   (lexical-let ((name name))
	     (lambda (defs) (js-show-definitions name defs)))))

(defun js-show-definitions (name defs)
  (unless (not defs) 
    (js-push-definition-stack))
  (cond ((null defs) (message "No definitions for: %s" name) (ding))
	((null (cdr defs)) (js-goto-definition (car defs)))
	(t (js-display-multiple-definitions defs))))

(defun js-goto-definition (definition &optional other-window)
  (destructuring-bind (filename line) definition
    (let ((buffer (js-find-source-buffer filename)))
      (cond (other-window (switch-to-buffer-other-window buffer))
	    (t (switch-to-buffer buffer)))
      (goto-line line))))

(defun js-find-source-buffer (filename)
  (cond ((string-match "^file:" filename)
	 (find-file-noselect (substring filename (length "file:"))))
	((string-match "^\\(http\\|chrome\\):" filename)
	 (or (get-buffer filename)
	     (with-current-buffer (get-buffer-create filename)
	       (insert (js-eval-wait `(load_source ,filename)))
	       (not-modified)
	       (setq buffer-read-only t)
	       (setq buffer-file-name filename)
	       (normal-mode)
	       (js-mode 1)
	       (current-buffer))))
	(t (error "cannot resolve url: %s" filename))))

(defun js-display-multiple-definitions (defs)
  "Display a buffer to browse the list of definitions DEFS." 
  (with-current-buffer (get-buffer-create "*definitions*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq js-old-window-config (current-window-configuration))
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "RET") 'js-show-definition-other-window)
      (define-key keymap (kbd "SPC") 'js-pop-to-definition)
      (define-key keymap [?q] 'js-quit)
      (use-local-map keymap)
      (dolist (def defs)
	(destructuring-bind (file line) def
	  (let ((start (point)))
	    (insert (format "%s:%d\n" file line))
	    (add-text-properties start (1- (point)) `(definition ,def)))))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (let ((w (select-window (display-buffer (current-buffer)))))
	(shrink-window-if-larger-than-buffer w)))))

(defun js-quit ()
  "Kill the current buffer and restore the old window configuration."
  (interactive)
  (let ((buffer (current-buffer)))
    (set-window-configuration js-old-window-config)
    (kill-buffer buffer)))

(defun js-property-at-point (prop)
  (or (get-text-property (point) prop)
      (error "No %s at point" prop)))

(defun js-pop-to-definition (definition)
  "Jump to the definition at point and close the current window."
  (interactive (list (js-property-at-point 'definition)))
  (delete-window)
  (js-goto-definition definition))

(defun js-show-definition-other-window (definition)
  "Display the defintion at point in window."
  (interactive (list (js-property-at-point 'definition)))
  (save-selected-window
    (js-goto-definition definition t)
    (let ((overlay (make-overlay (line-beginning-position) 
				 (line-end-position))))
      (overlay-put overlay 'face 'secondary-selection)
      (run-with-timer 0.3 nil 'delete-overlay overlay))))
 
;;; Debugger

(defvar js-debugger-level 0)

(define-derived-mode js-debugger-mode fundamental-mode "jsdbg"
  "Mode to inspect backtraces.  
See also `js-toggle-break-on-throw'.

\\{js-debugger-mode-map}"
  (erase-buffer)
  (set (make-local-variable 'truncate-lines) t)
  (set-syntax-table c++-mode-syntax-table))

(let ((m js-debugger-mode-map))
  (define-key m "v" 'js-debugger-show-source)
  (define-key m "q" 'js-debugger-quit)
  (define-key m "t" 'js-debugger-toggle-locals)
  (define-key m "i" 'js-debugger-inspect-variable))

(defun js-debugger-buffer ()
  (get-buffer-create "*js-debugger*"))

(defun js-debugger-activate (level)
  (with-current-buffer (js-debugger-buffer)
    (unless (equal js-debugger-level level)
      (with-lexical-bindings (level)
        (js-eval `(debugger_info_for_emacs 0 1)
		 (lambda (info)
		   (apply #'js-debugger-setup level info)))))))

(defun js-debugger-setup (level message backtrace)
  (with-current-buffer (js-debugger-buffer)
    (unless (equal js-debugger-level level)
      (setq buffer-read-only nil)
      (js-debugger-mode)
      (js-mode 1)
      (unless js-old-window-config
        (setq js-old-window-config  (current-window-configuration)))
      (setq mode-name (format "js-debugger[%d]" js-debugger-level))
      (insert message "\n")
      (insert "\nBacktrace: \n")
      (save-excursion (js-insert-backtrace backtrace))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer))
      (when (and js-wait-tags
                 (y-or-n-p "Enter recursive edit? "))
        (message "Entering recursive edit..")
        (recursive-edit)))))

(defun js-insert-backtrace (backtrace)
  (mapc #'js-insert-frame backtrace))

(defun js-insert-frame (frame)
  (destructuring-bind (number text) frame
    (let ((start (point-marker)))
      (set-marker-insertion-type start nil)
      (insert text)
      (let ((end (point-marker)))
	(insert "\n")
	(set-marker-insertion-type end t)
	(add-text-properties start end 
			     (list 'frame number 
				   'text text
				   'start start 'end end))))))

(defun js-debugger-show-source (frame)
  "Shoue the source buffer for the frame at point."
  (interactive (list (js-property-at-point 'frame)))
  (js-eval `(frame_source_location ,frame)
	   (lambda (location)
	     (destructuring-bind (file baseline line) location
	       (js-show-definition-other-window  (list file line))))))

(defun js-debugger-quit ()
  "Exit from the debugger and continue execution."
  (interactive)
  (js-eval `(debugger_quit) 
	   (lambda (x) 
	     (message "%S" x)
	     (let ((buffer (current-buffer))) 
	       (set-window-configuration js-old-window-config)
	       (kill-buffer buffer)))))

(defun js-debugger-toggle-locals (frame)
  "Show the local variables for the current frame."
  (interactive (list (js-property-at-point 'frame)))
  (let ((inhibit-read-only t))
      (destructuring-bind (text start end detailsp) 
	  (loop for p in '(text start end detailsp)
		collect (get-text-property (point) p))
	(cond ((not detailsp)
	       (let ((locals (js-eval-wait `(frame_locals ,frame))))
		 (goto-char end)
		 (let ((new-start (point)))
		   (insert-and-inherit text "\n")
		   (loop for l in locals for i from 0 
			 do (js-insert-line-with-props (concat "     " l )
						       `(local-var ,i)))
		   (delete-region start new-start)
		   (put-text-property start end 'detailsp t))))
	      (t
	       (goto-char end)
	       (let ((new-start (point)))
		 (insert-and-inherit text)
		 (delete-region start new-start)
		 (put-text-property start end 'detailsp nil))))
	(goto-char start))))

(defun js-insert-line-with-props (text props)
  (let ((start (point)))
    (insert-and-inherit text)
    (let ((end (point)))
      (insert-and-inherit "\n")
      (add-text-properties start end props))))

(defun js-debugger-inspect-variable (frame var)
  "Inspect the variable at point."
  (interactive (list (js-property-at-point 'frame)
		     (js-property-at-point 'local-var)))
  (js-eval `(inspect_local_variable ,frame ,var)
	   'js-open-inspector))

(defun js-toggle-break-on-throw ()
  "Enable or disable debugging on throws."
  (interactive)
  (js-eval `(toggle_break_on_throw) (lambda (m) (message "%s" m))))


;;; Inspector

(defun js-inspect (string)
  "Evalute STRING and inspect the result."
  (interactive (list (read-from-minibuffer "Inspect value (evaluated): ")))
  (js-eval `(init_inspector ,string) 'js-open-inspector))

(define-derived-mode js-inspector-mode fundamental-mode "Javascript-Inspector"
  "Mode to inspect Javascript objects. 

\\{js-debugger-mode-map}"
  (set-syntax-table java-mode-syntax-table)
  (set (make-local-variable 'truncate-lines) t)
  (setq buffer-read-only t))

(let ((m js-inspector-mode-map))
  (define-key m (kbd "RET") 'js-inspect-part)
  (define-key m "l" 'js-inspector-last)
  (define-key m "n" 'js-inspector-next)
  (define-key m "a" 'js-inspector-apply-part)
  (define-key m "q" 'js-quit-inspector))

(defun js-inspector-buffer ()
  (or (get-buffer "*Javascript Inspector*")
      (with-current-buffer (get-buffer-create "*Javascript Inspector*")
	(setq js-inspector-mark-stack '())
	(js-inspector-mode)
	(js-mode 1)
        (setq js-old-window-config (current-window-configuration))
	(current-buffer))))

(defun js-open-inspector (parts &optional point)
  (with-current-buffer (js-inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (destructuring-bind (printed type lines) parts
	(insert printed "\n")
	(insert "   [type: " type "]\n\n")
	(save-excursion
	  (loop for (label text) in lines for i from 0 
		do (js-insert-line-with-props (concat label ": " text)
					      `(part ,i))))
	(pop-to-buffer (current-buffer))
	(when point 
	  (goto-char (min (point-max) point)))))))

(defun js-inspect-part (id)
  "Inspect the slot at point."
  (interactive (list (js-property-at-point 'part)))
  (js-eval `(inspect_nth_part ,id) 'js-open-inspector)
  (push (point) js-inspector-mark-stack))

(defun js-inspector-next ()
  "Inspect the next object in the history."
  (interactive)
  (js-eval `(inspector_next)
	   (lambda (parts) 
	     (cond ((eq parts 'false)
		    (message "No next object")
		    (ding))
		   (t
		    (push (point) js-inspector-mark-stack)
		    (js-open-inspector parts))))))

(defun js-inspector-last ()
  "Inspect the previous object."
  (interactive)
  (js-eval `(inspector_pop)
	   (lambda (result)
	     (cond ((eq result 'false)
		    (message "No previous object")
		    (ding))
		   (t
		    (let ((point (pop js-inspector-mark-stack)))
		      (js-open-inspector result point)))))))

(defun js-inspector-apply-part (number &optional args)
  "Call the function at the current slot and inspect the result.
Call this command with a prefix argument to supply arguments."
  (interactive (list (js-property-at-point 'part)
		     (if current-prefix-arg
			 (read-from-minibuffer "Argument vector: " "[]"))))
  (let ((args (or args "[]")))
    (js-eval `(inspector_apply_part ,number ,args) 'js-open-inspector)
    (push (point) js-inspector-mark-stack)))

(defun js-quit-inspector ()
  "Close the inspector."
  (interactive)
  (js-eval `(quit_inspector)
	   (lambda (x)
	     (set-window-configuration js-old-window-config)
	     (kill-buffer (current-buffer)))))

;;; Portability kludges

(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (string &optional start end)
    (let* ((start (or start 0))
	   (end (or end (length string)))
	   (string (substring string start end)))
      (set-text-properties start end nil string)
      string)))

(unless (fboundp 'string-make-unibyte)
  (defalias 'string-make-unibyte #'identity))

;;;

(run-hooks 'js-load-hook)

(provide 'js-mode)

;;; js-mode.el ends here
