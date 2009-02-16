;;;
;;; Chunking suppot functions
;;;
;;; Definition:
;;; - "chunk" is a complete request to the server or the
;;; complete response from the server. It does not include any
;;; communication-level data such as a seperator.
;;;
;;; Note: currently multi-chunking (more then one chunk in the queue)
;;; is not supported and it will display a message. Results are not
;;; predictable.
;;;

(require 'el4r-support)
(require 'cl) ;; push/pop

;; FIX these should be buffer-local?
(setq el4r-chunks ())

(defun el4r-chunk-ready-p ()
  "Non-nil if a chunk is ready."
  (/= (length el4r-chunks) 0))

(defun el4r-add-chunk (chunk)
  "Add a chunk to the end chunk FIFO queue. The chunk should already be a
list."
  (el4r-log-info ">> Chunk in: [[%s]]"
                 (truncate-string (prin1-to-string chunk) 120))
  (when el4r-chunks
    (el4r-log-warn "Multiple chunks in buffer."))
  (push chunk el4r-chunks))

;;;
;;; This entire function is made obsolete by switching to returning a
;;; single sexpr.
;;;
;; (defun el4r-parse-chunk (data)
;;   "Turns some DATA into a chunk. Returns (HEADER . DATA).  HEADER is a
;; sexpr.  DATA is a string.  Signals an error on a malformed chunk."
;;   (if (string-match ";" chunk)
;;       (let* ((read-data (read-from-string chunk))
;;              (valid-header (and read-data
;;                                 (eq (cdr read-data)
;;                                     (match-beginning 0))))
;;              (data (substring chunk (match-end 0))))
;;         (if valid-header
;;             (cons (car read-data) data)
;;           (error (format "Invalid header: [[%s]]"
;;                          (let ((header-str
;;                                 (substring chunk 0 (match-beginning 0))))
;;                            header-str)))))
;;     (error "Invalid chunk: missing data section")))

(defun el4r-reset-chunks ()
  "Remove all chunks"
  (setq el4r-chunks ()))

(defun el4r-next-chunk ()
  "Returns the next chunk or nil is there are no chunks ready.
The chunk queue is altered."
  (if el4r-chunks
      (let ((chunks-after-pop (1- (length el4r-chunks))))
        (el4r-log-info "<< Chunk read: chunks-left=%d" chunks-after-pop))
    (el4r-log-info "<< Chunk read failed: no chunks left"))
  (pop el4r-chunks))


(provide 'el4r-chunks)