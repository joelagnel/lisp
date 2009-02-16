;; gse-underline-previous-line.el
;;   Summary:     Underline the previous line with dashes.
;;   Author:      Scott Evans <gse@antisleep.com>
;;   Home:        http://www.antisleep.com/elisp
;;   Time-stamp:  <2004.12.23 00:11:14 gse>
;;
;; Installation:
;;   (require 'gse-underline-previous-line)
;;
;;---------------------------------------------------------------------------
;; Change Log
;; ----------
;; 2004.12.23 Created.
;;---------------------------------------------------------------------------

(defun gse-underline-previous-line ()
  "Underline the previous line with dashes."
  (interactive)
  (let ((start-pos (point))
        (start-col nil)
        (end-col nil))
    (beginning-of-line 0)
    (if (re-search-forward "[^ ]" (save-excursion (end-of-line) (point)) t)
        (progn
          (setq start-col (- (current-column) 1))

          (end-of-line)
          (re-search-backward "[^ ]" nil t)
          (setq end-col (current-column))

          ;; go to next line and insert dashes
          (beginning-of-line 2)
          (insert
           (make-string start-col ?\ )
           (make-string (+ 1 (- end-col start-col)) ?-)
           "\n")
          )
      (goto-char start-pos)
      (error "No text on previous line"))
    ))

(global-set-key "\C-c-" 'gse-underline-previous-line)
(global-set-key "\C-c_" 'gse-underline-previous-line)

;;---------------------------------------------------------------------------

(provide 'gse-underline-previous-line)
