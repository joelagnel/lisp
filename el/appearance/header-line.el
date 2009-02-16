(defconst my-header "   Ruby { overwhelming simplicity }")

(setq header-line-format
      '(:eval (substring my-header
               (min (length my-header)
                (window-hscroll)))))


(setq header-line t)
(provide 'header-line)

;;  (setq header-line-format
;;           '(:eval
;;             (concat (and (display-graphic-p)
;;                          (concat " "  ;; fringe
;;                                  (and (eq 'left (frame-parameter nil 'vertical-scroll-bars))
;;                                       "  ")))  ;; left scrollbar
;;                     "some header text")))
