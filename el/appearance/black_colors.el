
;; Make things black

(set-face-attribute 'default nil
 		    :width 'condensed
;; 		    :height 110
;; 		    :bold nil
		    :foreground "gray75"
		    :background "black")


(set-face-background 'fringe "black")

(set-face-attribute 'mode-line nil
		    :family "neep"
		    :height 90
		    :width 'condensed
		    :box nil
		    :bold nil
		    :overline t
		    :underline nil
		    :foreground "gray70"
		    :background "#112233")

(set-face-attribute 'header-line nil
		    :family "helvetica"
		    :box nil 
		    :bold nil
		    :overline nil
		    :underline nil
		    :foreground "gray90"
		    :background "#000044")



(set-face-background 'region "gray")
(set-face-foreground 'region "black")

(set-face-background 'cursor "orange")


(set-face-attribute 'underline nil
		    :bold nil
		    :underline t)

;; Info Faces
(require 'info)
(set-face-attribute 'info-xref nil
		    :bold nil
		    :underline nil
		    :foreground "steelblue")


;; Speedbar
(require 'speedbar)
(set-face-attribute 'speedbar-button-face nil
		    :foreground "slategray")
(set-face-attribute 'speedbar-directory-face nil
		    :family "helvetica"
		    :foreground "slategray3")

(set-face-attribute 'speedbar-file-face nil
		    :family "helvetica"
		    :height 1.0
		    :foreground "lemonchiffon4")

(set-face-attribute 'speedbar-tag-face nil
		    :family "helvetica"
		    :height 0.8
		    :foreground "snow4")

(set-face-attribute 'speedbar-highlight-face nil
		    :background "snow4"
		    :foreground "black")

(set-face-attribute 'speedbar-selected-face nil
		    :family "helvetica"
		    :bold t
		    :height 1.0
		    :underline nil
		    :foreground "snow3")


;; Font locking

(set-face-foreground 'font-lock-warning-face "red3")
(set-face-foreground 'font-lock-constant-face "steelblue")
(set-face-attribute 'font-lock-keyword-face nil
		    :bold nil
		    :foreground "lemonchiffon3")
(set-face-foreground 'font-lock-string-face "snow4")
(set-face-foreground 'font-lock-function-name-face "white")
(set-face-attribute 'font-lock-comment-face nil
		    :family "neep"
		    :width 'condensed
		    :bold nil
		    :height 1.0
		    :foreground "darkorange3")

(set-face-attribute 'font-lock-function-name-face nil
		    :family "helvetica"
		    :bold t
		    :height 1.0
		    :foreground "slategray")

(set-face-attribute 'font-lock-type-face nil
		    :bold t
		    :foreground "slategrey")


(set-face-attribute 'font-lock-builtin-face nil
		    :bold nil
		    :foreground "lightslategrey")


;; Gnus
;; (require 'gnus)
;; (set-face-attribute 'message-header-to-face nil
;; 		    :foreground "grey75")


;; (set-face-attribute 'gnus-header-name-face nil
;; 		    :foreground "gray30")

;; (set-face-attribute 'gnus-header-from-face nil
;; 		    :family "helvetica"
;; 		    :bold t
;; 		    :height 1.3
;; 		    :foreground "slategrey")

;; (set-face-attribute 'gnus-header-subject-face nil
;; 		    :family "helvetica"
;; 		    :bold t
;; 		    :height 1.0
;; 		    :foreground "goldenrod")

;; (set-face-attribute 'gnus-summary-normal-read-face nil
;; 		    :bold nil
;; 		    :foreground "gray50")

;; (set-face-attribute 'gnus-summary-normal-unread-face nil
;; 		    :bold nil
;; 		    :foreground "steelblue")

;; (set-face-attribute 'gnus-summary-normal-ticked-face nil
;; 		    :bold nil
;; 		    :foreground "goldenrod")

;; (set-face-attribute 'gnus-group-news-1-face nil
;; 		    :bold nil
;; 		    :foreground "slategrey")

;; (set-face-attribute 'gnus-group-news-2-face nil
;; 		    :bold nil
;; 		    :foreground "steelblue2")

;; (set-face-attribute 'gnus-group-news-2-empty-face nil
;; 		    :bold nil
;; 		    :foreground "steelblue")

;; (set-face-attribute 'gnus-group-mail-3-face nil
;; 		    :bold nil
;; 		    :foreground "snow4")

;; (set-face-attribute 'gnus-group-news-3-face nil
;; 		    :bold nil
;; 		    :foreground "goldenrod")



;; (set-face-attribute 'message-header-subject-face nil
;; 		    :bold nil
;; 		    :foreground "gold")

;; (set-face-attribute 'gnus-cite-face-1 nil
;; 		    :height 1.0
;; 		    :foreground "gray60")

;; (set-face-attribute 'gnus-cite-face-2 nil
;; 		    :height 1.0
;; 		    :foreground "gray40")

;; (set-face-attribute 'gnus-cite-face-3 nil
;; 		    :height 1.0
;; 		    :foreground "gray30")
