;;; cus-new-gui.el --- Testing a new GUI for Emacs Customize

;; Author: Lennart Borgman (lennart dot borgman dot 073 at student dot lu dot se)
;; Version: 0.5

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; 
;; This is a proposal for a new (or rather partly rewritten) GUI for
;; Emacs Customize.  This is (I hope) working code for Emacs
;; 21.3.1. (Though the code is code under construction!)
;; 
;; To use this file just require or eval it. Then use customize (from
;; the menus for example). Look in the menu Customize - View and test
;; the different settings there.
;;
;; The background to this work is some discussions about the current
;; Customize interface on Emacs Devel and private discussions with
;; Drew Adams.  There have also been comments on the Emacs help list
;; where people have found the current GUI difficult to
;; understand. That was also my own experience in the beginning.
;;
;; I have therefore tried to change the layout and menus. The
;; customize GUI resembles what is normally called options pages in
;; other programs. My goal has been to follow what I believe are
;; normal guidelines for GUIs like this as far as possible.  The
;; audience is of course different for a program like Emacs.  In
;; addition to this the usual types of widgets are not available in
;; Emacs yet (because of its nature and multiplatform goal).
;;
;; Please critize my attempts here but try to keep the small facts
;; above in mind.  What I have done is mainly:
;;
;; - Reordering of buttons (first used comes first).
;;
;; - Added possibilities to hide/show parts of the informations (to
;;   make it easier to get an overview).
;;
;; - In an attempt to simplify the look I have made a distinction
;;   between buttons that just hides/shows something, links and those that
;;   acts on the variables beeing customized.
;;
;; - Links of different kinds are now blue underlined like on web pages.
;;
;; - Buttons for hide/show now have a style that I believe is rather
;;   common on web pages (blue underlined with small arrows).
;;
;; - Buttons what pops up menus (eq the state button and choice buttons)
;;   have a small arrow (but I can not find a good char to use for this).
;;
;; - The buttons for saving are now enabled/disabled according to the
;;   state of the option items in the buffer.
;;
;; - There are menu entries doing the same things as the buttons. 
;;
;; - There are also menu entries working on just the current option item.
;;
;; - I have colorized the small state button in a way I believe is
;;   more in accordance with how colors normally are used.
;;
;; - The small state button also acts like an image button.
;;
;; - I have tried to use colors to give a better overview over the
;;   structure in the buffer. My pc does not give the best colors so
;;   you might very well dislike my choices here. Please suggest
;;   better ones!
;;
;; - Subgroups are now shown in the same buffer in a manner I suppose
;;   was intended (indented).
;;
;; - I have tried to simplify some messages to the user.


;;; Bugs:
;;
;; - Face have not been rewritten yet.


;;; History:
;; 
;; 2005-01-25 Corrected an error in nested group style. Internal
;;            button list where not updated. (Quick fix only.)
;;            


;;; Code:

(require 'cus-edit) ;; for mode map

(defun 1plus (num) num) ;(1+ num))

(defun custom-insert-border(s)
  (put-text-property 0 (length s) 'face 'custom-border2 s)
  (insert s))


;;; Defaults
(setq custom-buffer-style 'brackets)

(face-spec-set 'custom-group-tag-face-1
;;(defface custom-group-tag-face-1
  `((((class color)
      (background dark))
     (:foreground "pink" :bold t :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "red" :bold t :height 1.2 :inherit variable-pitch))
    (t (:bold t)))
  );;"Face used for group tags."
  ;;:group 'custom-faces)

;;(defface custom-group-tag-face
(face-spec-set 'custom-group-tag-face
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :height 1.2))
    (((class color)
      (background light))
     (:foreground "OrangeRed4" :bold t :height 1.2 :inherit variable-pitch))
    (t (:bold t)))
  );;"Face used for low level group tags."
  ;;:group 'custom-faces)

(defface custom-border2
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :inherit default))
    (((class color)
      (background light))
     (:foreground "OrangeRed4" :bold t :inherit default))
    (t (:bold t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)

(defface custom-h1
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :height 1.5 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t :height 1.5 :inherit variable-pitch))
    (t (:bold t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)

(face-spec-set 'custom-h1
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :height 1.5 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "DarkOliveGreen" :bold t :height 1.5 :inherit variable-pitch))
    (t (:bold t))))

(face-spec-set 'custom-variable-tag-face
;;(defface custom-variable-tag-face
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "blue4" :bold t :height 1.2 :inherit variable-pitch))
    (t (:bold t)))
  );;"Face used for unpushable variable tags."
  ;;:group 'custom-faces)


(face-spec-set 'custom-invalid-face ;value frame)))
;;(defface custom-invalid-face
	       '((((class color))
		  (:foreground "yellow" :background "red"))
		 (t
		  (:bold t :italic t :underline nil)))
  );;"Face used when the customize item is invalid."
  ;;:group 'custom-magic-faces)

(face-spec-set 'custom-rogue-face ;value frame)))
;;(defface custom-rogue-face
	       '((((class color))
		  (:foreground "pink" :background "black"))
		 (t
		  (:underline t)))
  );;"Face used when the customize item is not defined for customization."
  ;;:group 'custom-magic-faces)

(face-spec-set 'custom-modified-face ;value frame)))
;;(defface custom-modified-face
	       '((((class color))
		  (:foreground "black" :background "sienna1"))
		 (t
		  (:italic t :bold)))
  );;"Face used when the customize item has been modified."
  ;;:group 'custom-magic-faces)

(face-spec-set 'custom-set-face ;value frame)))
;;(defface custom-set-face
	       '((((class color))
		  (:foreground "black" :background "yellow"))
		 (t
		  (:italic t)))
  );;"Face used when the customize item has been set."
  ;;:group 'custom-magic-faces)

(face-spec-set 'custom-changed-face ;value frame)))
;;(defface custom-changed-face
	       '((((class color))
		  (:foreground "black" :background "sienna1"))
		 (t
		  (:italic t)))
	       );;"Face used when the customize item has been changed."
;;:group 'custom-magic-faces)

(face-spec-set 'custom-saved-face ;value frame)))
;;(defface custom-saved-face
	       '((((class color))
		  (:foreground "black" :background "LawnGreen"))
		 (t (:underline nil)))
	       );;"Face used when the customize item has been saved."
;;:group 'custom-magic-faces)

(defface custom-unmodified-face '((((class color))
				   (:foreground "black" :background "SkyBlue1")))
  "Face used for items that are not customized at all."
  :group 'custom-magic-faces)

(defface custom-hidden-face '((((class color))
				   (:foreground "black" :background "DarkKhaki")))
  "Face used for items that are not customized at all."
  :group 'custom-magic-faces)

(defcustom custom-magic-show nil
  "If non-nil, show textual description of the state.
If `long', show a full-line description, not just one word."
  :type '(choice (const :tag "None" nil)
		 (other :tag "Short" short)
		 (const :tag "Long" long))
  :group 'custom-buffer)

(defcustom custom-display-option-doc nil
  "Display option and face documentation."
  ;;:version "21.1.50"
  :type 'boolean
  :group 'custom)

;;; Sorry, setting these for now:

(setq custom-magic-show nil)
(setq custom-buffer-done-function 'custom-quit-window-kill)

;;; Quitting

(defun custom-quit-window-kill(&optional buffer)
  (quit-window t))
(defun custom-quit-window(&optional buffer)
  (quit-window nil))
(defcustom custom-buffer-done-function 'custom-quit-window-kill
  "*Function called to remove a Custom buffer when the user is done with it.
Called with one argument, the buffer to remove."
  :type '(choice
	  (function-item :tag "Quit window, kill buffer" custom-quit-window-kill)
	  (function-item :tag "Quit window, bury buffer" custom-quit-window)
	  (function-item :tag "Bury buffer" custom-bury-buffer)
	  (function-item :tag "Kill buffer" kill-buffer)
	  (function :tag "Other"))
  :version "21.1"
  :group 'custom-buffer)

;;; Basic

(define-widget 'choice 'menu-choice
  "A union of several sexp types."
  :tag "Choice"
  :format "%{%t%}: %[ Choose » %] %v"
  ;;:format "%{%t%}: (%v) %[ Choose » %]"
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  :prompt-value 'widget-choice-prompt-value)


;;(define-widget 'visibility 'item
(define-widget 'value-visibility 'visibility
  "An indicator and manipulator for hidden values."
  :format "%[%v%]"
  :button-face '(underline
		 (foreground-color . "blue"))
  :button-prefix ""
  :button-suffix ""
  :on "«"
  :off "Show value »"
  )
(define-widget 'doc-visibility 'value-visibility
  "An indicator and manipulator for hidden documentation part."
  ;;:format "%[%v%]"
  ;;:button-face '(underline (foreground-color . "blue"))
  ;;:button-prefix ""
  ;;:button-suffix ""
  ;;:on "«"
  :off "More »"
  )
(define-widget 'group-visibility 'value-visibility
  "An indicator and manipulator for hidden groups."
  :on "Hide «"
  :off "Show »"
  :action 'custom-group-toggle-action
  )
(defun custom-group-toggle-action (widget &optional event)
  (widget-toggle-action widget event)
  (custom-buffer-custom-fields))
(define-widget 'custom-group-visibility 'value-visibility
  "An indicator and manipulator for hidden group contents."
  :on "Hide «"
  :off "Show here »"
  :create 'custom-group-visibility-create)

(define-widget 'boolean 'toggle
  "To be nil or non-nil, that is the question."
  :tag "Boolean"
  :prompt-value 'widget-boolean-prompt-value
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  ;;:format "%{%t%}: %[Toggle%]  %v\n"
  :format "%{%t%}: %v %[ Toggle %]\n"
  ;;:on "on (non-nil)"
  ;;:off "off (nil)"
  :on "on"
  :off "off"
  :help-echo "Toggle value on/off."
  )

;; (define-widget 'boolean 'checkbox
;;   "To be nil or non-nil, that is the question."
;;   :tag "Boolean"
;;   :prompt-value 'widget-boolean-prompt-value
;;   :button-prefix 'widget-push-button-prefix
;;   :button-suffix 'widget-push-button-suffix
;;   :format "%{%t%}: %[Toggle%]  %v\n"
;;   :on "on (non-nil)"
;;   :off "off (nil)")

(defun widget-before-change (from to)
  "This is how an item change its state to `modified' when beeing edited."
  (unless inhibit-read-only
    (let ((from-field (widget-field-find from))
	  (to-field (widget-field-find to)))
      (cond ((not (eq from-field to-field))
	     (add-hook 'post-command-hook 'widget-add-change nil t)
	     (signal 'text-read-only
		     '("Change should be restricted to a single field")))
	    ((null from-field)
	     (add-hook 'post-command-hook 'widget-add-change nil t)
	     ;; 	     (signal 'text-read-only
	     ;; 		     '("Attempt to change text outside editable field")))
	     (signal 'text-read-only
		     '("Please use the buttons, links and editable fields!")))
	    (widget-field-use-before-change
	     (widget-apply from-field :notify from-field))))))


(defface custom-variable-tag-face
  `((((class color)
      (background dark))
     (:foreground "light blue" :bold t :height 1.0 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "blue" :bold t :height 1.0 :inherit variable-pitch))
    (t (:bold t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)




;;; Building

(defun widget-documentation-string-value-create (widget)
  ;; Insert documentation string.
  (let ((doc (widget-value widget))
	(indent (widget-get widget :indent))
	(shown (widget-get (widget-get widget :parent) :documentation-shown))
	(start (point)))
    (if (string-match "\n" doc)
	(let ((before (substring doc 0 (match-beginning 0)))
	      (after (substring doc (match-beginning 0)))
	      button)
	  (insert before ?\ )
	  (widget-documentation-link-add widget start (point))
	  (setq button
		(widget-create-child-and-convert
		 widget 'doc-visibility
		 :help-echo "Show or hide rest of the documentation."
		 :off "More »"
		 ;;:on "Less «"
		 :on "«"
		 :always-active t
		 :action 'widget-parent-action
		 shown))
	  (when shown
	    (setq start (point))
	    (when (and indent (not (zerop indent)))
	      (insert-char ?\  indent))
	    (insert after)
	    (widget-documentation-link-add widget start (point)))
	  (widget-put widget :buttons (list button)))
      (insert doc)
      (widget-documentation-link-add widget start (point))))
  (insert ?\n))


(defmacro custom-info-link(txt url)
  (list 'widget-create
	''info-link
	;;':button-face '(list 'quote 'underline
	':button-face '(list 'underline
			     '(foreground-color . "blue"))
	':button-prefix ""
	':button-suffix ""
	':tag txt
	url)
  )
  

(defun custom-add-parent-links (widget &optional initial-string)
  "Add \"Parent groups: ...\" to WIDGET if the group has parents.
The value if non-nil if any parents were found.
If INITIAL-STRING is non-nil, use that rather than \"Parent groups:\"."
  (let ((name (widget-value widget))
	(type (widget-type widget))
	(buttons (widget-get widget :buttons))
	(start (point))
	found)
    (insert (or initial-string "\nParent groups:"))
    (mapatoms (lambda (symbol)
		(let ((entry (assq name (get symbol 'custom-group))))
		  (when (eq (nth 1 entry) type)
		    (insert " ")
		    (push (widget-create-child-and-convert
			   widget 'custom-group-link
			   :tag (custom-unlispify-tag-name symbol)
			   :button-face '(underline
					  (foreground-color . "blue"))
			   :button-prefix ""
			   :button-suffix ""
			   symbol)
			  buttons)
		    (setq found t)))))
    (widget-put widget :buttons buttons)
    (if found
	(insert "\n")
      (delete-region start (point)))
    found))


(defun custom-add-see-also (widget &optional prefix)
  "Add `See also ...' to WIDGET if there are any links.
Insert PREFIX first if non-nil."
  (let* ((symbol (widget-get widget :value))
	 (links (get symbol 'custom-links))
	 (many (> (length links) 2))
	 (buttons (widget-get widget :buttons))
	 (indent (widget-get widget :indent))
	 (level (widget-get widget :custom-level)))
    (when links
      (when indent
	;;(insert-char ?\a  indent))
	(insert-char ?\  indent))
      (when level
	(insert-char ?\  (* custom-buffer-indent (- level 2))))
      (when prefix
	(insert prefix))
      (insert "(See also ")
      (while links
	(push (widget-create-child-and-convert widget
					       (car links)
					       :button-face '(underline
							      (foreground-color . "blue"))
					       :button-prefix ""
					       :button-suffix ""
					       )
	      buttons)
	(setq links (cdr links))
	(cond ((null links)
	       (insert ".)\n"))
	      ((null (cdr links))
	       (if many
		   (insert ", and ")
		 (insert " and ")))
	      (t
	       (insert ", "))))
      (widget-put widget :buttons buttons))))


(defun custom-buffer-create-internal (options &optional description)
  (message "Creating customization buffer...")
  (custom-mode)
  (let ((s "Emacs Customize Interface for User Options"))
    (put-text-property 0 (length s)
		       'face
		       'custom-h1
;; 		       '(bold
;; 			 (:height 1.5)
;; 			 (font-family . "sans serif")
;; 			 (foreground-color . "blue4"))
		       s)
    (widget-insert s))
  (widget-insert "  (")
  (custom-info-link "Help" "(emacs)Easy Customization")
  (widget-insert ")\n")
  (when description
    (widget-insert "Customization buffer")
    (widget-insert description))
  ;;(widget-insert ". (")
  ;;   (widget-create 'info-link
  ;; 		 :tag "Help"
  ;; 		 :help-echo "Read the online help."
  ;; 		 "(emacs)Easy Customization")
  ;;(widget-insert ")\n\n")
  (widget-insert "\n\n")
  (message "Creating customization buttons...")


  (message "Creating customization items...")
  ;;(message "length options=%s" (length options))
  (buffer-disable-undo)
  (setq custom-options
	(if (= (length options) 1)
	    (mapcar (lambda (entry)
		      (widget-create (nth 1 entry)
				     :documentation-shown t
				     :custom-state 'unknown
				     :tag (custom-unlispify-tag-name
					   (nth 0 entry))
				     :value (nth 0 entry)))
		    options)
	  (let ((count 0)
		(length (length options)))
	    (mapcar (lambda (entry)
		      (prog2
			  (message "Creating customization items ...%2d%%"
				   (/ (* 100.0 count) length))
			  (widget-create (nth 1 entry)
					 :tag (custom-unlispify-tag-name
					       (nth 0 entry))
					 :value (nth 0 entry))
			(setq count (1+ count))
			(unless (eq (preceding-char) ?\n)
			  (widget-insert "\n"))
			(widget-insert "\n")))
		    options))))
  (unless (eq (preceding-char) ?\n) (widget-insert "\n"))
  (message "Creating customization items ...done")


  (custom-buffer-custom-fields)
  (let ((num-fields (length custom-buffer-custom-fields)))
    (let ((s "______________________________________________"))
      (put-text-property 0 (length s)
			 'face '(bold
				 (:height 1.5)
				 (foreground-color . "sienna4")) s)
      (widget-insert s)))
  (widget-insert "\n ")
  (widget-insert "Operate on all options in this buffer:");)
  (widget-insert "\n ")
  (setq custom-set-button
	(widget-create 'push-button
		       :tag " Set for Current Session "
		       :help-echo "\
Make your editing in this buffer take effect for this session."
		       :action (lambda (widget &optional event)
				 (custom-set-update-buttons))))
  (widget-insert " ")
  (setq custom-save-button
	(widget-create 'push-button
		       :tag " Save for Future Sessions "
		       :help-echo "\
Make your editing in this buffer take effect for future Emacs sessions."
		       :action (lambda (widget &optional event)
				 (custom-save-update-buttons))))
  (if custom-reset-button-menu
      (progn
	(widget-insert " ")
	(widget-create 'push-button
		       :tag " Reset "
		       :help-echo "Show a menu with reset operations."
		       :mouse-down-action (lambda (&rest junk) t)
		       :action (lambda (widget &optional event)
				 (custom-reset event))))
    (widget-insert "\n ")
    (setq custom-reset-to-current-button
	  (widget-create 'push-button
			 :tag " Reset "
			 :help-echo "\
Reset all edited text in this buffer to reflect current values."
			 :action 'custom-reset-current-update-buttons))
    (widget-insert " ")
    (setq custom-reset-to-saved-button
	  (widget-create 'push-button
			 :tag " Reset to Saved "
			 :help-echo "\
Reset all values in this buffer to their saved settings."
			 :action 'custom-reset-saved-update-buttons))
    (widget-insert " ")
    (setq custom-erase-customization-button
	  (widget-create 'push-button
			 :tag " Erase Customization "
			 :help-echo "\
Un-customize all values in this buffer.  They get their standard settings."
			 :action 'custom-reset-standard-update-buttons)))
  (widget-insert "   ")


  (widget-create 'push-button
		 :tag " Finish "
		 :help-echo
		 (lambda (&rest ignore)
		   (cond
		    ((eq custom-buffer-done-function
			 'custom-bury-buffer)
		     "Bury this buffer")
		    ((eq custom-buffer-done-function 'kill-buffer)
		     "Kill this buffer")
		    (t "Finish with this buffer")))
		 :action #'Custom-buffer-done)
  (widget-insert "\n\n")


  (unless (eq custom-buffer-style 'tree)
    (mapc 'custom-magic-reset custom-options))
  (message "Creating customization setup...")
  (widget-setup)
  (buffer-enable-undo)
  (goto-char (point-min))
  (custom-button-state-update)
  (message "Creating customization buffer...done"))


(defun custom-group-value-create (widget)
  "Insert a customize group for WIDGET in the current buffer."
  (let* ((state (widget-get widget :custom-state))
	 (level (widget-get widget :custom-level))
	 ;; (indent (widget-get widget :indent))
	 (prefix (widget-get widget :custom-prefix))
	 (buttons (widget-get widget :buttons))
	 (tag (widget-get widget :tag))
	 (symbol (widget-value widget))
	 (members (custom-group-members symbol
					(and (eq custom-buffer-style 'tree)
					     custom-browse-only-groups))))
    (cond ((and (eq custom-buffer-style 'tree)
		(eq state 'hidden)
		(or members (custom-unloaded-widget-p widget)))
	   (custom-browse-insert-prefix prefix)
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-visibility
		  ;; :tag-glyph "plus"
		  :tag "+")
		 buttons)
	   (insert "-- ")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((and (eq custom-buffer-style 'tree)
		(zerop (length members)))
	   (custom-browse-insert-prefix prefix)
	   (insert "[ ]-- ")
	   ;; (widget-glyph-insert nil "[ ]" "empty")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq custom-buffer-style 'tree)
	   (custom-browse-insert-prefix prefix)
	   (custom-load-widget widget)
	   (if (zerop (length members))
	       (progn
		 (custom-browse-insert-prefix prefix)
		 (insert "[ ]-- ")
		 ;; (widget-glyph-insert nil "[ ]" "empty")
		 ;; (widget-glyph-insert nil "-- " "horizontal")
		 (push (widget-create-child-and-convert
			widget 'custom-browse-group-tag)
		       buttons)
		 (insert " " tag "\n")
		 (widget-put widget :buttons buttons))
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-visibility
		    ;; :tag-glyph "minus"
		    :tag "-")
		   buttons)
	     (insert "-\\ ")
	     ;; (widget-glyph-insert nil "-\\ " "top")
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-group-tag)
		   buttons)
	     (insert " " tag "\n")
	     (widget-put widget :buttons buttons)
	     (message "Creating group...")
	     (let* ((members (custom-sort-items members
						custom-browse-sort-alphabetically
						custom-browse-order-groups))
		    (prefixes (widget-get widget :custom-prefixes))
		    (custom-prefix-list (custom-prefix-add symbol prefixes))
		    (extra-prefix (if (widget-get widget :custom-last)
				      "   "
				    " | "))
		    (prefix (concat prefix extra-prefix))
		    children entry)
	       (while members
		 (setq entry (car members)
		       members (cdr members))
		 (push (widget-create-child-and-convert
			widget (nth 1 entry)
			:group widget
			:tag (custom-unlispify-tag-name (nth 0 entry))
			:custom-prefixes custom-prefix-list
			:custom-level (1plus (1+ level))
			:custom-last (null members)
			:value (nth 0 entry)
			:custom-prefix prefix)
		       children))
	       (widget-put widget :children (reverse children)))
	     (message "Creating group...done")))
	  ;; Nested style.
	  ((eq state 'hidden)
	   ;; Create level indicator.
	   ;;(insert-char ?\Y (* custom-buffer-indent (1plus (1- level))))
	   (insert-char ?\  (* custom-buffer-indent (1plus (1- level))))
	   (unless (eq custom-buffer-style 'links)
	     (insert "-- ")
	     ;; Create tag.
	     (let ((begin (point)))
	       (insert tag)
	       (widget-specify-sample widget begin (point)))
	     (insert " subgroup: "))
	   ;; Create link/visibility indicator.
	   (if (eq custom-buffer-style 'links)
	       (push (widget-create-child-and-convert
		      widget 'custom-group-link
		      ;;:tag "Go to Group"
		      :tag tag
		      :button-face '(underline
				     bold
				     (foreground-color . "blue"))
		      :button-prefix ""
		      :button-suffix ""
		      symbol)
		     buttons)
	     (push (widget-create-child-and-convert
		    widget 'custom-group-link
		    ;;:tag "Go to Group"
		    :tag "Show in new buffer"
		    :button-face '(underline
				   ;;bold
				   (foreground-color . "blue"))
		    :button-prefix ""
		    :button-suffix ""
		    symbol)
		   buttons)
	     (insert ", ")
	     (push (widget-create-child-and-convert
		    widget 'custom-group-visibility
		    :help-echo "Show members of this group."
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons))
	   (when (eq custom-buffer-style 'links) (insert " subgroup "))
	   (insert " \n")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (when custom-display-option-doc
	     (if (and (eq custom-buffer-style 'links) (> level 1))
		 ;;(widget-put widget :documentation-indent 0))
		 (widget-put widget :documentation-indent (* custom-buffer-indent (1+ level))))
	     ;;(insert-char ?\h (* custom-buffer-indent level))
	     (widget-default-format-handler widget ?h)))
	  ;; Nested style.
	  (t				;Visible.
	   ;; Add parent groups references above the group.
	   (if t    ;;; This should test that the buffer
		    ;;; was made to display a group.
	       (when (eq level 1)
		 (if (custom-add-parent-links widget
					      "Parent groups:")
		     (insert "\n"))))
	   ;; Create level indicator.
	   ;;(insert-char ?\Z (* custom-buffer-indent (1plus (1- level))))
	   (insert-char ?\  (* custom-buffer-indent (1plus (1- level))))
	   (custom-insert-border "/- ")
	   ;; Create tag.
	   (let ((start (point)))
	     (insert tag)
	     (widget-specify-sample widget start (point)))
	   (custom-insert-border " customization group: ")
	   ;; Create visibility indicator.
	   ;;(unless (eq custom-buffer-style 'links)
	   (when (> level 1)
	     (custom-insert-border "------- ")
	     (push (widget-create-child-and-convert
		    widget 'group-visibility
		    :help-echo "Hide members of this group."
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons)
	     (insert " "))
	   ;; Create more dashes.
	   ;; Use 76 instead of 75 to compensate for the temporary "<"
	   ;; added by `widget-insert'.
;; 	   (insert-char ?- (- 76 (current-column)
;; 			      (* custom-buffer-indent (1plus level))))
	   (custom-insert-border
	    (make-string
	     (- 76 (current-column)
		(* custom-buffer-indent (1plus level))) ?-))
	   (custom-insert-border "\\")
	   (insert "\n")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic
			 :indent 0
			 nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (widget-put widget :documentation-indent (* custom-buffer-indent (1+ level)))
	   (widget-default-format-handler widget ?h)
	   ;; Parent groups.
	   (if nil  ;;; This should test that the buffer
		    ;;; was not made to display a group.
	       (when (eq level 1)
		 ;;(insert-char ?\W custom-buffer-indent)
		 (insert-char ?\  custom-buffer-indent)
		 (custom-add-parent-links widget)))
	   (custom-add-see-also widget
				(make-string (* custom-buffer-indent (1+ level))
					     ?\ ))
	   ;; Members.
	   (message "Creating group...")
	   (custom-load-widget widget)
	   (let* ((members (custom-sort-items members
					      custom-buffer-sort-alphabetically
					      custom-buffer-order-groups))
		  (prefixes (widget-get widget :custom-prefixes))
		  (custom-prefix-list (custom-prefix-add symbol prefixes))
		  (length (length members))
		  (count 0)
		  (children (mapcar (lambda (entry)
				      (widget-insert "\n")
				      (message "\
Creating group members... %2d%%"
					       (/ (* 100.0 count) length))
				      (setq count (1+ count))
				      (prog1
					  (widget-create-child-and-convert
					   widget (nth 1 entry)
					   :group widget
					   :tag (custom-unlispify-tag-name
						 (nth 0 entry))
					   :custom-prefixes custom-prefix-list
					   :custom-level (1plus (1+ level))
					   :value (nth 0 entry))
					(unless (eq (preceding-char) ?\n)
					  (widget-insert "\n"))))
				    members)))
	     (message "Creating group magic...")
	     (mapc 'custom-magic-reset children)
	     (message "Creating group state...")
	     (widget-put widget :children children)
	     (custom-group-state-update widget)
	     (message "Creating group... done"))
	   ;; End line
	   (insert "\n")
	   ;;(insert-char ?\V (* custom-buffer-indent (1plus (1- level))))
	   (insert-char ?\  (* custom-buffer-indent (1plus (1- level))))
	   (custom-insert-border "\\- ")
	   (custom-insert-border (widget-get widget :tag))
	   (custom-insert-border " group end ")
	   ;;(insert-char ?- (- 75 (current-column) (* custom-buffer-indent (1plus level))))
	   (custom-insert-border
	    (make-string (- 75 (current-column) (* custom-buffer-indent (1plus level)))
			 ?-))
	   (custom-insert-border "/")
	   (insert "\n\n\n\n")
	   ))))


(defun custom-magic-value-create (widget)
  "Create compact status report for WIDGET."
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state))
	 (hidden (eq state 'hidden))
	 (entry (assq state custom-magic-alist))
	 (magic (nth 1 entry))
	 (face (nth 2 entry))
	 (category (widget-get parent :custom-category))
	 (text (or (and (eq category 'group)
			(nth 4 entry))
		   (nth 3 entry)))
	 (form (widget-get parent :custom-form))
	 children)
;;     (cond ((eq category 'group)
;; 	   (message "GGGGGGGG %s" (widget-get widget :parent)))
;; 	  (t
;; 	   (message "YYYYY %s" (widget-get widget :parent))))
;;     (sleep-for 1) (message "")(sleep-for 1)
    (while (string-match "\\`\\(.*\\)%c\\(.*\\)\\'" text)
      (setq text (concat (match-string 1 text)
			 (symbol-name category)
			 (match-string 2 text))))
    (when (and custom-magic-show
	       (or (not hidden)
		   (memq category custom-magic-show-hidden)))
      ;;(insert "   ")
      (when (and (eq category 'group))
;; 		 (not (and (eq custom-buffer-style 'links)
;; 			   (> (widget-get parent :custom-level) 1))))
	;;(insert-char ?\T (* custom-buffer-indent
	(insert-char ?\  (* custom-buffer-indent
			    (1+ (widget-get parent :custom-level)))))
			    ;;(- (widget-get parent :custom-level) 1))))
      (unless (eq category 'group)
	;;(insert-char ?\u (* custom-buffer-indent
	(insert-char ?\  (* custom-buffer-indent
			    (- (widget-get parent :custom-level) 2))))
      (push (widget-create-child-and-convert
	     widget 'choice-item
	     :help-echo "Change the state of this item."
	     :format (if hidden "%t" "%[ %t » %]")
	     :button-prefix 'widget-push-button-prefix
	     :button-suffix 'widget-push-button-suffix
	     :mouse-down-action 'widget-magic-mouse-down-action
	     :tag (if (eq category 'group) "Group state" "State"))
	    children)
      (insert ": ")
      (let ((start (point)))
	(if (eq custom-magic-show 'long)
	    (insert text)
	  (insert (symbol-name state)))
	(cond ((eq form 'lisp)
	       (insert " (lisp)"))
	      ((eq form 'mismatch)
	       (insert " (mismatch)")))
	(put-text-property start (point) 'face 'custom-state-face))
      (insert "\n"))
    (when (and (eq category 'group))
;; 	       (not (and (eq custom-buffer-style 'links)
;; 			 (> (widget-get parent :custom-level) 1))))
      ;;(insert-char ?\X (* custom-buffer-indent
      (insert-char ?\  (* custom-buffer-indent
			  (1+ (widget-get parent :custom-level)))))
    (when custom-magic-show-button
      ;;(when custom-magic-show
	(let ((indent (widget-get parent :indent))
	      (level (widget-get parent :custom-level)))
	  ;;(message "show-button parent.:indent=%s level=%s" indent level)
	  (when level
	    ;;(insert-char ?\B (* custom-buffer-indent (- level 2)))))
	    (insert-char ?\  (* custom-buffer-indent (- level 2)))))
	;;)
      ;;(save-excursion
;; 	(let* ((pp parent)
;; 	       found
;; 	       (item (progn
;; 		       (while (and pp (not found))
;; 			 ;;(message "pp=%s" pp)
;; 			 (if (eq (car pp) 'custom-variable)
;; 			     (setq found t))
;; 			 (setq pp (widget-get pp :parent)))
;; 		       pp)
;; 		     )
;; 	       ;;(overlays (overlays-at (point)))
;; 	       ;;(from (widget-get (widget-get parent :parent) :from))
;; 	       ;;(from (when overlays
;; 		       ;;(message "overlay here")(sleep-for 1)
;; 		       ;;(overlay-get (car overlays) :from)))
;; 	       (from (when item (widget-get pp :from)))
;; 	       )
;; 	  (when from (goto-char from))
;; 	  (beginning-of-line)
;; 	  (forward-line -4)
;; 	  )
	(push (widget-create-child-and-convert
	       widget 'choice-item
	       :mouse-down-action 'widget-magic-mouse-down-action
	       :button-face face
	       :button-prefix ""
	       :button-suffix ""
	       :help-echo (concat text " Click to save/reset.")
	       :format "%[%t%]" ;(if hidden "%t" "%[%t%]")
	       :tag (if (memq form '(lisp mismatch))
			(concat "(" magic ")")
		      (concat "[" magic "]")))
	      children)
	(insert " ")
	;;(unless custom-display-option-doc (insert "\n"))
	;; )
      )
    (widget-put widget :children children)))


(defconst custom-magic-alist '((nil "#" underline "\
Uninitialized, you should not see this.")
			       (unknown "?" italic "\
Unknown, you should not see this.")
			       (hidden "-" custom-hidden-face "\
Hidden, invoke \"Show value\" in the previous line to show." "\
Group is hidden, invoke \"Show\", above, to show contents.")
			       (invalid "x" custom-invalid-face "\
The value displayed for this %c is invalid and cannot be set.")
			       (modified "*" custom-modified-face "\
You have changed this value, but you have not set or saved it." "\
You have changed something in this group, but not set or saved it.")
			       (set "+" custom-set-face "\
You have set this %c, but not saved it for future sessions." "\
Something in this group has been set, but not saved.")
			       (changed ":" custom-changed-face "\
This %c has been changed outside of this customize buffer." "\
Something in this group has been changed outside customize.")
			       (saved "!" custom-saved-face "\
This %c has been set and saved." "\
Something in this group has been set and saved.")
			       (rogue "@" custom-rogue-face "\
This %c has not been changed with customize." "\
Something in this group is not prepared for customization.")
			       ;;(standard " " nil "\
			       (standard " " custom-unmodified-face "\
This %c is unchanged from its standard setting." "\
Visible group members are all at standard settings."))
  "Alist of customize option states.
Each entry is of the form (STATE MAGIC FACE ITEM-DESC [ GROUP-DESC ]), where

STATE is one of the following symbols:

nil
   For internal use, should never occur.
`unknown'
   For internal use, should never occur.
`hidden'
   This item is not being displayed.
`invalid'
   This item is modified, but has an invalid form.
`modified'
   This item is modified, and has a valid form.
`set'
   This item has been set but not saved.
`changed'
   The current value of this item has been changed temporarily.
`saved'
   This item is marked for saving.
`rogue'
   This item has no customization information.
`standard'
   This item is unchanged from the standard setting.

MAGIC is a string used to present that state.

FACE is a face used to present the state.

ITEM-DESC is a string describing the state for options.

GROUP-DESC is a string describing the state for groups.  If this is
left out, ITEM-DESC will be used.

The string %c in either description will be replaced with the
category of the item.  These are `group', `option', and `face'.

The list should be sorted most significant first.")


(defun custom-variable-value-create (widget)
  "Here is where you edit the variable's value."
  (custom-load-widget widget)
  (unless (widget-get widget :custom-form)
    (widget-put widget :custom-form custom-variable-default-form))
  (let* ((buttons (widget-get widget :buttons))
	 (children (widget-get widget :children))
	 (form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (symbol (widget-get widget :value))
	 (tag (widget-get widget :tag))
	 (type (custom-variable-type symbol))
	 (conv (widget-convert type))
	 (get (or (get symbol 'custom-get) 'default-value))
	 (prefix (widget-get widget :custom-prefix))
	 (last (widget-get widget :custom-last))
	 (value (if (default-boundp symbol)
		    (funcall get symbol)
		  (widget-get conv :value)))
	 (level (widget-get widget :custom-level))
	 )
    ;;(message "custom-variable-value-create.level=%s parent=" level )
    ;; If the widget is new, the child determines whether it is hidden.
    (cond (state)
	  ((custom-show type value)
	   (setq state 'unknown))
	  (t
	   (setq state 'hidden)))
    ;; If we don't know the state, see if we need to edit it in lisp form.
    (when (eq state 'unknown)
      (unless (widget-apply conv :match value)
	;; (widget-apply (widget-convert type) :match value)
	(setq form 'mismatch)))
    ;; Now we can create the child widget.
    (cond ((eq custom-buffer-style 'tree)
	   (insert prefix (if last " `--- " " |--- "))
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-variable-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq state 'hidden)
	   ;;(insert-char ?\% (* custom-buffer-indent (- level 2)))
	   (insert-char ?\  (* custom-buffer-indent (- level 2)))
	   ;; Indicate hidden value.
	   (push (widget-create-child-and-convert
		  widget 'item
		  :format "%{%t%}: "
		  :sample-face 'custom-variable-tag-face
		  :tag tag
		  :parent widget)
		 buttons)
	   (push (widget-create-child-and-convert
		  widget 'value-visibility
		  :help-echo "Show the value of this option."
		  :action 'custom-toggle-parent
		  nil)
		 buttons))
	  ((memq form '(lisp mismatch))
	   ;; In lisp mode edit the saved value when possible.
	   (let* ((value (cond ((get symbol 'saved-value)
				(car (get symbol 'saved-value)))
			       ((get symbol 'standard-value)
				(car (get symbol 'standard-value)))
			       ((default-boundp symbol)
				(custom-quote (funcall get symbol)))
			       (t
				(custom-quote (widget-get conv :value))))))
	     (let ((s (propertize (symbol-name symbol) 'face 'custom-variable-tag-face)))
	       (widget-insert s))
	     (insert ": ")
	     (push (widget-create-child-and-convert
		    widget 'value-visibility
		    :help-echo "Hide the value of this option."
		    :action 'custom-toggle-parent
		    t)
		   buttons)
	     (insert " ")
	     (push (widget-create-child-and-convert
		    widget 'sexp
		    :button-face 'custom-variable-button-face
		    :format "%v"
		    :tag (symbol-name symbol)
		    ;;:sample-face 'custom-variable-tag-face ;; use same face
		    :parent widget
		    :value value)
		   children)))
	  (t
	   ;; Edit mode.
	   (let* ((format (widget-get type :format))
		  tag-format value-format)
	     (unless (string-match ":" format)
	       (error "Bad format"))
	     (setq tag-format (substring format 0 (match-end 0)))
	     (setq value-format (substring format (match-end 0)))
	     ;;(insert-char ?\+ (* custom-buffer-indent (- level 2)))
	     (insert-char ?\  (* custom-buffer-indent (- level 2)))
	     (push (widget-create-child-and-convert
		    widget 'item
		    :format tag-format
		    :action 'custom-tag-action
		    :help-echo "Change value of this option."
		    :mouse-down-action 'custom-tag-mouse-down-action
		    :button-face 'custom-variable-button-face
		    :sample-face 'custom-variable-tag-face
		    tag)
		   buttons)
	     (insert " ")
	     (push (widget-create-child-and-convert
		    widget 'value-visibility
		    :help-echo "Hide the value of this option."
		    :action 'custom-toggle-parent
		    t)
		   buttons)
	     (insert " ")
	     (push (widget-create-child-and-convert
		    widget type
		    :format value-format
		    :value value)
		   children))))
    (unless (eq custom-buffer-style 'tree)
      (unless (eq (preceding-char) ?\n)
	(widget-insert "\n"))
      ;; Create the magic button.
      (let ((magic (widget-create-child-and-convert
		    widget 'custom-magic nil)))
	(widget-put widget :custom-magic magic)
	(push magic buttons))
      ;; ### NOTE: this is ugly!!!! I need to update the :buttons property
      ;; before the call to `widget-default-format-handler'. Otherwise, I
      ;; loose my current `buttons'. This function shouldn't be called like
      ;; this anyway. The doc string widget should be added like the others.
      ;; --dv
      (widget-put widget :buttons buttons)
      ;; Insert documentation.
      (when custom-display-option-doc
	(widget-insert "\n")
	(widget-put widget :documentation-indent (* custom-buffer-indent (- level 2)))
	;;(insert-char ?\h (* custom-buffer-indent (- level 2)))
	(insert-char ?\  (* custom-buffer-indent (- level 2)))
	(widget-default-format-handler widget ?h))

      ;; The comment field
      (unless (eq state 'hidden)
	(let* ((comment (get symbol 'variable-comment))
	       (comment-widget
		(widget-create-child-and-convert
		 widget 'custom-comment
		 :parent widget
		 :value (or comment ""))))
	  (widget-put widget :comment-widget comment-widget)
	  ;; Don't push it !!! Custom assumes that the first child is the
	  ;; value one.
	  (setq children (append children (list comment-widget)))))
      ;; Update the rest of the properties properties.
      (widget-put widget :custom-form form)
      (widget-put widget :children children)
      ;; Now update the state.
      (if (eq state 'hidden)
	  (widget-put widget :custom-state state)
	(custom-variable-state-set widget))
      ;; See also.
      (unless (eq state 'hidden)
	(when (eq (widget-get widget :custom-level) 1)
	  (custom-add-parent-links widget "Member of groups:"))
	(when custom-display-option-doc
	  (custom-add-see-also widget))
	))))


;;; Menus and buttons etc

(defvar custom-buffer-var-custom-form nil)
(defvar custom-buffer-face-custom-form nil)

(defvar custom-set-button nil)
(defvar custom-save-button nil)
(defvar custom-reset-to-current-button nil)
(defvar custom-reset-to-saved-button nil)
(defvar custom-erase-customization-button nil)

;; I guess all local vars should go into mode function (for efficiancy)
(make-variable-buffer-local 'custom-reset-to-saved-button)
(make-variable-buffer-local 'custom-erase-customization-button)
(make-variable-buffer-local 'custom-set-button)
(make-variable-buffer-local 'custom-save-button)
(make-variable-buffer-local 'custom-reset-to-current-button)

(make-variable-buffer-local 'custom-magic-show-button)
(make-variable-buffer-local 'custom-magic-show)

(defun custom-item-action-possible-any(action)
  (let ((num 0))
    (mapc (lambda (widget)
	    (when (funcall action widget)
	      (setq num (+ num 1))))
	    custom-buffer-custom-fields)
    (when (> num 0) num)))

(defun custom-set-possible-any()
  (custom-item-action-possible-any 'custom-set-possible))
(defun custom-save-possible-any()
  (custom-item-action-possible-any 'custom-save-possible))
(defun custom-reset-to-saved-possible-any()
  (custom-item-action-possible-any 'custom-reset-to-saved-possible))
(defun custom-erase-customization-possible-any()
  (custom-item-action-possible-any 'custom-erase-customization-possible))
(defun custom-reset-to-current-possible-any()
  (custom-item-action-possible-any 'custom-reset-to-current-possible))

(defun custom-reset-to-saved-possible(widget)
  (when widget
    (let ((type (car widget)))
      (cond ((eq type 'custom-variable)
	     (and (or (get (widget-value widget) 'saved-value)
		      (get (widget-value widget) 'saved-variable-comment))
		  (memq (widget-get widget :custom-state)
			'(modified set changed rogue))))
	    ((eq type 'custom-face)
	     (or (get (widget-value widget) 'saved-face)
		 (get (widget-value widget) 'saved-face-comment)))
	    ))))
(defun custom-erase-customization-possible(widget)
  (when widget
    (let ((type (car widget)))
      (cond ((eq type 'custom-variable)
	     (and (get (widget-value widget) 'standard-value)
		  (memq (widget-get widget :custom-state)
			'(modified set changed saved rogue)))
	     )
	    ((eq type 'custom-face)
	     (get (widget-value widget) 'face-defface-spec)
	     )
	    ))))
(defun custom-set-possible(widget)
  (when widget
    (let ((type (car widget)))
      (cond ((eq type 'custom-variable)
	     (eq (widget-get widget :custom-state) 'modified)
	     )
	    ((eq type 'custom-face)
	     t
	     )
	    ))))
(defun custom-save-possible(widget)
  (when widget
    (let ((type (car widget)))
      (cond ((eq type 'custom-variable)
	     (memq (widget-get widget :custom-state) '(modified set changed rogue))
	     )
	    ((eq type 'custom-face)
	     t
	     )
	    ))))
(defun custom-reset-to-current-possible(widget)
  (when widget
    (let ((type (car widget)))
      (cond ((eq type 'custom-variable)
	     (and (default-boundp (widget-value widget))
		  (memq (widget-get widget :custom-state) '(modified changed)))
	     )
	    ((eq type 'custom-face)
	     t
	     )
	    ))))

(defun custom-set-current-item()
  (interactive)
  (custom-variable-set (custom-buffer-current-option)))
(defun custom-save-current-item()
  (interactive)
  (custom-variable-save (custom-buffer-current-option)))
(defun custom-reset-current-field()
  (interactive)
  (custom-redraw (custom-buffer-current-option)))
(defun custom-reset-current-item-to-saved()
  (interactive)
  (custom-variable-reset-saved (custom-buffer-current-option)))
(defun custom-erase-current-items-customization()
  (interactive)
  (custom-variable-reset-standard (custom-buffer-current-option)))

(defun custom-set-buffer-custom-forms(var-form face-form)
  (interactive)
  (setq custom-buffer-var-custom-form var-form)
  (setq custom-buffer-face-custom-form face-form)
  (let ((children custom-buffer-custom-fields)
	(num 0))
    (mapc
     (lambda (widget)
       (widget-put widget :custom-state 'unknown)
       (let ((type (car widget)))
	 (cond ((eq type 'custom-variable)
		(widget-put widget :custom-form var-form))
	       ((eq type 'custom-face)
		(widget-put widget :custom-form face-form))
	       ))
       (custom-redraw widget))
     children)
    ;;(message "num=%s" num)
    ))

(defun custom-set-magic-show-button (show)
  (setq custom-magic-show-button show)
  (mapc (lambda (widget) (custom-redraw widget)) (custom-buffer-custom-fields)))
(defun custom-set-display-option-doc(display)
  (setq custom-display-option-doc display)
  (mapc (lambda (widget) (custom-redraw widget)) (custom-buffer-custom-fields)))
(defun custom-set-magic-show(how)
  (unless (memq how '(nil long short))
    (error "Invalid value"))
  (setq custom-magic-show how)
  (mapc (lambda (widget) (custom-redraw widget)) (custom-buffer-custom-fields)))


(defvar custom-buffer-custom-fields nil)
(make-variable-buffer-local 'custom-buffer-custom-fields)
(defun custom-buffer-custom-fields ()
  (let ((buffer-custom-fields))
    (custom-get-buffer-custom-fields2 custom-options nil)
    buffer-custom-fields
    (setq custom-buffer-custom-fields buffer-custom-fields)))
(defun custom-get-buffer-custom-fields2 (root visited)
  (unless (memq root visited)
    (add-to-list 'visited root)
    (let* ((children root)
	   (first (car children))
	   (child first))
      ;;(when (eq root custom-options) (message "equal"))
      (unless (consp first)
	(when (or (eq child 'custom-variable)
		  (eq child 'custom-face))
	  ;;(message "Child=%s" child )
	  ;;(message "widget.tag %s" (widget-get root :tag))
	  ;;(message "widget.custom-form %s" (widget-get root :custom-form))
	  (add-to-list 'buffer-custom-fields root)
	  ))
      (while (and children (listp children))
	(setq child (car children))
	(when (listp child)
	  (custom-get-buffer-custom-fields2 child visited))
	(setq children (cdr children)))
  )))



(defvar custom-buffer-current-option nil)
(make-variable-buffer-local 'custom-buffer-current-option)
(defun custom-buffer-current-option()
  (custom-buffer-option-at (point)))
(defun custom-buffer-option-at(marker)
  (interactive)
  (custom-buffer-custom-fields) ;; TODO: remove from here
  (let ((current))
    (mapc (lambda (widget)
	    (let ((from (marker-position (widget-get widget :from)))
		  (to (marker-position (widget-get widget :to))))
	      (when (and (<= marker to)
			 (<= from marker))
		;;(message "%s" (widget-get widget :tag)) (sleep-for 1)
		(setq current widget))))
	  custom-buffer-custom-fields)
    ;;(message "current=%s len=%s" current (length custom-buffer-custom-fields))
    (setq custom-buffer-current-option current)
    ))

(defun custom-update-submenu()
  (when (eq major-mode 'custom-mode)
    (custom-buffer-current-option)
    (custom-menu-update-current-item)
    ))
(add-hook 'menu-bar-update-hook 'custom-update-submenu)


(defun custom-menu-update-current-item()
  ;;(custom-buffer-current-option)
  (easy-menu-change
   '("Custom" "Set or Save") "Current Item"
   (list
     ;(vector
      (if custom-buffer-current-option
	  ;;"hej"
	  ;;"Current"
	  (widget-get custom-buffer-current-option :tag)
	"(No current option)")
      ;t)
     "--"
     ["Set for Current Session" (custom-variable-set custom-buffer-current-option)
      (custom-set-possible custom-buffer-current-option) ]
     ["Save for Future Sessions" (custom-variable-save custom-buffer-current-option)
      (custom-save-possible custom-buffer-current-option) ]
     ["Reset Field to Current" (custom-redraw custom-buffer-current-option)
      (custom-reset-to-current-possible custom-buffer-current-option) ]
     ["Reset to Saved Value" (custom-variable-reset-saved custom-buffer-current-option)
      (custom-reset-to-saved-possible custom-buffer-current-option) ]
     ["Erase Customization" (custom-variable-reset-standard custom-buffer-current-option)
      (custom-erase-customization-possible custom-buffer-current-option) ]
     )
   ))
;; (custom-menu-update-current-item)
(easy-menu-define Custom-mode-menu
  custom-mode-map
  "Menu used in customization buffers."
  `("Custom"
    ("Set or Save"
     ("Current Item"
      :active custom-buffer-current-option
;;       ["Set for Current Session" (custom-variable-set custom-buffer-current-option)
;;        (custom-set-possible custom-buffer-current-option) ]
;;       ["Save for Future Sessions" (custom-variable-save custom-buffer-current-option)
;;        (custom-save-possible custom-buffer-current-option) ]
;;       ["Reset Field to Current" (custom-redraw custom-buffer-current-option)
;;        (custom-reset-to-current-possible custom-buffer-current-option) ]
;;       ["Reset to Saved Value" (custom-variable-reset-saved custom-buffer-current-option)
;;        (custom-reset-to-saved-possible custom-buffer-current-option) ]
;;       ["Erase Customization" (custom-variable-reset-standard custom-buffer-current-option)
;;        (custom-erase-customization-possible custom-buffer-current-option) ]
      )
     ("All Items in Buffer"
      ["Set for Current Session" custom-set-update-buttons
       (custom-set-possible-any)]
      ["Save for Future Sessions" custom-save-update-buttons
       (custom-save-possible-any)]
      ["Reset to Current" custom-reset-current-update-buttons
       (custom-reset-to-current-possible-any)]
      ["Reset to Saved" custom-reset-saved-update-buttons
       (custom-reset-to-saved-possible-any)]
      ["Erase Customization" custom-reset-standard-update-buttons
       (custom-erase-customization-possible-any)]
      )
     )
    ("View"
     ("Value Display"
      ["Hide All Values" (custom-hide-all-values)]
      ["Show All Values" (custom-hide-all-values t)]
      "--"
      ["Pretty"
       ;;custom-toggle-buffer-var-custom-form
       (custom-set-buffer-custom-forms 'edit 'selected)
       :style radio
       :selected (and (eq custom-buffer-var-custom-form 'edit)
		      (eq custom-buffer-face-custom-form 'selected))
       ]
      ["Full Face Specifications+Pretty"
       (custom-set-buffer-custom-forms 'edit 'all)
       :style radio
       :selected (and (eq custom-buffer-var-custom-form 'edit)
		      (eq custom-buffer-face-custom-form 'all))
       ]
      ["Lisp"
       ;;custom-toggle-buffer-var-custom-form
       (custom-set-buffer-custom-forms 'lisp 'lisp)
       :style radio
       :selected (and (eq custom-buffer-var-custom-form 'lisp)
		      (eq custom-buffer-face-custom-form 'lisp))
       ]
      )
     ("State Display"
      ["None" (custom-set-magic-show nil)
       :style radio
       :selected (eq custom-magic-show nil)]
      ["Short" (custom-set-magic-show 'short)
       :style radio
       :selected (eq custom-magic-show 'short)]
      ["Long" (custom-set-magic-show 'long)
       :style radio
       :selected (eq custom-magic-show 'long)]
      "--"
      ["Colored State Button Visible"
       (custom-set-magic-show-button (not custom-magic-show-button))
       :style toggle
       :selected custom-magic-show-button
       ]
      )
     ["Documentation Visible"
      (custom-set-display-option-doc (not custom-display-option-doc))
      :style toggle
      :selected custom-display-option-doc
      ]
     )
    "-"
    ["Info" (Info-goto-node "(emacs)Easy Customization") t]
    ,(customize-menu-create 'customize)
    ))


(defconst custom-variable-menu
  '(("Set for Current Session" custom-variable-set
     (lambda (widget)
       (eq (widget-get widget :custom-state) 'modified)))
    ("Save for Future Sessions" custom-variable-save
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified set changed rogue))))
    ;;("Reset to Current" custom-redraw
    ("Reset Field to Current" custom-redraw
     (lambda (widget)
       (and (default-boundp (widget-value widget))
	    (memq (widget-get widget :custom-state) '(modified changed)))))
    ;;("Reset to Saved" custom-variable-reset-saved
    ("Reset to Saved Value" custom-variable-reset-saved
     (lambda (widget)
       (and (or (get (widget-value widget) 'saved-value)
		(get (widget-value widget) 'saved-variable-comment))
	    (memq (widget-get widget :custom-state)
		  '(modified set changed rogue)))))
    ("Erase Customization" custom-variable-reset-standard
    ;;("Reset Field to Standard Value" custom-variable-reset-standard
     (lambda (widget)
       (and (get (widget-value widget) 'standard-value)
	    (memq (widget-get widget :custom-state)
		  '(modified set changed saved rogue)))))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ;;     ("---" ignore ignore)
    ;;     ("Don't show as Lisp expression" custom-variable-edit
    ;;      (lambda (widget)
    ;;        (eq (widget-get widget :custom-form) 'lisp)))
    ;;     ("Show initial Lisp expression" custom-variable-edit-lisp
    ;;      (lambda (widget)
    ;;        (eq (widget-get widget :custom-form) 'edit)))
    )
  "Alist of actions for the `custom-variable' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-variable'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")


;; (defun custom-toggle-buffer-var-custom-form(&rest ignore)
;;   (interactive)
;;   (if (eq custom-buffer-var-custom-form 'lisp)
;;       (custom-set-buffer-var-custom-form 'edit)
;;     (custom-set-buffer-var-custom-form 'lisp)))

(defconst custom-face-menu
  '(("Set for Current Session" custom-face-set)
    ("Save for Future Sessions" custom-face-save-command)
    ("Reset to Saved" custom-face-reset-saved
     (lambda (widget)
       (or (get (widget-value widget) 'saved-face)
	   (get (widget-value widget) 'saved-face-comment))))
    ("Erase Customization" custom-face-reset-standard
     (lambda (widget)
       (get (widget-value widget) 'face-defface-spec)))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
;;     ("---" ignore ignore)
;;     ("Show all display specs" custom-face-edit-all
;;      (lambda (widget)
;;        (not (eq (widget-get widget :custom-form) 'all))))
;;     ("Just current attributes" custom-face-edit-selected
;;      (lambda (widget)
;;        (not (eq (widget-get widget :custom-form) 'selected))))
;;     ("Show as Lisp expression" custom-face-edit-lisp
;;      (lambda (widget)
;;        (not (eq (widget-get widget :custom-form) 'lisp))))
    )
  "Alist of actions for the `custom-face' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-face'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")


;;; Mode

(defun custom-mode ()
  "Major mode for editing customization buffers.

The following commands are available:

Move to next button or editable field.     \\[widget-forward]
Move to previous button or editable field. \\[widget-backward]
\\<widget-field-keymap>\
Complete content of editable text field.   \\[widget-complete]
\\<custom-mode-map>\
Invoke button under the mouse pointer.     \\[Custom-move-and-invoke]
Invoke button under point.		   \\[widget-button-press]
Set all modifications.			   \\[Custom-set]
Make all modifications default.		   \\[Custom-save]
Reset all modified options.		   \\[Custom-reset-current]
Reset all modified or set options.	   \\[Custom-reset-saved]
Reset all options.			   \\[Custom-reset-standard]

Entry to this mode calls the value of `custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (easy-menu-add Custom-mode-menu)
  (make-local-variable 'custom-buffer-var-custom-form) ;; cus-new.el
  (setq custom-buffer-var-custom-form 'edit) ;; cus-new.el - edit or lisp
  (make-local-variable 'custom-buffer-face-custom-form) ;; cus-new.el
  (setq custom-buffer-face-custom-form 'edit) ;; cus-new.el - edit or lisp
  (make-local-variable 'custom-options)
  (make-local-variable 'widget-documentation-face)
  (setq widget-documentation-face 'custom-documentation-face)
  (make-local-variable 'widget-button-face)
  (setq widget-button-face 'custom-button-face)
  (set (make-local-variable 'widget-button-pressed-face)
       'custom-button-pressed-face)
  (set (make-local-variable 'widget-mouse-face)
       'custom-button-pressed-face)    ; buttons `depress' when moused
  ;; When possible, use relief for buttons, not bracketing.  This test
  ;; may not be optimal.
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) "")
    (set (make-local-variable 'widget-push-button-suffix) "")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (add-hook 'widget-edit-functions 'custom-state-buffer-message nil t)
  (add-hook 'widget-edit-functions 'custom-button-state-update nil t)
  (run-hooks 'custom-mode-hook))

;; (defun Custom-set ()
;;   "Set changes in all modified options."
;;   (interactive)
;;   (let ((children custom-options))
;;     (mapc (lambda (child)
;; 	    (message "tag=%s" (widget-get child :tag))
;; 	    (when (eq (widget-get child :custom-state) 'modified)
;; 	      (message "modified.tag=%s" (widget-get child :tag))
;; 	      (widget-apply child :custom-set)))
;; 	    children)))


;;; Work

(defun custom-hide-all-values(&optional show)
  (interactive)
  (save-excursion
    (let ((cur (point-min))
	  (widget nil)
	  (parent nil)
	  (overlays (overlay-lists)))
      (setq overlays (append (car overlays) (cdr overlays)))
      (while (setq cur (pop overlays))
	(setq widget (overlay-get cur 'button))
	(when (and (eq (widget-type widget) 'value-visibility)
		   )
	  (when (if show
		    (not (widget-value widget))
		  (widget-value widget))
	    (widget-apply-action widget))
	  )))))


(defun custom-button-state-helper(button activate)
  (if activate
      ;;(unless (widget-apply button :active)
      (widget-apply button :activate)
    ;;)
    ;;(when (widget-apply button :active)
    (widget-apply button :deactivate)
    ;;)
    ))
(defun custom-button-state-update (&rest ignore)
;; button states should be updated here! (I believe)
  "Update big buttons' states.
Do not know if IGNORE argument is needed."
  (custom-button-state-helper custom-set-button (custom-set-possible-any))
  (custom-button-state-helper custom-save-button (custom-save-possible-any))
  (custom-button-state-helper custom-reset-to-current-button
			      (custom-reset-to-current-possible-any))
  (custom-button-state-helper custom-reset-to-saved-button
			      (custom-reset-to-saved-possible-any))
  (custom-button-state-helper custom-erase-customization-button
			      (custom-erase-customization-possible-any))
  )

(define-key custom-mode-map [down-mouse-3] 'custom-menu-mouse-3-menu)
(define-key custom-mode-map [mouse-3] (lambda()(interactive)))

;; Adopted from function in buffer-menu+.el (by Drew Adams)
(defun custom-menu-mouse-3-menu (event)
  "Pop up menu for Mouse-3 for customization items."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
	 (widget (custom-buffer-option-at (posn-point mouse-pos)))
	 (menu-list nil))
    (sit-for 0)
    (if (not widget)
	(progn (message "Please click on an option") (sleep-for 2) nil)
      (when (custom-set-possible widget)
	(add-to-list 'menu-list
		     '("Set for Current Session" .
		       (lambda() (interactive) (custom-variable-set widget))) t))
      (when (custom-save-possible widget)
	(add-to-list 'menu-list
		     '("Save for Future Sessions" .
		       (lambda() (interactive) (custom-variable-save widget))) t))
      (when (custom-reset-to-current-possible widget)
	(add-to-list 'menu-list
		     '("Reset Field to Current" .
		       (lambda() (interactive) (custom-redraw widget))) t))
      (when (custom-reset-to-saved-possible widget)
	(add-to-list 'menu-list
		     '("Reset to Saved Value" .
		       (lambda() (interactive) (custom-variable-reset-saved widget))) t))
      (when (custom-erase-customization-possible widget)
	(add-to-list 'menu-list
		     '("Erase Customization" .
		       (lambda() (interactive) (custom-variable-reset-standard widget))) t))
      (unless menu-list
	(add-to-list 'menu-list
		     '("(No possible set/save action)" .
		       (lambda() (interactive) )) t))
      (add-to-list 'menu-list "dummy")
      (let ((selection
	     (x-popup-menu
	      event
	      (list
	       (widget-get widget :tag)
	       menu-list
	       )
	      )))
	(when selection (call-interactively selection))))))

(defun custom-state-buffer-message (widget)
  (if (eq (widget-get (widget-get widget :parent) :custom-state) 'modified)
      (message "To install your edits, invoke [State] and choose the Set operation")))

;; (defun custom-redraw-magic (widget)
;;   "Redraw WIDGET state with current settings."
;;   (while widget
;;     (let ((magic (widget-get widget :custom-magic)))
;;       (cond (magic
;; 	     (widget-value-set magic (widget-value magic))
;; 	     (when (setq widget (widget-get widget :group))
;; 	       (custom-group-state-update widget)))
;; 	    (t
;; 	     (setq widget nil)))))
;;   (custom-button-state-update)
;;   (widget-setup))

(defun custom-reset-standard-update-buttons(&rest ignore)
  (interactive)
  (Custom-reset-standard)
  (custom-button-state-update))
(defun custom-reset-saved-update-buttons(&rest ignore)
  (interactive)
  (Custom-reset-saved)
  (custom-button-state-update))
(defun custom-reset-current-update-buttons(&rest ignore)
  (interactive)
  (Custom-reset-current)
  (custom-button-state-update))
(defun custom-save-update-buttons(&rest ignore)
  (Custom-save)
  (custom-button-state-update))
(defun custom-set-update-buttons(&rest ignore)
  (Custom-set)
  (custom-button-state-update))

(message "You can now test the new Custom GUI!")(sleep-for 3)

(provide 'cus-new-gui)

;;; cus-new-gui.el ends here
