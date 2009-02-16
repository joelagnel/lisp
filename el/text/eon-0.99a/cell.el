;;; cell.el --- object-oriented spreadsheet 

;; Copyright (C) 2006, 2007  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: hypermedia, tools, lisp
;; Package-Version: 1.8
;; Time-stamp: <2007-09-15 00:24:20 dto>
;; Version: $Id: cell.el,v 1.80 2007/09/11 09:30:56 dto Exp dto $
;; Website: http://dto.freeshell.org/notebook/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is an object-oriented spreadsheet system with a fancy user
;; interface. Not well documented yet.

;;; Status:

;; During the port to Eon's object system, a bug crept into the fancy
;; rendering code, and I haven't fixed it yet. So only "raw" display
;; mode works right now. You can check out
;; http://dto.freeshell.org/e/pixel.el for an example of that.

;; The original (out-of-date) page for cell-mode is
;; http://dto.freeshell.org/notebook/CellMode.html

;;; Code:

(require 'eon)

;;;; Options

(defcustom cell-sheet-blank-width 2 "Default width of blank cells, in spaces." 
  :group 'cell)

(defcustom cell-sheet-no-label-string "no label" 
  "Default string for when a cell has no label." 
  :group 'cell)

;;;; Grids

;; We need generic two-dimensional arrays. Vectors of vectors should
;; suffice.

(defsubst grid-get (grid row column)
  "Retrieve the value of ROW, COLUMN from the GRID."
  (aref (aref grid row) column))

(defsubst grid-set (grid row column value)
  "Store VALUE in the GRID at location ROW, COLUMN."
  (let ((row (aref grid row)))
    (setf (aref row column) value)))

(defalias 'gref 'grid-get)
(defsetf gref grid-set) ;; now you can say (setf (gref grid row column) 'foo)

(defun make-grid (rows columns &optional seed)
  "Create a grid of size ROWS x COLUMNS. 
The cells are filled with a copy of SEED, if provided.
If SEED is an Eon object, clone it into each grid cell."
  (let ((grid (make-vector rows nil)))
    (dotimes (row rows) 
      (setf (aref grid row)
	    (make-vector columns nil))
      (dotimes (col columns) 
	(setf (gref grid row col)
	      (if (object-p seed)
		  (>> seed :clone)
		(copy-tree seed :vector)))))
    grid))

(defsubst grid-columns (grid)
  "Return the number of columns in GRID."
  (length (aref grid 0)))

(defsubst grid-rows (grid)
  "Return the number of rows in GRID."
  (length grid))

(defun grid-map (func grid)
  "Call FUNC for each cell in GRID, left-to-right, top-to-bottom."
  (dotimes (row (grid-rows grid))
    (dotimes (column (grid-columns grid))
      (funcall func (gref row column)))))

(defun vector-insert (oldvec pos elt)
  "Insert ELT into VECTOR at POS, moving elements at POS and
afterward down the list."
  (let* ((len (length oldvec))
	 (newvec (make-vector (+ len 1) nil)))
    (dotimes (i (+ 1 len))
      (setf (aref newvec i) (cond 
			     (( < i pos)
			      (aref oldvec i))
			     (( equal i pos)
			      elt)
			     (( > i pos) 
			      (aref oldvec (- i 1))))))
    newvec))

(defun vector-delete (oldvec pos)
  "Remove position POS from OLDVEC."
  (let* ((len (length oldvec))
	 (newvec (make-vector (- len 1) nil)))
    (dotimes (i (- len 1))
      (setf (aref newvec i) (cond
			     (( < i pos)
			      (aref oldvec i))
			     (( >= i pos)
			      (aref oldvec (+ i 1))))))
    newvec))

(defun grid-insert-row (grid row)
  "Returns a new grid with a row inserted at row ROW. You should
  replace the original grid with this one."
  (let* ((newrow (make-vector (grid-columns grid) nil)))
    (vector-insert grid row newrow)))
	
(defun grid-insert-column (grid col)
  "Returns a new grid with a column inserted at column COL. You
should replace the original grid with this one."
  (dotimes (i (grid-rows grid))
    (setf (aref grid i) (vector-insert (aref grid i) col nil)))
  grid)
          
(defun grid-delete-row (grid row)
  "Returns a copy of GRID with the row ROW removed. You should replace the original 
grid with this one."
  (vector-delete grid row))

(defun grid-delete-column (grid col)
  "Returns a copy of GRID with the column COL removed. You should
replace the original grid with this one."
  (dotimes (i (grid-rows grid))
    (setf (aref grid i) (vector-delete (aref grid i) col)))
  grid)

;;;; Cell object protocol

;; TODO put this feature back in
;; (defprotocol :cell 
;;   "Protocol for objects occupying cells in a spreadsheet."
;;   ((:calculate (:documentation "Update the contents of the cell."))
;;    (:label (:documentation "Brief string (possibly with properties) to label the object."))
;;    (:display-width 
;;     (:documentation "When non-nil, floating-point width of label in character width units."))
;;    (:formula (:documentation "Lisp formula."))
;;    (:value "Resulting lisp output of the cell."))))

;;;; Cell sheets 

(make-variable-buffer-local
 (defvar cell-sheet-of-current-buffer nil))  

(defvar cell-sheet-minimum-header-width 2)

(defvar cell-sheet-blank-rows 16)

(defvar cell-sheet-blank-columns 16)

(define-prototype :cell-sheet
  "A spreadsheet full of cell objects."
  ((:grid (:documentation "A two-dimensional array of spreadsheet cells."))
   (:display-buffer (:documentation "The emacs display-buffer where this cell sheet is displayed."))
   (:cursor (:documentation "Selected cell location in the form (ROW COLUMN)."))
   (:name (:documentation "String name of sheet. This is used to construct the buffer name."))
   (:column-stops 
    (:documentation "A vector of integers where v[x] is first column number of column x"))
   (:column-styles 
    (:documentation "A vector of property lists used to customize the properties of a column."))
   (:border-style (:documentation "When non-nil, draw cell borders." :default-value t))
   (:header-style (:documentation "When non-nil, draw row and column headers." :default-value t))
   (:raw-display-p (:documentation "When non-nil, use raw display mode."))
   (:tool (:documentation "Keyword symbol identifying the method to be applied."))
   (:tool-data (:documentation "Arguments for tool invocation."))))


(define-method :initialize :cell-sheet 
  ((:rows (:default-value 16 :documentation "Number of rows in new sheet."))
   (:columns (:default-value 16 
			     :documentation "Number of columns in new sheet."))
   (:name (:default-value "untitled")))
  "Prepare a blank cell sheet of size ROWS x COLUMNS with name NAME."
  (message "Initializing cell-sheet.")
  (with-slots (grid cursor column-stops column-styles display-buffer
		    border-style header-style raw-display-p)
    (setf (@ self :name) name)
    (setf display-buffer (get-buffer-create name))
    (with-current-buffer display-buffer
      (cell-mode)
      (setf cell-sheet-of-current-buffer self)
      (buffer-disable-undo)
	(setf cursor-type nil)
	(setf truncate-lines t)
	(setf buffer-read-only t)
	(goto-char (point-min))
	(setf cursor '(0 0))
	(setf grid (make-grid rows columns))
	(setf column-stops (make-vector (+ 1 columns) 0))
	(setf column-styles (make-vector (+ 1 columns) nil))
	(self>> :render))))


(define-method :show :cell-sheet ()
  "Show the cell sheet in the current window."
  (display-buffer (@ self :display-buffer)))

(define-method :cell-at :cell-sheet (row column)
  "Return the object at ROW, COLUMN in the cell sheet."
  (gref (@ self :grid) row column))

(define-method :clone-fill :cell-sheet (with-object)
  "Clone OBJECT into each of the cells."
  (let ((object (eon-find with-object)))
    (with-slots (grid)
      (dotimes (row (grid-rows grid))
	(dotimes (column (grid-columns grid))
	  (setf (gref grid row column) (>> object :clone)))))))

(define-method :calculate :cell-sheet ()
  "Send the :calculate message to all cells."
  (with-slots (grid)
    (let (cell)
      (dotimes (row (grid-rows grid))
	(dotimes (column (grid-columns grid))
	  (setf cell (gref grid row column))
	  (when cell
	    (>> cell :calculate)))))))

(define-method :apply-tool :cell-sheet ()
  "Apply the selected tool to the selected cell."
  (with-slots (tool tool-data cursor)
    (let ((cell (>> self :cell-at
		    :row (first cursor)
		    :column (second cursor))))
      (when cell
	(>> cell tool :data tool-data)))))

(defun cell-sheet-apply-tool ()
  (interactive)
  (let ((s cell-sheet-of-current-buffer))
    (>> s :apply-tool)
    (>> s :calculate)
    (>> s :render)))

(defun cell-sheet-set-tool (tool)
  (interactive "SKeyword symbol for tool: ")
  (let ((s cell-sheet-of-current-buffer))
    (setf (@ s :tool) tool)
    (>> s :render)))

(defun cell-sheet-set-tool-data ()
  (interactive)
  (let ((s cell-sheet-of-current-buffer))
    (setf (@ s :tool-data) 
	  (read-from-minibuffer "Set tool data to: "
				(@ s :tool-data)))
    (>> s :render)))

(define-method :render :cell-sheet ()
  "Render the cell-sheet to its display buffer."
 (with-slots (grid cursor column-stops column-styles display-buffer
		    border-style header-style raw-display-p
		    tool tool-data)
    (with-current-buffer display-buffer
      (let* ((inhibit-read-only t)
	     (rows (grid-rows grid))
	     (columns (grid-columns grid))
	     (cursor-row (first cursor))
	     (cursor-column (second cursor))
	     (cursor-cell (gref grid cursor-row cursor-column))
	     (widths (make-vector columns 0))
	     (cell nil)
	     (label nil)
	     (column-width 0)
	     (cell-width 0)
	     (row-header-width nil)
	     (face nil))
	(setf header-line-format 
	      (format " :cursor %S :tool %S :tool-data %S "
		      cursor tool tool-data))
	(if raw-display-p
	    ;;
	    ;; raw display mode. just insert all the cell labels with a
	    ;; newline between rows.
	    (progn 
	      (setf cursor-type 'hollow)
	      (delete-region (point-min) (point-max))
	      (dotimes (row rows)
		(dotimes (column columns)
		  (setf cell (gref grid row column))
		  (when cell
		    (setf label (@ cell :label))
		    (when label
		      (insert label))))
		;;
		;; move to next line
		(insert "\n"))
	      ;;
	      ;; move point to where the spreadsheet cursor says it should be
	      (goto-char (+ 1 cursor-column 
			    (* cursor-row 
			       (+ 1 (grid-columns grid))))))
	  ;;
	  ;; TODO fix the bugs in the fancy rendering code
	  ;; 
	  ;; we're not in raw display mode. so, we need to find the
	  ;; column widths and stops before rendering, and draw
	  ;; whatever fancy borders and headers are required.
	  ;;
	  ;; how many digits are in the longest row number?
	  (when header-style
	    (setf row-header-width (length (format "%d" rows))))
	  ;;
	  ;; factor in headers if needed
	  (setf (aref column-stops 0) (or row-header-width 0))
	  ;;
	  ;; now calculate the column widths
	  (dotimes (column columns)
	    (setf column-width 0)
	    (dotimes (row rows)
	      (setf cell (grid-get grid row column))
	      (setf column-width 
		    (max column-width
			 (if cell
			     (+ 1 
				(ceiling (or (@ cell :display-width)
					     (length (or (@ cell :label) 
							 cell-sheet-no-label-string))))
						     (ceiling (length (@ cell :label))))
			   ;;
			   ;; there's no cell at this location. 
			   cell-sheet-blank-width))))
	    (setf (aref widths column) column-width)
	    (when (< column columns)
	      (setf (aref column-stops (+ 1 column))
		    (+ column-width (aref column-stops column)))))
	  ;;
	  ;; now draw to the buffer!
	  (delete-region (point-min) (point-max))
	  ;;
	  ;; draw the column headers if needed
	  (when header-style
	    (cell-sheet-insert-header row-header-width 0)
	    (dotimes (column columns)
	      (cell-sheet-insert-header (- (aref column-stops (+ column 1)) 
						 (aref column-stops column)) column)))
	  ;;
	  (insert "\n")
	  ;;
	  (dotimes (row rows)
	    ;;
	    ;; draw the row header if needed
	    (when header-style
	      (cell-sheet-insert-header row-header-width row))
	    ;; 
	    ;; render the cells in the row
	    (dotimes (column columns)
	      (setf column-width (aref widths column))
	      (setf cell (grid-get grid row column))
	      (if cell
		  ;;
		  ;; we've got a non-blank cell. draw it.
		  (progn
		    (setf face (list 'cell-sheet-default-face
				     (if (evenp column) 'cell-blank-face
				       'cell-blank-odd-face)))
		    (setf cell-width (cell-sheet-draw-cell cell face))
		    ;;
		    ;; fill column if needed 
		    (let ((display-width (@ cell :display-width)))
		      (if display-width
			  ;;
			  ;; we need to insert spacers to make things
			  ;; line up, because inserted images have
			  ;; made the cell's display-width
			  ;; non-integral.
			  (progn 
			    (cell-sheet-insert-spacer (+ 1.0 
							       (- (ceiling display-width)
								  display-width))
							    face)
			    (cell-sheet-insert-blank (- column-width 
							      (ceiling display-width)
							      1) 
							   face))
			;;
			;; no special handling needed
			(cell-sheet-insert-blank (- column-width cell-width)))))
		;; 
		;; we've got a blank cell
		(setf face (if (evenp column) 'cell-blank-face
			     'cell-blank-odd-face))
		(setf cell-width (cell-sheet-insert-blank column-width face)))
	      ;;
	      ;; add text properties to store (ROW COLUMN) for retrieval upon mouse click
	      (let* ((end (point))
		     (beg (- end column-width)))
		(put-text-property beg end 'cell-sheet-position (list row column))))
	    ;;
	    ;; this row is completed. move along.
	    (insert "\n"))
	  ;;
	  ;; we're done rendering. now display the cursor
	  (let* ((cursor-cell-display-width (when cursor-cell 
					      (@ cursor-cell :display-width)))
		 (cursor-width (if cursor-cell-display-width
				   (ceiling cursor-cell-display-width)
				 (- (aref column-stops (+ cursor-column 1)) 
				    (aref column-stops cursor-column)))))
	    ;;
	    ;; move point to the right place
	    (goto-char (point-min))
	    (forward-line (+ 1 cursor-row))
	    (forward-char (aref column-stops cursor-column))
	    ;;
	    ;; adjust cursor when images are present in any cells to left
	    (dotimes (c cursor-column)
	      (let ((cl (grid-get grid cursor-row c)))
		(when cl
		  (when (@ cl :display-width)
		    (backward-char 1)))))
	    ;;
	    ;; now draw the cursor
	    (let* ((p (point)))
	      (put-text-property p (+ p (if cursor-cell
					    (length (@ cursor-cell :label))
					  cell-sheet-blank-width))
				 'face 'cell-cursor-face))
	    ;;
	    (set-buffer-modified-p nil))))))) 	
;;;; cell sheet key bindings

(defvar cell-mode-map nil)
;; (setq cell-mode-map nil)
(if cell-mode-map
    ()
  (setq cell-mode-map (make-sparse-keymap))
  (mapcar (lambda (mapping)
	    (define-key cell-mode-map (car mapping) (cdr mapping)))
	  `(([(up)] . cell-sheet-move-cursor-up)
	    ([(down)] . cell-sheet-move-cursor-down)
	    ([(left)] . cell-sheet-move-cursor-left)
	    ([(right)] . cell-sheet-move-cursor-right)
	    ;; traditional cursor-motion keys
	    ([(control f)] . cell-sheet-move-cursor-right)
	    ([(control b)] . cell-sheet-move-cursor-left)
	    ([(control n)] . cell-sheet-move-cursor-down)
	    ([(control p)] . cell-sheet-move-cursor-up)
	    ([(control e)] . cell-sheet-end-of-line)
	    ([(control a)] . cell-sheet-beginning-of-line)
;; 	    ([( n)] . cell-sheet-move-cursor-down)
;; 	    ([(control p)] . cell-sheet-move-cursor-up)
	    (,(kbd "t") . cell-sheet-apply-tool)
	    (,(kbd "C-t") . cell-sheet-set-tool)
	    (,(kbd "M-t") . cell-sheet-set-tool-data)
	    ;; clicking
;;	    (,(kbd "RET") . cell-sheet-click)
;;      	    ;; paintbox
;; 	    ([(meta up)] . cell-sheet-paintbox-cursor-up)
;; 	    ([(meta down)] . cell-sheet-paintbox-cursor-down)
;; 	    ([(meta left)] . cell-sheet-paintbox-cursor-left)
;; 	    ([(meta right)] . cell-sheet-paintbox-cursor-right)
;; 	    ([(p)] . cell-paint)
;; 	    ([(control o)] . cell-paint-operation-cycle)
;; 	    ([(mouse-3)] . cell-sheet-mouse-paint)
;; 	    ([(meta down-mouse-1)] . cell-sheet-mouse-open)
;; 	    ([(control c) (control p)] . cell-sheet-bang-compile)
	    ;; the mouse
	    ([(mouse-1)] . cell-sheet-mouse-move-cursor)	  
	    ([(drag-mouse-1)] . cell-sheet-mouse-select)	
	    )))
;; 	    ;; cut and paste
;; 	    ([(meta w)] . cell-sheet-copy-to-clipboard)
;; 	    ([(control w)] . cell-sheet-cut-to-clipboard)
;; 	    ([(control y)] . cell-sheet-paste)	    
	    ;; other commands
;; 	    ([(control c)(control c)] . cell-sheet-create-cell)
;; 	    ([(control c)(control d)] . cell-sheet-duplicate))))

;;;; cell-mode

;;;###autoload
(define-derived-mode cell-mode nil
  "Cell" "Mode for object-oriented spreadsheets."
  nil)
      
(define-method :move-cursor :cell-sheet (to)
  "Move the cursor toward TO."
  (with-slots (grid cursor display-buffer raw-display-p)
    (let* ((rows (grid-rows grid))
	   (cols (grid-columns grid))
	   (cursor-row (first cursor))
	   (cursor-column (second cursor))
	   (new-cursor 
	    (if (listp to)
		to
	      (case to
		((:up :down :left :right)
		 ;;
		 ;; move one cell in the specified direction
		 (case to
		   (:up (if (/= 0 cursor-row)
			    (list (- cursor-row 1) cursor-column)
			  cursor))
		   (:left (if (/= 0 cursor-column)
			      (list cursor-row (- cursor-column 1))
			    cursor))
		   (:down (if (< cursor-row (- rows 1))
			      (list (+ cursor-row 1) cursor-column)
			    cursor))
		   (:right (if (< cursor-column (- cols 1))
			       (list cursor-row (+ cursor-column 1))
			     cursor))))
		;;
		;; move to beginning or end of line
		(:beginning-of-line 
		 (list cursor-row 0))
		(:end-of-line 
		 (list cursor-row (- cols 1)))))))
      (setf cursor new-cursor)
      ;;
      ;; choose technique for display
      ;;
      (if raw-display-p
	  ;;
	  ;; just move point to where cursor should go
	  (progn
	    (let ((buffer-position (+ 1 (second new-cursor)
				      (* (first new-cursor)
					 (+ 1 (grid-columns grid))))))
	      (goto-char buffer-position))))
      ;;
      ;; now render
      (>> self :render))))

(defun cell-sheet-move-cursor (direction)
  (interactive)
  (>> cell-sheet-of-current-buffer :move-cursor :to direction))
  
(defun cell-sheet-move-cursor-up ()
  (interactive)
  (cell-sheet-move-cursor :up))

(defun cell-sheet-move-cursor-left ()
  (interactive)
  (cell-sheet-move-cursor :left))

(defun cell-sheet-move-cursor-down ()
  (interactive)
  (cell-sheet-move-cursor :down))

(defun cell-sheet-move-cursor-right ()
  (interactive)
  (cell-sheet-move-cursor :right))

(defun cell-sheet-beginning-of-line ()
  (interactive)
  (cell-sheet-move-cursor :beginning-of-line))

(defun cell-sheet-end-of-line ()
  (interactive)
  (cell-sheet-move-cursor :end-of-line))

;; TODO update these so that they work again
 
;; ;;;; Adding rows and columns

;; (defun cell-sheet-insert-row ()
;;   (interactive)
;;   (with-current-cell-sheet 
;;    (setf (cell-sheet-grid sheet) (grid-insert-row grid cursor-row))
;;    (cell-sheet-update)))

;; (defun cell-sheet-insert-column ()
;;   (interactive)
;;   (with-current-cell-sheet
;;    (let ((columns (+ 1 (grid-columns (cell-sheet-grid sheet)))))
;;      (setf (cell-sheet-grid sheet) (grid-insert-column grid cursor-column))
;;      (setf (cell-sheet-column-stops sheet) (make-vector (+ 1 columns) 0))))
;;   (cell-sheet-update))

;; ;;;; Removing rows and columns

;; (defun cell-sheet-kill-row ()
;;   (interactive)
;;   (with-current-cell-sheet
;;    (setf (cell-sheet-grid sheet) (grid-delete-row grid cursor-row))
;;    (cell-sheet-update)))

;; (defun cell-sheet-kill-column ()
;;   (interactive)
;;   (with-current-cell-sheet
;;    (setf (cell-sheet-grid sheet) (grid-delete-column grid cursor-column))
;;    (cell-sheet-update)))
				  				 			     
;;;; Mouse support

;; (defun cell-sheet-mouse-move-cursor (event)
;;   (interactive "e")
;;   (with-current-cell-sheet
;;    (when event
;;      (destructuring-bind (event-type position &optional ignore) event
;;        (let* ((clicked-position (posn-point position))
;; 	      (clicked-cell (get-text-property clicked-position 'cell-mode-position)))
;; 	 ;;
;; 	 ;; are we in raw display mode? 
;; 	 (when (getf properties :raw-display)
;; 	   (goto-char clicked-position)
;; 	   ;;
;; 	   ;; bounds check 
;; 	   (let ((clicked-row (/ clicked-position (+ 1 (grid-columns grid))))
;; 		 (clicked-column (+ -1 (% clicked-position (+ 1 (grid-columns grid))))))
;; 	     (when (and (<= 0 clicked-row)
;; 			(<= 0 clicked-column)
;; 			(< clicked-row (grid-rows grid))
;; 			(< clicked-column (grid-columns grid)))
;; 	       (setf (cell-sheet-cursor sheet) 
;; 		     (list clicked-row clicked-column)))))
;; 	 ;;
;; 	 ;; not in raw display
;; 	 (when clicked-cell
;; 	   (destructuring-bind (clicked-row clicked-column) clicked-cell
;; 	     (setf (cell-sheet-cursor sheet) clicked-cell)
;; 	     ;; clear selection
;; 	     (setf (cell-sheet-selection sheet) nil)
;; 	     (cell-sheet-render sheet))))))))

;;;; Customization

(defgroup cell-mode nil
  "Options for cell-mode."
  :group 'applications)

;;;; Utility functions for rendering the spreadsheet

(defun cell-sheet-draw-cell (cell &optional face)
  (let ((label (@ cell :label)))
    (insert label)
    (length label)))

(defun cell-sheet-insert-blank (width &optional face)
  (when (> width 0)
    (insert (propertize (make-string width ? ) 
			'face (or face 'cell-blank-face)))
    width))

(defun cell-sheet-insert-spacer (width &optional face)
  (insert "Q")
  (backward-char)
  (put-text-property (point) (1+ (point)) 'display
		     `(space . (:width ,width)))
  (when face
    (put-text-property (point) (1+ (point)) 'face face))
  (forward-char))

(defun cell-sheet-insert-header (width number)
  (let* ((label (format "%d" number))
	 (blank (make-string (- width (length label)) ? ))
	 (face (if (evenp number)
		  'cell-axis-face
		'cell-axis-odd-face)))
    (insert (propertize (concat blank label)
			'face face))))

;;;; Faces

;; TODO get rid of useless ones

(defface cell-default-face '((t (:foreground "white")))
  "Face for cells." :group 'cell)

(defface cell-bang-face '((t (:foreground "cyan" :background "red")))
  "Face for bang cells." :group 'cell)

(defface cell-lisp-face '((t (:foreground "gray50" :background "white")))
  "Face for cells that represent simple lisp operations." :group 'cell)

(defface cell-send-var-face '((t (:foreground "white" :background "mediumblue")))
  "Face for cells that represent variable references." :group 'cell)

(defface cell-action-face '((t (:foreground "yellow" :background "red" :bold t :weight bold)))
  "Face for cells that perform an action." :group 'cell)

(defface cell-receive-var-face '((t (:foreground "white" :background "mediumblue")))
  "Face for cells that represent variable references." :group 'cell)

(defface cell-print-face '((t (:foreground "gray80" :background "gray40"))) 
  "Face for printed value cells." :group 'cell)

(defface cell-comment-face '((t (:foreground "white")))
  "Face for cells that simply label stuff." :group 'cell)

(defface cell-file-face '((t (:foreground "yellowgreen")))
  "Face for simple links to files." :group 'cell)

(defface cell-wiki-face '((t (:foreground "white" :background "forestgreen" )))
  "Face for wiki cells." :group 'cell)

(defface cell-elisp-face '((t (:foreground "white" :background "OliveDrab" )))
  "Face for wiki cells." :group 'cell)

(defface cell-keyword-face '((t (:foreground "black" :background "Gold" )))
  "Face for wiki cells." :group 'cell)

(defface cell-subr-face '((t (:foreground "gray40" :background "white" )))
  "Face for wiki cells." :group 'cell)

(defface cell-cursor-face '((t (:background "yellow" :foreground "black")))
  "Face for selected cell." :group 'cell)

(defface cell-selection-face '((t (:background "seagreen" :foreground "cyan")))
  "Face for multi-cell selection." :group 'cell)

(defface cell-compute-cursor-face '((t (:box (:line-width 1 :color "red"))))
  "Face for compute cursor." :group 'cell)

(defface cell-text-face '((t (:foreground "yellow")))
  "Face for text entry cells." :group 'cell)

(defface cell-blank-face '((t (:background 
			       "gray20" 
			       :box 
			       (:line-width 1 :color "grey30"))))
  "Face for blank cells." :group 'cell)

(defface cell-blank-odd-face '((t (:background 
				   "gray18" 
				   :box 
				   (:line-width 1 :color "grey29"))))
  "Face for blank cells in odd columns" :group 'cell)
 
(defface cell-axis-face '((t :foreground "gray45" 
			     :background "gray25" 
			     :box (:line-width 1 :color "grey30")))
  "Face for numbered axes."  :group 'cell)
      			   
(defface cell-axis-odd-face '((t :foreground "gray42" 
			     :background "gray23" 
			     :box (:line-width 1 :color "grey29")))
  "Face for numbered axes in odd columns." :group 'cell)
	   
(provide 'cell)

;;; cell.el ends here
