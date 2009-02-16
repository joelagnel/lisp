;;;; jpeg-mode.el -- major mode for JPEG files
;;; Time-stamp: <2006-03-22 16:06:49 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'jpeg-mode)

;;;; Notes from development:

;; The header of a JPEG file consists of a series of blocks, called "markers".
;; The image height and width are stored in a marker of type SOFn (Start Of
;; Frame, type N).  To find the SOFn you must skip over the preceding markers;
;; you don't have to know what's in the other types of markers, just use their
;; length words to skip over them.  The minimum logic needed is perhaps a page
;; of C code. 

;; Then, within some of the markers, there may be further structure
;; that we can display, such as the TIFF tags within an APP1

;; rdjpgcom.c in the IJG distribution was another useful reference

;; Perl code can be found in wwwis, from http://www.tardis.ed.ac.uk/~ark/wwwis/.

(defvar jpeg-magic "\377\330")

(defconst jpeg-markers 
  [
   jpeg-marker-SOF0                     ; M_SOF0  = 0xc0,
   jpeg-marker-SOF1                     ; M_SOF1  = 0xc1,
   jpeg-marker-SOF2                     ; M_SOF2  = 0xc2,
   jpeg-marker-SOF3                     ; M_SOF3  = 0xc3,
   jpeg-marker-DHT                      ; M_DHT   = 0xc4,
   jpeg-marker-SOF5                     ; M_SOF5  = 0xc5,
   jpeg-marker-SOF6                     ; M_SOF6  = 0xc6,
   jpeg-marker-SOF7                     ; M_SOF7  = 0xc7,
   jpeg-marker-JPG                      ; M_JPG   = 0xc8,
   jpeg-marker-SOF9                     ; M_SOF9  = 0xc9,
   jpeg-marker-SOF10                    ; M_SOF10 = 0xca,
   jpeg-marker-SOF11                    ; M_SOF11 = 0xcb,
   jpeg-marker-DAC                      ; M_DAC   = 0xcc,
   jpeg-marker-SOF13                    ; M_SOF13 = 0xcd,
   jpeg-marker-SOF14                    ; M_SOF14 = 0xce,
   jpeg-marker-SOF15                    ; M_SOF15 = 0xcf,
  
   jpeg-marker-RST0                     ; M_RST0  = 0xd0,
   jpeg-marker-RST1                     ; M_RST1  = 0xd1,
   jpeg-marker-RST2                     ; M_RST2  = 0xd2,
   jpeg-marker-RST3                     ; M_RST3  = 0xd3,
   jpeg-marker-RST4                     ; M_RST4  = 0xd4,
   jpeg-marker-RST5                     ; M_RST5  = 0xd5,
   jpeg-marker-RST6                     ; M_RST6  = 0xd6,
   jpeg-marker-RST7                     ; M_RST7  = 0xd7,
   jpeg-marker-SOI                      ; M_SOI   = 0xd8,
   jpeg-marker-EOI                      ; M_EOI   = 0xd9,
   jpeg-marker-SOS                      ; M_SOS   = 0xda,
   jpeg-marker-DQT                      ; M_DQT   = 0xdb,
   jpeg-marker-DNL                      ; M_DNL   = 0xdc,
   jpeg-marker-DRI                      ; M_DRI   = 0xdd,
   jpeg-marker-DHP                      ; M_DHP   = 0xde,
   jpeg-marker-EXP                      ; M_EXP   = 0xdf,
  
   jpeg-marker-APP0                     ; M_APP0  = 0xe0,
   jpeg-marker-APP1                     ; M_APP1  = 0xe1,
   jpeg-marker-APP2                     ; M_APP2  = 0xe2,
   jpeg-marker-APP3                     ; M_APP3  = 0xe3,
   jpeg-marker-APP4                     ; M_APP4  = 0xe4,
   jpeg-marker-APP5                     ; M_APP5  = 0xe5,
   jpeg-marker-APP6                     ; M_APP6  = 0xe6,
   jpeg-marker-APP7                     ; M_APP7  = 0xe7,
   jpeg-marker-APP8                     ; M_APP8  = 0xe8,
   jpeg-marker-APP9                     ; M_APP9  = 0xe9,
   jpeg-marker-APP10                    ; M_APP10 = 0xea,
   jpeg-marker-APP11                    ; M_APP11 = 0xeb,
   jpeg-marker-APP12                    ; M_APP12 = 0xec,
   jpeg-marker-APP13                    ; M_APP13 = 0xed,
   jpeg-marker-APP14                    ; M_APP14 = 0xee,
   jpeg-marker-APP15                    ; M_APP15 = 0xef,
  

   jpeg-marker-JPG0                     ; M_JPG0  = 0xf0,
   jpeg-marker-JPG1                     ; M_JPG1  = 0xf1,
   jpeg-marker-JPG2                     ; M_JPG2  = 0xf2,
   jpeg-marker-JPG3                     ; M_JPG3  = 0xf3,
   jpeg-marker-JPG4                     ; M_JPG4  = 0xf4,
   jpeg-marker-JPG5                     ; M_JPG5  = 0xf5,
   jpeg-marker-JPG6                     ; M_JPG6  = 0xf6,
   jpeg-marker-JPG7                     ; M_JPG7  = 0xf7,
   jpeg-marker-JPG8                     ; M_JPG8  = 0xf8,
   jpeg-marker-JPG9                     ; M_JPG9  = 0xf9,
   jpeg-marker-JPG10                    ; M_JPG9  = 0xfa,
   jpeg-marker-JPG11                    ; M_JPG9  = 0xfb,
   jpeg-marker-JPG12                    ; M_JPG9  = 0xfc,
   jpeg-marker-JPG13                    ; M_JPG13 = 0xfd,
   jpeg-marker-COM                      ; M_COM   = 0xfe,
  
   ]
  "Possible marker values. Add 0xc0 and then index into this table."
)

(defun jpeg-sof (start length key descr)
  "START is the start of the actual data, after the length."
  (save-excursion
    (goto-char start)
    (let ((precision (char-after (point)))
	  (height (jpeg-read-two-bytes))
	  (width (jpeg-read-two-bytes))
	  (n_components (char-after (point))))
      (list key descr precision height width n_components)
      )))

(defun jpeg-marker-SOF0 (start length tag-no)
 "Process a SOF0 "
  (jpeg-sof start length  'jpeg-marker-SOF0 "Baseline"))

(defun jpeg-marker-SOF1 (start length tag-no)
 "Process a SOF1 "
  (jpeg-sof start length  'jpeg-marker-SOF1 "Extended sequential"))

(defun jpeg-marker-SOF2 (start length tag-no)
 "Process a SOF2 "
  (jpeg-sof start length  'jpeg-marker-SOF2 "Progressive"))

(defun jpeg-marker-SOF3 (start length tag-no)
 "Process a SOF3 "
  (jpeg-sof start length  'jpeg-marker-SOF3 "Lossless"))

(defun jpeg-marker-SOF5 (start length tag-no)
 "Process a SOF5 "
  (jpeg-sof start length  'jpeg-marker-SOF5 "Differential sequential"))

(defun jpeg-marker-SOF6 (start length tag-no)
 "Process a SOF6 "
  (jpeg-sof start length  'jpeg-marker-SOF6 "Differential progressive"))

(defun jpeg-marker-SOF7 (start length tag-no)
 "Process a SOF7 "
  (jpeg-sof start length  'jpeg-marker-SOF7 "Differential lossless"))

(defun jpeg-marker-SOF9 (start length tag-no)
 "Process a SOF9 "
  (jpeg-sof start length  'jpeg-marker-SOF9 "Extended sequential, arithmetic coding"))

(defun jpeg-marker-SOF10 (start length tag-no)
 "Process a SOF10 "
  (jpeg-sof start length  'jpeg-marker-SOF10 "Progressive, arithmetic coding"))

(defun jpeg-marker-SOF11 (start length tag-no)
 "Process a SOF11 "
  (jpeg-sof start length  'jpeg-marker-SOF11 "Lossless, arithmetic coding"))

(defun jpeg-marker-SOF13 (start length tag-no)
 "Process a SOF13 "
  (jpeg-sof start length  'jpeg-marker-SOF13 "Differential sequential, arithmetic coding"))

(defun jpeg-marker-SOF14 (start length tag-no)
 "Process a SOF14 "
  (jpeg-sof start length  'jpeg-marker-SOF14 "Differential progressive, arithmetic coding"))
 
(defun jpeg-marker-SOF15 (start length tag-no)
 "Process a SOF15 "
  (jpeg-sof start length  'jpeg-marker-SOF15 "Differential lossless, arithmetic coding"))

(defvar jfif-extension-handlers
  '((1 . "No thumbnail")
    (16 . (format "JPEG thumbnail %dx%d %dx%ddpi" width height x-density y-density))
    (17 . (format "1-byte-per-pixel thumbnail %dx%d %dx%ddpi" width height x-density y-density))
    (19 . (format "3-byte-per-pixel thumbnail %dx%d %dx%ddpi" width height x-density y-density))
    (t . "Unknown extension"))
  "Expressions to handle each JFIF extension.")

(defun jpeg-marker-APP0 (start length jpeg-tag-no)
  "Process an APP0 tag as used by JFIF."
  (if (string= "JFIF" (buffer-substring-no-properties start (+ start 4)))
      (let ((version-high (char-after (+ start 5))) 
	    (version-low (char-after (+ start 6)))
	    (extension (char-after (+ start 7)))
	    (x-density (jpeg-read-two-bytes (+ start 8)))
	    (y-density (jpeg-read-two-bytes (+ start 10)))
	    (width (char-after (+ start 12)))
	    (height (char-after (+ start 13))))
	(concat
	 (format "JFIF %d.%d; " version-high version-low)
	 (eval (cdr (or (assoc extension jfif-extension-handlers)
			(assoc t jfif-extension-handlers))))))
    "Non-JFIF APP0"))

(defconst tiff-tag-unit-sizes 
  [ 0 1 1 2 4 8 1 1 2 4 8 4 8 ]
  "The number of bytes for each tag data type.")

(defconst tiff-tag-types
  [ undefined
    unsigned-byte ascii-text
    unsigned-short unsigned-long unsigned-rational
    signed-byte undefined signed-short signed-long signed-rational single-float double-float]
  "Names for the tag data types.")

(defun convert-four-bytes (raw-data-string order)
  "Convert the first four bytes of RAW-DATA-STRING according to ORDER."
  (if (eq order 'little-first)
      (+ (* (+ (* (+ (* (aref raw-data-string 3) 256)
		     (aref raw-data-string 2)) 256)
	       (aref raw-data-string 1)) 256)
	 (aref raw-data-string 0))
    (+ (* (+ (* (+ (* (aref raw-data-string 0) 256)
		   (aref raw-data-string 1)) 256)
	     (aref raw-data-string 2)) 256)
       (aref raw-data-string 3))))

(defvar tiff-tag-to-lisp-converters
  '((ascii-text . (substring tag-raw-data 0 (string-match "\0" tag-raw-data)))
    (unsigned-byte . (aref tag-raw-data 0))
    (signed-byte . (aref tag-raw-data 0))
    (unsigned-short . (if (eq byte-order 'little-first)
			  (+ (* (aref tag-raw-data 1) 256) (aref tag-raw-data 0))
			(+ (* (aref tag-raw-data 0) 256) (aref tag-raw-data 1))))
    (signed-short .  (if (eq byte-order 'little-first)
			 (+ (* (aref tag-raw-data 1) 256) (aref tag-raw-data 0))
		       (+ (* (aref tag-raw-data 0) 256) (aref tag-raw-data 1))))
    (unsigned-long. (convert-four-bytes tag-raw-data byte-order))
    (signed-long . (convert-four-bytes tag-raw-data byte-order))
    (unsigned-rational . (/ (float (convert-four-bytes tag-raw-data byte-order))
			    (float (convert-four-bytes (substring tag-raw-data 4) byte-order))))
    (signed-rational . (/ (float (convert-four-bytes tag-raw-data byte-order))
			  (float (convert-four-bytes (substring tag-raw-data 4) byte-order))))
    (t . nil))
  "Expressions to convert TIFF tags to lisp")

(defun tiff-tag-as-lisp (tag-raw-data tag-format tag-n-parts byte-order)
  "Convert TAG-RAW-DATA from TAG-FORMAT to Lisp, treating it as TAG-N-PARTS, in BYTE-ORDER."
  (eval (cdr (or (assoc tag-format tiff-tag-to-lisp-converters)
		 (assoc t tiff-tag-to-lisp-converters)))))

(defvar jpeg-tiff-tags-by-number
  '((270 . tiff-tag-image-description)
    (271 . tiff-tag-make)
    (272 . tiff-tag-model)
    (274 . tiff-tag-orientation)
    (282 . tiff-tag-x-resolution)
    (283 . tiff-tag-y-resolution)
    (296 . tiff-tag-resolution-unit)
    (305 . tiff-tag-software)
    (306 . tiff-tag-date-time-last-modified)
    (513 . tiff-tag-jpeg-if-offset)
    (514 . tiff-tag-jpeg-if-byte-count)
    (529 . tiff-tag-y-cb-cr-coefficients)
    (531 . tiff-tag-y-cb-cr-positioning)
    (532 . tiff-tag-reference-black-white)
    (33434 . tiff-tag-exposure-time)
    (33437 . tiff-tag-f-number)
    (34850 . tiff-tag-exposure-program)
    (34855 . tiff-tag-iso-speed-ratings)
    (36864 . tiff-tag-exif-version)
    (36867. tiff-tag-date-time-original)
    (36868 . tiff-tag-date-time-digitized)
    (37121 . tiff-tag-components-configuration)
    (37122 . tiff-tag-compressed-bits-per-pixel)
    (37377 . tiff-tag-shutter-speed-value)
    (37378 . tiff-tag-aperture-value)
    (37379 . tiff-tag-brightness-value)
    (37380 . tiff-tag-exposure-bias-value)
    )
  "Alist of tag numbers to their names.")

(defvar jpeg-hide-analyzed-data nil
  "*Whether to make raw data invisible when a converted version is displayed.")

(defvar tiff-tag-data-recorders
  '((tiff-tag-date-time-last-modified . (let* ((raw-time (mapcar 'string-to-int (split-string tag-lisp-data "[ :]")))
					       (encoded-time (apply 'encode-time (nreverse raw-time)))
					       (time-string (current-time-string encoded-time)))
					  (setq jpeg-details (cons (cons 
								    (intern (format "ifd%d-%s-raw-string"
										    ifd-number
										    (symbol-name tag-name)))
								    tag-lisp-data)
								   (cons (cons (intern (format "ifd%d-%s-encoded"
											       ifd-number
											       (symbol-name tag-name)))
									       encoded-time)
									 (cons (cons (intern (format "ifd%s-%s-emacs-time-string"
												     ifd-number
												     (symbol-name tag-name)))
										     (current-time-string encoded-time))
									       jpeg-details))))))

    (tiff-tag-date-time-original . (let* ((raw-time (mapcar 'string-to-int (split-string tag-lisp-data "[ :]")))
					(encoded-time (apply 'encode-time (nreverse raw-time)))
					(time-string (current-time-string encoded-time)))
				   (setq jpeg-details (cons (cons 
							     (intern (format "ifd%d-%s-raw-string"
									     ifd-number
									     (symbol-name tag-name)))
							     tag-lisp-data)
							    (cons (cons (intern (format "ifd%d-%s-encoded"
											ifd-number
											(symbol-name tag-name)))
									encoded-time)
								  (cons (cons (intern (format "ifd%s-%s-emacs-time-string"
											      ifd-number
											      (symbol-name tag-name)))
									      (current-time-string encoded-time))
									jpeg-details))))))


    (tiff-tag-date-time-digitized . (let* ((raw-time (mapcar 'string-to-int (split-string tag-lisp-data "[ :]")))
					 (encoded-time (apply 'encode-time (nreverse raw-time)))
					 (time-string (current-time-string encoded-time)))
				    (setq jpeg-details (cons (cons 
							      (intern (format "ifd%d-%s-raw-string"
									      ifd-number
									      (symbol-name tag-name)))
							      tag-lisp-data)
							     (cons (cons (intern (format "ifd%d-%s-encoded"
											 ifd-number
											 (symbol-name tag-name)))
									 encoded-time)
								   (cons (cons (intern (format "ifd%s-%s-emacs-time-string"
											       ifd-number
											       (symbol-name tag-name)))
									       (current-time-string encoded-time))
									 jpeg-details))))))
    (t . (setq jpeg-details (cons (cons (intern (format "ifd%d-%s" ifd-number tag-name))
					tag-lisp-data)
				  jpeg-details))))
  "How to record each kind of tiff tag.")

(defun jpeg-marker-APP1 (start length jpeg-tag-no)
  "Parse an APP1 segment, as used by EXIF."
  (let* ((app1-type-string (buffer-substring-no-properties start (+ start 4)))
	 (app1-order-string (buffer-substring-no-properties (+ start 6) (+ start 8)))
	 (byte-order (if (string= app1-order-string "MM") 'big-first 'little-first))
	 (ifd-offset (jpeg-read-four-bytes (+ start 10) byte-order))
	 (ifd-start (+ start ifd-offset 6))
	 (ifd0-base (+ start ifd-offset -2))
	 (ifd-base ifd0-base)
	 (ifd-number -1)
	 (n-ifd-entries (jpeg-read-two-bytes ifd-start byte-order))
	 (i 0)
	 (entries nil)
	 (result ""))
    (while (not (or (zerop ifd-offset)
		    (>= ifd-base (buffer-size))))
      (setq ifd-number (1+ ifd-number))
      (message "Reading IFD%d(%d entries) at %d" ifd-number n-ifd-entries ifd-base)
      ;; todo: get second and subsequent IFDs read correctly -- must adjust start, base etc
      ;; the document says that they are relative to the start of the first IFD, (and so not the current one)
      ;; (message "IFD start is %d, char there is %c" ifd-start (char-after ifd-start))
      ;; (message "IFD base is %d, char there is %c" ifd-base (char-after ifd-base))
      (while (< i n-ifd-entries)
	(message "Reading entry %d of ifd %d" i ifd-number)
	(let* ((tag-number (jpeg-read-two-bytes nil byte-order))
	       (tag-name (cdr (assoc tag-number jpeg-tiff-tags-by-number)))
	       (tag-format (jpeg-read-two-bytes nil byte-order))
	       (tag-n-parts (jpeg-read-four-bytes nil byte-order))
	       (tag-offset (jpeg-read-four-bytes nil byte-order))
	       (tag-unit-size (if (< tag-format (length tiff-tag-unit-sizes))
				  (aref tiff-tag-unit-sizes tag-format)
				1))
	       (tag-size (* tag-n-parts tag-unit-size))
	       (tag-type (if (< tag-format (length tiff-tag-types))
			     (aref tiff-tag-types tag-format)
			   nil))
	       (tag-overlay (make-overlay (- (point) 12) (point)))
	       (tag-data-overlay (if (> tag-size 4) 
				     (make-overlay (+ ifd0-base tag-offset)
						   (+ ifd0-base tag-offset tag-size))
				   nil))
	       (tag-raw-data (if (> tag-size 4)
				 (buffer-substring-no-properties (+ ifd0-base tag-offset)
								 (+ ifd0-base tag-offset tag-size))
			       (buffer-substring-no-properties (- (point) 4) (point))))
	       (tag-lisp-data (tiff-tag-as-lisp tag-raw-data tag-type tag-n-parts byte-order))
	       (tag-description
		(format "%d.%d.%d: TIFF %d(%04x)%s, %d parts, offset %d, type %S, size %d, value %S"
			jpeg-tag-no ifd-number i
			tag-number tag-number (if tag-name (format "=%S" tag-name) "")
			tag-n-parts
			tag-offset tag-type tag-size tag-lisp-data))
	       )
	  (overlay-put tag-overlay 'before-string
		       (concat "\n" (propertize tag-description
						'face (cons 'background-color "orange")) "\n"))
	  (when (and jpeg-hide-analyzed-data
		     tag-lisp-data)
	    (overlay-put tag-overlay 'invisible t))
	  (setq jpeg-overlays (cons tag-overlay jpeg-overlays))
	  (when tag-data-overlay
	    (overlay-put tag-data-overlay 'before-string
			 (concat "\n" (propertize tag-description
						  'face (cons 'background-color "pale green")) "\n"))
	    (when (and jpeg-hide-analyzed-data
		       tag-lisp-data)
	      (overlay-put tag-data-overlay 'invisible t))
	    (setq jpeg-overlays (cons tag-data-overlay jpeg-overlays)))
	  (message "Tag is %s" tag-name)
	  (eval (cdr (or (assoc tag-name tiff-tag-data-recorders)
			 (assoc t tiff-tag-data-recorders))))
	  (setq i (1+ i))))
      (message "Reading new offset from %d" (point))
      (setq ifd-offset (jpeg-read-four-bytes nil byte-order)
	    ifd-base (+ ifd-base ifd-offset)
	    n-ifd-entries (jpeg-read-two-bytes ifd-base byte-order)
	    i 0)
      (message "got offset=%d base=%d n-entries=%d" ifd-offset ifd-base n-ifd-entries)
      ;; return the details
      (setq result
	    (concat result
		    (format "%s \"%s\"-->%S, %d offset, %d entries;"
			    app1-type-string app1-order-string byte-order ifd-offset n-ifd-entries))))
    result))

(defun jpeg-next-marker (&optional from)
  "Move point to the next marker, and return it.
Point is left on the character after the marker, which is the first
byte of the length."
  (condition-case evar
      (let (c)
	(when from (goto-char from))
	;; skip to padding
	(while (/= (char-after (point)) 255)
	  (forward-char))
	;; skip over padding
	(while (= (setq c (char-after (point))) 255)
	  (forward-char))
	;; leave it on the char after the marker
	(forward-char)
	;; return the marker
	c)
    (error 0)))

(defun jpeg-read-two-bytes (&optional place order)
  "Read two bytes at point.
If given PLACE, go there first.
If also passed 'little-first, it reads it little-endian,
otherwise big-endian."
  (when place (goto-char place))
  (let* ((first (char-after (point)))
	 (second (char-after (1+ (point)))))
    (forward-char 2)
    (if (eq order 'little-first)
	(+ (* second 256) first)
      (+ (* first 256) second))))

(defun jpeg-read-four-bytes (&optional place order)
  "Read four bytes at point.
If given PLACE, go there first.
If also passed 'little-first, it reads it little-endian,
otherwise big-endian."
  (when place (goto-char place))
  (let* ((first (char-after (point)))
	 (second (char-after (1+ (point))))
	 (third (char-after (+ 2 (point))))
	 (fourth (char-after (+ 3 (point)))))
    (forward-char 4)
    (if (eq order 'little-first)
	(+ (* (+ (* (+ (* fourth 256) third) 256) second) 256) first)
      (+ (* (+ (* (+ (* first 256) second) 256) third) 256) fourth))))

(defconst jpeg-SOI 216)

(defun jpeg-make-description (marker-name marker-start marker-length colour &optional details)
  ""
  (concat "\n"
	  (propertize (format "Bytes %d..%d: %s, %d bytes long%s"
			      marker-start (+ marker-start marker-length)
			      marker-name marker-length
			      (if details
				  (prin1-to-string details)
				""))
		      'face (cons 'background-color colour))
	  "\n"))

(defvar jpeg-just-colour-tags nil
  "*Whether to simply colour the tags in.")

(defvar jpeg-overlays nil
  "The JPEG tag description overlays of the current buffer.")

(make-variable-buffer-local 'jpeg-overlays)

(defvar jpeg-overlays-array nil
  "An array containing the same as jpeg-overlays.")

(make-variable-buffer-local 'jpeg-overlays-array)

(defvar jpeg-current-tag 0
  "The current tag number.")

(make-variable-buffer-local 'jpeg-current-tag)

(defvar jpeg-details nil
  "An alist of what is known about the current file.")

(make-variable-buffer-local 'jpeg-details)

(defun jpeg-find-current-tag ()
  "Return the tag number that point is within."
  (catch 'found
    (let ((i (1- (length jpeg-overlays-array))))
      (while (>= i 0)
	(let ((overlay (aref jpeg-overlays-array i)))
	  (message "%d: %d..%d" i (overlay-start overlay) (overlay-end overlay))
	  (if (and (>= (point) (overlay-start overlay))
		   (<= (point) (overlay-end overlay)))
	      (throw 'found i)
	    (setq i (1- i)))))
      nil)))

(defun jpeg-goto-tag (n)
  "Go to tag N."
  (interactive "N")
  (goto-char (overlay-start (aref jpeg-overlays-array
				  (max 0 (min n (1- (length jpeg-overlays-array))))))))

(defun jpeg-next-tag ()
  "Move to the next tag."
  (interactive)
  (jpeg-goto-tag (1+ (jpeg-find-current-tag))))

(defun jpeg-previous-tag ()
  "Move to the previous tag."
  (interactive)
  (jpeg-goto-tag (1- (jpeg-find-current-tag))))

(defvar c0 (* 12 16)
  "The hex constant c0.")

(defun jpeg-parse (colour)
  "Parse a series of JPEG tags. Use COLOUR to mark them in the buffer."
  ;; (message "In jpeg-parse(%s) at %d" colour (point))
  (let* ((start (1- (point)))
	 (magic (buffer-substring-no-properties (+ 1 start) (+ 3 start)))
	 marker
	 (marker-count -1)
	 (details nil)
	 )
    ;; (unless (string= magic jpeg-magic) (message "Not a jpeg file: magic is %s, should be %s" magic jpeg-magic))
    (setq jpeg-overlays (cons (make-overlay (+ 0 start) (+ 3 start))
			      jpeg-overlays))
    (overlay-put (car jpeg-overlays) 'before-string
		 (propertize "JPEG magic number: " 'face (cons 'background-color colour)))
    (while (/= 0 (setq marker (jpeg-next-marker)))
      (setq marker-count (1+ marker-count))
      (let* ((marker-parameter-length (jpeg-read-two-bytes))
	     (marker-name (if (>= marker c0)
			      (aref jpeg-markers (- marker c0))
			    nil))
	     (overlay (make-overlay (- (point) 4) (+ (point) marker-parameter-length -2)))
	     (details (if (functionp marker-name)
			  (setq details (cons (save-excursion
						(funcall marker-name
							 (point)
							 marker-parameter-length
							 marker-count))
					      details))
			nil)))
	(overlay-put overlay 'details details)
	(setq jpeg-overlays (cons overlay jpeg-overlays))
	(when nil
	  (message "Marker %d=%x (%s), length %d, at %d"
		   marker-count
		   marker
		   marker-name
		   marker-parameter-length
		   (- (point) 3)))
	(if jpeg-just-colour-tags
	    (overlay-put overlay 'face
			 (cons 'background-color
			       (if (evenp marker-count)
				   "red"
				 "green")))
	  ;; (overlay-put overlay 'invisible t)
	  (overlay-put overlay 'before-string
		       (jpeg-make-description marker-name
					      (point)
					      marker-parameter-length
					      colour
					      details))
	  )
	(forward-char (- marker-parameter-length 2))
	))))

(defvar jpeg-mode-map (make-keymap "JPEG")
  "Keymap for JPEG mode.")

(suppress-keymap jpeg-mode-map)

(define-key jpeg-mode-map "\C-n" 'jpeg-next-tag)
(define-key jpeg-mode-map "\C-p" 'jpeg-previous-tag)

(defun jpeg-mode ()
  "Major mode for JPEG files.
The file is annotated with overlays (these do not change the actual contents
of the file) to explain parts of the metadata.
JPEG markers (sections of the file) are described in yellow.
Markers which contain TIFF-style data are annotated with their tag tables
in orange and the actual data in pale green. (The annotations are the same.)
Special commands are:
\\{jpeg-mode-map}"
  (interactive)
  (setq buffer-read-only t)
  (mapcar 'delete-overlay jpeg-overlays)
  (kill-all-local-variables)
  (use-local-map jpeg-mode-map)
  (let ((subtype-string (buffer-substring-no-properties  7 11))
	(version-high (char-after (+ 12 (point))))
	(version-low (char-after (+ 13 (point))))
	)
    (setq major-mode 'jpeg-mode
	  mode-name (format "JPEG(%s)" subtype-string)
	  jpeg-overlays nil
	  jpeg-details nil)
    ;; (message "Subtype %s, version %d.%02d" subtype-string version-high version-low)
    )
  (goto-char 2)
  (jpeg-parse "yellow")
  (setq jpeg-overlays (nreverse jpeg-overlays)
	jpeg-overlays-array (apply 'vector jpeg-overlays)
	jpeg-current-tag 0)
  (jpeg-goto-tag 0))

;;; end of jpeg-mode.el
