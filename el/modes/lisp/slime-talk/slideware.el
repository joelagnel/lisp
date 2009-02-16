;; slideware.el -- Slide-presentation mode

(require 'slime)

(defvar slideware-dir "/home/ike/repository/lisp/el/modes/lisp/slime-talk")

(global-font-lock-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq slime-compilation-finished-hook nil)
(setq slime-words-of-encouragement
      '("Doesn't that feel better!"))
(defvar startup-action1
  (find-file "notes.lisp"))

(set-face-attribute
 (defface slide-heading-1-face ()
   "Face for level-1 slide headings.")
 nil :font "-*-helvetica-*-i-*-*-*-240-*-*-*-*-*-*" :foreground "cyan")

(set-face-attribute
 (defface slide-heading-2-face ()
   "Face for level-2 slide headings.")
 nil :font "-*-helvetica-*-r-*-*-*-140-*-*-*-*-*-*" :foreground "cyan")

(set-face-attribute
 (defface slide-text-face ()
   "Face for slide text")
 nil :font "-*-fixed-*-r-*-*-*-200-*-*-*-*-*-*")
  
(defvar slides nil
  "The slides being presented.")

(make-variable-buffer-local
 (defvar slide-number 0
   "The index of the current slide."))

(make-variable-buffer-local
 (defvar slide-actions '()
   "Sequence of actions remaining available for the current slide."))

(defvar slides-buffer-name-format "*Slide %d*")

(defvar current-slide-buffer nil)

(defun present-slideshow (slideshow)
  (loop for i from 1
        for slide in slideshow
        do (create-slide slide i))
  (switch-to-buffer (slide-buffer-name 1)))

(defun slide-buffer-name (n)
  (format slides-buffer-name-format n))

(defun create-slide (slide i)
  (let ((name (slide-buffer-name i)))
    (ignore-errors (kill-buffer name))
    (with-current-buffer (get-buffer-create name)
      (slides-mode)
      (slime-mode 1)
      (setq slide-number i)
      (present-slide slide))))

(defun display-slides-buffer ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer slides-buffer-name))

(defun present-current-slide ()
  (interactive)
  (switch-to-buffer-if-exists (slide-buffer-name slide-number)))

(defun next-slide ()
  (interactive)
  (switch-to-buffer-if-exists (format slides-buffer-name-format (1+ slide-number))))

(defun previous-slide ()
  (interactive)
  (switch-to-buffer-if-exists (format slides-buffer-name-format (1- slide-number))))

(defun switch-to-buffer-if-exists (buffer)
  (if (get-buffer buffer)
      (progn (switch-to-buffer buffer)
             (place-point))
    (error "No such buffer: %S" buffer)))

(defun slide-action ()
  (interactive)
  (unless (null slide-actions)
    (let ((inhibit-read-only t))
      (funcall (pop slide-actions)))))

(defun init-slide-buffer ()
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create slides-buffer-name)
      (clear-buffer)
      (slides-mode)
      (slime-mode)
      (slime-autodoc-mode -1)
      (cd dir)
      (current-buffer))))

(defun clear-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)))


(defun slides-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'slides-mode)
  (use-local-map slides-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'slides-mode-hook))

(defvar slides-mode-map nil)

(progn
  (setq slides-mode-map (make-sparse-keymap))
  (define-key slides-mode-map "f" 'next-slide)
  (define-key slides-mode-map "b" 'previous-slide)
  (define-key slides-mode-map "a" 'slide-action)
  (define-key slides-mode-map "/" 'fade-to-next-slide)
  (define-key slides-mode-map "t" 'toggle-cursor)
  (define-key slides-mode-map "r" 'present-current-slide)
  (define-key slides-mode-map "c" 'compile-lisp-ahead)
  )

(defvar cursor-on-p t)

(defun toggle-cursor ()
  (interactive)
  (setq cursor-on-p (not cursor-on-p))
  (modify-frame-parameters (selected-frame)
                           (list (cons 'cursor-color
                                       (if cursor-on-p "red" "#010101")))))

(def-slime-selector-method ?\H-s "slideshow buffer"
  (delete-other-windows)
  (switch-to-buffer (slime-recently-visited-buffer 'slides-mode))
  (place-point)
  (recenter)
  (current-buffer))


;;; Fading

(defface slide-fade-face ()
  "Face for fading.")

(defun fade-to-next-slide ()
  (interactive)
  (fade nil 'slide-text-face "white")
  (if current-prefix-arg
      (set-face-foreground 'slide-heading-1-face "black")
    (fade nil 'slide-heading-1-face "cyan"))
  (next-slide)
  (fade t 'slide-heading-1-face "cyan")
  (fade t 'slide-text-face "white"))

(defun fade-in ()
  (interactive)
  (fade t))

(defun fade-out ()
  (interactive)
  (fade nil))

(defun fade (in &optional face color steps)
  (when (null face) (setq face 'slide-fade-face))
  (when (null steps) (setq steps 10))
  (dotimes (step (1+ steps))
    (multiple-value-bind (r b g)
        (mapcar (lambda (v) (/ v 256))
                (x-color-values (or color (face-foreground face) "white")))
      (let* ((n (if in step (- steps step)))
             (x (sin (* pi 0.5 (/ n 1.0 steps))))
             (shade (round (* x 255)))
             (hex (hex-2digit shade))
             (color (format "#%s%s%s"
                            (hex-2digit (round (* x r)))
                            (hex-2digit (round (* x g)))
                            (hex-2digit (round (* x b))))))
        (set-face-foreground face color)
        (sit-for 0.03)))))

(defun hex-2digit (n)
  (if (< n 16) (format "0%x" n) (format "%x" n)))


(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
	(operands (gensym "rand-"))
	(tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
	    (,operator (car ,tmp))
	    (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
                     (if (eq (car clause) t)
                         `(t ,@(cdr clause))
                       (destructuring-bind ((op &rest rands) &rest body) clause
                         `(,op (destructuring-bind ,rands ,operands
                                 . ,body)))))
		   patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	     `((t (error "destructure-case failed: %S" ,tmp))))))))

(put 'destructure-case 'lisp-indent-function 1)


(defun present-slide (slide)
  (setq slide-actions '())
  (let ((inhibit-read-only t))
    (clear-buffer)
    (present-elements slide)
    (place-point)
    t))

(defun place-point ()
  "Put the point in an out-of-the-way position."
  (goto-char (point-min))
  (re-search-forward "^$" nil t))     ; put the point on a blank line

(defun present-elements (list)
  (mapc #'present-element list))

(defun present-element (e)
  (destructure-case e
    ((:linebreak)
     (present-element '(:line "")))
    ((:indent level &rest body)
     (let ((p (point)))
       (present-elements body)
       (indent-rigidly p (point) level)))
    ((:line string)
     (insert-line-with-face string 'slide-text-face))
    ((:line* string)
     (insert-line-with-face string 'slide-text-face :no-paragraph-gap t))
    ((:include-buffer buffer)
     (insert (with-current-buffer buffer (buffer-string))))
    ((:text string)
     (insert string))
    ((:marker)
     t)
    ((:image filename)
     (insert-image (create-image filename)))
    ((:center string)
     (insert-line-with-face string 'slide-text-face :center t))
    ((:action function)
     (push function slide-actions))
    ((:h1 string &optional center)
     (insert-line-with-face string 'slide-heading-1-face :center center))
    ((:h2 string &optional center)
     (insert-line-with-face string 'slide-heading-2-face :center center))))

(defun* insert-line-with-face (string face &key center no-paragraph-gap)
  (insert (propertize string 'face face))
  (when center (center-line))
  (insert "\n")
  (unless no-paragraph-gap (insert "\n")))


(defvar my-slideshow
  (list `((:text "                   ")
          (:image ,(concat (directory-file-name slideware-dir) "/slime.png"))
          (:linebreak)
          (:line "                 The Superior Lisp Interaction Mode for Emacs")
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:linebreak)
          (:line "                an illustrated story presented by Luke Gorrie")
          (:action (lambda ()
                     (save-excursion
                       (goto-line 2)
                       (let ((p (point)))
                         (animate-string "The Superior" 2 17)
                         (put-text-property p (point) 'face 'slide-text-face))))))
        '((:h1 "Cast of characters")
          (:indent 4
                   (:line "\
Helmut Eller         Luke Gorrie               Marco Baringer
Peter Seibel         Alan Ruttenberg           Daniel Barlow
Edi Weitz            Wolfgang Jenkner          Matthias Koeppe
Martin Simmons       Lawrence Mitchell         Christophe Rhodes
Brian Downing        Bill Clementson           Thomas Schilling
Thomas F. Burdick    Michael Weber             Matthew Danish
James Bielman        Lars Magne Ingebrigtsen   John Paul Wallington
Bryan O'Connor       Antonio Menezes Leitao    Alan Shutko
Utz-Uwe Haus         Tiago Maduro-Dias         Robert Lehr
Robert E. Brown      Raymond Toy               Jouni K Seppanen
Eric Blood           Eduardo Muñoz             Chris Capel
Bjørn Nordbø         Andras Simon              Zach Beane
Wolfgang Mederle     Travis Cross              Sean O'Rourke
Russell McManus      Rui Patrocínio            Reini Urban
Pawel Ostrowski      Nikodemus Siivola         Lynn Quam
Lasse Rasinen        Julian Stecklina          Juergen Gmeiner
Jan Rychter          James McIlree             Ivan Boldyrev
Ignas Mikalajunas    Ian Eslick                Hannu Koivisto
Frederic Brunel      Christian Lynbech         Brian Mastenbrook
Barry Fishman        Andreas Fuchs             Alexey Dejneka
Alan Caulkins")))
        '((:h1 "SLIM: the very beginning (august 2003)")
          (:line "\
Eric Marsden on #lisp:
  I have rewritten bits of ILISP: arglist display, DESCRIBE, DISASSEMBLE, ..
  Luke: please feel free to take over maintenance, if you're motivated.

Notable features:
  Hemlock-style design -- RPC over a dedicated socket.
  CMUCL only, no portability cruft!
  Compiler message annotations...
")
          (:marker)
          (:action (lambda ()
                     (goto-char (point-max))
                     (present-element '(:include-buffer "notes.lisp")))))
        '((:h1 "SLIME: the next beginning (september 2003)")
          (:line "\
Luke Gorrie on cmucl-imp:
  slime is a previously unreleased program by Eric Marsden that I've
  been hacking on recently, essentially a CMUCL-specific clone of ILISP.
  Slime has too few features to be a whizzy-bang development environment
  just yet. But it's easy to setup (provided you have a recent CMUCL
  snapshot), and should work fine with GNU Emacs 21 or XEmacs
  21. Comments and suggestions welcome!

Helmut Eller on slime-devel:
  Here are some patches.
  I rewrote the wire stuff so that reading doesn't block anymore. I
  also added something similar to \"eval-last-expression\" and a
  debugger hook. The other things are basic versions of apropos,
  describe and macroexpand (needs more work) and Emacs 20
  compatibility.

Debug example: (foo 1)"))
        '((:h1 "Early history: september-october 2003")
          (:line "\
James Bielman on slime-devel:
  I've started an attempt to port Slime to OpenMCL --- so far I've been
  able to get enough of the backend going to evaluate forms, get
  function arglists, and most of the debugger functionality.

Dan Barlow in mail:
  Starting with the CVS repository I found on Luke's web page, an
  evening's hacking of the rape&paste kind that Christophe would frown
  on (as would I, actually) has left me with a SLIME whose basic
  functionality works on SBCL.

James Bielman in mail:
  First off, let me apologize for blatantly ignoring your stated
  non-goal of porting Slime to other Lisps.  :-)
  [Design for separate backends...]

Luke Gorrie on slime-devel:
  Today we have migrated SLIME to common-lisp.net."))
        '((:h1 "Summary going towards the present")
          (:line "\n\n\n\n\n\n
                             hack hack hack"))
        '((:h1 "Development style")
          (:line "\
Experimental rewriting. Fun activity in pairs, especially with a test suite!

There's no place like HEAD, there's no place like HEAD.

All feedback is good feedback. (Well, nearly.)



Follow a few simple rules:

  Don't copy ugly code -- rethink it and rewrite it.

  .el is for Elisp and .lisp is for Lisp.

  Keep it readable."))
        '((:h1 "Milestones")
          (:line "\
1.0 -- September 2004, marking a year of hacking.
       (For old ladies only. -- Paolo Amoroso)

Featured in Practical Common Lisp. (Woohoo!)

180 mailing list subscribers. (Unknown number of Gmane readers.)

Around 3,500 mailing list posts.

Used in some interesting places:

  Stanford computational music course - environment for \"Common Music\"
    (Check out their SLIME tutorial!)

  ITA Software.





                        Another release one day? Maybe, maybe not."))
        '((:h1 "Demos")
          (:line "\
Inspector.

          Selector.

                    Edit-variable.

                               M-."))

        ))

(defun present-my-slideshow ()
  (interactive)
  (present-slideshow my-slideshow))


