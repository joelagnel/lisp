;; I'm not sure if this is a working file...
;; I need to test it tomorrow.
;; Edrx, 2006aug14

;; This is VERY related to the psne and "S=$HOME/snarf" thingy
;; described at <http://angg.twu.net/eev-article.html#local-copies>.
;; Think about opening ~/.psne.log in a buffer and running M-x brfl,
;; M-x brml, M-x brm, etc, on the urls listed there (and M-x brgvl on
;; the ps/pdf urls)...

;; The original code, which was very terse and hard to understand:
;; (find-angg ".emacs" "brwl-and-friends")
;; http://angg.twu.net/.emacs.html#brwl-and-friends

;; Four functions just to make this file somewhat standalone-ish.
;; I copied them from my .emacs. They need documentation.

;; The "official" version of find-firefox lives in my .emacs, iirc, so here is.
' (defun find-firefox (url &optional rest)
    (interactive "sURL: ")
    (start-process "firefox" "*Messages*" "firefox" url)
    url)

;; Same for find-pspage. Hm, not sure. Whatever.
' (defun find-pspage (fname &optional page gvargs)
    (interactive "fPS or PDF file: ")
    (apply 'start-process "gv" "*Messages*"
	   `("gv"
	     ,@(if page (list (format "--page=%d" page)))
	     ,@gvargs
	     ,(ee-expand fname))))

;; http://angg.twu.net/eev-article.html#local-copies
(defun eepsne (url &rest ignore)
  (interactive (browse-url-interactive-arg "psne "))
  (eev (format "psne '%s'" url)))
 
(defun eetmpwget (url &rest ignore)
  (interactive (browse-url-interactive-arg "cd /tmp; wget "))
  (eev (concat "cd /tmp\nwget " url)))



;;
;; Utility functions.
;;

(defun eeurl-dired-file-name-at-point ()
  (if (eq major-mode 'dired-mode)
      (file-name-sans-versions (dired-get-filename) t)
    (error "Not in dired mode")))

(defun eeurl-u-to-f (url)
  "Convert an url like http://foo/bar to a filename like $S/http/foo/bar."
  ;; Add comments about psne and the snarf directory
  (replace-regexp-in-string "^\\(http\\|ftp\\)://" "$S/\\1/" url))

(defun eeurl-f-to-u (fname)
  "Convert a filename to a \"file://\" url"
  (concat "file://" (expand-file-name (ee-expand fname))))

(defun eeurl-u-to-u-l (url)
  "Convert a url like http://foo/bar to a url like file://<$S>/http/foo/bar.
This should be made smarter - file:// urls should be returned unchanged."
  ;; Add comments about psne and the snarf directory
  (eeurl-f-to-u (eeurl-u-to-f url)))


;; The functions that generate the defuns.
;; Here is the explanation for the cryptic names that they use.
;; Names, long form:
;;              dired-    Names, short form:
;;   url-at-   fname-at-
;;    point      point        up     dfp
;;      |          |          |       |
;;      v          v          v       v
;;     url <===> fname        u <===> f
;;        \      /             \     /
;;         v    v               v   v
;;         action                 a
;;
;; Also, an "l" suffix means "prefer local copy" when both local and
;; remote make sense.
;;
;; Example: `eeurl-utoa-to-uptoa-defun' takes the name of a u->a
;; function (a symbol) and produces the defun for a up->a function
;; that is a wrapper around the original function.

(defun eeurl-utoa-to-uptoa-defun (find-uxxx brxxx)
  "Try this: (find-epp (eeurl-utoa-to-uptoa-defun 'find-w3m 'brw))"
  `(defun ,brxxx (url &rest ignore)
     ,(format "Apply `%S' on URL." find-uxxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxx)
     (list ',find-uxxx url '-> (,find-uxxx url))))

(defun eeurl-utoa-to-uptoal-defun (find-uxxx brxxxl)
  "Try this: (find-epp (eeurl-utoa-to-uptoal-defun 'find-w3m 'brwl))"
  `(defun ,brxxxl (url &rest ignore)
     ,(format "Apply `%S' on the local url associated to URL." find-uxxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxxl)
     (setq url (eeurl-u-to-u-l url))
     (list ',find-uxxx url '-> (,find-uxxx url))))

(defun eeurl-ftoa-to-uptoa-defun (find-xxx brxxxl)
  "Try this: (find-epp (eeurl-ftoa-to-uptoa-defun 'find-fline 'brfl))"
  `(defun ,brxxxl (url &rest ignore)
     ,(format "Apply `%S' on the local file name associated to URL." find-xxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxxl)
     (let ((fname (eeurl-u-to-f url)))
       (list ',find-xxx fname '-> (,find-xxx fname)))))

(defun eeurl-ftoa-to-dfptoa-defun (find-xxx brxxxd)
  "Try this: (find-epp (eeurl-ftoa-to-dfptoa-defun 'find-pspage 'brgvd))"
  `(defun ,brxxxd ()
     ,(format "Apply `%S' on the dired file at point." find-xxx)
     (interactive)
      (let ((fname (eeurl-dired-file-name-at-point)))
	(message (format "%S" (list ',find-xxx fname '->
				    (,find-xxx fname)))))))

(defun eeurl-utoa-to-dfptoa-defun (find-uxxx brxxxd)
  "Try this: (find-epp (eeurl-utoa-to-dfptoa-defun 'find-w3m 'brwd))"
  ;; Note: a command like brgvd is in the right format to be bound in
  ;; dired-mode-map... See, for example: (find-efunction 'dired-find-file)
  `(defun ,brxxxd ()
     ,(format "Apply `%S' on the url of the dired file at point." find-uxxx)
     (interactive)
      (let ((url (eeurl-f-to-u
		  (eeurl-dired-file-name-at-point))))
	(message (format "%S" (list ',find-uxxx url '->
				    (,find-uxxx url)))))))


;;
;; The high-level interface - eeurl-define-from
;;

(defun eeurl-keywords-to-builder (keyword1 keyword2)
  (let ((ks (list keyword1 keyword2)))
    (cond ((equal ks '(:url->action:  :remote:)) 'eeurl-utoa-to-uptoa-defun)
	  ((equal ks '(:url->action:   :local:)) 'eeurl-utoa-to-uptoal-defun)
	  ((equal ks '(:url->action:   :dired:)) 'eeurl-utoa-to-dfptoa-defun)
	  ((equal ks '(:fname->action: :local:)) 'eeurl-ftoa-to-uptoa-defun)
	  ((equal ks '(:fname->action: :dired:)) 'eeurl-ftoa-to-dfptoa-defun))))

(defun eeurl-builders-for-define-from
  (keyword1 origfun keyword2 newfun &rest rest)
  "Internal use - see: (find-efunctiondescr 'eeurl-define-from)"
  (cons `(,(eeurl-keywords-to-builder keyword1 keyword2)
	  ',origfun ',newfun)
	(if rest (apply 'eeurl-builders-for-define-from
			keyword1 origfun rest))))

(defun eeurl-defuns-for-define-from (&rest rest)
  "Internal use - see: (find-efunctiondescr 'eeurl-define-from)"
  (mapcar 'eval (apply 'eeurl-builders-for-define-from rest)))

;; This is pretty nice...
(defun find-eeurl-define-from (&rest rest)
  "Show the code that a `eeurl-define-from' call would evaluate, without evaluating it."
  (find-epp (cons 'progn (apply 'eeurl-defuns-for-define-from rest))))

(defun eeurl-define-from (&rest rest)
  "Define a series of browse-url or dired-visit functions from a standard function.
This is hard to describe abstractly, so try the `find-epp' sexps
below - they just produce lists and display them, and have no
side-effects.

  (find-epp (eeurl-builders-for-define-from
	     :fname->action: 'find-pspage
	     :local:         'brgvl
	     :dired:         'brgvd))

  (find-epp (eeurl-defuns-for-define-from
	     :fname->action: 'find-pspage
	     :local:         'brgvl
	     :dired:         'brgvd))

`eeurl-define-from' runs the defuns that
`eeurl-defuns-for-define-from' generates, so...
There are more examples in the source file. Eh, more later."
  (eval (cons 'progn (apply 'eeurl-defuns-for-define-from rest))))


;;
;; Define lots of br functions.
;;

(eeurl-define-from :fname->action: 'find-fline
                   :local:         'brfl)
(eeurl-define-from :fname->action: 'eecd
                   :local:         'brcdl)
(eeurl-define-from :fname->action: 'find-pspage
                   :local:         'brgvl
                   :dired:         'brgvd)
(eeurl-define-from :fname->action: 'find-dvipage
                   :local:         'brxdvil
                   :dired:         'brxdvid)
(eeurl-define-from :fname->action: 'find-xpdfpage
                   :local:         'brxpdfl
                   :dired:         'brxpdfd)
(eeurl-define-from :url->action:   'find-firefox
		   :remote:        'brm
                   :local:         'brml
		   :dired:         'brmd)
(eeurl-define-from :url->action:   'find-w3m
		   :remote:        'brw
		   :local:         'brwl
		   :dired:         'brwd)
(eeurl-define-from :url->action:   'eepsne
		   :remote:        'brp)
(eeurl-define-from :url->action:   'eetmpwget
		   :remote:        'brtmpwget)



(provide 'eev-browse-url)


;; (eval-buffer)

;; Ooops, this block of notes is about how I'm planning to make an
;; intro to eev using lots of screenshots, like dto did for org-mode...
;; (find-angg "bin/Xscreenshot-window")
;; http://angg.twu.net/bin/Xscreenshot-window.html
;; Oops, Mod4-w is not yet bound by default...
;; (find-angg ".fvwm/keys.fvwm")
;; http://angg.twu.net/.fvwm/keys.fvwm.html
;;
;; The parts of eev
;; ================
;; Hyperlinks
;; Hyperlink generators
;;   Temporary buffers
;;   code-c-d, find-code-c-d
;;   dff
;; Sending regions
;;   bounded regions
;; The steppers
;; Glyphs
;; Help tools
;;
;; Auxiliars
;; =========
;; The installer
;; The snarfer
;; browse-url and friends
;; hippie-expand
;;
;; file:///home/edrx/TH/L/eev-article.html

;; Old notes, random crap.
;;
;; (find-efunction 'find-w3m)
;; (find-fline "~/TH/L/")
;; (progn (find-fline "~/TH/L/") (find-w3m "01jul14.html"))

;; u  - string: an url, like http://foo/bar
;; sf - string: a snarfed filename, like $S/http/foo/bar
;; f  - string: a filename, like /tmp/foo
;; fu - string: a "file://" url, like file:///tmp/foo
;; su - string: a snarfed file url, like file:///home/edrx/snarf/http/foo/bar
;; _fun - symbol; a function whose argument is a _
;; brfun - symbol: a browse-url-like function
;; def___ - a defun sexp
