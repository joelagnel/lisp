;;; babel.el --- interface to web translation services such as Babelfish
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;; Keywords: translation, web
;;; Copyright: (C) 1999-2001 Eric Marsden
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>. 
;; The latest version of this package should be available at
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>

;;; Commentary:

;;; Overview ==========================================================
;;
;; This module provides an Emacs interface to different translation
;; services available on the Internet. You give it a word or paragraph
;; to translate and select the source and destination languages, and
;; it connects to the translation server, retrieves the data, and
;; presents it in a special *babel* buffer. Currently the following
;; backends are available:
;;
;;  * the Babelfish service at babelfish.altavista.com (now uses UTF-8
;;    encoding, which w3 doesn't know how to handle, so you'll see
;;    strange characters).
;;  * the SysTran motor at onlinetrans.com
;;  * the InterTrans server at tranexp.com
;;  * the Transparent Language motor at FreeTranslation.com
;;  * the Leo Dictionary at dict.leo.org (thanks to Klaus Berndl) 
;;  * the e-PROMPT motor at www.translate.ru (thanks to Ferenc Wagner)
;;
;; Entry points: either 'M-x babel', which prompts for a phrase, a
;; language pair and a backend, or 'M-x babel-region', which prompts
;; for a language pair and backend, then translates the currently
;; selected region, and 'M-x babel-buffer' to translate the current
;; buffer.
;;
;; The InterTrans server can translate to and from any of 25 languages.
;; Babelfish and Systran (which use the same translation engine)
;; can translate between a smaller number of languages, typically with
;; less poor results. If you ask for a language combination which
;; several backends could translate, babel.el will allow you to choose
;; which backend to use. Since most servers have limits on the
;; quantity of text translated, babel.el will split long requests into
;; translatable chunks and submit them sequentially.
;;
;; Please note that the washing process (which takes the raw HTML
;; returned by a translation server and attempts to extract the useful
;; information) is fragile, and can easily be broken by a change in
;; the server's output format. In that case, check whether a new
;; version is available (and if not, warn me; I don't translate into
;; Welsh very often).
;;
;; Also note that by accessing an online translation service you are
;; bound by its Terms and Conditions; in particular
;; FreeTranslation.com is for "personal, non-commercial use only".
;;
;;
;; Installation ========================================================
;;
;; Place this file in a directory in your load-path (to see a list of
;; appropriate directories, type 'C-h v load-path RET'). Optionally
;; byte-compile the file (for example using the 'B' key when the
;; cursor is on the filename in a dired buffer). Then add the
;; following lines to your ~/.emacs.el initialization file:
;;
;;   (autoload 'babel "babel"
;;     "Use a web translation service to translate the message MSG." t)
;;   (autoload 'babel-region "babel"
;;     "Use a web translation service to translate the current region." t)
;;   (autoload 'babel-as-string "babel"
;;     "Use a web translation service to translate MSG, returning a string." t)
;;   (autoload 'babel-buffer "babel"
;;     "Use a web translation service to translate the current buffer." t)
;;
;; babel.el requires w3, the EmacsOS web browser to be installed. w3
;; comes preinstalled with XEmacs before v21, and as a package for
;; 21.x; Emacs users can download it from
;; <URL:ftp://ftp.xemacs.org/pub/xemacs/emacs-w3/>. It is also
;; available as a package called `w3-el' for Debian GNU/Linux; see
;; <URL:http://packages.debian.org/w3-el>.
;;
;;
;; Backend information =================================================
;;
;; A babel backend named <zob> must provide three functions:
;;
;;    (babel-<zob>-translation from to)
;;
;;    where FROM and TO are three-letter language abbreviations from
;;    the alist `babel-languages'. This should return non-nil if the
;;    backend is capable of translating between these two languages.
;;
;;    (babel-<zob>-fetch msg from to)
;;
;;    where FROM and TO are as above, and MSG is the text to
;;    translate. Connect to the appropriate server and fetch the raw
;;    HTML corresponding to the request.
;;
;;    (babel-<zob>-wash)
;;
;;    When called on a buffer containing the raw HTML provided by the
;;    server, remove all the uninteresting text and HTML markup.
;;
;; I would be glad to incorporate backends for new translation servers
;; which are accessible to the general public. List of translation
;; engines and multilingual dictionaries at
;; <URL:http://funsan.biomed.mcgill.ca/~funnell/language.html>.
;;
;;
;; <URL:http://www.xmethods.net/sd/BabelFishService.wsdl>
;;
;; babel.el was inspired by a posting to the ding list by Steinar Bang
;; <sb@metis.no>. Morten Eriksen <mortene@sim.no> provided several
;; patches to improve InterTrans washing. Thanks to Per Abrahamsen and
;; Thomas Lofgren for pointing out a bug in the keymap code. Matt
;; Hodges <pczmph@unix.ccc.nottingham.ac.uk> suggested ignoring case
;; on completion. Colin Marquardt suggested
;; `babel-preferred-to-language'. David Masterson suggested adding a
;; menu item.
;;
;; User quotes: Dieses ist die größte Sache seit geschnittenem Brot.
;;                 -- Stainless Steel Rat <ratinox@peorth.gweep.net> 


;;; Code:

(require 'cl)
(require 'easymenu)


(defconst babel-version 0.20
  "The version number of babel.el.")

(defconst babel-languages
  '(("English" . "eng")
    ("Brazilian Portuguese" . "pob")
    ("German" . "ger")
    ("Dutch" . "dut")
    ("Latin American Spanish" . "spl")
    ("Spanish" . "spa")
    ("European Spanish" . "spe")
    ("French" . "fre")
    ("Japanese (Shift JIS)" . "jpn")
    ("Danish" . "dan")
    ("Icelandic" . "ice")
    ("Finnish" . "fin")
    ("Italian" . "ita")
    ("Norwegian" . "nor")
    ("Swedish" . "swe")
    ("Portuguese" . "poe")
    ("Russian (CP 1251)" . "rus")
    ("Croatian (CP 1250)" . "cro")
    ("Hungarian (CP 1250)" . "hun")
    ("Polish (CP 1250)" . "pol")
    ("Czech (CP 1250)" . "che")
    ("Serbian (Latin)" . "sel")
    ("Slovenian (CP 1250)" . "slo")
    ("Greek" . "grk")
    ("Welsh" . "wel")
    ("Esperanto" . "esp")
    ("Lojban" . "loj")))

(defvar babel-preferred-to-language "French"
  "*Default target translation language.
This must be the long name of one of the languages in the alist
`babel-languages'.") 

(defvar babel-to-history (list babel-preferred-to-language))
(defvar babel-from-history (list))
(defvar babel-backend-history (list))

(defvar babel-mode-hook nil)

(defvar babel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")     #'bury-buffer)
    (define-key map (kbd "SPC")   #'scroll-up)
    (define-key map (kbd "DEL")   #'scroll-down)
    (define-key map (kbd "<")   #'beginning-of-buffer)
    (define-key map (kbd ">")   #'end-of-buffer)
    (define-key map (kbd "s")   #'isearch-forward)
    (define-key map (kbd "r")   #'isearch-backward)
    (define-key map (kbd "h")   #'describe-mode)
    map)
  "Keymap used in Babel mode.")

(defvar babel-backends
  '( ;; ("Babelfish at Altavista" . fish)
    ("SysTran" . systran)
    ("InterTrans" . intertrans)
    ("FreeTranslation" . free)
    ("WorldBlaze" . blaze)
    ("Leo Dictionary" . leo)
    ("e-PROMPT" . prompt)
    ("Lojban Glossary" . lojban))
  "List of backends for babel translations.")


;;;###autoload
(defun babel (msg &optional no-display)
  "Use a web translation service to translate the message MSG.
Display the result in a buffer *babel* unless the optional argument
NO-DISPLAY is nil."
  (interactive "sTranslate phrase: ")
  (let* ((completion-ignore-case t)
         (from-suggest (or (first babel-from-history) (caar babel-languages)))
         (from-long
          (completing-read "Translate from: "
                           babel-languages nil t
                           (cons from-suggest 0)
                           'babel-from-history))
         (to-avail (remove* from-long babel-languages
                            :test #'(lambda (a b) (string= a (car b)))))
         (to-suggest (or (first babel-to-history) (caar to-avail)))
         (to-long
          (completing-read "Translate to: " to-avail nil t
                           (cons to-suggest 0)
                           'babel-to-history))
         (from (cdr (assoc from-long babel-languages)))
         (to   (cdr (assoc to-long babel-languages)))
         (backends (babel-get-backends from to))
         (backend-str
          (completing-read "Using translation service: "
                           backends nil t
                           (cons (or (first babel-backend-history) (caar backends)) 0)
                           'babel-backend-history))
         (backend (symbol-name (cdr (assoc backend-str babel-backends))))
         (fetcher (intern (concat "babel-" backend "-fetch")))
         (washer  (intern (concat "babel-" backend "-wash")))
         (chunks (babel-chunkify msg 700))
         (translated-chunks '())
         (view-read-only nil))
    (loop for chunk in chunks 
          do (push (babel-work chunk from to) translated-chunks))
    (if no-display
        (apply #'concat (nreverse translated-chunks))
      (with-output-to-temp-buffer "*babel*"
        (message "Translating...")
        (loop for tc in (nreverse translated-chunks)
              do (princ tc))
        (save-excursion
          (set-buffer "*babel*")
          (babel-mode))
        (message "Translating...done")))))

;;;###autoload
(defun babel-region (start end)
  "Use a web translation service to translate the current region."
  (interactive "r")
  (babel (buffer-substring-no-properties start end)))

;;;###autoload
(defun babel-as-string (msg)
  "Use a web translation service to translate MSG, returning a string."
  (interactive "sTranslate phrase: ")
  (babel msg t))

;; suggested by Djalil Chafai <djalil@free.fr>
;;
;;;###autoload
(defun babel-buffer ()
  "Use a web translation service to translate the current buffer.
Default is to present the translated text in a *babel* buffer.
With a prefix argument, replace the current buffer contents by the
translated text."
  (interactive)
  (let (pos)
    (cond (prefix-arg
           (setq pos (point-max))
           (goto-char pos)
           (insert
            (babel-as-string
             (buffer-substring-no-properties (point-min) (point-max))))
           (delete-region (point-min) pos))
          (t
           (babel-region (point-min) (point-max))))))

(defun babel-work (msg from to)
  (save-excursion
    (save-window-excursion
      (set-buffer (get-buffer-create " *babelurl*"))
      (erase-buffer)
      (funcall fetcher (babel-preprocess msg) from to)
      (setq buffer-file-name nil)       ; don't know why w3 sets this
      (funcall washer)
      (babel-postprocess)
      (babel-display)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun babel-get-backends (from to)
  "Return a list of those backends which are capable of translating
language FROM into language TO."
  (loop for b in babel-backends
        for name = (symbol-name (cdr b))
        for translator = (intern (concat "babel-" name "-translation"))
        for translatable = (funcall translator from to)
        if translatable collect b))

(defun babel-display ()
  (require 'w3)
  (save-excursion
    (w3-region (point-min) (point-max))))

(defun babel-mode ()
  (interactive)
  (use-local-map babel-mode-map)
  (setq major-mode 'babel-mode
        mode-name "Babel")
  (run-hooks 'babel-mode-hook))


(cond ((fboundp 'string-make-unibyte)
       (fset 'babel-make-unibyte #'string-make-unibyte))
      ((fboundp 'string-as-unibyte)
       (fset 'babel-make-unibyte #'string-as-unibyte))
      (t
       (fset 'babel-make-unibyte #'identity)))

;; from nnweb.el, with added `string-make-unibyte'.
(defun babel-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (require 'w3-forms)
  (mapconcat
   (lambda (data)
     (concat (w3-form-encode-xwfu (babel-make-unibyte (car data))) "="
             (w3-form-encode-xwfu (babel-make-unibyte (cdr data)))))
   pairs "&"))

;; We mark paragraph endings with a special token, so that we can
;; recover a little information on the original message's format after
;; translation and washing and rendering. Should really be using
;; `paragraph-start' and `paragraph-separate' here, but we no longer
;; have any information on the major-mode of the buffer that STR was
;; ripped from.
;;
;; This kludge depends on the fact that all the translation motors
;; seem to leave words they don't know how to translate alone, passing
;; them through untouched.
(defun babel-preprocess (str)
  (while (string-match "\n\n\\|^\\s-+$" str)
    (setq str (replace-match " FLOBSiCLE " nil t str)))
  str)

;; decode paragraph endings in current buffer
(defun babel-postprocess ()
  (goto-char (point-min))
  (while (search-forward "FLOBSiCLE" nil t)
    (replace-match "\n<p>" nil t)))

;; split STR into chunks of around LENGTH characters, trying to
;; maintain sentence structure (this is used to send big requests in
;; several batches, because otherwise the motors cut off the
;; translation).
(defun babel-chunkify (str chunksize)
  (let ((start 0)
        (pos 0)
        (chunks '()))
    (while (setq pos (string-match sentence-end str pos))
      (incf pos)
      (when (> (- pos start) chunksize)
        (push (substring str start pos) chunks)
        (setq start pos)))
    (when (/= start (length str))
      (push (substring str start) chunks))
    (nreverse chunks)))

;;;###autoload
(defun babel-version (&optional here)
  "Show the version number of babel in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((version-string 
         (format "Babel version %s" babel-version)))
    (if here 
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))


;; Babelfish-specific functions ================================================
;;
;; Babelfish (which uses the SysTran engine) is only able to translate
;; between a limited number of languages.

;; translation from 3-letter names to Babelfish 2-letter names
(defconst babel-fish-languages
  '(("eng" . "en")
    ("ger" . "de")
    ("ita" . "it")
    ("poe" . "pt")
    ("spe" . "es")
    ("fre" . "fr")))

;; those inter-language translations that Babelfish is capable of
(defconst babel-fish-translations
  '("en_fr" "en_de" "en_it" "en_pt" "en_es" "fr_en" "de_en" "it_en"
    "es_en" "pt_en"))

;; if Babelfish is able to translate from language FROM to language
;; TO, then return the corresponding string, otherwise return nil
(defun babel-fish-translation (from to)
  (let* ((fromb (cdr (assoc from babel-fish-languages)))
         (tob   (cdr (assoc to babel-fish-languages)))
         (comb (and fromb tob (concat fromb "_" tob))))
    (find comb babel-fish-translations :test #'string=)))

(defun babel-fish-fetch (msg from to)
  "Connect to the Babelfish server and request the translation."
  (require 'url)
  ;; Babelfish now outputs UTF-8. If our emacs has UTF support (from
  ;; the Mule-UCS package for instance), treat the contents of the
  ;; buffer as UTF8. Otherwise we will have garbled output for 8-bit
  ;; characters.
  (let ((coding-system 'no-conversion))
    (when (fboundp 'set-buffer-multibyte)
      (set-buffer-multibyte t))
    (when (member 'utf-8 (coding-system-list t))
      (setq coding-system 'utf-8))
    (let ((translation (babel-fish-translation from to)))
      (unless translation
        (error "Babelfish can't translate from %s to %s" from to))
      (let* ((pairs `(("urltext" . ,msg)
                     ("lp" . ,translation)
                     ("doit" . "done")
                     ("bblType" . "urltext")))
             (url-request-data (babel-form-encode pairs))
             (url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/x-www-form-urlencoded")))
             (network-coding-system-alist '((80 . ,coding-system))))
        (url-insert-file-contents "http://babelfish.altavista.com/translate.dyn")))))

(defun babel-fish-wash ()
  "Extract the useful information from the HTML returned by Babelfish."
  (goto-char (point-min))
  (cond ((search-forward "http://altavista.com/cgi-bin/query\" method=get>" nil t)
         ;; short response format
         (delete-region (point-min) (match-end 0))
         (when (re-search-forward "^</textarea>\\s-*$" nil t)
           (delete-region (match-beginning 0) (point-max))))
        ((prog2
             (goto-char (point-min))
             (re-search-forward "^<td bgcolor=white>" nil t))
         ;; long response format
         (delete-region (point-min) (match-end 0))
         (when (search-forward "</td>" nil t)
           (delete-region (point-max) (match-beginning 0))))
        (t
         (error "Babelfish HTML has changed; please look for a new version of babel.el")))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]+>" nil t)
    (replace-match "" t)))


;; SysTrans-specific functions ===========================================
(defalias 'babel-systran-translation 'babel-fish-translation)

(defun babel-systran-fetch (msg from to)
  "Connect to the SysTran server and request the translation."
  (require 'url)
  (let ((translation (babel-systran-translation from to)))
    (unless translation
      (error "Systran can't translate from %s to %s" from to))
    (let* ((pairs `(("partner" . "demo-SystranSoft")
                    ("urltext" . ,msg)
                    ("lp" . ,translation)))
           (url-request-data (babel-form-encode pairs))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (url-insert-file-contents "http://www.systranlinks.com/systran/cgi"))))
    
(defun babel-systran-wash ()
  "Extract the useful information from the HTML returned by SysTran."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "name=translated_text[^>]+>" nil t)
      (delete-region (point-min) (match-end 0)))
    (goto-char (point-max))
    (when (re-search-backward "^</textarea><br>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))))


;; InterTrans-specific functions ==========================================

;; InterTrans can do all the possible language combinations, except
;; for Lojban
(defun babel-intertrans-translation (to from)
  (and (not (string= to "loj"))
       (not (string= from "loj"))))

(defun babel-intertrans-fetch (msg from to)
  "Connect to the InterTrans server and request the translation."
  (require 'url)
  (let* ((pairs `(("type" . "text")
                  ("url" . "http://")
                  ("text" . ,msg)
                  ("from" . ,from)
                  ("to" . ,to)))
         (url-request-data (babel-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.tranexp.com:2000/InterTran")))  

;; these functions by Morten Eriksen <mortene@sim.no>. InterTrans
;; returns different HTML depending on whether you request the
;; translation of a phrase or of a single word. If you ask for a
;; single word which it doesn't know how to translate it will attempt
;; to find the closest match.
(defun babel-intertrans-wash ()
  "Extract the useful information from the HTML returned by InterTrans."
  (goto-char (point-min))
  (if (search-forward "Translated text:" nil t)
      (babel-intertrans-wash-expression)
    (if (search-forward "Closest match" nil t)
	(babel-intertrans-wash-closest-match)
      (babel-intertrans-wash-single-word))))

(defun babel-intertrans-wash-expression ()
  "Wash the HTML page InterTrans returns when translating an expression."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (search-forward "Translated text:" nil t)
      (delete-region (point-min) (match-end 0)))
    (goto-char (point-max))
    (when (re-search-backward "</TEXTAREA" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))))

(defun babel-intertrans-wash-single-word ()
  "Wash the HTML page InterTrans returns when translating a single word."
  (goto-char (point-min))
  (when (search-forward ":</B>\n<UL>" nil t)
    (delete-region (point-min) (match-end 0))
    (when (search-forward "<BR>" nil t)
      (delete-region (match-beginning 0) (point-max)))))

(defun babel-intertrans-wash-closest-match ()
  "Wash the HTML page InterTrans returns when the word is not found."
  (goto-char (point-min))
  (when (search-forward "Closest match" nil t)
    (delete-region (point-min) (match-beginning 0))
    (when (search-forward "<BR>" nil t)
      (delete-region (match-beginning 0) (point-max)))))


;; FreeTranslation.com stuff ===========================================

;; translation from 3-letter names to FreeTranslation names
(defconst babel-free-languages
  '(("eng" . "English")
    ("ger" . "German")
    ("ita" . "Italian")
    ("poe" . "Portuguese")
    ("spe" . "Spanish")
    ("fre" . "French")))

;; those inter-language translations that FreeTranslation is capable of
(defconst babel-free-translations
  '("Spanish/English" "French/English" "German/English" "English/Spanish"
    "English/French" "English/German" "English/Italian" "English/Portuguese"))

(defun babel-free-translation (from to)
  (let* ((ffrom (cdr (assoc from babel-free-languages)))
         (fto   (cdr (assoc to babel-free-languages)))
         (trans (concat ffrom "/" fto)))
    (find trans babel-free-translations :test #'string=)))

(defun babel-free-fetch (msg from to)
  "Connect to the FreeTranslation server and request the translation."
  (require 'url)
  (let ((translation (babel-free-translation from to)))
    (unless translation
      (error "FreeTranslation can't translate from %s to %s" from to))
    (let* ((pairs `(("Sequence"  . "core")
                    ("Mode"      . "html")
                    ("Template"  . "TextResult2.htm")
                    ("SrcText"   . ,msg)
                    ("Language"  . ,translation)))
           (url-request-data (babel-form-encode pairs))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (url-insert-file-contents "http://ets.freetranslation.com:5081/")))  )

(defun babel-free-wash ()
  "Extract the useful information from the HTML returned by FreeTranslation."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (search-forward "by SDL International --</b></p>" nil t)
      (delete-region (point-min) (match-end 0)))
    (goto-char (point-max))
    (when (search-backward "<b>-----------" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))))


;; WorldBlaze.com stuff ===========================================

;; translation from 3-letter names to WorldBlaze names
(defconst babel-blaze-languages
  '(("eng" . "en")
    ("ger" . "de")
    ("ita" . "it")
    ("poe" . "pt")
    ("spe" . "es")
    ("fre" . "fr")))

;; those inter-language translations that WorldBlaze is capable of
(defconst babel-blaze-translations
  '("enes-LHS" "enfr-LHS" "ende-LHS" "enit-LHS" "enpt-LHS"
    "enja-ATL" "enru-PROMT" "jaen-ATL" "fren-LHS" "frde-LHS"
    "frit-LHS" "frru-PROMT" "esen-LHS" "esit-LHS" "deen-LHS"
    "defr-LHS" "deit-LHS" "deru-PROMT" "iten-LHS" "itde-LHS"
    "ites-LHS" "itfr-LHS" "itru-PROMT" "pten-LHS" "ruen-PROMT"
    "rude-PROMT" "rufr-PROMT"))

(defun babel-blaze-translation (from to)
  (let* ((ffrom (cdr (assoc from babel-blaze-languages)))
         (fto   (cdr (assoc to babel-blaze-languages)))
         (trans (concat ffrom fto "-")))
    (find trans babel-blaze-translations :test #'string-match)))

(defun babel-blaze-fetch (msg from to)
  "Connect to the WorldBlaze server and request the translation."
  (require 'url)
  (let ((translation (babel-blaze-translation from to)))
    (unless translation
      (error "WorldBlaze can't translate from %s to %s" from to))
    (let* ((pairs `(("go"   . "Translate")
                    ("data" . ,msg)
                    ("lp"  . ,translation)))
           (data (babel-form-encode pairs)))
      (url-insert-file-contents
       (concat "http://www.worldblaze.com/scripts/frtpgdemo.cgi?" data)))))
    
(defun babel-blaze-wash ()
  "Extract the useful information from the HTML returned by BlazeTranslation."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (search-forward "<h1>Translated Text:</h1>" nil t)
      (delete-region (point-min) (match-end 0)))
    (goto-char (point-max))
    (when (search-backward "<h1>Original Text:</h1>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))))



;; Leo-specific functions ================================================
;;
;; this code contributed by Klaus Berndl <klaus.berndl@sdm.de>. This
;; backend is mainly word-oriented; if you submit a phrase it will
;; give translations for the words (or subphrases) in the phrase. 

(defconst babel-leo-languages
  '(("eng" . "en")
    ("ger" . "de")))

(defconst babel-leo-translations
  '("en_de" "de_en"))

(defun babel-leo-translation (from to)
  (let* ((fromb (cdr (assoc from babel-leo-languages)))
         (tob   (cdr (assoc to babel-leo-languages)))
         (comb (and fromb tob (concat fromb "_" tob))))
    (find comb babel-leo-translations :test #'string=)))

(defun babel-leo-fetch (msg from to)
  "Connect to the Leo server and request the translation."
  (require 'url)
  (let* ((translation (babel-leo-translation from to))
         (pairs `(("search" . ,msg)
                  ("lang" . "en"))) ; indicates in which language the
                                    ; leo-resultpage is displayed
         (url-request-data (babel-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (unless translation
      (error "Leo Dictionary can't translate from %s to %s" from to))
    (url-insert-file-contents "http://dict.leo.org")))

(defun babel-leo-wash ()
  (goto-char (point-min))
  (if (re-search-forward
       "<TABLE.*</TABLE>"
       nil t)
      (let ((result-table (match-string 0)))
        (erase-buffer)
        (insert result-table))
    (error "Leo Dictionary HTML has changed ; please look for a new version of babel.el")))
    


;; PROMPT stuff ===========================================
;;
;; Contributed by Ferenc Wagner

;; translation from 3-letter names to e-PROMPT names
(defconst babel-prompt-languages
  '(("eng" . 1)
    ("rus" . 2)
    ("ger" . 4)
    ("fre" . 8)
    ("ita" . 16)
    ("spe" . 32)))

;; those inter-language translations that e-PROMPT is capable of
(defconst babel-prompt-translations '(
    262145   ; English-German
     65540   ; German-English
    524289   ; English-French
     65544   ; French-English
    524292   ; German-French
    262152   ; French-German
   2097153   ; English-Spanish
     65568   ; Spanish-English
     ))

(defun babel-prompt-translation (from to)
  (let ((ffrom (cdr (assoc from babel-prompt-languages)))
	(fto   (cdr (assoc to babel-prompt-languages))))
    (and ffrom fto
	 (find (+ ffrom (* 65536 fto))
	       babel-prompt-translations :test #'=))))

(defun babel-prompt-fetch (msg from to)
  "Connect to the e-PROMPT server and request the translation."
  (require 'url)
  (let ((translation (babel-prompt-translation from to)))
    (unless translation
      (error "PROMPT can't translate from %s to %s" from to))
    (let* ((pairs `(("CharSet"    . "windows-1252") ; iso-8859-2
                    ("CharSetTo"  . "windows-1252") ; iso-8859-2
                    ("Language"   . "English")
		    ("status"     . "translate")
		    ("source"     . ,msg)
		    ("directions" . ,(number-to-string translation))
		    ("templates"  . "Default")))
           (url-request-data (babel-form-encode pairs))
	   (url-request-method "POST")
	   (url-request-extra-headers
	    '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (url-insert-file-contents
       "http://www.translate.ru/eng/other.asp"))))

(defun babel-prompt-wash ()
  "Extract the useful information from the HTML returned by e-PROMPT."
  (goto-char (point-min))
  (unless
      (re-search-forward "<textarea\\s-+language=\\\"[a-z]+\\\"\\s-+\
name=\\\"target\\\"\\s-+[^>]+>\\([^<]*\\)</textarea>" nil t)
    (error "e-PROMPT HTML has changed; please look for a new version of babel.el"))
  (delete-region (match-end 1) (point-max))
  (delete-region (point-min) (match-beginning 1)))


;; Lojban Glossary ===============================================
;;
;; Contributed by Mario Lang <mlang@delysid.org>.

(defun babel-lojban-translation (from to)
  "Lojban Glossary can only translate to english."
  (and (string= from "loj") (string= to "eng")))

(defun babel-lojban-fetch (msg from to)
  "Connect to the Lojban Glossary server and request the translation."
  (require 'url)
  (let* ((translation (babel-lojban-translation from to))
         (pairs `(("lojtext" . ,msg)))
	 (url-request-data (babel-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (unless translation
      (error "Lojban Glossary can't translate from %s to %s" from to))
    (url-insert-file-contents "http://www.barsoom.net/lojban/hezekiah/glossary.cgi")))

(defun babel-lojban-wash ()
  (goto-char (point-min))
  (if (re-search-forward
       "<hr>"
       nil t)
      (progn (delete-region (point-min) (point))
	     (goto-char (point-max))
	     (if (re-search-backward "<dl>" nil t)
		 (delete-region (point) (point-max))
	       (error "Lojban Glossary format has changed.")))
    (error "Lojban Glossary format has changed.")))


;; (defun babel-debug ()
;;   (let ((buf (get-buffer-create "*babel-debug*")))
;;     (set-buffer buf)
;;     (babel-free-fetch "state mechanisms are too busy" "eng" "ger")))


(easy-menu-add-item nil '("tools") ["Babel Translation" babel t]) 

(provide 'babel)

;; babel.el ends here
