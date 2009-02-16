;;; babel.el --- interface to web translation services such as Babelfish
;;;
;;; Author: Eric Marsden <emars...@laas.fr>
;;;         Juergen Hoetzel <juer...@hoetzel.info>
;;; Keywords: translation, web
;;; Copyright: (C) 1999-2001 Eric Marsden
;;;                2005 Juergen Hoetzel
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
;; Please send suggestions and bug reports to <emars...@laas.fr>.
;; The latest version of this package should be available at
;;
;;     <URL:http://www.hoetzel.info/Hacking/emacs/babel.el>

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
;;  * the Babelfish service at babelfish.altavista.com
;;  * the Google service at translate.google.com
;;  * the Transparent Language motor at FreeTranslation.com

;;
;; Entry points: either 'M-x babel', which prompts for a phrase, a
;; language pair and a backend, or 'M-x babel-region', which prompts
;; for a language pair and backend, then translates the currently
;; selected region, and 'M-x babel-buffer' to translate the current
;; buffer.
;;

;; If you ask for a language combination which several backends could
;; translate, babel.el will allow you to choose which backend to
;; use. Since most servers have limits on the quantity of text
;; translated, babel.el will split long requests into translatable
;; chunks and submit them sequentially.
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
;; babel.el requires emacs 22 (cvs)
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
;; <s...@metis.no>. Morten Eriksen <mort...@sim.no> provided several
;; patches to improve InterTrans washing. Thanks to Per Abrahamsen and
;; Thomas Lofgren for pointing out a bug in the keymap code. Matt
;; Hodges <pcz...@unix.ccc.nottingham.ac.uk> suggested ignoring case
;; on completion. Colin Marquardt suggested
;; `babel-preferred-to-language'. David Masterson suggested adding a
;; menu item.
;;
;; User quotes: Dieses ist die größte Sache seit geschnittenem Brot.
;;                 -- Stainless Steel Rat <rati...@peorth.gweep.net>

;;; History

;;    0.4: * revised FreeTranslation backend

;;;   0.3: * removed non-working backends: systran, intertrans, leo, e-PROMPT
;;;        * added Google backend
;;;        * revised UTF-8 handling
;;;        * Added customizable variables: babel-preferred-to-language, babel-preferred-from-language
;;;        * revised history handling
;;;        * added helper function: babel-wash-regex

;;; Code:

(require 'cl)
(require 'easymenu)

;; ======================================================================
;;; Customizables
;; ======================================================================
(defgroup babel nil
  "provides an Emacs interface to different translation services available on the Internet"
  :group 'applications)

(defconst babel-version 0.3
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
    ("Russian" . "rus")
    ("Croatian (CP 1250)" . "cro")
    ("Hungarian (CP 1250)" . "hun")
    ("Polish (CP 1250)" . "pol")
    ("Czech (CP 1250)" . "che")
    ("Serbian (Latin)" . "sel")
    ("Slovenian (CP 1250)" . "slo")
    ("Greek" . "grk")
    ("Welsh" . "wel")
    ("Esperanto" . "esp")
    ("Lojban" . "loj")
    ("Simplified Chinese" . "schi")
    ("Traditional Chinese" . "tchi")
    ))

(defcustom babel-preferred-to-language "German"
  "*Default target translation language.
This must be the long name of one of the languages in the alist"
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s))) babel-languages))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq babel-to-history (list value)))
  :group 'babel)

(defcustom babel-preferred-from-language "English"
  "*Default target translation language.
This must be the long name of one of the languages in the alist"
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s))) babel-languages))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq babel-from-history (list value)))
  :group 'babel)

(defvar babel-to-history (list babel-preferred-to-language))
(defvar babel-from-history (list babel-preferred-to-language))
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
  '(("Babelfish at Altavista" . fish)
    ("FreeTranslation" . free)
    ("Google" . google))              
  "List of backends for babel translations.")

(defun babel-sentence-end()            
  "portability function. emacs 22.0.50 introduced sentence-end
function, not available on other emacsen"
  (if (fboundp 'sentence-end)
      (sentence-end)
    sentence-end))      

(defun babel-url-retrieve (url charset)
  "Retrieve URL and decode to charset"
  (let ((tmp (url-retrieve-synchronously url))
        (current (current-buffer)))
    (with-current-buffer tmp
      (mm-decode-coding-region (point-min) (point-max) charset)
      (set-buffer-file-coding-system charset)
      (mm-enable-multibyte)
      (copy-to-buffer current (point-min) (point-max)))
    (kill-buffer tmp)))

(defun babel-wash-regex (regex)
  "Extract the useful information from the HTML returned by fetch function
translated text should be inside parenthesized expression in regex"
  (goto-char (point-min))
  (if (search-forward-regexp regex (point-max) t)
      (progn
        (delete-region (match-end 1) (point-max))
        (delete-region (point-min) (match-beginning 1))
        t)))

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
         (to-suggest (or (first
                          (remove* from-long babel-to-history
                                   :test #'string=))
                         (caar to-avail)))
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
                           (cons (or (member (first babel-backend-history) backends) (caar backends)) 0) ;TOD also check rest of babel-backend-history
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
  "Use a web translation service to translate MSG, returning a string."SysTran
  (interactive "sTranslate phrase: ")
  (babel msg t))

;; suggested by Djalil Chafai <dja...@free.fr>
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
    (while (setq pos (string-match (babel-sentence-end) str pos))
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
  (let ((coding-system-for-read 'utf-8)
        (translation (babel-fish-translation from to)))
    (unless translation
      (error "Babelfish can't translate from %s to %s" from to))
    (let* ((pairs `(("trtext" . ,(mm-encode-coding-string msg 'utf-8))
                    ("lp" . ,translation)
                    ("doit" . "done")
                    ("intl" . "1")
                    ("tt" . "urltext")
                    ("btnTrTxt" . "Translate")))
           (url-request-data (babel-form-encode pairs))
             (url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (babel-url-retrieve "http://babelfish.altavista.com/tr" 'utf-8))))

(defun babel-fish-wash ()
  "Extract the useful information from the HTML returned by Babelfish."
  (if (not (babel-wash-regex "<td bgcolor=white class=s><div style=padding:10px;>\\([^<]*\\)</div></td>"))
      (error "Babelfish HTML has changed ; please look for a new version of babel.el")))

;; FreeTranslation.com stuff ===========================================

;; translation from 3-letter names to FreeTranslation names
(defconst babel-free-languages
  '(("eng" . "English")
    ("ger" . "German")
    ("ita" . "Italian")
    ("dut" . "Dutch")
    ("poe" . "Portuguese")
    ("spe" . "Spanish")
    ("nor" . "Norwegian")
    ("rus" . "Russian")  
    ("schi" . "SimplifiedChinese")
    ("tchi" . "TraditionalChinese")
    ("fre" . "French")))

;; those inter-language translations that FreeTranslation is capable of
(defconst babel-free-translations
  '("English/Spanish" "English/French" "English/German" "English/Italian" "English/Dutch" "English/Portuguese"
    "English/Russian" "English/Norwegian" "English/SimplifiedChinese" "English/TraditionalChinese" "Spanish/English"
    "French/English" "German/English" "Italian/English" "Dutch/English" "Portuguese/English"))

(defun babel-free-translation (from to)
  (let* ((ffrom (cdr (assoc from babel-free-languages)))
         (fto   (cdr (assoc to babel-free-languages)))
         (trans (concat ffrom "/" fto)))
    (find trans babel-free-translations :test #'string=)))

(defun babel-free-fetch (msg from to)
  "Connect to the FreeTranslation server and request the translation."
  (require 'url)
  (let ((coding-system-for-read 'utf-8)
        (translation (babel-free-translation from to))
        url)
    (unless translation
      (error "FreeTranslation can't translate from %s to %s" from to))
    (setq url
          (cond ((string= translation "English/Russian") "http://ets6.freetranslation.com/")
                ((string= translation "English/SimplifiedChinese") "http://ets6.freetranslation.com/")
                ((string= translation "English/TraditionalChinese") "http://ets6.freetranslation.com/")
                (t "http://ets.freetranslation.com/")))
    (let* ((pairs `(("sequence"  . "core")
                    ("mode"      . "html")
                    ("template"  . "results_en-us.htm")
                    ("srctext"   . ,msg)
                    ("charset"   . "UTF-8")
                    ("language"  . ,translation)))
           (url-request-data (babel-form-encode pairs))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (babel-url-retrieve url 'utf-8))))

(defun babel-free-wash ()
  "Extract the useful information from the HTML returned by FreeTranslation."
  ;;; <textarea name="dsttext" cols="40" rows="6">hello together</textarea><br />
  (if (not (babel-wash-regex "<textarea name=\"dsttext\"[^>]+>\\([^<]*\\)</textarea>"))
      (error "FreeTranslations HTML has changed ; please look for a new version of babel.el")))

;; Google stuff ===========================================
;;
;; Contributed by Juergen Hoetzel <juer...@hoetzel.info> (Only works on emacs-22.0.5 (CVS))

;; translation from 3-letter names to Google 2-letter names
(defconst babel-google-languages
  '(("eng" . "en")
    ("ger" . "de")
    ("spe" . "es")
    ("fre" . "fr")
    ("ita" . "it")
    ("pob" . "pt")
    ("jpn" . "ja")
    ;TODO:  "en|ko"English to Korean BETA
    ;TODO:  "en|zh-CN"English to Chinese&nbsp;(Simplified) BETA      
    ))

;; those inter-language translations that Google is capable of
(defconst babel-google-translations
  '("en|de" "en|es" "en|fr" "en|it" "en|pt" "en|ja" "en|ko" "en|zh-CN"
    "de|en" "de|fr" "es|en" "fr|en" "fr|de" "it|en" "pt|en" "ja|en" "ko|en" "zh-CN|en"))

;; if Google is able to translate from language FROM to language
;; TO, then return the corresponding string, otherwise return nil
(defun babel-google-translation (from to)
  (let* ((fromb (cdr (assoc from babel-google-languages)))
         (tob   (cdr (assoc to babel-google-languages)))
         (comb (and fromb tob (concat fromb "|" tob))))
    (find comb babel-google-translations :test #'string=)))

(defun babel-google-fetch (msg from to)
  "Connect to google server and request the translation."
  (require 'url)
  (let ((coding-system-for-read 'utf-8)
        (translation (babel-google-translation from to)))
    (unless translation
      (error "PROMPT can't translate from %s to %s" from to))
    (let* ((pairs `(("text"       . ,(mm-encode-coding-string msg 'utf-8))
                    ("hl"         . "en")
                    ("Language"   . "English")
                    ("ie"         . "UTF-8")
                    ("oe"         . "UTF-8")
                    ("langpair" . ,translation)))
           (url-request-data (babel-form-encode pairs))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded"))))
      (babel-url-retrieve "http://translate.google.com/translate_t" 'utf-8))))

(defun babel-google-wash ()
  "Extract the useful information from the HTML returned by google."
  (if (not (babel-wash-regex "<textarea name=q[^>]+>\\([^<]*\\)</textarea>"))
      (error "Google HTML has changed ; please look for a new version of babel.el")))

;; TODO: ecs.freetranslation.com

;; (defun babel-debug ()
;;   (let ((buf (get-buffer-create "*babel-debug*")))
;;     (set-buffer buf)
;;     (babel-free-fetch "state mechanisms are too busy" "eng" "ger")))

(easy-menu-add-item nil '("tools") ["Babel Translation" babel t])

(provide 'babel)

;; babel.el ends here
