;;; webjump-plus.el -- supplemental Web site list for webjump
     
;; Copyright (C) 1997-2003 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <neil@neilvandyke.org>
;; Version:  2.4
;; X-URL:    http://www.neilvandyke.org/webjump/
;; X-CVS:    $Id: webjump-plus.el,v 1.61 2003/05/23 18:33:29 neil Exp $ GMT

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines a WebJump hotlist, `webjump-plus-sites', to supplement
;; your personal hotlist and the sample hotlist defined in `webjump.el'.
;; Included here are Web site entries that do not really belong in
;; `webjump.el', but that may save people from having to reinvent the wheel.
;; This package will be updated more frequently than `webjump.el'.

;; To use WebJump and `webjump-plus', you may wish to add something like the
;; following to your `.emacs' file:
;;
;;   (require 'webjump-plus)
;;   (global-set-key "\C-cj" 'webjump)
;;   (setq webjump-sites
;;         (append '(
;;                   ("My Home Page" . "www.someisp.foo/users/joebobjr/")
;;                   ("Pop's Site"   . "www.joebob-and-son.foo/")
;;                   )
;;                 webjump-plus-sites
;;                 webjump-sample-sites))
;;
;; The above loads this package (and also the `webjump' package), binds `C-c j'
;; to invoke WebJump, and adds your personal favorite sites to the hotlist.

;; TODO: I really must made a certain couple syntax tweaks to `webjump.el'.

;;; Change Log:

;; [Version 2.4, 23-May-2003]
;; * Added "CiteSeer Citation Search", "CiteSeer Document Search", "Google
;;   News".
;; * Changed "MapQuest Address", "PGPi", "Project Gutenberg", "RFC Editor",
;;   "US Patents", "W3C".
;; * Removed "Cryptome" (it was just a URL, and not directly relevant to
;;   Emacs, Free Software, or CS), "Ohio State Emacs Lisp Archive" (seems to no
;;   longer exist).
;;
;; [Version 2.3, 02-Dec-2002]
;; * Fixed "MapQuest Address" bug for non-default country.
;; * Updated "ACM Digital Library", "Internet Drafts", "MapQuest Address",
;;   "NTK", "PGP Key Server", "US Patents", "W3C" (they use Google now),
;; * Removed "ACLU" (they've disabled their search function), "Ask Jeeves"
;;   (just too useless).
;; * Added "Cambridge Dictionaries Online", "EmacsWiki", "IMDB", 
;; * Removed editorial comments; foster an air of mystery.
;; * Updated email address.
;;
;; [Version 2.2, 14-Feb-2001]
;; * This Valentine's Day, give the gift of WebJump.
;; * Added big new "Java API" support for Java2SE JDK 1.3.
;; * Added smaller new "Google Groups", "Java Docs", "W3C".
;; * Enhanced "Advogato".
;; * Removed the old Java API 1.1 entry from webjump-plus-sites', but you can
;;   left the support in for it so that you can add it back in to your own
;;   `webjump-sites' list as:   ("Java API 1.1" . webjump-to-javaapi11)
;;
;; [Version 2.1, 09-Feb-2001]
;; * Minor formatting changes.  Not posted to `gnu.emacs.sources'.
;;
;; [Version 2.0, 09-Feb-2001]
;; * Almost complete redo of hotlist.
;; * Note that `webjump-plus-sites' no longer includes `webjump-sample-sites';
;;   see revised example for how to include both with your personal hotlist.
;; * Note that `webjump-to-javaapi-prefix' has been renamed to
;;   `webjump-to-javaapi11-prefix'.
;;
;; [Version 1.3, 04-Dec-1998] Too many changes to note.
;;
;; [Version 1.2, 17-Jul-1997] Split some constants of `webjump-to-javaapi' out
;; into defvars.  Added IBM Patent Server and Computer Science Bibliographies.
;; Added ISI Internet RFCs and Standards.  Added Merriam-Webster.
;;
;; [Version 1.1, 07-Jun-1997] Made append `webjump-sample-sites'.  Added
;; several sites removed from `webjump.el' at RMS' request.
;;
;; [Version 1.0, 02-Jun-1997] Created.  Not much here yet, but it establishes
;; the idea of a separate package of WebJump samples site entries.

;;; Code:

(require 'webjump)

(defconst webjump-plus-version "2.4")

(defconst webjump-plus-sites
  `(

    ("ACM Digital Library" .
     [simple-query "www.acm.org/dl/"
                   "portal.acm.org/results.cfm?coll=portal&dl=ACM&query="
                   ""])

    ("Advogato" . [simple-query "advogato.org" "advogato.org/person/" "/"])

    ("Bartleby.com" .
     [simple-query
      "www.bartleby.com"
      "www.bartleby.com/cgi-bin/texis/webinator/sitesearch?FILTER=&query=" ""])

    ("Cambridge Dictionaries Online" .
     [simple-query "dictionary.cambridge.org"
                   "dictionary.cambridge.org/cmd_search.asp?searchword="
                   ""])

    ("CiteSeer Document Search" .
     [simple-query "citeseer.nj.nec.com"
                   "citeseer.nj.nec.com/cs?q="
                   "&submit=Search+Documents&cs=1"])

    ("CiteSeer Citation Search" .
     [simple-query "citeseer.nj.nec.com"
                   "citeseer.nj.nec.com/cs?q="
                   "&submit=Search+Citations&cs=1"])

    ("Collection of Computer Science Bibliographies" .
     [simple-query "liinwww.ira.uka.de/bibliography/"
                   "liinwww.ira.uka.de/searchbib/index?query="
                   ""])

    ("Debian Bug Number" .
     [simple-query "www.debian.org/Bugs/"
                   "bugs.debian.org/cgi-bin/bugreport.cgi?bug="
                   ""])

    ("Debian GNU/Linux" .
     [simple-query "www.debian.org" "search.debian.org/?q=" ""])

    ("Dictionary.com" .
     [simple-query "www.dictionary.com"
                   "www.dictionary.com/cgi-bin/dict.pl?term=" "&db=*"])
    
    ("Ebay" .
     [simple-query "www.ebay.com"
                   "search.ebay.com/search/search.dll?query="
                   ""])

    ("Emacs Lisp List" . "anc.ed.ac.uk/~stephen/emacs/ell.html")

    ("EmacsWiki" . 
     [simple-query "www.emacswiki.org/cgi-bin/wiki.pl"
                   "www.emacswiki.org/cgi-bin/wiki.pl?search="
                   "&dosearch=1"])

    ("Fairness & Accuracy In Reporting" .
     [simple-query "www.fair.org"
                   "www.fair.org/search.cgi?Match=0&Realm=All&Terms=" ""])

    ("Freshmeat" .
     [simple-query "freshmeat.net" "freshmeat.net/search/?q=" ""])

    ("Geektools Whois" .
     [simple-query "www.geektools.com/whois.html"
                   "www.geektools.com/geektools-cgi/whois.cgi?query=" ""])

    ("GnuPG" . "www.gnupg.org")

    ("Google" .
     [simple-query "www.google.com" "www.google.com/search?q=" ""])

    ("Google Groups" .
     [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])

    ("Google News" .
     [simple-query "news.google.com" "news.google.com/news?q=" ""])

    ("IMDB" .
     [simple-query "www.imdb.com" "www.imdb.com/Find?select=All&for=" ""])

    ("Internet Drafts" .
     [simple-query
      "www.ietf.org/ID.html"
      ,(concat "search.ietf.org/cgi-bin/htsearch?restrict="
               (webjump-url-encode "http://www.ietf.org/internet-drafts/")
               "&words=")
      ""])

    ("Java API" . webjump-to-javaapi)

    ("Java Docs" .
     [simple-query "http://java.sun.com/j2se/1.3/docs/"
                   "http://search.java.sun.com/query.html?qt=" ""])

    ("Linux Kernel Archives" . "www.kernel.org")
                   
    ("Mailcrypt" . "mailcrypt.sourceforge.net")

    ("MapQuest Address" . webjump-to-mapquestaddr)

    ("Merriam-Webster Dictionary" .
     [simple-query "www.m-w.com/dictionary"
                   "www.m-w.com/cgi-bin/netdict?va=" ""])

    ("MP3.Com" .
     [simple-query "www.mp3.com" "search.mp3.com/bin/search/?query=" ""])

    ("NTK" .
     [simple-query "www.ntk.net"
                   "www.ntk.net/index.cgi?searchv=" "&search.x=1&search.y=1"])

    ("Ohio State Emacs Lisp Archive" .
     [simple-query "www.cis.ohio-state.edu/emacs-lisp/"
                   "neutral.verbum.org/search?q=" "&archive=archive"])

    ("PGP Key Server" .
     [simple-query "pgp.mit.edu"
                   "pgp.mit.edu:11371/pks/lookup?op=index&search=" ""])
    
    ("PGPi" .
     [simple-query "www.pgpi.org" "www.pgpi.org/cgi/search.cgi?keywords=" ""])

    ("Project Gutenberg" . webjump-to-gutenberg)

    ("RFC Editor" .
     [simple-query "www.rfc-editor.org"
                   "www.rfc-editor.org/cgi-bin/rfcsearch.pl?searchwords="
                   ,(concat "&opt=All%20Fields"
                            "&filefmt=txt"
                            "&search_doc=search_all"
                            "&match_method=prefix"
                            "&sort_method=newer"
                            "&num=25"
                            "&format=ftp")])
    
    ("Roget's Internet Thesaurus" .
     [simple-query "www.thesaurus.com"
                   "www.thesaurus.com/cgi-bin/htsearch?config=roget&words="
                   ""])

    ("Rpmfind" .
     [simple-query "rpmfind.net/linux/RPM/"
                   "rpmfind.net/linux/rpm2html/search.php?query=" ""])

    ("Red Rock Eater News Service" .
     "dlis.gseis.ucla.edu/people/pagre/rre.html")

    ("Slashdot" .
     [simple-query "slashdot.org" "slashdot.org/search.pl?query=" ""])

    ("The Register" .
     [simple-query "www.theregister.co.uk"
                   "www.theregister.co.uk/cgi-bin/dispatcher.cgi?search="
                   "&url=section_1.html&action=search"])

    ("US Patents" .
     [simple-query
      "www.uspto.gov/patft/"
      ,(concat "appft1.uspto.gov/netacgi/nph-Parser?Sect1=PTO2&Sect2=HITOFF"
               "&p=1&u=%2Fnetahtml%2FPTO%2Fsearch-bool.html&r=0&f=S&l=50"
               "&TERM1=")
      "&FIELD1=&co1=AND&TERM2=&FIELD2=&d=PG01"])

    ("W3C" .
     [simple-query
      "www.w3.org"
      "www.google.com/custom?q="
      ,(concat "&sa=Go&cof=T%3Ablack%3BLW%3A72%3BALC%3A%23ff3300%3BL%3A"
               "http%3A%2F%2Fwww.w3.org%2FIcons%2F"
               "w3c_home%3BLC%3A%23000099%3BLH%3A48%3BBGC%3Awhite"
               "%3BAH%3Aleft%3BVLC%3A%23660066%3BGL%3A0%3BAWFID"
               "%3A0b9847e42caf283e%3B&sitesearch=w3.org&domains=w3.org")])

    ("webjump.el" . "www.neilvandyke.org/webjump/")

    ("XEmacs" . [simple-query "www.xemacs.org"
                              "www.xemacs.org/cgi-bin/namazu.cgi?query="
                              "&whence=0&lang=en&submit=Go"])))

;; Misc.:

(defun webjump-split-on-char (string char)
  ;; Note: Taken from spamprod.el by me. Note that this could be more elegant.
  (when (and string char)
    (when (stringp char)
      (if (= (length char) 1)
          (setq char (aref 0 char))
        (error "webjump-split-on-char: char is a string of length /= 1.")))
    (let* ((start  0)
           (length (length string))
           (i      0)
           (result '()))
      (while (<= i length)
        (when (or (= i length) (= (aref string i) char))
          (setq result (nconc result (list (substring string start i))))
          (setq start (1+ i)))
        (setq i (1+ i)))
      result)))

;; Code for webjump-to-gutenberg:

(defun webjump-to-gutenberg (name)
  (let ((author (webjump-read-string (concat name " author")))
        (title  (webjump-read-string (concat name " title words"))))
    (if (or author title)
        (concat
         "http://www.ibiblio.org/gutenberg/cgi-bin/sdb/t9.cgi"
         "?whole=yes"
         "&author="
         (webjump-url-encode author)
         "&title="
         (webjump-url-encode title)
         "&ftpsite="
         (webjump-url-encode "http://www.ibiblio.org/gutenberg/"))
      "http://www.gutenberg.net/")))

;; Code for webjump-to-javaapi11:

(defvar webjump-to-javaapi11-prefix 
  "http://www.javasoft.com/products/jdk/1.1/docs/api/"
  "*URL prefix for `webjump-to-javaapi11'.")

(defvar webjump-to-javaapi11-packages
  '("java.applet" "java.awt" "java.awt.datatransfer" "java.awt.event"
    "java.awt.image" "java.awt.peer" "java.beans" "java.io" "java.lang"
    "java.lang.reflect" "java.math" "java.net" "java.rmi" "java.rmi.dgc"
    "java.rmi.registry" "java.rmi.server" "java.security" "java.security.acl"
    "java.security.interfaces" "java.sql" "java.text" "java.util"
    "java.util.zip" "sun.tools.debug" "sunw.io" "sunw.util")
  "*List of Java packages for `webjump-to-javaapi11'.")

(defun webjump-to-javaapi11 (name)
  (let* ((prefix webjump-to-javaapi11-prefix)
	 (packages (mapcar 'list webjump-to-javaapi11-packages))
	 (completion-ignore-case t)
	 (package (completing-read (concat name " package: ") packages nil t)))
    (if (webjump-null-or-blank-string-p package)
        (concat prefix "packages.html")
      (concat prefix "Package-" package ".html"))))

;; Code for webjump-to-javaapi:

;; Note that the `webjump-to-javaapi' stuff has gotten a little big.  It might
;; eventually be modified to fit into the Emacs JDE project instead, if they
;; want it.

(defvar webjump-to-javaapi-default-url
  "http://java.sun.com/j2se/1.3/docs/api/overview-summary.html"
  "*Default URL for `webjump-to-javaapi'.")

(defvar webjump-to-javaapi-dir-package-filenames
  '("package-summary.html" "index.html")
  "*Base filenames under the directory trees of `webjump-to-javaapi-dirs'.")

(defvar webjump-to-javaapi-dirs
  '()
  "*List of directories that contain trees of Java API documentation.  Each
list entry can have one of two forms.  The simple form of an entry is a
string, which is the name of a directory tree root, such as:

    \"/usr/java/jdk1.3/docs/api\"
    
The alternate form of an entry is a list of two elements, with the first
element being the directory tree root, and the second element being a java
package prefix to which package documentation in the tree is relative.  For
example:

    (\"/home/jabba/my-java-packages\" \"FOO.jabba.leet.pkgs.yo\")

Each directory tree will be scanned by `webjump-to-javaapi-compile' to help
build `wejump-to-javaapi-alist'.  If these directory trees include the normal
JDK API docs, then you should set `webjump-to-javaapi-jdk-packages' to nil.

Note that the scanning of these directories is unfortunately slow, and uncached
between Emacs sessions.  Stay tuned.")

(defvar webjump-to-javaapi-jdk-packages
  '("java.applet" "java.awt" "java.awt.color" "java.awt.datatransfer"
    "java.awt.dnd" "java.awt.event" "java.awt.font" "java.awt.geom"
    "java.awt.im" "java.awt.im.spi" "java.awt.image"
    "java.awt.image.renderable" "java.awt.print" "java.beans"
    "java.beans.beancontext" "java.io" "java.lang" "java.lang.ref"
    "java.lang.reflect" "java.math" "java.net" "java.rmi" "java.rmi.activation"
    "java.rmi.dgc" "java.rmi.registry" "java.rmi.server" "java.security"
    "java.security.acl" "java.security.cert" "java.security.interfaces"
    "java.security.spec" "java.sql" "java.text" "java.util" "java.util.jar"
    "java.util.zip" "javax.accessibility" "javax.naming"
    "javax.naming.directory" "javax.naming.event" "javax.naming.ldap"
    "javax.naming.spi" "javax.rmi" "javax.rmi.CORBA" "javax.sound.midi"
    "javax.sound.midi.spi" "javax.sound.sampled" "javax.sound.sampled.spi"
    "javax.swing" "javax.swing.border" "javax.swing.colorchooser"
    "javax.swing.event" "javax.swing.filechooser" "javax.swing.plaf"
    "javax.swing.plaf.basic" "javax.swing.plaf.metal" "javax.swing.plaf.multi"
    "javax.swing.table" "javax.swing.text" "javax.swing.text.html"
    "javax.swing.text.html.parser" "javax.swing.text.rtf" "javax.swing.tree"
    "javax.swing.undo" "javax.transaction" "org.omg.CORBA"
    "org.omg.CORBA.DynAnyPackage" "org.omg.CORBA.ORBPackage"
    "org.omg.CORBA.TypeCodePackage" "org.omg.CORBA.portable"
    "org.omg.CORBA_2_3" "org.omg.CORBA_2_3.portable" "org.omg.CosNaming"
    "org.omg.CosNaming.NamingContextPackage" "org.omg.SendingContext"
    "org.omg.stub.java.rmi")
  "*List of Java JDK package names under `webjump-to-javaapi-jdk-prefix'.
Set this variable to nil if you prefer to use solely `webjump-to-javaapi-dirs'
for specifying where to find API documentation.")

(defvar webjump-to-javaapi-jdk-prefix 
  "http://java.sun.com/j2se/1.3/docs/api/"
  "*JDK URL prefix for `webjump-to-javaapi'.
Used if `webjump-to-javaapi-jdk-packages' is non-nil.")

(defvar webjump-to-javaapi-jdk-suffix
  "package-summary.html"
  "*JDK URL suffix for `webjump-to-javaapi'.
Used if `webjump-to-javaapi-jdk-packages' is non-nil.")

(defvar webjump-to-javaapi-alist nil
  "Compiled alist for `webjump-to-javaapi'.
Usually this is set via `webjump-to-javaapi-compile', not directly by users.")

(defun webjump-to-javaapi (name)
  (unless webjump-to-javaapi-alist
    (message "%s compiling..." name)
    (webjump-to-javaapi-compile))
  (let ((url (webjump-read-choice name
                                  "package"
                                  webjump-to-javaapi-alist)))
    (if (webjump-null-or-blank-string-p url)
        webjump-to-javaapi-default-url
      url)))

(defun webjump-to-javaapi-compile ()
  (let ((alist nil))
    ;; Build from JDK Web docs.
    (when (and webjump-to-javaapi-jdk-packages 
               webjump-to-javaapi-jdk-prefix)
      (setq alist
            (mapcar (function
                     (lambda (package)
                       (cons package
                             (concat webjump-to-javaapi-jdk-prefix 
                                     (mapconcat
                                      'identity
                                      (webjump-split-on-char package ?.)
                                      "/")
                                     "/"
                                     webjump-to-javaapi-jdk-suffix))))
                    webjump-to-javaapi-jdk-packages)))
    ;; Scan directories.
    (mapcar
     (function
      (lambda (entry)
        (let (dir dotpath)
          (cond ((stringp entry) (setq dir entry))
                ((listp entry)   (setq dir     (nth 0 entry)
                                       dotpath (nth 1 entry)))
                (t (error "Syntax error in `webjump-to-javaapi-dirs.")))
          (setq alist
                (webjump-to-javaapi-compile-dir dir dotpath alist)))))
     webjump-to-javaapi-dirs)
    ;; Sort.
    (setq alist (sort alist (function (lambda (a b)
                                        (string-lessp (car a) (car b))))))
    ;; Set and return.
    (setq webjump-to-javaapi-alist alist)))

(defun webjump-to-javaapi-compile-dir (dir &optional dotpath alist)
  (let ((files       (condition-case nil (directory-files dir) (error '())))
        (indexless-p t))
    (mapcar
     (function
      (lambda (file-base)
        (unless (eq (aref file-base 0) ?.)
          (let ((file-full (expand-file-name file-base dir)))
            (if (file-directory-p file-full)
                (setq alist (webjump-to-javaapi-compile-dir
                             file-full
                             (if dotpath
                                 (concat dotpath "." file-base)
                               file-base)
                             alist))
              (when (and dotpath
                         indexless-p
                         (member file-base
                                 webjump-to-javaapi-dir-package-filenames))
                (setq alist (cons (cons dotpath (concat "file:" file-full))
                                  alist))))))))
     files))
  alist)

;; Code for webjump-to-mapquestaddr:

(defvar webjump-to-mapquestaddr-countries
  (mapcar (function (lambda (n) (cons n n)))
          '("Afghanistan" "Albania" "Algeria" "Samoa" "Andorra" "Angola"
            "Anguilla" "Antigua and Barbuda" "Argentina" "Armenia" "Aruba" 
            "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh"
            "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bermuda" "Bhutan"
            "Bolivia" "Bosnia and Herzegovina" "Botswana" "Brazil"
            "British Virgin Islands" "Bulgaria" "Burkina Faso" "Burundi"
            "Cambodia" "Cameroon" "Canada" "Cape Verde" "Cayman Islands"
            "Central African Republic" "Chad" "Chile" "China" "Colombia"
            "Comoros" "Congo" "Zaire" "Cook Islands" "Costa Rica" "Ivory Coast"
            "Croatia" "Cuba" "Cyprus" "Czech Republic" "Denmark" "Djibouti"
            "Dominica" "Dominican Republic" "Ecuador" "Egypt" "El Salvador"
            "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia"
            "Falkland Islands" "Faroe Islands" "Fiji" "Finland" "France"
            "French Guiana" "French Polynesia" "Gabon" "Gambia" "Georgia"
            "Germany" "Ghana" "Gibraltar" "Greece" "Greenland" "Grenada"
            "Guadeloupe" "Guatemala" "Guinea" "Guinea Bissau" "Guyana" "Haiti"
            "Honduras" "Hungary" "Iceland" "Samoa" "India" "Indonesia" "Iran"
            "Iraq" "Ireland" "Israel" "Italy" "Jamaica" "Japan" "Jordan"
            "Kazakhstan" "Kenya" "Kiribati" "Kuwait" "Kyrgyzstan" "Laos"
            "Latvia" "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein"
            "Lithuania" "Luxembourg" "Macau" "Macedonia" "Madagascar" "Malawi"
            "Malaysia" "Maldives" "Mali" "Malta" "Martinique"
            "Marshall Islands" "Mauritania" "Mauritius" "Mexico" "Micronesia"
            "Moldova" "Monaco" "Mongolia" "Montserrat" "Morocco" "Mozambique"
            "Myanmar" "Namibia" "Nepal" "Netherlands" "Netherlands Antilles"
            "New Caledonia" "New Zealand" "Nicaragua" "Niger" "Nigeria"
            "Norfolk Island" "North Korea" "Northern Mariana Islands" "Norway"
            "Oman" "Pakistan" "Palau" "Panama" "Papua New Guinea" "Paraguay"
            "Peru" "Philippines" "Poland" "Portugal" "Puerto Rico" "Qatar"
            "Reunion" "Romania" "Russia" "Rwanda" "Saint Helena"
            "Saint Kitts and Nevis" "Saint Lucia" "Saint Pierre and Miquelon"
            "Saint Vincent/Grenadines" "San Marino" "Saotome and Principe"
            "Saudi Arabia" "Senegal" "Seychelles" "Sierra Leone" "Singapore"
            "Slovak Republic" "Slovenia" "Solomon Islands" "Somalia"
            "South Africa" "South Korea" "Spain" "Sri Lanka" "Sudan" "Suriname"
            "Swaziland" "Sweden" "Switzerland" "Syria" "Taiwan" "Tajikistan"
            "Tanzania" "Thailand" "Togo" "Tokelau" "Tonga"
            "Trinidad and Tobago" "Tunisia" "Turkey" "Turkmenistan"
            "Turks and Caicos Islands" "Uganda" "Ukraine"
            "United Arab Emirates" "Great Britain" "United States"
            "United States Virgin Islands" "Uruguay" "Uzbekistan" "Vanuatu"
            "Vatican City" "Venezuela" "Vietnam" "Western Sahara" "Yemen"
            "Yugoslavia" "Zambia" "Zimbabwe"))
  "*List of country names for `webjump-to-mapquestaddr'.")

(defun webjump-to-mapquestaddr (name)
  (let ((country (webjump-read-choice name
                                      "country (default \"United States\")"
                                      webjump-to-mapquestaddr-countries
                                      "United States")))
    ;; TODO: Make the queries dependent on the country.
    (let ((street (webjump-read-string (concat name " street address")))
          (city   (webjump-read-string (concat name " city")))
          (state  (webjump-read-string (concat name " state")))
          (zip    (webjump-read-string (concat name " zip code"))))
      (concat
       "http://www.mapquest.com/maps/map.adp?"
       "country="  (webjump-url-encode country)
       "&address=" (webjump-url-encode street)
       "&city="    (webjump-url-encode city)
       "&state="   (webjump-url-encode state)
       "&zip="     (webjump-url-encode zip)))))

;; End:

(provide 'webjump-plus)

;; webjump-plus.el ends here
