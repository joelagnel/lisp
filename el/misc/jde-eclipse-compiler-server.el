;;; jde-eclipse-compiler-server.el --- Eclipse compiler as a compile server for JDEE.


;; Copyright (C) 2006 by Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>
;; Maintainer: Suraj Acharya <sacharya@cs.indiana.edu>
;; Created: 22 Feb 2006
;; Keywords: cc-mode java annotations indentation

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; 
;; This library adds the option of using the eclipse java compiler as
;; a compile server to `jde-compiler'.  See
;; http://help.eclipse.org/help31/index.jsp?topic=/org.eclipse.jdt.doc.isv/guide/jdt_api_compile.htm
;; for a description of the eclipse batch compiler and a list of all
;; the "warn" options that it can take.

;; To use this library, ensure that this file in your load path and
;; add the following code to your .enacs: (require
;; 'jde-eclipse-compiler-server)

;; Customizing jde-compiler after this should give you a buffer that looks like this:

;; Operate on everything in this buffer:
;;  Set for Current Session Save for Future Sessions
;;  Reset Reset to Saved Erase Customization   Finish

;; Jde Compiler: Hide Value
;; Compiler type 
;; ( ) javac
;; ( ) javac server
;; ( ) eclipse java compiler server
;;     Path to jdt core jar: 
;; (*) jikes
;;     Path to jikes: c:/jde/jikes-1.22/jikes.exe
;;    State: this option has been changed outside the customize buffer.

;; Note the new option for "eclipse java compiler server". After
;; selecting this option you will also need to specify the location of
;; the eclipse java compiler classes. 

;; If you've installed eclipse locally then this is the jdtcore.jar
;; under <eclipse dir>/plugins/org.eclipse.jdt.core_x.x.x/, where
;; x.x.x depends on the version of eclipse you have.

;; If you don't have eclipse you can download just the JDT
;; compiler. Go to http://download.eclipse.org/eclipse/downloads/ and
;; pick the release you want, the latest release is usually stable
;; enough to use. Once you get to the downloads page for the release,
;; scroll down to find the link to download just the "JDT Runtime
;; Binary". The compiler is included in a jar inside this zip and is
;; called something like org.eclipse.jdt.core_x.x.x.jar

;; Check that you have the correct jar by trying to run the compiler
;; from a command line like so:
;; java -cp <path to jdt core jar> org.eclipse.jdt.internal.compiler.batch.Main
;; This should print out a usage message for the "Eclipse Java Compiler".

;; This library changes the format of the jde-compiler variable so you
;; might encounter problems the first time you switch to using this
;; library or the first time you switch back you have a value for
;; jde-compiler customized using the old customizer.

;; If you see a customization buffer like this :

;;  Set for Current Session Save for Future Sessions
;;  Reset Reset to Saved Erase Customization   Finish
;; 
;; jde-compiler: Hide Value 
;; '(("eclipse java compiler server" "c:/org.eclipse.jdt.core_3.0.0/org.eclipse.jdt.core_3.1.1.jar"))
;;    State: this option has been set and saved. (mismatch)

;; That is, there are no radio buttons to select, set the value of
;; jde-compiler manually using the minibuffer to its default. If you
;; are not using jde-eclipse-compiler-server the default is '("javac
;; server" ""), and if you are it is '("javac server")



;;; Code:
(defclass jde-compile-ejc-server (jde-compile-compiler)
  ()
  "Class for using the Eclipse java compiler as a JDEE compile server."
)

(defcustom jde-compiler-new-compile-el 
  (condition-case err
    (progn
      (symbol-value 'compilation-error-regexp-alist-alist) 
      t)
    (void-variable nil))
  "Check if we have the new (21.3+) compile.el.
Set this to t if you are running an Emacs with the new compile.el
and want to get slightly better font-locking in the compile
buffer. A value of nil will force the use of older style
compilation-error-regexp. This variable tries to auto-detect the
compile.el version by checking if
`compilation-error-regexp-alist-alist' is defined."
  :type 'boolean)


(if jde-compiler-new-compile-el
    (progn
      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. ERROR in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 2 1 (6 compilation-error-face)
                    )
                  compilation-error-regexp-alist))
      
      (setq compilation-error-regexp-alist
            (cons '("----------\n\\([0-9]+. WARNING in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)\\)"
                    2 3 nil 1 1 (6 compilation-warning-face)
                    )
                  compilation-error-regexp-alist)))
  ;; else
  (setq compilation-error-regexp-alist
        (cons '("----------\n[0-9]+. \\(ERROR\\|WARNING\\) in \\(.*\\)\n (at line \\([0-9]+\\))\n\\(\\(.*\n\\)+?\\).*^+\n\\(.*\n\\)"
                2 3)
              compilation-error-regexp-alist)))


(defmethod jde-compile-run-server ((this jde-compile-ejc-server))
    (let* ((directory-sep-char ?/)
	   (args
	    (cons
             "-noExit"
	    (jde-compile-get-args this)))
	   (source-path
	    (jde-normalize-path buffer-file-name))
	   (arg-array (concat "new String[] {\"" source-path "\"")))
    
      (if args
	  (setq arg-array
		(concat
		 arg-array
		 ","
		 (mapconcat
		  (lambda (arg)
		    (concat "\"" arg "\""))
		  args
		  ","))))

      (setq arg-array (concat arg-array "}"))
     
	
      (save-excursion
	(set-buffer (oref (oref this buffer) buffer))

	(insert "CompileServer output:\n")
	(insert "\n")

	(let (flag temp)
	  (setq temp
	    (mapconcat
	     (lambda (x)
	       (if (and flag
			jde-compile-option-hide-classpath)
		   (progn
		     (setq flag nil)
		     "...")
		 (if (not (string= x "-classpath"))
		     x
		   (progn
		     (setq flag t)
		     x)))) args " "))

	  (insert temp " "))

	(insert source-path "\n"))


      (if (not (jde-bsh-running-p))
	  (progn
	    (bsh-launch (oref 'jde-bsh the-bsh))
	    (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))
      (bsh-eval (oref 'jde-bsh the-bsh)
                (format "addClassPath(\"%s\");\n" (oref this :path))
                )
      (bsh-buffer-eval
       (oref 'jde-bsh the-bsh)
       (concat
	(format
         "if ((new org.eclipse.jdt.internal.compiler.batch.Main(new java.io.PrintWriter(System.out), new java.io.PrintWriter(System.err), true)).compile(%s)) { print (\"0\");} else {print (\"1\");};"
         arg-array)
	"\n")
       (oref this buffer))))

;; convert jde-compiler values from the format defined in
;; jde-compile.el to the one used in this file
(let ((compiler-name (car jde-compiler)))
(when (not (listp compiler-name))
  (setq jde-compiler
        (cond 
         ((equal compiler-name "javac") '("javac"))
         ((equal compiler-name "javac server") '("javac server"))
         ((equal compiler-name "jikes") (list (cons "jikes" (cdr jde-compiler))))
         (t "javac server")))))


(defcustom jde-compiler '("javac server")
  "Specify the type, and if necessary, the location of the compiler to
be used to compile source files for the current project. The JDE
supports three compilers: javac server, javac executable, and
jikes. The javac server runs the com.sun.tools.javac package included
with the JDK in the Beanshell. The javac executable shipped with the
JDK also uses this package. The advantage of the javac server is that
it avoids the vm startup time that accounts for most of the
compilation time consumed by the javac executable. The javac server
uses the version of com.sun.tools.javac included in the JDK for the
current project. See `jde-jdk' for more information. If you want to
use the javac executable to compile your project's source files,
select \"javac\" as the compiler type and, optionally, specify
the path to the executable in the \"Path\" field. If you do
not specify a path, the JDE uses the javac executable included in the
JDK for the current project. Similarly, to use jikes, select \"jikes\"
and, if jikes is not on the command path of the Emacs
environment, specify the path of the jikes executable."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Compiler type"
           (item "javac")
	   (item "javac server")
	   (list :format "%v"
                 (const "eclipse java compiler server")
                 (file :tag "Path to jdt core jar"))
           (list :format "%v"
                 (const "jikes")
                 (file :tag "Path to jikes"))
           ))
  )

(defun jde-compile-get-javac ()
  (let* ((jdk-version (jde-java-version))
	 (jdk-split-version (split-string jdk-version "[.]"))
	 (jdk-major-version (nth 0 jdk-split-version))
	 (jdk-minor-version (nth 1 jdk-split-version))
	 (compiler
	  (find-if
	   (lambda (compiler-x)
	     (let* ((compiler-split-version (split-string (oref compiler-x :version) "[.]"))
		    (compiler-major-version (nth 0 compiler-split-version))
		    (compiler-minor-version (nth 1 compiler-split-version)))
	       (and
		(string= jdk-major-version compiler-major-version)
		(string= jdk-minor-version compiler-minor-version))))
	   jde-compile-javac-compilers)))
    (unless compiler
      (let ((latest-javac (car (last jde-compile-javac-compilers))))
	(if
	    (yes-or-no-p
	     (format "The JDE does not recognize JDK %s javac. Assume JDK %s javac?"
		     jdk-version (oref latest-javac :version)))
	    (setq compiler latest-javac))))
    (if compiler
	(if (string= (car jde-compiler) "javac server")
	    (oset compiler :use-server-p t)
	  (progn
	    (oset compiler :use-server-p nil)
	    (oset compiler
		  :path
		  (let ((compiler-path
                         (if (listp (car jde-compiler))
                             (substitute-in-file-name (nth 1 (car jde-compiler)))
                           "")))
		    (if (string= compiler-path "")
			(setq compiler-path (jde-get-jdk-prog 'javac))
		      (if (file-exists-p compiler-path)
			  compiler-path
			(error (format "Invalid compiler path %s"
				       compiler-path)))))))))
    compiler))
	   
	     
(defun jde-compile-get-jikes ()
  (let ((compiler-path
         (substitute-in-file-name (nth 1 (car jde-compiler)))))

    (if (string= compiler-path "")
	(if (executable-find "jikes")
	    (setq compiler-path "jikes")
	  (error "Cannot find jikes"))
      )

  (jde-compile-jikes
     "Jikes"
     :use-server-p nil
     :path compiler-path)))

(defun jde-compile-get-the-compiler ()
  "Get a compiler object that represents the compiler specified
by `jde-compiler'."
  (let* ((car-jde-compiler (car jde-compiler))
         (compiler-name (if (listp car-jde-compiler) (car car-jde-compiler) car-jde-compiler)))
    (cond
     ((string-match "javac" compiler-name)
       (jde-compile-get-javac))
     ((string-match "jikes" compiler-name)
      (jde-compile-get-jikes))
     ((string-match "eclipse java compiler server" compiler-name)
      (jde-compile-get-ejc))
     (t
      (error "The JDEE does not support a compiler named %s" compiler-name)))))

(defun jde-compile-get-ejc ()
  (let ((compiler-path
         (substitute-in-file-name (nth 1 (car jde-compiler)))))

    (if (string= compiler-path "")
        (error "Cannot find jdt core jar"))
    (jde-compile-ejc-server
     "Eclipse java compiler server"
     :use-server-p t
     :path compiler-path)))


(provide 'jde-eclipse-compiler-server)

;;; jde-eclipse-compiler-server.el ends here
