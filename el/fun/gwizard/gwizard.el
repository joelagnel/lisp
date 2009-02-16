;;; gwizard.el --- helper functions for Gtk+/Gnome/Glib/Bonobo programming

;; Copyright (C) 2001 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Version: 0.0.0
;; Created: 2001/11
;; Date: 2001/11

;; gwizard.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gwizard.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; NOTE: while you are encouraged to release your code under these
;; conditions as well, there's no obligation whatsoever; code generated
;; with gwizard can be under placed under any license.


;;; Documentation

;; Overview:
;;
;; Read http://www.djcbsoftware/projecten/bonobo-gwizard
;;
;; Gwizard provides a number of elisp interactive functions (aka wizards)
;; to generate the boilerplate code for GNOME/Gtk+ related things such as 
;; widgets, GObjects and BonoboObjects.
;; 
;; Most of the 'magic' is course just rewriting of the names.
;;
;; Provided functions:
;;
;; Gwizard provides the following functions:
;; M-x gwizard-new-gtk1-widget                 [UNTESTED]
;; M-x gwizard-new-gnome1-widget               [UNTESTED]
;; M-x gwizard-new-bonobo1-interface
;; M-x gwizard-new-bonobo1-factory 
;;
;; Naming of files:
;;
;; Gwizard tries to mimic the naming conventions of existing libraries,
;; this means that names are in dashed-notation, unless they are in the
;; 'Gtk namespace': in that case the name is simply the downcased object name.
;; Examples:
;;      - GnomeFoobar --> gnome-foo-bar.[ch]
;;      - GtkFoobar   --> gtkfoobar.[ch]
;;      - MyFoobar    --> my-foo-bar.[ch]
;; A special case is BonoboXObject, which corresponds to bonobo-xobject.h
;;
;;

;; Installation:
;; 
;; 1) put gwizard.el somewhere where emacs can find it
;; 2) in your .emacs, put 
;;           (require 'gwizard)
;; 3) that should do it! Now, read 'Customization'.         


;; Customization:
;; 
;; Gwizard tries to discern whether some new widget/object/... has a parent
;; that's part of Gtk+/Gnome/Bonobo., or that the parent's also a custom
;; thing. To do this, gwizard searches the include-dirs for the libraries.
;; It has some reasonable (for me) defaults, but you may want to change them.
;; The variables with their defaults are:
;;
;;     (defvar gwizard-gnomeui-include-path '("/usr/include/libgnome/" "/usr/include/libgnomeui/"))
;;     (defvar gwizard-gtk-include-path '("/usr/include/gtk/"))
;;     (defvar gwizard-bonobo-include-path '("/usr/include/bonobo"))
;;
;; To change them, put in your .emacs (right below "(require 'gwizard)"):
;; (setq gwizard-gnomeui-include-path '("path1" "path2"))
;; (setq gwizard-gtk-include-path '("path1" "path2"))
;; (setq gwizard-bonobo-include-path '("path1" "path2"))
;;
;; IMPORTANT: don't forget to end dirs with a '/'

;; You can also customize the variable gwizard-author, which holds the name that's filled
;; in in (c)-notices etc. Its default value is:
;;
;;     (defvar gwizard-author "Author <email@address>")
;;
;; to change it, put in your .emacs (right below "(require 'gwizard)"):
;; (setq gwizard-author "Evil Hacker <evil@hacks-r-us.org>")


;; Compatibility:
;;
;; gwizard.el was tested with GNU/Emacs 20.x and 21.x. XEmacs probably won't work.
;; We accept patches. 

;;
;; gwizard defaults
;; don't forget the closing '/'
;;
(defvar gwizard-gnomeui-include-path 
  '("/usr/include/libgnome/" 
    "/usr/include/libgnomeui/"))
(defvar gwizard-gtk-include-path 
  '("/usr/include/gtk/"))
(defvar gwizard-bonobo-include-path 
  '("/usr/include/gnome-1.0/bonobo/"))
(defvar gwizard-namespace-names  
  '("Gtk" "Gnome" "Bonobo"))
(defvar gwizard-author 
  "Author <email@address>")

;;
;; main functions
;;
(defun gwizard-new-gnome1-widget (obj parent virtual license)
  "generate a new Gnome v1.x widget"
  (interactive "sWidget name: \ns%s Parent: \ncVirtual (y/n)?: \ncLicense: (1=GPL, 2=LGPL, 3=none): \n")
  (gwizard-new-gtk1-widget obj parent virtual license))


(defun gwizard-new-gtk1-widget (obj parent virtual license)
  "generate a new Gtk+ v1.x widget"
  (interactive "sWidget name: \ns%s Parent: \ncVirtual (y/n)?: \ncLicense: (1=GPL, 2=LGPL, 3=none): \n")   
    ; dot-h
    (find-file (gwizard-dot-h obj))
    (gwizard-clear-buffer)
    (insert (gwizard-file-comment (gwizard-dot-h obj)) "\n"
	    (gwizard-license-text license (gwizard-current-year) gwizard-author) "\n"
	    (gwizard-include-guard-start obj) "\n"
	    (gwizard-gtk-hash-include parent) 
	    "/* TODO: add other include files here */\n\n"
	    (gwizard-cplusplus-start) "\n"
	    (gwizard-macros obj (gwizard-obj-type obj)) "\n"
	    (gwizard-struct-typedefs obj) "\n"
	    (gwizard-obj-struct obj parent) "\n"
	    (gwizard-class-struct obj parent) "\n\n"
	    (gwizard-gtk-standard-functions-decl obj (gwizard-is-virtual virtual))
	    "/* TODO: add other function declarations here */\n\n\n"
	    (gwizard-cplusplus-finish) "\n"
	    (gwizard-include-guard-finish obj))

    ; dot-c
    (find-file (gwizard-dot-c obj))
    (gwizard-clear-buffer)
    (insert (gwizard-file-comment (gwizard-dot-c obj)) "\n"
	    (gwizard-license-text license (gwizard-current-year) gwizard-author) "\n"
	    (gwizard-hash-include obj) "\n"
	    (gwizard-static-func-decls obj) "\n"
	    (gwizard-static-get-set-arg-func-decls obj) "\n"
	    "/* TODO: add additional static function declarations, if any */\n\n"
	    (gwizard-static-parent-class-inst parent) "\n"
	    (gwizard-gtk-get-type-function obj parent) "\n"
	    (gwizard-class-init-func obj parent) "\n"
	    (gwizard-obj-init-func obj) "\n"
	    (gwizard-get-arg-func obj) "\n"
	    (gwizard-set-arg-func obj) "\n"
	    (gwizard-gtk-new-widget-func obj) "\n"
	    "/* TODO: other function implementations */\n"))


(defun gwizard-new-bonobo1-interface (interface parent-interface long-names license)
  "generate a new Bonobo 1.x interface implementation"
  (interactive "sInterface: \nsParent: \ncLong names (y/n)?: \ncLicense: (1=GPL, 2=LGPL, 3=none): \n")
  (let ((obj (gwizard-bonobo-name-from-interface interface (gwizard-long-names long-names)))
	(poa (gwizard-bonobo-poa-from-interface interface))
	(parent (gwizard-bonobo-name-from-interface parent-interface t)))
    (find-file (gwizard-dot-h obj))
     ; dot-h
    (find-file (gwizard-dot-h obj))
    (gwizard-clear-buffer)
    (insert (gwizard-file-comment (gwizard-dot-h obj)) "\n"
	    (gwizard-license-text license (gwizard-current-year) gwizard-author) "\n"
	    (gwizard-include-guard-start obj) "\n"
	    (gwizard-bonobo-hash-include parent)
	    "/*\n * TODO: fill in IDL-generated include file:\n * #include \"...\"\n */\n\n"
	    (gwizard-cplusplus-start) "\n"
	    (gwizard-macros obj (gwizard-bonobo-obj-type obj)) "\n"
	    (gwizard-struct-typedefs obj) "\n"
	    (gwizard-obj-struct obj parent) "\n"
	    (gwizard-bonobo-class-struct obj parent poa) "\n\n"
	    (gwizard-bonobo-standard-functions-decl obj) "\n"
	    "/* TODO: add custom function declarations here */\n\n"
	    (gwizard-cplusplus-finish) "\n"
	    (gwizard-include-guard-finish obj))

    ; dot-c
    (find-file (gwizard-dot-c obj))
    (gwizard-clear-buffer)
    (insert (gwizard-file-comment (gwizard-dot-c obj)) "\n"
	    (gwizard-license-text license (gwizard-current-year) gwizard-author) "\n"
	    (gwizard-hash-include obj) "\n"
	    (gwizard-static-func-decls obj) "\n"
	    (gwizard-bonobo-static-parent-class-inst) "\n"
	    (gwizard-bonobo-get-type-function obj parent poa) "\n"
	    (gwizard-bonobo-class-init-func obj parent poa) "\n"
	    (gwizard-obj-init-func obj) "\n"
	    (gwizard-bonobo-new-func obj) "\n\n"
	    "/*\n * TODO: add your custom function implementations here\n */")))

(defun gwizard-new-bonobo1-factory (interface license)
  "generate a new Bonobo 1.x factory implementation"
  (interactive "sComponent: \ncLicense: (1=GPL, 2=LGPL, 3=none): \n")
  (let* ((obj (gwizard-bonobo-name-from-interface interface t))
	 (component (gwizard-bonobo-underscorify-interface interface))
	 (dot-c (gwizard-dot-c obj)))
  ; dot-c
    (find-file dot-c)
    (gwizard-clear-buffer)
    (insert  (gwizard-file-comment dot-c) "\n"
	     (gwizard-license-text license (gwizard-current-year) gwizard-author) "\n"
	     "/* TODO: #include interface headers */\n\n"
	     (gwizard-factory-func obj) "\n"
	     (gwizard-oaf-macro obj component))  
  ; dot-oaf
    (find-file (concat component ".oaf"))
    (gwizard-clear-buffer)
    (insert (gwizard-oaf-file obj component))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; internal functions
;;


;;
;; general
;;

;1->gpl, 2->lpgl
(defun gwizard-license-text (c year c-holder)
  (cond 
   ((= ?1 c) (gwizard-gpl year c-holder))
   ((= ?2 c) (gwizard-lgpl year c-holder))
   (t "")))

(defun gwizard-get-y-n (c)
  (or (= c ?y) (= c ?Y)))

(defun gwizard-is-virtual (c)
  (gwizard-get-y-n c))

(defun gwizard-long-names (c)
  (gwizard-get-y-n c))

(defun gwizard-include-guard-start (obj)
  (concat 
   "#ifndef " (gwizard-include-guard-label obj) "\n"
   "#define " (gwizard-include-guard-label obj) "\n"))

(defun gwizard-include-guard-finish (obj)
  (concat "#endif /*" (gwizard-include-guard-label obj) "*/\n"))

(defun gwizard-include-guard-label (obj)
  (concat "__" (renaming-hash-definify obj) "_H__"))

(defun gwizard-cplusplus-start ()
  "#ifdef __cplusplus\nextern \"C\" {\n#endif /*__cplusplus*/\n")

(defun gwizard-cplusplus-finish ()
  "#ifdef __cplusplus\n}\n#endif /*__cplusplus*/\n")

(defun gwizard-file-comment (str)
  (concat "/*\n"
          " * " str "\n"
          " * generated by gwizard " gwizard-version " on " (current-time-string) "\n" 
	  " */\n"))

(defun gwizard-struct-typedefs (obj)
  (concat "typedef struct _" obj "       " obj ";\n"
	  "typedef struct _" obj "Class  " obj "Class;\n"))

(defun gwizard-obj-struct (obj parent)
  (concat "struct _" obj " {\n"
	  "\t" parent " parent;\n"
	  "\n\t/* TODO: add member vars */\n"
	  "};\n"))

(defun gwizard-class-struct (obj parent)
  (concat "struct _" obj "Class {\n"
	  "\t" parent "Class parent_class;\n"
	  "\n\t/* TODO: add class vars, signals here */\n"
	  "};\n"))

(defun gwizard-file-name (obj)
  (if (eq 0 (string-match "Gtk" obj))
      (downcase obj)
    (renaming-dashify obj)))

(defun gwizard-dot-h (obj)
  (concat (gwizard-file-name obj) ".h"))

(defun gwizard-dot-c (obj)
  (concat (gwizard-file-name obj) ".c"))

(defun gwizard-hash-include (obj)
  (concat "#include \"" (gwizard-dot-h obj) "\"\n"))


(defun gwizard-static-parent-class-inst (parent)
  (concat "static " parent "Class *parent_class = NULL;\n"))


(defun gwizard-obj-init-func (obj)
  (concat
   "static void\n" 
   (renaming-underscorify obj) "_init (" obj " *"  
   (renaming-underscorify obj)")\n"	  
   "{\n\t/* TODO: object initialization */\n}\n"))
  

(defun gwizard-file-readable-in-path-p (name pathlst)
  "does file with <name> exist in the paths in <pathlist>?"
  (when pathlst
    (or (file-readable-p (concat (car pathlst) name))
	(gwizard-file-readable-in-path-p name (cdr pathlst)))))

(defun gwizard-basename (str)
  "get the basename: GtkWidget=>Widget, BonoboBla=>Bla, GnomeFoo=>Foo, etc."
  "when there's no recognized prefix, evaluates to str"
  (let ((prefix (gwizard-prefix str)))
    (if (> (length prefix) 0)
	(substring str (length prefix))
      str)))

(defun gwizard-prefix (str)
  "get the prefix: GtkWidget=>Gtk, BonoboBla=>Bonobo, GnomeFoo=>Gnome, etc."
  "when there's no recognized prefix, evaluates to the empty string"
  (let ((prefix (gwizard-prefix-r str gwizard-namespace-names)))
    (if prefix prefix "")))

(defun gwizard-prefix-r (str nspace)
  "check if str start with any of the prefixes in nspace"
  "if so, return this prefix. Otherwise, return nil"     
  (when (and str nspace)
    (if (eq 0 (string-match (car nspace) str))
	(car nspace)
      (gwizard-prefix-r str (cdr nspace)))))


(defun gwizard-obj-is (obj)
  "eg. GTK_IS_BUTTON"
  (let ((prefix (gwizard-prefix obj))
	(basename (gwizard-basename obj)))
    (concat
     (if (> (length prefix) 0)
	 (concat (upcase prefix) "_"))  
     "IS_" 
     (renaming-hash-definify basename))))

(defun gwizard-obj-type (obj)
  "eg. GTK_TYPE_BUTTON"
  (let ((prefix (gwizard-prefix obj))
	(basename (gwizard-basename obj)))
    (concat
     (if (> (length prefix) 0)
	 (concat (upcase prefix) "_"))  
     "TYPE_" 
     (renaming-hash-definify basename))))


(defun gwizard-obj-type-macro (obj type)
  "e.g. #define GTK_TYPE_BUTTON ... "
  (concat 
   "#define " type "                (" (renaming-underscorify obj) "_get_type ())\n"))

(defun gwizard-clear-buffer ()
  (delete-region (point-min) (point-max)))

(defun gwizard-current-year ()
  (let ((date (current-time-string)))
    (substring date (- (length date) 4))))

(defun gwizard-gpl (year c-holder)
  "the GNU General Public License"
  (concat 
   "/*\n"
   "** Copyright (C) " year " "  c-holder "\n"
   "**\n"
   "** This program is free software; you can redistribute it and/or modify\n"
   "** it under the terms of the GNU General Public License as published by\n"
   "** the Free Software Foundation; either version 2 of the License, or\n"
   "** (at your option) any later version.\n"
   "**\n"
   "** This program is distributed in the hope that it will be useful,\n"
   "** but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   "** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   "** GNU General Public License for more details.\n"
   "**\n"
   "** You should have received a copy of the GNU General Public License\n"
   "** along with this program; if not, write to the Free Software\n"
   "** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.\n"
   "**\n"
   "*/\n"))

(defun gwizard-lgpl (c-holder)
  "the GNU Lesser General Public License"
  (concat 
   "/*\n"
   "** Copyright (C) " year " " c-holder "\n"
   "**\n"
   "** This library is free software; you can redistribute it and/or modify\n"
   "** it under the terms of the GNU Library General Public License as published\n"
   "** by the Free Software Foundation; either version 2 of the License, or\n"
   "** (at your option) any later version.\n"
   "**\n"
   "** This program is distributed in the hope that it will be useful,\n"
   "** but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   "** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   "** GNU General Public License for more details.\n"
   "**\n"
   "** You should have received a copy of the GNU Library General Public License\n"
   "** along with this program; if not, write to the Free Software\n"
   "** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.\n"
   "**\n"
   "*/\n"))


;; 
;; gtk-specific
;;
(defun gwizard-gtk-hash-include (parent)
  "if the parent exists in the gtk include path, return #include <gtk/....h>,"
  "otherwise, return #include \"....h\""
  (let 
      ((include-name (concat (downcase parent) ".h")))
    (if (gwizard-file-readable-in-path-p include-name gwizard-gtk-include-path)
	(concat "#include <gtk/" include-name ">\n")
      (concat "#include \"" include-name "\"\n"))))


(defun gwizard-gtk-standard-functions-decl (obj &optional is-virtual)
  "generate standard gtk+ function declarations"
  (concat
   "GtkType        "
   (renaming-underscorify obj) "_get_type   (void);\n"
   (when (not is-virtual)
     (concat 
      "GtkWidget*     " 
      (renaming-underscorify obj) "_new        (void);\n"))))


(defun gwizard-set-arg-func (obj)
  (let ((var-name (downcase (gwizard-basename obj))))
    (concat
     "static void\n"
     (renaming-underscorify obj) "_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)\n"
     "{\n\t" obj "*" var-name ";\n\n"
     "\t" var-name " = " (renaming-hash-definify obj) "(object);\n\n"
     "\tswitch (arg_id) {\n"
     "\t/* TODO: fill in cases for args */\n"
     "\n\tdefault:\n"
     "\t\tg_warning (\"arg not found\");\n"
     "\t};\n}\n")))
   
(defun gwizard-get-arg-func (obj)
  (let ((var-name (downcase (gwizard-basename obj))))
    (concat
     "static void\n"
     (renaming-underscorify obj) "_get_arg (GtkObject *object, GtkArg *arg, guint arg_id)\n"
     "{\n\t" obj "*" var-name ";\n\n"
     "\t" var-name " = " (renaming-hash-definify obj) "(object);\n\n"
     "\tswitch (arg_id) {\n"
     "\t/* TODO: fill in cases for args */\n"
     "\n\tdefault:\n"
     "\t\tg_warning (\"arg not found\");\n"
     "\t};\n}\n")))

(defun gwizard-gtk-new-widget-func (obj)
  (let ((underscore-name (renaming-underscorify obj)))
    (concat
     "GtkWidget*\n" underscore-name "_new (void)\n"
     "{\n\treturn GTK_WIDGET (gtk_type_new (" underscore-name "_get_type ());\n"
     "}\n")))

(defun gwizard-static-func-decls (obj)
  (let ((underscore-name (renaming-underscorify obj))
	(var-name (downcase (gwizard-basename obj))))
    (concat
     "/*\n * NOTE: these may not (all) be needed. Please remove\n"
     " * unneeded declarations, as well as their implementation.\n */\n" 
     "static void " underscore-name "_class_init  (" obj "Class *klass);\n"
     "static void " underscore-name "_init        (" obj " *" var-name ");\n")))

(defun gwizard-static-get-set-arg-func-decls (obj)
  (let ((underscore-name (renaming-underscorify obj))
	(var-name (downcase (gwizard-basename obj))))
    (concat
     "static void " underscore-name "_set_arg     (" 
                   "GtkObject *obj, GtkArg *arg, guint arg_id);\n"
     "static void " underscore-name "_get_arg     ("
                  "GtkObject *obj, GtkArg *arg, guint arg_id);\n")))


(defun gwizard-obj-macro (obj type)
  "e.g. #define GTK_BUTTON(obj) ..."
  (concat
   "#define " (renaming-hash-definify obj) "(obj)"
   "                (GTK_CHECK_CAST ((obj), " type ", " obj "))\n"))

(defun gwizard-obj-class-macro (obj type)
  "e.g. #define GTK_BUTTON_CLASS(klass) ..."
  (concat 
   "#define " (renaming-hash-definify obj) "_CLASS(klass)"
   "        (GTK_CHECK_CLASS_CAST((klass), " type ", " obj "Class))\n"))

(defun gwizard-is-obj-macro (obj type)
  "e.g. #define GTK_IS_BUTTON(obj) ..."
  (concat 
   "#define " (gwizard-obj-is obj) "(obj)"
   "             (GTK_CHECK_TYPE ((obj), " type "))\n")) 

(defun gwizard-is-obj-class-macro (obj type)
  "e.g. #define GTK_IS_BUTTON_CLASS ..."
  (concat
   "#define " (gwizard-obj-is obj) "_CLASS(klass)"
   "     (GTK_CHECK_CLASS_TYPE ((klass), " type "))\n"))

(defun gwizard-macros (obj type)
  "generate funky gtk macros"
  (concat
   (gwizard-obj-type-macro obj type)
   (gwizard-obj-macro obj type)
   (gwizard-obj-class-macro obj type)
   (gwizard-is-obj-macro obj type )
   (gwizard-is-obj-class-macro obj type)))


(defun gwizard-gtk-get-type-function (obj parent)
  (let* ((underscore-name (renaming-underscorify obj))
	(var-name (downcase (gwizard-basename obj)))
	(var-name-type (concat var-name "_type"))
	(var-name-info (concat var-name "_info")))
    (concat "GtkType\n"
	    underscore-name "_get_type (void)\n"
	    "{\n\tstatic GtkType " var-name-type " = 0;\n\n"
	    "\tif (!" var-name-type ") {\n"
	    "\t\tGtkTypeInfo " var-name-info " = {\n"
	    "\t\t\t\"" obj  "\",\n"
	    "\t\t\tsizeof (" obj "),\n"
	    "\t\t\tsizeof (" obj "Class),\n"
	    "\t\t\t(GtkClassInitFunc) " underscore-name "_class_init,\n"
	    "\t\t\t(GtkObjectInitFunc) " underscore-name "_init,\n"
	    "\t\t\t/* reserved_1 */ NULL,\n"
	    "\t\t\t/* reserved_2 */ NULL,\n"
	    "\t\t\t(GtkClassInitFunc) NULL\n"
	    "\t\t};\n\n"
	    "\t\t" var-name-type " = gtk_type_unique (" (gwizard-obj-type parent) 
                                    ", &" var-name-info ");\n"
	    "\t}\n;\treturn " var-name-type ";\n"
	    "}\n")))

(defun gwizard-class-init-func (obj parent)
  (concat
   "static void\n" 
   (renaming-underscorify obj) "_class_init (" obj "Class *klass)\n"
   "{\n"
   "\tGtkObjectClass *object_class;\n\n\tobject_class = (GtkObjectClass*) klass;\n\n"
   "\tparent_class = gtk_type_class (" (gwizard-obj-type parent) ");\n" 
   "\n\t/* TODO: add arg types, if any */\n\n"
   "\tobject_class->set_arg = " (renaming-underscorify obj) "_set_arg;\n"
   "\tobject_class->get_arg = " (renaming-underscorify obj) "_get_arg;\n\n"
   "\t/* TODO: more class initialization */\n}\n"))
     

;;
;; bonobo1 specific
;; 
(defun gwizard-bonobo-hash-include (parent)
  "if the parent exists in the bonobo include path, return #include <bonobo/....h>,"
  "otherwise, return #include \"....h\""
  (let 
      ((include-name 
	; special case for BonoboXObject....
	(if (string= parent "BonoboXObject")
	    "bonobo-xobject.h"
	  (concat (renaming-dashify parent) ".h"))))
    (if (gwizard-file-readable-in-path-p include-name gwizard-bonobo-include-path)
	(concat "#include <bonobo/" include-name ">\n")
      (concat "#include \"" include-name "\"\n"))))


(defun gwizard-bonobo-class-struct (obj parent poa)
  (concat "struct _" obj "Class {\n"
	  "\t" parent "Class parent_class;\n"
	  "\t" poa "__epv epv;\n"
	  "\n\t/* TODO: add other class vars, signals here */\n"
	  "};\n"))

(defun gwizard-repeat-str (str n)
  (when (> n 0)
    (concat str (gwizard-repeat-str str (- n 1)))))

(defun gwizard-bonobo-standard-functions-decl (obj)
  "generate standard gtk+ function declarations"
  (let* ((spc2 (gwizard-repeat-str " " 8)) 
	 (spc1 (gwizard-repeat-str " " 
				   (+ (length spc2) 1 (- (length obj) 
							 (length "GtkType"))))))
    (concat
     "GtkType" spc1 (renaming-underscorify obj) "_get_type   (void);\n"
     obj "*" spc2 (renaming-underscorify obj) "_new        (void);\n")))

(defun gwizard-bonobo-class-init-func (obj parent poa)
  (concat
   "static void\n" 
   (renaming-underscorify obj) "_class_init (" obj "Class *klass)\n"
   "{\n"
   "\t" poa "__epv *epv = &klass->epv;\n"
   "\tparent_class = gtk_type_class (" (gwizard-bonobo-obj-type parent) ");\n" 
    "\n\t/* TODO: more class initialization */\n\n"
    "\t/*\n\t * TODO: place ptrs to member functions in epv\n"
    "\t * ie. epv->my_func = my_obj_my_func;\n\t */\n}\n"
    ))


(defun gwizard-bonobo-get-type-function (obj parent poa)
  (let* ((underscore-name (renaming-underscorify obj))
	(var-name (downcase (gwizard-basename obj)))
	(var-name-type (concat var-name "_type"))
	(var-name-info (concat var-name "_info")))
    (concat "GtkType\n"
	    underscore-name "_get_type (void)\n"
	    "{\n\tstatic GtkType " var-name-type " = 0;\n\n"
	    "\tif (!" var-name-type ") {\n"
	    "\t\tstatic const GtkTypeInfo " var-name-info " = {\n"
	    "\t\t\t\"" obj  "\",\n"
	    "\t\t\tsizeof (" obj "),\n"
	    "\t\t\tsizeof (" obj "Class),\n"
	    "\t\t\t(GtkClassInitFunc) " underscore-name "_class_init,\n"
	    "\t\t\t(GtkObjectInitFunc) " underscore-name "_init,\n"
	    "\t\t\t/* reserved_1 */ NULL,\n"
	    "\t\t\t/* reserved_2 */ NULL,\n"
	    "\t\t\t(GtkClassInitFunc) NULL\n"
	    "\t\t};\n\n"
	    "\t\t" var-name-type " = bonobo_x_type_unique (\n"
	                    "\t\t\t" (gwizard-bonobo-obj-type parent) ",\n"
			    "\t\t\t" poa "__init,\n"
			    "\t\t\tNULL,\n"
			    "\t\t\tGTK_STRUCT_OFFSET (" obj "Class, epv),\n"
                            "\t\t\t&" var-name-info ");\n"
	    "\t}\n;\treturn " var-name-type ";\n"
	    "}\n")))


(defun gwizard-bonobo-new-func (obj)
  (let ((underscore-name (renaming-underscorify obj)))
    (concat
     obj "*\n" underscore-name "_new (void)\n"
     "{\n\treturn gtk_type_new (" underscore-name "_get_type ());\n"
     "}\n")))


(defun gwizard-bonobo-poa-from-interface (interface)
  "e.g. Bonobo::Sample::Foobar => POA_Bonobo_Sample_Foobar"
  (concat "POA_" (gwizard-bonobo-underscorify-interface interface)))
  
(defun gwizard-bonobo-underscorify-interface (interface)
  "e.g. Bonobo::Sample::Foobar => Bonobo_Sample_Foobar"
  (let ((lst (split-string interface "::")) 
	(str ""))
    (while lst
      (setq str (concat str (car lst) (when (cdr lst) "_")))
      (setq lst (cdr lst)))
    str))

(defun gwizard-bonobo-name-from-interface (interface &optional long-name)
  "e.g. Bonobo::Sample::Foobar => Foobar"
  (if (string= interface "Bonobo::Unknown")
      "BonoboXObject"
    (let ((lst (split-string interface "::")) (name ""))
      (if long-name
	  (while lst
	    (setq name (concat name (car lst)))
	    (setq lst (cdr lst)))
	(setq name (car (last (split-string interface "::")))))
      name)))


(defun gwizard-bonobo-static-parent-class-inst ()
  (concat "static GtkObjectClass *parent_class = NULL;\n"))

(defun gwizard-bonobo-obj-type (obj)
  "eg. BONOBO_STREAM_TYPE (different from GTK_TYPE_BUTTON)"
  (concat
   (renaming-hash-definify obj) "_TYPE"))

;; 
;; bonobo-factory
;;
(defun gwizard-factory-func (obj)
  (concat
   "static BonoboObject*\n"
   (downcase (renaming-underscorify obj)) 
   "_factory (BonoboGenericFactory* factory, void* data)\n"
   "{\n\t/*\n\t * TODO: declare/initialize interfaces, ie.:\n"
   "\t * MyInterface* my_interface = my_interface_new ();\n\t */\n\n"
   "\t/* TODO: if you have >1 interfaces, choose a primary one and do:\n"
   "\t * bonobo_object_add_interface\n"
   "\t * \t(BONOBO_OBJECT(my_interface), BONOBO_OBJECT(another_interface));\n\t */\n\n"
   "\t/* TODO: return the object, ie.:\n"
   "\t * return BONOBO_OBJECT (my_interface);\n\t */\n}\n"))


(defun gwizard-oaf-macro (obj component)
  (concat
   "BONOBO_OAF_FACTORY(\"OAFIID:" component "_Factory\",\n"
   "\t\"" (gwizard-file-name obj) "\", \"0.0.0\",\n"
   "\t" (downcase (renaming-underscorify obj)) "_factory,\n"
   "\tNULL)\n"))

(defun gwizard-oaf-file (obj component)
  (concat
   "<oaf_info>\n"
   "\t<oaf_server\n"
   "\t\tiid=\"OAFIID:" component "_Factory\"\n\t\ttype=\"exe\"\n\t\tlocation=\""  
   (gwizard-file-name obj) "\">\n\n"
   
   "\t\t<oaf_attribute name=\"repo_ids\" type=\"stringv\">\n"
   "\t\t\t<item value=\"IDL:GNOME/GenericFactory:1.0\"/>\n"
   "\t\t</oaf_attribute>\n\n"
   
   "\t\t<oaf_attribute name=\"name\" type=\"string\" value=\"" obj " factory\"/>\n"
   "\t\t<oaf_attribute name=\"description\" type=\"string\" value=\""
   "A factory for " obj " components\"/>\n"
   "\t</oaf_server>\n\n"
   
   "\t<oaf_server\n"
   "\t\tiid=\"OAFIID:" component "\"\n\t\ttype=\"factory\"\n\t\tlocation="
   "\"OAFIID:" component "_Factory\">\n\n"
   
    "\t\t<oaf_attribute name=\"repo_ids\" type=\"stringv\">\n"
   "\t\t\t<!-- TODO: fill in the interfaces the component implements -->\n"
   "\t\t\t<!-- ie. <item value=\"IDL:Bonobo/MyInterface:1.0\"/> -->\n"
   "\t\t</oaf_attribute>\n\n"
   
   "\t\t<oaf_attribute name=\"name\" type=\"string\" value=\"" obj " component\"/>\n"
   "\t\t<oaf_attribute name=\"description\" type=\"string\" value=\""
   obj " Component\"/>\n"
   "\t</oaf_server>\n\n"
   "</oaf_info>"))
  

;;
;; renaming.el
;;

(defun renaming-camel-casify (str)
  (renaming-camel-casify-list (split-string str "")))

(defun renaming-camel-casify-with-starting-capital (str)
  (renaming-camel-casify-list (split-string str "") t))
 
(defun renaming-underscorify (str)
  (renaming-partition-list (split-string str "") "_" nil t))

(defun renaming-dashify (str)
  (renaming-partition-list (split-string str "") "-"  nil t))

(defun renaming-hash-definify (str)
  (upcase (renaming-underscorify str)))


(defun renaming-partition-list (lst symb &optional prev-is-upper is-first)
  (when lst
    (concat (when (and 
		   (not is-first) 
		   (renaming-is-upper (car lst)) 
		   (or (not prev-is-upper)
		       (and (cdr lst) (not (renaming-is-upper (cadr lst))))))
	      symb)
	    (downcase (car lst))
	    (renaming-partition-list (cdr lst) symb (renaming-is-upper (car lst))))))

(defun renaming-camel-casify-list (lst &optional starting-capital)
  (when lst
    (if starting-capital
	(concat (upcase (car lst)) (renaming-camel-casify-list (cdr lst)))
      (if (and (string= (car lst) "_") (cdr lst))
	  (concat (upcase (cadr lst)) (renaming-camel-casify-list (cddr lst)))
	(concat (car lst) (renaming-camel-casify-list (cdr lst)))))))

(defun renaming-is-upper (s)
  (string= s (upcase s)))

(defun renaming-is-lower (s)
  (string= s (downcase s)))

;;
;; test
;;
;;(renaming-hash-definify "GtkWidget")
;;(renaming-dashify "GtkWidget")
;;(renaming-camel-casify  "my_funky_widget")
;;(renaming-camel-casify-with-starting-capital  "my_funky_widget")
;;(renaming-underscorify "GtkWidget")


(defconst gwizard-version "0.0.0")
(provide 'gwizard)
;;
;; end of gwizard.el
;;