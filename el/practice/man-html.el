(setq man-local-alist '(
       ("arch" "/doc/tla/arch.html")
       ("commonlisp" "/l/backup/www.lisp.org/HyperSpec/FrontMatter/index-text.html")
       ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki/SiteMap")
       ("elk" "/usr/local/share/elk/doc/kernel/kernel.html")
       ("execline" "/var/djb/package/admin/execline/doc/index.html")
       ("gnus" "/usr/local/share/doc/emacs/manual/gnus_toc.html")
       ("gprolog" "/l/gprolog-1.2.16/doc/Html/index.html")
       ("guile" "file:///usr/local/backup/www.gnu.org/software/guile/docs/index.html")
       ("handbook" "/usr/share/doc/en_US.ISO_8859-1/books/handbook/index.html")
       ("lua" "/doc/lua/lua5/doc/contents.html")
       ("nistp224" "/l/backup/cr.yp.to/nistp224.html")
       ("pl" "/usr/local/share/prolog/pl-5.0.1/Manual/Contents.html")
       ("scheme" "/l/backup/www.cs.utexas.edu/users/wilson/schintro/schintro_toc.html")
       ("siod" "/l/lib/siod/siod.html")
       ("t-scheme" "/usr/local/share/doc/teach-scheme/t-y-scheme-Z-H-1.html")
       ("tinydns" "~/archiv/tinydns-data.html")
       ("w3m" "/l/share/doc/w3m/MANUAL.html")
       ("xref" "/l/doc/xref.html")
    ))
    (defun man-local (what)
      "browse manual page"
      (interactive
       (let ((page (completing-read (format "What? (default %s): "
                                            (thing-at-point 'word))
                                    man-local-alist nil t)))
         (list page)))
      (browse-url (cadr (assoc what man-local-alist))) ; w3m-browse-url w3m-goto-url-new-session
    )