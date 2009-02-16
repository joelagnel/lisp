### remove first 2 lines
#!/usr/bin/env bash

dir=icicles

if [ -d "$dir" ]; then
  typeset -i i=0
  while [ -d "${dir}_OLD$i" ]; do
    i="$i + 1"
  done
  mv "$dir" "${dir}_OLD$i"
fi


wget -nd -P $dir \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-cmd.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-face.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-fn.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mac.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mcmd.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-menu.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-mode.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-opt.el \
http://www.emacswiki.org/cgi-bin/wiki/download/icicles-var.el \
 \
http://www.emacswiki.org/cgi-bin/wiki/icomplete+.el/download/icomplete+.el \
