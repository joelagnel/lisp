# This is the `rcfiles/.pythonrc.py' file of GNU eev.
# This file is in the Public Domain.
# Author and version: Eduardo Ochs, 2004nov03
# This is not installed by default.
# Thx to The-Fixer @ #python @ irc.free.net.
# (find-man "1 python" "~/.pythonrc.py")
# (find-man "1 python" "PYTHONSTARTUP")
# (find-es "python" "ee-for-python")

# A test:
# (ee-write "print 222" nil "" "" "$EEVTMPDIR/ee.py")
# (eev "PYTHONSTARTUP=$EEVRCDIR/.pythonrc.py python")

import os
def ee():
  execfile(os.getenv("EEVTMPDIR")+"/ee.py", globals())
