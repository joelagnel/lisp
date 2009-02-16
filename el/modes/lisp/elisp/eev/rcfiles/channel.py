# This is the `rcfiles/channel.py' file of GNU eev.
# Author and version: Eduardo Ochs, 2005jan07
# This file is in the Public Domain.

# Usage:
#   import os
#   execfile(os.getenv('EEVRCDIR')+'/channel.py', globals());
# This is very new (2005jan03) and I don't know python, so... :) Edrx
# See: (find-eevex "python.e" "channel.py")

import signal, time, os
channel = os.getenv('EECHANNEL') or 'python'
pidfile = os.getenv('EEVTMPDIR') + '/eeg.' + channel + '.pid'
strfile = os.getenv('EEVTMPDIR') + '/eeg.' + channel + '.str'
fhandle = open (pidfile, 'w')
fhandle.write (str(os.getpid()) + '\n')
fhandle.close ()
def signal_handler (num, frame):
  execfile(strfile, globals());
signal.signal (signal.SIGUSR1, signal_handler)
