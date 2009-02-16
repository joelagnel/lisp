

# «.eechannel-python»	(to "eechannel-python")
# «.channel.py»		(to "channel.py")




#####
#
# talking to python through eechannel
# 2005jan01
#
#####

# «eechannel-python»  (to ".eechannel-python")
# http://people.debian.org/~kov/stuff/edrx.tar.gz
# (find-pylibnode "signal")
# (find-pyrefnode "Comments")
# (find-pyrefnode "String literals")

 (eebg-channel-xterm "pysh")
 (eechannel "pysh")

python

import signal, time, os

channel = 'python'
pidfile = os.getenv('EEVTMPDIR') + '/eeg.' + channel + '.pid'
strfile = os.getenv('EEVTMPDIR') + '/eeg.' + channel + '.str'
fhandle = open (pidfile, 'w')
fhandle.write (str(os.getpid()) + '\n')
fhandle.close ()

def signal_handler (num, frame):
  execfile(strfile, globals())

signal.signal (signal.SIGUSR1, signal_handler)

a = 0
while (1):
    print a
    time.sleep (1)
    a = a + 1

 (eechannel "python")
print '(hi)'
print '(hi again)'





#####
#
# channel.py
# 2005jan03
#
#####

# «channel.py»  (to ".channel.py")
# (find-eevrc "channel.py")

 (eebg-channel-xterm "pysh")
 (eechannel "pysh")

python

import os
execfile(os.getenv('EEVRCDIR')+'/channel.py', globals());
a = 0
while (1):
    print a
    time.sleep (1)
    a = a + 1

 (eechannel "python")
print '(hi)'
print '(hi again)'





# (find-pytutnode "Defining Functions")

#
cat > /tmp/foo.py <<'---'
def square(a):
  return a*a
def cube(a):
  return a*square(a)
print cube(5)
---

python /tmp/foo.py

#
pdb /tmp/foo.py
# (pdb "pdb /tmp/foo.py")


# (find-efunction 'ee-gdb-start)
# (find-efunction 'eeb-gdb-start)

(defun ee-pdb-start (dir &optional fname)
  (pdb (format "pdb %s%s" dir (or fname "")))
  (eegud-keys-mode))

(ee-pdb-start "/tmp/" "foo.py")


# http://www.freedom-to-tinker.com/tinyp2p.html





#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
