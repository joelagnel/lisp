DLIBS =

CC = gcc

SWITCHES = -Wall -O6 -g -funroll-loops\
	-fstrength-reduce -ffast-math -malign-functions=4 \
	-malign-jumps=4 -malign-loops=4
INSTALLATION_DIRECTORY=../bin

all: .depend summarize bsplit

.c.o: 
	$(CC) $(SWITCHES) -c $*.c

clean:
	rm -f *.o .depend summarize bsplit

distclean:
	make clean
	rm -f *~ #*#

include .depend

.depend:
	-gcc -MM -MG *.c >> .depend

depend:
	rm -f .depend
	make .depend


