#
# Makefile for bonobo-sample-moody-client
#

CORBA_GENERATED = \
	Bonobo_Sample_Moody-common.c \
	Bonobo_Sample_Moody-stubs.c \
	Bonobo_Sample_Moody.h

OBJECTS = \
	Bonobo_Sample_Moody-common.o \
	Bonobo_Sample_Moody-stubs.o \
	bonobo-sample-moody-client.o 


bonobo-sample-moody-client: ${CORBA_GENERATED} ${OBJECTS}
	gcc -o $@ ${OBJECTS} `orbit-config --libs client` `gnome-config --libs bonobo gnomeui`

.c.o:	
	gcc -c $< `orbit-config --cflags client` `gnome-config --cflags bonobo` 

$(CORBA_GENERATED):
	orbit-idl --noskels ../Bonobo_Sample_Moody.idl `gnome-config --cflags idl`

clean:
	rm -f *~ ${OBJECTS} ${CORBA_GENERATED} bonobo-sample-moody-client