#
# Makefile for bonobo-sample-moody-component
#

CORBA_GENERATED = \
	Bonobo_Sample_Moody-common.c \
	Bonobo_Sample_Moody-skels.c \
	Bonobo_Sample_Moody.h

OBJECTS = \
	Bonobo_Sample_Moody-common.o \
	Bonobo_Sample_Moody-skels.o \
	good-mood.o \
	bad-mood.o \
	bonobo-sample-moody-component.o 


bonobo-sample-moody-component: ${CORBA_GENERATED} ${OBJECTS}
	gcc -o $@ ${OBJECTS} `orbit-config --libs server` `gnome-config --libs bonobo gnomeui`

.c.o:	
	gcc -c $< `orbit-config --cflags server` `gnome-config --cflags bonobo` 

$(CORBA_GENERATED):
	orbit-idl --nostubs Bonobo_Sample_Moody.idl `gnome-config --cflags idl`

clean:
	rm -f *~ ${OBJECTS} ${CORBA_GENERATED} bonobo-sample-moody-component
