## Process this file with automake to produce Makefile.in

INCLUDES = `gtk-config --cflags`

bin_PROGRAMS = ffssclient

ffssclient_SOURCES = \
	callbacks.c\
	callbacks.h\
	interface.c\
	interface.h\
	main.c\
	support.c\
	support.h\
	client.c\
	client.h\
	main.h

ffssclient_LDADD =  \
	$(GTK_LIBS)\
	 -lgthread

