## Process this file with automake to produce Makefile.in

INCLUDES = `gtk-config --cflags`

bin_PROGRAMS = ffss-client

ffss_client_SOURCES = \
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

ffss_client_LDADD =  \
	$(GTK_LIBS)\
	 -lgthread

