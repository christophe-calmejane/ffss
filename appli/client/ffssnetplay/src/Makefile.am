## Process this file with automake to produce Makefile.in

INCLUDES = `gtk-config --cflags`

bin_PROGRAMS = ffssnetplay

ffssnetplay_SOURCES = \
	callbacks.c\
	callbacks.h\
	interface.c\
	interface.h\
	main.c\
	client.c \
	client.h \
	support.c\
	support.h

ffssnetplay_LDADD =  \
	$(GTK_LIBS) \
	 -lgthread
