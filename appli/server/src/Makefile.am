
SUBDIRS = Windows

bin_PROGRAMS = ffss-server ffsswho

#		arch_dep_win.c
ffss_server_SOURCES = \
		arch_dep_unix.c \
		conf.c  		\
		index.c 		\
		parser.y  		\
		lexer.l  		\
		server.c  		\
		server.h  		\
		transfer.c

ffsswho_SOURCES = ffsswho.c server.h
