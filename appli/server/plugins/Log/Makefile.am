pkgdatadir = $(datadir)/@PACKAGE@/plugins

SUBDIRS = Log

EXTRA_PROGRAMS =
noinst_PROGRAMS = log.so
pkgdata_DATA = log.so

log_so_SOURCES = log.c
log.so: log.c
	$(COMPILE) -shared -Wl,-soname,log.so -o log.so log.c

