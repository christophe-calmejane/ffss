pkgdatadir = $(datadir)/@PACKAGE@/plugins

EXTRA_PROGRAMS =
noinst_PROGRAMS = confconn.so
pkgdata_DATA = confconn.so

confconn_so_SOURCES = confconn.c
confconn.so: confconn.c
	$(COMPILE) -shared -Wl,-soname,confconn.so -o confconn.so confconn.c @ffss_libs@ @O_CRYPT_LIB@

