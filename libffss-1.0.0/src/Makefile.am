
SUBDIRS = Windows

lib_LTLIBRARIES = libffss.la

include_HEADERS = ffss.h

libffss_la_SOURCES = \
	client.c  \
	common.c  \
	common.h  \
	ffss.h  \
	master.c  \
	message.c  \
	server.c  \
	transfer.c \
	transfer.h \
	utils.c \
	utils.h \
        driver.c

libffss_la_LDFLAGS = \
	-version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) \
	-release $(LT_RELEASE)
	@SKYUTILS_LIB@

CFLAGS = @CFLAGS@ @SKYUTILS_CFLAGS@
