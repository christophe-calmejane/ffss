CC=gcc
PROG=ffss-cgi

OBJS=client.o

LIBS=../../../libffss-1.0.0/src/.libs/libffss.a ../../../skyutils/src/.libs/libskyutils.a -ldl -lpthread -lz -lbz2 -static
CFLAGS=-Wall -D_REENTRANT -I../../../libffss-1.0.0/src -I../../../skyutils/src -O2
#CFLAGS=-Wall -D_REENTRANT -I../../../libffss-1.0.0/src -I../../../skyutils/src -O0 -g -DDEBUG

all: $(PROG)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean :
	rm -f *.o
	rm -f $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS)

