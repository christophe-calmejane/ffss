CC=gcc
PROG=ffss-cgi

LIBS=-lpthread -lz -lbz2 ../../../libffss-1.0.0/src/.libs/libffss.a -L../../../libffss-1.0.0/skyutils-1.16 -lskyutils
OBJS=client.o
CFLAGS=-Wall -D_REENTRANT -I../../../libffss-1.0.0/src -I../../../libffss-1.0.0/skyutils-1.16 -O0 -g -DDEBUG

all: $(PROG)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean :
	rm -f *.o
	rm -f $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS)

