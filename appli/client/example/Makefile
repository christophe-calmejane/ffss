CC=gcc
PROG=ffss-client

OBJS=client.o

LIBS=-lpthread -lz -lbz2 ../../../libffss-1.0.0/src/.libs/libffss.a ../../../skyutils/src/.libs/libskyutils.a -ldl
CFLAGS=-Wall -D_REENTRANT -I../../../libffss-1.0.0/src -I../../../skyutils/src -O0 -g -DDEBUG

all: $(PROG)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean :
	rm -f *.o
	rm -f $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS) $(LIBS)

