CC=gcc
CFLAGS=-pthread -I . -I ../../../../../skyutils/src -I ../../../../../libffss-1.0.0/src -O0 -g -DDEBUG
LDFLAGS=-L . -lfmp -L ../../../../../libffss-1.0.0/src/.libs -lffss -L ../../../../../skyutils/src/.libs -lskyutils -lz -lbz2 -pthread -ldl

all: libfmp.a try

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

libfmp.a: fmp.o
	ar rs libfmp.a fmp.o

try: libfmp.a main.o
	$(CC) main.o $(LDFLAGS) -o $@

clean:
	rm -rf *.o
	rm -rf libfmp.a
	rm -rf try
