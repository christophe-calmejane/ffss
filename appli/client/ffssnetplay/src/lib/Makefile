CC=gcc
CFLAGS=-pthread -I . -I ../../../../../skyutils/src -I ../../../../../libffss-1.0.0/src -O2 -g
LDFLAGS=-L . -lfnp -L ../../../../../libffss-1.0.0/src/.libs -lffss -L ../../../../../skyutils/src/.libs -lskyutils -lz -lbz2 -pthread -ldl

all: libfnp.a try

%.o : %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

libfnp.a: fnp.o
	ar rs libfnp.a fnp.o

try: libfnp.a main.o
	$(CC) main.o $(LDFLAGS) -o $@

clean:
	rm -rf *.o
	rm -rf libfnp.a
	rm -rf try
