CFLAGS=-Wall `pkg-config fuse --cflags`
LDFLAGS=`pkg-config fuse --libs`

hello: hello.o
hello.o: hello.c
clean:
	rm -f hello hello.o
.PHONY: clean
