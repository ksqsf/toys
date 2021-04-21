all: hello bp work

hello.o: hello.asm
	nasm -f elf64 hello.asm

bp.o: bp.asm
	nasm -f elf64 bp.asm

hello: hello.o
	ld hello.o -o hello

bp: bp.o
	ld bp.o -o bp -Ttext=4000b0

work: work.c
	gcc work.c -o work -g

clean:
	rm -f hello.o hello
	rm -f bp.o bp
	rm -f work

.PHONY: clean
