all: ir1.bc ir2.bc main.bc

ir1.bc: ir1.ll
	llvm-as ir1.ll

ir2.bc: ir2.ll
	llvm-as ir2.ll

main.ll: main.c
	clang -emit-llvm -S main.c

main.bc: main.ll
	llvm-as main.ll
