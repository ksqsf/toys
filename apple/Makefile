.PHONY: all clean

all: ocr

clean:
	rm -f ocr

ocr: ocr.mm
	clang++ ocr.mm -o ocr -framework Vision -framework Cocoa -Wall -std=c++17
