#include <stdio.h>

int main() {
	char buf[4096];
	while (gets(buf)) {
		puts(buf);
		fflush(stdout);
	}
	return 0;
}
