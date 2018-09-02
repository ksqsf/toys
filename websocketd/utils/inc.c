#include <stdio.h>

int main() {
	int x;
	while (scanf("%d", &x) == 1) {
		printf("%d\n", x*3);
		fflush(stdout);
	}
	return 0;
}
