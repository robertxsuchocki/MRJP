#include <stdio.h>
int fibRec(int n) {
	if (n < 2) {
		return n;
	} else {
		return fibRec(n - 1) + fibRec(n - 2);
	}
}
int fibIter(int n) {
	if (n == 0) {
		return 0;
	} else {
		int a = 0;
		int b = 1;
		while (n > 1) {
			int c = a;
			a = b;
			b = b + c;
			--n;
		}
		return b;
	}
}

int main(void) {
	int a = fibRec(8);
	int b = fibIter(8);
	printf("%d%d\n", a, b);
	return 0;
}

