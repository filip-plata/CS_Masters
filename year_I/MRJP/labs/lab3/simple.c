#include <stdio.h>
#include <stdlib.h>

void foo(int a) {
	if (a > 2)
		exit(1);
}


int main(int argc, char **argv) {
  int a;
  scanf("%d\n", &a);
  printf("%d\n", 1 + 20 * a);
  printf("%s\n", argv[1]);
  foo(a);
  return 0;
}
