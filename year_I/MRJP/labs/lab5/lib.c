#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>


void printInt(int n) {
  printf("%d\n", n);
}

void printString(char * c) {
  printf("%s\n", c);
}

void error() {
  printf("ERROR\n");
  exit(1);
}

int readInt() {
  int n;
  scanf("%d", &n);
  return n;
}

char * readString() {
  return NULL;
}
