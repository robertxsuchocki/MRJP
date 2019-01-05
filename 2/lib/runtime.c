#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
  printf("%d\n", x);
}

void printString(char* x) {
  printf("%s\n", x);
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
}

int readInt() {
  int x;
  scanf("%d\n", &x);
  return x;
}

char* readString() {
  char *line = NULL;
  size_t len = 0;
  ssize_t read;

  read = getline(&line, &len, stdin);

  line[read - 1] = '\0';

  return line;
}
