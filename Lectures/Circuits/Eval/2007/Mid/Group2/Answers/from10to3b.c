// This program converts a number from its decimal representation
// into its balanced ternary representation.

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int* from10to3b (int dec) {
  int index = 0;
  int input = dec;
  int rem;  // Safe because followed by a `do' loop where it is
            // assigned a value.
  do {
    rem = dec % 3;
    dec /= 3;
    if (rem == 2) {rem = -1; dec++;}
    else if (rem == -2) {rem = 1; dec--;}
    index++;
  } while (dec != 0);
  int* bter = (int*) malloc((index+1) * sizeof(int));
  bter[0] = index;
  do {
    rem = input % 3;
    input /= 3;
    if (rem == 2) {rem = -1; input++;}
    else if (rem == -2) {rem = 1; input--;}
    bter[index] = rem;
    index--;
  } while (input != 0);
  return bter;
}

void print (int* num) {
  for (int index = 1; index <= num[0]; index++)
    if (num[index] == -1)
      printf("I");
    else printf("%d",num[index]);
  printf("\n");
}

int main(int argc, char** argv) {
  if (argc == 2) {
    int num = 0;
    for (int i = 0; i < strlen(argv[1]); i++)
      if ('0' <= argv[1][i] && argv[1][i] <= '9')
        num = num * 10 + (int)(argv[1][i]) - 48;
      else {
        fprintf(stderr,"Only decimal digits allowed.\n");
        return -2;
      }
    int* bter = from10to3b(num);
    print(bter);
    free(bter);
    return 0;
  }
  fprintf(stderr,"Provide exactly one decimal number.\n");
  return -1;
}
