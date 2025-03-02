// This programme converts a number from its decimal representation
// into its negabinary representation.

// In C, if a => 0 and b < 0, then a/b <= 0 and a%b >= 0.
//       if a <= 0 and b < 0, then a/b >= 0 and a%b <= 0.

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int* from10toNeg2 (int dec) {
  int index = 0;
  int input = dec;
  int rem;  // Safe because followed by a `do' loop where it is
            // assigned a value.
  do {
    rem = dec % (-2);
    dec /= -2;
    if (rem == -1) dec++;
    index++;
  } while (dec != 0);
  int* neg2 = (int*) malloc((index+1) * sizeof(int));
  neg2[0] = index;
  do {
    rem = input % (-2);
    input /= -2;
    if (rem == -1) {rem += 2; input++;}
    neg2[index] = rem;
    index--;
  } while (input != 0);
  return neg2;
}

void print (int* num) {
  for (int index = 1; index <= num[0]; index++)
    if (num[index] == -1)
      printf("I");
    else printf("%d",num[index]);
  printf("\n");
}

int main (int argc, char** argv) {
  if (argc == 2) {
    int num = 0;
    int start = (argv[1][0] == '-')? 1 : 0;
    for (int i = start; i < strlen(argv[1]); i++)
      if ('0' <= argv[1][i] && argv[1][i] <= '9')
        num = num * 10 + (int)(argv[1][i]) - 48;
      else {
        fprintf(stderr,"Only decimal digits allowed.\n");
        return -2;
      }
    if (start) num *= -1;
    int* neg2 = from10toNeg2(num);
    print(neg2);
    return 0;
  }
  fprintf(stderr,"Provide exactly one decimal number.\n");
  return -1;
}
