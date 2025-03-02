// This programme converts from ternary to balanced ternary.

/*   0   1   2    0   1   2 */
/* + 1   1   1    0   0   0 */
/* = 1  1I  10    0   1  1I */

// Carries:
//
// 2*3^i = 3^{i+1} + (-1)*3^{i}
// 3*3^i = 3^{i+1} +    0*3^{i}

// ASCII: 0..9 -> 48..57

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

// Computation
//
int* from3to3b (int* ternary) {
  int carry = 0;
  for (int ter_idx = ternary[0]; 0 < ter_idx; ter_idx--) {
    int trit = ternary[ter_idx] + carry;
    carry = (trit == 2 || trit == 3)? 1 : 0;
  }
  int bter_len = ternary[0] + carry;
  int* bter = (int*) malloc((1+bter_len) * sizeof(int));
  bter[0] = bter_len;
  int bter_idx = bter_len;
  carry = 0;
  for (int ter_idx = ternary[0]; 0 < ter_idx; ter_idx--) {
    bter[bter_idx] = ternary[ter_idx] + carry;
    if (bter[bter_idx] == 2 || bter[bter_idx] == 3)
      { carry = 1; bter[bter_idx] -= 3; }
    else carry = 0;
    bter_idx--;
  }
  if (carry) bter[bter_idx] = 1;
  return bter;
}

// Interface (out)
//
void print (int* a) {
  //  printf("[%d]",a[0]);
  for (int index = 1; index <= a[0]; index++)
    if (a[index] == -1)
      printf("I");
    else printf("%d",a[index]);
  printf("\n");
}

int fatal_error (char* message, int err_code) {
  fprintf(stderr,"error: %s\n",message);
  exit(err_code);
}

// Interface (in)
//
int main(int argc, char** argv) {
  if (argc == 2) {
    int str_len = strlen(argv[1]);
    int lnz_idx = 0; // leftmost nonzero trit index
    while (lnz_idx < str_len && argv[1][lnz_idx] == '0') lnz_idx++;
    if (lnz_idx == str_len)
      printf("0\n");
    else {
      int ter_len = str_len - lnz_idx;
      int* ter = (int*) malloc((1+ter_len) * sizeof(int));
      ter[0] = ter_len;
      int ter_idx = ter_len;
      for (int str_idx = str_len; lnz_idx < str_idx; str_idx--) {
        ter[ter_idx] = (int) argv[1][str_idx-1] - 48;
        if (ter[ter_idx] < 0 || ter[ter_idx] > 2) {
          free(ter);
          fatal_error("Argument must be ternary.",-2);
        }
        ter_idx--;
      }
      //print(ter);
      int* bter = from3to3b(ter);
      print(bter);
      free(bter);
      free(ter);
    }
    return 0;
  }
  fatal_error("One argument only is required.",-1);
}
