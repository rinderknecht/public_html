// This programme converts from balanced ternary to ternary.

/*   0   1   2    0   1   2 */
/* + 1   1   1    0   0   0 */
/* = 1  1I  10    0   1  1I */

// Borrows:
//
// (-1)*3^i = (-1)*3^{i+1} + (2)*3^i
// (-2)*3^i = (-1)*3^{i+1} + (1)*3^i

// ASCII: '0'..'9' -> 48..57

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

// Computation
//
int* from3bto3 (int* bter) {
  int borrow = 0;
  int trit = 0;
  for (int bter_idx = bter[0]; 0 < bter_idx; bter_idx--) {
    trit = bter[bter_idx] - borrow;
    borrow = (trit == -1 || trit == -2)? 1 : 0;
  }
  int ter_len = trit? bter[0] : bter[0]-1;
  int* ter = (int*) malloc((1+ter_len) * sizeof(int));
  ter[0] = ter_len;
  int bter_idx = bter[0];
  borrow = 0;
  for (int ter_idx = ter_len; 0 < ter_idx; ter_idx--) {
    ter[ter_idx] = bter[bter_idx] - borrow;
    if (ter[ter_idx] == -1 || ter[ter_idx] == -2)
      { borrow = 1; ter[ter_idx] += 3; }
    else borrow = 0;
    bter_idx--;
  }
  return ter;
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
      int bter_len = str_len - lnz_idx;
      int* bter = (int*) malloc((1+bter_len) * sizeof(int));
      bter[0] = bter_len;
      int bter_idx = bter_len;
      for (int str_idx = str_len; lnz_idx < str_idx; str_idx--) {
        switch (argv[1][str_idx-1]) {
          case '0': bter[bter_idx] = 0; break;
          case '1': bter[bter_idx] = 1; break;
          case 'I': bter[bter_idx] = -1; break;
          default : 
            free(bter);
            fatal_error("Argument must be balanced ternary (0, 1 or I).",-2);
        }
        bter_idx--;
      }
      // The nonzero leftmost trit must not be I (i.e., -1)
      //
      if (bter[1] == -1) {
        free(bter);
        fatal_error("Argument must be positive.",-3);
      }
      // print(bter);
      int* ter = from3bto3(bter);
      print(ter);
      free(ter);
      free(bter);
    }
    return 0;
  }
  fatal_error("One argument only is required.",-1);
}
