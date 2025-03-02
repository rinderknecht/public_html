// This program converts from negabinary to 2-complement binary.

// UNFINISHED

// Carries and borrows:
//
// (-1)*(-2)^{2p}   = -2^{2p}
//                  = 2^{2p} - 2*2^{2p}
//                  = 2^{2p} - 2^{2p+1}
//                  = (-2)^{2p+1} + 2^{2p}
//                  = (1)*(-2)^{2p+1} + (1)*2^{2p}
// (-1)*(-2)^{2p+1} = (0)*(-2)^{2p+2} + (1)*2^{2p+1}
//
// (1)*(-2)^{2p}    = (0)*2^{2p+1} + (1)*2^{2p}
// (1)*(-2)^{2p+1}  = -2^{2p+1}
//                  = -2*2^{2p+1} + 2^{2p+1}
//                  = (-1)*(2)^{2p+2} + 2^{2p+1}
//                  = (-1)*(-2)^{2p+2} + (1)*2^{2p+1}

// ASCII: '0'..'9' -> 48..57

#include<stdio.h>
#include<stdlib.h>
#include<string.h>

// Computation
//
int* fromNeg2to2 (int* negbin) {
  int report = 0;
  int bit = 0;
  for (int negbin_idx = negbin[0]; 0 < negbin_idx; negbin_idx--) {
    bit = negbin[negbin_idx] + report;
    report = (bit == -1 || bit == -2)? 1 : 0;
  }
  int bin_len = bit? negbin[0] : negbin[0]-1;
  int* bin = (int*) malloc((1+bin_len) * sizeof(int));
  bin[0] = bin_len;
  int negbin_idx = negbin[0];
  report = 0;
  for (int bin_idx = bin_len; 0 < bin_idx; bin_idx--) {
    bin[bin_idx] = negbin[negbin_idx] - report;
    if (bin[bin_idx] == -1 || bin[bin_idx] == -2)
      { report = 1; bin[bin_idx] += 3; }
    else report = 0;
    negbin_idx--;
  }
  return bin;
}

// Interface (out)
//
void print (int* a) {
  for (int index = 1; index <= a[0]; index++)
    printf("%d",a[index]);
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
    int lnz_idx = 0; // leftmost nonzero bit index
    while (lnz_idx < str_len && argv[1][lnz_idx] == '0') lnz_idx++;
    if (lnz_idx == str_len)
      printf("0\n");
    else {
      int negbin_len = str_len - lnz_idx;
      int* negbin = (int*) malloc((1+negbin_len) * sizeof(int));
      negbin[0] = negbin_len;
      int negbin_idx = negbin_len;
      for (int str_idx = str_len; lnz_idx < str_idx; str_idx--) {
        char bit = argv[1][str_idx-1];
        if (bit == '0' || bit == '1')
          negbin[negbin_idx] = ((int)bit) - 48;
        else {
          free(negbin);
          fatal_error("Argument must be a negabinary number.",-1);
        }
        negbin_idx--;
      }
      print(negbin);
      //    int* bin = fromNeg2to2(negbin);
      //    print(bin);
      free(negbin);
      //    free(bin);
    }
    return 0;
  }
  fatal_error("One argument only is required.",-1);
}
