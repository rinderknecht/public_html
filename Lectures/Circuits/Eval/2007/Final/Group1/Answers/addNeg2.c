// Addition of negabinary numbers

// ASCII: '0'..'9' -> 48..57

// Library headers
//
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

// Error codes
//
#define ARG_LEN   1
#define NO_ARG    2
#define NO_BIN_1  3
#define NO_BIN_2  4
#define OVERFLOW  5
#define UNDERFLOW 6

// Interface (out)
//
int fatal_error (int err_code) {
  fflush(stdout);
  switch(err_code) {
  case ARG_LEN:   fprintf(stderr,"error: numbers must be of same length.\n"); break;
  case NO_ARG:    fprintf(stderr,"error: one operation and two numbers required.\n"); break;
  case NO_BIN_1:  fprintf(stderr,"error: first number is not binary.\n"); break;
  case NO_BIN_2:  fprintf(stderr,"error: second number is not binary.\n");break;
  case OVERFLOW:  fprintf(stderr,"error: overflow\n"); break;
  case UNDERFLOW: fprintf(stderr,"error: underflow\n"); break;
  }
  exit(-err_code);
}

void print (int* num) {
  for (int index = 1; index <= num[0]; index++)
    printf("%d",num[index]);
  printf("\n");
}


// Converts from negabinary to decimal
//
int fromNeg2To10(int* nbin) {
  int res = nbin[1];
  for(int index=2; index <= nbin[0]; index++)
    res = nbin[index] - res * 2;
  return res;
}

// Addition
//
// [-1]*(-2)^i = [ 1]*(-2)^{i+1} + [1]*(-2)^i
// [ 2]*(-2)^i = [-1]*(-2)^{i+1} + [0]*(-2)^i
// [ 3]*(-2)^i = [-1]*(-2)^{i+1} + [1]*(-2)^i
//
int* addNeg2 (int* a, int* b) {
  int carry = 0;
  for (int index = a[0]; 0 < index; index--) {
    int bit = a[index] + b[index] + carry;
    switch (bit) {
    case -1: carry =  1; break;
    case  2: carry = -1; break;
    case  3: carry = -1; break;
    default: carry =  0;
    }
  }
  int res_len = 0;
  switch (carry) {
  case -1: res_len = a[0] + 2; break;
  case  0: res_len = a[0];     break;
  case  1: res_len = a[0] + 1;
  }
  int* res = (int*) malloc ((1+res_len)*sizeof(int));
  res[0] = res_len;
  carry = 0;
  int res_idx = res_len;
  for (int index = a[0]; 0 < index; index--) {
    res[res_idx] = a[index] + b[index] + carry;
    switch (res[res_idx]) {
    case -1: carry =  1; res[res_idx] = 1; break;
    case  2: carry = -1; res[res_idx] = 0; break;
    case  3: carry = -1; res[res_idx] = 1; break;
    default: carry =  0;
    }
    res_idx--;
  }
  switch (carry) {
  case -1: res[1] = 1; res[2] = 1; break;
  case  1: res[1] = 1;
  }
  return res;
}

// Interface (in)
//
int main(int argc, char** argv) {
  if (argc == 3) {
    int len1 = strlen(argv[1]);
    int len2 = strlen(argv[2]);
    if (len1 == len2) {
      int* a = (int*) malloc((1+len1) * sizeof(int));
      int* b = (int*) malloc((1+len2) * sizeof(int));
      a[0] = len1;
      b[0] = len2;
      for (int index = len1; 0 < index; index--) {
        a[index] = (int) argv[1][index-1] - 48;
        if (a[index] < 0 || 1 < a[index]) {
          free(a); free(b);
          fatal_error(NO_BIN_1);
        }
        b[index] = (int) argv[2][index-1] - 48;
        if (b[index] < 0 || 1 < b[index]) {
          free(a); free(b);
          fatal_error(NO_BIN_2);
        }
      }
      int* res = NULL;
      printf("(%d)+(%d) = ",fromNeg2To10(a),fromNeg2To10(b));
      res = addNeg2(a,b);
      print(res);
      printf("%d\n",fromNeg2To10(res));
      free(res);
      free(a); free(b); 
      return 0;
    }
    fatal_error(ARG_LEN);
  }
  fatal_error(NO_ARG);
}
