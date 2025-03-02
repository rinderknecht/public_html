// Arithmetics on binary 2-complement numbers

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
#define WRONG_OP  7

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
  case WRONG_OP:  fprintf(stderr,"error: Only + and * are supported\n"); break;
  }
  exit(-err_code);
}

void print (int* num) {
  for (int index = 1; index <= num[0]; index++)
    printf("%d",num[index]);
  printf("\n");
}

// Binary addition
//
int* add2 (int* a, int* b) {
  int* sum = (int*) malloc ((1+a[0])*sizeof(int));
  sum[0] = a[0];
  int carry = 0;
  for(int index=a[0]; 0 < index; index--) {
    sum[index] = carry + a[index] + b[index];
    switch(sum[index]) {
      case 2: carry = 1; sum[index] = 0; break;
      case 3: carry = 1; sum[index] = 1; break;
      default: carry = 0;
    }    
  }
  if (carry) {
   free(a); free(b); free(sum);
   fatal_error(OVERFLOW);
  }
  return sum;
}

// 2-complement binary negation
//
int* neg (int* num) {
  for (int index = 1; index <= num[0]; index++)
    if (num[index]) num[index] = 0;
    else num[index] = 1;
  int* one = (int*) malloc ((1+num[0])*sizeof(int));
  one[0] = num[0];
  for(int index=1; index < one[0]; index++)
    one[index] = 0;
  one[one[0]] = 1;
  return add2(num,one);
}

// 2-complement binary addition
//
int* add2c (int* a, int* b) {
  int* sum = (int*) malloc ((1+a[0]) * sizeof(int));
  sum[0] = a[0];
  int carry = 0;
  for(int index=a[0]; 1 < index; index--) {
    sum[index] = carry + a[index] + b[index];
    switch(sum[index]) {
      case 2:  carry = 1; sum[index] = 0; break;
      case 3:  carry = 1; sum[index] = 1; break;
      default: carry = 0;
    }
  }
  sum[1] = carry + a[1] + b[1];
  switch (sum[1]) {
    case 2: carry++; sum[1] = 0; break;
    case 3: carry++; sum[1] = 1; break;
  }
  if (carry == 1) {
    free(a); free(b); free(sum);
    if (sum[1] == 0)
      fatal_error(UNDERFLOW);
    fatal_error(OVERFLOW);
  }
  return sum;
}

// Binary multiplication
// (all numbers are supposed of the same length)
//
int* mult2 (int* a, int* b, int err_code) {
  int len = a[0];
  int* prod = (int*) malloc ((1+len) * sizeof(int));
  prod[0] = len;

  for(int index=len; 0 < index; index--)
    prod[index] = 0;

  int offset = 0;

  for(int index=len; 0 < index; index--) {
    if (b[index]) {
      for(int idx=1; idx <= offset; idx++) {
        if (a[idx]) {
          free(a); free(b); free(prod);
          fatal_error(err_code);
        }
      }
      int carry = 0;
      for(int idx=len-offset; 0 < idx; idx--) {
        prod[idx] += a[offset+idx] + carry;
        if (prod[idx] == 2) {carry = 1; prod[idx] = 0;}
        else if (prod[idx] == 3) {carry = 1; prod[idx] = 1;}
             else carry = 0;
      }
      if (carry) fatal_error(err_code);
    };
    offset++;
  }
  return prod;
}

// 2-complement binary  multiplication
// (all numbers are supposed of the same length)
//
// BEWARE: The calls to `neg' can cause an overflow.
// Thus, before calling it, it should be checked if
// the other argument is 1. If so, give the result
// immediately; otherwise, use the following code
// (the possible overflow would occur anyway in the 
// multiplication).
//
int* mult2c (int* a, int* b) {
  int* prod = NULL;
  if (a[1]) 
    if (b[1]) {
      prod=mult2(neg(a),neg(b),OVERFLOW);
      if (prod[1]) fatal_error(OVERFLOW);
    }
    else {
      prod=neg(mult2(neg(a),b,UNDERFLOW));
      if (!prod[1]) fatal_error(UNDERFLOW);
    }
  else
    if (b[1]) {
      prod=neg(mult2(a,neg(b),UNDERFLOW));
      if (!prod[1]) fatal_error(UNDERFLOW);
    }
    else {
      prod=mult2(a,b,OVERFLOW);
      if (prod[1]) fatal_error(OVERFLOW);
    }
  return prod;
}

// Converts from 2-complement binary to decimal
//
int from2cTo10(int* compl) {
  int res = -compl[1];
  for (int index=2; index <= compl[0]; index++)
    res = res * 2 + compl[index];
  return res;
}

// Converts from binary to decimal
//
int from2To10(int* bin) {
  int res = bin[1];
  for (int index=2; index <= bin[0]; index++)
    res = res * 2 + bin[index];
  return res;
}

// Interface (in)
//
int main(int argc, char** argv) {
  if (argc == 4) {
    int len1 = strlen(argv[2]);
       int len2 = strlen(argv[3]);
    if (strcmp(argv[1],"plus") && strcmp(argv[1],"mult"))
      fatal_error(WRONG_OP);
    if (len1 == len2) {
      int* a = (int*) malloc((1+len1) * sizeof(int));
      int* b = (int*) malloc((1+len2) * sizeof(int));
      a[0] = len1;
      b[0] = len2;
      for (int index = len1; 0 < index; index--) {
        a[index] = (int) argv[2][index-1] - 48;
        if (a[index] < 0 || 1 < a[index]) {
          free(a); free(b);
          fatal_error(NO_BIN_1);
        }
        b[index] = (int) argv[3][index-1] - 48;
        if (b[index] < 0 || 1 < b[index]) {
          free(a); free(b);
          fatal_error(NO_BIN_2);
        }
      }
      int* res = NULL;
      if (strcmp(argv[1],"mult")) {
        printf("(%d)+(%d) = ",from2cTo10(a),from2cTo10(b));
        res = add2c(a,b); }
      else {
        printf("(%d)*(%d) = ",from2cTo10(a),from2cTo10(b));
        res = mult2c(a,b); 
      }
      printf("%d\n",from2cTo10(res));
      print(res);
      free(res);
      free(a); free(b); 
      return 0;
    }
    fatal_error(ARG_LEN);
  }
  fatal_error(NO_ARG);
}
