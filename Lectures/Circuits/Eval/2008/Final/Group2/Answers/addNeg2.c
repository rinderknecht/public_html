// Addition of negabinary numbers

// Library headers
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM   -1
#define NO_BIN_1  -2
#define NO_BIN_2  -3
#define ARG_LEN   -4
#define OVERFLOW  -5
#define UNDERFLOW -6

// Putting together the error code (0 if no error) with the result
// (NULL in case of error).
//
struct res {
  char* val;
  int code;
};

// Addition
//
// [-1]*(-2)^i = [ 1]*(-2)^{i+1} + [1]*(-2)^i
// [ 2]*(-2)^i = [-1]*(-2)^{i+1} + [0]*(-2)^i
// [ 3]*(-2)^i = [-1]*(-2)^{i+1} + [1]*(-2)^i
//
// Th. N contains an even number of bits iff N is negative.
//
// This and the rule for the nonzero carries lead to:
//
// If N contains an even number of bits, then
//  if the carry is 1, then overflow (the complete number is positive)
//  else if it is -1, then underflow (the complete number is negative).
//
// If N contains an odd number of bits, then
//   if the carry is 1, then underflow (the complete number is negative)
//   else if it is -1, then overflow (the complete number is positive).
//
struct res addNeg2 (const char p[], const char q[], unsigned int len) {
  int err_code = 0;

  // Allocating without checking memory overflow
  //
  char* res = (char*) malloc(sizeof(char)*(len+1));
  res[len] = '\0';

  unsigned int i = len;
  int carry = 0;
  do {
    i--;
    res[i] = (p[i] - '0') + (q[i] - '0') + carry;
    switch (res[i]) {
    case -1: res[i] = '1'; carry =  1; break;
    case  2: res[i] = '0'; carry = -1; break;
    case  3: res[i] = '1'; carry = -1; break;
    default: res[i] = res[i] + '0'; carry = 0;
    }
  } while (i > 0);
  
  // Detecting underflow and overflow
  //
  unsigned int rem = len % 2;
  switch (carry) {
  case  1: err_code = rem ? UNDERFLOW : OVERFLOW;  break;
  case -1: err_code = rem ?  OVERFLOW : UNDERFLOW; break;
  }

  // Making the result
  //
  struct res add;
  add.val = res;
  add.code = err_code;

  return add;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: two arguments exactly are required.\n");
    break;
  case NO_BIN_1:
    fprintf(stderr,"error: first number is not binary.\n"); break;
  case NO_BIN_2:
    fprintf(stderr,"error: second number is not binary.\n");break;
  case ARG_LEN:
    fprintf(stderr,"error: numbers must be of same length.\n"); break;
  case OVERFLOW:  fprintf(stderr,"error: overflow\n"); break;
  case UNDERFLOW: fprintf(stderr,"error: underflow\n"); break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int err_code = 0;

  if (argc == 3) {
    // For faster access to the input.
    //
    char* p = argv[1];
    char* q = argv[2];

    // The input must be two bitstrings of same length.
    //
    unsigned int i = 0;
    while (   (p[i] == '0' || p[i] == '1')
           && (q[i] == '0' || q[i] == '1')) i++;

    // At this point, `i' is the rightmost index
    // of a cell whose contents is neither '0' nor '1'.
    //
    if (p[i] == '0' || p[i] == '1') {
      err_code = q[i]? NO_BIN_2 : ARG_LEN;
    } else
        if (q[i] == '0' || q[i] == '1')
          err_code = p[i]? NO_BIN_1 : ARG_LEN;
        else {
          struct res add = addNeg2(p,q,i);
          if (!add.code) { puts(add.val); free(add.val); }
          err_code = add.code;
        }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
