// Adding two 2-complement binary numbers
// of different lengths with overflow/underflow

// Note: The code could be improved by removing all the leading 1s of
// each negative input (except the one), and all leading 0s of each
// positive input. Currently, this program computes for example
// 10001 + 00001001 = 11111010, which is not pretty.

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM   -1
#define NO_BIN_1  -2
#define NO_BIN_2  -3
#define OVERFLOW  -4
#define UNDERFLOW -5

// Printing error messages
//
void err_msg (int err_code) {
  switch(err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: two arguments exactly are required.\n");
    break;
  case NO_BIN_1:
    fprintf(stderr,"error: first argument must be binary.\n");
    break;
  case NO_BIN_2:
    fprintf(stderr,"error: second argument must be binary.\n");
    break;
  case OVERFLOW:
    fprintf(stderr,"error: overflow.\n");
    break;
  case UNDERFLOW:
    fprintf(stderr,"error: underflow.\n");
    break;
  }
  fflush(stderr);
}

// Computation
//

// Putting together the error code (0 if no error) with the result
// (NULL in case of error).
//
struct sum {
  char* val;
  int code;
};

// `i' is the length of the bit string `p'.
// `j' is the length of the bit string `q'.
//
struct sum add2c_diff (const char p[], const char q[],
                       unsigned int i, unsigned int j) {
  int err_code = 0;

  // Make sure that `p' is the longest (length `i')
  // and `q' is the shortest (length `j').
  //
  if (i < j) {
    const char* r = p; 
    p = q; 
    q = r;
    unsigned int k = i;
    i = j;
    j = k;
  }
  
  // Allocating without checking memory overflow
  //  
  char* res = (char*) malloc(sizeof(char)*(i+1));
  res[i] = '\0';

  // Adding up to the length of `q' (ie. `j')
  //
  unsigned int carry = 0;

  do {
    i--; j--;
    res[i] = p[i] + (q[j] - '0') + carry;
    switch (res[i]) {
    case '2': carry = 1; res[i] = '0'; break;
    case '3': carry = 1; res[i] = '1'; break;
    default : carry = 0;
    }
  } while (1 < j);

  // If `p' is strictly longer than `q', then
  // add the bits of `p' to q[0].
  //
  int padding_bit = q[0] - '0';

  while (1 < i) { // `p' is strictly longer than `q'
    i--;
    res[i] = p[i] + padding_bit + carry;
    switch (res[i]) {
    case '2': carry = 1; res[i] = '0'; break;
    case '3': carry = 1; res[i] = '1'; break;
    default : carry = 0;
    }
  }

  // In order to detect undeflows and overflows,
  // we accumulate the two last values of the carry.
  //
  res[0] = p[0] + padding_bit + carry;  
  switch (res[0]) {
  case '2': carry++; res[0] = '0'; break;
  case '3': carry++; res[0] = '1'; break;
  }

  // Error detection
  //
  if (carry == 1)
    if (res[0] == '0') { free(res); err_code = UNDERFLOW; }
    else { free(res); err_code = OVERFLOW; }

  // Making the result
  //
  struct sum add;
  add.val = res;
  add.code = err_code;

  return add;
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

    // The input must be two valid bit strings.
    //
    unsigned int i = 0;
    while (p[i] == '0' || p[i] == '1') i++;

    if (p[i]) err_code = NO_BIN_1;
    else {
      unsigned int j = 0;
      while (q[j] == '0' || q[j] == '1') j++;

      if (q[j]) err_code = NO_BIN_2;
      else {
        struct sum add = add2c_diff(p,q,i,j);
        if (!add.code) { puts(add.val); free(add.val); }
        err_code = add.code;
      }
    }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
