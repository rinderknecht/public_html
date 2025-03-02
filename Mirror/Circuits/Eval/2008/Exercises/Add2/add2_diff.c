// Adding two binary numbers of different lengths
// with overflow detection

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM   -2
#define NO_BIN_1  -3
#define NO_BIN_2  -4
#define OVERFLOW  -6

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
  }
  fflush(stderr);
}

// Computation
//
// `i' is the length of the bit string `p'.
// `j' is the length of the bit string `q'.
//
char* add2 (const char p[], const char q[], 
            unsigned int i, unsigned int j) {
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
  } while (0 < j);

  // If `p' is strictly longer than `q', then
  // add the bits of `p' to 0.
  //
  while (0 < i) {
    i--;
    res[i] = p[i] + carry;
    if (res[i] == '2') res[i] = '0';
    else carry = 0;
  }

  // Checking for an overflow
  //
  if (carry) { free(res); res = NULL; }

  return res;
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
        char* res = add2(p,q,i,j);
        if (res) { puts(res); free(res); }
        else err_code = OVERFLOW;
      }
    }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
