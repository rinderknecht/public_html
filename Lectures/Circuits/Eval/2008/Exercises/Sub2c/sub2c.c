// Subtracting two 2-complement binary numbers
// of same length with overflow/underflow

// Standard libraries
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
  case ARG_LEN:
    fprintf(stderr,"error: numbers must have same length.\n");
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

struct sum sub2c (const char p[], const char q[], unsigned int i) {
  int err_code = 0;

  // Allocating without checking memory overflow
  //
  char* res = (char*) malloc(sizeof(char)*(i+1));
  res[i] = '\0';

  // Subtracting two binary numbers up to the second bit
  // from the left.
  //
  unsigned int carry = 1;
  do {
    i--;
    res[i] = p[i] + ('1' - q[i]) + carry;
    switch (res[i]) {
    case '2': carry = 1; res[i] = '0'; break;
    case '3': carry = 1; res[i] = '1'; break;
    default : carry = 0;
    }
  } while (0 < i);

  // Error detection
  //
  if (p[0] == '1' && q[0] == '0' && res[0] == '0')
     { free(res); err_code = UNDERFLOW; }
  else if (p[0] == '0' && q[0] == '1' && res[0] == '1')
          { free(res); err_code = OVERFLOW; }

  // Making the result
  //
  struct sum sub;
  sub.val = res;
  sub.code = err_code;

  return sub;
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

    // The input must be two bit strings of same length.
    //
    unsigned int i = 0;
    while (   (p[i] == '0' || p[i] == '1')
           && (q[i] == '0' || q[i] == '1')) i++;

    // At this point, `i' is the rightmost index
    // of a cell whose contents is neither '0' nor '1'.
    //
    // At this point, `i' is the rightmost index
    // of a cell whose contents is neither '0' nor '1'.
    //
    if (p[i] == '0' || p[i] == '1') {
      err_code = q[i]? NO_BIN_2 : ARG_LEN;
    } else
        if (q[i] == '0' || q[i] == '1')
          err_code = p[i]? NO_BIN_1 : ARG_LEN;
        else {
          struct sum sub = sub2c(p,q,i);
          if (!sub.code) { puts(sub.val); free(sub.val); }
          err_code = sub.code;
        }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
