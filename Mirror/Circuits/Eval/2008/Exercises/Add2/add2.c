// Adding two binary numbers of same length
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
#define ARG_LEN   -5
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
  case ARG_LEN:
    fprintf(stderr,"error: numbers must have same length.\n");
    break;
  case OVERFLOW:
    fprintf(stderr,"error: overflow.\n");
    break;
  }
  fflush(stderr);
}

// Computation
//
char* add2 (const char p[], const char q[], unsigned int i) {
  // Allocating without checking memory overflow
  //
  char* res = (char*) malloc(sizeof(char)*(i+1));
  res[i] = '\0';

  unsigned int carry = 0;

  do {
    i--;
    res[i] = p[i] + (q[i] - '0') + carry;
    switch (res[i]) {
    case '2': carry = 1; res[i] = '0'; break;
    case '3': res[i] = '1'; break;
    default : carry = 0;
    }
  } while (0 < i);

  // Checking for an overflow
  //
  if (carry) { free(res); res = NULL; }

  return res;
}

// Input/Output
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
          char* res = add2(p,q,i);
          if (res) { puts(res); free(res); }
          else err_code = OVERFLOW;
        }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
