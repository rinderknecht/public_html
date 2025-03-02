// Adding two balanced ternary numbers of same length
// with overflow and underflow detection

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM   -2
#define NO_BTER_1  -3
#define NO_BTER_2  -4
#define ARG_LEN   -5
#define OVERFLOW  -6
#define UNDERFLOW -7

// Printing error messages
//
void err_msg (int err_code) {
  switch(err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: two arguments exactly are required.\n");
    break;
  case NO_BTER_1:
    fprintf(stderr,"error: first argument must be balanced ternary.\n");
    break;
  case NO_BTER_2:
    fprintf(stderr,"error: second argument must be balanced ternary.\n");
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

struct sum add3b (const char p[], const char q[], unsigned int i) {
  int err_code = 0;

  // Allocating without checking memory overflow
  //
  char* res = (char*) malloc(sizeof(char)*(i+1));
  res[i] = '\0';

  unsigned int carry = 0;
  int temp = 0;

  do {
    i--;
    temp =   (p[i] == 'I'? -1 : p[i]-'0')
           + (q[i] == 'I'? -1 : q[i]-'0')
           + carry;
    switch (temp) {
    case -3: res[i] = '0';             break;
    case -2: res[i] = '1'; carry = -1; break;
    case -1: res[i] = 'I'; carry =  0; break;
    case  2: res[i] = 'I'; carry =  1; break;
    case  3: res[i] = '0';             break;
    default: res[i] = temp + '0'; carry = 0;
    }
  } while (0 < i);

  // Error detection
  //
  if (carry == 1) {
    free(res); err_code = OVERFLOW;
  }
  else if (carry == -1) {
    free(res); err_code = UNDERFLOW;
  }

  // Making the result
  //
  struct sum add;
  add.val = res;
  add.code = err_code;

  return add;
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

    // The input must be two strings of same length.
    //
    unsigned int i = 0;
    while (   (p[i] == '0' || p[i] == '1' || p[i] == 'I')
           && (q[i] == '0' || q[i] == '1' || q[i] == 'I')) i++;

    // At this point, `i' is the rightmost index
    // of a cell whose contents is neither '0' nor '1'.
    //
    if (p[i] == '0' || p[i] == '1' || p[i] == 'I') {
      err_code = q[i]? NO_BTER_2 : ARG_LEN;
    } else
        if (q[i] == '0' || q[i] == '1' || q[i] == 'I')
          err_code = p[i]? NO_BTER_1 : ARG_LEN;
        else {
          struct sum add = add3b(p,q,i);
          if (!add.code) { puts(add.val); free(add.val); }
          err_code = add.code;
        }
  }
  else err_code = ARG_NUM;
  
  if (err_code) err_msg(err_code);
  
  return err_code;
}
