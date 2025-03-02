// Conversion from decimal to balanced ternary

// Note: In C, the sign of the remainder is the sign of the dividend.
// That is, the sign of (a % b) is the sign of (a).

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM    -1
#define DEC_DIGITS -2

// Computation
//
char* from10to3b (int dec) {
  int in = dec; // Copy
  int i = 0;

  do {
    switch(dec%3) {
    case -2: dec = dec/3 - 1; break;
    case  2: dec = dec/3 + 1; break;
    default: dec = dec/3;
    }
    i++;
  } while (dec);
  // Here, i+1 is the number of trits

  // Allocating without checking for memory overflow
  //
  char* bter = (char*) malloc((i+2)*sizeof(char));
  bter[i+1] = '\0';

  do {
    i--;
    switch(in%3) {
    case -2: bter[i] = '1'; in = in/3 - 1;    break;
    case -1: bter[i] = 'I'; in = in/3;        break;
    case  2: bter[i] = 'I'; in = in/3 + 1;    break;
    default: bter[i] = in%3 + '0'; in = in/3;
    }
  } while (in);
 
  return bter;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case DEC_DIGITS:
    fprintf(stderr,"error: only decimal digits allowed\n");
    break;
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int ret_code = 0;

  if (argc == 2) {
    unsigned int dec = 0;
    char* arg = argv[1];
    int sign = 1; // 1 if `0 < dec', -1 if `0 > dec'

    if (*arg == '-') { sign = -1; arg++; }

    while ('0' <= *arg && *arg <= '9') {
      dec = 10 * dec + (*arg - '0');
      arg++;
    };

    if (*arg) {
      ret_code = DEC_DIGITS;
    } else 
      if (dec) {
        char* bter = from10to3b(sign*dec);
        puts(bter);
        free(bter);
      } else puts("0");
  }
  else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
