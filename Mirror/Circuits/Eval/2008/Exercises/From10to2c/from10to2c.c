// Conversion from decimal to 2-complement binary

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
char* from10to2c (int dec) {
  int in = dec; // Copy
  int i = 0;

  do {
    dec /= 2;
    i++;
  } while (dec);
  // Here, i+1 is the number of bits (including an 
  // additional leftmost bit to zero).

  // Allocating without checking for memory overflow
  //
  char* bin = (char*) malloc((i+2)*sizeof(char));
  bin[i+1] = '\0';

  if (in > 0) {
    do {
      bin[i] = in % 2 + '0';
      in /= 2;
      i--;
    } while (in);
    bin[0] = '0'; 
  } else {
    unsigned int carry = 2;
    do {
      bin[i] = in % 2 + '0' + carry;
      if (bin[i] == '2') bin[i] = '0';
      else carry = 1;
      in /= 2;
      i--;
    } while (in);
    bin[0] = '1';
  }
 
  return bin;
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
        char* bin = from10to2c(sign*dec);
        puts(bin);
        free(bin);
      } else puts("0");
  }
  else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
