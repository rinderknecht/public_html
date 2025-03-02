// Conversion from decimal to negabinary

// In C, if a => 0 and b < 0, then a/b <= 0 and a%b >= 0.
//       if a <= 0 and b < 0, then a/b >= 0 and a%b <= 0.

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
char* from10toNeg2 (int dec) {
  int in = dec; // Copy
  int i = 0;
  int rem = 0;

  do {
    rem = dec%(-2);
    dec = dec/(-2);
    if (rem == -1) dec++;
    i++;
  } while (dec);

  // Allocating without checking for memory overflow
  //
  char* neg2 = (char*) malloc((i+1)*sizeof(char));
  neg2[i] = '\0';

  do {
    i--;
    rem = in%(-2);
    in = in/(-2);
    if (rem == -1) { rem = 1; in++; }
    neg2[i] = rem + '0';
  } while (in);
   
  return neg2;
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
        char* neg2 = from10toNeg2(sign*dec);
        puts(neg2);
        free(neg2);
      } else puts("0");
  }
  else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
