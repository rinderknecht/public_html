// Multiplication of two binary numbers
//
// NOTE: This version uses signed integers. See mul2u.c

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_BIN_1 -2
#define NO_BIN_2 -3
#define ARG_LEN  -4
#define OVERFLOW -5

// Conversions from binary to decimal
//
unsigned int from2to10__ (unsigned int dec, const char bin[]) {
  return *bin ? from2to10__(dec*2+*bin-'0',bin+1) : dec;
}

unsigned int from2to10 (const char bin[]) {
  return from2to10__(0,bin);
}

// Position of the leftmost nonzero bit or '\0' if all bits are '0'.
//
int lnz__ (const char bin[], int i) {
  return *bin == '0'? lnz__(bin+1,i+1) : i;
}

int lnz (const char bin[]) {
  return lnz__(bin,0);
}

// Checking that a prefix of a bit string is made of '0'
//
unsigned int zero_prefix (const char bin[], unsigned int len) {
  unsigned int res = 1;

  if (len > 0) 
    res = (*bin == '0')? zero_prefix (bin+1,len-1) : 0;

  return res;
}

// Multiplication
//
char* mul2 (const char p[], const char q[], int len) {
  char* r = NULL;
  int  carry = 0;

  int lnz_q = lnz(q);
  int zpref_p = len - lnz_q - 1; 

  if (zero_prefix (p,zpref_p)) {
    r = (char*) malloc ((len+1)*sizeof(char));
    r[len] = '\0';

    for (int i = 0; i < len; i++) r[i] = '0';

    int offset = 0; // Number of computed bits

    for (int j = len-1; !carry && lnz_q <= j; j--) {
      // Computing bit #j
      if (q[j] == '1') {
        offset = len-1-j;
        for (int i = j; zpref_p <= i+offset; i--) {
          r[i] = r[i] + (p[i+offset] - '0') + carry;
          switch (r[i]) {
          case '3': r[i] = '1'; break;
          case '2': r[i] = '0'; carry = 1; break;
          default : carry = 0;
          }
        }
      }
    }
  }
  else // Sufficient condition for an overflow 
    carry = 1;

  return carry? NULL : r;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case NO_BIN_1:
    fprintf(stderr,"error: first number must be binary\n");
    break;
  case NO_BIN_2:
    fprintf(stderr,"error: second number must be binary\n");
    break;
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  case ARG_LEN:
    fprintf(stderr,"error: binary numbers must be of same length\n");
    break;
  case OVERFLOW:
    fprintf(stderr,"error: overflow\n");
    break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int ret_code = 0;

  if (argc == 3) {
    char* p = argv[1];
    char* q = argv[2];
    int len1 = 0;
    int len2 = 0;

    while (p[len1] == '0' || p[len1] == '1') len1++;

    if (p[len1])
      ret_code = NO_BIN_1;
    else {
      while (q[len2] == '0' || q[len2] == '1') len2++;

      if (q[len2])
        ret_code = NO_BIN_2;
      else
        if (len1 != len2)
          ret_code = ARG_LEN;
        else {
          unsigned int dec1 = from2to10(p);
          unsigned int dec2 = from2to10(q);

          char* r  = mul2(p,q,len1);
          if (r) {
            unsigned int dec = from2to10(r);
            printf("%s (%u * %u = %u)\n",r,dec1,dec2,dec);
            free(r);
          }
          else ret_code = OVERFLOW;
        }
    }
  } else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);

  return ret_code;
}
