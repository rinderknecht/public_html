#include<stdio.h>

int from3to10(int* ternary, int t_size) {
  int decimal=ternary[0];
  for (int index=1; index<t_size; index++) {
    decimal = decimal * 3 + ternary[index];
  }
  return decimal;
}

int main() {
  int t [3] = {1,1,2};
  printf("%d\n",from3to10(t,3));
  return 0;
}
