%{
int char_count=0, line_count=1;
%}
%%
.  {char_count++;}
\n {char_count++; line_count++;}
%%
int main () {
  yylex();
  printf ("There were %d characters in %d lines\n",char_count,line_count);
  return 0;
}
