%{
int char_count=0, line_count=1, word_count=0;
%}
letter [A-Za-z]
%%
{letter}+ {word_count++; char_count+=yyleng;
           printf ("[%s]\n",yytext);
          }
.         {char_count++;}
\n        {line_count++; char_count++;}
%%
int main () {
  yylex();
  printf ("There were %d characters, %d lines and %d words.\n",
          char_count, line_count, word_count);
  return 0;
}
