%{
#include<string.h>
char* keyword[] = {"else", "if", "return", "then"};
%}
num  (\.[0-9]+|[+\-]?[0-9]+(\.[0-9]+)?)(E[+\-]?[0-9]+)?
id   [A-Za-z]([_]*[A-Za-z0-9])*
ws   [ \n\t]+
%%
{num}  { printf ("num<%s>\n",yytext); }
{id}   { int index = 0;
         while (index <= 3 && strcmp(keyword[index],yytext))
           index++;
         if (index == 4) printf("id<%s>\n",yytext);
         else printf("kwd<%s>\n",yytext);
       }
{ws}   {}
"+"    { printf ("plus<>\n"); }
%%
