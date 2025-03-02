%{
#include<stdlib.h>

typedef struct stack_ stack;
struct stack_ {
  double arg;
  stack* next;
};

void free_stack (stack* s) {
  if (s != NULL) {
    stack* sub_stack = (*s).next;
    free(s);
    free_stack(sub_stack);
  }
}

int is_empty(stack* s) {
  return s == NULL? 1 : 0;
}

/* Exit value 2 means "Memory overflow" */
 void push (stack** s, double arg, int line_count, int char_count) {
  stack* new_stack = malloc(sizeof(stack));
  if (new_stack == NULL) {
    printf("Fatal error: Memory overflow at line %d, char %d.\n",
           line_count, char_count);
    if (s != NULL) free_stack(*s);
    exit (2);
  }
  (*new_stack).arg = arg;
  (*new_stack).next = *s;
  *s = new_stack;
}

/*  [pop(s,t)] pops a value in stack [*s] to which [*t] is set. */
/*  [pop(s,t)] is 1 if the stack pointer [s] is NULL
                    (no actual stack)  */
/*                2 if the stack [*s] is empty (NULL) */
int pop (stack** s, double* top) {
  if (s != NULL)
    if (*s != NULL) {
      *top = (**s).arg;
      *s = (**s).next;
      return 0;
    }
    return 2;
  return 1;
}

/* [top(s,t)] finds the top element of stack [*s]  */
/*            to which [*t] is set. */
/* [top(s,t)] is 1 if the stack is empty */
/*            is 0 if the stack is not empty */
int top (stack* s, double* top) {
  if (is_empty(s)) 
    return 1;
  *top=(*s).arg;
  return 0;
}

/* Exit value 1 means "Missing operator" */
 double wrapped_pop(stack** s, char* op_name, int line_count, int char_count) {
  double arg = 0;
  if (pop(s, &arg)) {
      printf("Error: Missing operand for operator `%s' at line %d, char %d.\n", 
             op_name, line_count, char_count);
      if (s != NULL) free_stack(*s);
      exit(1);
  }
  return arg; 
}

void print (stack* s) {
  if (s != NULL) {
    printf("%f ",(*s).arg);
    print((*s).next);
  }
}

int char_count = 1;
int line_count = 1;
stack* arg_stack = NULL;
%}
%%
[ \t]+   {
          char_count+=yyleng;
         }
[0-9]+   { /* No check for overflows and underflows. */ 
          double arg = strtod(yytext,(char **) NULL);
          printf("arg is %f\n",arg);
          push(&arg_stack, arg, line_count, char_count);
          printf("stack is ");
          print(arg_stack);
          printf("\n");
          char_count+=yyleng;
         }
"+"      { 
          double arg2 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          double arg1 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          push(&arg_stack, arg1 + arg2, line_count, char_count);
          char_count++;
         }
"-"      { 
          double arg2 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          double arg1 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          push(&arg_stack, arg1 - arg2, line_count, char_count);
          char_count++;
         }
"*"      { 
          double arg2 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          double arg1 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          push(&arg_stack, arg1 * arg2, line_count, char_count);
          char_count++;
         }
"/"      {
          /* Exit status 3 means "Division by zero" */
          double arg2 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          double arg1 = wrapped_pop(&arg_stack, yytext, line_count, char_count);
          if (arg2 == 0) {
            printf("Error: Division by zero at line %d, char %d.\n",
                   line_count, char_count);
            free_stack(arg_stack);
            exit(3);
          }
          push(&arg_stack, arg1 / arg2, line_count, char_count);
          char_count++; 
         }
\n       {/* Exit status 4 means "Empty input" */
          /* Exit status 5 means "Too many operands" */
          /* Exit status 6 means "Illegal character" */
          double result=0;

          if (pop(&arg_stack, &result)) {
            printf("Error: Empty input.\n");
            free_stack(arg_stack);
            exit(4);
          }

          if (!is_empty(arg_stack)) {
            printf("Error: Too many operands at line %d.\n", line_count);
            free_stack(arg_stack);
            exit(5);
          }
  
          printf(">>> Result is %f.\n", result);
          char_count=1;
          line_count++;
         }
.        {
          printf("Error: Illegal character `%s' at line %d, char %d.\n", 
                 yytext, line_count, char_count);
          free_stack(arg_stack);
          exit(6);
         }
%%
int main (int argc, char** argv) {
  ++argv, --argc; /* Skip over programme name */
  if (argc > 0)
    yyin = fopen(argv[0], "r");
  else
    yyin = stdin;

  yylex();

  return 0;
}
