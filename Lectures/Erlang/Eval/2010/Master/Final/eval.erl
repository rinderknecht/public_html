-module(eval).
-export([eval/2]).

% Evaluation of the abstract syntax tree of an arithmetic expression
%
lookup(X,[{X,Val}|_]) -> Val;
lookup(X,    [_|Env]) -> lookup(X,Env).

eval(        {var,X},Env) -> lookup(X,Env);
eval(        {num,N},  _) -> N;
eval(      {neg,Exp},Env) -> -eval(Exp,Env);
eval({add,Exp1,Exp2},Env) -> eval(Exp1,Env) + eval(Exp2,Env);
eval({sub,Exp1,Exp2},Env) -> eval(Exp1,Env) - eval(Exp2,Env);
eval({mul,Exp1,Exp2},Env) -> eval(Exp1,Env) * eval(Exp2,Env);
eval({dvn,Exp1,Exp2},Env) -> eval(Exp1,Env) / eval(Exp2,Env).
