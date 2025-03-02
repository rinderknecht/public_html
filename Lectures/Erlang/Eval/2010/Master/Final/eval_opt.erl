-module(eval_opt).
-export([eval/2]).

% Calculator for simple arithmetic expressions with variables,
% optimised for speed.
%
% Christian Rinderknecht, 15 June 2010.

% lookup(X,Env,UB) is {val,Val} if variable X occurs in the
% environment Env and no previous unbound variables were reported in
% UB. Otherwise, the value of the call is {unbound,Vars}, where Vars
% is the list of unbound variables up to now (UB), extended with X if
% not present already.
%
lookup(X,[{X,Val}|_],[]) -> {val,Val};           % Hit
lookup(X,[{X,  _}|_],UB) -> {unbound,UB}; % Hit but already unbound vars
lookup(X,         [],UB) -> {unbound,add(X,UB)}; % New unbound variable
lookup(X,    [_|Env],UB) -> lookup(X,Env,UB).    % Search

% add(X,Vars) is the list of variables Vars with X at the end if not
% already occurring, otherwise it is exactly Vars. The code makes use
% of continuation-passing style (CPS) in order to allow the result to
% share 100% of the input list when X is already in Vars. Had we use
% instead
%
% add(X,        []) -> [X];              % Added
% add(X,Vars=[X|_]) -> Vars;             % Hit
% add(X,  [Y|Vars]) -> [Y|add(X,Vars)].  % Search
%
% then the case `Hit' would result in a list identical to the input
% but different in memory up to the occurrence of X in the original
% Vars (in the worst case, X is the last variable in Vars).
%
add(X,Vars)            -> add(X,Vars,Vars,fun(I) -> I end).
add(X,      [],   _,K) -> K([X]);         % Added at the end
add(X,[X|   _],Orig,_) -> Orig;           % Hit (*continuation dropped*)
add(X,[Y|Vars],Orig,K) -> add(X,Vars,Orig,fun(V) -> K([Y|V]) end). % Search

% The main evaluation loop of the interpreter: eval(Exp,Env) evaluates
% in a numerical value, if no error occurred, or in an error. An error
% is either an attempt at division by zero (resulting in the atom
% div_by_zero) or failed variable lookups in the environment Env. If
% an error is encountered, no subsequent numerical calculations are
% carried out. The difference in the processing of the two kinds of
% errors is that it is required that *all* unbound variables in Exp be
% reported, and only once, in Vars in the result {unbound,Vars}.
%
eval(Exp,Env) -> case eval(Exp,Env,[]) of
                   {val,Val} -> Val;  % To get rid of the atom `val'
                         Err -> Err   % Errors unchanged
                 end.                         

eval(       {var,X},Env,UB) -> lookup(X,Env,UB);
eval(       {num,N},  _,[]) -> {val,N};
eval(       {num,_},  _,UB) -> {unbound,UB}; % No value: unbound vars
eval(     {neg,Exp},Env,UB) -> case eval(Exp,Env,UB) of
                                 {val,Val} -> {val,-Val};
                                       Err -> Err
                               end;
eval({Op,Exp1,Exp2},Env,UB) ->
  case {Op,eval(Exp2,Env,UB)} of
    {  _,{unbound,Vars}} -> eval(Exp1,Env,Vars);
    {  _,   div_by_zero} -> div_by_zero;
    {dvn,       {val,0}} -> div_by_zero; % That is why we match a pair
    {  _,    {val,Val2}} -> 
      case eval(Exp1,Env,UB) of
        {val,Val1} -> {val,case Op of add -> Val1 + Val2;
                                      sub -> Val1 - Val2;
                                      mul -> Val1 * Val2;
                                      dvn -> Val1 / Val2
                           end};
               Err -> Err
      end
  end.
