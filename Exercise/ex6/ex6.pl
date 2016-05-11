:- module(ex6,
       [ nat/1,
         succ/1,
         even/1,
         odd/1,
         subtract/3]).

% Natural numbers.
succ(0).
succ(succ(X)) :- succ(X).
nat(0).
nat(succ(X)) :- nat(X).

% Even and odd predicates.
even(0).
even(succ(succ(X))) :- even(X).

odd(succ(0)).
odd(succ(succ(X))) :- odd(X).

% Subtraction
subtract(X, 0, X).
subtract(succ(X), succ(Y), Z) :- subtract(X, Y, Z).

