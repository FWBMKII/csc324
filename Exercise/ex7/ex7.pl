:- module(ex7,
       [ sublist/2,
         with/3,
         shuffled/2]).

% sublist(S, L) : S is a sublist of list L.
% Assumes L is instantiated/ground (will be a concrete list).
sublist([S|Ss], [L|Ls]) :- sublist([S|Ss], Ls).
sublist([S|Ss], [S|Ls]) :- sublistHelper(Ss, Ls).

sublistHelper([], _).
sublistHelper([S|Ss], [S|Ls]) :- sublistHelper(Ss, Ls). 

% with(L, E, LE) : LE is the the list L with E inserted somewhere.
% Assumes L and E are instantiated.
with([], E, E).
with(L, [], L).
with([L|Ls], [E|Es], [L|LEs]) :- with(Ls, [E|Es], LEs).
with([L|Ls], [E|Es], [E|LEs]) :- withHelper([L|Ls], Es, LEs).

withHelper(L, [], L).
withHelper(L, [S|Ss], [S|Ls]) :- withHelper(L, Ss, Ls). 

% shuffled(L, S) : S is list L in some order.
shuffled(L, S) :- sort(L, X), sort(S, Y), X = Y. 