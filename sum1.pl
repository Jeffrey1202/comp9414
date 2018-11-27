filter([],[]).

filter([H|T],S) :-
  H>=0,
  filter(T,S).

filter([H|T], L) :- 
 H<0, 
 filter(T, S),
 append([H],S,L).

sumsq_f([],0).
sumsq_f([Item|Rest],Sum):-
    sumsq_f(Rest,SumOfRest),
    Sum is Item^2+SumOfRest.

sumsq_neg(List,Sum):-
    filter(List,Y),
    sumsq_f(Y,Sum).