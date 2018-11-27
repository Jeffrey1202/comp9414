chop_up([], []).
chop_up([Head|Tail], [[Head]|NewList]) :-
  is_decrentedal(Head, Tail),
  chop_up(Tail, NewList).
chop_up([Head|Tail], [Head|NewList]) :-
  not(is_decrentedal(Head, Tail)),
  chop_up(Tail, NewList).

is_decrentedal(Number, [Head|_]) :-
  Number =:= Head - 1.



filter([],[]).

filter([H|[B|T]],S) :-
  B-H=:=1,
  filter([B|T],S).

filter([H|[B|T]], L) :- 
 not(B-H=:=1), 
 filter([B|T], S),
 append([H],S,L).
