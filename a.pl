consecutive(N,P):-
    N is P-1.

removeS([],[]).
removeS([H],[H]).

removeS([H1,H2],[]):-
    consecutive(H1,H2).
removeS([H1,H2|T],[H1|L]):-
    not(consecutive(H1,H2)),
    removeS([H2|T],L).
removeS([H1,H2,H3|T],[[H1,H2]|L]):-
    consecutive(H1,H2),
    not(consecutive(H2,H3)),
    removeS([H2,H3|T],L).
removeS([H1,H2,H3|T],[[_,B2]|L]):-
    consecutive(H1,H2),
    consecutive(H2,H3),
    [_,B2]=[_,H3],
    removeS(T,L).


removes([],[]).
removes([H],[H]).

removes([H1,H2],[]):-
    consecutive(H1,H2).
removes([H1,H2|T],[H|L]):-
    not(consecutive(H1,H2)),
    H=H1,
    removes([H2|T],L).
removes([H1,H2|T],[H|L]):-
    consecutive(H1,H2),
    H=[H1,_],
    removes([H2|T],L).
removes([H1,H2|T],[H|L]):-
    not(consecutive(H1,H2)),
    H=[_,H1],
    removes([H2|T],L).
removes([H1,H2,H3|T],[H|L]):-
    consecutive(H1,H2),
    not(consecutive(H2,H3)),
    H=[_,H2],
    removes([H2,H3|T],L).

    
    
   

    