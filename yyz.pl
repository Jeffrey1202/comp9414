likes(mary, apple).
likes(mary, pear).
likes(mary, grapes).
likes(tim, mango).
likes(tim, apple).
likes(jane, apple).
likes(jane, mango).



all_like_all([],[]).
all_like_all([],_).
all_like_all(_,[]).
all_like_all([P1|P2],[F1|F2]):-
        likes(P1,F1),
        all_like_all(P2,[F1|F2]),
        all_like_all([P1],F2).
