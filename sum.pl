member(X,[X|_]):-
    X<0.
member(X,[_|T]):-
       member(X,T).
sumsq_neg([],0).  
sumsq_neg(List,Sum):-
    member(X,List).
    sum_items(X,Sum1),
    Sum is Sum1+X*X.


%sum_items([X|_], 0).              % empty list
%sum_items([Item|Rest], Sum) :- % recursive case
 %   sum_items(Rest, SumOfRest),
  %  Sum is Item*Item + SumOfRest.



