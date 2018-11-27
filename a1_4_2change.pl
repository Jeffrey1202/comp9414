get_last_ele(L,Ele):-
	reverse(L,New_L),
	New_L=[X|_],
      Ele is X.

chop_up([],[]).

chop_up([H],[H]).

chop_up([L,R|Tail],[L|NewListOfRest]):-
	number(L),
	L=\=R-1,
	chop_up([R|Tail],NewListOfRest).

chop_up([L,R|Tail],NewListOfRest):-
	number(L),
	L=:=R-1,
	chop_up([[L,R]|Tail],NewListOfRest).
	


chop_up([L,R|Tail],[L|NewListOfRest]):-
	is_list(L),
	get_last_ele(L,Ele),
	Ele=\=R-1,
	chop_up([R|Tail],NewListOfRest).

chop_up([L,R|Tail],NewListOfRest):-
	is_list(L),
	[H|_]=L,% H is the first element in L.
	get_last_ele(L,Ele),%Ele is the last element in L.
	Ele=:=R-1,
	chop_up([[H,R]|Tail],NewListOfRest).
	

	