get_last_ele(L,Ele):-
	reverse(L,New_L),
	New_L=[X|_],
      Ele is X.

chop_up([],[]).

chop_up([H],[H]).

chop_up([H|T],[H|Result]):-
	number(H),
	[H1|_]=T,
	H1=\=H+1,
	chop_up(T,Result).

chop_up([H|T],Result):-
	number(H),
	[H1|T1]=T,
	H1 is H+1,
	chop_up([[H,H1]|T1],Result).
	


chop_up([H|T],[H|Result]):-
	not(number(H)),
	reverse(H,H1),
	[H2|_]=H1,
	[H3|_]=T,
	H3=\=H2+1,
	chop_up(T,Result).

chop_up([H|T],Result):-
	not(number(H)),
	[H1|_]=H,
	reverse(H,H2),
	[H3|_]=H2,
	[H4|T1]=T,
	H4 is H3+1,
	chop_up([[H1,H4]|T1],Result).
	

	
