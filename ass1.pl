%COMP9414 Assignment 1 - Prolog Programming
%programmed by�� Yizheng Ying
%zID:z5141180
%
%create date:25/2/2018

%Question1:
% Base predicate
% Result of two empty lists.
filter([],[]).
%Recursive predicate for new head larger than or equal to 0
filter([H|T],S) :-
  H>=0,
  filter(T,S).
% Recursive predicate for new head less than 0
%   
filter([H|T], L) :- 
  H<0, 
  filter(T, S),
  append([H],S,L).
% Result of empty list should be zero.
sumsq_f([],0).
% The result of calculation.
sumsq_f([Item|Rest],Sum):-
    sumsq_f(Rest,SumOfRest),
    Sum is Item^2+SumOfRest.
%Use two Recursive predicates to get the result.
sumsq_neg(List,Sum):-
    filter(List,Y),
    sumsq_f(Y,Sum).


%Question2:
% Case1 two arguments are all lists.
%   
all_like_all([],[]).
% Case2 the first argument is list.
%   
all_like_all([],_).
% Case3 the second argument is list.
%   
all_like_all(_,[]).
%Recursive predicate for person who like all fruits
all_like_all([P1|P2],[F1|F2]):-
        likes(P1,F1),
        all_like_all(P2,[F1|F2]),
        all_like_all([P1],F2).

%Question3:
%Recursive predicate for the condition when the first argument and the
%second argument are same number.
sqrt_table(N,N,R):-
   Result is sqrt(N),
   R=[[N,Result]].
%Recursive predicate for N>M and they are all larger than 0
sqrt_table(N,M,R):-
      N>M,
      M>0,
      Result is sqrt(N),
      Head=[N,Result],
      N1 is N-1,
      sqrt_table(N1,M,R1),
      R=[Head|R1].

%Question4:
% Base predicate
% Result of two empty lists.
chop_up([], []).
% Result of two not empty lists.
chop_up([H],[H]).
%Case1
%Recursive predicate for H1 is not equal to H+1 and H is a number.
chop_up([H|T],[H|Result]):-
	number(H),
	[H1|_]=T,
	H1=\=H+1,
	chop_up(T,Result).
%Case2
%Recursive predicate for H1 is equal to H+1 and H is still a number.
chop_up([H|T],Result):-
	number(H),
	[H1|T1]=T,
	H1 is H+1,
	chop_up([[H,H1]|T1],Result).
%Case3
%Recursive predicate for H3 is not equal to H2+1 when H is not a number.
chop_up([H|T],[H|Result]):-
	not(number(H)),
	reverse(H,H1),
	[H2|_]=H1,
	[H3|_]=T,
	H3=\=H2+1,
	chop_up(T,Result).
%Case4
%Recursive predicate for H4 is equal to H3+1 when H is not a number 
%
chop_up([H|T],Result):-
	not(number(H)),
	[H1|_]=H,
	reverse(H,H2),
	[H3|_]=H2,
	[H4|T1]=T,
	H4 is H3+1,
	chop_up([[H1,H4]|T1],Result).



%Question5:
%Case1 
%tree(empty, M, empty), which is a variable.
tree_eval(Number, tree(empty, M, empty), Value):-
    M=z,
    Value is Number.
%Case2
%tree(empty, M, empty), where M is a number.
tree_eval(_, tree(empty, M, empty), Value) :-
    number(M),
    Value is M.
%  - tree(L, Op, R), where L and R are other trees, and Op is an arithmetic
% operator,'-'
tree_eval(Num, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     X = '-',
     Eval is LE- RE.
%  - tree(L, Op, R), where L and R are other trees, and Op is an arithmetic
% operator,'+'
tree_eval(Num, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     X = '+',
     Eval is LE + RE.
%  - tree(L, Op, R), where L and R are other trees, and Op is an arithmetic
% operator,'/'
tree_eval(Num, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     X = '/',
     RE =\= 0,
     Eval is LE / RE.
%  - tree(L, Op, R), where L and R are other trees, and Op is an arithmetic
% operator,'*'
tree_eval(Num, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     X = '*',
     Eval is LE * RE.








