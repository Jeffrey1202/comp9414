tree_eval(Number, tree(empty, M, empty), Value):-
    M=z,
    Value is Number.
tree_eval(_, tree(empty, M, empty), Value) :-
    number(M),
    Value is M.
tree_eval(Num, tree(Left,X,Right), Eval) :- 
	X = '-',
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     
     Eval is LE- RE.
tree_eval(Num, tree(Left,X,Right), Eval) :- 
	X = '+',
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     
     Eval is LE + RE.
tree_eval(Num, tree(Left,X,Right), Eval) :- 
	X = '/',
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     
     Eval is LE / RE.
tree_eval(Num, tree(Left,X,Right), Eval) :- 
	X = '*',
　　 tree_eval(Num, Left, LE), 
　　 tree_eval(Num, Right, RE),
     
     Eval is LE * RE.



