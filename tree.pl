tree_eval(Value, tree(empty, z, empty), Value).
tree_eval(_, tree(empty, Eval, empty), Eval) :-
    not(Eval = z).
tree_eval(Value, tree(L, Op, R), Eval) :-
    not(L = empty),
    not(R = empty),
    tree_eval(Value, L, EvalLeft),
    tree_eval(Value, R, EvalRight),
    Expr =.. [Op, EvalLeft, EvalRight],
    Eval is Expr.
