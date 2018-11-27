% COMP9814 Assignment3
% Option 2: Prolog (BDI Agent)
% Group224: Yichen Zhang(z5124474) Yizheng Ying (z5141180)



% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.
solve(Start, Solution, G, N)  :-
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

solve(state(X,Y), _, G, _)  :-
    agent_at(X0, Y0),
    X =:= X0,
    Y =:= Y0,
    G is 0.

solve(goal(X,Y), _, G, _)  :-
    agent_at(X0, Y0),
    X =:= X0,
    Y =:= Y0,
    G is 0.

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).



%_____________________________________________________________
% Q1 initial_intentions(Intentions)
% bind Intentions to intens(L,[]),L is the list whose elemnts are like 
% [goal(X1,Y1),Plan] Plan=[]

% step_1: find a path by ucsdijkstra
% cost=1 when agent start from a land node to a land node
% First, determine whether the coordinates are land(X,Y) or dropped(X,Y)
s(state(X1,Y1),state(X2,Y2),1):-
	land_or_dropped(X2,Y2),
	mandist(X1/Y1,X2/Y2,1).

% cost=1000 when agent move from a land node to a water node
% the coordinates are not land(X,Y) or dropped(X,Y)
s(state(X1,Y1),state(X2,Y2),1000):-
	between(1,10,X2),
	between(1,10,Y2),
	not(land_or_dropped(X2,Y2)),
	mandist(X1/Y1,X2/Y2,1).

% Find_path : Base case -- the agent move to the gridworld from (1,1)
% check each node in the path
% If the node is a water node, add it into the intents[L,[]]
% If the node is a land node, continue 
% Base case

%initial([]).
%goal(state(9,9)).
% use agent_at(X,Y) to get the location of start point and use monster(X,Y)to get the goal
% use solve() to get the path and use reverse to change the path to the order we need
initial_intentions(intents(L,[])):-
	agent_at(X0,Y0),
	monster(X1,Y1),
    retractall(goal(_)),
	assert(goal(state(X1,Y1))),
	solve(state(X0,Y0),Path,_,_),
	newlist(Path,Water),
	reverse(Water,L).

% Base case
newlist([],[]).

% The condition when this point is land or dropped
newlist([state(X,Y)|Tail],Water):-
	land_or_dropped(X,Y),
	newlist(Tail,Water).

% The condition when this point is not land or dropped
newlist([state(X,Y)|Tail],[[goal(X,Y),[]]|Water]):-
	not(land_or_dropped(X,Y)),
	newlist(Tail,Water).






	

% code from A2 -- Calculate the Manhattan distance
mandist(X/Y, X1/Y1, D) :- % D is Manhattan Dist between two positions
    diff(X, X1, Dx),
    diff(Y, Y1, Dy),
    D is Dx + Dy.

diff(A, B, D) :-                % D is |A-B|
    D is A-B, D >= 0, !.

diff(A, B, D) :-                % D is |A-B|
    D is B-A.


% ___________________________________________________________
% Q2 trigger(Percepts,Goals)
% which takes a list of percepts, each of the form stone(X,Y), and 
% converts it into a corresponding list of goals, each of the form 
% goal(X,Y).

% Base case: Empty Percepts -> Empty Goals
trigger([],[]).

% If there is a stone(X,Y) in the next percept,add it into the Goals[]
trigger([stone(X,Y)|OthersPre],[goal(X,Y)|OtherGoals]):-
	trigger(OthersPre,OtherGoals).




% ___________________________________________________________
% Q3 incorporate_goals(Goals,Intentions,Intentions1)
% Input:  A set of Goals,the current Intentions of the agent
% Return: The updated Intentions of the agent after inserting the new goals into Int_pick

% No goals, nothing to update
% Base case
incorporate_goals([], Intentions, Intentions).

% If goal(X,Y)  already in Int_pick, do not add it.
incorporate_goals([G|Restgoals], intents(Int_drp,Int_pick), intents(Int_drp,Int_pick_1)):-
    member([G,_], Int_pick),
    incorporate_goals(Restgoals, intents(Int_drp,Int_pick), intents(Int_drp,Int_pick_1)).
    
% If goal(X,Y)  not in Int_pick,but it is not reachable, do not add it.
incorporate_goals([G|Restgoals], intents(Int_drp,Int_pick), intents(Int_drp,Int_pick_1)):-
    not(member([G,_], Int_pick)),
    retractall(goal(_)),
    goal(X,Y)=G,
    not(stone_reachable(X,Y)),
    incorporate_goals(Restgoals, intents(Int_drp,Int_pick), intents(Int_drp,Int_pick_1)).
    
% If Goal G not in Int_pick, then add it.
incorporate_goals([G|Restgoals], intents(Int_drp,Int_pick), intents(Int_drp,Int_pick_1)):-
    not(member([G,_], Int_pick)),
    retractall(goal(_)),
    goal(X,Y)=G,
    stone_reachable(X,Y),
    insert(G, Int_pick, Int_pick_new),
    incorporate_goals(Restgoals, intents(Int_drp,Int_pick_new), intents(Int_drp,Int_pick_1)).


% insert([goal(X,Y),[]], +Intentions, -IntentionsNew).
% The new goal are inserted into the existing list in decreasing order 
% of length of the shortest distance from the agent's current position. And the path should be valid.

% Base case ,there is no goals in Int_pick.
%insert(G, [], [[G,[]]]).
insert(G, [], [[G,[]]]).

% If there are some goals in the Int_pick, then find a place to insert the current goal(X,Y).
insert(G, Int_pick, [[G,[]]|Int_pick]):-
	[[goal(X1,Y1),_]|_]=Int_pick,
	goal(X,Y)=G,
	agent_at(X0,Y0),
	retractall(goal(_)),
	assert(goal(state(X0,Y0))),
	solve(state(X,Y),_,G1,_),
	solve(state(X1,Y1),_,G2,_),
	% if G1 < G2 ,insert goal(X,Y) immediately
	G1<G2.


% if G2<G1, than compare goal(X,Y) with next goal in the Int_pick
insert(G, [[goal(X1,Y1),_]|Tail], [[goal(X1,Y1),[]]|Int_pick_1]):-
	goal(X,Y)=G,
	agent_at(X0,Y0),
	retractall(goal(_)),
	assert(goal(state(X0,Y0))),
	solve(state(X,Y),_,G1,_),
	solve(state(X1,Y1),_,G2,_),
	G1>=G2,
	insert(G,Tail,Int_pick_1).


% to check whether can get to the stone
stone_reachable(X,Y):-
	valid_pt(X, Y, _).

valid_pt(X1, Y1, L):-
	retractall(goal(_)),
	agent_at(X0, Y0),
	assert(goal(state(X0,Y0))),
	Start1=state(X1,Y1),
	solve(Start1,Path,_,_),
	newlist(Path,Water),
	reverse(Water,L),
	length(L,K),
    K =:= 0.


%Q4
% when the agent get a stone, then try to use the stone to drop to the goal in Int_drop

% find full path from current position to the destination.
findPath(X1, Y1, Path, W):-
    retractall(goal(_)),
    agent_at(X0, Y0),
    assert(goal(state(X0,Y0))),
    Start1=state(X1,Y1),
    solve(Start1,Path,_,_),
    newlist(Path, W).

% remove to get the new action
remove_get_newDrop([[G, ActionList]| Tail],[[G, NewList]| Tail]):-
    remove_start_position(ActionList, NewList).

% Base case
get_action(intents(Int_drop, []), intents(Int_drop, []), move(X,Y)):-
    agent_at(X,Y).

% generate drop path when have a stone
get_action(intents([[goal(X,Y), _]| Tail], [[goal(X1,Y1), _]| Tail1]), intents(NeededDropList, [[goal(X1,Y1), []]| Tail1]), Action):-
    agent_stones(1),
    new_plan([[goal(X,Y), _]| Tail], NewDropList, drop(X,Y), X, Y, Action),
    remove_get_newDrop(NewDropList, NeededDropList).

% retarded alert
get_action(intents([[goal(X1,Y1), _]| Tail1], [[goal(X,Y), _]| Tail]), intents([[goal(X1,Y1), []]| Tail1],  [[goal(X,Y), []]| Tail]), pick(X,Y)):-
    agent_stones(0),
    agent_at(X0, Y0),
    X =:= X0,
    Y =:= Y0.

% generate pick path when don't have a stone
get_action(intents([[goal(X1,Y1), _]| Tail1], [[goal(X,Y), _]| Tail]), intents([[goal(X1,Y1), []]| Tail1], NeededPickList), Action):-
    agent_stones(0),
    new_plan([[goal(X,Y), _]| Tail], NewPickList, pick(X,Y), X, Y, Action),
    remove_get_newDrop(NewPickList, NeededPickList).

% remove the start point
remove_start_position([_|Tail], Tail).

% get plan lists.
new_plan([[goal(X,Y), _]| Tail], [[goal(X,Y), [FirstMove | RestMove]]| Tail], LastAction, X, Y, FirstMove):-
    findPath(X, Y, L, W),
    length(W, WL),
    WL =< 1,
    remove_start_position(L, NewPath),
    generate_path(LastAction, NewPath, [FirstMove | RestMove]).

% empty
new_plan([[goal(X,Y), _]| Tail], [[goal(X,Y), []]| Tail], X, Y, []):-
    findPath(X, Y, _, W),
    length(W , WL),
    WL > 1.

% generate movement list
generate_path(LastAction, L, [LastAction]):-
    length(L, K),
    K =:= 1.

generate_path(LastAction, [state(X,Y)| Tail], [move(X,Y)| MoveList]):-
    length([state(X,Y)| Tail], K),
    K > 1,
    generate_path(LastAction, Tail, MoveList).


%% Task 5 - update_intentions
% update_intentions(Observation, Intentions, Intentions1)
% to update the agent's intentions, based on observation. 
% An at(X,Y) observation does not change the agent's intentions. 
% picked() or dropped() observation, the agent removes the corresponding intention from its list of intentions

update_intentions(at(_,_), Intentions, Intentions).
update_intentions(picked(_,_), intents(Int_drop,[_|Rest]), intents(Int_drop,Rest)).
update_intentions(dropped(_,_), intents([_|Rest],Int_pick), intents(Rest,Int_pick)).