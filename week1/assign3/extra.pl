:- [tree].


height(nil, 0).
height(t(L, _, R), H) :-
    height(L, HL),
    height(R, HR),
    H is max(HL, HR) + 1.

isbalanced(nil).
isbalanced(t(L, _, R)) :-
    isbalanced(L),
    isbalanced(R),
    height(L, HL),
    height(R, HR),
    AbsDiff is abs(HL - HR),
    AbsDiff =< 1.


rotateRight(t(L, N, R), t(NewL, MaxL, t(nil, N, R))) :-
    max(L, MaxL),
    deleteOne(L, MaxL, NewL).

rotateLeft(t(L, N, R), t(t(L, N, nil), MinR, NewR)) :-
    min(R, MinR),
    deleteOne(R, MinR, NewR).

balance(T,T) :- isbalanced(T).
balance(UB, t(BL, N, BR)) :-
    UB = t(L, _, R),
    height(L, HL),
    height(R, HR),
    (HL >= HR -> 
        rotateRight(UB, t(RotL, N, RotR)); 
        rotateLeft(UB, t(RotL, N, RotR))),
    balance(RotL, BL),
    balance(RotR, BR).
        

listtree_balanced([], nil).
listtree_balanced([Head | Tail], T) :-
    listtree_balanced(Tail, Subtree),
    insert(Subtree, Head, UnBal),
	(isbalanced(UnBal) ->
    	T = UnBal;
    	balance(UnBal, T)).

balanced_treesort(L, L1) :-
    listtree_balanced(L, T),
    treelistAll(T, L1).

% Tests

:- balanced_treesort([9,8,7,6,5,4,3,2,1], [1,2,3,4,5,6,7,8,9]).
:- balanced_treesort([9,8,7,6,5,4,3,5], [3,4,5,5,6,7,8,9]).