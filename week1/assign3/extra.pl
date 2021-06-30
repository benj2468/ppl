:- [tree].


height(nil, H) :- H = 0.
height(T, H) :-
    T = t(L, _, R),
    height(L, HL),
    height(R, HR),
    H is max(HL, HR) + 1.

isbalanced(nil).
isbalanced(T) :-
    T = t(L, _, R),
    isbalanced(L),
    isbalanced(R),
    height(L, HL),
    height(R, HR),
    AbsDiff is abs(HL - HR),
    AbsDiff =< 1.


rotateRight(T, Rotated) :-
    T = t(L, N, R),
    max(L, MaxL),
    deleteOne(L, MaxL, NewL),
    NewR = t(nil, N, R),
    Rotated = t(NewL, MaxL, NewR).

rotateLeft(T, Rotated) :-
    T = t(L, N, R),
    min(R, MinR),
    deleteOne(R, MinR, NewR),
    NewL = t(L, N, nil),
    Rotated = t(NewL, MinR, NewR).

balance(T,B) :- isbalanced(T), B = T.
balance(UB, B) :-
    UB = t(L, _, R),
    height(L, HL),
    height(R, HR),
    (HL >= HR -> rotateRight(UB, Rotated); rotateLeft(UB, Rotated)),
    print(Rotated),
    Rotated = t(RotL, N, RotR),
    balance(RotL, BL),
    balance(RotR, BR),
    B = t(BL, N, BR).
        

listtree_balanced([], T) :- T = nil.
listtree_balanced(L, T) :-
    L = [Head | Tail],
    listtree_balanced(Tail, Subtree),
    insert(Subtree, Head, UnBal),
	(isbalanced(UnBal) ->
    	T = UnBal;
    	balance(UnBal, T)).

balanced_treesort(L, L1) :-
    listtree_balanced(L, T),
    treelistAll(T, L1).