istree(nil).
istree(T) :- T = t(L, _, R), istree(L), istree(R).

max(T,N) :- T = t(_, N, nil).
max(T,N) :- T = t(_, _, R), max(R, N).

min(T, N) :- T = t(nil, N, _).
min(T, N) :- T = t(L, _, _), min(L, N).

issorted(nil).
issorted(t(nil, _, nil)).
issorted(T) :-
    T = t(L, N, nil),
    issorted(L),
    max(L, N1),
    N1 =< N.
issorted(T) :-
    T = t(nil, N, R),
    issorted(R),
    min(R, N2),
    N =< N2.
issorted(T) :- 
    T = t(L, N, R),
    issorted(L),
    issorted(R),
    max(L, N1),
  	min(R, N2),
	N1 =< N,
	N =< N2.


find(T, N, S) :- T = t(_, N, _), S = T.
find(T, N, S) :- 
    T = t(L, NotS, _),
    not(NotS = N),
    find(L, N, S).
find(T, N, S) :- 
    T = t(_, NotS, R),
    not(NotS = N),
    find(R, N, S).

% What should happen if the value already exists?
insert(nil, N, S) :- S = t(nil, N, nil).
insert(T, N, S) :-
    T = t(L, V, R),
    (N =< V -> 
    	insert(L, N, SL),
        S = t(SL, V, R)
    ;
    	insert(R, N, SR),
    	S = t(L, V, SR)).

insertTree(T, nil, S) :- S = T.
insertTree(nil, T, S) :- S = T.
insertTree(T, I, S) :-
	T = t(L, V, R),
    I = t(_, IV, _),
    (IV =< V ->
    	insertTree(L, I, New),
        S = t(New, V, R)
    ;   
    	insertTree(R, I, New),
        S = t(L, V, New)).

deleteOne(T, N, S) :-
    T = t(L, N, nil),
   	S = L.
deleteOne(T, N, S) :-
    T = t(L, N, R),
   	R = t(RL, RN, RR),
    NewTree = t(L, RN, RR),
    insertTree(NewTree, RL, Res),
    S = Res.

deleteOne(T, N, S) :-
    T = t(L, V, R),
    (N < V -> 
    	deleteOne(L, N, SL),
        S = t(SL, V, R)
    ;
    	deleteOne(R, N, SR),
    	S = t(L, V, SR)).


deleteAll(T, N, S) :-
    T = t(_, N, _),
    deleteOne(T, N, OneRemoved),
    deleteAll(OneRemoved, N, S).
deleteAll(nil, _, S) :- S = nil.
deleteAll(T, N, S) :-
    T = t(L, V, R),
    not(V = N),
    deleteAll(L, N, SL),
    deleteAll(R, N, SR),
	S = t(SL, V, SR).

listtree([N], T) :-
    T = t(nil, N, nil).
listtree(L,T) :-
	L = [Head | Tail],
    listtree(Tail, Recursive),
    insert(Recursive, Head, T).

treelist(nil,L) :- L = [].
treelist(T,L) :-
    T = t(_, N, _),
    deleteAll(T, N, S),
    treelist(S, Temp),
    append(Temp, [N], L).

treelistAll(nil, L) :- L = [].
treelistAll(T, L) :-
    T = t(_, N, _),
    deleteOne(T, N, S),
    treelistAll(S, Temp),
    append(Temp, [N], L).

% This function translates T1 into T2, removing duplicates and creating a tree of unique nodes
treesort(T1, T2) :-
    treelist(T1, L),
    listtree(L, T2).

% This function translates T1 into T2, maintaining duplicates
treesortAll(T1, T2) :-
    treelistAll(T1, L),
    listtree(L, T2).

% Extra

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

partition(List, L, M, R) :-
	append(L, [M], Left),
    append(Left, R, List),
    length(L, LL),
    length(R, LR),
    AbsDiff is abs(LL - LR),
    AbsDiff =< 1.
    

listtree_balanced([],T) :- T = nil.
listtree_balanced(L, T) :-
	sort(L, Sorted), % O(nlgn)
    partition(Sorted, PL, M, PR),
	listtree_balanced(PL, TL),
    listtree_balanced(PR, TR),
    T = t(TL, M, TR).
    

balanced_treesort(T, T1) :-
    treelistAll(T, L),
    listtree_balanced(L, T1).    


    
    


