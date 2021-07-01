/*  
    File: tree.pl
    Purpose: Assignment 3
    Author: Benjamin Cape
*/


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
treelist(T,Res) :-
    T = t(L, N, R),
    deleteAll(L, N, LC),
    deleteAll(R, N, RC),
    treelist(LC, LL),
    treelist(RC, RL),
    append(LL, [N | RL], Res).

treelistAll(nil, L) :- L = [].
treelistAll(T, Res) :-
    T = t(L, N, R),
    treelist(L, LL),
    treelist(R, RL),
    append(LL, [N | RL], Res).

% This function translates L1 into L2, removing duplicates and creating a tree of unique nodes
treesort(L1, L2) :-
    listtree(L1, T),
    treelist(T, L2).

% This function translates L1 into L2, maintaining duplicates
treesortAll(L1, L2) :-
    listtree(L1, T),
    treelistAll(T, L2).

% TESTING
% % The treesort does not maintain duplicates
% treesort([9,8,7,6,5,4,3,2,2], L)
% L = [2, 3, 4, 5, 6, 7, 8, 9]
% ?- treesort([6,5,3,8,5,5,3,2,7,6,5,9], L).
% L = [2, 3, 5, 6, 7, 8, 9] ;
% 
% % The treesortAll maintains duplicates
% treesortAll([9,8,7,6,5,4,3,2,2], L)
% L = [2, 2, 3, 4, 5, 6, 7, 8, 9]

% treesort doesn't retain duplicates becuase treelist(T, L) uses deleteAll when we remove the root node and add it to the list
% treesortAll retains multiples becuase it only removes the root node, not the remaining nodes. 
