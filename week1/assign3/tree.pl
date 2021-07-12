/*  
    File: tree.pl
    Purpose: Assignment 3
    Author: Benjamin Cape
*/


istree(nil).
istree(t(L, _, R)) :-istree(L), istree(R).

max(t(_, N, nil),N).
max(t(_, _, R),N) :- max(R, N).

min(t(nil, N, _), N).
min(t(L, _, _), N) :- min(L, N).

issorted(nil).
issorted(t(nil, _, nil)).
issorted(t(L, N, nil)) :-
    issorted(L),
    max(L, N1),
    N1 =< N.
issorted(t(nil, N, R)) :-
    issorted(R),
    min(R, N2),
    N =< N2.
issorted(t(L, N, R)) :- 
    issorted(L),
    issorted(R),
    max(L, N1),
  	min(R, N2),
	N1 =< N,
	N =< N2.


find(t(L, N, R), N, t(L, N, R)).
find(t(L, NotS, _), N, S) :- 
    not(NotS = N),
    find(L, N, S).
find(t(_, NotS, R), N, S) :- 
    not(NotS = N),
    find(R, N, S).

% What should happen if the value already exists?
insert(nil, N, t(nil, N, nil)).
insert(t(L, V, R), N, S) :-
    (N =< V -> 
    	insert(L, N, SL),
        S = t(SL, V, R)
    ;
    	insert(R, N, SR),
    	S = t(L, V, SR)).

insertTree(T, nil, T).
insertTree(nil, T, T).  
insertTree(t(L, V, R), I, S) :-
    I = t(_, IV, _),
    (IV =< V ->
    	insertTree(L, I, New),
        S = t(New, V, R)
    ;   
    	insertTree(R, I, New),
        S = t(L, V, New)).

deleteOne(t(L, N, nil), N, L).
deleteOne(t(L, N, t(RL, RN, RR)), N, S) :-
    insertTree(t(L, RN, RR), RL, S).
deleteOne(t(L, V, R), N, S) :-
    (N < V -> 
    	deleteOne(L, N, SL),
        S = t(SL, V, R)
    ;
    	deleteOne(R, N, SR),
    	S = t(L, V, SR)).


deleteAll(nil, _, nil).
deleteAll(T, N, S) :-
    T = t(_, N, _),
    deleteOne(T, N, OneRemoved),
    deleteAll(OneRemoved, N, S).
deleteAll(t(L, V, R), N, t(SL, V, SR)) :-
    deleteAll(L, N, SL),
    deleteAll(R, N, SR).


listtree([N], t(nil, N, nil)).
listtree([Head | Tail],T) :-
    listtree(Tail, Recursive),
    insert(Recursive, Head, T).

treelist(nil,[]).
treelist(t(L, N, R),Res) :-
    deleteAll(L, N, LC),
    deleteAll(R, N, RC),
    treelist(LC, LL),
    treelist(RC, RL),
    append(LL, [N | RL], Res).

treelistAll(nil, []).
treelistAll(t(L, N, R), Res) :-
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
:- treesort([9,8,7,6,5,4,3,2,2], [2, 3, 4, 5, 6, 7, 8, 9]).
:- treesort([6,5,3,8,5,5,3,2,7,6,5,9], [2, 3, 5, 6, 7, 8, 9]).
% 
% % The treesortAll maintains duplicates
:- treesortAll([9,8,7,6,5,4,3,2,2], [2, 2, 3, 4, 5, 6, 7, 8, 9]).

% treesort doesn't retain duplicates becuase treelist(T, L) uses deleteAll when we remove the root node and add it to the list
% treesortAll retains multiples becuase it only removes the root node, not the remaining nodes. 
