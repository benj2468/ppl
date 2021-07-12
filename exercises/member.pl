% member/1

member(X, [X | _]).
member(X, [_ | L]) :- member(X, L).

:- member(X, [6,5,4,3,6,7,8]).