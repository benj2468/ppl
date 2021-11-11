pair([0], _).
pair([0, 1 | Rem], over) :- !, fail.
pair([0, 0 | Rem], over) :- pair([0 | Rem], over), !.
pair([0, 0 | Rem], blank) :- pair([0 | Rem], blank), !.
pair([0, 1 | Rem], blank) :- pair([1 | Rem], start), !.
pair([1, 1 | Rem], start) :- pair([1 | Rem], start), !.
pair([1, 1 | Rem], blank) :- pair([1 | Rem], start), !.
pair([1, 0 | Rem], start) :- pair([0 | Rem], over), !.


check_connectedness(List) :-
    pair(List, blank).