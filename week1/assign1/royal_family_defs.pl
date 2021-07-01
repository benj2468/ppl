/*  
    File: royal_family_defs.pl
    Purpose: Assignment 1
    Author: Benjamin Cape
*/

:- [royal_family_facts].

% X is the father of Y
father(X, Y) :- husband(X, M), mother(M, Y).

% X is the child of Y
child(X, Y) :- mother(Y, X).
child(X, Y) :- father(Y, X).

% X is the grandparent of Y
grandparent(X, Y) :- child(Y, Z), child(Z, X).

% X is the sibling of Y
sibling(X, Y) :- mother(M, X), mother(M, Y), not(X = Y), father(F, X), father(F, Y).

% X is the brother of Y
brother(X, Y) :- sibling(X, Y), male(X).

% X is the sister of Y
sister(X, Y) :- sibling(X, Y), female(X).

% X is the parent of Y
parent(X, Y) :- child(Y, X).

% X is the aunt of Y
aunt(X, Y) :- sister(X, P), parent(P, Y).
aunt(X, Y) :- brother(B, P), parent(P, Y), husband(B, X).

% X is the uncle of Y
uncle(X, Y) :- brother(X, P), parent(P, Y).
uncle(X, Y) :- sister(S, P), parent(P, Y), husband(X, S).

% X is the cousin of Y
cousin(X, Y) :- child(X, P), uncle(P, Y).
cousin(X, Y) :- child(X, P), aunt(P, Y).

% X is the nephew of Y
nephew(X, Y) :- aunt(Y, X), male(X).
nephew(X, Y) :- uncle(Y, X), male(X).

:- brother(maurits, floris).
:- brother(bernhard_jr, floris).
:- brother(pieter_christiaan, floris).

% ?- brother(X, floris).
% X = maurits ;
% X = bernhard_jr ;
% X = pieter_christiaan ;
% All of the people above are brother's of floris

% ?- sister(X, floris).
% false
% Floris has not sisters

:- not(sister(_, floris)).

% ?- sister(X, beatrix).
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = irene ;
% X = christina ;
% Margriet is listed 5 times because she is considered a female in 5 ways.

:- sister(margriet, beatrix).
:- sister(irene, beatrix).
:- sister(christina, beatrix).

% ?- parent(X, floris).
% X = margriet ;
% X = pieter ;
% Margiet and Pieter are FLoris' parents

:- parent(pieter, floris).
:- parent(margriet, floris).

% ?- parent(floris, X).
% false.
% Floris does not have any children, and therefore is not a parent to anyone.

:- not(parent(floris, _)).

% ?- aunt(X, friso).
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = margriet ;
% X = irene ;
% X = christina ;
% We get margriet 5 different times becuase margriet is both a mother to 4 (defining her as a female in 4 ways), and a husband to 1, defining her as a female in a 5th way
% the fifth 

:- aunt(margriet, friso).
:- aunt(irene, friso).
:- aunt(christina, friso).

% ?- uncle(X, floris).
% X = claus ;
% X = claus ;
% X = claus ;
% X = claus ;
% Repitition occurs becuase claus is connected to floris through beatrix, who is a female in 4 different ways (female is a requirement when finding a sister)

:- uncle(claus, floris).

% ?- nephew(X, margriet).
% X = alexander ;
% X = friso ;
% X = constantijn ;
% X = alexander ;
% X = friso ;
% X = constantijn ;
% X = alexander ;
% X = friso ;
% X = constantijn ;
% X = alexander ;
% X = friso ;
% X = constantijn ;
% X = alexander ;
% X = friso ;
% X = constantijn ;
% Again we get duplicates here becuase, 5 duplicates for each one specifically, becuase margriet, required to be female in this predicate, is defined as female in 5 different way

:- nephew(constantijn, margriet).
:- nephew(friso, margriet).
:- nephew(alexander, margriet).


% ?- cousin(X, floris).
% X = alexander ;
% X = alexander ;
% X = alexander ;
% X = alexander ;
% X = friso ;
% X = friso ;
% X = friso ;
% X = friso ;
% X = constantijn ;
% X = constantijn ;
% X = constantijn ;
% X = constantijn ;
% Again, here we get duplicates becuase beatrix, all of these children's mother, is a female is 4 ways

:- cousin(alexander, floris).
:- cousin(friso, floris).
:- cousin(constantijn, floris).

% X is an ancestor of Y
% parent(juliana, juliana).
% We can make this loop forever by making someone there own parent.
ancestor(X, Y) :- parent(X, Y), not(X = Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y), not(X = Y).

% ?- ancestor(X, margriet).
% X = juliana ;
% X = bernhard ;
% X = emma ;
% X = wilhelmina ;

:- ancestor(juliana, margriet).
:- ancestor(bernhard, margriet).
:- ancestor(emma, margriet).
:- ancestor(wilhelmina, margriet).

% ?- ancestor(floris, X).
% false.

:- not(ancestor(floris, _)).