/*  
    File: monkey.pl
    Purpose: Assignment 2
    Author: Benjamin Cape
*/

goal(State) :- State = state(_, _, _, has).

init(State) :- State = state(atdoor, onfloor, atwindow, hasnot).

% This is the ideal wining move set
% init - atdoor, onfloor, atwindow, hasnot
% walk - atwindow, onfloor, atwindow, hasnot
% push - atwindow, onfloor, middle, hasnot
% walk - middle, onfloor, middle, hasnot
% climb - middle, onbox, middle, hasnot
% grasp - middle, onbox, middle, has

% Locations
% We use these to validate certain walks/pushes
% There are in a particular order, becuase we want to  prefer moving to the middle over moving anywhere else
% If we change the order to atdoor, middle, atwindow, then we enter into an infinite loop becuase we keep moving back to the door and never read the window (which is the same location as the monkey)
location(middle).
location(atwindow).
location(atdoor).

% Actions
% These particular actions need special validation.
walk(L1, L2) :- location(L1), location(L2), not(L1 = L2).
push(L1, L2) :- location(L2), location(L2), not(L1 = L2).

% grasp: If the monkey and the box are in the middle and the monkey is on the box without
% a banana, it can grasp for the banana and get it.
move(
    state(middle, onbox, middle, hasnot), 
    grasp, 
    state(middle, onbox, middle, has)).

% % climb: If the monkey and the box are in the same location, and the monkey is on the floor,
% % it can climb on top of the box. If the monkey is on the box, it can climb down to the floor.
% We should ONLY climb if we are in the middle, and about to get the banana.
move(
    state(middle, onfloor, middle, Has), 
    climb, 
    state(middle, onbox, middle, Has)).

% push(L1,L2) If the monkey and the box are in location L1 and the monkey is on the floor,
% it can move with the box to location L2.
move(
    state(L1, onfloor, L1, Has), 
    push(L1, L2), 
    state(L1, onfloor, L2, Has)) :- push(L1, L2).

% walk(L1,L2) If the monkey is in location L1 on the floor, it can walk to position L2
move(
    state(L1, onfloor, Pos, Has), 
    walk(L1, L2), 
    state(L2, onfloor, Pos, Has)) :- walk(L1, L2).

% Removed the other climb becuase we should never leave the box once we get there
% move(
%     state(Pos, onbox, Box, Has), 
%     climb, 
%     state(Pos, onfloor, Box, Has)).

% Order above is important.
% IF we can grasp, we always should, so that should come first.
% IF we cannot grasp, BUT we can move onto the box, in order to grasp, we should do that, becuase it will allow us to grasp on the next BFS level
% IF we are not in the middle and on the floor, then we need to move ourselves on the floor, so we should do a series of pushes and walks until we get to where we need to be.
% IT is important that we push first, becuase if we prefer to walk, we will just keep walking around forever, but pushing is a harder operation to come by, so it should be prefered. 


solve(State) :- goal(State).
solve(State) :- move(State, _, State2), solve(State2).

solve(State, L) :- goal(State), L = [].
solve(State, L) :- move(State, M, State2), append(L2, [M], L), solve(State2, L2).

:- init(S), solve(S).
:- init(S), solve(S, [grasp, climb, walk(atwindow, middle), push(atwindow, middle), walk(middle, atwindow), walk(atdoor, middle)]).