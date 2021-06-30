
% Monkey(atdoor, atwindow, middle)
% Pos(onfloor, onbox)
% Box(atdoor, atwindow, middle)
% Has(has, hasnot)

goal(State) :- State = state(_, _, _, has).

init(State) :- State = state(atdoor, onfloor, atwindow, hasnot).

% init - atdoor, onfloor, atwindow, hasnot
% walk - atwindow, onfloor, atwindow, hasnot
% push - atwindow, onfloor, middle, hasnot
% walk - middle, onfloor, middle, hasnot
% climb - middle, onbox, middle, hasnot
% grasp - middle, onbox, middle, has

% Locations
% We use these to validate certain walks/pushes
location(atwindow).
location(middle).
location(atdoor).

% Actions
% These particular actions need special validation.
walk(L1, L2) :- location(L1), location(L2), not(L1 = L2).
push(L1, L2) :- location(L2), location(L2), not(L1 = L2).

% % climb: If the monkey and the box are in the same location, and the monkey is on the floor,
% % it can climb on top of the box. If the monkey is on the box, it can climb down to the floor.
% We should ONLY climb if we are in the middle, and about to get the banana.
move(
    state(middle, onfloor, middle, Has), 
    climb, 
    state(middle, onbox, middle, Has)).

% grasp: If the monkey and the box are in the middle and the monkey is on the box without
% a banana, it can grasp for the banana and get it.
move(
    state(middle, onbox, middle, hasnot), 
    grasp, 
    state(middle, onbox, middle, has)).

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


solve(State) :- goal(State).
solve(State) :- move(State, _, State2), solve(State2).

solve(State, L) :- goal(State), L = [].
solve(State, L) :- move(State, M, State2), append(L2, [M], L), solve(State2, L2).