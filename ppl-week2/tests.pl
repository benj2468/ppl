%% You can use this file by including it as follows:
%%
%% :- [tests]
%% 

%% This file will provide you with some test puzzles,
%% as well as some test solutions, and a print function.

%% There are two print functions: print_only_grid/1 and print_puzzle/3
%% You can also call them via helper functions:
%%
%% To see what a puzzle looks like, type at the ?-
%% showPuzzle(p2x2).
%% 
%% Or you can see all puzzles, by typing at the ?-
%% showPuzzle(P).

%% Make sure you define 'snake.pl'.
%% If you have, to solve a puzzle and see the result, say p2x2:
%% solvePuzzle(p2x2).
%% 
%% You should check it accepts correct solutions quickly:
%% checkCorrect(P).
%% This should work even if your solver is slow!
%% 
%% Most puzzles should always yield exactly one solution without ?.
%% There are some puzzles with multiple solutions (indicated as such).
%%
%% Don't forget to make sure every distinct solution is given exactly once.
%%
%% 

solvePuzzle(P)
  :- puzzle(P,RowClues,ColClues,Grid)
   , snake(RowClues,ColClues,Grid,Solution) %% You define this !
   , print_only_grid(Solution)
   .

showPuzzle(P)
  :- puzzle(P,RowClues,ColClues,Grid)
   , print_puzzle(RowClues,ColClues,Grid).

%% Make sure that your solver does not mark correct solutions as incorrect.
%% These checks should be instant!
checkCorrect(P)
  :- solution(P,RowClues,ColClues,Grid)
   , snake(RowClues,ColClues,Grid,Grid).
  
% Some puzzles in order of increasing difficulty..

puzzle(test,[-1,-1,-1],
  [-1,-1,-1],
 [[ 1, 2, 1]
 ,[ 2, 0, 2]
 ,[ 2, -1, 2]]).

puzzle(p2x2,[-1,-1],
 [ -1,-1],
[[ 1, 1]
,[-1,-1]]).

puzzle(p3x3,[ 1,-1,-1],
 [ 2,-1,-1],
[[-1,-1, 1]
,[-1,-1,-1]
,[ 1,-1,-1]]).

puzzle(p3x3b,[-1,-1,-1],
 [-1, 2,-1],
[[-1, 1,-1]
,[ 1,-1,-1]
,[-1,-1,-1]]).

% Two solutions..
puzzle(p5x5_two,[3,1,5,1,3],
 [3,2,3,2,3],
[[-1,-1,1,-1,-1]
,[-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1]
,[-1,-1,1,-1,-1]]).

% Unique solution, but two solutions if no cycle detection was built
puzzle(pCycle,[-1,-1,-1,-1,3],
 [-1,-1,-1],
[[ 1,-1, 1]
,[-1,-1,-1]
,[-1,-1,-1]
,[-1,-1,-1]
,[-1,-1,-1]]).

puzzle(p4x4,[-1,-1,2,-1],
 [-1,-1,-1, 3],
[[-1, 1,-1,-1]
,[-1,-1,-1,-1]
,[ 1,-1,-1,-1]
,[-1,-1,-1,-1]]).

puzzle(p5x5,[-1,-1,-1,-1, 5],
 [-1, 2, 4,-1,-1],
[[-1, 1,-1,-1,-1]
,[-1,-1,-1,-1,-1]
,[ 1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1]]).

puzzle(p7x7,[ 4, 5, 3,-1,-1,-1, 3],
 [ 3,-1,-1,-1, 3, 5, 4],
[[-1, 1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1, 1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1]]).

puzzle(p10x10,[-1, 3, -1, 5, 4, 7, -1, 9, -1, -1],
 [-1,-1, 2,-1,-1,-1, 6,-1, 8,-1],
[[-1,-1,-1,-1,-1,-1,-1,-1,-1, 1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1, 1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
,[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]]
).

puzzle(p12x12,[-1, 3, 7,-1, -1,-1, -1, -1, -1,2,3,4],
  [ 8, 5, 2, 6, 4, 5, 6, 4, 5,-1, 7],
[ [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1, 1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
, [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]]
).

solution(p4x4,[-1,-1,2,-1],[ -1,-1,-1,3],[[0, 1, 2, 0], [0, 0, 2, 2], [1, 0, 0, 2], [2, 2, 2, 2]]).

solution(p5x5,[-1, -1, 3, -1, -1],[-1, 4, -1, -1, 3],
 [ [0, 1, 2, 2, 0]
 , [0, 0, 0, 2, 2]
 , [1, 2, 0, 0, 2]
 , [0, 2, 0, 2, 2]
 , [0, 2, 2, 2, 0]]).

solution(p7x7,[4, 5, 3, -1, -1, -1, 3],[3, -1, -1, -1, 3, 5, 4],
[ [0, 1, 0, 0, 2, 2, 2]
, [2, 2, 0, 2, 2, 0, 2]
, [2, 0, 0, 2, 0, 0, 2]
, [2, 2, 2, 2, 0, 2, 2]
, [0, 0, 0, 0, 0, 2, 0]
, [0, 0, 0, 1, 0, 2, 0]
, [0, 0, 0, 2, 2, 2, 0]]
).

solution(p10x10,[-1, 3, -1, 5, 4, 7, -1, 9, -1, -1],
  [-1,-1, 2,-1,-1,-1, 6,-1, 8,-1],
[ [ 2, 2, 2, 2, 2, 2, 2, 0, 2, 1]
, [ 2, 0, 0, 0, 0, 0, 2, 0, 2, 0]
, [ 2, 0, 0, 2, 2, 2, 2, 0, 2, 0]
, [ 2, 2, 0, 2, 0, 0, 0, 0, 2, 2]
, [ 0, 2, 0, 2, 0, 1, 0, 0, 0, 2]
, [ 2, 2, 0, 2, 2, 2, 0, 0, 2, 2]
, [ 2, 0, 0, 0, 0, 0, 0, 0, 2, 0]
, [ 2, 2, 2, 2, 2, 2, 2, 0, 2, 2]
, [ 0, 0, 0, 0, 0, 0, 2, 0, 0, 2]
, [ 0, 0, 0, 0, 0, 0, 2, 2, 2, 2]]
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%  Printing  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Only print the grid (without the clues)
print_only_grid([]) :- nl.
print_only_grid([GridH|GridT]) :- print_row(GridH), nl, print_only_grid(GridT).


% Printing function for the puzzle (starting with empty part in top-left corner)
print_puzzle(RowClues,ColClues,Grid) :- write('     '), print_colclues(RowClues,ColClues,ColClues,Grid).
% Print the column clues
print_colclues(RowClues,[], ColClues,Grid) :- nl, write('     '), print_colline(RowClues,ColClues,Grid).
print_colclues(RowClues,[ColH|ColT], ColClues,Grid) :- print_clue(ColH), print_colclues(RowClues,ColT,ColClues,Grid).
% Print a column line
print_colline(RowClues,[],Grid) :- nl, print_grid(RowClues,[],Grid).
print_colline(RowClues,[_|ColT],Grid) :- write('---'), print_colline(RowClues,ColT,Grid).
% Print the grid itself (with the row clues)
print_grid([],[],[]) :- nl.
print_grid([RowH|RowT],[],[GridH|GridT]) :- print_clue(RowH), write('| '), print_row(GridH), nl, print_grid(RowT,[],GridT).
% Print row
print_row([]).
print_row([H|T]) :- print_cell(H), print_row(T).
% Print cell
print_cell(-1) :- write(' ? ').
print_cell(0) :- write(' - ').
print_cell(2) :- write('[#]').
print_cell(1) :- write('[S]').
% Print clue (assuming one-digit clues)
print_clue(-1) :- write('   ').
print_clue(0) :- write(' 0 ').
print_clue(1) :- write(' 1 ').
print_clue(2) :- write(' 2 ').
print_clue(3) :- write(' 3 ').
print_clue(4) :- write(' 4 ').
print_clue(5) :- write(' 5 ').
print_clue(6) :- write(' 6 ').
print_clue(7) :- write(' 7 ').
print_clue(8) :- write(' 8 ').
print_clue(9) :- write(' 9 ').
