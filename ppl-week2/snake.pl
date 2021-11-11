:- [tests].
:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true). % setting to get useful errors sometimes

% Puzzle Representation
% puzzle(Name,RowHints,ColHints,Grid)

% Assignments
% Input: RowClues, ColClues, Grid
% Output: Solution
% snake(RowClues, ColClues, Grid, Solution)

is_segment(1).
is_segment(2).
not_segment(0).

% O(mn), m is columns n is rows
copy_grid([],[]).
copy_grid([Row|Grid],[RowS|Sol]) :- copy_row(Row,RowS), copy_grid(Grid,Sol).

copy_row([],[]).
copy_row([-1|Row],[_|Sol]) :- copy_row(Row,Sol).
copy_row([Clue|Row],[Clue|Sol]) :- Clue #>= 0, copy_row(Row,Sol).

% O(n) where n is size of list
segments_in_list([], 0).
segments_in_list([Seg | List], Count) :-
    is_segment(Seg), !,
    segments_in_list(List, SubCount),
    Count is SubCount + 1.
segments_in_list([Seg | List], Count) :-
    not_segment(Seg),
    segments_in_list(List, Count).
    

% O(n), where n is number of rows
check_row_hints([], []).
check_row_hints([_ | Graph], [-1 | Hints]) :- check_row_hints(Graph, Hints).
check_row_hints([Row | Graph], [Hint | Hints]) :- Hint #>= 0, segments_in_list(Row, Hint), check_row_hints(Graph, Hints).

% Idealy this is O(m) m is columns, but transpose might take O(mn)
check_col_hints(Grid, Hints) :-
    % I wonder what the runtime of this transpose function is?
    transpose(Grid, Transposed), !,
    check_row_hints(Transposed, Hints).


extend_grid_row(OldRow, NewRow) :- append([0 | OldRow], [0], NewRow).

extend_grid_rows([], []).
extend_grid_rows([Row | Grid], [NewRow | Remainder]) :-
    extend_grid_row(Row, NewRow),
    extend_grid_rows(Grid, Remainder).

pad_solution(OldGrid, NewGrid) :-
    transpose(OldGrid,TransGrid), !,
    extend_grid_rows(TransGrid,RowTransGrid), !,
    transpose(RowTransGrid,RowGrid), !,
    extend_grid_rows(RowGrid,NewGrid), !.

% Test
:- pad_solution([[-1, -1, -1], [-1, -1, -1]], [[0, 0, 0, 0, 0], [0, -1, -1, -1, 0], [0, -1, -1, -1, 0], [0, 0, 0, 0, 0]]).

count_cell(Cell, 1) :- is_segment(Cell).
count_cell(Cell, 0) :- not_segment(Cell).
 
check_neighbors_pattern(0, _, _, _, _).
check_neighbors_pattern(C, N, E, S, W) :- is_segment(C),
    count_cell(N, XN),
    count_cell(E, XE),
    count_cell(S, XS),
    count_cell(W, XW),
    C is XN + XE + XS + XW.

check_corner(Corner, _, _) :- not_segment(Corner).
check_corner(Corner, Edge1, Edge2) :- 
    is_segment(Corner),
    not_segment(Edge1),
    is_segment(Edge2).
check_corner(Corner, Edge1, Edge2) :-
    is_segment(Corner),
    is_segment(Edge1),
    not_segment(Edge2).

check_neighbors_corners(0, _, _, _, _, _, _, _, _).
check_neighbors_corners(C, N, E, S, W, NE, NW, SE, SW) :- is_segment(C),
    check_corner(NE, N, E),
    check_corner(NW, N, W),
    check_corner(SE, S, E),
    check_corner(SW, S, W).

check_neighbors_rows([NW, N | []], [W, C | []], [SW, S | []]) :-
    check_neighbors_pattern(C, N, 0, S, W),
    check_neighbors_corners(C, N, 0, S, W, 0, NW, 0, SW).
check_neighbors_rows([NW, N, NE | Row1], [W, C, E | Row2], [SW, S, SE | Row3]) :-
    check_neighbors_pattern(C, N, E, S, W),
    check_neighbors_corners(C, N, E, S, W, NE, NW, SE, SW),
    check_neighbors_rows([N, NE | Row1],[C, E | Row2],[S, SE | Row3]).
  

get_edges_row([_], _, []) :- !.
get_edges_row([A, B | Row], (R, C), Edges) :-
    (not_segment(A) | not_segment(B)),
    NextCol is C + 1, !,
    get_edges_row([B | Row], (R, NextCol), Edges).
get_edges_row([A, B | Row], (R, C), [e((R, C),(R, NextCol)) | Edges]) :-
    is_segment(A), !,
    is_segment(B), !,
    NextCol is C + 1, !,
    get_edges_row([B | Row], (R, NextCol), Edges).

get_edges_columns([], [], _, []).
get_edges_columns([A | Row1], [B| Row2], (R,C), Edges) :-
    (not_segment(A) | not_segment(B)), !,
    NextCol is C + 1, !,
    get_edges_columns(Row1, Row2, (R, NextCol), Edges).
get_edges_columns([A | Row1], [B | Row2], (R, C), [e((R, C),(NextRow, C)) | Edges]) :-
    is_segment(A), !,
    is_segment(B), !,
    NextCol is C + 1, !,
    NextRow is R + 1, !,
    get_edges_columns(Row1, Row2, (R, NextCol), Edges).

get_edges(Row1, Row2, RowNum, Edges) :-
    get_edges_row(Row1, (RowNum, 0), Row1Edges),
    get_edges_columns(Row1, Row2, (RowNum, 0), Row12Edges),
    append(Row1Edges, Row12Edges, Edges).

get_graph([LastRow], RowNum, Edges) :- get_edges_row(LastRow, (RowNum, 0), Edges).
get_graph([Row1, Row2 | Remainder], RowNum, Graph) :-
    get_edges(Row1, Row2, RowNum, CurrentIterEdges),
    NextRow is RowNum + 1, !,
    get_graph([Row2 | Remainder], NextRow, SubGraph),
    append(CurrentIterEdges, SubGraph, Graph).

check_neighbors_helper([_, _]).
check_neighbors_helper([Row1, Row2, Row3 | Remainder]) :-
    check_neighbors_rows(Row1, Row2, Row3),
    check_neighbors_helper([Row2, Row3 | Remainder]).

% Checks 2 Things
% 1. Whether each square has the correct number of neighbors
% 2. Whether the snake doesn't touch itself
check_neighbors(Solution) :-
    pad_solution(Solution, Padded), !,
    check_neighbors_helper(Padded).

% Test
:- check_neighbors([[0,0,1],[2,2,2],[1,0,0]]).

collect_segments(_, _, []) :- !.
collect_segments(Head, Tail, Edges) :-
    append(LHS, [e(Head, Next) | RHS], Edges), !,
    append(LHS, RHS, RemEdges),
    collect_segments(Next, Tail, RemEdges), !.
collect_segments(Head, Tail, Edges) :-
    append(LHS, [e(Next, Head) | RHS], Edges), !,
    append(LHS, RHS, RemEdges),
    collect_segments(Next, Tail, RemEdges), !.
collect_segments(Head, Tail, Edges) :-
    append(LHS, [e(Next, Tail) | RHS], Edges), !,
    append(LHS, RHS, RemEdges),
    collect_segments(Head, Next, RemEdges), !.
collect_segments(Head, Tail, Edges) :-
    append(LHS, [e(Tail, Next) | RHS], Edges), !,
    append(LHS, RHS, RemEdges),
    collect_segments(Head, Next, RemEdges), !.


check_connected(Solution) :-
    get_graph(Solution, 0, [e(Head, Tail) | Edges]), !,
    collect_segments(Head, Tail, Edges).

% test
:- check_connected([edge(coord(1,1),coord(1,2)),edge(coord(1,0),coord(1,1)),edge(coord(1,0),coord(2,0)),edge(coord(0,2),coord(1,2))]).
:- not(check_connected([edge(coord(1,0),coord(1,1)),edge(coord(1,0),coord(2,0)),edge(coord(0,2),coord(1,2))])).

% snake([-1,-1,2,-1], [-1,-1,-1,3], [[0, 1, -1, -1], [-1, -1, -1, -1], [1, -1, -1, -1], [-1, -1, 2, -1]], S)
snake(RowHints, ColHints, Grid, Solution) :-
    copy_grid(Grid, Solution),
    check_neighbors(Solution),
    check_row_hints(Solution, RowHints),
    check_col_hints(Solution, ColHints),
    check_connected(Solution).



% Timing our test cases
:- time(solvePuzzle(p2x2)).
:- time(solvePuzzle(p3x3)).
:- time(solvePuzzle(p3x3b)).
:- time(solvePuzzle(p5x5_two)).
:- time(solvePuzzle(pCycle)).
:- time(solvePuzzle(p4x4)).
:- time(solvePuzzle(p5x5)).

% Testing Correctness of our algorithm
:- checkCorrect(p4x4).
:- checkCorrect(p5x5).
:- checkCorrect(p7x7).
:- checkCorrect(p10x10).