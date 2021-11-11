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

clean_row([0], []).
clean_row([Val|Row], [Val|Sol]) :- not_segment(Val),!, clean_row(Row, Sol).
clean_row([Val|Row], [Val|Sol]) :- is_segment(Val), clean_row(Row, Sol).

clean_grid([_],[]).
clean_grid([[0 | Row] | Rows],[SolRow|Sol]) :- clean_row(Row,SolRow), clean_grid(Rows,Sol).

clean_solution([_ | Rows], Cleaned) :- clean_grid(Rows, Cleaned).

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

get_row([Row | _], 1, Row).
get_row([_ | Remainder], RowNum, Res) :-
    Next is RowNum - 1,
    get_row(Remainder, Next, Res).

get_cell(Sol, c(R, C), Val) :-
    get_row(Sol, R, Row),
    Fixed is C,
    nth1(Fixed, Row, Val).


get_neighbors(c(R, C), [
    c(PrevRow, C),
    c(NextRow, C),
    c(R, NextCol),
    c(R, PrevCol)
]) :-
    NextCol is C + 1,
    PrevCol is C - 1,
    NextRow is R + 1,
    PrevRow is R - 1.

get_corners(c(R, C), [
    c(PrevRow, PrevCol), % NW
    c(PrevRow, NextCol), % SW
    c(NextRow, PrevCol), % NE
    c(NextRow, NextCol) % SE
]) :-
    NextCol is C + 1,
    PrevCol is C - 1,
    NextRow is R + 1,
    PrevRow is R - 1.

add_edge(Solution, Prev, [N, S, E, W], E) :-
    get_cell(Solution, N, NX), (N = Prev; not_segment(NX)),
    get_cell(Solution, S, SX), (S = Prev; not_segment(SX)),
    get_cell(Solution, W, WX), (W = Prev; not_segment(WX)),
    get_cell(Solution, E, 2).
add_edge(Solution, Prev, [N, S, E, W], S) :-
    get_cell(Solution, N, NX), (N = Prev; not_segment(NX)),
    get_cell(Solution, E, EX), (E = Prev; not_segment(EX)),
    get_cell(Solution, W, WX), (W = Prev; not_segment(WX)),
    get_cell(Solution, S, 2).
add_edge(Solution, Prev, [N, S, E, W], W) :-
    get_cell(Solution, N, NX), (N = Prev; not_segment(NX)),
    get_cell(Solution, S, SX), (S = Prev; not_segment(SX)),
    get_cell(Solution, E, EX), (E = Prev; not_segment(EX)),
    get_cell(Solution, W, 2).
add_edge(Solution, Prev, [N, S, E, W], N) :-
    get_cell(Solution, S, SX), (S = Prev; not_segment(SX)),
    get_cell(Solution, E, EX), (E = Prev; not_segment(EX)),
    get_cell(Solution, W, WX), (W = Prev; not_segment(WX)),
    get_cell(Solution, N, 2).

find_path(Solution, PrevHead, Head, Tail) :-
    get_neighbors(Head, Neighbors), !,
    (member(Tail, Neighbors) -> true; 
        add_edge(Solution, PrevHead, Neighbors, NewHead),
        find_path(Solution, Head, NewHead, Tail)).
    

find_leaf(Sol, c(Row, Col)) :-
    append(TopRows, [LeafRow | _], Sol),
    append(BeforeLeaf, [1 | _], LeafRow),
    length(BeforeLeaf, Before),
    Col is Before + 1,
    length(TopRows, Above),
    Row is Above + 1.

% Input is Solution, output is coordinates of the head and the tail
find_head_tail(Sol, Head, Tail) :-
    find_leaf(Sol, Head),
    find_leaf(Sol, Tail),
    not(Head = Tail).

% Test
:- find_head_tail([[-1,1, -1],[-1,-1,-1],[ 1,-1,-1]], c(3,1), c(1,2)).

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
    check_neighbors_corners(C, N, 0, S, W, 0, NW, 0, SW).
check_neighbors_rows([NW, N, NE | Row1], [W, C, E | Row2], [SW, S, SE | Row3]) :-
    check_neighbors_corners(C, N, E, S, W, NE, NW, SE, SW),
    check_neighbors_rows([N, NE | Row1],[C, E | Row2],[S, SE | Row3]).

check_touching([_, _]).
check_touching([Row1, Row2, Row3 | Remainder]) :-
    check_neighbors_rows(Row1, Row2, Row3),
    check_touching([Row2, Row3 | Remainder]).

% snake([-1,-1,2,-1], [-1,-1,-1,3], [[0, 1, -1, -1], [-1, -1, -1, -1], [1, -1, -1, -1], [-1, -1, 2, -1]], S)
snake(RowHints, ColHints, Grid, Final) :-
    pad_solution(Grid, Padded),  
    find_head_tail(Padded, Head, Tail), !,
    copy_grid(Padded, Solution), !,
    find_path(Solution, nil, Head, Tail),
    clean_solution(Solution, Final),
    check_row_hints(Final, RowHints),
    check_col_hints(Final, ColHints),
    check_touching(Final).
    
    

:- time(solvePuzzle(p2x2)).
:- time(solvePuzzle(p3x3)).
:- time(solvePuzzle(p3x3b)).
:- time(solvePuzzle(p5x5_two)).
:- time(solvePuzzle(pCycle)).
:- time(solvePuzzle(p4x4)).
:- time(solvePuzzle(p5x5)).
:- time(solvePuzzle(p7x7)).
% :- time(solvePuzzle(p10x10)).



% :- checkCorrect(p4x4).
% :- checkCorrect(p5x5).
% :- checkCorrect(p7x7).
% :- checkCorrect(p10x10).

