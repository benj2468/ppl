# Principles of Programming Language Week 2 Assignment

Terms:
- AD :- Assignment Doc.
- Segment :- Part of the Snake (inc. head and tail)

Most of our solution is based off of the hints, and what was provided in the assignment file. Here is our high level procedure for solving the puzzle:

```prolog
copy_grid(Grid, Solution),
check_neighbors(Solution),
check_row_hints(Solution, RowHints),
check_col_hints(Solution, ColHints),
check_connected(Solution).
```

For the first check, `copy_grid`, we followed the standard procedure, adding some performance gains (i.e. cuts, and added checks), in order to not run unnecessary case checks.

For the second check, `check_neighbors`, we similarly followed the standard provided by the AD. We padded the solution to ensure that we can check each box while also maintaining logic integrity across each check. We then recursively checked each window of 3 rows so that we could fetch each cell's 4 neighbors.

In this portion of our solution, we also checked for corner logic (equivalent to the "non-touching" logic defined in the AD). This enhanced our runtime, by not requiring us to loop (recursively) over the solution twice. To check touching, we required the following logic:

---
Given a segment `A`, if `A` has some segment `B` diagonal to it, then there must exists some `C` such that `A` -> `C` -> `B` forms a path of length 3, and there cannot exists some `D` such that `A` -> `C` -> `B` -> `D` -> `A` forms a cycle.

---
The next part of the solution, checking hints, was almost identical to the AD, summing up the segments in the list, and checking that against the hint. Similarly for the columns, just after a simple transpose.

The final portion of our solution, and the most complex (if we may say so, innovative), is our check to connectedness. We build on Graph Theory and the Tree Theorem to ensure connectedness by finding a path in the graph. First, we create a list of all the edges in the grid, defining a vertex as a segment, and an edge as two segments that are neighbors. 

Then, starting from the first element of the list of edges, we recursively (iteratively) remove elements of the list to extend the path. Once we cannot extend the path any further, we know the graph is connected if the set of remaining edges is empty. 

## Efficiency

Our primary sources of efficiency gains come from cuts and clever algorithmic choices. We place cuts where cases are disjoint and `prolog` should never back-track down different paths, or where we know a certain predicate should never fail and prolog should always stick with it's first solution. 

## Additional Solution

The additional solution in [`snake2.pl`](./snake2.pl) houses another, faster solution to solving the problem. This solution deviates quite heavily from the hints. Rather than building the snake via the rules provided, we build the snake by recursively extending the snake by adding a segment. This solution is more of an imperative solution, but explicitly stating how to create the snake, rather than a declarative, which simply says what the snake should be at the end.

This way, we guarantee that the snake is always connected, and never touches itself. We still need to check for corners, because of the weirdness of being able to turn, but not being able to go to other squares corners. (though I think there is another way of doing this, by checking not the current head, but the previous head against the one you want to move to, but I couldn't quite get it working. We still need to check the row hints and column hints obviously. But the run-times seem to be much faster with this solution.

## Contributions
- Samuel Baker - f003mcg
- Benjamin Cape - f002q3z

We live code-shared and collaborated on each project segment. Each function solution was a team effort.
