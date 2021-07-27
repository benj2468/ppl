# Assignment 5: Chess Preliminaries

## Outcomes

For the recursion, I found 1241 different valid states that the game can reach. It took (0.56 secs, 289,006,304 bytes) to make the calculation

## Notes

I ended up calling some of the functions in my code a bit different than what the assignment requested.

- First, my `Pos` is called `Position`
- I have no `stringToState` because I created a new Type Class, `Display`, that has one function `display` that displays a data type in human readable form, unlike `Show` which is the machine representation.
- Similarly, I have no `stateToString`, because I create a new Type Class `Parse`, which parses a String into a structure.
- My `move` follows more directly with the 2-D chess equivalent, which is `Position` -> `Position`. So my move have the following function declaration: `move :: GameState -> Position -> Position -> GameState`, a potential move can then be thought of as a tuple (`Position`, `Position`), using the `uncurry` function to easily pass this move into `move`.
- I do have an `occupantColor` function, thought I don't use it
- I do not have a `PieceOnBoard` data type, rather, I create the Board as a type alias on a `Map`, mapping `Position`: `Piece`. If a piece is not on the board, then it is not in the map, and if a position doesn't have a piece, then the position is not in the map either. Consequently, we only store the data that we need in the game. This does make indexing the map a bit difficult, but I think the structure is intuitive of the problem at hand.
