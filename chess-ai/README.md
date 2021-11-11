# Week 6 Assignment: Haskell Chess AI

# Solver

Our solver first builds a map of solutions for each state. Mapping `GameStateString -> Solution`. A solution can either be a win for black or white, or a stalemate. A stalemate implies that nobody can win. Then, once this map is built, we can use the map to determine the best move for a given move in the map, this is usually the head of the map (the origin of the tree). Determining the best move is simply to pick the move where I (current turn) still win, and with the fewest moves, or that creates a stalemate, or that makes my opponent win with the most amount of moves

# Generalization

Our generalization makes use primarily of classes. Each game can be described with a series of data types that must implement certain classes. A game then is defined by a `Pos` and a `PieceTy`. A `Pos` defines the positions that exist on the game's board, i.e. `P1`, `P2`, `P3` .... A `PieceTy` defines the types of pieces that exist on the board, i.e. `King`, `Queen`, `Rook` ...

We then combine these two into a `GameState`, which takes both of these types as parameters.

One thing to note in regards to our abstraction is the move method. In our chess12 game we add the additional functionality of the Castling Rook. In our solution we only add the forfeit of the castle as a legal move if it is the only move left. This is due to the fact that otherwise this move is guaranteed to be worse than others, so we wish to prune those game trees as early as possible. Because we are getting rid of these losing states earlier, we save time and make our program more efficient.

# Contributors

Benjamin Cape '22
Samuel Baker '22
