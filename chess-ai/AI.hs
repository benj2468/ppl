{-# LANGUAGE FlexibleContexts #-}
module AI where

import           Data.List
import           Data.Map
import           Data.Maybe
import           Data.Ord                       ( Ord )
import           Lib                            ( Color
                                                , GameState
                                                , Piece
                                                , PieceTy
                                                , Pos
                                                , display
                                                , legalMoves
                                                , nextColor
                                                , parse
                                                , turn
                                                )


-- Data Declarations 
data Outcome = Winning Color Int | Stalemate deriving (Eq, Show)
newtype Solution = Solution (GString, Outcome)

stateString :: Solution -> GString
stateString (Solution (ss, _)) = ss

type SolutionMap = Map GString Outcome

-- This is a special type that will hold the GameStateString. Only attainable through this function stateToString
type GString = String
stateToString :: (Pos p, PieceTy ty) => GameState p ty -> GString
stateToString = display

class ColorOrd p where
    best :: Color -> p -> p -> p
    dft :: (Pos pos, PieceTy ty) => GameState pos ty -> p


-- Compares two outcomes, given the color of the current turn. 
-- Priority, I win in the fewest moves, Stalemate, You win in the most moves.
instance ColorOrd Outcome where
    best c (Winning c1 i) (Winning c2 j)
        | c1 == c2 && c1 == c = Winning c1 (min i j)
        | c1 == c             = Winning c1 i
        | c2 == c             = Winning c2 j
        | c1 == c2 && c1 /= c = Winning c1 (max i j)
    best c a              (Winning c1 i) = if c1 == c then Winning c1 i else a
    best c (Winning c1 i) b              = if c1 == c then Winning c1 i else b
    best _ Stalemate      Stalemate      = Stalemate
    dft state = Winning (nextColor (turn state)) (-1)

instance ColorOrd Solution where
    best c (Solution (gs1, out1)) (Solution (gs2, out2)) =
        if best c out1 out2 == out1
            then Solution (gs1, out1)
            else Solution (gs2, out2)
    dft state = Solution (stateToString state, dft state)

maxOrd
    :: (ColorOrd ord, Pos p, PieceTy ty)
    => GameState p ty
    -> Maybe ord
    -> [ord]
    -> ord
maxOrd state Nothing  (next : outcomes) = maxOrd state (Just next) outcomes
maxOrd state Nothing  []                = dft state
maxOrd state (Just p) []                = p
maxOrd state (Just p) (next : outcomes) =
    maxOrd state (Just (best (turn state) p next)) outcomes


-- increments the # of winning moves necessary to win
inc :: Outcome -> Outcome
inc (Winning c i) = Winning c (i + 1)
inc Stalemate     = Stalemate

--- Actually build the map - behind the buildSolution function
buildMap
    :: (Pos p, PieceTy ty) => SolutionMap -> [GameState p ty] -> SolutionMap
buildMap pMap []               = pMap
buildMap pMap (state : states) = do
    let currentMap = Data.Map.insert (stateToString state) Stalemate pMap
    let nextStates = Data.List.filter
            (\x -> notMember (stateToString x) currentMap)
            (legalMoves state)
    let nextMap = fillStalemate currentMap nextStates
    let newMap  = buildMap nextMap (states ++ nextStates)
    Data.Map.alter (const (Just (getSolution state newMap)))
                   (stateToString state)
                   newMap

--- Fills all the currently viewed states as stalemate until we can update them. And if we cannot update them, then they stay a stalemate
fillStalemate
    :: (Pos p, PieceTy ty) => SolutionMap -> [GameState p ty] -> SolutionMap
fillStalemate pMap []               = pMap
fillStalemate pMap (state : states) = do
    let currentMap = Data.Map.insert (stateToString state) Stalemate pMap
    fillStalemate currentMap states

-- This is the recursive step. Given that we've visited all the states that are reachable from one node, 
--- let's accumulate them all to find the best outcome of the current state.
getSolution :: (Pos p, PieceTy ty) => GameState p ty -> SolutionMap -> Outcome
getSolution state sMap = do
    let outcomes = Data.Maybe.mapMaybe
            (\x -> Data.Map.lookup (stateToString x) sMap)
            (legalMoves state)
    inc (maxOrd state Nothing outcomes)

-- Helper to determine the best move given a solution map and a starting state
bestMoveHelper
    :: (Pos p, PieceTy ty)
    => SolutionMap
    -> GameState p ty
    -> Maybe (GameState p ty)
bestMoveHelper sMap state = do
    let moves         = Data.List.map stateToString (legalMoves state)
    let moveSolutions = Data.List.map (\x -> (x, Data.Map.lookup x sMap)) moves
    let filtered = Data.List.map
            (\(gs, Just outcome) -> Solution (gs, outcome))
            moveSolutions
    let nextState = parse (stateString (maxOrd state Nothing filtered))
    if state == nextState then Nothing else Just nextState



----------------------------------------------------
-- Exported Functions ------------------------------
----------------------------------------------------

-- Builds a solution map given an initial state.
buildSolution :: (Pos p, PieceTy ty) => GameState p ty -> SolutionMap
buildSolution gs = buildMap Data.Map.empty [gs]

-- Gets the best outcome of a game given the initial state (for the person who's turn it is)
getOutcome :: (Pos p, PieceTy ty) => GameState p ty -> Maybe Outcome
getOutcome game = Data.Map.lookup (stateToString game) (buildSolution game)


-- Determines the best move for a starting state. There might not be a move to make, which is why it is a Maybe return
bestMove :: (Pos p, PieceTy ty) => GameState p ty -> Maybe (GameState p ty)
bestMove state = do
    let map = buildSolution state
    bestMoveHelper map state
