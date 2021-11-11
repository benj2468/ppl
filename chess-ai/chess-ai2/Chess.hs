{-# OPTIONS_GHC -Wall #-}
module Chess where
import           Chess.Abstract                 ( AbsPos(..)
                                                , GameState
                                                , PieceKind(..)
                                                )
import           Chess.Data
import           Chess.Rules                    ( kingAttacked
                                                , legalMoves
                                                )
import           Data.Char                      ( toLower )
import qualified Data.Set                      as Set

type GameStates p = Set.Set (GameState p)
type SolverState p
  = (GameStates p, GameStates p, GameStates p, GameStates p, GameStates p)
solveGameStep :: (AbsPos p) => SolverState p -> SolverState p
solveGameStep (winningStates, drawingStates, losingStates, reachableStates, newReachableStates)
  = ( Set.union newWinning winningStates
    , Set.union newDrawing drawingStates
    , Set.union newLosing losingStates
    , Set.difference (Set.union reachableStates newReachableStates)
                     newSolvedStates
    , Set.difference
      (Set.fromList
        [ ps | st <- Set.toList $ newReachableStates, ps <- legalMoves st ]
      )
      allOldStates
    )
 where
  allOldStates =
    winningStates
      `Set.union` drawingStates
      `Set.union` losingStates
      `Set.union` reachableStates
  newSolvedStates = newWinning `Set.union` newDrawing `Set.union` newLosing
  -- todo: in the code below we calculate 'legalmoves st' three times..
  --       we should do this only once for every element in 'reachableStates'
  newWinning      = Set.filter isWinning reachableStates
  newDrawing      = Set.filter isDrawing reachableStates
  newLosing       = Set.filter isLosing reachableStates
  isWinning st = case legalMoves st of
    []  -> False
    lst -> any (`Set.member` losingStates) lst
  isDrawing st = case legalMoves st of
    [] -> not (kingAttacked (snd st) (fst st))
    lst ->
      all (`Set.member` (drawingStates `Set.union` winningStates)) lst
        && any (`Set.member` drawingStates) lst
  isLosing st = case legalMoves st of
    []  -> kingAttacked (snd st) (fst st)
    lst -> all (`Set.member` winningStates) lst

-- | solve a game starting in any of the states in 'lst'.
--   Returns sets of winning, drawing and losing states.
solveGame
  :: (AbsPos p) => [GameState p] -> (GameStates p, GameStates p, GameStates p)
solveGame states = go (mempty, mempty, mempty, mempty, Set.fromList states)
 where
  go st@(_, _, _, _, n) | Set.null n = go2 0 st
                        | otherwise  = go (solveGameStep st)
  go2 n st =
    let st2@(a, b, c, r2, _) = solveGameStep st
        n2                   = Set.size r2
    in  if n == n2 then (a, Set.union b r2, c) else go2 n2 st2

-- wins, draws, loses :: (AbsPos p) => GameStates p
-- (wins, draws, loses) = solveGame [initialBoard]

extract1 :: (a, a, a) -> a
extract1 (a, _, _) = a

extract2 :: (a, a, a) -> a
extract2 (_, a, _) = a

extract3 :: (a, a, a) -> a
extract3 (_, _, a) = a

wins :: (AbsPos p) => GameStates p
wins = extract1 (solveGame [initialBoard])

draws :: (AbsPos p) => GameStates p
draws = extract2 (solveGame [initialBoard])

loses :: (AbsPos p) => GameStates p
loses = extract3 (solveGame [initialBoard])

bestMoves :: (AbsPos p) => GameState p -> [GameState p]
bestMoves st = case (Set.member st wins, Set.member st draws) of
  (True , _   ) -> [ mv | mv <- legalMoves st, Set.member mv loses ]
  (False, True) -> [ mv | mv <- legalMoves st, Set.member mv draws ]
  _             -> legalMoves st

gsShow :: (AbsPos p) => GameState p -> String
gsShow (mb, c) =
  take 1 (show c) ++ ": " ++ map (mbPieceToChar . getSquare mb) allPos
 where
  mbPieceToChar Nothing  = '-'
  mbPieceToChar (Just v) = pieceToChar v
  pieceToChar (King      , White) = 'K'
  pieceToChar (Rook      , White) = 'R'
  pieceToChar (Knight    , White) = 'N'
  pieceToChar (CastleRook, White) = 'O'
  pieceToChar (pk        , Black) = toLower (pieceToChar (pk, White))

view :: (AbsPos p) => [GameState p] -> IO ()
view = mapM_ (putStrLn . gsShow)
