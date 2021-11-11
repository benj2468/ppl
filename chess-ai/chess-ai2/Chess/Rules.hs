module Chess.Rules where
import           Chess.Abstract                 ( AbsPos(..)
                                                , Game
                                                , GameState
                                                , PieceKind(..)
                                                )
import           Chess.Data                     ( Color(..)
                                                , PieceOnBoard
                                                , changeColor
                                                , color
                                                , nextPos
                                                , prevPos
                                                )
import           Data.Maybe                     ( catMaybes )

potentialNextMoves :: (AbsPos p) => Game p -> PieceOnBoard p -> [p]
potentialNextMoves mb ((King, c), ps) =
  filter ((/= Just c) . occupantColor mb) $ catMaybes [nextPos ps, prevPos ps]
potentialNextMoves mb ((Knight, c), ps) =
  filter ((/= Just c) . occupantColor mb)
    $ catMaybes [nextPos ps >>= nextPos, prevPos ps >>= prevPos]
potentialNextMoves mb ((CastleRook, c), ps) =
  potentialNextMoves mb ((Rook, c), ps) ++ kingPos c mb
potentialNextMoves mb ((Rook, c), ps) = moveFrom nextPos ++ moveFrom prevPos
 where
  moveFrom f = stopOnPiece f (f ps)
  stopOnPiece f (Just ps') = case occupantColor mb ps' of
    Nothing -> ps' : stopOnPiece f (f ps')
    Just c2 | c == c2   -> []
            | otherwise -> [ps']
  stopOnPiece _f Nothing = []

-- | Check if a square indicated by Pos is attacked by the enemy (not 'Color')

isAttacked :: (AbsPos p) => Game p -> Color -> [p] -> Bool
isAttacked _  _ []                = False
isAttacked mb c (pos : remainder) = or
  [ pos `elem` (potentialNextMoves mb pob)
  | pob@((_, pc), _) <- pieces mb
  , pc /= c
  ]

kingAttacked :: (AbsPos p) => Color -> Game p -> Bool
kingAttacked c mb = isAttacked mb c kingPos
  where kingPos = [ ps | ((King, c'), ps) <- pieces mb, c' == c ]

kingPos :: (AbsPos p) => Color -> Game p -> [p]
kingPos c mb = [ ps | ((King, c'), ps) <- pieces mb, c' == c ]

legalMoves :: (AbsPos p) => GameState p -> [GameState p]
legalMoves (mb, c) =
  map (flip (,) (changeColor c)) $ filter (not . kingAttacked c) $ concatMap
    (\x -> map (move mb x) (potentialNextMoves mb x))
    mypieces
  where mypieces = [ p | p <- pieces mb, color p == c ]

-- | Determine who the winner is in a certain state.
--   note that this can only be the player whose turn it isn't.
won :: (AbsPos p) => GameState p -> Maybe Color
won (mb, c) = if kingAttacked c mb
  then case legalMoves (mb, c) of
    [] -> Just (changeColor c)
    _  -> Nothing
  else Nothing

