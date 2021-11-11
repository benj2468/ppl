{-# OPTIONS_GHC -Wall #-}
module Chess.Data
  ( nextPos
  , prevPos
  , PieceOnBoard
  , Color(..)
  , color
  , changeColor
  ) where
import           Chess.Abstract                 ( AbsPos(..)
                                                , Color(..)
                                                , PieceOnBoard
                                                , adjPos
                                                )


color :: (AbsPos p) => PieceOnBoard p -> Color
color ((_pk, c), _ps) = c

changeColor :: Color -> Color
changeColor Black = White
changeColor White = Black

-- | nextPos and prevPos give the next/previous position on the board, if any.
nextPos, prevPos :: (AbsPos p) => p -> Maybe p
nextPos = fst . adjPos
prevPos = snd . adjPos
