module Chess8 where

import           AI
import           Data.Map
import           Lib

-- Position, this holds all the positions on the board
data Position = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 deriving (Show, Ord, Eq)

-- Get the two positions adjacent to a given position, returning Nothing if a position is at the edge of the board
adjacent :: Position -> (Maybe Position, Maybe Position)
adjacent C0 = (Nothing, Just C1)
adjacent C1 = (Just C0, Just C2)
adjacent C2 = (Just C1, Just C3)
adjacent C3 = (Just C2, Just C4)
adjacent C4 = (Just C3, Just C5)
adjacent C5 = (Just C4, Just C6)
adjacent C6 = (Just C5, Just C7)
adjacent C7 = (Just C6, Nothing)

instance Lib.Pos Position where
    prev x = fst (adjacent x)
    next x = snd (adjacent x)
    all = [C0, C1, C2, C3, C4, C5, C6, C7]

-- A PieceType are the three different types of pieces that are in play, irrespective of color
data PieceType = King | Rook | Knight deriving (Show, Eq)

instance Display PieceType where
    display King   = "k"
    display Rook   = "r"
    display Knight = "n"

instance Parse PieceType where
    parse "k" = King
    parse "r" = Rook
    parse "n" = Knight
    parse "K" = King
    parse "R" = Rook
    parse "N" = Knight
    parse e   = error ("Could not parse that piece: " ++ show e)

instance PieceTy PieceType where
    king = King
    potentialPos game p start = case ty p of
        King   -> filterMoves game [prev start, next start]
        Knight -> filterMoves game [prev =<< prev start, next =<< next start]
        Rook   -> rookMoves game start

-- Constant Initial Position
initialPosition :: GameState Position PieceType
initialPosition = parse "W: KN---Rnk"

testPos :: GameState Position PieceType
testPos = parse "W: KNR-r-nk"



