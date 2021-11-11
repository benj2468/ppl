module Chess12 where

import           AI
import           Control.Monad.Trans.RWS.CPS    ( pass )
import           Control.Monad.Writer.Lazy      ( MonadWriter(pass) )
import           Data.List
import           Data.Map
import           Lib

-- Position, this holds all the positions on the board
data Position = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 deriving (Show, Ord, Eq)

-- Get the two positions adjacent to a given position, returning Nothing if a position is at the edge of the board
adjacent :: Position -> (Maybe Position, Maybe Position)
adjacent C0  = (Nothing, Just C1)
adjacent C1  = (Just C0, Just C2)
adjacent C2  = (Just C1, Just C3)
adjacent C3  = (Just C2, Just C4)
adjacent C4  = (Just C3, Just C5)
adjacent C5  = (Just C4, Just C6)
adjacent C6  = (Just C5, Just C7)
adjacent C7  = (Just C6, Just C8)
adjacent C8  = (Just C7, Just C9)
adjacent C9  = (Just C8, Just C10)
adjacent C10 = (Just C9, Just C11)
adjacent C11 = (Just C10, Nothing)

instance Lib.Pos Position where
    prev x = fst (adjacent x)
    next x = snd (adjacent x)
    all = [C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11]

-- A PieceType are the three different types of pieces that are in play, irrespective of color
data PieceType = King | Rook | Knight | CastleRook deriving (Show, Eq)

instance Display PieceType where
    display King       = "k"
    display Rook       = "r"
    display Knight     = "n"
    display CastleRook = "o"

instance Parse PieceType where
    parse "k" = King
    parse "r" = Rook
    parse "o" = CastleRook
    parse "n" = Knight
    parse "K" = King
    parse "R" = Rook
    parse "O" = CastleRook
    parse "N" = Knight
    parse e   = error ("Could not parse that piece: " ++ show e)


rook :: (PieceTy ty) => Color -> Piece ty
rook White = parse "R"
rook Black = parse "r"

instance PieceTy PieceType where
    king = King
    potentialPos game p start = do
        case ty p of
            King -> filterMoves game [prev start, next start]
            Knight ->
                filterMoves game [prev =<< prev start, next =<< next start]
            Rook       -> rookMoves game start
            CastleRook -> rookMoves game start ++ Data.List.map
                fst
                (Data.List.filter (\(k, v) -> v == kingPiece (turn game))
                                  (Data.Map.toList (board game))
                )
    newGameState game pc start end = case ty pc of
        CastleRook -> case Data.Map.lookup end (board game) of
            Just endPiece -> case ty endPiece of
                King -> swapRook game start end endPiece
                Rook -> swapRook game start end endPiece
                _    -> defaultNewGameState game pc start end
            _ -> defaultNewGameState game pc start end
        _ -> defaultNewGameState game pc start end
    lastResortMove game =
        case piecePosition game (Piece (turn game) CastleRook) of
            Nothing    -> []
            Just start -> Data.List.map
                (\(p, _) -> move game (start, p))
                (Data.List.filter
                    (\(k, v) ->
                        v == rook (turn game) || v == kingPiece (turn game)
                    )
                    (Data.Map.toList (board game))
                )




-- First, move piece at destination to piece at start
-- Then make the other destination a rook
swapRook
    :: (Pos p, PieceTy ty)
    => GameState p ty
    -> p
    -> p
    -> Piece ty
    -> GameState p ty
swapRook game start end endPiece = do
    let firstMove = Data.Map.alter (const (Just endPiece)) start (board game)
    let secondMove =
            Data.Map.alter (const (Just (rook (turn game)))) end firstMove
    let color = if endPiece == rook (turn game)
            then turn game
            else nextColor (turn game)

    Game { turn = color, board = secondMove }




-- Constant Initial Position
initialPosition :: GameState Position PieceType
initialPosition = parse "W: ONKNR--rnkno"

testPos :: GameState Position PieceType
-- testPos = parse "B: KNRNR--rnkno"
testPos = parse "W: KNRNRr--nknr"
