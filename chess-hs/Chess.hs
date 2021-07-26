{-# OPTIONS_GHC -Wall #-}

import Data.Char
import Data.Map (Map, elems, filter, fromList, lookup, mapMaybe)
import Data.Maybe
import Data.Text
import Prelude hiding (head, lookup)

class Parse n where
  parse :: String -> n

-- Position, this holds all the positions on the board
data Position = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 deriving (Show, Ord, Eq)

adjacent :: Position -> (Maybe Position, Maybe Position)
adjacent C0 = (Nothing, Just C1)
adjacent C1 = (Just C0, Just C2)
adjacent C2 = (Just C1, Just C4)
adjacent C3 = (Just C2, Just C5)
adjacent C4 = (Just C3, Just C5)
adjacent C5 = (Just C4, Just C6)
adjacent C6 = (Just C5, Just C7)
adjacent C7 = (Just C6, Nothing)

prevPosition :: Position -> Maybe Position
prevPosition x = fst (adjacent x)

nextPosition :: Position -> Maybe Position
nextPosition x = snd (adjacent x)

-- A PieceType are the three different types of pieces that are in play, irrespective of color
data PieceType = King | Rook | Knight

instance Show PieceType where
  show King = "k"
  show Rook = "r"
  show Knight = "n"

instance Parse PieceType where
  parse "k" = King
  parse "r" = Rook
  parse "n" = Knight
  parse e = error ("Could not parse that piece: " ++ show e)

-- Holds data for a Color of a piece
data Color = Black | White

instance Show Color where
  show Black = "B"
  show White = "W"

instance Parse Color where
  parse "B" = Black
  parse "W" = White
  parse e = error ("Could not parse that color: " ++ show e)

nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

-- Combines a color and a type of create a piece
data Piece = Piece {ty :: PieceType, color :: Color}

instance Show Piece where
  show piece = case color piece of
    Black -> [Data.Char.toUpper (show (ty piece) !! 0)]
    White -> show (ty piece)

instance Parse Piece where
  parse str =
    Piece
      { ty = parse [Data.Char.toLower (str !! 0)],
        color = if isUpper (str !! 0) then Black else White
      }

parseM :: String -> Maybe Piece
parseM "-" = Nothing
parseM str = Just (parse str)

showMPiece :: Maybe Piece -> String
showMPiece (Just piece) = show piece
showMPiece Nothing = "-"

-- Maps Locations to Pieces on a Board. We might not have all locations filled at a given time.
-- This is how we keep track of pieces on a board
type Board =
  Map
    Position
    Piece

-- Parsing a Board
parseSplitBoard :: [Text] -> Board
parseSplitBoard text =
  Data.Map.mapMaybe
    (\a -> a)
    ( Data.Map.filter
        isJust
        ( fromList
            [ (C0, parseM (unpack (text !! 0))),
              (C1, parseM (unpack (text !! 1))),
              (C2, parseM (unpack (text !! 2))),
              (C3, parseM (unpack (text !! 3))),
              (C4, parseM (unpack (text !! 4))),
              (C5, parseM (unpack (text !! 5))),
              (C6, parseM (unpack (text !! 6))),
              (C7, parseM (unpack (text !! 7)))
            ]
        )
    )

parseBoard :: Text -> Board
parseBoard str = parseSplitBoard (chunksOf 1 str)

-- Data Structure for a Game, holds the current turn, and a board
data GameState = Game {turn :: Color, board :: Board} deriving (Show)

-- Get all the active pieces, since we use a Map, this is just all the values
activePieces :: GameState -> [Piece]
activePieces state = elems (board state)

-- Determine's which color occupies a given position on the board, if at all
occupantColor :: Board -> Position -> Maybe Color
occupantColor b pos = do
  case lookup pos b of
    Nothing -> Nothing
    Just p -> Just (color p)

-- Moves the piece in position 1 to position 2
move :: GameState -> Position -> Position -> GameState
move game start end = do
  case lookup start (board game) of
    Nothing -> game
    -- This is not correct, I have no idea how to use the update/delete functions
    Just p -> game

stateToString :: GameState -> String
stateToString game = do
  let p0 = showMPiece (lookup C0 (board game))
  let p1 = showMPiece (lookup C1 (board game))
  let p2 = showMPiece (lookup C2 (board game))
  let p3 = showMPiece (lookup C3 (board game))
  let p4 = showMPiece (lookup C4 (board game))
  let p5 = showMPiece (lookup C5 (board game))
  let p6 = showMPiece (lookup C6 (board game))
  let p7 = showMPiece (lookup C7 (board game))

  let t = turn game

  "" ++ show t ++ ": " ++ p0 ++ p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ p6 ++ p7 ++ ""

parseSplitString :: [Text] -> GameState
parseSplitString text =
  Game
    { turn = parse (unpack (text !! 0)),
      board = parseBoard (text !! 1)
    }

splitStateString :: Text -> [Text]
splitStateString = splitOn (pack ": ")

stringToState :: String -> GameState
stringToState str = parseSplitString (splitStateString (pack str))

-- Part 2
-- Need to read the rules to figure out what actually needs to happen here
-- potentialSquares :: GameState -> Pos -> [Pos]

-- kingInCheck :: GameState -> Bool

-- legalMoves :: GameState -> [GameState]

initialPosition :: GameState
initialPosition = stringToState "W: KNR--rnk"