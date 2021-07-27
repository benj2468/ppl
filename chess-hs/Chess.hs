{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import           Data.Char
import           Data.List               hiding ( lookup )
import           Data.Map                       ( Map
                                                , assocs
                                                , elems
                                                , filter
                                                , fromList
                                                , insert
                                                , lookup
                                                , mapMaybe
                                                , update
                                                )
import           Data.Maybe
import           Data.Set                       ( Set
                                                , empty
                                                , fromAscList
                                                , toList
                                                , union
                                                )
import           Data.Text
import           Prelude                 hiding ( head
                                                , lookup
                                                )

-- Parsing a Structure from a String
class Parse n where
  parse :: String -> n

-- Display a Structure in a human readable format
class Display n where
  display :: n -> String

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

-- Get the previous position, to the left
prevPosition :: Position -> Maybe Position
prevPosition x = fst (adjacent x)

-- Get the next position, to the right
nextPosition :: Position -> Maybe Position
nextPosition x = snd (adjacent x)

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
  parse e   = error ("Could not parse that piece: " ++ show e)

-- Holds data for a Color of a piece
data Color = Black | White deriving (Show, Eq)

instance Display Color where
  display Black = "B"
  display White = "W"

instance Parse Color where
  parse "B" = Black
  parse "W" = White
  parse e   = error ("Could not parse that color: " ++ show e)

-- Determines the next color to play, identical to (not color) if color were
nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

-- Combines a color and a type of create a piece
data Piece = Piece
  { ty    :: PieceType
  , color :: Color
  }
  deriving (Show, Eq)

instance Display Piece where
  display piece = case color piece of
    White -> [Data.Char.toUpper (Data.List.head (display (ty piece)))]
    Black -> display (ty piece)

instance Parse Piece where
  parse str = Piece
    { ty    = parse [Data.Char.toLower (Data.List.head str)]
    , color = if isUpper (Data.List.head str) then White else Black
    }

-- Parse a String to create an optional piece, Nothing implies a blank square
parseM :: String -> Maybe Piece
parseM "-" = Nothing
parseM str = Just (parse str)

-- The opposite of parseM, shows a Piece, filling a piece with a "-" if it is not actually a Piece
showMPiece :: Maybe Piece -> String
showMPiece (Just piece) = display piece
showMPiece Nothing      = "-"

-- Maps Locations to Pieces on a Board. We might not have all locations filled at a given time.
-- This is how we keep track of pieces on a board
type Board = Map Position Piece

-- Get the current position, if any of a piece
piecePosition :: GameState -> Piece -> Maybe Position
piecePosition game piece =
  case Data.List.find (\(_, v) -> v == piece) (assocs (board game)) of
    Nothing  -> Nothing
    Just pos -> Just (fst pos)

-- Parsing a Board helper
parseSplitBoard :: [Text] -> Board
parseSplitBoard text = Data.Map.mapMaybe
  id
  (Data.Map.filter
    isJust
    (fromList
      [ (C0, parseM (unpack (Data.List.head text)))
      , (C1, parseM (unpack (text !! 1)))
      , (C2, parseM (unpack (text !! 2)))
      , (C3, parseM (unpack (text !! 3)))
      , (C4, parseM (unpack (text !! 4)))
      , (C5, parseM (unpack (text !! 5)))
      , (C6, parseM (unpack (text !! 6)))
      , (C7, parseM (unpack (text !! 7)))
      ]
    )
  )

-- Parse a Board from a String (Text) to the structure
parseBoard :: Text -> Board
parseBoard str = parseSplitBoard (chunksOf 1 str)

-- Data Structure for a Game, holds the current turn, and a board
data GameState = Game
  { turn  :: Color
  , board :: Board
  }
  deriving (Show, Eq)

-- Get all the active pieces, since we use a Map, this is just all the values
activePieces :: GameState -> [Piece]
activePieces game = elems (board game)

-- Get all the active pieces, of the current turn
currentTurnPieces :: GameState -> [Piece]
currentTurnPieces game =
  Data.List.filter (\p -> color p == turn game) (activePieces game)

-- Determine's which color occupies a given position on the board, if at all
-- This is never used though...
occupantColor :: Board -> Position -> Maybe Color
occupantColor b pos = case lookup pos b of
  Nothing -> Nothing
  Just p  -> Just (color p)

-- Moves the piece in position 1 to position 2
move :: GameState -> Position -> Position -> GameState
move game start end = case lookup start (board game) of
  Nothing -> game
  Just p  -> do
    let newBoard = Data.Map.update (const Nothing)
                                   start
                                   (Data.Map.insert end p (board game))
    Game { board = newBoard, turn = nextColor (turn game) }

instance Display GameState where
  display game = do
    let p0 = showMPiece (lookup C0 (board game))
    let p1 = showMPiece (lookup C1 (board game))
    let p2 = showMPiece (lookup C2 (board game))
    let p3 = showMPiece (lookup C3 (board game))
    let p4 = showMPiece (lookup C4 (board game))
    let p5 = showMPiece (lookup C5 (board game))
    let p6 = showMPiece (lookup C6 (board game))
    let p7 = showMPiece (lookup C7 (board game))

    let t  = turn game

    ""
      ++ display t
      ++ ": "
      ++ p0
      ++ p1
      ++ p2
      ++ p3
      ++ p4
      ++ p5
      ++ p6
      ++ p7
      ++ ""

-- Parse helper to parse each individual Char (Text) 
parseSplitString :: [Text] -> GameState
parseSplitString text = Game { turn  = parse (unpack (Data.List.head text))
                             , board = parseBoard (text !! 1)
                             }

instance Parse GameState where
  parse str = parseSplitString (splitOn (pack ": ") (pack str))

-- Returns true if the current player can move ANY piece to the provided position
filterMove :: GameState -> Position -> Bool
filterMove game pos = case lookup pos (board game) of
  Nothing -> True
  Just p  -> color p /= turn game

-- Returns a list of valid positions for ANY piece, i.e. each output location is a valid place the SOME piece can move to
filterMoves :: GameState -> [Maybe Position] -> [Position]
filterMoves game positions =
  Prelude.filter (filterMove game) (catMaybes positions)

-- Helper to recursively collect positions that a rook can move to
rookMovesHelper
  :: (Position -> Maybe Position) -> GameState -> Position -> [Position]
rookMovesHelper f game current = do
  let next = f current
  case (\x -> lookup x (board game)) =<< next of
    Nothing -> maybe [] (\x -> x : rookMovesHelper f game x) next
    Just p  -> maybe [] (\x -> [ x | turn game /= color p ]) next

-- Get all moves a rook can move to
rookMoves :: GameState -> Position -> [Position]
rookMoves game pos =
  rookMovesHelper prevPosition game pos ++ rookMovesHelper nextPosition game pos

-- Part 2
-- Rules
-- King - SAME
-- Knight can move to positions 2 away from current position
-- Rook - SAME
-- No piece can move to a location occupied by a piece of the same color
-- Captures - SAME

potentialSquaresPos :: GameState -> Position -> [Position]
potentialSquaresPos game start = case lookup start (board game) of
  Nothing -> []
  Just p  -> case ty p of
    King   -> filterMoves game [prevPosition start, nextPosition start]
    Knight -> filterMoves
      game
      [prevPosition =<< prevPosition start, nextPosition =<< nextPosition start]
    Rook -> rookMoves game start

potentialMovePos :: GameState -> Position -> [(Position, Position)]
potentialMovePos game start =
  Data.List.map (start, ) (potentialSquaresPos game start)

potentialSquares :: GameState -> Piece -> [Position]
potentialSquares game piece =
  maybe [] (potentialSquaresPos game) (piecePosition game piece)

potentialSquaresTurn :: GameState -> [Position]
potentialSquaresTurn game =
  Data.List.concatMap (potentialSquares game) (currentTurnPieces game)

-- Checks if the other persons King can be captured currently
kingInCheck :: GameState -> Bool
kingInCheck game = do
  let yourKing =
        piecePosition game (Piece { ty = King, color = nextColor (turn game) })
  case yourKing of
    Nothing -> False
    Just p  -> p `elem` potentialSquaresTurn game

-- Get all potential moves for a specific piece
potentialMoves :: GameState -> Piece -> [(Position, Position)]
potentialMoves game piece =
  maybe [] (potentialMovePos game) (piecePosition game piece)

-- Get all potential moves for the current turn
potentialMovesTurn :: GameState -> [(Position, Position)]
potentialMovesTurn game =
  Data.List.concatMap (potentialMoves game) (currentTurnPieces game)

-- A move is legal if the king of the person who just went is not in check
isLegal :: GameState -> Bool
isLegal game = not (kingInCheck game)

-- Collect all legal moves from a given state
legalMoves :: GameState -> [GameState]
legalMoves game = Data.List.filter
  isLegal
  (Data.List.map (uncurry (move game)) (potentialMovesTurn game))

-- Constant Initial Position
initialPosition :: GameState
initialPosition = parse "W: KNR--rnk"

-- Part 4 Recursion

-- Recursive Helper to count possible outcome states from a list of states
countStatesHelper :: [GameState] -> [GameState] -> (Int, [GameState])
countStatesHelper visited  []             = (0, visited)
countStatesHelper previous (game : games) = do
  let new = Data.List.filter
        (\x -> x `notElem` previous && x `notElem` games)
        (legalMoves game)
  let (c, res) = countStatesHelper (game : previous) (games ++ new)
  (c + 1, res)

-- Count all the possible states that are reachable from a provided state, and return all the states
countStates :: GameState -> (Int, [GameState])
countStates game = countStatesHelper [] [game]
