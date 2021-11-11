{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Lib where


import           Data.Char                      ( isUpper
                                                , toLower
                                                , toUpper
                                                )
import           Data.List               hiding ( lookup )
import           Data.Map                       ( Map
                                                , alter
                                                , assocs
                                                , delete
                                                , elems
                                                , empty
                                                , filter
                                                , fromList
                                                , insert
                                                , lookup
                                                , mapMaybe
                                                , notMember
                                                )

import           Data.Maybe
import           Data.String
import           Data.Text
import           Distribution.Simple.Command    ( ArgPlaceHolder )
import           GHC.Stack.Types                ( HasCallStack )
import           Prelude                 hiding ( head
                                                , lookup
                                                )

-- Type Class for parsing a Structure from a String
class Parse n where
  parse :: HasCallStack => String -> n

-- Type Class for displaying a Structure in a human readable format
class Display n where
  display :: n -> String

class Ord n => Pos n where
    next :: n -> Maybe n
    prev :: n -> Maybe n
    all :: [n]

-- Holds data for a Color of a piece
data Color = Black | White deriving (Show, Eq)

instance Display Color where
    display Black = "B"
    display White = "W"

instance Parse Color where
    parse "B" = Black
    parse "W" = White
    parse e   = error ("Could not parse that color: " ++ show e)

nextColor :: Color -> Color
nextColor Black = White
nextColor White = Black

defaultNewGameState
    :: (Pos p) => GameState p n -> Piece n -> p -> p -> GameState p n
defaultNewGameState game p start end = do
    let newBoard = Data.Map.delete start (Data.Map.insert end p (board game))

    Game { board = newBoard, turn = nextColor (turn game) }
class (Display n, Parse n, Eq n) => PieceTy n where
    king :: n
    potentialPos :: (Pos p) => GameState p n -> Piece n -> p -> [p]
    newGameState :: (Pos p) => GameState p n -> Piece n -> p -> p -> GameState p n
    newGameState = defaultNewGameState
    lastResortMove :: (Pos p) => GameState p n -> [GameState p n]
    lastResortMove _ = []



-- Combines a color and a type of create a piece
data Piece ty where
    Piece ::(Display ty, Parse ty, PieceTy ty) => Color -> ty -> Piece ty

deriving instance Eq a => Eq (Piece a)
deriving instance Show a => Show (Piece a)

ty :: Piece p -> p
ty (Piece _ ty) = ty

color :: Piece p -> Color
color (Piece c _) = c

kingPiece :: (PieceTy ty) => Color -> Piece ty
kingPiece c = Piece c king

instance (Display p) => Display (Piece p) where
    display piece = case color piece of
        White -> [Data.Char.toUpper (Data.List.head (display (ty piece)))]
        Black -> display (ty piece)

instance (Parse ty, Display ty, PieceTy ty) => Parse (Piece ty) where
    parse str = Piece
        (if isUpper (Data.List.head str) then White else Black)
        (parse str)


-- Parse a String to create an optional piece, Nothing implies a blank square
parseM :: (Parse ty, Display ty, PieceTy ty) => String -> Maybe (Piece ty)
parseM "-" = Nothing
parseM str = Just (parse str)
-- parseM str = error ("Debug " ++ str)

-- The opposite of parseM, shows a Piece, filling a piece with a "-" if it is not actually a Piece
showMPiece :: (Parse p, Display p) => Maybe (Piece p) -> String
showMPiece (Just piece) = display piece
showMPiece Nothing      = "-"

-- Maps Locations to Pieces on a Board. We might not have all locations filled at a given time.
-- This is how we keep track of pieces on a board
type Board pos ty = Map pos (Piece ty)

-- Data Structure for a Game, holds the current turn, and a board
data GameState pos ty = Game
    { turn  :: Color
    , board :: Board pos ty
    }
    deriving (Show, Eq)

-- Logic

-- Get the current position, if any of a piece
piecePosition
    :: (Pos p, PieceTy ty, Eq ty) => GameState p ty -> Piece ty -> Maybe p
piecePosition game piece =
    Just . fst =<< Data.List.find (\(_, v) -> v == piece) (assocs (board game))

filterPosMayPiece
    :: (Pos p, PieceTy ty) => (p, Maybe (Piece ty)) -> Maybe (p, Piece ty)
filterPosMayPiece (pos, Nothing) = Nothing
filterPosMayPiece (pos, Just pc) = Just (pos, pc)

-- Parse a Board from a String (Text) to the structure
parseBoard :: (Pos p, Ord p, PieceTy ty) => Text -> Board p ty
parseBoard str = Data.Map.fromList
    (Data.Maybe.mapMaybe
        filterPosMayPiece
        (Data.List.zip Lib.all
                       (Data.List.map (parseM . unpack) (chunksOf 1 str))
        )
    )


checkUniqueHelper :: Eq a => [a] -> [a] -> Bool
checkUniqueHelper _ [] = True
checkUniqueHelper seen (next : remain) =
    notElem next seen && checkUniqueHelper (next : seen) remain

checkUnique :: Eq a => [a] -> Bool
checkUnique = checkUniqueHelper []

-- Is a board valid?
validBoard :: (Pos p, PieceTy ty) => Board p ty -> Bool
validBoard b = checkUnique (elems b)

-- Get all the active pieces, since we use a Map, this is just all the values
activePieces :: (Pos p, PieceTy ty) => GameState p ty -> [Piece ty]
activePieces game = elems (board game)

-- Get all the active pieces, of the current turn
currentTurnPieces :: (Pos p, PieceTy ty) => GameState p ty -> [Piece ty]
currentTurnPieces game =
    Data.List.filter (\p -> color p == turn game) (activePieces game)

-- Determine's which color occupies a given position on the board, if at all
-- This is never used though...
occupantColor :: (Pos p) => Board p ty -> p -> Maybe Color
occupantColor b pos = Just . color =<< lookup pos b

-- Moves the piece in position 1 to position 2
move :: (Pos p, PieceTy ty) => GameState p ty -> Move p -> GameState p ty
move game (start, end) = case lookup start (board game) of
    Nothing -> game
    Just p  -> do
        newGameState game p start end

instance (Pos p, PieceTy ty) => Display (GameState p ty) where
    display game = do
        let
            lst = Data.List.filter
                (/= ' ')
                (Data.String.unwords
                    (Data.List.map
                        (\l -> showMPiece (lookup l (board game)))
                        Lib.all
                    )
                )
        let t = turn game

        display t ++ ": " ++ lst -- SJ: check out how I cleaned this up!

-- Parse helper to parse each individual Char (Text) 
parseSplitString :: (Pos p, PieceTy ty) => [Text] -> GameState p ty
parseSplitString []      = error "Invalid state"
parseSplitString (c : b) = do
    let parsed = parseBoard (Data.List.head b)
    Game { turn = parse (unpack c), board = parsed }

instance (Pos p, PieceTy ty) => Parse (GameState p ty) where
    parse str = parseSplitString (splitOn (pack ": ") (pack str))


-- Returns true if the current position is not occupied by the current player 
filterMove :: (Pos p, PieceTy ty) => GameState p ty -> p -> Bool
filterMove game pos = case lookup pos (board game) of
    Nothing -> True
    Just p  -> color p /= turn game

-- Given a list of potential pieces, return a subset of them if they are pieces and they are not occupied by the current player
filterMoves :: (Pos p, PieceTy ty) => GameState p ty -> [Maybe p] -> [p]
filterMoves game positions =
    Prelude.filter (filterMove game) (catMaybes positions)

-- Helper to recursively collect positions that a rook can move to
rookMovesHelper :: (Pos p) => (p -> Maybe p) -> GameState p ty -> p -> [p]
rookMovesHelper mover game current = do
    let next = mover current
    case (\x -> lookup x (board game)) =<< next of
        Nothing -> maybe [] (\x -> x : rookMovesHelper mover game x) next
        Just p  -> maybe [] (\x -> [ x | turn game /= color p ]) next

-- Get all moves a rook can move to
rookMoves :: (Pos p, PieceTy ty) => GameState p ty -> p -> [p]
rookMoves game pos =
    rookMovesHelper prev game pos ++ rookMovesHelper next game pos

-- Part 2
-- Rules
-- King - SAME
-- Knight can move to positions 2 away from current position
-- Rook - SAME
-- No piece can move to a location occupied by a piece of the same color
-- Captures - SAME

-- Calculates the potential moves from a given square to another square, first by finding what piece is on that square, 
-- and then figuring out where it can move to
potentialSquaresPos :: (Pos p, PieceTy ty) => GameState p ty -> p -> [p]
potentialSquaresPos game start = case lookup start (board game) of
    Nothing  -> []
    Just pos -> potentialPos game pos start

-- This is the same as potentialSquaresPos, but instead of passing in a position, we pass in a piece, and get it's position to perform the calculation
potentialSquares :: (Pos p, PieceTy ty) => GameState p ty -> Piece ty -> [p]
potentialSquares game piece =
    maybe [] (potentialSquaresPos game) (piecePosition game piece)

-- This tells us, given a state, where can the CURRENT player move to. 
potentialSquaresTurn :: (Pos p, PieceTy ty) => GameState p ty -> [p]
potentialSquaresTurn game =
    Data.List.concatMap (potentialSquares game) (currentTurnPieces game)

-- The following three functions are meant to be counterparts to the above three, just returning moves rather than destinations

type Move p = (p, p)

-- Given a position, what are the potential moves, as tuples. The first element in the tuple will always be 
-- the second parameter to this function
potentialMovePos :: (Pos p, PieceTy ty) => GameState p ty -> p -> [Move p]
potentialMovePos game start =
    Data.List.map (start, ) (potentialSquaresPos game start)

  -- Get all potential moves for a specific piece
potentialMoves :: (Pos p, PieceTy ty) => GameState p ty -> Piece ty -> [Move p]
potentialMoves game piece =
    maybe [] (potentialMovePos game) (piecePosition game piece)

-- Get all potential moves for the current turn
potentialMovesTurn :: (Pos p, PieceTy ty) => GameState p ty -> [Move p]
potentialMovesTurn game =
    Data.List.concatMap (potentialMoves game) (currentTurnPieces game)

-- Checks if the other persons King can be captured currently
kingInCheck :: (Pos p, PieceTy ty) => GameState p ty -> Bool
kingInCheck game = do
    let yourKing = piecePosition game (kingPiece (nextColor (turn game)))

    case yourKing of
        Nothing -> False
        Just p  -> p `elem` potentialSquaresTurn game

-- A move is legal if the king of the person who just went is not in check
isLegal :: (Pos p, PieceTy ty) => GameState p ty -> Bool
isLegal game = not (kingInCheck game)

-- Collect all legal moves from a given state
legalMoves :: (Pos p, PieceTy ty) => GameState p ty -> [GameState p ty]
legalMoves game = do
    let moves = Data.List.filter
            isLegal
            (Data.List.map (move game) (potentialMovesTurn game))
    if Data.List.null moves then lastResortMove game else moves


-- Part 4 Recursion

-- Recursive Helper to count possible outcome states from a list of states
countStatesHelper
    :: (Pos p, PieceTy ty)
    => [GameState p ty]
    -> [GameState p ty]
    -> [GameState p ty]
countStatesHelper visited  []             = visited
countStatesHelper previous (game : games) = do
    let new = Data.List.filter
            (\x -> x `notElem` previous && x `notElem` games)
            (legalMoves game)
    countStatesHelper (game : previous) (games ++ new)

-- Count all the possible states that are reachable from a provided state, and return all the states
countStates :: (Pos p, PieceTy ty) => GameState p ty -> Int
countStates game = Data.List.length (countStatesHelper [] [game])

getGameStates :: (Pos p, PieceTy ty) => GameState p ty -> [GameState p ty]
getGameStates game = countStatesHelper [] [game]




