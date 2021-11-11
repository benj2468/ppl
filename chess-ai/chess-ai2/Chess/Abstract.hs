{-# OPTIONS_GHC -Wall #-}
module Chess.Abstract
  ( AbsPos(..)
  -- , AbsAr
  , Piece
  , Color(..)
  , PieceKind(..)
  , PieceOnBoard
  , Game
  , GameState
  , gsParse
  , turn
  ) where

import           Data.List
import           Data.Map
import           Data.Maybe
data PieceKind = King | Knight | Rook | CastleRook deriving (Show, Ord, Eq)
data Color = Black | White deriving (Show, Ord, Eq)
type Piece = (PieceKind, Color)
type PieceOnBoard p = (Piece, p)

type Game p = Map p Piece
type GameState p = (Game p, Color)

turn :: (AbsPos p) => GameState p -> Color
turn (_, c) = c

gsParse :: (AbsPos p) => String -> GameState p
gsParse ('B' : _ : _ : pcs) = (gsParseMb pcs, Black)
gsParse ('W' : _ : _ : pcs) = (gsParseMb pcs, White)
gsParse _                   = error "No parse"

gsParseMb :: (AbsPos p) => String -> Game p
gsParseMb lst = Data.List.foldr setSquare' (fst initialBoard) (zip lst allPos)
 where
  setSquare' :: (AbsPos p) => (Char, p) -> Game p -> Game p
  setSquare' ('k', p) = setSquareP p $ Just (King, Black)
  setSquare' ('r', p) = setSquareP p $ Just (Rook, Black)
  setSquare' ('n', p) = setSquareP p $ Just (Knight, Black)
  setSquare' ('K', p) = setSquareP p $ Just (King, White)
  setSquare' ('R', p) = setSquareP p $ Just (Rook, White)
  setSquare' ('N', p) = setSquareP p $ Just (Knight, White)
  setSquare' (_  , p) = setSquareP p Nothing
  setSquareP pos piece mb = setSquare mb pos piece

class (Show p, Eq p, Ord p) => AbsPos p where
  allPos :: [p]
  adjPos :: p -> (Maybe p, Maybe p)
  getSquare :: Game p -> p -> Maybe Piece
  getSquare game p = Data.Map.lookup p game
  setSquare :: Game p -> p -> Maybe Piece -> Game p
  setSquare game pos pc = Data.Map.alter (const pc) pos game
  pieces :: Game p -> [PieceOnBoard p]
  pieces mb =
        catMaybes $ Data.List.map (\x -> flip (,) x <$> getSquare mb x) allPos
  occupantColor ::  Game p -> p -> Maybe Color
  occupantColor mb pos = case getSquare mb pos of
        Nothing     -> Nothing
        Just (_, c) -> Just c
  move :: Game p -> PieceOnBoard p -> p -> Game p
  move mb pob to =
        setSquare (setSquare mb (snd pob) Nothing) to (Just $ fst pob)
  initialBoard ::  GameState p





