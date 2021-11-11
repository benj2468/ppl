{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chess9 where


import           Chess
import           Chess.Abstract                 ( AbsPos(..)
                                                , Color(..)
                                                , GameState
                                                , PieceKind(..)
                                                )
import           Chess.Rules
import           ChessSolver
import           Data.Map

data Pos = A| B | C | D | E | F | G | H | I deriving (Show, Ord, Eq, Enum)


instance AbsPos Pos where
  allPos = [A ..]

  adjPos A = (Nothing, Just B)
  adjPos B = (Just A, Just C)
  adjPos C = (Just B, Just D)
  adjPos D = (Just C, Just E)
  adjPos E = (Just D, Just F)
  adjPos F = (Just E, Just G)
  adjPos G = (Just F, Just H)
  adjPos H = (Just G, Just I)
  adjPos I = (Just H, Nothing)

  initialBoard =
    ( Data.Map.fromList
      [ (A, white King)
      , (B, white Knight)
      , (C, white Rook)
      , (G, black Rook)
      , (H, black Knight)
      , (I, black King)
      ]
    , White
    )
   where
    white x = (x, White)
    black x = (x, Black)

initialState :: GameState Pos
initialState = initialBoard

outcome :: Maybe (Color, Int)
outcome = Data.Map.lookup initialState categorizeStates

categorizeStates9 :: Map (GameState Pos) (Color, Int)
categorizeStates9 = categorizeStates

categorizeStatesP :: Map (String) (Color, Int)
categorizeStatesP = Data.Map.mapKeys gsShow categorizeStates9
