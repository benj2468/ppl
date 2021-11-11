{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chess8 where


import           Chess
import           Chess.Abstract                 ( AbsPos(..)
                                                , Color(..)
                                                , GameState
                                                , PieceKind(..)
                                                )
import           ChessSolver
import           Data.Map

data Pos = A| B | C | D | E | F | G | H deriving (Show, Ord, Eq, Enum)


instance AbsPos Pos where
    allPos = [A ..]

    adjPos A = (Nothing, Just B)
    adjPos B = (Just A, Just C)
    adjPos C = (Just B, Just D)
    adjPos D = (Just C, Just E)
    adjPos E = (Just D, Just F)
    adjPos F = (Just E, Just G)
    adjPos G = (Just F, Just H)
    adjPos H = (Just G, Nothing)

    initialBoard =
        ( Data.Map.fromList
            [ (A, white King)
            , (B, white Knight)
            , (C, white Rook)
            , (F, black Rook)
            , (G, black Knight)
            , (H, black King)
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

categorizeStates8 :: Map (GameState Pos) (Color, Int)
categorizeStates8 = categorizeStates

categorizeStatesP :: Map (String) (Color, Int)
categorizeStatesP = Data.Map.mapKeys gsShow categorizeStates8
