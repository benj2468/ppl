{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
module Chess12 where


import           Chess
import           Chess.Abstract                 ( AbsPos(..)
                                                , Color(..)
                                                , GameState
                                                , PieceKind(..)
                                                )
import           Chess.Rules
import           ChessSolver
import           Data.Map

data Pos = A| B | C | D | E | F | G | H | I | J | K | L  deriving (Show, Ord, Eq, Enum)


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
  adjPos I = (Just H, Just J)
  adjPos J = (Just I, Just K)
  adjPos K = (Just J, Just L)
  adjPos L = (Just K, Nothing)

  move mb ((srcPcTy, srcPcColor), src) dst = do
    let sub = setSquare mb src (Data.Map.lookup dst mb)
    let srcPc = if srcPcTy == CastleRook
          then (Rook, srcPcColor)
          else (srcPcTy, srcPcColor)

    setSquare sub dst (Just $ srcPc)

  initialBoard =
    ( Data.Map.fromList
      [ (A, white CastleRook)
      , (B, white Knight)
      , (C, white King)
      , (D, white Knight)
      , (E, white Rook)
      , (H, black Rook)
      , (I, black Knight)
      , (J, black King)
      , (K, black Knight)
      , (L, black CastleRook)
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

categorizeStates12 :: Map (GameState Pos) (Color, Int)
categorizeStates12 = categorizeStates

categorizeStatesP :: Map (String) (Color, Int)
categorizeStatesP = Data.Map.mapKeys gsShow categorizeStates12
