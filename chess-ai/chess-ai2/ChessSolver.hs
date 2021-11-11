{-# OPTIONS_GHC -Wall #-}
module ChessSolver where
import           Chess                          ( gsShow )
import           Chess.Abstract
import           Chess.Data                     ( changeColor )
import           Chess.Rules                    ( kingAttacked
                                                , legalMoves
                                                )
import           Data.List
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Set                      as Set
getAllStates :: (AbsPos p) => [GameState p]
getAllStates = nub (helper (Set.empty) [initialBoard])
 where
  helper
    :: (AbsPos p) => Set.Set (GameState p) -> [GameState p] -> [GameState p]
  helper lookedAt [] = Set.toList lookedAt
  helper lookedAt (e : es)
    | Set.member e lookedAt = helper lookedAt es
    | otherwise = helper (Set.insert e lookedAt) (es ++ legalMoves e)


winning :: (AbsPos p) => GameState p -> Maybe Color
winning (box, c) =
  if Data.List.null (legalMoves (box, c)) && kingAttacked c box
    then Just (changeColor c)
    else Nothing

categorizeStates :: (AbsPos p) => Map (GameState p) (Color, Int)
categorizeStates = helper
  getAllStates
  (Map.fromList [ (st, (c, 0)) | st <- getAllStates, Just c <- [winning st] ])
 where
  helper
    :: (AbsPos p)
    => [GameState p]
    -> Map (GameState p) (Color, Int)
    -> Map (GameState p) (Color, Int)
  helper unprocessed processed =
    case findNodeToProcess unprocessed processed of
      (unprocessed', processed')
        | length unprocessed == length unprocessed' -> processed
        | otherwise -> helper unprocessed' processed'
findNodeToProcess
  :: (AbsPos p)
  => [GameState p]
  -> Map (GameState p) (Color, Int)
  -> ([GameState p], Map (GameState p) (Color, Int))
findNodeToProcess [] processed = ([], processed)
findNodeToProcess (x : xs) processed =
  let nextStates = legalMoves x
      processedStates =
        [ (st, c, i)
        | st          <- nextStates
        , Just (c, i) <- [Map.lookup st processed]
        ]
      winnin = [ (st, c, i) | (st, c, i) <- processedStates, c == turn x ]
      losing = [ (st, c, i) | (st, c, i) <- processedStates, c /= turn x ]
      result = if null winnin
        then
          (if length losing == length nextStates && not (null (losing))
            then Just (pickWorstMove losing)
            else Nothing
          )
        else Just (pickBestMove winnin)
  in  (case result of
        Nothing ->
          let (xs', processed') = findNodeToProcess xs processed
          in  (x : xs', processed')
        Just (_, c, i) ->
          let (xs', processed') =
                findNodeToProcess xs (Map.insert x (c, i) processed) -- SJ: during the lecture, I wrote Map.insert st here, but I of course should add the state that we processed to the map, not the state that we're going to visit.
          in  (xs', processed')
      )
 where
  pickMove opt lst =
    let minNr      = opt (map trd lst)
        (st, c, i) = pickNr minNr lst
    in  (st, c, i + 1)
  pickWorstMove = pickMove maximum
  pickBestMove  = pickMove minimum
  pickNr i ((st, c, i') : sts) | i == i'   = (st, c, i')
                               | otherwise = pickNr i sts
  pickNr _i [] = error "pickNr has been given a number not in the list"
  trd (_, _, i) = i
