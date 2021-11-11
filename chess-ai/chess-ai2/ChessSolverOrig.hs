{-# OPTIONS_GHC -Wall #-}
module ChessSolver where
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
data GS = G1 | G2 | G3 | G4 | G5 | G6 deriving (Eq,Show,Ord)
data Color = Black | White deriving (Eq, Show, Ord)
getNextStates :: GS -> [GS]
getNextStates G3 = []
getNextStates G4 = []
getNextStates G5 = [G3,G4]
getNextStates _ = [G1,G2,G6,G5]
initialState :: GS
initialState = G1
winning :: GS -> Maybe Color
winning G3 = Just Black
winning G4 = Just White
winning _ = Nothing
toMove :: GS -> Color
toMove G1 = Black
toMove G2 = Black
toMove G4 = Black
toMove G5 = Black
toMove _ = White

getAllStates :: [GS]
getAllStates = nub (helper (Set.empty) [initialState])
  where
     helper :: Set.Set GS -> [GS] -> [GS]
     helper lookedAt [] = Set.toList lookedAt
     helper lookedAt (e:es) | Set.member e lookedAt = helper lookedAt es
       | otherwise = helper (Set.insert e lookedAt) (es ++ getNextStates e)

categorizeStates :: Map GS (Color, Int)
categorizeStates = helper getAllStates (Map.fromList [(st,(c,0)) | st <- getAllStates, Just c <- [winning st]])
  where
     helper :: [GS] -> Map GS (Color, Int) -> Map GS (Color, Int)
     helper unprocessed processed
      = case findNodeToProcess unprocessed processed of
           (unprocessed', processed') | length unprocessed == length unprocessed'-> processed
             | otherwise -> helper unprocessed' processed'
findNodeToProcess :: [GS] -> Map GS (Color, Int)
                  -> ([GS], Map GS (Color, Int))
findNodeToProcess [] processed = ([],processed) 
findNodeToProcess (x:xs) processed
 = let player st = toMove st
       nextStates = getNextStates x
       processedStates = [ (st,c,i) | st <- nextStates, Just (c,i) <- [Map.lookup st processed]]
       winnin = [(st,c,i) | (st,c,i) <- processedStates, c == player st]
       losing = [(st,c,i) | (st,c,i) <- processedStates, c /= player st]
       result = if null winnin
                then (if length losing == length nextStates && not (null (losing))
                      then Just (pickWorstMove losing)
                      else Nothing)
                else Just (pickBestMove winnin)
   in (case result of
              Nothing -> let (xs',processed') = findNodeToProcess xs processed
                         in (x:xs',processed')
              Just (_,c,i) -> let (xs',processed') = findNodeToProcess xs (Map.insert x (c,i) processed) -- SJ: during the lecture, I wrote Map.insert st here, but I of course should add the state that we processed to the map, not the state that we're going to visit.
                         in (xs',processed'))
  where
        pickMove opt lst =
          let minNr = opt (map trd lst)
              (st,c,i) = pickNr minNr lst
          in (st,c,i+1)
        pickWorstMove = pickMove maximum
        pickBestMove = pickMove minimum
        pickNr i ((st,c,i'):sts) | i == i' = (st,c,i')
          | otherwise = pickNr i sts
        pickNr _i [] = error "pickNr has been given a number not in the list"
        trd (_,_,i) = i
        
