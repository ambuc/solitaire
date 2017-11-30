{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Movement
  ( stockL, wasteL, tableLN, foundLN
  , move, attempt
  ) where

import Lens.Micro
import Lens.Micro.TH      (makeLenses)
import Data.Functor.Const (Const)
import Data.List          (findIndex)
import Data.Maybe         (fromJust, isJust, isNothing, fromMaybe)

import CardTypes
import Utils

--------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''GSt
makeLenses ''Field

--------------------------------------------------------------------------------

stockL :: Lens' Field [DCard] --all
stockL = lens (\f -> f ^. stock.cards & each.facedir .~ FaceUp)               
              (\f dcs -> f & stock.cards .~ (dcs & each.facedir .~ FaceDown)) 

wasteL :: Lens' Field [DCard]
wasteL = lens (\f -> f ^. waste.cards)           
              (\f dcs -> f & waste.cards .~ dcs) 

tableLN :: Int -> Lens' Field Pile
tableLN n = lens (\f -> f ^. table ^?! ix n)     
                 (\f p -> f & table . ix n .~ p) 

foundLN :: Int -> Lens' Field Pile
foundLN n = lens (\f -> f ^. found ^?! ix n)    
                 (\f p -> f & found . ix n .~ p) 

--------------------------------------------------------------------------------

maybeAnywhere :: Card -> Field -> Maybe Int
maybeAnywhere c f = findIndex (\l -> canPlace c (f ^. l)) 
                  $ map foundLN [0..3] ++ map tableLN [0..6] 
maybeTableau  :: Card -> Field -> Maybe Int
maybeTableau  c f = findIndex (\l -> canPlace c (f ^. l)) 
                  $ map tableLN [0..6] 

--------------------------------------------------------------------------------

-- returns true if a card could be moved anywhere else in the field of play
canMove :: Int -> Card -> Field -> Bool
canMove 0 c f = isJust $ maybeAnywhere c f
canMove _ c f = isJust $ maybeTableau  c f

--------------------------------------------------------------------------------

-- returns a (Lens' Field Pile) to the next valid move location 
mkMove :: Functor f => Int -> Card -> Field 
                    -> (Pile -> f Pile) -> Field -> f Field
mkMove 0 c f = if idx <= 3
                 then foundLN idx
                 else tableLN (idx - 4)
  where idx = fromJust $ maybeAnywhere c f
mkMove _ c f = tableLN $ fromJust $ maybeTableau  c f

--------------------------------------------------------------------------------

-- actually performs the move, if possible
move :: [Ext] -> Field -> Field
move extents f = fromMaybe f (attempt extents f)

--------------------------------------------------------------------------------

-- attempts to perform the move and returns a maybe field
attempt :: [Ext] -> Field -> Maybe Field
--if we click the empty stock icon, recall the entire waste
attempt [StockX] f = let load = f ^. wasteL
                     in  Just $ f & stockL %~ (reverse load ++)
                                  & wasteL .~ []
--if we click the non-empty stock icon, swap 3 (or less) from the stock to the
--waste
attempt [_, StockX] f = let load = f ^. stockL & take 3
                        in  Just $ f & stockL %~ drop 3 
                                     & wasteL %~ (reverse load ++)
  
--if we click the very top card of the waste, attempt to move it
attempt [DCX dc@DCard{_card=c}, IdX 0, WasteX] f
  = if canMove 0 c f
      then Just $ f & mkMove 0 c f %~ (\p -> p & cards %~ (dc:))
                    & wasteL %~ drop 1
      else Nothing

--if we click anything but the very top card of a tableau, it won't be alone.
--also it has to be face up.
attempt [DCX DCard{_facedir=FaceUp, _card=c}, IdX row, IdX col, TableX] f
  = if canMove row c f
      then Just $ f & mkMove row c f %~ (\p -> p & cards %~ (load ++))
                    & tableLN col %~ (\p -> p & cards %~ drop (succ row))
                    & tableLN col %~ (\p -> p & cards . _head . facedir .~ FaceUp)
      else Nothing
  where
    load  = f ^. tableLN col . cards & take (succ row) 

--if we click anything in the foundation it'll be the head of the pile
attempt [DCX dc@DCard{_card=c}, IdX row, FoundX] f
  = if canMove row c f
      then Just $ f & mkMove row c f %~ (\p -> p & cards %~ (dc:))
                    & foundLN row %~ (\p -> p & cards %~ drop 1)
      else Nothing

--otherwise do nothing
attempt _ f = Nothing
