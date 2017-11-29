{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Movement
  ( alter
  ) where

import Lens.Micro
import Lens.Micro.TH      (makeLenses)
import Data.Functor.Const (Const)
import Data.List          (findIndex)
import Data.Maybe         (fromJust, isJust, isNothing)

import CardTypes
import Utils

--------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''GSt

--------------------------------------------------------------------------------

stockL :: Lens' GSt [DCard] --all
stockL = lens (\s -> s ^. stock.cards & each.facedir .~ FaceUp)               
              (\s dcs -> s & stock.cards .~ (dcs & each.facedir .~ FaceDown)) 

wasteL :: Lens' GSt [DCard]
wasteL = lens (\s -> s ^. waste.cards)           
              (\s dcs -> s & waste.cards .~ dcs) 

tableauLN :: Int -> Lens' GSt Pile
tableauLN n = lens (\s -> s ^. tableau ^?! ix n)     
                   (\s p -> s & tableau . ix n .~ p) 

foundationLN :: Int -> Lens' GSt Pile
foundationLN n = lens (\s -> s ^. foundation ^?! ix n)    
                      (\s p -> s & foundation . ix n .~ p) 

allCands
  :: [(Pile -> Data.Functor.Const.Const Pile Pile)
      -> GSt -> Data.Functor.Const.Const Pile GSt]
allCands = map foundationLN [0..3] ++ map tableauLN [0..6] 

canMove :: Card -> GSt -> Bool
canMove c s = isJust $ findIndex (\l -> canPlace c (s ^. l)) allCands

mkMove :: Functor f => Card -> GSt -> (Pile -> f Pile) -> GSt -> f GSt
mkMove c s = if idx <= 3
               then foundationLN idx
               else tableauLN (idx - 4)
  where idx = fromJust $ findIndex (\l -> canPlace c (s ^. l)) allCands

alter :: [Ext] -> GSt -> GSt
--if we click the empty stock icon, recall the entire waste
alter [StockX] s = let load = s ^.wasteL
                   in  s & stockL %~ (reverse load ++)
                         & wasteL .~ []
--if we click the non-empty stock icon, swap 3 (or less) from the stock to the
--waste
alter [_, StockX] s = let load = s ^. stockL & take 3
                      in  s & stockL %~ drop 3 
                            & wasteL %~ (reverse load ++)

--if we click the very top card of the waste, attempt to move it
alter [DCX dc@DCard{_card=c}, IdX 0, WasteX] s
  = if canMove c s
      then s & mkMove c s %~ (\p -> p & cards %~ (dc:))
             & wasteL %~ drop 1
      else s

--if we click the very top card of a column of the tableau, attempt to move it 
--alone
alter [DCX dc@DCard{_card=c}, IdX 0, IdX col, TabX] s
  = if canMove c s
      then s & mkMove c s %~ (\p -> p & cards %~ (dc:))
             & tableauLN col %~ (\p -> p & cards %~ drop 1)
             & tableauLN col %~ (\p -> p & cards . _head . facedir .~ FaceUp)
      else s

--if we click anything but the very top card of a tableau, it won't be alone.
--also it has to be face up.
alter [DCX DCard{_facedir=FaceUp, _card=c}, IdX row, IdX col, TabX] s
  = if canMove c s
      then s & mkMove c s %~ (\p -> p & cards %~ (load ++ ))
             & tableauLN col %~ (\p -> p & cards %~ drop (succ row))
             & tableauLN col %~ (\p -> p & cards . _head . facedir .~ FaceUp)
      else s
  where
    load  = s ^. tableauLN col . cards & take (succ row) 

--if we click anything in the foundation it'll be the head of the pile
alter [DCX dc@DCard{_card=c}, IdX row, FoundationX] s
  = if canMove c s
      then s & mkMove c s %~ (\p -> p & cards %~ (dc:))
             & foundationLN row %~ (\p -> p & cards %~ drop 1)
      else s

--otherwise do nothing
alter _ s = s
