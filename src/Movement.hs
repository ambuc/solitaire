{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Movement
  ( doMove
  ) where

import Data.List     (findIndex)
import Data.Maybe    (fromJust, isJust)
import Lens.Micro    ( Getting, Lens'
                     , (%~), (&), (.~), (^.), (^?!)
                     , each, ix, lens, _head
                     )
import Lens.Micro.TH (makeLenses)

import CardTypes
import Utils     (canPlace)

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''GSt
makeLenses ''Field

--------------------------------------------------------------------------------

-- creates a lens from the field to the stock
-- operates on lists of displaycards
stockL :: Lens' Field [DCard] --all
stockL = lens (\f -> f ^. stock.cards & each.facedir .~ FaceUp)               
              (\f dcs -> f & stock.cards .~ (dcs & each.facedir .~ FaceDown)) 

-- creates a lens from the field to the waste
-- operates on lists of displaycards
wasteL :: Lens' Field [DCard]
wasteL = lens (\f -> f ^. waste.cards)           
              (\f dcs -> f & waste.cards .~ dcs) 

-- creates a lens from the field to an indexed tableau column
-- operates on piles
tableLN :: Int -> Lens' Field Pile
tableLN n = lens (\f -> f ^. table ^?! ix n)     
                 (\f p -> f & table . ix n .~ p) 

-- creates a lens from the field to an indexed foundation row
-- operates on piles.
foundLN :: Int -> Lens' Field Pile
foundLN n = lens (\f -> f ^. found ^?! ix n)    
                 (\f p -> f & found . ix n .~ p) 

--------------------------------------------------------------------------------
-- data PileType    = StockP | WasteP | TableP | FoundP deriving (Eq, Show, Ord)

-- returns a list of [Lens' Field Pile], or pileLenses, which can be used in a
-- construction like (field .^ pileLens).
inTableau :: Functor f0 => [(Pile -> f0 Pile) -> Field -> f0 Field]
inTableau = map tableLN [0..6] 

inFoundation :: Functor f0 => [(Pile -> f0 Pile) -> Field -> f0 Field]
inFoundation = map foundLN [0..3] 

-- findSpot takes a list of pileLenses, a card, and a field and iterates thru
-- the piles as derived from (field .^ pileLens) to see if the card can be
-- placed on any of them. it returns either Nothing or Just Idx, the index of
-- the first pile it could be placed on.
--
-- we do this instead of simply returning the pile to avoid collapsing the
-- type-vague pileLens from a lens to specifically a setter. the index lets us
-- read it as a getter from the _LN functions above without resolving it
-- to being a setter here.
findSpot :: [Getting Pile s Pile] -> Card -> s -> Maybe Int
findSpot pLenses c f = findIndex (\pL -> canPlace c (f ^. pL)) pLenses

-- alias for isJust of the above
isSpot pLs c f = isJust $ findSpot pLs c f
-- alias for fromJust of the above
mkSpot pLs c f = fromJust $ findSpot pLs c f

--------------------------------------------------------------------------------

-- takes a field and returns an updated field with the next move applied, if
-- possible. defaults to returning the same field. To be used by (.~), not (%~).
-- * clicking the empty stockX recalls the entire waste
-- * clicking a non-empty stockX moves 3 or less cards from stock to waste
-- * clicking a top waste card tries a move to the tableau or foundation
-- * clicking a top foundation card tries a move to the tableau
-- * clicking any tableau card tries a move to the tableau or foundation,
--   depending on row

tryMove :: [Ext] -> Field -> (Field, Int->Int)

tryMove [StockX] f = (f',id)
  where f' = f & stockL %~ (reverse load ++)
               & wasteL .~ []
        load = f ^. wasteL                                 

tryMove [_, StockX] f = (f',id)
  where f' = f & stockL %~ drop 3            --drop 3 from stock
               & wasteL %~ (reverse load ++) --add 3 to waste
        load = f ^. stockL & take 3          --get 3 from stock

tryMove [DCX dc, IdX 0, WasteX] f
  | canMove 0 dc f = (f', scoreFn)
  | otherwise      = (f , id)
  where (moveL, pType) = mkMoveL 0 (dc ^. card) f
        f'             = f & moveL . cards %~ (dc:) --write 1 to _
                           & wasteL %~ drop 1       --drop 1 from waste
        scoreFn
          | pType == FoundP = (+10)
          | otherwise       = (+5)

tryMove [DCX dc, IdX row, FoundX] f
  | canMove row dc f = (f', scoreFn)
  | otherwise        = (f , id)
  where (moveL, pType) = mkMoveL row (dc ^.  card) f
        f'             = f & moveL . cards %~ (dc:)        --write 1 to _
                           & foundLN row . cards %~ drop 1 --drop 1 from found.
        scoreFn i = i - 15 

tryMove [DCX dc, IdX row, IdX col, TableX] f
  | canMove row dc f = (f', scoreFn)
  | otherwise        = (f , id)
  where load = f ^. tableLN col . cards & take (succ row) 
        (moveL, pType) = mkMoveL row (dc ^. card) f
        f'             = f & moveL . cards %~ (load++)   --write n to _
                           & tableLN col . cards 
                              %~ drop (succ row)         --drop n from tableau
                           & tableLN col . cards . _head . facedir
                              .~ FaceUp                  --flip underlying card
        scoreFn
          | pType == FoundP = (+15)
          | otherwise       = (+5)
tryMove _ f = (f,id)

--------------------------------------------------------------------------------

-- returns true if a card could be moved anywhere else in the field of play
-- * if we're talking about a facedown card, the answer is no. 
-- * else a card at the top of its pile could go anywhere
-- * any other card must remain in the tableau 
canMove :: Int -> DCard -> Field -> Bool
canMove _ DCard{_facedir=FaceDown} _ = False
canMove 0 DCard{_card=c}           f = isSpot (inFoundation ++ inTableau) c f 
canMove _ DCard{_card=c}           f = isSpot inTableau c f 

--------------------------------------------------------------------------------

-- returns a (Lens' Field Pile) to the next valid move location 
-- * if we're moving from a row 0, it could be to the foundation or to the tableau
-- * otherwise we're certainly moving to the tableau
mkMoveL :: Functor f => Int -> Card -> Field 
                     -> ( (Pile -> f Pile) -> Field -> f Field, PileType)
mkMoveL 0 c f = if idx <= 3 
                  then (foundLN idx       , FoundP)
                  else (tableLN (idx - 4) , TableP)
  where idx = mkSpot (inFoundation ++ inTableau) c f 
mkMoveL _ c f = (tableLN $ mkSpot inTableau c f , TableP)

--------------------------------------------------------------------------------

-- wrapper for tryMove to add logging of historical states
doMove :: GSt -> [Ext] -> GSt
doMove s exs = if wasChange
                 then s & field .~ newField
                        & history %~ ((oldField, oldScore):)
                        & score %~ scoreFn
                        & moves %~ succ
                 else s
  where
    wasChange = oldField /= newField
    oldField = s ^. field
    oldScore = s ^. score
    (newField, scoreFn) = tryMove exs oldField

