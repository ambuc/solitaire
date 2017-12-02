{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( toColor
  , canPlace
  , hasWon
  , newGame
  , undoMove
  , mkInitS
  , initialDeal
  ) where

import Data.Maybe (fromJust)
import Data.List.Split (splitPlaces)
import qualified Brick.Types as T
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, newStdGen, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------

toColor :: Suit -> Color -- assigns colors to suits
toColor Spade = Black
toColor Club  = Black
toColor _     = Red

canPlace :: Card -> Pile -> Bool -- says whether a card can be placed on a pile
-- given an empty foundation pile, a card needs to match both biases
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
-- nonempty foundation piles reject aces
canPlace (Card RA _ ) Pile { _pileType = FoundP
                           , _cards    = (dc:_)
                           } = False
-- nonempty foundation piles accept cards if they match suit and ascend rank
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _suitBias = Just sb
                           } = (pred r == r') && (s == sb)
-- given an empty tableau pile, a card needs to match its rankbias
canPlace (Card r  _ ) Pile { _pileType = TableP
                           , _cards    = []
                           , _rankBias = Just rb
                           } = r == rb
-- nonempty tableau piles reject kings
canPlace (Card RK _ ) Pile { _pileType = TableP
                           , _cards    = (dc:_)
                           } = False
-- nonempty tableau piles accept cards if they alternate color and descend rank
canPlace (Card r  s ) Pile { _pileType = TableP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (toColor s /= toColor s')
canPlace _ _ = False -- if not covered above, default invalid

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- given a game with a seed, get a new seed and use it to spawn a new game
newGame :: GSt -> GSt  
newGame s = let seed' = snd $ R.next $ s ^. seed 
            in  mkInitS seed'

undoMove :: GSt -> GSt
undoMove s = if hasHistory
               then s & field .~ oldField
                      & history %~ drop 1
                      & score .~ oldScore
                      & moves %~ pred

               else s
  where (oldField, oldScore) = s ^. history ^?! _head -- assured if called
        hasHistory = not $ null $ s ^. history

-- if a game is won, all 52 cards are in the foundation
hasWon :: GSt -> Bool
hasWon s = length (s ^. field . found . traverse . cards) == 52

-- the default deal is a sorted list of cards. to be shuffled below
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

-- take a random generator and create a game state...
mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _field = field , _logLine = "..."
                   , _seed  = seed  , _history = [] 
                   , _score = 0     , _moves = 0
                   }
  where
    deal  = R.shuffle' initialDeal 52 seed -- ...by shuffling the initialDeal
    field = Field { _stock = stock, _waste = waste -- and doling it out amongst
                  , _table = table, _found = found -- the stock, waste, tableau
                  }
    stock =   Pile { _cards    = [ DCard { _card    = c
                                         , _facedir = FaceDown 
                                         } 
                                 | c <- drop 31 deal 
                                 ]
                   , _display  = Stacked
                   , _rankBias = Nothing
                   , _suitBias = Nothing
                   , _pileType = StockP
                   }
    waste =   Pile { _cards    = [ DCard { _card    = c
                                         , _facedir = FaceUp 
                                         } 
                                 | c <- take 3 $ drop 28 deal 
                                 ]
                   , _display  = Sp3 -- wastes only show their top three cards
                   , _rankBias = Nothing
                   , _suitBias = Nothing
                   , _pileType = WasteP
                   }
    table = [ Pile { _cards    = [ DCard { _card    = c
                                         , _facedir = d
                                         } 
                                 | (c,d) <- zip cs (FaceUp:repeat FaceDown) 
                                 ]
                   , _display  = Splayed
                   , _rankBias = Just RK -- tableaus only accept base kings
                   , _suitBias = Nothing
                   , _pileType = TableP
                   } 
            | cs <- splitPlaces [7,6..1] deal -- list of lists of lengths 7,6..
            ]
    found = [ Pile { _cards    = []
                   , _display  = Stacked
                   , _rankBias = Just RA -- foundations only accept base aces
                   , _suitBias = Just s
                   , _pileType = FoundP
                   } 
            | s <- allSuits ]

