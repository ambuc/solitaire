{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( toColor
  , canPlace
  , hasWon
  , mkInitS
  , initialDeal
  ) where

import Data.Maybe (fromJust)
import Data.List.Split (splitPlaces)
import qualified Brick.Types as T
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------

toColor :: Suit -> Color
toColor Spade = Black
toColor Club  = Black
toColor _     = Red

canPlace :: Card -> Pile -> Bool
-- if a foundation pile is empty, the card must match both its biases
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
-- if a foundationPile isn't empty you can never put an Ace on it.
-- this is to get around trying to pred the first of an enum type.
canPlace (Card RA _ ) Pile { _pileType = FoundP
                           , _cards    = (dc:_)
                           } = False
-- if a foundataionPile isn't empty, the card must be the same suit and one rank
-- greater than its under
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _suitBias = Just sb
                           } = (pred r == r') && (s == sb)
-- if a tableauPile is empty, the card must match its rankBias
canPlace (Card r  _ ) Pile { _pileType = TableP
                           , _cards    = []
                           , _rankBias = Just rb
                           } = r == rb
-- if a tableauPile isn't empty, we can never place a King on it.
-- this is to get around trying to succ the last of an enum type.
canPlace (Card RK _ ) Pile { _pileType = TableP
                           , _cards    = (dc:_)
                           } = False
-- if a tableauPile isn't empty, the card must be another color than and of one
-- rank lesser than its under
canPlace (Card r  s ) Pile { _pileType = TableP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (toColor s /= toColor s')
canPlace _ _ = False -- base case

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank]

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit]

hasWon :: GSt -> Bool
hasWon s = length (s ^. field . found . traverse . cards) == 52

-- to be randomized on play
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _field   = field
                   , _logLine = "..."
                   , _seed    = seed
                   }
  where
    deal  = R.shuffle' initialDeal 52 seed
    field = Field { _stock = stock
                  , _waste = waste
                  , _table = table
                  , _found = found
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
                   , _display  = Sp3
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
                   , _rankBias = Just RK
                   , _suitBias = Nothing
                   , _pileType = TableP
                   } 
            | cs <- splitPlaces [7,6..1] deal
            ]
    found = [ Pile { _cards    = []
                   , _display  = Stacked
                   , _rankBias = Just RA
                   , _suitBias = Just s
                   , _pileType = FoundP
                   } 
            | s <- allSuits ]

