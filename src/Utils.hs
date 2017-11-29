{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( toColor
  , canPlace
  , mkInitS
  , initialDeal
  ) where

import Data.Maybe (fromJust)
import Data.List.Split (splitPlaces)
import qualified Brick.Types as T
import Lens.Micro
import Lens.Micro.TH (makeLenses)

import CardTypes

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''GSt

-------------------------------------------------------------------------------

toColor :: Suit -> Color
toColor Spade = Black
toColor Club  = Black
toColor _     = Red

canPlace :: Card -> Pile -> Bool
-- foundation piles have both biases
canPlace (Card r  s ) Pile { _pileType = FoundationPile
                           , _cards    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
canPlace (Card RA _ ) Pile { _pileType = FoundationPile
                           , _cards    = (dc:_)
                           } = False
canPlace (Card r  s ) Pile { _pileType = FoundationPile 
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _suitBias = Just sb
                           } = (pred r == r') && (s == sb)
-- tableau piles only have a rank bias
canPlace (Card r  _ ) Pile { _pileType = TabPile
                           , _cards    = []
                           , _rankBias = Just rb
                           } = r == rb
canPlace (Card RK _ ) Pile { _pileType = TabPile
                           , _cards    = (dc:_)
                           } = False
canPlace (Card r  s ) Pile { _pileType = TabPile
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (toColor s /= toColor s')
canPlace _ _ = False -- base case

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank]

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit]

-- to be randomized on play
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

mkInitS :: [Card] -> GSt
mkInitS deal = GSt { _stock      = stock
                   , _waste      = waste
                   , _tableau    = tableau
                   , _foundation = foundation
                   , _logLine    = "..."
                   }
  where 
    stock      =   Pile { _cards    = [ DCard { _card    = c
                                              , _facedir = FaceDown 
                                              } 
                                      | c <- drop 31 deal 
                                      ]
                        , _display  = Stacked
                        , _rankBias = Nothing
                        , _suitBias = Nothing
                        , _pileType = StockPile
                        }
    waste      =   Pile { _cards    = [ DCard { _card    = c
                                              , _facedir = FaceUp 
                                              } 
                                      | c <- take 3 $ drop 28 deal 
                                      ]
                        , _display  = Sp3
                        , _rankBias = Nothing
                        , _suitBias = Nothing
                        , _pileType = WastePile
                        }
    tableau    = [ Pile { _cards    = [ DCard { _card    = c
                                              , _facedir = d
                                              } 
                                      | (c,d) <- zip cs (FaceUp:repeat FaceDown) 
                                      ]
                        , _display  = Splayed
                        , _rankBias = Just RK
                        , _suitBias = Nothing
                        , _pileType = TabPile
                        } 
                 | cs <- splitPlaces [7,6..1] deal
                 ]
    foundation = [ Pile { _cards    = []
                        , _display  = Stacked
                        , _rankBias = Just RA
                        , _suitBias = Just s
                        , _pileType = FoundationPile
                        } 
                 | s <- allSuits ]

