module CardTypes
  ( Action(..)
  , Axis(..)
  , Card(..)
  , Color(..)
  , DCard(..)
  , DisplayMode(..)
  , Ext(..)
  , FaceDir(..)
  , Field(..)
  , GSt(..)
  , Pile(..)
  , PileType(..)
  , Rank(..) 
  , Suit(..)
  ) where

import qualified System.Random as R (StdGen)

-- CARD TYPES ------------------------------------------------------------------

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = [toEnum 0x2491] :: String; -- unicode ligature for one-char width 
  show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

data Suit    = Spade | Heart | Club | Diamond 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Suit where
  show Spade   = [toEnum 0x2660] :: String -- unicode characters for suits
  show Heart   = [toEnum 0x2665] :: String
  show Diamond = [toEnum 0x2666] :: String 
  show Club    = [toEnum 0x2663] :: String

data Card        = Card Rank Suit                    deriving (Eq, Show, Ord)

data FaceDir     = FaceUp | FaceDown                 deriving (Eq, Show, Ord)

data DCard       = DCard { _card    :: Card
                         , _facedir :: FaceDir }     deriving (Eq, Show, Ord)

data DisplayMode = Stacked | Splayed | Sp3           deriving (Eq, Show, Ord)

data PileType    = StockP | WasteP | TableP | FoundP deriving (Eq, Show, Ord)

data Pile = Pile { _cards    :: [DCard]     --   piles contain cards
                 , _display  :: DisplayMode -- , opinions on how to be drawn
                 , _rankBias :: Maybe Rank  -- , possibly opinions on base rank
                 , _suitBias :: Maybe Suit  -- , possibly opinions on base suit
                 , _pileType :: PileType    -- , and an identifier for location
                 } deriving (Eq, Show)      -- , since it makes canPlace simpler

-- GAME TYPES ------------------------------------------------------------------

data Field = Field { _stock :: Pile      -- fields are game wrappers for the
                   , _waste :: Pile      -- four board components
                   , _table :: [Pile]
                   , _found :: [Pile]
                   } deriving (Eq, Show)

                                      --   the gamestate is a record for the
data GSt = GSt { _field   :: Field    --   field as seen above
               , _seed    :: R.StdGen -- , and a random seed to be passed thru
               , _history :: [(Field, Int)]  -- , and a list of previous fields
               , _score   :: Int
               , _moves   :: Int
               } deriving (Show)

-- DISPLAY TYPES ---------------------------------------------------------------

data Color       = Red | Black                       deriving (Eq, Show, Ord)

data Axis = NS | EW deriving (Eq, Show) -- data type for pile splay orientation

data Action = New | Undo deriving (Eq, Show, Ord) -- data type for button action

data Ext = StockX | WasteX | TableX | FoundX -- named extents for click regions
         | IdX Int | DCX DCard | ActionX Action
  deriving (Eq, Show, Ord)
