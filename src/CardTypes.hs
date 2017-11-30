module CardTypes
  ( Rank(..) 
  , Suit(..)
  , Color(..)
  , Card(..)
  , FaceDir(..)
  , DCard(..)
  , DisplayMode(..)
  , PileType(..)
  , Pile(..)
  , Field(..)
  , GSt(..)
  , Axis(..)
  , Ext(..)
  ) where

import qualified System.Random as R (StdGen)

data Rank    = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = [toEnum 0x2491] :: String; -- ;)
  show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

data Suit    = Spade | Heart | Club | Diamond 
  deriving (Eq, Ord, Bounded, Enum)
instance Show Suit where
  show Spade   = [toEnum 0x2660] :: String
  show Heart   = [toEnum 0x2665] :: String
  show Diamond = [toEnum 0x2666] :: String 
  show Club    = [toEnum 0x2663] :: String

data Color       = Red | Black                       deriving (Eq, Show, Ord)
data Card        = Card Rank Suit                    deriving (Eq, Show, Ord)
data FaceDir     = FaceUp | FaceDown                 deriving (Eq, Show, Ord)
data DCard       = DCard { _card :: Card
                         , _facedir :: FaceDir }     deriving (Eq, Show, Ord)
data DisplayMode = Stacked | Splayed | Sp3           deriving (Eq, Show, Ord)
data PileType    = StockP | WasteP | TableP | FoundP deriving (Eq, Show, Ord)

-- Piles are opinionated about whether they're stacked or splayed, and whether
-- or not they are biased on a per-rank and per-suit basis. The display of each
-- card is up to the card itself.
data Pile = Pile { _cards    :: [DCard]
                 , _display  :: DisplayMode
                 , _rankBias :: Maybe Rank
                 , _suitBias :: Maybe Suit
                 , _pileType :: PileType
                 } deriving (Eq, Show)

-- Field
data Field = Field { _stock :: Pile
                   , _waste :: Pile
                   , _table :: [Pile]
                   , _found :: [Pile]
                   } deriving (Eq, Show)

-- GameState
data GSt = GSt { _field   :: Field
               , _logLine :: String
               , _seed    :: R.StdGen
               } deriving (Show)

-- data types for display
data Axis = NS | EW deriving (Eq, Show)

-- Extents
data Ext = StockX | WasteX | TableX | FoundX | IdX Int | DCX DCard 
         | NewX | UndoX | OkX
  deriving (Eq, Show, Ord)
