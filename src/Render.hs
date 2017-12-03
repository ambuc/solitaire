module Render
  ( drawUI
  ) where

import Brick.AttrMap              (attrName)
import Brick.Types                (Widget, Padding(Pad))
import Brick.Widgets.Border       (vBorder, border, borderWithLabel)
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center       (center)
import Brick.Widgets.Core
import Text.Printf                (printf)

import CardTypes
import Utils (toColor, hasWon)

-------------------------------------------------------------------------------

cardStyle :: Widget Ext -> Widget Ext -- borderwithstyle wrapper
cardStyle = withBorderStyle unicodeRounded . border 

rrGhost :: Pile -> Widget Ext -- renders a 'ghost' card with no content
rrGhost _ = withBorderStyle ghostRounded $ border $ str "  "
  where ghostRounded = BorderStyle 
          { bsIntersectFull = toEnum 0x253C
          , bsCornerTL      = toEnum 0x256D , bsCornerTR      = toEnum 0x256E
          , bsCornerBR      = toEnum 0x256F , bsCornerBL      = toEnum 0x2570
          , bsIntersectL    = toEnum 0x251C , bsIntersectR    = toEnum 0x2524
          , bsIntersectT    = toEnum 0x252C , bsIntersectB    = toEnum 0x2534
          , bsHorizontal    = ' '           , bsVertical      = ' '
          }

rrDCard :: Axis -> Int -> DCard -> Widget Ext -- renders a displaycard.
rrDCard axis idx dc = reportExtent (DCX dc)   -- by necessity displaycards
                    $ cropBy margin           -- are aware of their position
                    $ rrCard inner            -- within a splayed pile.
  where cropBy = if axis == NS then cropBottomBy else cropRightBy 
        margin = mkMargin axis idx (_facedir dc)
          where mkMargin :: Axis -> Int -> FaceDir -> Int
                mkMargin _  0 _        = 0
                mkMargin NS _ FaceUp   = 1
                mkMargin NS _ FaceDown = 2
                mkMargin EW _ FaceUp   = 1
                mkMargin EW _ FaceDown = 3
        inner  = if _facedir dc == FaceDown then Nothing else Just (_card dc)

rrCard :: Maybe Card -> Widget Ext               -- renders card internals
rrCard Nothing           = withAttr (attrName "bold")
                         $ cardStyle             -- either a card back
                         $ str ([toEnum 0x03BB, '='] :: String)
rrCard (Just (Card r s)) = withAttr (attrName c) -- or a card front.
                         $ cardStyle $ str $ show r ++ show s
  where c = if Red == toColor s then "redCard" else "blackCard"

rrDCards :: Axis -> [DCard] -> Widget Ext -- renders a pile of displaycards
rrDCards axis dcs = nBox [ reportExtent (IdX idx) 
                            $ rrDCard axis idx dc 
                         | (idx,dc) <- zip idxs dcs 
                         ]
  where nBox = if axis == NS then vBox else hBox -- aware of splay axis
        idxs = reverse [0..(length dcs - 1)]

rrPile :: Axis -> Pile -> Widget Ext -- renders a pile of cards
rrPile axis p
  | null (_cards p)        = rrGhost p -- don't render an empty pile
  | _display p == Stacked  = rrDCard NS 0 (head $ _cards p) 
  | _display p == Sp3      = rrDCards axis $ reverse $ take 3 $ _cards p
  | _display p == Splayed  = rrDCards axis $ reverse $          _cards p
  | otherwise              = str "!!" -- shouldn't happen

rrPiles :: Axis -> Axis -> [Pile] -> Widget Ext -- renders a list of piles
rrPiles ax ax' ps = nBox [ reportExtent (IdX idx) $ rrPile ax' p 
                         | (p,idx) <- zip ps [0..] 
                         ]
  where nBox = if ax == NS then vBox else hBox -- along its own 2ndary axis


mkButton :: Action -> Widget Ext
mkButton action = reportExtent (ActionX action)
                $ padBottom (Pad 1)
                $ str "[" <+> 
                ( withAttr (attrName "btnAttr")
                $ str (show action)
                ) <+> str "]"

scoreBox :: Int -> Widget Ext
scoreBox i = padBottom (Pad 1)
           $ str "Score: "
         <+> withAttr (attrName "bold") (str $ printf "%3d" i)

movesBox :: Int -> Widget Ext
movesBox i = padBottom (Pad 1)
           $ str "Moves: "
         <+> withAttr (attrName "bold") (str $ printf "%3d" i)

drawUI :: GSt -> [ Widget Ext ]
drawUI state = [ui]
  where 
    ui = center $ setAvailableSize (120,29) 
       $ board <+> rSidebar
    title      = if hasWon state then " Nice! " else " Solitaire "
    board      = withBorderStyle unicodeRounded
               $ borderWithLabel (str title) 
               $ drawField $ _field state
    rSidebar   = padAll 1
               $ scoreBox (_score state)
             <=> movesBox (_moves state)
             <=> vBox (map mkButton [New, Undo])

    drawField :: Field -> Widget Ext
    drawField f = (stock <=> waste) <+> vBorder <+> tableau 
                                    <+> vBorder <+> foundation
      where stock      = reportExtent StockX $ rrPile     NS $ _stock f
            waste      = reportExtent WasteX $ rrPile     NS $ _waste f
            tableau    = reportExtent TableX $ rrPiles EW NS $ _table f
            foundation = reportExtent FoundX $ rrPiles NS NS $ _found f

