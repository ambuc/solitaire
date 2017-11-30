module Render
  ( rrPile
  , rrPiles
  ) where

import Brick.AttrMap 
import Brick.Types 
import Brick.Widgets.Border 
import Brick.Widgets.Border.Style 
import Brick.Widgets.Core 

import CardTypes
import Utils

-------------------------------------------------------------------------------

cardStyle :: Widget Ext -> Widget Ext
cardStyle = withBorderStyle unicodeRounded . border

rrGhost :: Pile -> Widget Ext
rrGhost _ = withBorderStyle ghostRounded $ border $ str "  "
  where ghostRounded = BorderStyle 
          { bsIntersectFull = toEnum 0x253C
          , bsCornerTL      = toEnum 0x256D , bsCornerTR      = toEnum 0x256E
          , bsCornerBR      = toEnum 0x256F , bsCornerBL      = toEnum 0x2570
          , bsIntersectL    = toEnum 0x251C , bsIntersectR    = toEnum 0x2524
          , bsIntersectT    = toEnum 0x252C , bsIntersectB    = toEnum 0x2534
          , bsHorizontal    = ' '           , bsVertical      = ' '
          }

rrDCard :: Axis -> Int -> DCard -> Widget Ext
rrDCard axis idx dc = reportExtent (DCX dc) $ cropBy margin $ rrCard inner
  where 
    cropBy = if axis == NS then cropBottomBy else cropRightBy 
    margin = mkMargin axis idx (_facedir dc)
      where
        --          NS/EW   idx    up/down    margin
        mkMargin :: Axis -> Int -> FaceDir -> Int
        mkMargin _  0 _        = 0
        mkMargin NS _ FaceUp   = 1
        mkMargin NS _ FaceDown = 2
        mkMargin EW _ FaceUp   = 1
        mkMargin EW _ FaceDown = 3
    inner  = if _facedir dc == FaceDown then Nothing else Just (_card dc)
    rrCard :: Maybe Card -> Widget Ext
    rrCard Nothing           = cardStyle $ str "<>"
    rrCard (Just (Card r s)) = withAttr (attrName c) 
                             $ cardStyle $ str $ show r ++ show s
      where c = if Red == toColor s then "redCard" else "blackCard"

rrDCards :: Axis -> [DCard] -> Widget Ext
rrDCards axis dcs = nBox [ reportExtent (IdX idx) $ rrDCard axis idx dc 
                         | (idx,dc) <- zip idxs dcs 
                         ]
  where nBox = if axis == NS then vBox else hBox
        idxs = reverse [0..(length dcs - 1)]

rrPile :: Axis -> Pile -> Widget Ext
rrPile axis p
  | null (_cards p)        = rrGhost p                      -- else, non-null
  | _display p == Stacked  = rrDCard NS 0 (head $ _cards p) -- else, splayed
  | _display p == Sp3      = rrDCards axis $ reverse $ take 3 $ _cards p
  | _display p == Splayed  = rrDCards axis $ reverse $          _cards p
  | otherwise              = rrGhost p                      -- shouldn't happen

rrPiles :: Axis -> Axis -> [Pile] -> Widget Ext
rrPiles ax ax' ps = nBox [ reportExtent (IdX idx) $ rrPile ax' p 
                         | (p,idx) <- zip ps [0..] 
                         ]
  where nBox = if ax == NS then vBox else hBox
