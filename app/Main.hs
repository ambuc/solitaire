{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Lens.Micro
import Lens.Micro.TH          (makeLenses)
import Graphics.Vty as Vty

import Brick.AttrMap 
import Brick.Main
import Brick.Types 
import Brick.Util 
import Brick.Widgets.Border 
import Brick.Widgets.Border.Style 
import Brick.Widgets.Center 
import Brick.Widgets.Core 

import CardTypes
import Render
import Movement
import Utils

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''GSt

-------------------------------------------------------------------------------

drawUI :: GSt -> [ Widget Ext ]
drawUI state = [ui] --drawUI state = [ui <=> logLine]
  where 
    ui = center -- $ setAvailableSize (120,23)
       $ withBorderStyle unicodeRounded 
       $ borderWithLabel (str " Solitaire ") board 
    board = (stock <=> waste) <+> vBorder <+> tableau <+> vBorder <+> foundation
    stock      = reportExtent StockX      $ rrPile     NS $ _stock      state
    waste      = reportExtent WasteX      $ rrPile     NS $ _waste      state
    tableau    = reportExtent TabX        $ rrPiles EW NS $ _tableau    state
    foundation = reportExtent FoundationX $ rrPiles NS NS $ _foundation state
    logLine    = str (_logLine state)

appEvent :: GSt -> BrickEvent Ext e -> EventM Ext (Next GSt)
appEvent s (VtyEvent e) =
  case e of
    Vty.EvKey Vty.KEsc        [] -> halt s
    Vty.EvKey (Vty.KChar 'q') [] -> halt s
    Vty.EvKey (Vty.KChar n  ) [] -> continue $ s & logLine .~ "KChar " ++ show n
    Vty.EvKey _ _                -> continue $ s & logLine .~ "EvKey"
    Vty.EvResize _ _             -> continue $ s & logLine .~ "EvResize"
    Vty.EvMouseDown col row _ _  -> do
      extents <- map extentName <$> findClickedExtents (col, row)
      continue $ alter extents s
    _                            -> continue $ s & logLine .~ "VtyEvent ?"
appEvent s (AppEvent e)           = continue $ s & logLine .~ "AppEvent ?"
appEvent s _                      = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "redCard"   , fg Vty.red   )
     , ( attrName "blackCard" , fg Vty.white )
     , ( attrName "cardBack"  , Vty.white `on` Vty.white )
     ]

-- App <application  state> <event> <resource name>
app :: App GSt e Ext
app = App { appDraw         = drawUI          -- s -> [Widget n]
          , appChooseCursor = showFirstCursor -- s -> [CursorLocation n] 
                                              --   -> Maybe (CursorLocation n)
          , appHandleEvent  = appEvent        -- s -> BrickEvent n e 
                                              --   -> EventM n (Next s)
          , appStartEvent   = return          -- s -> EventM n s
          , appAttrMap      = const aMap      -- s -> AttrMap
          }

main :: IO ()
main = do
  let buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
  --     customMain (IO Vty) (?BChan)(App) ( init state )
  void $ customMain buildVty Nothing app $ mkInitS initialDeal
