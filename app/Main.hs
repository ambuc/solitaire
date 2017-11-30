{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe             (isNothing, fromJust, isJust)
import Lens.Micro
import Lens.Micro.TH          (makeLenses)
import Graphics.Vty as Vty
import qualified System.Random         as R (next, newStdGen)
import qualified System.Random.Shuffle as R (shuffle')

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
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------
mkButton :: Ext -> String -> Widget Ext
mkButton extent text = reportExtent extent 
                     $ withBorderStyle unicodeRounded $ border $ str text

drawField :: Field -> Widget Ext
drawField f = (stock <=> waste) <+> vBorder <+> tableau <+> vBorder <+> foundation
  where
    stock      = reportExtent StockX $ rrPile     NS $ _stock f
    waste      = reportExtent WasteX $ rrPile     NS $ _waste f
    tableau    = reportExtent TableX $ rrPiles EW NS $ _table f
    foundation = reportExtent FoundX $ rrPiles NS NS $ _found f

drawUI :: GSt -> [ Widget Ext ]
drawUI state = [ui <=> logLine] --drawUI state = [ui] 
  where 
    ui = center -- $ setAvailableSize (120,23)
       $ withBorderStyle unicodeRounded 
       $ borderWithLabel (str title) board <+> btns
    title      = if hasWon state then " Nice! " else " Solitaire "
    board      = drawField $ _field state
    logLine    = str       $ _logLine state
    btns  = vBox [ mkButton NewX "New "
                 ]

appEvent :: GSt -> BrickEvent Ext e -> EventM Ext (Next GSt)
appEvent s (VtyEvent e)        = case e of
  Vty.EvKey Vty.KEsc        [] -> halt s
  Vty.EvKey (Vty.KChar 'q') [] -> halt s
  Vty.EvKey (Vty.KChar n  ) [] -> continue $ s & logLine .~ "KChar " ++ show n
  Vty.EvKey _ _                -> continue $ s & logLine .~ "EvKey"
  Vty.EvResize _ _             -> continue $ s & logLine .~ "EvResize"
  Vty.EvMouseDown col row _ _  -> do
    extents <- map extentName <$> findClickedExtents (col, row)
    case extents of
      [NewX]  -> let seed' = snd $ R.next $ s ^. seed
                 in  continue $ mkInitS seed'
      _       -> if hasWon s
                   then continue s
                   else continue $ s & field %~ move extents
                                     & logLine .~ "EvMouseDown " ++ show extents
  _                            -> continue $ s & logLine .~ "VtyEvent ?"
appEvent s (AppEvent e)        = continue $ s & logLine .~ "AppEvent ?"
appEvent s _                   = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "redCard"   , fg Vty.red   )
     , ( attrName "blackCard" , fg Vty.white )
     , ( attrName "greenField", Vty.white `on` Vty.green )
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
  n <- R.newStdGen
  --let deal = R.shuffle' initialDeal 52 n
  void $ customMain buildVty Nothing app $ mkInitS n 




