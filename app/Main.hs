{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad                (void)
import Control.Monad.IO.Class       (liftIO)
import Data.Maybe                   (isNothing, fromJust, isJust, fromMaybe)
import Lens.Micro
import Lens.Micro.TH                (makeLenses)
import Graphics.Vty as Vty
import qualified System.Random as R (next, newStdGen)

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

mkButton :: Action -> Widget Ext
mkButton action = reportExtent (ActionX action)
                -- $ withBorderStyle unicode $ border 
                $ padBottom (Pad 1)
                $ withAttr (attrName "btnAttr")
                $ str (show action)

scoreBox :: Int -> Widget Ext
scoreBox i = padBottom (Pad 1)
           $ (str "Score: ")
         <+> (withAttr (attrName "scoreAttr") $ str $ show i)

movesBox :: Int -> Widget Ext
movesBox i = padBottom (Pad 1)
           $ (str "Moves: ")
         <+> (withAttr (attrName "scoreAttr") $ str $ show i)

drawUI :: GSt -> [ Widget Ext ]
drawUI state = [ui <=> logLine]
  where 
    ui = center $ setAvailableSize (120,29) 
       $ board <+> rSidebar
    title      = if hasWon state then " Nice! " else " Solitaire "
    board      = withBorderStyle unicodeRounded
               $ borderWithLabel (str title) 
               $ drawField $ _field state
    logLine    = str       $ _logLine state
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
      [ActionX New]            -> continue $ newGame s
      [ActionX Undo]           -> continue $ undoMove s
      _                        -> if hasWon s
                                    then continue $ s & logLine .~ "already won"
                                    else continue $ doMove s extents
  _                            -> continue $ s & logLine .~ "VtyEvent ?"
appEvent s (AppEvent e)         = continue $ s & logLine .~ "AppEvent ?"
appEvent s _                    = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "redCard"   , fg Vty.red   )
     , ( attrName "blackCard" , fg Vty.white )
     , ( attrName "btnAttr"   , withStyle defAttr underline)
     , ( attrName "scoreAttr" , withStyle defAttr bold)
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




