{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad                (void)
import Graphics.Vty as Vty
import Lens.Micro.TH                (makeLenses)
import qualified System.Random as R (next, newStdGen)

import Brick.AttrMap 
import Brick.Main
import Brick.Types 
import Brick.Util 

import CardTypes
import Movement  (doMove)
import Render    (drawUI)
import Utils     (hasWon, undoMove, newGame, mkInitS)

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------
appEvent :: GSt -> BrickEvent Ext e -> EventM Ext (Next GSt)
appEvent s (VtyEvent e) = case e of
  Vty.EvKey Vty.KEsc        [] -> halt s
  Vty.EvKey (Vty.KChar 'q') [] -> halt s
  Vty.EvMouseDown col row _ _  -> do
    extents <- map extentName <$> findClickedExtents (col, row)
    case extents of 
      [ActionX New]            -> continue $ newGame s
      [ActionX Undo]           -> continue $ undoMove s
      _                        -> if hasWon s
                                    then continue s 
                                    else continue $ doMove s extents
  _                            -> continue s 
appEvent s _                    = continue s

aMap :: AttrMap
aMap = attrMap Vty.defAttr
     [ ( attrName "redCard"   , fg Vty.red   )
     , ( attrName "blackCard" , fg Vty.white )
     , ( attrName "btnAttr"   , withStyle defAttr underline)
     , ( attrName "bold"      , withStyle defAttr bold)
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




