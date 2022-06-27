{-# LANGUAGE NoImplicitPrelude #-}

module Print.I3 where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans.Class (lift)
import Config (ItemParams)
import Control.Monad (forM_, return)
import Data.Bool (Bool (False, True))
import Data.Eq ((==))
import Data.Function (($))
import Data.List ((!!))
import Data.Maybe (Maybe (Just, Nothing))
import qualified Lemonbar as L
import qualified State.I3 as I3
import Utils.Types (Id, Index, Name)

workspaceColor :: I3.Focused -> I3.Focused -> L.ColorPair
workspaceColor _ False = L.unfocusedColorPair
workspaceColor False True = L.semifocusedColorPair
workspaceColor True True = L.focusedColorPair

windowColor :: I3.Focused -> I3.Focused -> I3.Focused -> I3.Urgent -> L.ColorPair
windowColor _ _ _ True = L.urgentColorPair
windowColor _ False _ False = L.unfocusedColorPair
windowColor _ _ False False = L.unfocusedColorPair
windowColor False True True False = L.semifocusedColorPair
windowColor True True True False = L.focusedColorPair

printWorkspace :: I3.Focused -> Name -> Maybe Id -> I3.Workspace -> L.Powerlemon
printWorkspace outFocused focusedWorkspace focusedWindow (I3.Workspace _ wspName windows) = do
  L.setStyle L.Round
  let wspFocused = focusedWorkspace == wspName
  L.openSection $ workspaceColor outFocused wspFocused
  L.write wspName
  forM_ windows $ \(I3.Window wndId wndName urgent) -> do
    let wndFocused = focusedWindow == Just wndId
        colorPair = windowColor outFocused wspFocused wndFocused urgent
    L.openSection colorPair
    L.write $ L.processWindowTitle wndName
  L.closeSection

printOutput :: I3.Output -> L.Powerlemon
printOutput (I3.Output _ _ isFocused focusedWorkspace focusedWindow workspaces) = forM_ workspaces $ printWorkspace isFocused focusedWorkspace focusedWindow

printI3 :: MVar I3.I3State -> ItemParams -> Index -> L.Powerlemon
printI3 shared _ monitorIndex = L.withMVar shared $ \(I3.I3State outputs) -> printOutput $ outputs !! monitorIndex
