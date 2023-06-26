{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Print.I3 (print) where

import Control.Concurrent.MVar (MVar)
import Data.Map.Strict qualified as Map
import Lemonbar qualified as L
import State.I3 (Focused, I3State (I3State, i3FocusedOutput, i3OutputFocusedWorkspaces, i3OutputsOrder, i3Workspaces, i3OutputWorkspaces), Output, Urgent, Window (Window), Workspace (Workspace), WorkspaceName)
import Utils.BiMultiMap qualified as BiMultiMap
import Utils.Color qualified as C
import Utils.MVar (withMVar)
import Utils.Types (Index, Name)
import Utils.WindowTitle (processWindowTitle)
import Prelude

workspaceColor :: Focused -> Focused -> Urgent -> C.ColorPair
workspaceColor _ _ True = C.urgentColorPair
workspaceColor _ False _ = C.unfocusedColorPair
workspaceColor False _ _ = C.semifocusedColorPair
workspaceColor True _ _ = C.neutralColorPair

windowColor :: Focused -> Focused -> Focused -> Urgent -> C.ColorPair
windowColor _ _ _ True = C.urgentColorPair
windowColor _ _ False _ = C.unfocusedColorPair
windowColor _ False _ _ = C.semifocusedColorPair
windowColor False _ _ _ = C.semifocusedColorPair
windowColor True _ _ _ = C.focusedColorPair

printWorkspace :: I3State -> WorkspaceName -> L.Powerlemon
printWorkspace state workspaceName = do
  L.write workspaceName

-- L.setStyle L.Round
-- let wspFocused = focusedWorkspace == wspName
-- L.openSection $ workspaceColor outputFocused wspFocused wspUrgent
-- L.write $ ' ' : wspName
-- forM_ wspWindows $ \(Window wndId wndName urgent) -> do
--   let wndFocused = focusedWindow == Just wndId
--       colorPair = windowColor outputFocused wspFocused wndFocused urgent
--   L.openSection colorPair
--   L.write $ ' ' : processWindowTitle wndName
-- L.closeSection

printOutput :: I3State -> Index -> L.Powerlemon
printOutput
  state@I3State
    { i3FocusedOutput = focusedOutput,
      i3OutputsOrder = outputsOrder,
      i3OutputFocusedWorkspaces = outputFocusedWorkspace,
      i3OutputWorkspaces = outputWorkspaces
    }
  monitorIndex =
    let currentOutputName = outputsOrder !! monitorIndex
        focusedIndex = fromJust $ elemIndex focusedOutput outputsOrder
        focusedWorkspaceName = fromJust $ Map.lookup focusedOutput outputFocusedWorkspace
        localWorkspaces = fromJust $ BiMultiMap.lookup currentOutputName outputWorkspaces
     in do
          when (focusedIndex < monitorIndex) $ do
            L.setDirection L.West
            L.setStyle L.Common
            L.openSection C.focusedColorPair
            L.openSection C.neutralColorPair
            L.write focusedWorkspaceName
            L.closeSection
            L.setDirection L.East

          forM_ localWorkspaces $ printWorkspace state

          when (focusedIndex > monitorIndex) $ do
            L.setStyle L.Common
            L.openSection C.neutralColorPair
            L.write focusedWorkspaceName
            L.openSection C.focusedColorPair
            L.closeSection

print :: MVar I3State -> Index -> L.Powerlemon
print shared monitorIndex = withMVar shared (`printOutput` monitorIndex)
