{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State.I3
  ( Focused,
    Urgent,
    WindowId,
    Window (Window, wndId, wndName, wndUrgent),
    WorkspaceName,
    Workspace (Workspace, wspName, wspUrgent),
    OutputName,
    Position,
    Output (Output, outPosition, outName),
    I3State
      ( I3State,
        i3FocusedOutput,
        i3Outputs,
        i3OutputsOrder,
        i3OutputWorkspaces,
        i3OutputFocusedWorkspaces,
        i3Workspaces,
        i3WorkspaceWindows,
        i3WorkspaceFocusedWindows,
        i3Windows
      ),
    empty,
    init,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Map.Strict qualified as Map
import Utils.BiMultiMap qualified as BiMultiMap
import Utils.Types (Id, Name)
import Prelude

type Focused = Bool

type Urgent = Bool

type WindowId = Id

data Window = Window {wndId :: WindowId, wndName :: Name, wndUrgent :: Urgent} deriving stock (Show)

type WorkspaceName = Name

data Workspace = Workspace {wspName :: WorkspaceName, wspUrgent :: Urgent} deriving stock (Show)

type OutputName = Name

type Position = (Int, Int)

data Output = Output {outName :: OutputName, outPosition :: Position} deriving stock (Show)

instance Eq Output where
  (==) Output {outPosition = a} Output {outPosition = b} = a == b

instance Ord Output where
  -- Outputs are orderable by their XY positions. That's how they are indexed in lemonbar.
  (<=) Output {outPosition = a} Output {outPosition = b} = a <= b

data I3State = I3State
  { i3FocusedOutput :: OutputName,
    i3Outputs :: Map.Map OutputName Output,
    i3OutputsOrder :: [OutputName],
    i3OutputWorkspaces :: BiMultiMap.BiMultiMap OutputName WorkspaceName,
    i3OutputFocusedWorkspaces :: Map.Map OutputName WorkspaceName,
    i3Workspaces :: Map.Map WorkspaceName Workspace,
    i3WorkspaceWindows :: BiMultiMap.BiMultiMap WorkspaceName WindowId,
    i3WorkspaceFocusedWindows :: Map.Map WorkspaceName WindowId,
    i3Windows :: Map.Map WindowId Window
  }
  deriving stock (Show)

empty :: I3State
empty = I3State "" Map.empty [] BiMultiMap.empty Map.empty Map.empty BiMultiMap.empty Map.empty Map.empty

init :: IO (MVar I3State)
init = newMVar empty
