{-# LANGUAGE NoImplicitPrelude #-}

module State.I3 where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Bool (Bool)
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Ord (Ord ((<=)))
import Data.String (String)
import System.IO (IO)
import Text.Show (Show)
import Utils.Types (Id, Index, Name)

type Focused = Bool

type Urgent = Bool

data Window = Window
  { wndId :: Id,
    wndName :: Name,
    wndUrgent :: Urgent
  }
  deriving (Show)

data Workspace = Workspace
  { wspIndex :: Index,
    wspName :: Name,
    wspWindows :: [Window]
  }
  deriving (Show)

type Position = (Int, Int)

data Output = Output
  { outPosition :: Position,
    outName :: Name,
    outFocused :: Bool,
    outFocusedWorkspace :: Name,
    outFocusedWindow :: Maybe Id,
    outWorkspaces :: [Workspace]
  }
  deriving (Show)

instance Eq Output where
  (==) Output {outPosition = a} Output {outPosition = b} = a == b

instance Ord Output where
  -- Outputs are orderable by their XY positions. That's how they are indexed in lemonbar.
  (<=) Output {outPosition = a} Output {outPosition = b} = a <= b

newtype I3State = I3State [Output] deriving (Show)

initI3State :: IO (MVar I3State)
initI3State = newMVar $ I3State []
