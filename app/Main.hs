{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config
import Control.Concurrent (forkIO, threadDelay, threadWaitRead)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Monad (forM_, forever, liftM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Default (Default (def))
import Data.Foldable (concatMap, forM_, mapM_, minimum)
import Data.List (intercalate, intersperse)
import Data.Map (elems)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, mapMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle, hFlush, hPutChar, hPutStr)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Types (Rectangle)
import I3IPC (connecti3, getOutputs, getTree, getWorkspaces)
import qualified I3IPC.Reply as I3
import Lemonbar
import Network.Socket (Socket)
import System.Posix.Types (Fd)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_in, std_out)
import TagPartition (partitionByConstructorTagWith)
import Utils
import Prelude (Either (Right), IO, Int, Show, String, filter, flip, map, return, show, undefined, zip, ($), (++), (.), (<$>), (==))

-- I3 State
type Id = Int

type Index = Int

type Name = String

data Window = Window Id Name deriving (Show) -- more bools here

data Workspace = Workspace Index Name [Window] deriving (Show)

data Output = Output Index Name [Workspace] deriving (Show)

newtype I3State = I3State [Output]

newtype I3PrivateState = I3PrivateState Socket

-- Time State
newtype TimeState = TimeState ZonedTime

data State = State
  { timeState :: Maybe TimeState,
    i3State :: Maybe I3State
  }

type SharedState = MVar State

createSharedState :: IO SharedState
createSharedState = newMVar $ State Nothing Nothing

-- Monitor setup
newtype Monitor = Monitor Index

-- I3 Printer
printI3 :: I3State -> I3Config -> String
printI3 _ _ = "TODO"

-- Time printer.
printTime :: TimeState -> TimeConfig -> String
printTime (TimeState time) (TimeConfig timeFormat) = formatTime defaultTimeLocale timeFormat time

printStateItem :: Monitor -> State -> ItemConfig -> String
printStateItem monitor State {i3State = Just i3State} (ItemConfig _ _ (I3 i3Config)) = printI3 i3State i3Config
printStateItem monitor State {timeState = Just timeState} (ItemConfig _ _ (Time timeConfig)) = printTime timeState timeConfig
printStateItem _ _ _ = "Loading..."

printState :: Monitor -> BarConfig -> State -> String
printState monitor@(Monitor index) (BarConfig l c r) state = tag ('S' : show index) allSections
  where
    allSections = printSection "l" l ++ printSection "c" c ++ printSection "r" r
    printSection :: Tag -> [ItemConfig] -> String
    printSection t [] = ""
    printSection t sources@(_ : _) = tag t $ printMany sources
    printMany :: [ItemConfig] -> String
    printMany = concatMap $ printStateItem monitor state

type PingChannel = Chan ()

waitPing :: PingChannel -> IO ()
waitPing = readChan

sendPing :: PingChannel -> IO ()
sendPing = flip writeChan ()

forkPrintStateLoop :: [Rectangle] -> BarConfig -> SharedState -> PingChannel -> Handle -> IO ()
forkPrintStateLoop screenInfo config sharedState pingChannel outputHandle =
  let monitors = [Monitor index | (_, index) <- zip screenInfo [0 ..]]
   in forkForever $ do
        waitPing pingChannel
        withMVar sharedState $ \state -> forM_ monitors $ \monitor -> do
          hPutStr outputHandle $ printState monitor config state
        hPutChar outputHandle '\n'
        hFlush outputHandle

initI3PrivateState :: IO I3PrivateState
initI3PrivateState = I3PrivateState <$> connecti3

i3NodeToOutputs :: I3.Node -> [Output]
i3NodeToOutputs I3.Node {I3.node_name = Just "root", I3.node_type = I3.RootType, I3.node_nodes = nodes} = concatMap i3NodeToOutputs nodes
i3NodeToOutputs I3.Node {I3.node_name = Just "__i3", I3.node_type = I3.OutputType} = []
i3NodeToOutputs node@I3.Node {I3.node_name = Just (Text.unpack -> name), I3.node_type = I3.OutputType} = [Output 0 name $ i3NodeToWorkspaces node]
i3NodeToOutputs _ = undefined

i3NodeToWorkspaces :: I3.Node -> [Workspace]
i3NodeToWorkspaces I3.Node {I3.node_type = I3.OutputType, I3.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWorkspaces nodes
i3NodeToWorkspaces I3.Node {I3.node_type = I3.DockAreaType} = []
i3NodeToWorkspaces I3.Node {I3.node_type = I3.ConType, I3.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWorkspaces nodes
i3NodeToWorkspaces node@I3.Node {I3.node_name = Just (Text.unpack -> name), I3.node_type = I3.WorkspaceType} = [Workspace 0 name $ i3NodeToWindows node]
i3NodeToWorkspaces _ = undefined

i3NodeToWindows :: I3.Node -> [Window]
i3NodeToWindows I3.Node {I3.node_type = I3.WorkspaceType, I3.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWindows nodes
i3NodeToWindows I3.Node {I3.node_name = Nothing, I3.node_type = I3.ConType, I3.node_nodes = (Vector.toList -> nodes@(_ : _))} = concatMap i3NodeToWindows nodes
i3NodeToWindows I3.Node {I3.node_id = id, I3.node_name = Just (Text.unpack -> name), I3.node_type = I3.ConType, I3.node_nodes = (Vector.toList -> [])} = [Window id name]
i3NodeToWindows _ = undefined

readI3Tree :: Socket -> IO [Output]
readI3Tree socket = do
  -- (Right (I3.Outputs (I3.OutputsReply outputs))) <- getOutputs socket
  -- (output_name output) (output_active output) (output_primary output)
  -- (Right (I3.Workspaces (I3.WorkspaceReply workspaces))) <- getWorkspaces socket
  -- (fromIntegral $ ws_num workspace) (ws_name workspace) (ws_visible workspace) (ws_focused workspace) (ws_urgent workspace) "asd" -- (ws_output workspace)
  (Right (I3.Tree rootNode)) <- getTree socket

  -- let buildWorkspace :: I3.Workspace -> Workspace
  --     buildWorkspace i3Workspace = undefined
  --
  --     buildOutput :: I3.Output -> Output
  --     buildOutput i3Output = Output undefined outputName $ map buildWorkspace matchingWorkspaces
  --       where
  --         outputName = Text.unpack $ I3.output_name i3Output
  --         matchingWorkspaces = filter ((== outputName) . Text.unpack . I3.ws_output) $ Vector.toList workspaces

  return $ i3NodeToOutputs rootNode

-- data Window = Window Id Name -- more bools here
--
-- data Workspace = Workspace Index Name [Window]
--
-- data Output = Output Index Name [Workspace]

updateI3 :: [ItemConfig] -> I3PrivateState -> SharedState -> PingChannel -> IO ()
updateI3 configs (I3PrivateState socket) sharedState pingChannel = do
  oldState <- readMVar sharedState
  case i3State oldState of
    (Just _) -> return ()
    Nothing -> do
      tree <- readI3Tree socket
      modifyMVar_ sharedState $ \state -> return $ state {i3State = Just $ I3State tree}

  sendPing pingChannel
  threadDelay $ seconds 1000

updateTime :: [ItemConfig] -> SharedState -> PingChannel -> IO ()
updateTime configs sharedState pingChannel = do
  zonedTime <- getZonedTime
  modifyMVar_ sharedState $ \state -> return $ state {timeState = Just $ TimeState zonedTime}

  sendPing pingChannel
  threadDelay $ seconds 30

forkUpdateStateLoops :: BarConfig -> SharedState -> PingChannel -> IO ()
forkUpdateStateLoops (BarConfig l c r) sharedState pingChannel =
  let getItem (ItemConfig _ _ x) = x
      partitioned = partitionByConstructorTagWith getItem $ l ++ c ++ r
      forkPerGroup :: [ItemConfig] -> IO ()
      forkPerGroup [] = undefined
      forkPerGroup configs@(ItemConfig _ _ (I3 _) : _) = do
        privateState <- initI3PrivateState
        updateI3 configs privateState sharedState pingChannel
      forkPerGroup configs@(ItemConfig _ _ (Time _) : _) = updateTime configs sharedState pingChannel
   in mapM_ (forkForever . forkPerGroup) $ elems partitioned

main :: IO ()
main = do
  display <- openDisplay ":0"
  screenInfo <- getScreenInfo display
  -- All data stored here, static in config, dynamic in state.
  config <- readConfig
  sharedState <- createSharedState

  (lemonbarInput, lemonbarOutput, processHandle) <- launchLemonbar

  -- The updater notifies the printer via the pingchannel.
  pingChannel <- newChan

  forkUpdateStateLoops config sharedState pingChannel
  forkPrintStateLoop screenInfo config sharedState pingChannel lemonbarInput

  -- Replace this with last shell command.
  forever $ threadDelay $ seconds 100
