{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config
import Control.Concurrent (forkIO, threadDelay, threadWaitRead)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Monad (forM_, forever, liftM, return, void, (>=>), (>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Bool (Bool (False, True))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Foldable (concatMap, forM_, mapM_, minimum)
import Data.Function (const, flip, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (filter, intercalate, map, sort, zip, zipWith, (!!), (++))
import Data.Map (elems)
import Data.Maybe (Maybe (Just, Nothing), fromJust, fromMaybe, mapMaybe)
import Data.Ord (Ord ((<=)))
import Data.Semigroup ((<>))
import Data.String (String, unwords)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle, hFlush, hPutChar, hPutStr)
import GHC.Int (Int (I#), Int32 (I32#))
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Types (Rectangle)
import I3IPC as I3
import qualified I3IPC.Event as I3Evt
import qualified I3IPC.Reply as I3Rpl
import qualified I3IPC.Subscribe as I3Sub
import qualified Lemonbar as L
import Network.Socket (Socket)
import System.IO (IO)
import System.Posix.Types (Fd)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_in, std_out)
import TagPartition (partitionByConstructorTagWith)
import Text.Show (Show, show)
import Utils (debounce, forkForever, mapWhere, millis, seconds, ($-))
import Prelude (error, undefined)

-- I3 State
type Id = Int

type Index = Int

type Name = String

type Focused = Bool

type Urgent = Bool

data Window = Window
  { wndId :: Id,
    wndName :: Name,
    wndUrgent :: Urgent
  }
  deriving (Show) -- more bools here

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

newtype I3PrivateState = I3PrivateState Socket

-- Time State
newtype TimeState = TimeState ZonedTime deriving (Show)

data State = State
  { timeState :: Maybe TimeState,
    i3State :: Maybe I3State
  }
  deriving (Show)

type SharedState = MVar State

modifyState :: SharedState -> (State -> State) -> IO ()
modifyState sharedState transform = modifyMVar_ sharedState $ return . transform

modifyI3State :: SharedState -> (I3State -> I3State) -> IO ()
modifyI3State sharedState transform = modifyState sharedState $ \state -> state {i3State = fmap transform $ i3State state}

createSharedState :: IO SharedState
createSharedState = newMVar $ State Nothing Nothing

workspaceColor :: Focused -> Focused -> L.ColorPair
workspaceColor _ False = L.unfocusedColorPair
workspaceColor False True = L.semifocusedColorPair
workspaceColor True True = L.focusedColorPair

windowColor :: Focused -> Focused -> Focused -> Urgent -> L.ColorPair
windowColor _ _ _ True = L.urgentColorPair
windowColor _ False _ False = L.unfocusedColorPair
windowColor _ _ False False = L.unfocusedColorPair
windowColor False True True False = L.semifocusedColorPair
windowColor True True True False = L.focusedColorPair

printWorkspace :: Focused -> Name -> Maybe Id -> Workspace -> L.Powerlemon
printWorkspace outFocused focusedWorkspace focusedWindow (Workspace _ wspName windows) = do
  L.setStyle L.Round
  let wspFocused = focusedWorkspace == wspName
  L.openSection $ workspaceColor outFocused wspFocused
  L.write wspName
  forM_ windows $ \(Window wndId wndName urgent) -> do
    let wndFocused = focusedWindow == Just wndId
        colorPair = windowColor outFocused wspFocused wndFocused urgent
    L.openSection colorPair
    L.write $ L.processWindowTitle wndName
  L.closeSection

printOutput :: Output -> L.Powerlemon
printOutput (Output _ _ isFocused focusedWorkspace focusedWindow workspaces) = forM_ workspaces $ printWorkspace isFocused focusedWorkspace focusedWindow

-- I3 Printer
printI3 :: Index -> I3State -> I3Config -> L.Powerlemon
printI3 monitorIndex (I3State outputs) _ = printOutput $ outputs !! monitorIndex

-- Time printer.
printTime :: Index -> TimeState -> TimeConfig -> L.Powerlemon
printTime monitorIndex (TimeState time) (TimeConfig timeFormat) = do
  L.setStyle L.Common
  L.openSection L.neutralColorPair
  L.write $ formatTime defaultTimeLocale timeFormat time
  L.closeSection

printStateItem :: Index -> State -> ItemConfig -> L.Powerlemon
printStateItem monitorIndex State {i3State = Just i3State} (ItemConfig _ _ (I3 i3Config)) = printI3 monitorIndex i3State i3Config
printStateItem monitorIndex State {timeState = Just timeState} (ItemConfig _ _ (Time timeConfig)) = printTime monitorIndex timeState timeConfig
printStateItem _ _ _ = L.write "Loading..."

printState :: Index -> BarConfig -> State -> L.Powerlemon
printState monitorIndex (BarConfig l c r) state = do
  L.withTag ('S' : show monitorIndex) $ do
    L.setDirection L.Right
    printSection "l" l
    printSection "c" c
    L.setDirection L.Left
    printSection "r" r
  where
    printSection :: L.Tag -> [ItemConfig] -> L.Powerlemon
    printSection _ [] = return ()
    printSection tag sources@(_ : _) = L.withTag tag $ forM_ sources printOne
    printOne = printStateItem monitorIndex state

type PingChannel = Chan ()

waitPing :: PingChannel -> IO ()
waitPing = readChan

sendPing :: PingChannel -> IO ()
sendPing = (`writeChan` ())

forkPrintStateLoop :: [Rectangle] -> BarConfig -> SharedState -> PingChannel -> Handle -> IO ()
forkPrintStateLoop screenInfo config sharedState pingChannel outputHandle =
  let monitors = zipWith const [0 ..] screenInfo
   in forkForever $ do
        -- waitPing pingChannel
        debounce $- millis 10 $- waitPing pingChannel
        withMVar sharedState $ \state -> do
          let printMonitor monitor = printState monitor config state
              statusline = L.toString $ forM_ monitors printMonitor
          hPutStr outputHandle statusline
          hPutStr outputHandle "\n"

mapOutputs :: (Output -> Output) -> I3State -> I3State
mapOutputs f (I3State outputs) = I3State $ map f outputs

mapWorkspaces :: (Workspace -> Workspace) -> Output -> Output
mapWorkspaces f output@Output {outWorkspaces = workspaces} = output {outWorkspaces = map f workspaces}

mapWindows :: (Window -> Window) -> Workspace -> Workspace
mapWindows f wsp@Workspace {wspWindows = windows} = wsp {wspWindows = map f windows}

focusWorkspace :: Maybe I3Rpl.Node -> Maybe I3Rpl.Node -> I3State -> I3State
focusWorkspace
  ( Just
      node@I3Rpl.Node
        { I3Rpl.node_name = (Text.unpack . fromJust -> wspName),
          I3Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
        }
    )
  _ = mapOutputs mapOutput
    where
      mapOutput output =
        if outName output == wndOutputName
          then output {outFocused = True, outFocusedWorkspace = wspName}
          else output {outFocused = False}

ignoreWorkspace :: Maybe I3Rpl.Node -> Maybe I3Rpl.Node -> I3State -> I3State
ignoreWorkspace _ _ x = x

openWindow :: I3Rpl.Node -> I3State -> I3State
openWindow _ x = x

closeWindow :: I3Rpl.Node -> I3State -> I3State
closeWindow node = mapOutputs $
  mapWorkspaces $ \(Workspace index name windows) ->
    Workspace index name $ filter ((I3Rpl.node_id node ==) . wndId) windows

focusWindow :: I3Rpl.Node -> I3State -> I3State
focusWindow
  node@I3Rpl.Node
    { I3Rpl.node_id = nodeId,
      I3Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
    } = mapOutputs mapOutput
    where
      mapOutput output =
        if outName output == wndOutputName
          then output {outFocusedWindow = Just nodeId}
          else output

renameWindow :: I3Rpl.Node -> I3State -> I3State
renameWindow
  node@I3Rpl.Node
    { I3Rpl.node_id = nodeId,
      I3Rpl.node_name = (fmap Text.unpack -> Just name),
      I3Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
    } = mapOutputs mapOutput
    where
      mapOutput output =
        if outName output == wndOutputName
          then mapWorkspaces' output
          else output
      mapWorkspaces' = mapWorkspaces $
        mapWindows $ \window ->
          if wndId window == nodeId
            then window {wndName = name}
            else window

moveWindow :: I3Rpl.Node -> I3State -> I3State
moveWindow _ x = x

urgentWindow :: I3Rpl.Node -> I3State -> I3State
urgentWindow _ x = x

ignoreWindow :: I3Rpl.Node -> I3State -> I3State
ignoreWindow _ x = x

handleI3Event :: SharedState -> PingChannel -> I3Evt.Event -> IO ()
handleI3Event sharedState pingChannel event = modifyI3State sharedState (transformer event) >> sendPing pingChannel
  where
    transformer :: I3Evt.Event -> (I3State -> I3State)
    transformer (I3Evt.Workspace (I3Evt.WorkspaceEvent change current old)) = workspaceChangeHandler change current old
    transformer (I3Evt.Window (I3Evt.WindowEvent change node)) = windowChangeHandler change node

    workspaceChangeHandler I3Evt.Focus = focusWorkspace
    workspaceChangeHandler I3Evt.Init = ignoreWorkspace
    workspaceChangeHandler I3Evt.Empty = ignoreWorkspace
    workspaceChangeHandler I3Evt.Urgent = ignoreWorkspace
    workspaceChangeHandler I3Evt.Move = ignoreWorkspace
    workspaceChangeHandler _ = ignoreWorkspace

    windowChangeHandler I3Evt.WinNew = openWindow
    windowChangeHandler I3Evt.WinClose = closeWindow
    windowChangeHandler I3Evt.WinFocus = focusWindow
    windowChangeHandler I3Evt.WinTitle = renameWindow
    windowChangeHandler I3Evt.WinMove = moveWindow
    windowChangeHandler I3Evt.WinUrgent = urgentWindow
    windowChangeHandler _ = ignoreWindow

handleI3Reply :: SharedState -> PingChannel -> Either String I3Evt.Event -> IO ()
handleI3Reply _ _ (Left err) = error err
handleI3Reply s c (Right event) = handleI3Event s c event

initI3PrivateState :: SharedState -> PingChannel -> IO I3PrivateState
initI3PrivateState sharedState pingChannel = do
  let eventTypes = [I3Sub.Workspace, I3Sub.Window]
      eventHandler = handleI3Reply sharedState pingChannel
  forkIO $ I3.subscribe eventHandler eventTypes
  I3PrivateState <$> connecti3

rectToPosition :: I3Rpl.Rect -> Position
rectToPosition I3Rpl.Rect {I3Rpl.x = I32# x, I3Rpl.y = I32# y} = (I# x, I# y)

i3NodeToOutputs :: I3Rpl.Node -> [Output]
i3NodeToOutputs
  node@I3Rpl.Node
    { I3Rpl.node_name = (fmap Text.unpack -> nodeName),
      I3Rpl.node_type = nodeType,
      I3Rpl.node_rect = nodeRect,
      I3Rpl.node_nodes = nodes
    } = case (nodeName, nodeType) of
    (Just "root", I3Rpl.RootType) -> concatMap i3NodeToOutputs nodes
    (Just "__i3", I3Rpl.OutputType) -> []
    (Just name, I3Rpl.OutputType) ->
      let workspaces@(workspace : _) = i3NodeToWorkspaces node
          focusedWorkspace = wspName workspace
       in [Output (rectToPosition nodeRect) name False focusedWorkspace Nothing workspaces] -- TODO
    _ -> error "Can't convert node type to output"

i3NodeToWorkspaces :: I3Rpl.Node -> [Workspace]
i3NodeToWorkspaces
  node@I3Rpl.Node
    { I3Rpl.node_name = (fmap Text.unpack -> nodeName),
      I3Rpl.node_type = nodeType,
      I3Rpl.node_nodes = (Vector.toList -> nodes)
    } = case (nodeName, nodeType) of
    (_, I3Rpl.OutputType) -> concatMap i3NodeToWorkspaces nodes
    (_, I3Rpl.DockAreaType) -> []
    (_, I3Rpl.ConType) -> concatMap i3NodeToWorkspaces nodes
    (Just name, I3Rpl.WorkspaceType) -> [Workspace 0 name $ i3NodeToWindows node]
    _ -> error "Can't convert node type to workspace"

i3NodeToWindows :: I3Rpl.Node -> [Window]
i3NodeToWindows
  I3Rpl.Node
    { I3Rpl.node_id = nodeId,
      I3Rpl.node_name = (fmap Text.unpack -> nodeName),
      I3Rpl.node_type = nodeType,
      I3Rpl.node_nodes = (Vector.toList -> nodes)
    } = case (nodeName, nodeType, nodes) of
    (_, I3Rpl.WorkspaceType, _) -> concatMap i3NodeToWindows nodes
    (Nothing, I3Rpl.ConType, _ : _) -> concatMap i3NodeToWindows nodes
    (Just name, I3Rpl.ConType, []) -> [Window nodeId name False]
    _ -> error "Can't convert node type to window"

readI3Tree :: Socket -> IO [Output]
readI3Tree socket = do
  -- (Right (I3.Outputs (I3.OutputsReply outputs))) <- getOutputs socket
  -- (output_name output) (output_active output) (output_primary output)
  -- (Right (I3.Workspaces (I3.WorkspaceReply workspaces))) <- getWorkspaces socket
  -- (fromIntegral $ ws_num workspace) (ws_name workspace) (ws_visible workspace) (ws_focused workspace) (ws_urgent workspace) "asd" -- (ws_output workspace)
  (Right (I3Rpl.Tree rootNode)) <- getTree socket

  -- let buildWorkspace :: I3.Workspace -> Workspace
  --     buildWorkspace i3Workspace = undefined
  --
  --     buildOutput :: I3.Output -> Output
  --     buildOutput i3Output = Output undefined outputName $ map buildWorkspace matchingWorkspaces
  --       where
  --         outputName = Text.unpack $ I3.output_name i3Output
  --         matchingWorkspaces = filter ((== outputName) . Text.unpack . I3.ws_output) $ Vector.toList workspaces

  return . sort $ i3NodeToOutputs rootNode

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
      initItem :: [ItemConfig] -> IO (IO ())
      initItem [] = undefined
      initItem configs@(ItemConfig _ _ (I3 _) : _) = do
        privateState <- initI3PrivateState sharedState pingChannel
        return $ updateI3 configs privateState sharedState pingChannel
      initItem configs@(ItemConfig _ _ (Time _) : _) = return $ updateTime configs sharedState pingChannel
   in forM_ (elems partitioned) $ initItem >=> forkForever

main :: IO ()
main = do
  display <- openDisplay ":0"
  screenInfo <- getScreenInfo display
  -- All data stored here, static in config, dynamic in state.
  config <- readConfig
  sharedState <- createSharedState

  (lemonbarInput, lemonbarOutput, processHandle) <- L.launchLemonbar

  -- The updater notifies the printer via the pingchannel.
  pingChannel <- newChan

  forkUpdateStateLoops config sharedState pingChannel
  forkPrintStateLoop screenInfo config sharedState pingChannel lemonbarInput

  -- Replace this with last shell command.
  forever $ threadDelay $ seconds 100
