{-# LANGUAGE FunctionalDependencies #-}
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
import Data.Int (Int)
import Data.List (filter, intercalate, map, sort, zip, zipWith, (!!), (++))
import Data.Map (elems)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, mapMaybe)
import Data.Ord (Ord ((<=)))
import Data.Semigroup ((<>))
import Data.String (String, unwords)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle, hFlush, hPutChar, hPutStr)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Types (Rectangle)
import I3IPC as I3
import qualified I3IPC.Event as I3Evt
import qualified I3IPC.Reply as I3Rpl
import qualified I3IPC.Subscribe as I3Sub
import qualified Lemonbar as L
import Network.Socket (Socket)
import System.IO (IO, print)
import System.Posix.Types (Fd)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_in, std_out)
import TagPartition (partitionByConstructorTagWith)
import Text.Show (Show, show)
import Utils (debounce, forkForever, mapWhere, millis, seconds, ($-))
import Prelude (error, fromIntegral, undefined)

-- I3 State
type Id = Int

type Index = Int

type Name = String

type Focused = Bool

type Urgent = Bool

data Window = Window {windowId :: Id, windowName :: Name, windowFocused :: Focused, windowUrgent :: Urgent} deriving (Show) -- more bools here

data Workspace = Workspace {workspaceIndex :: Index, workspaceName :: Name, workspaecFocused :: Focused, workspaceWindows :: [Window]} deriving (Show)

type Position = (Int, Int)

data Output = Output {outputPosition :: Position, outputName :: Name, outputWorkspaces :: [Workspace]} deriving (Show)

instance Eq Output where
  (==) (Output a _ _) (Output b _ _) = a == b

instance Ord Output where
  -- Outputs are orderable by their XY positions. That's how they are indexed in lemonbar.
  (<=) (Output a _ _) (Output b _ _) = a <= b

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

printWorkspace :: Workspace -> L.Powerlemon
printWorkspace (Workspace _ name wspFocused windows) = do
  L.setStyle L.Round
  L.openSection L.inactiveColorPair
  L.write $ ' ' : name <> " "
  forM_ windows $ \(Window _ name winFocused urgent) -> do
    let colorPair = case (wspFocused, winFocused, urgent) of
          (_, _, True) -> L.urgentColorPair
          (_, True, _) -> L.focusedColorPair
          (_, _, _) -> L.inactiveColorPair
    L.openSection colorPair
    L.write $ ' ' : L.processWindowTitle name <> " "
  L.closeSection

printOutput :: Output -> L.Powerlemon
printOutput (Output _ _ workspaces) = forM_ workspaces printWorkspace

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
        debounce $- millis 10 $- waitPing pingChannel
        withMVar sharedState $ \state -> do
          let printMonitor monitor = printState monitor config state
              statusline = L.toString $ forM_ monitors printMonitor
          hPutStr outputHandle statusline
          hPutStr outputHandle "\n"

mapOutputs :: (Output -> Output) -> I3State -> I3State
mapOutputs f (I3State outputs) = I3State $ map f outputs

mapWorkspaces :: (Workspace -> Workspace) -> I3State -> I3State
mapWorkspaces f = mapOutputs $ \(Output pos name workspaces) -> Output pos name $ map f workspaces

mapWindows :: (Window -> Window) -> I3State -> I3State
mapWindows f = mapWorkspaces $ \(Workspace index name focused windows) -> Workspace index name focused $ map f windows

ignoreWorkspace :: Maybe I3Rpl.Node -> Maybe I3Rpl.Node -> I3State -> I3State
ignoreWorkspace _ _ x = x

openWindow :: I3Rpl.Node -> I3State -> I3State
openWindow _ x = x

closeWindow :: I3Rpl.Node -> I3State -> I3State
closeWindow node = mapWorkspaces $ \(Workspace index name focused windows) ->
  Workspace index name focused $ filter ((I3Rpl.node_id node ==) . windowId) windows

focusWindow :: I3Rpl.Node -> I3State -> I3State
focusWindow node = mapWindows $ \window ->
  if windowId window == I3Rpl.node_id node
    then window {windowFocused = True} -- TODO
    else window

renameWindow :: I3Rpl.Node -> I3State -> I3State
renameWindow node = mapWindows $ \window ->
  if windowId window == I3Rpl.node_id node
    then window {windowName = fromMaybe "undefined" $ Text.unpack <$> I3Rpl.node_name node}
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

    workspaceChangeHandler I3Evt.Focus = ignoreWorkspace
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

-- handle (Right evt) = case evt of
--   Workspace WorkspaceEvent {wrk_current} -> print wrk_current
--   Window WindowEvent {win_container} -> print win_container
--   _ -> error "No other event types"

initI3PrivateState :: SharedState -> PingChannel -> IO I3PrivateState
initI3PrivateState sharedState pingChannel = do
  let eventTypes = [I3Sub.Workspace, I3Sub.Window]
      eventHandler = handleI3Reply sharedState pingChannel
  forkIO $ I3.subscribe eventHandler eventTypes
  I3PrivateState <$> connecti3

i3NodeToOutputs :: I3Rpl.Node -> [Output]
i3NodeToOutputs I3Rpl.Node {I3Rpl.node_name = Just "root", I3Rpl.node_type = I3Rpl.RootType, I3Rpl.node_nodes = nodes} = concatMap i3NodeToOutputs nodes
i3NodeToOutputs I3Rpl.Node {I3Rpl.node_name = Just "__i3", I3Rpl.node_type = I3Rpl.OutputType} = []
i3NodeToOutputs node@I3Rpl.Node {I3Rpl.node_name = Just (Text.unpack -> name), I3Rpl.node_type = I3Rpl.OutputType, I3Rpl.node_rect = rect} = [Output (fromIntegral $ I3Rpl.x rect, fromIntegral $ I3Rpl.y rect) name $ i3NodeToWorkspaces node]
i3NodeToOutputs _ = undefined

i3NodeToWorkspaces :: I3Rpl.Node -> [Workspace]
i3NodeToWorkspaces I3Rpl.Node {I3Rpl.node_type = I3Rpl.OutputType, I3Rpl.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWorkspaces nodes
i3NodeToWorkspaces I3Rpl.Node {I3Rpl.node_type = I3Rpl.DockAreaType} = []
i3NodeToWorkspaces I3Rpl.Node {I3Rpl.node_type = I3Rpl.ConType, I3Rpl.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWorkspaces nodes
i3NodeToWorkspaces node@I3Rpl.Node {I3Rpl.node_name = Just (Text.unpack -> name), I3Rpl.node_type = I3Rpl.WorkspaceType} = [Workspace 0 name False $ i3NodeToWindows node]
i3NodeToWorkspaces _ = undefined

i3NodeToWindows :: I3Rpl.Node -> [Window]
i3NodeToWindows I3Rpl.Node {I3Rpl.node_type = I3Rpl.WorkspaceType, I3Rpl.node_nodes = (Vector.toList -> nodes)} = concatMap i3NodeToWindows nodes
i3NodeToWindows I3Rpl.Node {I3Rpl.node_name = Nothing, I3Rpl.node_type = I3Rpl.ConType, I3Rpl.node_nodes = (Vector.toList -> nodes@(_ : _))} = concatMap i3NodeToWindows nodes
i3NodeToWindows I3Rpl.Node {I3Rpl.node_id = id, I3Rpl.node_name = Just (Text.unpack -> name), I3Rpl.node_type = I3Rpl.ConType, I3Rpl.node_nodes = (Vector.toList -> [])} = [Window id name False False]
i3NodeToWindows _ = undefined

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
  threadDelay $ seconds 5

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
