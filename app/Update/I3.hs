{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update.I3 where

import Config (ItemParams)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, putMVar, withMVar)
import Control.Monad (return, (>=>), (>>), (>>=))
import Data.Aeson (encode)
import Data.Bool (Bool (False, True))
import Data.Either (Either (Left, Right))
import Data.Eq ((==))
import Data.Foldable (concatMap)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List (filter, map, sort)
import Data.Maybe (Maybe (Just, Nothing), fromJust)
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (toList)
import GHC.Err (error)
import GHC.Int (Int (I#), Int32 (I32#))
import qualified I3IPC as Ipc
import qualified I3IPC.Event as Evt
import qualified I3IPC.Message as Msg
import qualified I3IPC.Reply as Rpl
import qualified I3IPC.Subscribe as Sub
import Network.Socket (Socket)
import State.I3
  ( I3State (I3State),
    Output (Output),
    Position,
    Window (Window),
    Workspace (Workspace),
    outFocused,
    outFocusedWindow,
    outFocusedWorkspace,
    outName,
    outWorkspaces,
    wndId,
    wndName,
    wspName,
    wspWindows,
  )
import System.IO (IO)
import Utils.MVar (modifyState_, recomputeState_)

mapOutputs :: (Output -> Output) -> I3State -> I3State
mapOutputs f (I3State outputs) = I3State $ map f outputs

mapWorkspaces :: (Workspace -> Workspace) -> Output -> Output
mapWorkspaces f output@Output {outWorkspaces = workspaces} = output {outWorkspaces = map f workspaces}

mapWindows :: (Window -> Window) -> Workspace -> Workspace
mapWindows f wsp@Workspace {wspWindows = windows} = wsp {wspWindows = map f windows}

focusWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
focusWorkspace
  ( Just
      node@Rpl.Node
        { Rpl.node_name = (Text.unpack . fromJust -> wspName),
          Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
        }
    )
  _ = mapOutputs mapOutput
    where
      mapOutput output =
        if outName output == wndOutputName
          then output {outFocused = True, outFocusedWorkspace = wspName}
          else output {outFocused = False}

ignoreWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
ignoreWorkspace _ _ x = x

openWindow :: Rpl.Node -> I3State -> I3State
openWindow _ x = x

closeWindow :: Rpl.Node -> I3State -> I3State
closeWindow node = mapOutputs $
  mapWorkspaces $ \(Workspace index name windows) ->
    Workspace index name $ filter ((Rpl.node_id node ==) . wndId) windows

focusWindow :: Rpl.Node -> I3State -> I3State
focusWindow
  node@Rpl.Node
    { Rpl.node_id = nodeId,
      Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
    } = mapOutputs mapOutput
    where
      mapOutput output =
        if outName output == wndOutputName
          then output {outFocusedWindow = Just nodeId}
          else output

renameWindow :: Rpl.Node -> I3State -> I3State
renameWindow
  node@Rpl.Node
    { Rpl.node_id = nodeId,
      Rpl.node_name = (fmap Text.unpack -> Just name),
      Rpl.node_output = (Text.unpack . fromJust -> wndOutputName)
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

moveWindow :: Rpl.Node -> I3State -> I3State
moveWindow _ x = x

urgentWindow :: Rpl.Node -> I3State -> I3State
urgentWindow _ x = x

ignoreWindow :: Rpl.Node -> I3State -> I3State
ignoreWindow _ x = x

handleI3Event :: MVar I3State -> Evt.Event -> IO ()
handleI3Event sharedState event = modifyState_ sharedState (transformer event)
  where
    transformer :: Evt.Event -> (I3State -> I3State)
    transformer (Evt.Workspace (Evt.WorkspaceEvent change current old)) = workspaceChangeHandler change current old
    transformer (Evt.Window (Evt.WindowEvent change node)) = windowChangeHandler change node

    workspaceChangeHandler Evt.Focus = focusWorkspace
    workspaceChangeHandler Evt.Init = ignoreWorkspace
    workspaceChangeHandler Evt.Empty = ignoreWorkspace
    workspaceChangeHandler Evt.Urgent = ignoreWorkspace
    workspaceChangeHandler Evt.Move = ignoreWorkspace
    workspaceChangeHandler _ = ignoreWorkspace

    windowChangeHandler Evt.WinNew = openWindow
    windowChangeHandler Evt.WinClose = closeWindow
    windowChangeHandler Evt.WinFocus = focusWindow
    windowChangeHandler Evt.WinTitle = renameWindow
    windowChangeHandler Evt.WinMove = moveWindow
    windowChangeHandler Evt.WinUrgent = urgentWindow
    windowChangeHandler _ = ignoreWindow

rectToPosition :: Rpl.Rect -> Position
rectToPosition Rpl.Rect {Rpl.x = I32# x, Rpl.y = I32# y} = (I# x, I# y)

i3NodeToOutputs :: Rpl.Node -> [Output]
i3NodeToOutputs
  node@Rpl.Node
    { Rpl.node_name = (fmap Text.unpack -> nodeName),
      Rpl.node_type = nodeType,
      Rpl.node_rect = nodeRect,
      Rpl.node_nodes = nodes
    } = case (nodeName, nodeType) of
    (Just "root", Rpl.RootType) -> concatMap i3NodeToOutputs nodes
    (Just "__i3", Rpl.OutputType) -> []
    (Just name, Rpl.OutputType) ->
      let workspaces@(workspace : _) = i3NodeToWorkspaces node
          focusedWorkspace = wspName workspace
       in [Output (rectToPosition nodeRect) name False focusedWorkspace Nothing workspaces] -- TODO
    _ -> error "Can't convert node type to output"

i3NodeToWorkspaces :: Rpl.Node -> [Workspace]
i3NodeToWorkspaces
  node@Rpl.Node
    { Rpl.node_name = (fmap Text.unpack -> nodeName),
      Rpl.node_type = nodeType,
      Rpl.node_nodes = (Vector.toList -> nodes)
    } = case (nodeName, nodeType) of
    (_, Rpl.OutputType) -> concatMap i3NodeToWorkspaces nodes
    (_, Rpl.DockAreaType) -> []
    (_, Rpl.ConType) -> concatMap i3NodeToWorkspaces nodes
    (Just name, Rpl.WorkspaceType) -> [Workspace 0 name $ i3NodeToWindows node]
    _ -> error "Can't convert node type to workspace"

i3NodeToWindows :: Rpl.Node -> [Window]
i3NodeToWindows
  Rpl.Node
    { Rpl.node_id = nodeId,
      Rpl.node_name = (fmap Text.unpack -> nodeName),
      Rpl.node_type = nodeType,
      Rpl.node_nodes = (Vector.toList -> nodes)
    } = case (nodeName, nodeType, nodes) of
    (_, Rpl.WorkspaceType, _) -> concatMap i3NodeToWindows nodes
    (Nothing, Rpl.ConType, _ : _) -> concatMap i3NodeToWindows nodes
    (Just name, Rpl.ConType, []) -> [Window nodeId name False]
    _ -> error "Can't convert node type to window"

readI3Tree :: Socket -> IO [Output]
readI3Tree socket = do
  (Right (Rpl.Tree rootNode)) <- Ipc.getTree socket
  return . sort $ i3NodeToOutputs rootNode

-- Full tree updater.
type UpdateI3Data = Socket

initUpdateI3 :: MVar UpdateI3Data -> IO ()
initUpdateI3 mvar = Ipc.connecti3 >>= putMVar mvar

updateI3 :: MVar UpdateI3Data -> MVar I3State -> [ItemParams] -> IO ()
updateI3 privateSharedState sharedState _ = recomputeState_ sharedState computeShared
  where
    computeShared = withMVar privateSharedState $ (I3State <$>) . readI3Tree

-- Diff updater (from events)
type UpdateI3EventData = Socket

initUpdateI3Event :: MVar UpdateI3EventData -> IO ()
initUpdateI3Event mvar = do
  socket <- Ipc.connecti3
  let eventTypes = [Sub.Workspace, Sub.Window]
  Msg.sendMsgPayload socket Msg.Subscribe $ encode eventTypes
  Ipc.receiveMsg socket
  putMVar mvar socket

updateI3Event :: MVar UpdateI3EventData -> MVar I3State -> [ItemParams] -> IO ()
updateI3Event privateSharedState sharedState _ =
  withMVar privateSharedState $
    Ipc.receiveEvent >=> \case
      (Left err) -> error err
      (Right event) -> handleI3Event sharedState event
