{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update.I3 (makeUpdater) where

import Base (PingIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar)
import Control.Monad.State (MonadState (get, put), State, execState)
import Data.Aeson (encode)
import Data.Either (Either (Left, Right))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text (unpack)
import Data.Vector qualified as Vector (head, toList)
import GHC.Int (Int (I#), Int32 (I32#))
import I3IPC qualified as Ipc
import I3IPC.Event qualified as Evt
import I3IPC.Message qualified as Msg
import I3IPC.Reply qualified as Rpl
import I3IPC.Subscribe qualified as Sub
import Network.Socket (Socket)
import State.I3
  ( I3State (I3State, i3OutputFocusedWorkspaces, i3OutputWorkspaces, i3Outputs, i3Windows, i3WorkspaceFocusedWindows, i3WorkspaceWindows, i3Workspaces),
    Output (Output, outName),
    OutputName,
    Position,
    Urgent,
    Window (Window, wndId, wndName),
    WindowId,
    Workspace (Workspace, wspName, wspUrgent),
    WorkspaceName,
    empty,
  )
import Utils.BiMultiMap qualified as BiMultiMap
import Utils.MVar (modifyState_, recomputeState_)
import Utils.Time (seconds)
import Utils.Types (Id, Name)
import Prelude

-- CONVENIENCE WRAPPERS.

type PartialTransformer partial whole = (partial -> partial) -> whole -> whole

-- { i3FocusedOutput :: OutputName,

applyToOutputs :: PartialTransformer (Map.Map OutputName Output) I3State
applyToOutputs f state = state {i3Outputs = f $ i3Outputs state}

applyToOutputWorkspaces :: PartialTransformer (BiMultiMap.BiMultiMap OutputName WorkspaceName) I3State
applyToOutputWorkspaces f state = state {i3OutputWorkspaces = f $ i3OutputWorkspaces state}

applyToOutputFocusedWorkspaces :: PartialTransformer (Map.Map OutputName WorkspaceName) I3State
applyToOutputFocusedWorkspaces f state = state {i3OutputFocusedWorkspaces = f $ i3OutputFocusedWorkspaces state}

applyToWorkspaces :: PartialTransformer (Map.Map WorkspaceName Workspace) I3State
applyToWorkspaces f state = state {i3Workspaces = f $ i3Workspaces state}

applyToWorkspaceWindows :: PartialTransformer (BiMultiMap.BiMultiMap WorkspaceName WindowId) I3State
applyToWorkspaceWindows f state = state {i3WorkspaceWindows = f $ i3WorkspaceWindows state}

applyToWorkspaceFocusedWindows :: PartialTransformer (Map.Map WorkspaceName WindowId) I3State
applyToWorkspaceFocusedWindows f state = state {i3WorkspaceFocusedWindows = f $ i3WorkspaceFocusedWindows state}

applyToWindows :: PartialTransformer (Map.Map WindowId Window) I3State
applyToWindows f state = state {i3Windows = f $ i3Windows state}

-- LOW LEVEL ACTIONS.

putOutput :: Output -> I3State -> I3State
putOutput output@Output {outName = outputName} = applyToOutputs $ Map.insert outputName output

assignWorkspaceToOutput :: WorkspaceName -> OutputName -> I3State -> I3State
assignWorkspaceToOutput workspaceName outputName = applyToOutputWorkspaces $ eitherToError . BiMultiMap.insert outputName workspaceName

focusWorkspaceOnOutput :: WorkspaceName -> OutputName -> I3State -> I3State
focusWorkspaceOnOutput workspaceName outputName = applyToOutputFocusedWorkspaces $ Map.insert outputName workspaceName

putWorkspace :: Workspace -> I3State -> I3State
putWorkspace workspace@Workspace {wspName = workspaceName} = applyToWorkspaces $ Map.insert workspaceName workspace

adjustWorkspace :: (Workspace -> Workspace) -> WorkspaceName -> I3State -> I3State
adjustWorkspace f workspaceName = applyToWorkspaces $ Map.adjust f workspaceName

assignWindowToWorkspace :: WindowId -> WorkspaceName -> I3State -> I3State
assignWindowToWorkspace windowId workspaceName = applyToWorkspaceWindows $ eitherToError . BiMultiMap.insert workspaceName windowId

focusWindowOnWorkspace :: WindowId -> WorkspaceName -> I3State -> I3State
focusWindowOnWorkspace windowId workspaceName = applyToWorkspaceFocusedWindows $ Map.insert workspaceName windowId

removeWorkspace :: WorkspaceName -> I3State -> I3State
removeWorkspace workspaceName = applyToWorkspaces $ Map.delete workspaceName

putWindow :: Window -> I3State -> I3State
putWindow window@Window {wndId = windowId} = applyToWindows $ Map.insert windowId window

adjustWindow :: (Window -> Window) -> WindowId -> I3State -> I3State
adjustWindow f windowId = applyToWindows $ Map.adjust f windowId

removeWindow :: WindowId -> I3State -> I3State
removeWindow windowId = applyToWindows $ Map.delete windowId

-- WORKSPACE EVENT HANDLERS.

workspaceData :: Maybe Rpl.Node -> (WorkspaceName, OutputName, Urgent)
workspaceData
  ( Just
      Rpl.Node
        { Rpl.node_name = Just (Text.unpack -> workspaceName),
          Rpl.node_output = Just (Text.unpack -> outputName),
          Rpl.node_urgent = urgent
        }
    ) =
    (workspaceName, outputName, urgent)
workspaceData _ = error "workspace node missing some data"

initWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
initWorkspace (workspaceData -> (workspaceName, outputName, _)) _ = assignWorkspaceToOutput workspaceName outputName . putWorkspace (Workspace workspaceName False)

focusWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
focusWorkspace (workspaceData -> (workspaceName, outputName, _)) _ = focusWorkspaceOnOutput workspaceName outputName

emptyWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
emptyWorkspace (workspaceData -> (workspaceName, _, _)) _ = removeWorkspace workspaceName

urgentWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
urgentWorkspace (workspaceData -> (workspaceName, _, urgent)) _ = adjustWorkspace (\w -> w {wspUrgent = urgent}) workspaceName

moveWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
moveWorkspace a b = error $ show a ++ " " ++ show b

--   ( Just
--       Rpl.Node
--         { Rpl.node_name = (Text.unpack . fromJust -> nodeWorkspaceName),
--           Rpl.node_output = (Text.unpack . fromJust -> nodeOutputName)
--         }
--     )
--   _ =
--     let isTargetWorkspace = (nodeWorkspaceName ==) . wspName
--         makeUrgent x workspace = workspace {wspUrgent = x}
--      in mapWorkspacesGuard isTargetWorkspace $ makeUrgent nodeUrgent
-- moveWorkspace _ _ = error "urgent workspace"

ignoreWorkspace :: Maybe Rpl.Node -> Maybe Rpl.Node -> I3State -> I3State
ignoreWorkspace _ _ = id

-- WINDOW EVENT HANDLERS.

windowData :: Rpl.Node -> (WindowId, Name, OutputName)
windowData
  Rpl.Node
    { Rpl.node_id = windowId,
      Rpl.node_name = Just (Text.unpack -> windowName),
      Rpl.node_type = Rpl.ConType,
      Rpl.node_output = Just (Text.unpack -> outputName)
    } =
    (windowId, windowName, outputName)
windowData _ = error "window node missing some data"

openWindow :: Rpl.Node -> I3State -> I3State
openWindow (windowData -> (windowId, windowName, outputName)) state
  | windowName == "bar" = state
  | otherwise =
    let workspaceName = fromJust $ Map.lookup outputName $ i3OutputFocusedWorkspaces state
     in assignWindowToWorkspace windowId workspaceName $ putWindow (Window windowId windowName False) state

closeWindow :: Rpl.Node -> I3State -> I3State
closeWindow (windowData -> (windowId, _, _)) = removeWindow windowId

focusWindow :: Rpl.Node -> I3State -> I3State
focusWindow (windowData -> (windowId, _, outputName)) state =
  let workspaceName = fromJust $ Map.lookup outputName $ i3OutputFocusedWorkspaces state
   in focusWindowOnWorkspace windowId workspaceName state

renameWindow :: Rpl.Node -> I3State -> I3State
renameWindow (windowData -> (windowId, windowName, _)) = adjustWindow (\w -> w {wndName = windowName}) windowId

moveWindow :: Rpl.Node -> I3State -> I3State
moveWindow _ = id

urgentWindow :: Rpl.Node -> I3State -> I3State
urgentWindow _ = id

ignoreWindow :: Rpl.Node -> I3State -> I3State
ignoreWindow _ = id

handleI3Event :: MVar I3State -> Evt.Event -> IO ()
handleI3Event sharedState event = modifyState_ sharedState (transformer event)
  where
    transformer :: Evt.Event -> (I3State -> I3State)
    transformer (Evt.Workspace (Evt.WorkspaceEvent change current old)) = workspaceChangeHandler change current old
    transformer (Evt.Window (Evt.WindowEvent change node)) = windowChangeHandler change node
    transformer _ = id

    workspaceChangeHandler Evt.Focus = focusWorkspace
    workspaceChangeHandler Evt.Init = initWorkspace
    workspaceChangeHandler Evt.Empty = emptyWorkspace
    workspaceChangeHandler Evt.Urgent = urgentWorkspace
    workspaceChangeHandler Evt.Move = moveWorkspace
    workspaceChangeHandler _ = ignoreWorkspace

    windowChangeHandler Evt.WinNew = openWindow
    windowChangeHandler Evt.WinClose = closeWindow
    windowChangeHandler Evt.WinFocus = focusWindow
    windowChangeHandler Evt.WinTitle = renameWindow
    windowChangeHandler Evt.WinMove = moveWindow
    windowChangeHandler Evt.WinUrgent = urgentWindow
    windowChangeHandler _ = ignoreWindow

handleI3Response :: MVar I3State -> Either String Evt.Event -> IO ()
handleI3Response _ (Left err) = error err
handleI3Response sharedState (Right event) = handleI3Event sharedState event

type I3BuildState x = State I3State x

rectToPosition :: Rpl.Rect -> Position
rectToPosition Rpl.Rect {Rpl.x = I32# x, Rpl.y = I32# y} = (I# x, I# y)

i3NodeToOutputs :: Id -> Rpl.Node -> I3BuildState ()
i3NodeToOutputs = undefined

-- focusedId
-- node@Rpl.Node
--   { Rpl.node_id = nodeId,
--     Rpl.node_name = (fmap Text.unpack -> nodeName),
--     Rpl.node_type = nodeType,
--     Rpl.node_rect = nodeRect,
--     Rpl.node_focus = (fromIntegral . Vector.head -> focusedChildId),
--     Rpl.node_nodes = nodes
--   } = case (nodeName, nodeType) of
--   (Just "root", Rpl.RootType) -> forM_ nodes $ i3NodeToOutputs focusedChildId
--   (Just "__i3", Rpl.OutputType) -> return ()
--   (Just name, Rpl.OutputType) -> do
--     maybeFocusedWorkspace <- i3NodeToWorkspaces name 0 node
--     let focusedWorkspace = fromMaybe (error "No focused workspace in output") maybeFocusedWorkspace
--     let position = rectToPosition nodeRect
--         outputFocused = focusedId == nodeId
--         output = Output position name outputFocused focusedWorkspace workspaces
--     putOutput output
--   _ -> error "Can't convert node type to output"

-- merge :: (Maybe a, [b]) -> (Maybe a, [b]) -> (Maybe a, [b])
-- merge (ma, la) (mb, lb) = (ma <|> mb, la ++ lb)
--
-- collect :: [(Maybe a, [b])] -> (Maybe a, [b])
-- collect = foldl merge (Nothing, [])
--
i3NodeToWorkspaces :: Name -> Id -> Rpl.Node -> I3BuildState [Name]
i3NodeToWorkspaces = undefined

-- outputName
-- focusedId
-- node@Rpl.Node
--   { Rpl.node_id = nodeId,
--     Rpl.node_name = (fmap Text.unpack -> nodeName),
--     Rpl.node_type = nodeType,
--     Rpl.node_urgent = nodeUrgent,
--     Rpl.node_focus = (fromIntegral . Vector.head -> focusedChildId),
--     Rpl.node_nodes = (Vector.toList -> nodes)
--   } =
--   let recurse = collect . map (i3NodeToWorkspaces outputName focusedChildId)
--    in case (nodeName, nodeType) of
--         (_, Rpl.OutputType) -> recurse nodes
--         (_, Rpl.DockAreaType) -> (Nothing, [])
--         (_, Rpl.ConType) -> recurse nodes
--         (Just name, Rpl.WorkspaceType) ->
--           let focusedWorkspace = if nodeId == focusedId then Just name else Nothing
--               (focusedWindow, windows) = i3NodeToWindows 0 node
--               workspace = Workspace name nodeUrgent focusedWindow windows
--            in (focusedWorkspace, [workspace])
--         _ -> error "Can't convert node type to workspace"

--
-- i3NodeToWindows :: Id -> Rpl.Node -> (Maybe Id, [Window])
-- i3NodeToWindows
--   focusedId
--   Rpl.Node
--     { Rpl.node_id = nodeId,
--       Rpl.node_name = (fmap Text.unpack -> nodeName),
--       Rpl.node_type = nodeType,
--       Rpl.node_focus = (map fromIntegral . Vector.toList -> focusedChildren),
--       Rpl.node_nodes = (Vector.toList -> nodes)
--     } =
--     let recurse = collect . map (i3NodeToWindows $ head focusedChildren)
--      in case (nodeName, nodeType, nodes) of
--           (_, Rpl.WorkspaceType, _) -> recurse nodes
--           (Nothing, Rpl.ConType, _ : _) -> recurse nodes
--           (Just name, Rpl.ConType, []) -> (if focusedId == nodeId then Just nodeId else Nothing, [Window nodeId name False])
--           _ -> error "Can't convert node type to window"

readI3Tree :: Socket -> IO I3State
readI3Tree socket = do
  (Right (Rpl.Tree rootNode)) <- Ipc.getTree socket
  return $ execState (i3NodeToOutputs 0 rootNode) empty

makeUpdater :: PingIO -> MVar I3State -> IO [IO ()]
makeUpdater ping sharedState = do
  socketA <- Ipc.connecti3
  let fullUpdate = recomputeState_ sharedState $ readI3Tree socketA
  -- ping
  -- threadDelay $ seconds 600

  socketB <- Ipc.connecti3
  let eventTypes = [Sub.Workspace, Sub.Window]
  void $ Msg.sendMsgPayload socketB Msg.Subscribe $ encode eventTypes
  void $ Ipc.receiveMsg socketB

  let diffUpdate = Ipc.receiveEvent socketB >>= handleI3Response sharedState

  return [fullUpdate >> ping >> threadDelay (seconds 60), diffUpdate >> ping]
