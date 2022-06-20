module Main where

-- import I3IPC (connecti3, getOutputs, getTree, getWorkspaces)
-- import I3IPC.Reply

import Control.Concurrent (forkIO, threadDelay, threadWaitRead)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Monad (forM_, forever, liftM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (concatMap, forM_, minimum)
import Data.List (intercalate, intersperse)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Text (Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import System.Posix.Types (Fd)
import GHC.IO.Handle (hPutStr, hFlush, Handle, hPutChar)
import System.Process (createProcess, proc, std_in, std_out, StdStream(CreatePipe))

data I3Config = I3Config

instance Semigroup I3Config where
  (<>) _ _ = I3Config

type TimeFormat = String

newtype TimeConfig = TimeConfig TimeFormat

newtype NetworkConfig = NetworkConfig {interface :: String}

data Source
  = I3 I3Config
  | Time TimeConfig
  | Weather
  | PulseAudio
  | Network NetworkConfig
  | Load
  | Storage

data PrintConfig = PrintConfig {left :: [Source], center :: [Source], right :: [Source]}

readConfig :: IO PrintConfig
readConfig = return $ PrintConfig [I3 I3Config] [] [Time $ TimeConfig "%F %R"]

data UpdateConfig = UpdateConfig
  { i3UConfig :: Maybe (),
    timeUConfig :: Maybe (),
    weatherUConfig :: Maybe (),
    pulseAudioUConfig :: Maybe (),
    networkUConfig :: Maybe (),
    loadUConfig :: Maybe (),
    storageUConfig :: Maybe ()
  }

instance Semigroup UpdateConfig where
  (<>) (UpdateConfig a b c d e f g) (UpdateConfig a2 b2 c2 d2 e2 f2 g2) =
    UpdateConfig (a <> a2) (b <> b2) (c <> c2) (d <> d2) (e <> e2) (f <> f2) (g <> g2)

instance Monoid UpdateConfig where
  mempty = UpdateConfig mempty mempty mempty mempty mempty mempty mempty

-- I don't like this.
toUpdateConfig' :: Source -> UpdateConfig
toUpdateConfig' (I3 _) = mempty {i3UConfig = Just ()}
toUpdateConfig' (Time _) = mempty {timeUConfig = Just ()}
toUpdateConfig' Weather = mempty {weatherUConfig = Just ()}
toUpdateConfig' PulseAudio = mempty {pulseAudioUConfig = Just ()}
toUpdateConfig' (Network _) = mempty {networkUConfig = Just ()}
toUpdateConfig' Load = mempty {loadUConfig = Just ()}
toUpdateConfig' Storage = mempty {storageUConfig = Just ()}

toUpdateConfig :: PrintConfig -> UpdateConfig
toUpdateConfig (PrintConfig l c r) = mconcat [foldSection l, foldSection c, foldSection r]
  where
    foldSection = mconcat . map toUpdateConfig'

type Id = String

type Index = Int

type Name = String

data Window = Window Id Name -- some bools here

data Workspace = Workspace Index Name [Window]

data Monitor = Monitor Index Name [Workspace]

type I3State = [Monitor]

type TimeState = ZonedTime

data State = State
  { timeState :: Maybe TimeState,
    i3State :: Maybe I3State
  }

type SharedState = MVar State

createSharedState :: IO SharedState
createSharedState = newMVar $ State Nothing Nothing

type USec = Int

withTimeout :: USec -> IO a -> IO (Maybe a)
withTimeout timeout generator = do
  mvar <- newEmptyMVar
  forkIO $ do
    forkIO $ do
      result <- generator
      putMVar mvar $ Just result

    threadDelay timeout
    putMVar mvar Nothing

  takeMVar mvar

type Tag = String

tag :: Tag -> String -> String
tag t text = "%{" ++ t ++ '}' : text ++ "%{" ++ t ++ "}"

-- I3 Printer
printI3 :: I3State -> I3Config -> String
printI3 _ _ = "TODO"

-- Time printer.
printTime :: TimeState -> TimeConfig -> String
printTime time (TimeConfig timeFormat) = formatTime defaultTimeLocale timeFormat time

printStateItem :: State -> Source -> String
printStateItem State {i3State = Just i3State} (I3 i3Config) = printI3 i3State i3Config
printStateItem State {timeState = Just timeState} (Time timeConfig) = printTime timeState timeConfig
printStateItem _ _ = "Loading..."

printState :: PrintConfig -> State -> String
printState (PrintConfig l c r) state = printSection "l" l ++ printSection "c" c ++ printSection "r" r
  where
    printSection :: Tag -> [Source] -> String
    printSection t [] = ""
    printSection t sources@(_ : _) = tag t $ printMany sources
    printMany :: [Source] -> String
    printMany = concatMap $ printStateItem state

type PingChannel = Chan ()

waitPing :: PingChannel -> IO ()
waitPing = readChan

sendPing :: PingChannel -> IO ()
sendPing = flip writeChan ()

forkPrintStateLoop :: PrintConfig -> SharedState -> PingChannel -> Handle -> IO ()
forkPrintStateLoop config sharedState pingChannel outputHandle = forever $ do
  waitPing pingChannel
  withMVar sharedState $ hPutStr outputHandle . printState config
  hPutChar outputHandle '\n'
  hFlush outputHandle

seconds :: Int -> USec
seconds = (*) 1000000

updateI3 :: SharedState -> PingChannel -> IO ()
updateI3 sharedState pingChannel = do
  threadDelay $ seconds 1000
  return ()

updateTime :: SharedState -> PingChannel -> IO ()
updateTime sharedState pingChannel = do
  zonedTime <- getZonedTime
  modifyMVar_ sharedState $ \state -> return $ state {timeState = Just zonedTime}
  sendPing pingChannel
  threadDelay $ seconds 30

forkForever :: IO a -> IO ()
forkForever = void . forkIO . forever

forkUpdateStateLoops :: UpdateConfig -> SharedState -> PingChannel -> IO ()
forkUpdateStateLoops (UpdateConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing) _ _ = return ()
forkUpdateStateLoops config sharedState pingChannel = do
    forkForever $ updateRoutine sharedState pingChannel
    forkUpdateStateLoops restConfig sharedState pingChannel
  where
    forkPartial :: UpdateConfig -> (UpdateConfig, SharedState -> PingChannel -> IO ())
    forkPartial UpdateConfig {i3UConfig = Just ()} = (config {i3UConfig = Nothing}, updateI3)
    forkPartial UpdateConfig {timeUConfig = Just ()} = (config {timeUConfig = Nothing}, updateTime)
    forkPartial UpdateConfig {i3UConfig = Nothing, timeUConfig = Nothing} = undefined

    (restConfig, updateRoutine) = forkPartial config

main :: IO ()
main = do
  -- All data stored here, static in config, dynamic in state.
  config <- readConfig
  let updateConfig = toUpdateConfig config
  sharedState <- createSharedState

  -- Launch lemonbar.
  let textFont = "UbuntuMono Nerd Font:size=12"
      lemonbarArgs = ["-f", textFont, "-a", "30", "-u", "-2"]
      rawProc = proc "lemonbar" lemonbarArgs
      procWithPipes = rawProc{ std_in = CreatePipe, std_out = CreatePipe }

  (Just lemonbarInput, Just lemonbarOutput, _, processHandle) <- createProcess procWithPipes

  -- The updater notifies the printer via the pingchannel.
  pingChannel <- newChan

  forkUpdateStateLoops updateConfig sharedState pingChannel
  forkPrintStateLoop config sharedState pingChannel lemonbarInput

  -- Replace this with last shell command.
  forever $ threadDelay $ seconds 100

-- populateI3Tables :: WithContextT ()
-- populateI3Tables = do
--   soc <- asks i3Socket
--
--   (Right (Outputs (OutputsReply outputs))) <- liftIO $ getOutputs soc
--   forM_ outputs $ \output -> do
--     insert i3Output $ I3Output (output_name output) (output_active output) (output_primary output)
--
--   (Right (Workspaces (WorkspaceReply workspaces))) <- liftIO $ getWorkspaces soc
--   forM_ workspaces $ \workspace -> do
--     insert i3Workspace $ I3Workspace (fromIntegral $ ws_num workspace) (ws_name workspace) (ws_visible workspace) (ws_focused workspace) (ws_urgent workspace) "asd" -- (ws_output workspace)

-- data I3Output = I3Output {outputName :: Text, outputActive :: Bool, outputPrimary :: Bool} deriving (Eq, Show, Generic)
--
-- -- instance FromRow I3Output where
-- --   fromRow = I3Output <$> field <*> field <*> field
-- --
-- -- instance ToRow I3Output where
-- --   toRow (I3Output name active primary) = [SQLText name, toSqlBool active, toSqlBool primary]
--
-- data I3Workspace = I3Workspace
--   { workspaceNum :: Int,
--     workspaceName :: Text,
--     workspaceVisible :: Bool,
--     workspaceFocused :: Bool,
--     workspaceUrgent :: Bool,
--     workspaceOutput :: Text
--   }
--   deriving (Eq, Show, Generic)
--
-- -- instance FromRow I3Workspace where
-- --   fromRow = I3Workspace <$> field <*> field <*> field <*> field <*> field <*> field
-- --
-- -- instance ToRow I3Workspace where
-- --   toRow (I3Workspace num name visible focused urgent output) = [toSqlInt num, SQLText name, toSqlBool visible, toSqlBool focused, toSqlBool urgent, SQLText output]
--
-- data I3Window = I3Window {windowId :: Int} deriving (Eq, Show, Generic)
--
--
-- instance Generic g => FromRow g where
--   fromRow = undefined
--
-- instance Generic g => ToRow g where
--   toRow = undefined
--
--
-- showNode :: Node -> String
-- showNode node =
--   unlines $
--     map
--       (\(name, valueGetter) -> name ++ ":\t" ++ valueGetter node)
--       [ ("Id", show . node_id),
--         ("Name", show . node_name),
--         ("Type", show . node_type),
--         ("Output", show . node_output),
--         ("Orientation", show . node_orientation),
--         ("Layout", show . node_layout),
--         ("Percent", show . node_percent),
--         ("Rect", show . node_rect),
--         ("WindowRect", show . node_window_rect),
--         ("DecoRect", show . node_deco_rect),
--         ("Geometry", show . node_geometry),
--         ("Window", show . node_window),
--         ("Urgent", show . node_urgent),
--         ("Focused", show . node_focused),
--         ("Focus", show . node_focus),
--         ("Nodes", unlines . concatMap (map ("    " ++) . lines . showNode) . Data.Vector.toList . node_nodes)
--       ]
--
-- main1 :: IO ()
-- main1 = do
--   soc <- connecti3
--   (Right (Workspaces (WorkspaceReply workspaces))) <- getWorkspaces soc
--   forM_ workspaces $ \x -> do
--     print x
--     print "----"
--
--   (Right (Outputs (OutputsReply outputs))) <- getOutputs soc
--   forM_ outputs $ \x -> do
--     print x
--     print "----"
--
--   (Right (Tree node)) <- getTree soc
--   print node
