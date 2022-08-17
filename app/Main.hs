{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config (BarConfig (BarConfig), ItemConfig (ItemConfig), ItemParams, ItemType (Ethernet, I3, Load, PulseAudio, Storage, Time, Weather), readConfig)
import Control.Applicative ((<*))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_, forever, mapM, return, unless, (>>), (>>=))
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Foldable (concat, concatMap, foldr, null)
import Data.Function (const, flip, ($))
import Data.Functor ((<$>))
import Data.List (head, map, unzip, zipWith, (++))
import qualified Data.Map as Map
import Data.String (String)
import Data.Traversable (sequenceA)
import Data.Tuple (uncurry)
import GHC.IO.Handle (Handle, hPutStr)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Types (Rectangle)
import qualified Lemonbar as L
import Module.Base (UpdaterBody, UpdaterInit)
import qualified Module.Ethernet
import qualified Module.I3
import qualified Module.Time
import Print.Base (PingIO, PongIO, createPingChannel)
import System.Environment (getArgs)
import System.IO (IO, hGetLine, print, putStrLn, stdout)
import Text.Show (show)
import Utils.IO (debounce)
import Utils.Monad (concatMapM)
import Utils.Time (USec, millis, seconds)
import Utils.Tuple (uncurry3)
import Utils.Types (Index)
import Prelude (undefined)

-- Printer.
type Printer = ItemParams -> Index -> L.Powerlemon

type Printers = Map.Map ItemType Printer

printItem :: Printers -> Index -> ItemConfig -> L.Powerlemon
printItem printers monitorIndex (ItemConfig itemType params) = (printers Map.! itemType) params monitorIndex

printMonitor :: Printers -> BarConfig -> Index -> L.Powerlemon
printMonitor printers (BarConfig l c r) monitorIndex = do
  L.withTag ('S' : show monitorIndex) $ do
    L.setDirection L.Right
    printMany "l" l
    printMany "c" c
    L.setDirection L.Left
    printMany "r" r
  where
    printMany :: L.Tag -> [ItemConfig] -> L.Powerlemon
    printMany tag sources = unless (null sources) $ L.withTag tag $ forM_ sources printOne
    printOne = printItem printers monitorIndex

render :: Printers -> BarConfig -> PongIO -> Handle -> [Rectangle] -> IO ()
render printers barConfig pong outputHandle screenInfo =
  let monitors = zipWith const [0 ..] screenInfo
   in forever $ do
        millis 10 `debounce` pong
        statusline <- L.toString $ forM_ monitors $ printMonitor printers barConfig
        hPutStr outputHandle statusline
        hPutStr outputHandle "\n"

-- Updaters.
type Updaters = [IO ()]

timedUpdateLoop :: PingIO -> UpdaterInit -> UpdaterBody -> USec -> IO ()
timedUpdateLoop ping init update itemDelay = init >> forever (update >> ping >> threadDelay itemDelay)

-- Main.
type PartitionedConfig = Map.Map ItemType [ItemParams]

partitionConfig :: BarConfig -> PartitionedConfig
partitionConfig (BarConfig l c r) = foldr (flip $ foldr insertIntoMap) Map.empty [l, c, r]
  where
    insertIntoMap :: ItemConfig -> Map.Map ItemType [ItemParams] -> Map.Map ItemType [ItemParams]
    insertIntoMap (ItemConfig itemType params) = Map.insertWith (++) itemType [params]

buildModule :: ItemType -> [ItemParams] -> IO (Printer, [(UpdaterInit, UpdaterBody, USec)])
buildModule I3 = Module.I3.buildModule
buildModule Time = Module.Time.buildModule
buildModule Ethernet = Module.Ethernet.buildModule

linkModule :: PingIO -> ItemType -> [ItemParams] -> IO ((ItemType, Printer), Updaters)
linkModule ping itemType itemParamsList = do
  (printerFunction, updaterParts) <- buildModule itemType itemParamsList
  let printer :: (ItemType, Printer)
      printer = (itemType, printerFunction)
      updaters :: [IO ()]
      updaters = map (uncurry3 $ timedUpdateLoop ping) updaterParts
  return (printer, updaters)

initializeModules :: PingIO -> BarConfig -> IO (Printers, Updaters)
initializeModules ping config = do
  let byItemType = Map.toList $ partitionConfig config
  (printers, updaters) <- unzip <$> forM byItemType (uncurry $ linkModule ping)
  return (Map.fromList printers, concat updaters)

-- I3 = seconds 300
-- Time = seconds 30
-- Ethernet = seconds 15
-- Weather = seconds 600
-- PulseAudio = seconds 30
-- Load = seconds 3
-- Storage = seconds 60

forkPrinterAndUpdaters :: BarConfig -> Handle -> [Rectangle] -> IO [ThreadId]
forkPrinterAndUpdaters config handle screenInfo = do
  (pong, ping) <- createPingChannel
  (printers, updaterIOs) <- initializeModules ping config
  let printerIO = render printers config pong handle screenInfo
  mapM forkIO $ printerIO : updaterIOs

runMain :: [String] -> IO ()
runMain args = do
  display <- openDisplay ":0"
  screenInfo <- getScreenInfo display
  config <- readConfig

  (lemonbarInput, lemonbarOutput, lemonbarError, processHandle) <- L.launchLemonbar
  let barHandle = case args of
        ("dry" : _) -> stdout
        _ -> lemonbarInput
  threadIds <- forkPrinterAndUpdaters config barHandle screenInfo

  -- Replace this with last shell command.
  forever $ do
    hGetLine lemonbarOutput >>= putStrLn
    hGetLine lemonbarError >>= putStrLn
    threadDelay $ seconds 1

main :: IO ()
main = catch (getArgs >>= runMain) handleAll
  where
    handleAll :: SomeException -> IO ()
    handleAll x = print x
