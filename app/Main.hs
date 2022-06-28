{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Config (BarConfig (BarConfig), ItemConfig (ItemConfig), ItemParams, ItemType (Ethernet, I3, Load, PulseAudio, Storage, Time, Weather), readConfig)
import Control.Applicative ((<*))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forM, forM_, forever, mapM, return, unless, (>>), (>>=))
import Data.Foldable (concatMap, foldr, null)
import Data.Function (const, flip, ($))
import Data.List (map, unzip, zipWith, (++))
import qualified Data.Map as Map
import Data.Traversable (sequenceA)
import Data.Tuple (uncurry)
import GHC.IO.Handle (Handle, hPutStr)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Graphics.X11.Xlib.Types (Rectangle)
import qualified Lemonbar as L
import qualified Module.Ethernet
import qualified Module.I3
import qualified Module.Time
import Print.Base (createPingChannel)
import System.IO (IO)
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

printer :: Printers -> BarConfig -> IO () -> Handle -> [Rectangle] -> IO ()
printer printers barConfig pong outputHandle screenInfo =
  let monitors = zipWith const [0 ..] screenInfo
   in forever $ do
        millis 10 `debounce` pong
        statusline <- L.toString $ forM_ monitors $ printMonitor printers barConfig
        hPutStr outputHandle statusline
        hPutStr outputHandle "\n"

-- Updaters.
type Updaters = [IO ()]

timedUpdateLoop :: IO () -> IO () -> IO () -> USec -> IO ()
timedUpdateLoop ping init update itemDelay = init >> forever (update >> ping >> threadDelay itemDelay)

-- Main.
type PartitionedConfig = Map.Map ItemType [ItemParams]

partitionConfig :: BarConfig -> PartitionedConfig
partitionConfig (BarConfig l c r) = foldr (flip $ foldr insertIntoMap) Map.empty [l, c, r]
  where
    insertIntoMap :: ItemConfig -> Map.Map ItemType [ItemParams] -> Map.Map ItemType [ItemParams]
    insertIntoMap (ItemConfig itemType params) = Map.insertWith (++) itemType [params]

initializeModules :: IO () -> [(ItemType, [ItemParams])] -> IO (Printers, Updaters)
initializeModules ping paramsByItemType = do
  modules <- forM paramsByItemType $ uncurry buildModule
  let printers :: [Printer]
      initAndUpdaters :: [[(IO (), IO (), USec)]]
      (printers, initAndUpdaters) = unzip modules
      printers' = Map.fromList $ zipWith (\ p (t, _) -> (t, p)) printers paramsByItemType
      updaters' = concatMap (map (uncurry3 $ timedUpdateLoop ping)) initAndUpdaters
  return (printers', updaters')
  where
    buildModule :: ItemType -> [ItemParams] -> IO (Printer, [(IO (), IO (), USec)])
    buildModule I3 = Module.I3.buildModule
    buildModule Time = Module.Time.buildModule
    buildModule Ethernet = Module.Ethernet.buildModule

-- forkUpdater :: IO () -> ItemType -> [ItemParams] -> IO [ThreadId]
-- forkUpdater ping I3 params = do
--   (printer, updaters) <- I3.buildModule
--   mapM forkTimedUpdateLoop ping
--   forkTimedUpdateLoop ping UI3.initUpdateI3
--   where
--     init = undefined

-- forkUpdater ping I3 = seconds 300
-- forkUpdater ping Time = seconds 30
-- forkUpdater ping Ethernet = seconds 15
-- forkUpdater ping Weather = seconds 600
-- forkUpdater ping PulseAudio = seconds 30
-- forkUpdater ping Load = seconds 3
-- forkUpdater ping Storage = seconds 60

main :: IO ()
main = do
  display <- openDisplay ":0"
  screenInfo <- getScreenInfo display
  config <- readConfig
  (lemonbarInput, lemonbarOutput, processHandle) <- L.launchLemonbar
  (ping, pong) <- createPingChannel

  (printers, updaters) <- initializeModules ping $ Map.toList $ partitionConfig config
  printerId <- forkIO $ printer printers config pong lemonbarOutput screenInfo
  updaterIds <- mapM forkIO updaters

  -- Replace this with last shell command.
  forever $ threadDelay $ seconds 100
