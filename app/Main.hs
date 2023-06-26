{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Base (PingIO, PongIO, createPingChannel)
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (SomeException, catch)
import Data.List (zipWith)
import GHC.IO.Handle (Handle, hPutStr)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (openDisplay)
import Lemonbar qualified as L
import Print.Ethernet qualified
import Print.I3 qualified
import Print.Time qualified
import Print.Weather qualified
import State.Ethernet qualified
import State.I3 qualified
import State.Weather qualified
import System.Environment (getArgs)
import System.IO (hGetLine, print, putStrLn, stdout)
import System.Process (cleanupProcess)
import Update.Ethernet qualified
import Update.I3 qualified
import Update.Weather qualified
import Utils.IO (debounce)
import Utils.Time (millis, seconds)
import Utils.Types (Index)
import Prelude

type SharedStates =
  ( MVar State.I3.I3State,
    MVar State.Ethernet.EthernetState,
    MVar State.Weather.WeatherState
  )

printMonitor :: SharedStates -> Index -> L.Powerlemon
printMonitor (i3State, ethernetState, weatherState) monitorIndex = L.withTag ('S' : show monitorIndex) $ do
  L.setDirection L.East
  L.withTag "l" $ do
    Print.I3.print i3State monitorIndex
  L.setDirection L.West
  L.withTag "r" $ do
    Print.Weather.print weatherState monitorIndex
    Print.Ethernet.print ethernetState monitorIndex
    Print.Time.print monitorIndex

type OutputHandle = Handle

-- Main.

initSharedStates :: IO SharedStates
initSharedStates = do
  i3 <- State.I3.init
  ethernet <- State.Ethernet.init
  weather <- State.Weather.init
  return (i3, ethernet, weather)

forkPrinter :: PongIO -> OutputHandle -> SharedStates -> IO ThreadId
forkPrinter pong outputHandle sharedStates = do
  display <- openDisplay ":0"
  screenInfo <- getScreenInfo display
  let monitors :: [Int]
      monitors = zipWith const [0 ..] screenInfo
  forkIO $
    forever $ do
      millis 10 `debounce` pong
      statusLine <- L.toString $ forM_ monitors $ printMonitor sharedStates
      hPutStr outputHandle statusLine
      hPutStr outputHandle "\n"

forkUpdaters :: PingIO -> SharedStates -> IO [ThreadId]
forkUpdaters ping (i3State, ethernetState, weatherState) = do
  i3Updaters <- Update.I3.makeUpdater ping i3State
  ethernet <- Update.Ethernet.forkUpdater ping ethernetState
  weather <- Update.Weather.forkUpdater ping weatherState
  i3ThreadIds <- forM i3Updaters $ forkIO . forever
  return $ ethernet : weather : i3ThreadIds

-- I3 = seconds 300
-- Time = seconds 30
-- Ethernet = seconds 15
-- Weather = seconds 600
-- PulseAudio = seconds 30
-- Load = seconds 3
-- Storage = seconds 60

runMain :: [String] -> IO ()
runMain args = do
  (lemonbarInput, lemonbarOutput, lemonbarError, processHandle) <- L.launchLemonbar

  -- When "dry" print to stdout.
  let barHandle = case args of
        ("dry" : _) -> stdout
        _ -> lemonbarInput

  (pong, ping) <- createPingChannel

  sharedStates <- initSharedStates
  printerThreadId <- forkPrinter pong barHandle sharedStates
  updaterThreadIds <- forkUpdaters ping sharedStates

  let busyLoop :: IO ()
      busyLoop = forever $ do
        -- Replace this with last shell command.
        hGetLine lemonbarOutput >>= putStrLn
        hGetLine lemonbarError >>= putStrLn
        threadDelay $ seconds 1
      handleException :: SomeException -> IO ()
      handleException e = do
        print e
        killThread printerThreadId
        mapM_ killThread updaterThreadIds
        cleanupProcess (Just lemonbarInput, Just lemonbarOutput, Just lemonbarError, processHandle)

  busyLoop `catch` handleException

main :: IO ()
main = getArgs >>= runMain
