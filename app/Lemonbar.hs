module Lemonbar where

import Data.Maybe (Maybe (Just))
import GHC.IO.Handle (Handle)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, std_in, std_out)

type Tag = String

tag :: Tag -> String -> String
tag t text = "%{" ++ t ++ '}' : text ++ "%{" ++ t ++ "}"

launchLemonbar :: IO (Handle, Handle, ProcessHandle)
launchLemonbar =
  let textFont = "UbuntuMono Nerd Font:size=12"
      lemonbarArgs = ["-f", textFont, "-a", "30", "-u", "-2"]
      rawProc = proc "lemonbar" lemonbarArgs
      procWithPipes = rawProc {std_in = CreatePipe, std_out = CreatePipe}
   in do
        (Just stdIn, Just stdOut, _, pHandle) <- createProcess procWithPipes
        return (stdIn, stdOut, pHandle)
