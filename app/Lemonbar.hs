{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lemonbar
  ( launchLemonbar,
    Style (..),
    Direction (..),
    SepTyp (..),
    getSep,
    Powerlemon,
    toString,
    setStyle,
    setDirection,
    setFg,
    setBg,
    flipColors,
    write,
    Tag,
    withTag,
    openSection,
    closeSection,
  )
where

import Control.Monad.Trans.State.Lazy (StateT, execStateT, get, modify)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, std_err, std_in, std_out)
import Utils.Color (Color, ColorPair, black, white)
import Prelude

launchLemonbar :: IO (Handle, Handle, Handle, ProcessHandle)
launchLemonbar =
  let textFont = "UbuntuMono Nerd Font:size=12"
      lemonbarArgs = ["-f", textFont, "-a", "30", "-u", "-2"]
      rawProc = proc "lemonbar" lemonbarArgs
      procWithPipes = rawProc {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
   in do
        (Just stdIn, Just stdOut, Just stdErr, pHandle) <- createProcess procWithPipes
        hSetBuffering stdIn LineBuffering
        return (stdIn, stdOut, stdErr, pHandle)

data Style = None | Common | Round | Fire | Data | Backslash | Slash | Lego

data Direction = West | East deriving stock (Eq)

data SepTyp = Full | Half

getSep :: Style -> Direction -> SepTyp -> String
getSep sepStyle West Full = fst $ getSepL' sepStyle
getSep sepStyle West Half = snd $ getSepL' sepStyle
getSep sepStyle East Full = snd $ getSepR' sepStyle
getSep sepStyle East Half = fst $ getSepR' sepStyle

getSepL' :: Style -> ([Char], [Char])
getSepL' None = ("", "")
getSepL' Common = ("\57522", "\57523")
getSepL' Round = ("\57526", "\57527 ")
getSepL' Fire = ("\57538", "\57539")
getSepL' Data = ("\57543", "\57543")
getSepL' Backslash = ("\57528", "\57529")
getSepL' Slash = ("\57532", "\57533")
getSepL' Lego = ("\57550\57556", "\57551")

getSepR' :: Style -> ([Char], [Char])
getSepR' None = ("", "")
getSepR' Common = ("\57521", "\57520")
getSepR' Round = (" \57525", "\57524")
getSepR' Fire = ("\57537", "\57536")
getSepR' Data = ("\57542", "\57542")
getSepR' Backslash = ("\57529", "\57534")
getSepR' Slash = ("\57533", "\57530")
getSepR' Lego = ("\57551", "\57554\57550")

data PWL = PWL {pwlFg :: Color, pwlBg :: Color, pwlDirection :: Direction, pwlStyle :: Style, pwlOutput :: String}

type Powerlemon = StateT PWL IO ()

toString :: Powerlemon -> IO String
toString = (pwlOutput <$>) . (`execStateT` PWL white black East None "")

setStyle :: Style -> Powerlemon
setStyle style = modify $ \state -> state {pwlStyle = style}

setDirection :: Direction -> Powerlemon
setDirection direction = modify $ \state -> state {pwlDirection = direction}

setFg, setBg :: Color -> Powerlemon
setFg color = modify $ \state@PWL {pwlOutput = output} -> state {pwlFg = color, pwlOutput = output <> "%{F" <> color <> "}"}
setBg color = modify $ \state@PWL {pwlOutput = output} -> state {pwlBg = color, pwlOutput = output <> "%{B" <> color <> "}"}

flipColors :: Powerlemon
flipColors = modify $ \(PWL fg bg dir style output) -> PWL bg fg dir style $ output <> "%{R}"

write :: String -> Powerlemon
write input = modify $ \state@PWL {pwlOutput = output} -> state {pwlOutput = output <> input}

type Tag = String

withTag :: Tag -> Powerlemon -> Powerlemon
withTag tag =
  let tagStr = "%{" ++ tag ++ "}"
      writeTag = write tagStr
   in (>> writeTag) . (writeTag >>)

openSection :: ColorPair -> Powerlemon
openSection (nextFg, nextBg) = do
  (PWL _ curBg direction style _) <- get

  if nextBg == curBg
    then do
      let sepString = getSep style direction Half
      setFg black
      write sepString
    else do
      let sepString = getSep style direction Full
      if direction == West
        then do
          setFg nextBg
          write sepString
          flipColors
        else do
          flipColors
          setBg nextBg
          write sepString

  setFg nextFg

closeSection :: Powerlemon
closeSection = openSection (white, black)
