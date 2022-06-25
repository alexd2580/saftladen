{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lemonbar where

import Control.Applicative ((<|>))
import Control.Monad (Monad, forM_, void, (>>), (>>=))
import Control.Monad.State (State, execState, get, modify, put)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Tuple (fst, snd, swap, uncurry)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, std_in, std_out)
import Utils (($-))
import Prelude (Bool (False, True), Char, Eq, IO, Int, Show, String, const, filter, foldl, foldr1, map, return, snd, undefined, (!!), ($), (++), (.), (<>), (==))

type Tag = String

-- tag :: Tag -> String -> String
-- tag t text = "%{" ++ t ++ '}' : text ++ "%{" ++ t ++ "}"

launchLemonbar :: IO (Handle, Handle, ProcessHandle)
launchLemonbar =
  let textFont = "UbuntuMono Nerd Font:size=12"
      lemonbarArgs = ["-f", textFont, "-a", "30", "-u", "-2"]
      rawProc = proc "lemonbar" lemonbarArgs
      procWithPipes = rawProc {std_in = CreatePipe, std_out = CreatePipe}
   in do
        (Just stdIn, Just stdOut, _, pHandle) <- createProcess procWithPipes
        hSetBuffering stdIn LineBuffering
        return (stdIn, stdOut, pHandle)

data Style = None | Common | Round | Fire | Data | Backslash | Slash | Lego

data Direction = Left | Right deriving (Eq)

data SepTyp = Full | Half

getSep :: Style -> Direction -> SepTyp -> String
getSep sepStyle Left Full = fst $ getSepL' sepStyle
getSep sepStyle Left Half = snd $ getSepL' sepStyle
getSep sepStyle Right Full = snd $ getSepR' sepStyle
getSep sepStyle Right Half = fst $ getSepR' sepStyle

getSepL' None = ("", "")
getSepL' Common = ("\57522", "\57523")
getSepL' Round = ("\57526", "\57527")
getSepL' Fire = ("\57538", "\57539")
getSepL' Data = ("\57543", "\57543")
getSepL' Backslash = ("\57528", "\57529")
getSepL' Slash = ("\57532", "\57533")
getSepL' Lego = ("\57550\57556", "\57551")

getSepR' None = ("", "")
getSepR' Common = ("\57521", "\57520")
getSepR' Round = ("\57525", "\57524")
getSepR' Fire = ("\57537", "\57536")
getSepR' Data = ("\57542", "\57542")
getSepR' Backslash = ("\57529", "\57534")
getSepR' Slash = ("\57533", "\57530")
getSepR' Lego = ("\57551", "\57554\57550")

--
-- void Lemonbar::separator(Separator sep, std::string const& next_bg, std::string const& next_fg, PowerlineStyle style) {
--     auto const& style_icons = powerline_styles.at(style);
--     if(next_bg == current_bg) {
--         if (sep == Separator::none) {
--             out << "%{F#FF000000}";
--         } else {
--             auto const separator_index = sep == Separator::left ? 1u : sep == Separator::vertical ? 1u : 2u;
--             out << "%{F#FF000000}" << style_icons[separator_index];
--         }
--     } else {
--         switch(sep) {
--         case Separator::none:
--             out << "%{B" << next_bg << "}";
--             break;
--         case Separator::left:
--             out << "%{F" << next_bg << "}" << style_icons[0] << "%{R}";
--             break;
--         case Separator::vertical:
--             out << "%{F" << next_bg << "}%{R}";
--             break;
--         case Separator::right:
--             out << "%{R}%{B" << next_bg << "}" << style_icons[3];
--             break;
--         }
--     }
--     out << "%{F" << next_fg << "}";
--
--     current_fg = next_fg;
--     current_bg = next_bg;
-- }

-- csvFile :: GenParser Char st String
-- csvFile =
--   do
--     result <- many line
--     eof
--     return result
--
-- -- Each line contains 1 or more cells, separated by a comma
-- line :: GenParser Char st [String]
-- line =
--   do
--     result <- cells
--     eol -- end of line
--     return result
--
-- -- Build up a list of cells.  Try to parse the first cell, then figure out
-- -- what ends the cell.
-- cells :: GenParser Char st [String]
-- cells =
--   do
--     first <- cellContent
--     next <- remainingCells
--     return (first : next)
--
-- -- The cell either ends with a comma, indicating that 1 or more cells follow,
-- -- or it doesn't, indicating that we're at the end of the cells for this line
-- remainingCells :: GenParser Char st [String]
-- remainingCells =
--   (char ',' >> cells) -- Found comma?  More cells coming
--     <|> (return []) -- No comma?  Return [], no more cells
--
-- -- Each cell contains 0 or more characters, which must not be a comma or
-- -- EOL
-- cellContent :: GenParser Char st String
-- cellContent =
--   many (noneOf ",\n")
--
-- -- The end of line character is \n
-- eol :: GenParser Char st Char
-- eol = char '\n'

-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" inputp

-- data ParseTree a = Token a | Symbols (Map.Map Char (ParseTree a)) | Anything (ParseTree a)

data Token = AnyLetter | Letter Char

data NFA = NFA (Maybe String) [(Token, NFA)]

replacements :: [(String, String)]
replacements =
  [ -- Chrome tabs.
    (".*telegram.*chrom.*", "\62056\57879"),
    (".*slack.*chrom.*", "\62056\61848"),
    (".*github.*chrom.*", "\62056\62472"),
    (".*gitlab.*chrom.*", "\62056\62102"),
    (".*stack overflow.*chrom.*", "\62056\61804"),
    (".*youtube.*chrom.*", "\62056\61802"),
    (".*jira.*chrom.*", "\62056\63490"),
    (".*paypal.*chrom.*", "\62056\61933"),
    (".*gmail.*chrom.*", "\62056\63402"),
    (".*amazon.*chrom.*", "\62056\62064"),
    (".*google.*chrom.*", "\62056\61856"),
    (".*chrom.*", "\62056"),
    -- Desktop programs.
    (".*vlc.*", "\64123"),
    (".*mumble.*", "\61744"),
    (".*volume control.*", "\61480"),
    (".*telegram.*", "\57879"),
    -- Vim (with filetype).
    (".*.html.*vim.*", "\58923\61755"),
    (".*.cpp.*vim.*", "\58923\64369"),
    (".*.hpp.*vim.*", "\58923\64369"),
    (".*.hs.*vim.*", "\58923\58911"),
    (".*.c.*vim.*", "\58923\64368"),
    (".*.h.*vim.*", "\58923\64368"),
    (".*.py.*vim.*", "\58923\57909"),
    (".*.ts.*vim.*", "\58923\64484"),
    (".*.tsx.*vim.*", "\58923\64484"),
    (".*.js.*vim.*", "\58923\59214"),
    (".*.jsx.*vim.*", "\58923\59214"),
    (".*.json.*vim.*", "\58923\64293"),
    (".*.rs.*vim.*", "\58923\59304"),
    (".*.docker.*vim.*", "\58923\62216"),
    (".*vim.*", "\58923"),
    -- Shell commands.
    (".*make.*", "\62499"),
    (".*psql.*", "\59246"),
    (".*htop.*", "\61668"),
    (".*man.*", "\61788"),
    (".*docker.*", "\62216"),
    (".*npm.*", "\59166"),
    (".*irssi.*", "\62098"),
    (".*gdb.*", "\62499"),
    (".*cabal.*", "\58911"),
    (".*zsh.*", "\61728")
    -- string "@.*: ~.*", "\xf120"},
  ]

loopThis :: NFA -> NFA
loopThis (NFA state next) =
  let this = NFA state $ next <> [(AnyLetter, this)]
   in this

sequence :: String -> String -> NFA
sequence [] output = NFA (Just output) []
sequence ('.' : '*' : cs) output = loopThis $ sequence cs output
sequence (c : cs) output = NFA Nothing [(Letter c, sequence cs output)]

alternative :: NFA -> NFA -> NFA
alternative (NFA sa na) (NFA sb nb) = NFA (sa <|> sb) (na <> nb)

-- Does not optimize the NFA.
buildNFA :: [(String, String)] -> NFA
buildNFA = foldr1 alternative . map (uncurry sequence)

runNFA :: NFA -> String -> Maybe String
runNFA (NFA state _) [] = state
runNFA (NFA _ transitions) (c : cs) =
  let matches (AnyLetter, _) = True
      matches (Letter x, _) | x == c = True
      matches _ = False
      nextNFAs = map snd $ filter matches transitions
   in foldl (<|>) Nothing $ map (`runNFA` cs) nextNFAs

processWindowTitle :: String -> String
processWindowTitle title = fromMaybe title $ runNFA (buildNFA replacements) $ map toLower title

type Color = String

type ColorPair = (Color, Color)

defaultFg, defaultBg :: Color
defaultFg = "#FFFFFFFF"
defaultBg = "#FF000000"

-- semiactive, {"#FF454545", "#FFCCCCCC"}},
-- good, {"#FF10D010", "#FF000000"}},

defaultColorPair, neutralColorPair, urgentColorPair, inactiveColorPair, focusedColorPair :: ColorPair
defaultColorPair = (defaultFg, defaultBg)
neutralColorPair = ("#FFCCCCCC", "#FF008000")
urgentColorPair = ("#FFCCCCCC", "#FFD01010")
inactiveColorPair = ("#FF707070", "#FF2A2A2A")
focusedColorPair = ("#FFCCCCCC", "#FF1010D0")

-- info, {"#FFCDCD00", "#FF000000"}},
-- warn, {"#FFD01010", "#FFCCCCCC"}},
-- critical, {"#FFFF0000", "#FFFFFFFF"}},

data PWL = PWL {pwlFg :: Color, pwlBg :: Color, pwlDirection :: Direction, pwlStyle :: Style, pwlOutput :: String}

type Powerlemon = State PWL ()

toString :: Powerlemon -> String
toString = pwlOutput . (`execState` PWL defaultFg defaultBg Right None "")

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

withTag :: Tag -> Powerlemon -> Powerlemon
withTag tag =
  let tagStr = "%{" ++ tag ++ "}"
      writeTag = write tagStr
   in (>> writeTag) . (writeTag >>)

openSection :: ColorPair -> Powerlemon
openSection next@(nextFg, nextBg) = do
  (PWL _ curBg direction style _) <- get

  if nextBg == curBg
    then do
      let sepString = getSep style direction Half
      setFg defaultBg
      write sepString
    else do
      let sepString = getSep style direction Full
      if direction == Left
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
closeSection = openSection defaultColorPair
