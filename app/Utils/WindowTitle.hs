{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.WindowTitle (processWindowTitle) where

import Prelude

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
    (".*discord.*", "\64366"),
    -- Vim (with filetype).
    (".*.html.*vim.*", "\58923\61755"),
    (".*.(cpp|hpp).*vim.*", "\58923\64369"),
    (".*.(cabal|hs).*vim.*", "\58923\58911"),
    (".*.(c|h).*vim.*", "\58923\64368"),
    (".*.py.*vim.*", "\58923\57909"),
    (".*.tsx?.*vim.*", "\58923\64484"),
    (".*.jsx?.*vim.*", "\58923\59214"),
    (".*.json.*vim.*", "\58923\64293"),
    (".*.rs.*vim.*", "\58923\59304"),
    (".*.docker.*vim.*", "\58923\62216"),
    (".*vim.*", "\58923"),
    -- Shell commands.
    (".*(ghci|cabal).*", "\58911"),
    (".*(python|flask|pytest|pip).*", "\57909"),
    (".*make.*", "\62499"),
    (".*psql.*", "\59246"),
    (".*htop.*", "\61668"),
    (".*man.*", "\61788"),
    (".*docker.*", "\62216"),
    (".*node.*", "\59214"),
    (".*npm.*", "\59166"),
    (".*irssi.*", "\62098"),
    (".*gdb.*", "\62499"),
    (".*zsh.*", "\61728")
    -- string "@.*: ~.*", "\xf120"},
  ]

loopThis :: NFA -> NFA
loopThis (NFA state next) =
  let this = NFA state $ next <> [(AnyLetter, this)]
   in this

sequence :: String -> NFA -> NFA
sequence [] baseCase = baseCase
sequence ('.' : '*' : cs) baseCase = loopThis $ sequence cs baseCase
sequence cs@('(' : _) baseCase = options [] cs baseCase
sequence (c : cs) baseCase = NFA Nothing [(Letter c, sequence cs baseCase)]

options :: [String] -> String -> NFA -> NFA
options os (')' : cs) baseCase =
  let csNfa = sequence cs baseCase
   in foldl1 alternative $ map (`sequence` csNfa) os
options os (x : cs) baseCase
  | x == '(' || x == '|' =
    let (pre, post) = span (\c -> c /= '|' && c /= ')') cs
     in options (pre : os) post baseCase
options _ _ _ = error "invalid regex?"

alternative :: NFA -> NFA -> NFA
alternative (NFA sa na) (NFA sb nb) = NFA (sa <|> sb) (na <> nb)

-- Does not optimize the NFA.
buildNFA :: [(String, String)] -> NFA
buildNFA = foldr1 alternative . map (\(i, o) -> sequence i $ NFA (Just o) [])

runNFA :: NFA -> String -> Maybe String
runNFA (NFA state _) [] = state
runNFA (NFA _ transitions) (c : cs) =
  let matches :: (Token, b) -> Bool
      matches (AnyLetter, _) = True
      matches (Letter x, _) | x == c = True
      matches _ = False
      nextNFAs = map snd $ filter matches transitions
   in foldl (<|>) Nothing $ map (`runNFA` cs) nextNFAs

processWindowTitle :: String -> String
processWindowTitle title = fromMaybe textTitle $ runNFA (buildNFA replacements) $ map toLower title
  where
    textTitle = if length title > 20 then take 17 title ++ "[\8230]" else title
