{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Color
  ( Color,
    black,
    white,
    red,
    green,
    blue,
    dullGreen,
    lightGray,
    mixComplex,
    ColorPair,
    unfocusedColorPair,
    semifocusedColorPair,
    focusedColorPair,
    urgentColorPair,
    neutralColorPair,
    warnColorPair,
    fromBackColor,
  )
where

import Utils.Tuple (map4, zipWith4)
import Prelude

hex1ToDec :: Char -> Int
hex1ToDec a
  | '0' <= a && a <= '9' = ord a - ord '0'
  | 'A' <= a && a <= 'F' = 10 + ord a - ord 'A'
  | otherwise = invalidArguments "hex1ToDec" a

hex2ToDec :: (Char, Char) -> Int
hex2ToDec (a, b) = hex1ToDec a * 16 + hex1ToDec b

decToHex1 :: Int -> Char
decToHex1 x
  | x >= 16 = invalidArguments "decToHex1" x
  | x >= 10 = chr $ ord 'A' + x - 10
  | x >= 0 = chr $ ord '0' + x
  | otherwise = invalidArguments "decToHex" x

decToHex2 :: Int -> (Char, Char)
decToHex2 x
  | x >= 256 = invalidArguments "decToHex" x
  | x >= 0 = (decToHex1 (x `div` 16), decToHex1 (x `mod` 16))
  | otherwise = invalidArguments "decToHex" x

type Color = String

black, white :: Color
black = "#FF000000"
white = "#FFFFFFFF"

red, green, blue :: Color
red = "#FFFF0000"
green = "#FF00FF00"
blue = "#FF0000FF"

dullGreen, lightGray :: Color
dullGreen = "#FF008000"
lightGray = "#FFCCCCCC"

colorToInt8 :: Color -> (Int, Int, Int, Int)
colorToInt8 ['#', a1, a2, r1, r2, g1, g2, b1, b2] = map4 hex2ToDec ((a1, a2), (r1, r2), (g1, g2), (b1, b2))
colorToInt8 x = invalidArguments "colorToInt8" x

int8ToColor :: (Int, Int, Int, Int) -> Color
int8ToColor
  (map4 decToHex2 -> ((a1, a2), (r1, r2), (g1, g2), (b1, b2))) = ['#', a1, a2, r1, r2, g1, g2, b1, b2]

mixInt :: Float -> Int -> Int -> Int
mixInt f a b = floor $ fromIntegral a + f * fromIntegral (b - a)

mixColors :: Color -> Color -> Float -> Color
mixColors (colorToInt8 -> c1) (colorToInt8 -> c2) f = int8ToColor $ zipWith4 (mixInt f) c1 c2

mixComplex :: [(Float, Color)] -> Float -> Color
mixComplex xs@[] f = invalidArguments "mixComplex" (xs, f)
mixComplex [(_, ca)] _ = ca
mixComplex ((a, ca) : xs@((b, cb) : _)) f
  | f <= a = ca
  | f < b = mixColors ca cb $ (f - a) / (b - a)
  | otherwise = mixComplex xs f

type ColorPair = (Color, Color)

unfocusedColorPair, semifocusedColorPair, focusedColorPair, urgentColorPair, neutralColorPair, warnColorPair :: ColorPair
unfocusedColorPair = ("#FF707070", "#FF2A2A2A")
semifocusedColorPair = (lightGray, "#FF454545")
focusedColorPair = (lightGray, "#FF1010D0")
urgentColorPair = (lightGray, "#FFD01010")
neutralColorPair = (lightGray, dullGreen)
warnColorPair = (black, "#FFFFEE52")

-- good, {"#FF10D010", "#FF000000"}},
-- info, {"#FFCDCD00", "#FF000000"}},
-- warn, {"#FFD01010", "#FFCCCCCC"}},
-- critical, {"#FFFF0000", "#FFFFFFFF"}},

fromBackColor :: Color -> ColorPair
fromBackColor back@(colorToInt8 -> (a, r, g, b)) = (int8ToColor (a, 255 - r, 255 - g, 255 - b), back)
