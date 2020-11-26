module PGExtras.Helpers (
  maybeText,
  maybeInt,
  maybeRational,
  maybeBool,
  maybeZonedTime
) where


import qualified Data.Text as Text
import Data.Time (ZonedTime)
import Data.Ratio

nullString :: [Char]
nullString = "NULL"

maybeInt :: Maybe Int -> [Char]
maybeInt Nothing = nullString
maybeInt (Just x) = show x

maybeRational :: Maybe Rational -> [Char]
maybeRational Nothing = nullString
maybeRational (Just x) = showRational x

maybeText :: Maybe Text.Text -> [Char]
maybeText Nothing = nullString
maybeText (Just x) = Text.unpack(x)

maybeBool :: Maybe Bool -> [Char]
maybeBool Nothing = nullString
maybeBool (Just x) = show x

maybeZonedTime :: Maybe ZonedTime -> [Char]
maybeZonedTime Nothing = nullString
maybeZonedTime (Just x) = show x

showRational :: Rational -> [Char]
showRational rat = (if num < 0 then "-" else "") ++ (shows d ("." ++ take 4 (go next)))
    where
        (d, next) = abs num `quotRem` den
        num = numerator rat
        den = denominator rat

        go 0 = ""
        go x = let (d, next) = (10 * x) `quotRem` den
               in shows d (go next)

