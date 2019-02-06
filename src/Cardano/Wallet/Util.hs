{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- ^ Disabling the above because we use 'trace' in the 'traceBuildable'
-- function. Note that, there's now an extra rule in our hlint rules that
-- make sure there's no 'traceBuildable' hanging in the code, such that it's
-- really used for debugging only.

-- | Module for small utility functions.
module Cardano.Wallet.Util
       ( -- * String manipulation
         headToLower
       , stripFieldPrefix
       , mkJsonKey

       -- * Debug
       , traceBuildable

       -- * Formatting
       , buildIndent
       , buildList
       , buildMap
       , buildTrunc
       , buildTuple2

       -- * Miscellaneous
       , eitherToParser

       -- * Time
       , defaultApiTimeLocale
       , apiTimeFormat
       , parseApiUtcTime
       , showApiUtcTime
       ) where

import           Universum hiding (truncate)

import           Data.Aeson.Types (Parser)
import           Data.Char (isUpper, toLower)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Text.Internal.Builder (Builder)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Time as Time
import           Formatting (Format, bprint, build, formatToString, later, (%))
import qualified Formatting.Buildable
import qualified Prelude


-- * Debug

traceBuildable :: Buildable a => a -> b -> b
traceBuildable a b =
    trace (formatToString build $ a) b


-- * Formatting

buildIndent
    :: Int
    -> Format Builder (a -> Builder)
    -> Format r (a -> r)
buildIndent n buildA =
    later $ fmapBuilder (safeInit . TL.unlines . fmap (prefix <>) . TL.lines) . bprint buildA
  where
    prefix = TL.replicate (fromIntegral n) " "
    safeInit t
        | TL.null t = t
        | otherwise = TL.init t

buildTrunc
    :: Format Builder (a -> Builder)
    -> Format r (a -> r)
buildTrunc buildA = later $ fmapBuilder (truncate' 8) . bprint buildA
  where
    truncate' n txt
        | TL.length txt <= 2*n + 3 = txt
        | otherwise = TL.take n txt <> "..." <> TL.takeEnd n txt

buildMap
    :: Format Builder (a -> Builder)
    -> String
    -> Format Builder (b -> Builder)
    -> Format r (Map a b -> r)
buildMap buildA sep buildB =
    later $ bprint (buildList $ buildTuple2 buildA sep buildB) . Map.toList

buildList
    :: Foldable t
    => Format Builder (a -> Builder)
    -> Format r (t a -> r)
buildList buildA =
    later $ buildList' . F.toList
  where
    buildLine = ("- " <>) . fmapBuilder (TL.drop 2) . bprint (buildIndent 2 buildA)
    buildList' []    = "[]"
    buildList' [h]   = buildLine h
    buildList' (h:q) = buildLine h <> "\n" <> buildList' q

buildTuple2
    :: Format Builder (a -> Builder)
    -> String
    -> Format Builder (b -> Builder)
    -> Format r ((a, b) -> r)
buildTuple2 buildA sep buildB =
    later $ \(a, b) -> (bprint buildA a) <> " " <> fromString sep <> " " <> (bprint buildB b)

fmapBuilder
    :: (TL.Text -> TL.Text)
    -> Builder
    -> Builder
fmapBuilder fn =
    B.fromLazyText . fn . B.toLazyText


-- * Miscellaneous

-- | Convert a given Either to an Aeson Parser
eitherToParser :: Buildable a => Either a b -> Parser b
eitherToParser =
    either (fail . formatToString build) pure


-- * String manipulation utils

headToLower :: String -> Maybe String
headToLower []     = Nothing
headToLower (x:xs) = Just $ toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper) . drop 1

mkJsonKey :: String -> String
mkJsonKey s = fromMaybe s . headToLower $ stripFieldPrefix s

-- * Time

-- | Currently we support only American usage.
defaultApiTimeLocale :: Time.TimeLocale
defaultApiTimeLocale = Time.defaultTimeLocale

-- | Time format used in API. Corresponds to ISO-8601.
-- Example of time: "2018-03-07T16:20:27.477318"
--
-- Note: there is more neat support of time formats in time-1.9,
-- Data.Time.Format.ISO8601 module, but that version is barely applicable with
-- current LTS-9.1.
apiTimeFormat :: String
apiTimeFormat = Time.iso8601DateFormat (Just "%H:%M:%S%Q")

newtype UtcTimeParseError = UtcTimeParseError Text

instance Buildable UtcTimeParseError where
    build (UtcTimeParseError msg) = bprint ("UTC time parse error: "%build) msg

instance Show UtcTimeParseError where
    show = formatToString build

-- | Parse UTC time from API.
parseApiUtcTime :: Text -> Either UtcTimeParseError Time.UTCTime
parseApiUtcTime =
    first UtcTimeParseError .
    Time.parseTimeM False defaultApiTimeLocale apiTimeFormat .
    toString

-- | Encode UTC time for API.
showApiUtcTime :: Time.UTCTime -> Text
showApiUtcTime = toText . Time.formatTime defaultApiTimeLocale apiTimeFormat
