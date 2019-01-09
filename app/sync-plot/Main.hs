{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Aeson hiding ((.=))
import           Data.Char (toLower)
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Options.Applicative (Parser, execParser, info, helper, fullDesc, progDesc, subparser, commandGroup, command, flag, strOption, argument, help, metavar, str, value, short, long)
import           System.Exit (die)
import           Universum hiding ((.~), die)

import           Graphics.Rendering.Chart.Backend.Cairo (FileFormat(..), toFile, fo_format)
import           Graphics.Rendering.Chart.Easy hiding (argument, both)

data PlotType
    = PlotSync
    | PlotRestore
    deriving (Show, Eq)

data PlotOptions = PlotOptions
    { poPlot       :: PlotType
    , poExtraTitle :: Text
    , poPlotLeft   :: Bool
    , poPlotRight  :: Bool
    , poInputFile  :: FilePath
    , poOutputFile :: FilePath
    } deriving (Show, Eq)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
           ( fullDesc
             <> progDesc "Plot the results of a wallet sync" )

optionsParser :: Parser PlotOptions
optionsParser = PlotOptions <$> plotP <*> titleP <*> leftP <*> rightP <*> inputP <*> outputP
  where
    plotP = subparser
      ( command "sync" (info (pure PlotSync) (progDesc "Blockchain sync chart"))
        <> command "restore" (info (pure PlotRestore) (progDesc "Wallet restore chart"))
        <> commandGroup "Plot type"
      )
    leftP = flag True False (long "no-left" <> help "Don't plot first dataset")
    rightP = flag True False (long "no-right" <> help "Don't plot second dataset")
    titleP = strOption (long "extra-title" <> short 't' <> value "" <> help "Append text to chart title")
    inputP = argument str (metavar "INFILE" <> help "Input JSON")
    outputP = argument str (metavar "OUTFILE" <> help "Output PNG")

run :: PlotOptions -> IO ()
run PlotOptions{..} = do
    df <- loadRecords poInputFile
    let p = plotRecords poPlot poExtraTitle poPlotLeft poPlotRight df
    plotToFile poOutputFile p

plotToFile :: (Default r, ToRenderable r) => FilePath -> EC r () -> IO ()
plotToFile output = toFile (def & fo_format .~ formatFromFileName output) output

data Record = Record
    { recTime   :: !Double
    , recValues :: ![Double]
    } deriving (Show)

data DataFile = DataFile
    { dfRecords   :: ![Record]
    , dfStartTime :: !(Maybe UTCTime)
    }

instance FromJSON DataFile where
    parseJSON = withObject "DataFile" $ \ob -> DataFile <$> ob .: "data" <*> ob .:? "start_time"

instance FromJSON Record where
    parseJSON = withArray "Record" $ \v ->
        if V.length v == 2
        then (Record <$> parseJSON (v ! 0) <*> parseJSON (v ! 1))
        else fail
             $ "Incorrect record array length for Record, expected 2 entries but got: "
             ++ show v

formatFromFileName :: FilePath -> FileFormat
formatFromFileName f | ".png" `isSuffixOf` map toLower f = PNG
                     | otherwise = SVG

timeAxisLabel :: Maybe UTCTime -> String
timeAxisLabel startTime = "Time (seconds" <> maybe "" since startTime <> ")"
  where
    since t = " since started at " <> fmt t
    fmt = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

plotRecords :: PlotType -> Text -> Bool -> Bool -> DataFile -> EC (LayoutLR Double Double Double) ()
plotRecords pl title pleft pright (DataFile rs startTime) = do
    layoutlr_title .= plotTitle pl <> T.unpack title
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    layoutlr_x_axis . laxis_title .= timeAxisLabel startTime

    let showRight = pright && pl == PlotRestore
    layoutlr_right_axis_visibility . axis_show_line .= showRight
    layoutlr_right_axis_visibility . axis_show_ticks .= showRight
    layoutlr_right_axis_visibility . axis_show_labels .= showRight

    case pl of
      PlotSync -> do
        when pleft $ do
          plotLeft (line "Local block height" [ [ (t,h) | Record t [h,_] <- rs] ])
        when pright $ do
          plotLeft (line "Remote block height" [ [ (t,h) | Record t [_,h] <- rs] ])
        layoutlr_left_axis . laxis_title .= "Blocks"
      PlotRestore -> do
        when pleft $ do
          plotLeft (line "Restore completion" [ [ (t,pc) | Record t [pc,_] <- rs] ])
          layoutlr_left_axis . laxis_title .= "Percent"
        when pright $ do
          plotRight (line "Rate" [ [ (t,r) | Record t [_,r] <- rs] ])
          layoutlr_right_axis . laxis_title .= "Blocks/second"

plotTitle :: PlotType -> String
plotTitle PlotSync    = "Blockchain sync"
plotTitle PlotRestore = "Wallet restore"

loadRecords :: FilePath -> IO DataFile
loadRecords = eitherDecodeFileStrict >=> handle
  where
    handle (Left err) = die err
    handle (Right rs) = pure rs
