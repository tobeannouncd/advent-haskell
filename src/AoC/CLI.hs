module AoC.CLI (cliMain) where

import AoC.CLI.Getter
import AoC.CLI.Input
import AoC.Solution (runSolution, runSolution')
import Data.Time
import Options.Applicative

cliMain :: IO ()
cliMain = do
  (year, day, runType) <- execParser =<< mkParser
  s <- getSolution (fromInteger year) (fromInteger day)
  case runType of
    RDownload -> runSolution s =<< getInput year day
    RStdin -> runSolution' s
    RFile f -> runSolution s =<< readFile f

latest :: IO (Integer, Integer)
latest = do
  let est = hoursToTimeZone (-5)
  (yy, mm, dd) <- toGregorian . localDay . utcToLocalTime est <$> getCurrentTime
  return $
    if mm == 12
      then (yy, min 25 (toInteger dd))
      else (yy - 1, 25)

data RunType
  = RDownload
  | RStdin
  | RFile FilePath

mkParser :: IO (ParserInfo (Integer, Integer, RunType))
mkParser = do
  (yearDef, dayDef) <- latest
  let parser = p yearDef dayDef
  return $ info (parser <**> helper) mods
 where
  mods = mempty
  p y d =
    (,,)
      <$> argument
        auto
        ( metavar "YEAR"
            <> value y
            <> showDefault
            <> help "puzzle year"
        )
      <*> argument
        auto
        ( metavar "DAY"
            <> value d
            <> showDefault
            <> help "puzzle day"
        )
      <*> ( flag' RStdin (short 's' <> long "stdin" <> help "use stdin as input")
              <|> RFile
              <$> strOption
                ( short 'f'
                    <> long "file"
                    <> completer (bashCompleter "directory")
                    <> help "get input from file"
                )
                <|> pure RDownload
          )