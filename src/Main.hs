module Main where


import           Options.Applicative
import Data.Monoid


main :: IO ()
main = print =<< execParser opts


data Options
  = Options
    { file       :: !FilePath
    , keyField   :: !Int
    , valueField :: !Int
    } deriving (Show, Eq)

opts' :: Parser Options
opts'
  =   Options
  <$> strOption (  short 'f' <> long "file" <> metavar "TSV_FILE"
                <> help "The TSV file to read input from.")
  <*> option auto (  short 'k' <> long "key-field" <> metavar "NUMBER"
                  <> help "The zero-based index of the field for the key.")
  <*> option auto (  short 'v' <> long "value-field" <> metavar "NUMBER"
                  <> help "The zero-based index of the field for the value.")

opts :: ParserInfo Options
opts
  = info (opts' <**> helper)
      (  fullDesc
      <> progDesc "Print the key and value with the maximum value."
      <> header "faster-cli -- print the key and value with the maximum value."
      )
