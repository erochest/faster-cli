{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}


module Main where


import           Conduit
import           Control.Exception.Safe
import           Control.Monad.Trans.Resource ()
import qualified Data.ByteString.Char8        as B8
import           Data.ByteString.Lex.Integral
import           Data.Data
import qualified Data.HashMap.Strict          as M
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Format             as F
import qualified Data.Text.Lazy.IO            as TL
import qualified Data.Vector                  as V
import           GHC.Generics
import           Options.Applicative


main :: IO ()
main = do
  Options{..} <- execParser opts
  freqs <- runConduitRes $
    sourceFile file
      =$= linesUnboundedAsciiC
      =$= mapC splitLine
      =$= concatMapC (toPair keyField valueField)
      =$= foldlC sumPair mempty
  TL.putStrLn $ maybe "No values." (F.format "max key: {}\tsum: {}")
              $ M.foldlWithKey' maxValue Nothing freqs
    where
      maxValue :: Maybe (T.Text, Int) -> T.Text -> Sum Int -> Maybe (T.Text, Int)
      maxValue Nothing k (Sum v) = Just (k, v)
      maxValue p1@(Just (_, v1)) k2 (Sum v2)
        | v1 > v2   = p1
        | otherwise = Just (k2, v2)

toPair :: Int -> Int -> V.Vector B8.ByteString -> Maybe DataPair
toPair kf vf v =
  (,) <$> fmap decodeUtf8 (v V.!? kf)
      <*> fmap (Sum . readDecimal_) (v V.!? vf)

sumPair :: Counter T.Text -> DataPair -> Counter T.Text
sumPair m (k, v) = M.insertWith mappend k v m

splitLine :: B8.ByteString -> V.Vector B8.ByteString
splitLine = V.fromList . B8.split '\t'

type Counter a = M.HashMap a (Sum Int)
type DataPair = (T.Text, Sum Int)

newtype CsvError = CsvError { unCsvError :: T.Text }
  deriving (Eq, Data, Typeable, Generic)

instance Show CsvError where
  show = T.unpack . unCsvError

instance Exception CsvError

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
