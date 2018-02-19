module Main where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.HashMap.Strict        as M
import           Data.Maybe                 (mapMaybe)
import           Data.Monoid                (Sum (..))
import           System.Environment         (getArgs)
import           System.IO                  (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename, keyIndexStr, valueIndexStr] -> do
      (maxKey, accumSum) <- parseFile filename (read keyIndexStr) (read valueIndexStr)
      putStrLn $ "max key: " ++ show maxKey ++ "\tsum: " ++ show accumSum
    _ -> hPutStrLn stderr $ "Invalid arguments: " ++ show args

type YearKey = Int
type Frequency = Int

parseFile :: FilePath -> Int -> Int -> IO (YearKey, Frequency)
parseFile inputFile keyIndex valueIndex = do
  contents <- B8.readFile inputFile
  maybeIO
    $ M.foldlWithKey' maximumKV Nothing
    $ fmap getSum
    $ M.fromListWith mappend
    $ mapMaybe ( getKeyValueFields keyIndex valueIndex
               . B8.split '\t'
               )
    $ B8.lines contents

getKeyValueFields :: Int -> Int -> [B8.ByteString] -> Maybe (YearKey, Sum Frequency)
getKeyValueFields i j row = (,) <$> readYearKey (row !! i) <*> fmap Sum (readFrequency (row !! j))

{-# INLINE readFrequency #-}
readFrequency :: B8.ByteString -> Maybe Frequency
readFrequency = fmap fst . B8.readInt

{-# INLINE readYearKey #-}
readYearKey :: B8.ByteString -> Maybe YearKey
readYearKey = fmap fst . B8.readInt
-- readYearKey = fmap fst . B8.readInt

maximumKV :: Maybe (YearKey, Frequency)
          -> YearKey
          -> Frequency
          -> Maybe (YearKey, Frequency)
maximumKV kv@(Just (_, v0)) k v
  | v > v0    = Just (k, v)
  | otherwise = kv
maximumKV Nothing k v = Just (k, v)

maybeIO :: Maybe a -> IO a
maybeIO (Just a) = return a
maybeIO Nothing  = fail "No value."
