{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Control.Monad
import Data.List

import Parser
import Data.Bits

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path1 path2 = do
  file1 <- BS.readFile path1
  file2 <- BS.readFile path2
  return . BS.filter (/=0) . BS.pack . BS.zipWith xor file1 $ file2

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  file <- BS.readFile $ path ++ ".enc"
  BS.writeFile path . BS.pack . BS.zipWith xor file $ BS.cycle key


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  file <- BS.readFile path
  return $ decode file


-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs path1 path2 = do
  victims <- parseFile path1 :: IO (Maybe [TId])
  transactions <- parseFile path2 :: IO (Maybe [Transaction])
  return $ maybe (Just [])
    (\victim_ids -> liftM ( filter 
      (\(Transaction {tid=x}) -> x `elem` victim_ids)
        ) transactions ) victims



-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = 
  foldr (\t inMap ->
    Map.alter (\a -> Just $ case a of
      Nothing -> negate $ amount t
      Just v -> v - amount t ) (from t) .
    Map.alter (\a -> Just $ case a of
      Nothing -> amount t
      Just v -> v + amount t ) (to t) $
    inMap ) Map.empty transactions

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal map = fst $ maximumBy (\l r -> snd l `compare` snd r) $ Map.toList map

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs inMap newIds
  | Map.size inMap <= 1 = []
  | otherwise = map snd pairs ++ undoTs remaining (drop (length pairs) newIds)
    where remaining = Map.fromList . filter (`notElem` (map fst pairs)) . filter ((/=0).snd) . Map.toList $ inMap
          pairs = zipWith3 (\a b t ->
            if (abs.snd$a) > (abs.snd$b) then
              (b, Transaction {from = fst a, to = fst b, amount = abs.snd$b, tid = t})
            else
              (a, Transaction {from = fst a, to = fst b, amount = abs.snd$a, tid = t})
            ) owing owed newIds
          owed = sortBy (\l r -> snd l `compare` snd r) . filter ((<0).snd) . Map.toList $ inMap
          owing = sortBy (\l r -> snd r `compare` snd l) . filter ((>0).snd) . Map.toList $ inMap

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path a = do
  let e = encode a
  BS.writeFile path e

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

