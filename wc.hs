{-# LANGUAGE TupleSections #-}

import Control.Parallel(pseq)
import Control.Parallel.Strategies
import Data.Char
import Data.Map(fromListWith, toList)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Data.List(sortBy)
import Data.Function(on)
import System.Environment(getArgs, getProgName)
import System.Exit(die)

{-

 Name: Ecenaz Ozmen and Yefri Gaitan
 Uni: eo2419 and yg2548

 ------------------------------

 COMS 4995 003 Parallel Functional Programming

 Final Project

 [Description]

 to compile:
 stack ghc -- -O2 -Wall -threaded -rtsopts -eventlog wc

 to run:
 ./wc big.txt seq +RTS -N8 -ls

 -----

 Use lts-14.5 as the "resolver" for the Haskell Tool Stack.

 Your code should load under GHCi 8.6.5 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw4

-}

-- have main function take two parameters: name of file and whether seq or par
main :: IO()
main = do 
    args <- getArgs
    case args of 
        [filename, "par"] -> do
            content <- B.readFile filename
            putStrLn "par"
        [filename, "seq"] -> do
            content <- B.readFile filename
            -- sort
            print $ take 10 $ mySort $ wcseq content
            -- no sort
            -- print $ take 10 $ wcseq content
        _ -> do 
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename> <par/seq>"
        
mySort :: Ord b => [(a,b)] -> [(a,b)]
mySort = sortBy (flip compare `on` snd)

wcseq :: B.ByteString -> [(B.ByteString, Int)]
wcseq s = wcreduce . map wcmap . chunk 64 $ map removeNonLetters $ B.words s 

-- take 1 chunk at a time of bytestrings and call map
wcmap :: [B.ByteString] -> [(B.ByteString, Int)]
wcmap = map (, 1) 

wcreduce :: [[(B.ByteString, Int)]] -> [(B.ByteString, Int)]
wcreduce  = Map.toList . Map.fromListWith (+) . concat 


getAsList:: B.ByteString -> [(B.ByteString, Int)]
getAsList content =  toList $ fromListWith (+) $ map (\a -> (removeNonLetters a,1)) $ B.words content



wcseqT1 :: B.ByteString -> [(B.ByteString, Int)]
wcseqT1 = seqMapReduce wcmap wcreduce . chunk 8 . cleanWords



seqMapReduce 
    :: (a   -> b)  -- map func
    -> ([b] -> c)  -- reduce func
    -> [a]         -- init list
    -> c
seqMapReduce mf rf = rf . map mf

parMapReduce
    :: Strategy b  -- for mapping
    -> (a   -> b)  -- map func
    -> Strategy c  -- for reducing
    -> ([b] -> c)  -- reduce func
    -> [a]         -- init list
    -> c
parMapReduce mstrat mf rstrat rf xs =
    mres `pseq` rres
  where mres = parMap mstrat mf xs
        rres = rf mres `using` rstrat



-- Helper functions

split :: Int -> [a] -> [[a]]
split nChunks xs = chunk (length xs `quot` nChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs

cleanWords :: B.ByteString -> [B.ByteString]
cleanWords = map removeNonLetters . B.words

removeNonLetters :: B.ByteString -> B.ByteString
removeNonLetters = B.filter (\x -> isAlpha x || isSpace x) . B.map toLower


