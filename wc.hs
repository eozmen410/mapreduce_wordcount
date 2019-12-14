{-# LANGUAGE TupleSections #-}

import Control.Parallel(pseq)
import Control.Parallel.Strategies
import Data.Char(isAlpha, isSpace, toLower)
import Data.Map(Map, fromListWith, toList, unionsWith)
import qualified Data.ByteString.Lazy.Char8 as B
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
            print $ length $ wcpar content
            -- print $ take 10 $ sort $ wcpar content
        [filename, "seq"] -> do
            content <- B.readFile filename
            print $ take 10 $ sort $ wcseq content
        _ -> do 
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename> <par/seq>"
        
wcseq :: B.ByteString -> [(B.ByteString, Int)]
wcseq = seqMapReduce wcmap wcreduce . split 16 . cleanWords

wcpar :: B.ByteString -> [(B.ByteString, Int)]
wcpar = finalreduce . parMapReduce rdeepseq wcmap rdeepseq parwcreduce . split 64 . cleanWords

-- wc helper functions
--
wcmap :: [B.ByteString] -> [(B.ByteString, Int)]
wcmap = map (, 1) 

parwcreduce :: [(B.ByteString, Int)] -> Map B.ByteString Int
parwcreduce = fromListWith (+)

finalreduce :: [Map B.ByteString Int] -> [(B.ByteString, Int)]
finalreduce = toList . unionsWith (+)

wcreduce :: [[(B.ByteString, Int)]] -> [(B.ByteString, Int)]
wcreduce  = toList . fromListWith (+) . concat 


-- map reduce library
--
seqMapReduce :: (a   -> b) -> ([b] -> c) -> [a] -> c
seqMapReduce mf rf = rf . map mf

parMapReduce
    :: Strategy b  -- for mapping
    -> (a   -> b)  -- map func
    -> Strategy c  -- for reducing
    -> (b -> c)  -- reduce func
    -> [a]         -- init list
    -> [c]
parMapReduce mstrat mf rstrat rf xs =
    mres `pseq` rres
  where mres = parMap mstrat mf xs
        rres = parMap rstrat rf mres  -- [[(B.ByteString, Int)]] 



-- Helper functions
--
sort :: Ord b => [(a,b)] -> [(a,b)]
sort = sortBy (flip compare `on` snd)

split :: Int -> [a] -> [[a]]
split nChunks xs = chunk (length xs `quot` nChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs

cleanWords :: B.ByteString -> [B.ByteString]
cleanWords = map removeNonLetters . B.words

removeNonLetters :: B.ByteString -> B.ByteString
removeNonLetters = B.filter (\x -> isAlpha x || isSpace x) . B.map toLower
