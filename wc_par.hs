{-# LANGUAGE TupleSections #-}

import Data.Char(isAlpha, toLower)
import Data.Map(Map, toList, unionWith, insert, empty)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(sortBy)
import Data.Function(on)
import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Stream
import Control.Monad.Par

{-

 Name: Ecenaz Ozmen and Yefri Gaitan
 Uni: eo2419 and yg2548

 ------------------------------

 COMS 4995 003 Parallel Functional Programming

 Final Project: MapReduce Word Counter

 to compile:
 stack ghc -- -O2 -Wall -threaded -rtsopts -eventlog wc_par

 to run:
 ./wc_par big.txt +RTS -N4 -ls -s

 -----

-}

main :: IO()
main = do 
    args <- getArgs
    case args of 
        [filename] -> do
            content <- B.readFile filename
            print $ take 10 $ sort $ pipeline 10000 content 
        _ -> do 
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename>"
        

wcmap :: Stream B.ByteString -> Par (Stream (B.ByteString, Int))
wcmap = streamMap (\bs -> (bs, 1))

wcreduce :: Stream (B.ByteString, Int) -> Par (Map B.ByteString Int)
wcreduce = streamFold (insertTuple) empty

finalreduce :: Stream (Map B.ByteString Int) -> Par (Map B.ByteString Int)
finalreduce = streamFold (unionWith (+)) empty

pipeline :: Int -> B.ByteString -> [(B.ByteString, Int)]
pipeline n bs = runPar $ do
    s0 <- streamFromList (chunk n (map removeNonLetters (B.words bs))) 
    s1 <- streamMap (runPar . streamFromList) s0 --using runPar to unbox monad
    s2 <- streamMap (runPar . wcmap) s1 
    s3 <- streamMap (runPar . wcreduce) s2 
    s4 <- finalreduce s3
    return $ toList s4


-- helper functions
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs

removeNonLetters :: B.ByteString -> B.ByteString
removeNonLetters = B.filter isAlpha . B.map toLower

insertTuple :: Map B.ByteString Int -> (B.ByteString, Int) -> Map B.ByteString Int
insertTuple m (k,v) = insert k v m

sort :: Ord b => [(a,b)] -> [(a,b)]
sort = sortBy (flip compare `on` snd)

