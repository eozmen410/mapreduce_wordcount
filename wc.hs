{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.Map(fromListWith, toList)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import System.Environment(getArgs, getProgName)
import System.Exit(die)
-- import System.IO(readFile)

{-

 Name: Ecenaz Ozmen and Yefri Gaitan
 Uni: eo2419 and yg2548

 ------------------------------

 COMS 4995 003 Parallel Functional Programming

 Final Project

 [Description]

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
            putStrLn "seq"
        _ -> do 
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <filename> <par/seq>"
        



-- take 1 chunk at a time of bytestrings and call map
wcmap :: [B.ByteString] -> [(B.ByteString, Int)]
wcmap = map (, 1) 

reduce :: [[(B.ByteString, Int)]] -> [(B.ByteString, Int)]
reduce  = Map.toList . Map.fromListWith (+) . concat 


split :: Int -> [a] -> [[a]]
split nChunks xs = chunk (length xs `quot` nChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs

removeNonLetters :: B.ByteString -> B.ByteString
removeNonLetters s = B.filter (\x -> isAlpha x || isSpace x) $ B.map toLower s

getAsList:: B.ByteString -> [(B.ByteString, Int)]
getAsList content =  toList $ fromListWith (+) $ map (\a -> (removeNonLetters a,1)) $ B.words content
