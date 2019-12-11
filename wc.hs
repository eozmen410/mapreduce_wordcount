{-# LANGUAGE TupleSections #-}

module Final where
import Data.Char
import Data.Map(fromListWith, toList)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as C


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

-- take 1 chunk at a time of bytestrings and call map
wcmap :: [ByteString] -> [(ByteString, Int)]
wcmap = map (, 1) 

reduce :: [[(ByteString, Int)]] -> [(ByteString, Int)]
reduce _ = error "hi"


split :: Int -> [a] -> [[a]]
split nChunks xs = chunk (length xs `quot` nChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs

removeNonLetters :: ByteString -> ByteString
removeNonLetters s = C.filter (\x -> isAlpha x || isSpace x) $ C.map toLower s

getAsList:: ByteString -> [(ByteString, Int)]
getAsList content =  toList $ fromListWith (+) $ map (\a -> (removeNonLetters a,1)) $ C.words content
