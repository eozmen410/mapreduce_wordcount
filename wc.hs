module Final where
import Data.Char
import Data.Map(fromListWith, toList)
import Data.ByteString(ByteString)


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

-- take chunks of bytestrings and call map and reduce on each and add up results
wc :: [[ByteString]] -> [(ByteString, Int)]
wc _ = error "Solution"



split :: Int -> [a] -> [[a]]
split nChunks xs = chunk (length xs `quot` nChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,bs) = splitAt n xs in as : chunk n bs



removeNonLetters :: String -> String
removeNonLetters s = filter (\x -> isAlpha x || isSpace x) $ map toLower s

getAsList:: String -> [(String, Int)]
getAsList content =  toList $ fromListWith (+) $ Prelude.map (\a -> (removeNonLetters a,1)) $ words content

