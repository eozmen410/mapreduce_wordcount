module Final where
import System.IO(Handle, hGetLine, hIsEOF, withFile, IOMode(ReadMode)) 


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


removeNonLetters :: String -> String
removeNonLetters s = (filter (\x -> isAlpha x || isSpace x)) $ map toLower s

getAsList:: String -> [(String, Int)]
getAsList content =  toList $ fromListWith (+) $ Prelude.map (\a -> (removeNonLetters a,1)) $ words content
