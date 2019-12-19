# Mapreduce Wordcount

Ecenaz Ozmen (eo2419)
Yefri Gaitan (yg2548)

Parallel Functional Programming 4995 - Final Project

There are 2 different implementations of parallel MapReduce for word counting in this folder. 
wc_eval.hs contains our initial attempt at parallelizing the task using the Eval Monad.
wc_eval also has the option to run sequentially, by using the seq option when running as listed below.

to compile:
 stack ghc -- -O2 -Wall -threaded -rtsopts -eventlog wc

to run sequantial word count:
 ./wc big.txt seq +RTS -N8 -ls (for sequential)
to run parallel word count:
 ./wc big.txt par +RTS -N8 -ls (for parallel)

wc_par.hs contains our improved solution to this problem by using the Par Monad.

to compile:
 stack ghc -- -O2 -Wall -threaded -rtsopts -eventlog wc

 to run:
 ./wc big.txt +RTS -N4 -ls 

