# patternmatching
Short pattern matching examples using fragmentation sets

IX. Pattern Matching with Fragmentation Sets

https://youtu.be/T15WBAWDefc

Three little "fragmentation sets" toy programs are demonstrated in order to demonstrate pattern recognition.

The experiments are at:

https://github.com/KedalionDaimon/patternmatching

How to run:

Pick the Scheme interpreter of your choice, e.g. GNU Guile, and do:

guile larcom-e2.scm

guile larcom-f.scm

For the Haskell thing, do (assuming you have GHC):

ghci

:load sf2.hs

From here on, you can do experiments about similiarity, like:

similarity [1,2,3,4] [6,2,3,1]

-- giving 40, or:

similarity [0,1,1] [1,1,1,1,0]

-- giving 4, or:

similarity [2,2,2,2,2] [2,2,2,3,2,1]

-- giving 17, etc., until you leave with:

:q

A higher value means greater similarity. - Essentially, these functions are made to be used in your own AI programming experiments.
