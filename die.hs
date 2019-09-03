module Die where

import System.Random

rollDie :: Int -> IO Int
rollDie x = getStdRandom (randomR (1, x))

d100 :: IO Int
d100 = rollDie 100
