-- minmax.hs
-- Better study this again before the VPW!

import Prelude hiding (readList)
import Control.Monad (replicateM)
import Data.List (intersperse)

-- Main function at the bottom

--------------------------------------------------------------------------------
                        -- Generally useful functions --

-- Input format: length of list, followed by list values, each on own line.
readList :: IO [Int]
readList = do
 len <- readLn :: IO Int
 replicateM len readLn :: IO [Int]
 -- Gotcha: this asks one list value, and repeats that!
 -- fmap (replicate len) readLn :: IO [Int]

-- Input format: n lists of the format expect by readList, concatened.
-- @hannes: sequence is pretty weird if you haven't seen this before.
readLists :: Int -> IO [[Int]]
readLists = flip replicateM readList
-- Working alternative:
-- readLists n = sequence $ replicate n readList

--------------------------------------------------------------------------------

showMinMax :: Int -> Int -> Int -> String
showMinMax pk min max = concat [show pk, " ", show min, " ", show max]

-- Efficiency low?
minMaxList :: [Int] -> (Int, Int)
minMaxList l = (min, max)
  where min = minimum l
        max = maximum l

minMaxLists:: [[Int]] -> [(Int, Int)]
minMaxLists = map minMaxList

number :: [a] -> [(Int,a)]
-- @hannes: [1..] is an infinite lists, which works because Haskell is lazy.
number l = zip [1..] l

numberMinMaxLists :: [[Int]] -> [(Int, (Int, Int))]
numberMinMaxLists = number . minMaxLists


showMinMaxLists' :: [[Int]] -> [String]
showMinMaxLists' = map (\(a, (b, c)) -> showMinMax a b c) . numberMinMaxLists

showMinMaxLists :: [[Int]] -> String
showMinMaxLists = concat . intersperse "\n" . showMinMaxLists'

main :: IO ()
main = do
  n <- readLn :: IO Int
  ll' <- readLists n
  putStrLn $ showMinMaxLists ll'

-- Diehard alternative
main' = do
  n <- readLn :: IO Int
  readLists n >>= \l -> putStrLn (showMinMaxLists l)

-- Dieharder or Brainfuck ;)
main'' = readLn >>= \n -> readLists n >>= \l -> putStrLn (showMinMaxLists l)
