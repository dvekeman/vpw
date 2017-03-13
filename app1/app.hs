module Main where

import           Control.Monad

import           Data.List
import           Data.Functor
import           Debug.Trace
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Concurrent (threadDelay)

import           Prelude hiding (readList)

-- Keep me at the top
---------------------- 
debugEnabled :: Bool
debugEnabled = False
---------------------- 

-- Sample input
{---------------------- 
<PASTE HERE>
 ----------------------} 

main :: IO ()
main = runCases

--------------------------------------------------------------------------------
                        -- Data Types --
--------------------------------------------------------------------------------

data X = X {
    x1 :: String
  , y1 :: Int
} deriving (Show, Read, Eq)

newtype Y = Veld [[X]] 
  deriving (Show, Read, Eq)

data Case = Case X Y

--------------------------------------------------------------------------------

readInput :: IO Case
readInput = undefined
-- readInput = do
--   (vx:vy:_)         <- readNumbers
  
--   let ...
--   debugM_ ("... " ++ show ...)
  
--   return $ Case ...

-- Run the computation and print the result 
-- Example output is
-- 1 <blah>
-- 2 <blah>
runAlgo :: (Int, Case) -> IO ()
runAlgo = undefined
-- runAlgo (i, (Case ...)) = do
--   ...
--   printLn i "Result"

--------------------------------------------------------------------------------
                        -- Generally useful functions --
--------------------------------------------------------------------------------
-- Run all cases, printing a case result is the responsability of runCase
--
runCases :: IO ()
runCases = do
  total     <- readLn :: IO Int
  allInput  <- times total readInput
  mapM_ runAlgo $ zip [1..total] allInput

-- Read a list of numbers on the input line
readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

-- Read number of test cases
readTestCases :: IO Int
readTestCases = readLn

-- Input format: length of list, followed by list values, each on own line.
-- Input: 
-- 3
-- 100
-- 150
-- 999
-- Output: [100, 150, 999]
readList :: IO [Int]
readList = do
 len <- readLn :: IO Int
 replicateM len readLn :: IO [Int]
 -- Gotcha: this asks one list value, and repeats that!
 -- fmap (replicate len) readLn :: IO [Int]

-- Input format: n lists of the format expect by readList, concatened.
readLists :: Int -> IO [[Int]]
readLists = flip replicateM readList

-- Read two numbers on a line as a pair of Integers
-- Input: 2 5
-- Output: (2, 5)
readDimension :: IO (Int, Int)
readDimension = do
  (x':y':_) <- readNumbers
  return (x', y') 

-- Add a line number to each element: (1, ..), (2, ..)
-- addLineNr :: [a] -> [(Int, a)]
-- addLineNr = zip [1..]

-- Format the line and print it
printLn :: Int -> String -> IO ()
printLn ln d = putStrLn $ fmtLn ln d

-- Format the line by printing the line number and the result separated by a space
fmtLn :: Int -> String -> String
fmtLn ln d = show ln ++ " " ++ d

-- Like trace, has a return value
debug :: String -> a -> a
debug text a
  | debugEnabled = trace text a
  | otherwise    = a

-- Like trace but in monadic context
debugM :: String -> a -> IO a
debugM text a = return $ debug text a

-- Like trace but has no output
debug_ :: String -> String
debug_ text = debug text ""

-- Like trace but in monadic context and without any 
debugM_ :: String -> IO ()
debugM_ text = debugM text ()

times :: Monad m => Int -> m a -> m [a]
times = replicateM

times_ :: Monad m => Int -> m a -> m ()
times_ = replicateM_

--------------------------------------------------------------------------------
                        -- Other example functions --
--------------------------------------------------------------------------------
-- Parse a single character into a data type
-- parseX :: Char -> X
-- parseX 'a' = ...

-- Read a full line of characters which are not separated (e.g. "x.xxxo")
-- readXLine :: (Char -> X) -> IO [X]
-- readXLine f = map f <$> getLine

--------------------------------------------------------------------------------
                        -- Test stuff --
--------------------------------------------------------------------------------

-- veld1 = ...

--------------------------------------------------------------------------------
