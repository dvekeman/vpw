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
1
3 1
..*
0 0
1 1 1
 ----------------------} 

main :: IO ()
main = runCases

readInput :: IO Case
-- readInput = undefined
readInput = do
  (vx:vy:_)         <- readNumbers
  field             <- times vy readFieldLine
  (x':y':_)         <- readNumbers -- ^ (x, y) co van de blok
  (bx: by: bz: _)   <- readNumbers -- ^ breedte, hoogte, diepte
  
  let veld = Veld field
      blok = Blok x' y' bx by bz
  debugM_ ("Het veld is " ++ show veld)
  debugM_ ("De blok is " ++ show blok)
  
  return $ Case veld blok

runAlgo :: (Int, Case) -> IO ()
-- runAlgo (i, (Case ...)) = do
runAlgo (i, (Case veld blok)) = do
  threadDelay $ 10 * 1000 * 1000
  printLn i "Result"

readDim :: IO (Int, Int)
readDim = do
  (x':y':_) <- readNumbers
  return (x', y') 

data VeldType = DOEL | OK | X deriving (Show, Eq)

data Blok = Blok {
 --  lo :: (Int, Int) -- denk dat we hier beter x :: Int en y :: Int van maken
   x       :: Int   -- x co van linksonder
 , y       :: Int   -- y co van linksonder
 , breedte :: Int
 , hoogte  :: Int
 , diepte  :: Int
} deriving (Show, Eq)

newtype Veld = Veld [[VeldType]] deriving (Show, Eq)

data Case = Case Veld Blok

parseVeldType :: Char -> VeldType
parseVeldType 'x' = X
parseVeldType '.' = OK
parseVeldType '*' = DOEL

readFieldLine :: IO [VeldType]
readFieldLine = map parseVeldType <$> getLine

kantel :: Blok -> [Blok]
kantel blok = [kantel1 blok, kantel2 blok, kantel3 blok, kantel4 blok]

-- kantel naar voor
kantel1 :: Blok -> Blok
kantel1 blok = blok {
  -- lo = (fst (lo blok), snd (lo blok) - hoogte blok)
    x = x blok
  , y = y blok - hoogte blok
  -- , breedte
  , hoogte = diepte blok
  , diepte = hoogte blok
}
front = kantel1

-- kantel naar links
kantel2 :: Blok -> Blok
kantel2 blok = blok {
  -- lo = (fst (lo blok) - hoogte blok, snd (lo blok))
    x = x blok - hoogte blok
  , y = y blok
  , breedte = hoogte blok
  , diepte = diepte blok
  , hoogte = breedte blok
}
left = kantel2

-- kantel naar achter
kantel3 :: Blok -> Blok
kantel3 blok = blok {
  -- lo = (fst (lo blok), snd (lo blok) + diepte blok)
    x = x blok
  , y = y blok + diepte blok
  , breedte = breedte blok
  , hoogte = diepte blok
  , diepte = hoogte blok
}
back = kantel3

-- kantel naar rechts
kantel4 :: Blok -> Blok
kantel4 blok = blok {
--   lo = (fst (lo blok) + breedte blok, snd (lo blok))
    x = x blok + breedte blok
  , y = y blok
  , breedte = hoogte blok
  , hoogte = breedte blok
  , diepte = diepte blok
}
right = kantel4

isBuiten :: Blok -> Veld -> Bool
isBuiten blok veld@(Veld speelveld)
  | x blok < 0 =
    debug "x < 0"
    True
  | y blok < 0 =
    debug "y < 0"
    True
  | x blok + breedte blok > length (head speelveld) =
    debug "te lang"
    True
  | y blok + diepte blok > length speelveld =
    debug "te diep"
    True
  | otherwise =
      let results = map (isBuitenVeld . moveY) [0..diepte blok - 1]
      --                 ^^^^^^^^^^^^^^^^^^^^ -- we checken rij per rij. We schuiven y-co op met 0, 1, ... diepte blok - 1
                where moveY i = blok { y = y blok + i }
                      isBuitenVeld = isBuiten' veld
                      -- Controleer de blok voor een specifieke rij
                      isBuiten' :: Veld -> Blok -> Bool
                      isBuiten' (Veld speelveld) blok =
                          --        Neem het deel van de rij waar de blok zich op bevindt
                          let rij = take (breedte blok) $ drop (x blok) $ speelveld !! (y blok)
                          -- Kijk of er in het overlappende deel een of meerdere x'en voorkomen
                          in  X `elem` rij
      in or results

--------------------------------------------------------------------------------
                        -- Generally useful functions --

runCases :: IO ()
runCases = do
  total     <- readLn :: IO Int
  allInput  <- times total readInput
  mapM_ runAlgo $ zip [1..total] allInput
  -- times_ total runCase
  -- times total runCase
  -- mapM_ printLn $ addLineNr allResults

-- Read a list of numbers on the input line
readNumbers :: IO [Int]
readNumbers = map read . words <$> getLine

-- Read number of testcases
readTestCases :: IO Int
readTestCases = readLn

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

-- Add a line number to each element: (1, ..), (2, ..)
-- addLineNr :: [a] -> [(Int, a)]
-- addLineNr = zip [1..]

-- Format the line and print it
-- printLn :: (Int, (Int, Int)) -> IO ()
printLn ln d = putStrLn $ fmtLn ln d

fmtLn :: Int -> String -> String
fmtLn ln d = show ln ++ " " ++ d

debug :: String -> a -> a
debug text a
  | debugEnabled = trace text a
  | otherwise    = a

debugM :: (Monad m) => String -> a -> m a
debugM text a = return $ debug text a

debug_ :: String -> String
debug_ text = debug text ""

debugM_ :: (Monad m) => String -> m String
debugM_ text = debugM text ""

times :: Monad m => Int -> m a -> m [a]
times = replicateM

times_ :: Monad m => Int -> m a -> m ()
times_ = replicateM_

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
                        -- Test data --

blok1 = -- Blok (-1, 1) 2 1 100
  Blok {
      x = -1
    , y = 1
    , breedte = 2
    , hoogte  = 1
    , diepte  = 100
  }

blok2 = -- Blok (1, 1) 2 1 100
  Blok {
      x = 1
    , y = 1
    , breedte = 2
    , hoogte  = 1
    , diepte  = 100
  }

blok3 = -- Blok (1, 1) 4 1 100
  Blok {
      x = 1
    , y = 1
    , breedte = 4
    , hoogte  = 1
    , diepte  = 100
  }

veld1 = Veld [[X, X, X], [OK, DOEL, OK]]

blok4 = -- Blok (3, 4) 2 10 3
  Blok {
      x = 3
    , y = 4
    , breedte = 2
    , hoogte  = 10
    , diepte  = 3
  }
blok5 = -- Blok (3, 4) 2 10 1
  Blok {
      x = 3
    , y = 4
    , breedte = 2
    , hoogte  = 10
    , diepte  = 1
  }
blok6 = -- Blok (3, 2) 2 2 2
  Blok {
      x = 3
    , y = 2
    , breedte = 2
    , hoogte  = 2
    , diepte  = 2
  }

blok7 = -- Blok (1, 1) 2 2 2
  Blok {
      x = 1
    , y = 1
    , breedte = 2
    , hoogte  = 2
    , diepte  = 2
  }

veld2 = Veld
  [
     [X, X, X, X, X, X]
  ,  [X, X, X, X, X, X]
  ,  [X, X, X, X, X, X]
  ,  [OK, OK, OK, OK, OK]
  ,  [OK, OK, OK, OK, OK]
  ]