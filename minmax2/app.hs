import Control.Monad

main = do
  -- Read the total line
  n <- readLn :: IO Int
  -- Repeat n times: parse a bunch of lines as one: 3 9 4 6 => [9, 4, 6]
  parsedLine <- replicateM n parseLine
  -- (1) get the min & max for each line, (2) add a line nr
  -- and (3) print each line as a formatted string
  mapM_ printLn . addLineNr . (map minmax) $ parsedLine

-- Format the line and print it
-- printLn :: (Int, (Int, Int)) -> IO ()
printLn = putStrLn . fmtLn

-- Add a line number to each element: (1, ..), (2, ..)
addLineNr :: [a] -> [(Int, a)]
addLineNr = zip [1..]

-- minmax [5, 1, 99] returns (1, 99)
minmax :: (Ord t) => [t] -> (t, t)
minmax l = (minimum l, maximum l)

-- fmtLine (1, (1, 99)) return "1 1 99"
fmtLn :: (Show a) => (a, (a, a)) -> String
fmtLn (ln, (mi, ma)) = (show ln) ++ " " ++ (show mi) ++ " " ++ (show ma)

-- First read the number of lines to read as total
-- then create a list
parseLine :: IO [Int]
parseLine = do
  total <- readLn :: IO Int
  createList total []

-- Recursively create a list, line by line
createList :: Int -> [Int] -> IO [Int]
createList 0 l = return l
createList n l =  do
  x  <- readLn :: IO Int
  xs <- createList (n-1) l
  return (x:xs)
