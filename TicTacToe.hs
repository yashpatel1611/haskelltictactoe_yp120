module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

--This functions checks if the game has a winner i.e. there is a
--column or a row that contains either all X's or all O's
gameOver :: Board -> Bool
gameOver b
  = foldl1 (||) (cols' ++ rows' ++ diag')
  where
    cols' = checkList (map nub (cols b))
    rows' = checkList (map nub (rows b))
    diag' = checkList (map nub (diags b))
    checkList (x : xs)
      | elemEmpty  = False : checkList xs
      | takenXOnly = [True]
      | takenOonly = [True]
      | otherwise  = False : checkList xs
      where
        elemTakenX = (Taken X `elem` x)
        elemTakenO = (Taken O `elem` x)
        elemEmpty  = (Empty `elem` x)
        takenXOnly = elemTakenX && not(elemTakenO)
        takenOonly = not(elemTakenX) && elemTakenO 
    checkList [] = [False]

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition "" = Nothing
parsePosition s
  | null x3   = readMaybe ("(" ++ x1 ++ "," ++ x2 ++ ")")
  | otherwise = Nothing   
  where
    x1 = head (words s)
    (x2 : x3) = tail (words s)

--tryMove will take a player, positon and baord
--returns Maybe Board
--function checks if it can place a player at the position
--if not, returns Nothing, if yes, returns Just Board
tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (x, y) board@(t, n)
  | checkEmpty t 0 = Just ((replace pos (Taken p) t), n)
  | otherwise      = Nothing
  where
    pos = (x * n) + y 
    checkEmpty [] n = False
    checkEmpty (t : ts) n
      | (n == pos) && (t == Empty) = True
      | (n == pos) && (t /= Empty) = False
      | otherwise                  = checkEmpty ts (n+1)

-------------------------------------------------------------------
-- I/O Functions

--PrettyPrint is done by using the given functions
--to seperate the rows to print out
--each row is then mapped to a string depending on its type
--each row is printed one by one through recursion
prettyPrint :: Board -> IO ()
prettyPrint b@(c : cs, n)
  = prettyPrint' rs
  where
    rs = map unwords (map (map (c')) (rows b))
    c' c
      | c == Taken O = "O"
      | c == Taken X = "X"
      | otherwise    = "-"
    prettyPrint' [] = do putStr ""
    prettyPrint' (r : r')
      = do
          putStrLn r
          prettyPrint' r'

prettyPrint ([] , _) = do putStrLn ""


-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b p
  = do
    putStrLn ""
    putStr $ "Player " ++ show p ++ ", make your move (row col) " 
    pos <- getLine
    case parsePosition pos of
      Nothing -> putStrLn invInp >> recur
      Just pos ->
        case tryMove p pos b of
          Nothing -> putStrLn invPos >> recur
          Just pos -> return pos
    where
      recur = takeTurn b p
      invInp = "Invalid input, try again"
      invPos = "Position taken/invalid, try again"

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Player -> Board -> IO ()
playGame p b
  = do
    prettyPrint b
    case gameOver b of
      True -> putStrLn $ "Congratulations, " ++ show p ++ ", has won the game!"
      False ->
        case p of
          O ->
            do
            takeTurn b X >>= playGame X
          X ->
            do
            takeTurn b O >>= playGame O
    where
      -- xplay = takeTurn b X
      -- oplay = takeTurn b O
      winMsg p' = putStrLn ("Congratulations, " ++ (show p') ++ ", you have won the game!")


-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
    putStrLn "Welcome to tic-tac-toe!"
    putStr "Enter board size: "
    bSize <- getLine
    do
    case readMaybe bSize :: Maybe Int of
      Nothing -> putStrLn invInp >> main
      Just bSize ->
        playGame O (createBoard bSize)   
    where
      invInp = "Invalid input, try again"
      createBoard :: Int -> Board
      createBoard n = (take (n*n) (repeat Empty), n) :: Board


-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
